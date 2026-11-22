import http.server
import json
import re
import socketserver
import subprocess
import threading
from pathlib import Path
from playwright.sync_api import sync_playwright

class QuietHandler(http.server.SimpleHTTPRequestHandler):
    def log_message(self, format, *args):
        pass

def test_terminal_outcome_acceptance():
    root_dir = Path(__file__).resolve().parents[1]
    acceptance_html = Path(__file__).parent / "terminal-outcome-acceptance.html"
    raw_html = Path(__file__).parent / "terminal-outcome-raw.html"
    multi_html = Path(__file__).parent / "terminal-outcome-multi.html"

    calibration_paths = [Path(__file__).parent / f"terminal-calibration-{model}.html" for model in ("true", "inflated")]

    try:
        # Prepare reports via public create_summary_report_times entry point
        r_cmd = """
library(rtichoke)
source("tests/testthat/helper-terminal-paths.R")
ref <- jsonlite::fromJSON(resolve_terminal_event_ref())

df_censored <- as.data.frame(ref$cases$all_censored$data)
names(df_censored) <- c("id", "times", "reals")

df_ties <- as.data.frame(ref$cases$tied_events_censor$data)
names(df_ties) <- c("id", "times", "reals")

# 1. Main heuristic report with 2 populations (censored and tied events) across horizons 0, 1, 2, 3
create_summary_report_times(
  reals = list("censored" = df_censored$reals, "tied events" = df_ties$reals),
  times = list("censored" = df_censored$times, "tied events" = df_ties$times),
  fixed_time_horizons = c(0, 1, 2, 3),
  output_file = "terminal-outcome-acceptance.html",
  output_dir = "browser-acceptance",
  heuristics = TRUE
)

# 2. Raw report with same data and heuristics = FALSE
create_summary_report_times(
  reals = list("censored" = df_censored$reals, "tied events" = df_ties$reals),
  times = list("censored" = df_censored$times, "tied events" = df_ties$times),
  fixed_time_horizons = c(0, 1, 2, 3),
  output_file = "terminal-outcome-raw.html",
  output_dir = "browser-acceptance",
  heuristics = FALSE
)

# 3. Second heuristic report to test multiple instances and absence of DOM ID collisions
create_summary_report_times(
  reals = list("censored" = df_censored$reals),
  times = list("censored" = df_censored$times),
  fixed_time_horizons = c(0, 1),
  output_file = "terminal-outcome-multi.html",
  output_dir = "browser-acceptance",
  heuristics = TRUE
)

# The same simulated subjects and ranking are used for both prediction sets.
set.seed(20261122)
n <- 5000L
horizons <- c(1, 3, 5)
target_rate <- 0.10 * exp(0.8 * rnorm(n))
competing_rate <- 0.06
target <- rexp(n, target_rate)
competing <- rexp(n, competing_rate)
censored <- pmin(rexp(n, 0.09), 6)
times <- pmin(target, competing, censored)
reals <- ifelse(times == target, 1L, ifelse(times == competing, 2L, 0L))
total_rate <- target_rate + competing_rate
true <- vapply(horizons, function(h) {
  target_rate / total_rate * -expm1(-total_rate * h)
}, numeric(n))
inflated <- plogis(qlogis(true) + log(1.75))
for (model in c("true", "inflated")) {
  create_summary_report_times(
    reals, times, c(5, 1, 0, 3, 1),
    output_file = paste0("terminal-calibration-", model, ".html"),
    output_dir = "browser-acceptance",
    heuristics = TRUE,
    probs = get(model)
  )
}
"""
        subprocess.run(["Rscript", "-e", r_cmd], check=True, cwd=root_dir)

        # Start local HTTP server
        with socketserver.TCPServer(("127.0.0.1", 0), lambda *args: QuietHandler(*args, directory=str(root_dir))) as httpd:
            port = httpd.server_address[1]
            t = threading.Thread(target=httpd.serve_forever, daemon=True)
            t.start()

            url_heuristics = f"http://127.0.0.1:{port}/browser-acceptance/terminal-outcome-acceptance.html"
            url_raw = f"http://127.0.0.1:{port}/browser-acceptance/terminal-outcome-raw.html"
            print(f"Loading {url_heuristics}")

            with sync_playwright() as p:
                browser = p.chromium.launch(headless=True)
                page = browser.new_page(viewport={"width": 1200, "height": 900})

                console_errors = []
                page_errors = []
                page.on("console", lambda msg: console_errors.append(msg.text) if msg.type == "error" else None)
                page.on("pageerror", lambda err: page_errors.append(str(err)))

                # 1. Test Heuristic Report
                page.goto(url_heuristics, wait_until="load")

                readout_sel = ".rtichoke-outcome-distribution__readout"
                page.wait_for_selector(readout_sel, timeout=5000)
                page.wait_for_selector(".rtichoke-report__heuristics-controls", timeout=5000)
                print("✓ Heuristic report and controls rendered")

                # In-browser comprehensive verification
                result = page.evaluate("""async () => {
                    const check = (ok, msg) => { if (!ok) throw new Error(msg); };

                    // Extract embedded spec from page
                    const scriptTag = document.querySelector('script[type="application/json"]');
                    check(scriptTag, 'spec script tag missing');
                    const reportSpec = JSON.parse(scriptTag.textContent);
                    const compSpec = reportSpec.sections[0].items[0].spec;

                    const root = document.querySelector('.rtichoke-report');
                    const readout = () => root.querySelector('.rtichoke-outcome-distribution__readout');

                    const choose = (selector) => {
                        const radio = root.querySelector(selector);
                        check(radio, 'missing expected control: ' + selector);
                        radio.checked = true;
                        radio.dispatchEvent(new Event('change', { bubbles: true }));
                    };

                    const setHorizon = (index) => {
                        const slider = root.querySelector('input[type="range"]');
                        check(slider, 'missing slider control');
                        slider.value = String(index);
                        slider.dispatchEvent(new Event('input', { bubbles: true }));
                    };

                    const verify = (dist, exclusion) => {
                        const card = readout();
                        check(card, 'readout element missing after update');
                        const text = card.textContent;
                        check(text.includes('Time = ' + dist.horizon.toFixed(3)), 'wrong horizon in readout: ' + text);

                        const counts = exclusion.excludedCounts.find(c => c.horizon === dist.horizon);
                        for (const [heuristic, label, count] of [
                            [dist.censoringHeuristic, 'Censored excluded', counts.censoredExcludedCount],
                            [dist.competingHeuristic, 'Competing excluded', counts.competingExcludedCount]
                        ]) {
                            if (heuristic === 'excluded') {
                                check(text.includes(label + count + ' / ' + exclusion.originalSampleSize + ' ('),
                                      'lost exclusion accounting: ' + JSON.stringify({count, exclusion, dist, text}));
                            }
                        }

                        const stateRows = [...card.querySelectorAll('tbody tr')].slice(0, dist.states.length);
                        check(stateRows.length === dist.states.length, 'incomplete state vector rows');

                        dist.states.forEach((state, i) => {
                            const row = stateRows[i];
                            if (dist.estimateStatus === 'undefined_empty_population') {
                                check(row.textContent.includes('Unavailable'), 'null estimate became available');
                                check(!row.querySelector('.rtichoke-performance-table__bar'), 'null has probability bar');
                            } else {
                                check(row.textContent.includes((state.estimate * 100).toFixed(1) + '%'), 'probability changed unexpectedly');
                            }
                        });

                        check(text.includes('no subjects remain') === (dist.estimateStatus === 'undefined_empty_population'), 'empty population status notice lost');
                        check(text.includes('estimator plateaus') === (dist.estimateStatus === 'beyond_support_plateau'), 'plateau status notice lost');
                    };

                    let contexts = 0;
                    for (const [key, variant] of Object.entries(compSpec.variants)) {
                        const [c, k] = key.split(':');
                        choose('input[name$="-censoring"][value="' + c + '"]');
                        choose('input[name$="-competing"][value="' + k + '"]');

                        for (const evaluation of compSpec.evaluations) {
                            choose('input[value="' + evaluation.id + '"]');
                            const distributions = variant.stateDistributions.filter(d => d.evaluationId === evaluation.id);
                            const exclusion = variant.exclusions.find(e => e.evaluationId === evaluation.id);

                            for (const [index, dist] of distributions.entries()) {
                                setHorizon(index);
                                verify(dist, exclusion);
                                contexts++;
                            }
                        }

                        // Verify horizon preservation across heuristic change
                        setHorizon(1);
                        choose('input[name$="-censoring"][value="' + (c === 'excluded' ? 'adjusted' : 'excluded') + '"]');
                        check(readout().textContent.includes('Time = 1.000'), 'heuristic change lost horizon');
                        check(root.querySelector('input[value="evaluation-2"]').checked, 'heuristic change lost population');
                    }

                    const ids = [...document.querySelectorAll('[id]')].map(e => e.id);
                    check(new Set(ids).size === ids.length, 'duplicate DOM identities detected');

                    return contexts;
                }""")

                expected_contexts = 8 * 2 * 4
                assert result == expected_contexts, f"Expected {expected_contexts} contexts, got {result}"
                print(f"✓ All {result} contexts verified (8 combinations x 2 populations x 4 horizons)")

                # Verify explanatory note in heuristic report (using text_content since <details> is collapsed)
                note_text = page.locator(".rtichoke-cheat-sheet").text_content()
                assert "Outcome estimates reflect the selected treatment" in note_text
                assert "Exclusions define a retained population" in note_text
                assert "Censoring-adjusted estimates rely" in note_text
                assert "raw observed outcome status" not in note_text

                # 2. Test Raw Report (heuristics = FALSE)
                print(f"Loading {url_raw}")
                page.goto(url_raw, wait_until="load")
                page.wait_for_selector(readout_sel, timeout=5000)

                # Assert NO heuristic controls are present in raw report
                assert page.locator(".rtichoke-report__heuristics-controls").count() == 0
                assert page.locator("input[name$='-censoring']").count() == 0
                assert page.locator("input[name$='-competing']").count() == 0

                raw_note = page.locator(".rtichoke-cheat-sheet").text_content()
                assert "raw observed outcome status over time" in raw_note
                assert "no Aalen-Johansen, IPCW, or other censoring adjustment is applied" in raw_note

                # 3. Test Offline Capability (file:// loading directly from disk)
                print("Testing offline file:// loading...")
                page.goto(f"file://{acceptance_html.resolve()}", wait_until="load")
                page.wait_for_selector(readout_sel, timeout=5000)
                page.wait_for_selector(".rtichoke-report__heuristics-controls", timeout=5000)
                print("✓ File:// offline loading passed without error")

                calibration_data = []
                for path in calibration_paths:
                    page.goto(path.as_uri(), wait_until="load")
                    page.get_by_text("Calibration method and group sizes").click()
                    page.get_by_text(re.compile(r"Horizon 1\s*:\s*1000, 1000, 1000, 1000, 1000")).wait_for(state="visible")
                    items = page.evaluate("""() => JSON.parse(
                        document.querySelector('script[id$="-spec"]').textContent
                    ).sections.find(section => section.id === 'calibration').items""")
                    calibration_data.append([item["spec"]["data"] for item in items])
                    for index, horizon in enumerate((1, 3, 5)):
                        tab = page.get_by_role("tab", name=f"Horizon {horizon}", exact=True)
                        tab.click()
                        assert tab.get_attribute("aria-selected") == "true"
                        panel = page.locator(f'[data-component-id="calibration-horizon-{index + 1}"]')
                        assert panel.is_visible()
                        assert "Estimated target-event risk" in panel.locator("svg").first.text_content()
                        assert panel.locator("svg circle").count() >= 5

                true_data, inflated_data = calibration_data
                for truth, inflated in zip(true_data, inflated_data, strict=True):
                    assert len(truth) == len(inflated) == 5
                    for true_point, inflated_point in zip(truth, inflated, strict=True):
                        assert true_point["total"] == inflated_point["total"] == 1000
                        assert true_point["observed"] == inflated_point["observed"]
                        assert inflated_point["predicted"] > inflated_point["observed"]
                    true_error = sum(abs(p["predicted"] - p["observed"]) for p in truth)
                    inflated_error = sum(abs(p["predicted"] - p["observed"]) for p in inflated)
                    assert true_error < inflated_error
                assert [rows[2]["observed"] for rows in true_data] == sorted(
                    {rows[2]["observed"] for rows in true_data}
                )

                assert len(console_errors) == 0, f"Console errors: {console_errors}"
                assert len(page_errors) == 0, f"Page errors: {page_errors}"

                browser.close()
                print("ALL TERMINAL OUTCOME ACCEPTANCE CHECKS PASSED SUCCESSFULLY!")
    finally:
        for pth in [acceptance_html, raw_html, multi_html, *calibration_paths]:
            if pth.exists():
                pth.unlink()

if __name__ == "__main__":
    test_terminal_outcome_acceptance()
