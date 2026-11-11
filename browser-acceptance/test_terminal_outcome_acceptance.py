import http.server
import json
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

    try:
        # Prepare data for all 8 combinations, multiple populations including all_censored
        r_cmd = """
source("R/terminal_outcome_distribution.R")
source("tests/testthat/helper-terminal-paths.R")
ref <- jsonlite::fromJSON(resolve_terminal_event_ref())

# Include both all_censored (empty retained population at positive horizons) and zero_ties
df_censored <- as.data.frame(ref$cases$all_censored$data)
names(df_censored) <- c("id", "times", "reals")
df_censored <- df_censored[, c("id", "reals", "times")]

df_ties <- as.data.frame(ref$cases$zero_ties$data)
names(df_ties) <- c("id", "times", "reals")
df_ties <- df_ties[, c("id", "reals", "times")]

prepared <- prepare_terminal_outcome_distributions(
  list("censored" = df_censored, "zero ties" = df_ties),
  c(0, 1, 2, 3)
)
spec <- rtichoke_viz_terminal_outcome_distribution_v2_spec(prepared)

report1 <- list(
  schemaVersion = "1.1",
  type = "report",
  heuristics = list(
    active = "excluded:excluded",
    available = as.list(names(spec$variants))
  ),
  sections = list(
    list(
      id = "outcome-first",
      title = "Outcome First",
      items = list(list(type = "component", id = "od1", spec = spec))
    )
  )
)

report2 <- list(
  schemaVersion = "1.1",
  type = "report",
  heuristics = list(
    active = "adjusted:adjusted_as_negative",
    available = as.list(names(spec$variants))
  ),
  sections = list(
    list(
      id = "outcome-second",
      title = "Outcome Second",
      items = list(list(type = "component", id = "od2", spec = spec))
    )
  )
)

html_content <- sprintf(
'<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <link rel="stylesheet" href="../inst/rtichoke-viz/rtichoke-viz.css">
  <title>Terminal Outcome Acceptance</title>
</head>
<body>
  <h2>Standalone Outcome Distribution</h2>
  <div id="standalone"></div>
  <h2>Report 1</h2>
  <div id="first"></div>
  <h2>Report 2</h2>
  <div id="second"></div>

  <script type="module">
    import * as viz from "../inst/rtichoke-viz/rtichoke-viz.js";

    window.viz = viz;
    window.spec = %s;
    window.report1 = %s;
    window.report2 = %s;

    viz.assertOutcomeDistributionReferentialIntegrity(window.spec);
    viz.assertReportReferentialIntegrity(window.report1);
    viz.assertReportReferentialIntegrity(window.report2);

    document.getElementById("standalone").appendChild(
      viz.renderOutcomeDistribution(window.spec)
    );

    document.getElementById("first").appendChild(
      viz.renderReport(window.report1)
    );

    document.getElementById("second").appendChild(
      viz.renderReport(window.report2)
    );
  </script>
</body>
</html>',
  jsonlite::toJSON(spec, auto_unbox = TRUE, null = "null", digits = 16),
  jsonlite::toJSON(report1, auto_unbox = TRUE, null = "null", digits = 16),
  jsonlite::toJSON(report2, auto_unbox = TRUE, null = "null", digits = 16)
)

writeLines(html_content, "browser-acceptance/terminal-outcome-acceptance.html")
"""
        subprocess.run(["Rscript", "-e", r_cmd], check=True, cwd=root_dir)

        # Start local HTTP server
        with socketserver.TCPServer(("127.0.0.1", 0), lambda *args: QuietHandler(*args, directory=str(root_dir))) as httpd:
            port = httpd.server_address[1]
            t = threading.Thread(target=httpd.serve_forever, daemon=True)
            t.start()

            url = f"http://127.0.0.1:{port}/browser-acceptance/terminal-outcome-acceptance.html"
            print(f"Loading {url}")

            with sync_playwright() as p:
                browser = p.chromium.launch(headless=True)
                page = browser.new_page(viewport={"width": 1200, "height": 900})

                console_errors = []
                page_errors = []
                page.on("console", lambda msg: console_errors.append(msg.text) if msg.type == "error" else None)
                page.on("pageerror", lambda err: page_errors.append(str(err)))

                page.goto(url, wait_until="load")

                readout_sel = ".rtichoke-outcome-distribution__readout"
                page.wait_for_selector(f"#standalone {readout_sel}", timeout=5000)
                page.wait_for_selector(f"#first {readout_sel}", timeout=5000)
                page.wait_for_selector(f"#second {readout_sel}", timeout=5000)
                print("✓ Standalone and Report instances attached")

                # In-browser comprehensive verification matching python test_terminal_outcome_browser_acceptance.py
                result = page.evaluate("""async () => {
                    const viz = window.viz;
                    const spec = window.spec;
                    const check = (ok, msg) => { if (!ok) throw new Error(msg); };

                    const first = document.querySelector('#first');
                    const second = document.querySelector('#second');
                    const readout = root => root.querySelector('.rtichoke-outcome-distribution__readout');

                    const choose = (root, selector) => {
                        const radio = root.querySelector(selector);
                        check(radio, 'missing expected control: ' + selector);
                        radio.checked = true;
                        radio.dispatchEvent(new Event('change', { bubbles: true }));
                    };

                    const horizon = (root, index) => {
                        const slider = root.querySelector('input[type="range"]');
                        check(slider, 'missing slider control');
                        slider.value = String(index);
                        slider.dispatchEvent(new Event('input', { bubbles: true }));
                    };

                    const initialSecond = readout(second).textContent;

                    const verify = (root, dist, exclusion) => {
                        const card = readout(root);
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
                    for (const [key, variant] of Object.entries(spec.variants)) {
                        const [c, k] = key.split(':');
                        choose(first, 'input[name$="-censoring"][value="' + c + '"]');
                        choose(first, 'input[name$="-competing"][value="' + k + '"]');

                        for (const evaluation of spec.evaluations) {
                            choose(first, 'input[value="' + evaluation.id + '"]');
                            const distributions = variant.stateDistributions.filter(d => d.evaluationId === evaluation.id);
                            const exclusion = variant.exclusions.find(e => e.evaluationId === evaluation.id);

                            const {variants, ...standaloneSpec} = spec;
                            const standalone = viz.renderOutcomeDistribution({
                                ...standaloneSpec,
                                evaluations: [evaluation],
                                stateDistributions: distributions,
                                exclusions: [exclusion]
                            });
                            document.body.append(standalone);

                            for (const [index, dist] of distributions.entries()) {
                                horizon(first, index);
                                horizon(standalone, index);

                                verify(first, dist, exclusion);
                                verify(standalone, dist, exclusion);

                                // Confirm second report remains completely isolated
                                check(readout(second).textContent === initialSecond, 'report instance state leaked to second report');
                                contexts++;
                            }
                            standalone.remove();
                        }

                        // Verify horizon preservation across heuristic change
                        horizon(first, 1);
                        choose(first, 'input[name$="-censoring"][value="' + (c === 'excluded' ? 'adjusted' : 'excluded') + '"]');
                        check(readout(first).textContent.includes('Time = 1.000'), 'heuristic change lost horizon');
                    }

                    const ids = [...document.querySelectorAll('[id]')].map(e => e.id);
                    check(new Set(ids).size === ids.length, 'duplicate DOM identities detected');

                    return contexts;
                }""")

                expected_contexts = 8 * 2 * 4
                assert result == expected_contexts, f"Expected {expected_contexts} contexts, got {result}"
                print(f"✓ All {result} contexts verified (8 combinations x 2 populations x 4 horizons)")

                assert len(console_errors) == 0, f"Console errors: {console_errors}"
                assert len(page_errors) == 0, f"Page errors: {page_errors}"

                browser.close()
                print("ALL ACCEPTANCE CHECKS PASSED SUCCESSFULLY!")
    finally:
        if acceptance_html.exists():
            acceptance_html.unlink()

if __name__ == "__main__":
    test_terminal_outcome_acceptance()
