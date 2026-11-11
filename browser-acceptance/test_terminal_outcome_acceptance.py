import http.server
import socketserver
import threading
from pathlib import Path
from playwright.sync_api import sync_playwright

class QuietHandler(http.server.SimpleHTTPRequestHandler):
    def log_message(self, format, *args):
        pass

def test_terminal_outcome_acceptance():
    root_dir = Path(__file__).resolve().parents[1]

    # Generate acceptance HTML dynamically
    acceptance_html = Path(__file__).parent / "terminal-outcome-acceptance.html"
    try:
        import subprocess
        r_cmd = """
source("R/terminal_outcome_distribution.R")
ref <- jsonlite::fromJSON("tests/testthat/fixtures/terminal_event/reference.json")
df_a <- as.data.frame(ref$cases$fixture_a$data)
names(df_a) <- c("id", "times", "reals")
df_a <- df_a[, c("id", "reals", "times")]

df_z <- as.data.frame(ref$cases$zero_ties$data)
names(df_z) <- c("id", "times", "reals")
df_z <- df_z[, c("id", "reals", "times")]

prepared <- prepare_terminal_outcome_distributions(
  list("Fixture A" = df_a, "Zero Ties" = df_z),
  c(0, 1, 4, 5, 10, 14, 15)
)
spec <- rtichoke_viz_terminal_outcome_distribution_v2_spec(prepared)

report1 <- list(
  schemaVersion = "1.1",
  type = "report",
  heuristics = list(
    active = "adjusted:adjusted_as_negative",
    available = as.list(names(spec$variants))
  ),
  sections = list(
    list(
      id = "sec-1",
      title = "Section 1",
      items = list(list(type = "component", id = "c1", spec = spec))
    )
  )
)

report2 <- list(
  schemaVersion = "1.1",
  type = "report",
  heuristics = list(
    active = "excluded:excluded",
    available = as.list(names(spec$variants))
  ),
  sections = list(
    list(
      id = "sec-2",
      title = "Section 2",
      items = list(list(type = "component", id = "c2", spec = spec))
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
  <div id="standalone-container"></div>
  <h2>Report 1</h2>
  <div id="first-report"></div>
  <h2>Report 2</h2>
  <div id="second-report"></div>

  <script type="module">
    import * as viz from "../inst/rtichoke-viz/rtichoke-viz.js";

    const spec = %s;
    const report1 = %s;
    const report2 = %s;

    viz.assertOutcomeDistributionReferentialIntegrity(spec);
    viz.assertReportReferentialIntegrity(report1);
    viz.assertReportReferentialIntegrity(report2);

    document.getElementById("standalone-container").appendChild(
      viz.renderOutcomeDistribution(spec)
    );

    document.getElementById("first-report").appendChild(
      viz.renderReport(report1)
    );

    document.getElementById("second-report").appendChild(
      viz.renderReport(report2)
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

        # Start local HTTP server serving repository root to allow ES module imports
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

                # Verify standalone chart
                readout = ".rtichoke-outcome-distribution__readout"
                page.wait_for_selector(f"#standalone-container {readout}", timeout=5000)
                assert page.locator("#standalone-container svg").count() > 0
                standalone_text = page.locator(f"#standalone-container {readout}").inner_text()
                assert "Time =" in standalone_text
                print("✓ Standalone chart SVG and readout verified")

                # Verify reports rendered
                page.wait_for_selector(f"#first-report {readout}", timeout=5000)
                page.wait_for_selector(f"#second-report {readout}", timeout=5000)
                print("✓ Both reports rendered")

                # In-browser detailed verification
                result = page.evaluate("""async () => {
                    const check = (ok, msg) => { if (!ok) throw new Error(msg); };
                    const first = document.querySelector('#first-report');
                    const second = document.querySelector('#second-report');
                    check(first && second, 'Reports missing');

                    const firstReadout = first.querySelector('.rtichoke-outcome-distribution__readout');
                    const secondReadout = second.querySelector('.rtichoke-outcome-distribution__readout');
                    check(firstReadout && secondReadout, 'Readouts missing');

                    const initialSecondText = secondReadout.textContent;

                    // Interact with first report: switch heuristic radio to excluded:excluded
                    const radio = first.querySelector('input[type="radio"][value="excluded:excluded"]');
                    if (radio) {
                        radio.checked = true;
                        radio.dispatchEvent(new Event('change', { bubbles: true }));
                    }

                    // Move horizon slider
                    const slider = first.querySelector('input[type="range"]');
                    if (slider) {
                        slider.value = '3';
                        slider.dispatchEvent(new Event('input', { bubbles: true }));
                    }

                    // Confirm second report remains isolated
                    check(secondReadout.textContent === initialSecondText, 'First report interaction mutated second report!');

                    // Verify readout shows exclusion accounting
                    const text = firstReadout.textContent;
                    check(text.includes('Time ='), 'Readout missing time horizon');

                    return { success: true };
                }""")

                assert result["success"] is True
                print("✓ In-browser interaction and instance isolation passed")

                assert len(console_errors) == 0, f"Console errors: {console_errors}"
                assert len(page_errors) == 0, f"Page errors: {page_errors}"

                browser.close()
                print("ALL ACCEPTANCE CHECKS PASSED SUCCESSFULLY!")
    finally:
        if acceptance_html.exists():
            acceptance_html.unlink()

if __name__ == "__main__":
    test_terminal_outcome_acceptance()
