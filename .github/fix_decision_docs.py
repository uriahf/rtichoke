from pathlib import Path

p = Path("R/decision.R")
text = p.read_text()
text = text.replace(
    "#' @param evaluation_metadata semantic metadata required for the precomputed browser path.\n",
    "",
    1,
)
marker = "#' @inheritParams create_decision_curve\n#'\n#' @examples"
replacement = "#' @inheritParams create_decision_curve\n#' @param evaluation_metadata semantic metadata required for the precomputed browser path.\n#'\n#' @examples"
if marker not in text:
    raise SystemExit("plot roxygen marker not found")
text = text.replace(marker, replacement, 1)
p.write_text(text)
Path(".github/fix_decision_docs.py").unlink()
