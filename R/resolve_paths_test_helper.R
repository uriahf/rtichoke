# Internal test helper for resolving test fixtures and vendored assets
# in both interactive dev and installed R CMD check environments.
.resolve_terminal_event_ref <- function() {
  candidates <- c(
    system.file(
      "tests",
      "testthat",
      "fixtures",
      "terminal_event",
      "reference.json",
      package = "rtichoke"
    ),
    system.file(
      "testthat",
      "fixtures",
      "terminal_event",
      "reference.json",
      package = "rtichoke"
    ),
    tryCatch(
      testthat::test_path("fixtures", "terminal_event", "reference.json"),
      error = function(e) ""
    ),
    tryCatch(
      testthat::test_path("../fixtures", "terminal_event", "reference.json"),
      error = function(e) ""
    ),
    "tests/testthat/fixtures/terminal_event/reference.json",
    "tests/fixtures/terminal_event/reference.json"
  )
  for (cand in candidates) {
    if (nzchar(cand) && file.exists(cand)) {
      return(normalizePath(cand, mustWork = TRUE))
    }
  }
  ""
}

.resolve_rtichoke_viz_js <- function() {
  candidates <- c(
    system.file("rtichoke-viz", "rtichoke-viz.js", package = "rtichoke"),
    tryCatch(
      testthat::test_path("../../inst/rtichoke-viz", "rtichoke-viz.js"),
      error = function(e) ""
    ),
    tryCatch(
      testthat::test_path("../inst/rtichoke-viz", "rtichoke-viz.js"),
      error = function(e) ""
    ),
    "inst/rtichoke-viz/rtichoke-viz.js"
  )
  for (cand in candidates) {
    if (nzchar(cand) && file.exists(cand)) {
      return(normalizePath(cand, mustWork = TRUE))
    }
  }
  ""
}
