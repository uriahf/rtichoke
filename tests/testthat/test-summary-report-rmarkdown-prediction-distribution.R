summary_rmarkdown_test_data <- function() {
  list(
    probs = list("Model A" = seq(0.01, 0.99, length.out = 100)),
    reals = list("Population A" = rep(c(0, 1), 50))
  )
}

test_that("conventional summary report embeds two prediction distribution components when interactive = TRUE", {
  dat <- summary_rmarkdown_test_data()
  output_dir <- tempfile("rtichoke-rmd-summary-")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  out_path <- file.path(output_dir, "summary_report.html")

  create_summary_report(
    probs = dat$probs,
    reals = dat$reals,
    interactive = TRUE,
    output_file = "summary_report.html",
    output_dir = output_dir,
    renderer = "rmarkdown"
  )

  expect_true(file.exists(out_path))

  html_lines <- readLines(out_path, warn = FALSE)
  html_text <- paste(html_lines, collapse = "\n")

  # Exactly two Prediction Distribution <h3> heading blocks
  heading_matches <- length(gregexpr(
    "<h3>Prediction Distribution</h3>",
    html_text,
    fixed = TRUE
  )[[1]])
  expect_equal(heading_matches, 2L)

  # Exactly two Prediction Distribution container roots
  root_matches <- length(gregexpr(
    'class="rtichoke-viz-chart"',
    html_text,
    fixed = TRUE
  )[[1]])
  expect_equal(root_matches, 2L)

  # Spec scripts exist with distinct component IDs (rtichoke-viz-<id>-spec)
  spec_script_ids <- regmatches(
    html_text,
    gregexpr('id="rtichoke-viz-[0-9]+-spec"', html_text)
  )[[1]]
  expect_equal(length(spec_script_ids), 2L)
  expect_equal(length(unique(spec_script_ids)), 2L)

  # Distinct module initialization scripts exist
  module_matches <- length(gregexpr(
    '<script type="module">',
    html_text,
    fixed = TRUE
  )[[1]])
  expect_equal(module_matches, 2L)

  # Subsections relative ordering:
  # "By Probability Threshold" -> "Prediction Distribution" -> "Performance Metrics Curves"
  thresh_sec_pos <- regexpr('id="by-probability-threshold"', html_text)[[1]]
  ppcr_sec_pos <- regexpr(
    'id="by-predicted-positives-condition-rate-ppcr"',
    html_text
  )[[1]]

  expect_true(thresh_sec_pos > 0L)
  expect_true(ppcr_sec_pos > 0L)
  expect_true(thresh_sec_pos < ppcr_sec_pos)

  thresh_chunk <- substr(html_text, thresh_sec_pos, ppcr_sec_pos)
  ppcr_chunk <- substr(html_text, ppcr_sec_pos, nchar(html_text))

  thresh_pred_pos <- regexpr("Prediction Distribution", thresh_chunk)[[1]]
  thresh_curves_pos <- regexpr("Performance Metrics Curves", thresh_chunk)[[1]]
  expect_true(thresh_pred_pos > 0L)
  expect_true(thresh_curves_pos > 0L)
  expect_true(thresh_pred_pos < thresh_curves_pos)

  ppcr_pred_pos <- regexpr("Prediction Distribution", ppcr_chunk)[[1]]
  ppcr_curves_pos <- regexpr("Performance Metrics Curves", ppcr_chunk)[[1]]
  expect_true(ppcr_pred_pos > 0L)
  expect_true(ppcr_curves_pos > 0L)
  expect_true(ppcr_pred_pos < ppcr_curves_pos)

  # Dimensions in spec JSON payloads
  expect_match(
    thresh_chunk,
    '"dimension":"probability_threshold"',
    fixed = TRUE
  )
  expect_match(ppcr_chunk, '"dimension":"ppcr"', fixed = TRUE)

  # Both components use the by = 0.01 operating-point grid (101 points in operatingPoints array)
  expect_match(thresh_chunk, '"value":0.01', fixed = TRUE)
  expect_match(ppcr_chunk, '"value":0.01', fixed = TRUE)

  # HTML remains self-contained (no external network or asset dependencies required)
  expect_match(html_text, "<style", fixed = TRUE)
  expect_match(html_text, "<script", fixed = TRUE)

  # Existing conventional plots and tables remain present
  expect_match(html_text, "plotly", fixed = TRUE)
  expect_match(html_text, "reactable", fixed = TRUE)
  expect_match(html_text, "Performance Metrics Cheat Sheet", fixed = TRUE)
})


test_that("conventional summary report omits prediction distribution components when interactive = FALSE", {
  dat <- summary_rmarkdown_test_data()

  # Note: The complete non-interactive report render is currently blocked by a pre-existing
  # defect in plot_decision_curve(..., interactive = FALSE).
  # We test template execution up through Discrimination to verify Prediction Distribution conditional behavior.
  template <- readLines(
    system.file("summary_report_template.Rmd", package = "rtichoke"),
    warn = FALSE
  )
  utility_idx <- grep("^# Utility", template)
  expect_true(length(utility_idx) > 0L)

  trimmed_template <- template[1:(utility_idx[1] - 1)]
  tmp_rmd <- tempfile(fileext = ".Rmd")
  writeLines(trimmed_template, tmp_rmd)

  tmp_html <- tempfile(fileext = ".html")
  rmarkdown::render(
    tmp_rmd,
    params = list(
      probs = dat$probs,
      reals = dat$reals,
      interactive = FALSE
    ),
    output_file = tmp_html,
    quiet = TRUE
  )

  expect_true(file.exists(tmp_html))

  html_lines <- readLines(tmp_html, warn = FALSE)
  html_text <- paste(html_lines, collapse = "\n")

  # Neither Prediction Distribution heading nor component emitted
  expect_equal(
    sum(grepl("Prediction Distribution", html_lines, fixed = TRUE)),
    0L
  )
  expect_equal(
    sum(grepl("rtichoke-viz-chart", html_lines, fixed = TRUE)),
    0L
  )

  # Existing static threshold and PPCR discrimination content remain present
  expect_match(html_text, "By Probability Threshold", fixed = TRUE)
  expect_match(
    html_text,
    "By Predicted Positives Condition Rate (PPCR)",
    fixed = TRUE
  )
  expect_match(html_text, "Performance Metrics Curves", fixed = TRUE)
})


test_that("conventional summary report preserves public signature defaults and browser report contract", {
  # Public create_summary_report signature
  formals_res <- formals(create_summary_report)
  expect_identical(eval(formals_res$interactive), TRUE)
  expect_identical(eval(formals_res$output_file), "summary_report.html")
  expect_identical(eval(formals_res$renderer), c("rmarkdown", "browser"))

  # Browser report composition stays 18 components
  dat <- summary_rmarkdown_test_data()
  browser_spec <- rtichoke:::summary_report_browser_spec(dat$probs, dat$reals)

  components <- list()
  for (section in browser_spec$sections) {
    for (item in section$items) {
      if (identical(item$type, "component")) {
        components <- c(components, list(item))
      } else {
        components <- c(components, item$components)
      }
    }
  }

  expect_length(components, 18L)
  expect_identical(browser_spec$schemaVersion, "1.1")
})
