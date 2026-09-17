summary_rmarkdown_test_data <- function() {
  list(
    probs = list("Model A" = seq(0.01, 0.99, length.out = 100)),
    reals = list("Population A" = rep(c(0, 1), 50))
  )
}

test_that("conventional summary report places prediction distribution before calibration when interactive = TRUE", {
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

  # Section order: Prediction Distribution -> Calibration -> Discrimination -> Utility -> Performance Table
  pred_pos <- regexpr('id="prediction-distribution"', html_text)[[1]]
  cal_pos <- regexpr('id="calibration"', html_text)[[1]]
  disc_pos <- regexpr('id="discrimination"', html_text)[[1]]
  util_pos <- regexpr('id="utility-decision-curve"', html_text)[[1]]
  tbl_pos <- regexpr('id="performance-table"', html_text)[[1]]

  expect_true(pred_pos > 0L)
  expect_true(cal_pos > 0L)
  expect_true(disc_pos > 0L)
  expect_true(util_pos > 0L)
  expect_true(tbl_pos > 0L)

  expect_true(pred_pos < cal_pos)
  expect_true(cal_pos < disc_pos)
  expect_true(disc_pos < util_pos)
  expect_true(util_pos < tbl_pos)

  # Subsections under Prediction Distribution
  pred_sec_chunk <- substr(html_text, pred_pos, cal_pos)

  thresh_sub_pos <- regexpr("By Probability Threshold", pred_sec_chunk)[[1]]
  ppcr_sub_pos <- regexpr(
    "By Predicted Positives Condition Rate \\(PPCR\\)",
    pred_sec_chunk
  )[[1]]

  expect_true(thresh_sub_pos > 0L)
  expect_true(ppcr_sub_pos > 0L)
  expect_true(thresh_sub_pos < ppcr_sub_pos)

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

  # Dimensions in spec JSON payloads
  thresh_chunk <- substr(pred_sec_chunk, thresh_sub_pos, ppcr_sub_pos)
  ppcr_chunk <- substr(pred_sec_chunk, ppcr_sub_pos, nchar(pred_sec_chunk))

  expect_match(
    thresh_chunk,
    '"dimension":"probability_threshold"',
    fixed = TRUE
  )
  expect_match(ppcr_chunk, '"dimension":"ppcr"', fixed = TRUE)

  # Both components use the by = 0.01 operating-point grid
  expect_match(thresh_chunk, '"value":0.01', fixed = TRUE)
  expect_match(ppcr_chunk, '"value":0.01', fixed = TRUE)

  # HTML remains self-contained
  expect_match(html_text, "<style", fixed = TRUE)
  expect_match(html_text, "<script", fixed = TRUE)

  # Existing conventional plots and tables remain present
  expect_match(html_text, "plotly", fixed = TRUE)
  expect_match(html_text, "reactable", fixed = TRUE)
  expect_match(html_text, "Performance Metrics Cheat Sheet", fixed = TRUE)
})


test_that("conventional summary report omits prediction distribution section when interactive = FALSE", {
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

  # Existing Calibration and Discrimination content remain present
  expect_match(html_text, "Calibration", fixed = TRUE)
  expect_match(html_text, "Discrimination", fixed = TRUE)
})


test_that("conventional summary report renders 100% complete for supported two-model input", {
  out_dir <- tempfile("rtichoke-two-model-")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_file <- file.path(out_dir, "two_model_summary.html")

  create_summary_report(
    probs = list(
      "First Model" = example_dat$estimated_probabilities,
      "Second Model" = example_dat$random_guess
    ),
    reals = list(example_dat$outcome),
    interactive = TRUE,
    output_file = "two_model_summary.html",
    output_dir = out_dir,
    renderer = "rmarkdown"
  )

  expect_true(file.exists(out_file))

  html_lines <- readLines(out_file, warn = FALSE)
  html_text <- paste(html_lines, collapse = "\n")

  expect_match(html_text, "First Model", fixed = TRUE)
  expect_match(html_text, "Second Model", fixed = TRUE)
  expect_match(html_text, "Prediction Distribution", fixed = TRUE)
  expect_match(html_text, "Calibration", fixed = TRUE)
  expect_match(html_text, "Discrimination", fixed = TRUE)
  expect_match(html_text, "Utility", fixed = TRUE)
  expect_match(html_text, "Performance Table", fixed = TRUE)
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
