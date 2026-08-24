report_browser_specs <- function() {
  probs <- list("Model A" = c(0.1, 0.2, 0.8, 0.9))
  reals <- list("Population A" = c(0, 0, 1, 1))
  performance_data <- prepare_performance_data(probs, reals, by = 0.5)
  metadata <- rtichoke:::build_evaluation_metadata(probs, reals)

  calibration_curve_list <- list(
    deciles_dat = data.frame(
      reference_group = "Model A",
      x = c(0.2, 0.8),
      y = c(0.25, 0.75),
      sum_reals = c(1, 3),
      total_obs = c(4, 4)
    ),
    smooth_dat = data.frame(
      reference_group = "Model A",
      x = c(0.2, 0.8),
      y = c(0.3, 0.7)
    ),
    histogram_for_calibration = data.frame(
      reference_group = "Model A",
      mids = c(0.25, 0.75),
      counts = c(2, 2)
    )
  )

  list(
    performance_table = rtichoke:::rtichoke_viz_performance_table_v2_spec(
      performance_data,
      metadata
    ),
    roc = rtichoke:::rtichoke_viz_roc_v2_spec(
      performance_data,
      metadata
    ),
    calibration = rtichoke:::rtichoke_viz_calibration_v2_spec(
      calibration_curve_list,
      metadata,
      method = "discrete"
    )
  )
}

test_that("browser ReportSpec path delegates complete report to renderReport", {
  specs <- report_browser_specs()
  report <- rtichoke:::rtichoke_viz_report_spec(
    specs$performance_table,
    specs$roc,
    specs$calibration,
    title = "Model performance"
  )
  before <- unserialize(serialize(report, NULL))

  browser <- rtichoke:::render_rtichoke_viz_report_browser(report)
  html <- as.character(browser)
  expected_json <- jsonlite::toJSON(report, auto_unbox = TRUE, digits = NA)

  expect_s3_class(browser, "shiny.tag.list")
  expect_match(html, "renderReport", fixed = TRUE)
  expect_match(html, "rtichoke-viz-0.5.0", fixed = TRUE)
  expect_match(html, expected_json, fixed = TRUE)
  expect_identical(report, before)
  expect_false(grepl("renderRocV2", html, fixed = TRUE))
  expect_false(grepl("renderCalibrationV2", html, fixed = TRUE))
  expect_false(grepl("renderPerformanceTable", html, fixed = TRUE))
})

test_that("browser report preserves component order and deterministic ids", {
  specs <- report_browser_specs()
  report <- rtichoke:::rtichoke_viz_report_spec(
    specs$performance_table,
    specs$roc,
    specs$calibration,
    specs$roc
  )

  expect_identical(
    vapply(report$components, `[[`, "", "id"),
    c("performance-table", "roc", "calibration", "roc-2")
  )

  html <- as.character(rtichoke:::render_rtichoke_viz_report_browser(report))
  positions <- vapply(
    c(
      '"id":"performance-table"',
      '"id":"roc"',
      '"id":"calibration"',
      '"id":"roc-2"'
    ),
    function(pattern) regexpr(pattern, html, fixed = TRUE)[[1]],
    integer(1)
  )
  expect_true(all(positions > 0L))
  expect_true(all(diff(positions) > 0L))
})

test_that("equal evaluation ids remain local across chart components", {
  specs <- report_browser_specs()
  expect_identical(specs$roc$evaluations[[1]]$id, "evaluation-1")
  expect_identical(specs$calibration$evaluations[[1]]$id, "evaluation-1")

  report <- rtichoke:::rtichoke_viz_report_spec(
    specs$roc,
    specs$calibration
  )
  browser <- rtichoke:::render_rtichoke_viz_report_browser(report)
  html <- as.character(browser)

  expect_identical(
    vapply(
      report$components,
      function(component) component$spec$evaluations[[1]]$id,
      character(1)
    ),
    c("evaluation-1", "evaluation-1")
  )
  expect_false("evaluations" %in% names(report))
  expect_equal(lengths(regmatches(
    html,
    gregexpr('"id":"evaluation-1"', html, fixed = TRUE)
  )), 2L)
})

test_that("calibration report component comes from canonical v2 producer", {
  specs <- report_browser_specs()
  calibration <- specs$calibration

  expect_identical(calibration$schemaVersion, "2.0")
  expect_identical(calibration$type, "calibration")
  expect_identical(calibration$evaluations[[1]]$id, "evaluation-1")
  expect_identical(calibration$data[[1]]$method, "discrete")

  report <- rtichoke:::rtichoke_viz_report_spec(calibration)
  expect_identical(report$components[[1]]$spec, calibration)
})
