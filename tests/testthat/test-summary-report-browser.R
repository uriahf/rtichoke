summary_report_test_data <- function() {
  list(
    probs = list("Model A" = seq(0.01, 0.99, length.out = 100)),
    reals = list("Population A" = rep(c(0, 1), 50))
  )
}


test_that("summary report keeps RMarkdown as the default backend", {
  dat <- summary_report_test_data()
  rmarkdown_called <- FALSE

  testthat::local_mocked_bindings(
    render_summary_report_rmarkdown = function(...) {
      rmarkdown_called <<- TRUE
      invisible(NULL)
    },
    summary_report_browser_spec = function(...) {
      stop("browser backend must not run")
    },
    .package = "rtichoke"
  )

  expect_message(
    create_summary_report(
      probs = dat$probs,
      reals = dat$reals,
      output_file = "legacy.html",
      output_dir = tempdir()
    ),
    NA
  )
  expect_true(rmarkdown_called)
  expect_identical(
    eval(formals(create_summary_report)$renderer),
    c("rmarkdown", "browser")
  )
})


test_that("browser summary report uses real canonical first-report components", {
  dat <- summary_report_test_data()
  report <- rtichoke:::summary_report_browser_spec(dat$probs, dat$reals)

  expect_identical(report$schemaVersion, "1.0")
  expect_identical(report$type, "report")
  expect_identical(report$title, "Summary Report")
  expect_identical(
    vapply(report$components, `[[`, "", "id"),
    c("performance-table", "roc", "calibration")
  )
  expect_identical(
    vapply(report$components, function(x) x$spec$type, character(1)),
    c("performance_table", "roc", "calibration")
  )
  expect_identical(
    vapply(report$components, `[[`, "", "title"),
    c("Performance Table", "ROC", "Calibration")
  )
  expect_identical(report$components[[3]]$spec$data[[1]]$method, "discrete")
})


test_that("browser summary report preserves component-local evaluation identity", {
  dat <- summary_report_test_data()
  report <- rtichoke:::summary_report_browser_spec(dat$probs, dat$reals)

  evaluation_ids <- vapply(
    report$components,
    function(component) component$spec$evaluations[[1]]$id,
    character(1)
  )

  expect_identical(evaluation_ids, rep("evaluation-1", 3))
  expect_false("evaluations" %in% names(report))
  expect_identical(
    report$components[[2]]$spec$evaluations[[1]]$population,
    "Population A"
  )
  expect_identical(
    report$components[[3]]$spec$evaluations[[1]]$population,
    "Population A"
  )
})


test_that("public browser renderer writes shared renderReport HTML", {
  dat <- summary_report_test_data()
  output_dir <- tempfile("rtichoke-summary-")
  output_file <- file.path("ignored-subdir", "browser.html")

  expect_message(
    create_summary_report(
      probs = dat$probs,
      reals = dat$reals,
      output_file = output_file,
      output_dir = output_dir,
      renderer = "browser"
    ),
    NA
  )

  rendered_file <- file.path(output_dir, "browser.html")
  expect_true(file.exists(rendered_file))

  html <- paste(readLines(rendered_file, warn = FALSE), collapse = "\n")
  expect_match(html, "renderReport", fixed = TRUE)
  expect_match(html, "rtichoke-viz-0.5.0", fixed = TRUE)
  expect_match(html, '"id":"performance-table"', fixed = TRUE)
  expect_match(html, '"id":"roc"', fixed = TRUE)
  expect_match(html, '"id":"calibration"', fixed = TRUE)
  expect_false(grepl("renderRocV2", html, fixed = TRUE))
  expect_false(grepl("renderCalibrationV2", html, fixed = TRUE))
  expect_false(grepl("renderPerformanceTable", html, fixed = TRUE))
})
