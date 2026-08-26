summary_report_test_data <- function() {
  list(
    probs = list("Model A" = seq(0.01, 0.99, length.out = 100)),
    reals = list("Population A" = rep(c(0, 1), 50))
  )
}

find_headless_browser <- function() {
  candidates <- Sys.which(c(
    "chromium",
    "chromium-browser",
    "google-chrome",
    "google-chrome-stable"
  ))
  candidates <- unname(candidates[nzchar(candidates)])
  for (candidate in candidates) {
    res <- tryCatch(
      suppressWarnings(system2(
        candidate,
        args = "--version",
        stdout = FALSE,
        stderr = FALSE
      )),
      error = function(...) 1L
    )
    if (identical(res, 0L)) {
      return(candidate)
    }
  }
  ""
}


component_contains <- function(dom, component_id, pattern) {
  component_pattern <- paste0(
    'data-component-id="',
    component_id,
    '"(?:(?!</section>).)*',
    pattern
  )
  grepl(component_pattern, dom, perl = TRUE)
}


summary_report_component_types <- c(
  "performance_table",
  "calibration",
  "calibration",
  "roc",
  "precision_recall",
  "gains",
  "lift",
  "decision_curve",
  "interventions_avoided",
  "performance_table",
  "roc",
  "precision_recall",
  "gains",
  "lift"
)


summary_report_component_ids <- c(
  "performance-table",
  "calibration",
  "calibration-2",
  "roc",
  "precision-recall",
  "gains",
  "lift",
  "decision-curve",
  "interventions-avoided",
  "performance-table-2",
  "roc-2",
  "precision-recall-2",
  "gains-2",
  "lift-2"
)


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


test_that("browser summary report composes all canonical static components", {
  dat <- summary_report_test_data()
  report <- rtichoke:::summary_report_browser_spec(dat$probs, dat$reals)

  expect_identical(report$schemaVersion, "1.0")
  expect_identical(report$type, "report")
  expect_identical(report$title, "Summary Report")
  expect_identical(
    vapply(report$components, `[[`, "", "id"),
    summary_report_component_ids
  )
  expect_identical(
    vapply(report$components, function(x) x$spec$type, character(1)),
    summary_report_component_types
  )
  expect_identical(
    vapply(report$components, `[[`, "", "title"),
    c(
      "Performance Table \u2014 Probability Threshold",
      "Calibration \u2014 Smooth",
      "Calibration \u2014 Discrete",
      "ROC \u2014 Probability Threshold",
      "Precision-Recall \u2014 Probability Threshold",
      "Gains \u2014 Probability Threshold",
      "Lift \u2014 Probability Threshold",
      "Decision Curve",
      "Interventions Avoided",
      "Performance Table \u2014 PPCR",
      "ROC \u2014 PPCR",
      "Precision-Recall \u2014 PPCR",
      "Gains \u2014 PPCR",
      "Lift \u2014 PPCR"
    )
  )
  expect_identical(report$components[[2]]$spec$data[[1]]$method, "smooth")
  expect_identical(report$components[[3]]$spec$data[[1]]$method, "discrete")
  expect_length(unique(summary_report_component_ids), 14L)
})


test_that("browser summary report preserves component-local evaluation identity", {
  dat <- summary_report_test_data()
  report <- rtichoke:::summary_report_browser_spec(dat$probs, dat$reals)

  evaluation_ids <- vapply(
    report$components,
    function(component) component$spec$evaluations[[1]]$id,
    character(1)
  )

  expect_identical(evaluation_ids, rep("evaluation-1", 14))
  expect_false("evaluations" %in% names(report))
  expect_identical(
    report$components[[4]]$spec$evaluations[[1]]$population,
    "Population A"
  )
  expect_identical(
    report$components[[3]]$spec$evaluations[[1]]$population,
    "Population A"
  )
})


test_that("browser summary uses authoritative threshold and PPCR data", {
  dat <- summary_report_test_data()
  report <- rtichoke:::summary_report_browser_spec(dat$probs, dat$reals)
  threshold_data <- prepare_performance_data(dat$probs, dat$reals)
  ppcr_data <- prepare_performance_data(
    dat$probs,
    dat$reals,
    stratified_by = "ppcr"
  )

  threshold_table <- report$components[[1]]$spec
  ppcr_table <- report$components[[10]]$spec
  expect_true(all(vapply(
    threshold_table$rows,
    function(row) identical(row$operatingPoint$type, "probability_threshold"),
    logical(1)
  )))
  expect_true(all(vapply(
    ppcr_table$rows,
    function(row) identical(row$operatingPoint$type, "ppcr"),
    logical(1)
  )))
  expect_equal(
    vapply(threshold_table$rows, function(row) row$operatingPoint$value, 0),
    threshold_data$probability_threshold
  )
  expect_equal(
    vapply(ppcr_table$rows, function(row) row$operatingPoint$value, 0),
    ppcr_data$ppcr
  )

  expect_equal(
    vapply(report$components[[4]]$spec$data, `[[`, 0, "cutoff"),
    threshold_data$probability_threshold
  )
  expect_equal(
    vapply(report$components[[11]]$spec$data, `[[`, 0, "cutoff"),
    unname(ppcr_data$probability_threshold)
  )
  expect_equal(
    vapply(report$components[[13]]$spec$data, `[[`, 0, "ppcr"),
    ppcr_data$ppcr
  )
})


test_that("summary report embedding leaves standalone specs unchanged", {
  dat <- summary_report_test_data()
  performance_data <- prepare_performance_data(dat$probs, dat$reals)
  metadata <- rtichoke:::build_evaluation_metadata(dat$probs, dat$reals)
  roc <- rtichoke:::rtichoke_viz_roc_v2_spec(performance_data, metadata)
  decision <- rtichoke:::rtichoke_viz_decision_curve_v2_spec(
    performance_data,
    metadata
  )

  report <- rtichoke:::rtichoke_viz_report_spec(roc, decision)

  expect_identical(report$components[[1]]$spec, roc)
  expect_identical(report$components[[2]]$spec, decision)
  expect_identical(roc, rtichoke:::rtichoke_viz_roc_v2_spec(
    performance_data,
    metadata
  ))
})


test_that("summary report keeps models and populations semantically distinct", {
  probs <- list(
    train = seq(0.01, 0.99, length.out = 100),
    test = seq(0.99, 0.01, length.out = 100)
  )
  reals <- list(
    train = rep(c(0, 1), 50),
    test = rep(c(1, 0), 50)
  )
  report <- rtichoke:::summary_report_browser_spec(probs, reals)

  for (component in report$components) {
    expect_identical(
      vapply(component$spec$evaluations, `[[`, "", "population"),
      c("train", "test")
    )
  }
})


test_that("public browser renderer writes file-safe shared renderReport HTML", {
  output_dir <- tempfile("rtichoke-summary-")

  expect_message(
    create_summary_report(
      probs = list(example_dat$estimated_probabilities),
      reals = list(example_dat$outcome),
      renderer = "browser",
      output_file = "browser_report.html",
      output_dir = output_dir
    ),
    NA
  )

  rendered_file <- file.path(output_dir, "browser_report.html")
  expect_true(file.exists(rendered_file))

  html <- paste(readLines(rendered_file, warn = FALSE), collapse = "\n")
  expect_match(html, "renderReport", fixed = TRUE)
  expect_match(html, "rtichoke-viz-0.6.0", fixed = TRUE)
  expect_match(html, '"id":"performance-table"', fixed = TRUE)
  expect_match(html, '"id":"roc"', fixed = TRUE)
  expect_match(html, '"id":"calibration"', fixed = TRUE)
  expect_match(html, '"id":"precision-recall"', fixed = TRUE)
  expect_match(html, '"id":"decision-curve"', fixed = TRUE)
  expect_match(html, '"id":"interventions-avoided"', fixed = TRUE)
  expect_match(html, '"id":"performance-table-2"', fixed = TRUE)
  expect_false(grepl("import { renderReport } from", html, fixed = TRUE))
  expect_false(grepl(
    'src="lib/rtichoke-viz-0.6.0/rtichoke-viz.js"',
    html,
    fixed = TRUE
  ))
})


test_that("browser output_dir still takes precedence over output_file path", {
  dat <- summary_report_test_data()
  output_dir <- tempfile("rtichoke-summary-path-")

  create_summary_report(
    probs = dat$probs,
    reals = dat$reals,
    renderer = "browser",
    output_file = file.path("ignored-subdir", "browser.html"),
    output_dir = output_dir
  )

  expect_true(file.exists(file.path(output_dir, "browser.html")))
  expect_false(file.exists(file.path(
    output_dir,
    "ignored-subdir",
    "browser.html"
  )))
})


test_that("public browser report renders populated components from a local file", {
  skip_on_os("windows")
  browser <- find_headless_browser()
  skip_if(!nzchar(browser), "No headless Chromium/Chrome available")

  output_dir <- tempfile("rtichoke-summary-browser-")
  create_summary_report(
    probs = list(example_dat$estimated_probabilities),
    reals = list(example_dat$outcome),
    renderer = "browser",
    output_file = "browser_report.html",
    output_dir = output_dir
  )

  rendered_file <- normalizePath(
    file.path(output_dir, "browser_report.html"),
    winslash = "/",
    mustWork = TRUE
  )
  url <- paste0("file://", rendered_file)
  stderr_file <- tempfile("rtichoke-browser-stderr-")
  dom_lines <- system2(
    browser,
    args = c(
      "--headless=new",
      "--no-sandbox",
      "--disable-gpu",
      "--disable-dev-shm-usage",
      "--virtual-time-budget=3000",
      "--dump-dom",
      shQuote(url)
    ),
    stdout = TRUE,
    stderr = stderr_file,
    timeout = 20
  )
  status <- attr(dom_lines, "status")
  dom <- paste(dom_lines, collapse = "\n")
  browser_stderr <- paste(readLines(stderr_file, warn = FALSE), collapse = "\n")

  expect_null(status, info = browser_stderr)
  expect_false(
    grepl(
      "ERROR:CONSOLE|Uncaught|Invalid ReportSpec|ReferenceError|TypeError|SyntaxError",
      browser_stderr,
      perl = TRUE
    ),
    info = browser_stderr
  )

  expect_true(
    component_contains(
      dom,
      "performance-table",
      '<table class="rtichoke-performance-table__table"'
    ),
    info = browser_stderr
  )
  expect_true(
    component_contains(dom, "precision-recall", "<svg"),
    info = browser_stderr
  )
  expect_true(
    component_contains(dom, "decision-curve", "<svg"),
    info = browser_stderr
  )
  expect_true(
    component_contains(dom, "interventions-avoided", "<svg"),
    info = browser_stderr
  )
  expect_true(
    component_contains(
      dom,
      "performance-table-2",
      '<table class="rtichoke-performance-table__table"'
    ),
    info = browser_stderr
  )
})
