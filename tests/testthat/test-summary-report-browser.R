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
  expect_match(html, "rtichoke-viz-0.5.0", fixed = TRUE)
  expect_match(html, '"id":"performance-table"', fixed = TRUE)
  expect_match(html, '"id":"roc"', fixed = TRUE)
  expect_match(html, '"id":"calibration"', fixed = TRUE)
  expect_false(grepl("import { renderReport } from", html, fixed = TRUE))
  expect_false(grepl(
    'src="lib/rtichoke-viz-0.5.0/rtichoke-viz.js"',
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
    component_contains(dom, "roc", "<svg"),
    info = browser_stderr
  )
  expect_true(
    component_contains(dom, "calibration", "<svg"),
    info = browser_stderr
  )
})
