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
  "summary_metrics",
  "calibration",
  "calibration",
  "summary_metrics",
  "roc",
  "lift",
  "precision_recall",
  "gains",
  "roc",
  "lift",
  "precision_recall",
  "gains",
  "decision_curve",
  "interventions_avoided",
  "performance_table",
  "performance_table"
)


summary_report_component_ids <- c(
  "prevalence-summary",
  "calibration-smooth",
  "calibration",
  "auroc",
  "roc",
  "lift",
  "precision-recall",
  "gains",
  "roc-2",
  "lift-2",
  "precision-recall-2",
  "gains-2",
  "decision-curve",
  "interventions-avoided",
  "performance-table",
  "performance-table-2"
)


summary_report_components <- function(report) {
  components <- list()
  for (section in report$sections) {
    for (item in section$items) {
      if (identical(item$type, "component")) {
        components <- c(components, list(item))
      } else {
        components <- c(components, item$components)
      }
    }
  }
  components
}


summary_report_component <- function(report, id) {
  components <- summary_report_components(report)
  components[[match(id, vapply(components, `[[`, "", "id"))]]
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


test_that("summary report keeps explicit RMarkdown rendering unchanged", {
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
      renderer = "rmarkdown",
      output_file = "legacy.html",
      output_dir = tempdir()
    ),
    NA
  )
  expect_true(rmarkdown_called)
})


test_that("browser summary report composes the structured v1.1 hierarchy", {
  dat <- summary_report_test_data()
  report <- rtichoke:::summary_report_browser_spec(dat$probs, dat$reals)
  components <- summary_report_components(report)

  expect_identical(report$schemaVersion, "1.1")
  expect_identical(report$type, "report")
  expect_identical(report$title, "Summary Report")
  expect_identical(
    vapply(report$sections, `[[`, "", "id"),
    c(
      "prevalence",
      "calibration",
      "discrimination",
      "utility",
      "performance-table"
    )
  )
  expect_identical(
    vapply(report$sections, `[[`, "", "title"),
    c(
      "Prevalence",
      "Calibration",
      "Discrimination",
      "Utility",
      "Performance Table"
    )
  )

  prevalence <- report$sections[[1]]
  expect_identical(vapply(prevalence$items, `[[`, "", "type"), "component")
  expect_identical(prevalence$items[[1]]$id, "prevalence-summary")
  expect_identical(prevalence$items[[1]]$title, "Prevalence summary")

  calibration <- report$sections[[2]]
  expect_identical(
    vapply(calibration$items, `[[`, "", "type"),
    rep("component", 2)
  )
  expect_identical(calibration$items[[1]]$id, "calibration-smooth")
  expect_identical(calibration$items[[1]]$title, "Smooth")
  expect_identical(calibration$items[[2]]$id, "calibration")
  expect_identical(calibration$items[[2]]$title, "Discrete")

  discrimination <- report$sections[[3]]
  expect_identical(discrimination$items[[1]]$type, "component")
  expect_identical(discrimination$items[[1]]$id, "auroc")
  expect_identical(discrimination$items[[1]]$title, "AUROC")

  expect_identical(
    vapply(discrimination$items[2:3], `[[`, "", "id"),
    c("discrimination-probability-threshold", "discrimination-ppcr")
  )
  expect_identical(
    vapply(discrimination$items[2:3], `[[`, "", "title"),
    c(
      "By Probability Threshold",
      "By Predicted Positives Condition Rate (PPCR)"
    )
  )
  for (group in discrimination$items[2:3]) {
    expect_identical(
      vapply(group$components, `[[`, "", "title"),
      c("ROC", "Lift", "Precision-Recall", "Gains")
    )
  }
  expect_identical(
    vapply(discrimination$items[[2]]$components, `[[`, "", "id"),
    c("roc", "lift", "precision-recall", "gains")
  )
  expect_identical(
    vapply(discrimination$items[[3]]$components, `[[`, "", "id"),
    c("roc-2", "lift-2", "precision-recall-2", "gains-2")
  )

  utility <- report$sections[[4]]
  expect_identical(
    vapply(utility$items, `[[`, "", "title"),
    c("Decision Curve", "Interventions Avoided")
  )

  tables <- report$sections[[5]]
  expect_identical(
    vapply(tables$items, `[[`, "", "id"),
    c(
      "performance-table-probability-threshold",
      "performance-table-ppcr"
    )
  )
  expect_identical(
    vapply(tables$items, `[[`, "", "title"),
    c(
      "By Probability Threshold",
      "By Predicted Positives Condition Rate (PPCR)"
    )
  )
  expect_identical(
    vapply(tables$items, function(x) x$components[[1]]$title, ""),
    rep("Performance Table", 2)
  )

  expect_identical(
    sort(vapply(components, `[[`, "", "id")),
    sort(summary_report_component_ids)
  )
  expect_identical(
    sort(vapply(components, function(x) x$spec$type, "")),
    sort(summary_report_component_types)
  )
  expect_identical(
    summary_report_component(report, "calibration")$spec$data[[1]]$method,
    "discrete"
  )
  expect_identical(
    summary_report_component(report, "calibration-smooth")$spec$data[[
      1
    ]]$method,
    "smooth"
  )
  expect_true(all(vapply(components, function(x) x$type == "component", TRUE)))
  groups <- c(discrimination$items[2:3], tables$items)
  expect_true(all(vapply(groups, function(x) x$type == "group", TRUE)))
  expect_length(unique(vapply(components, `[[`, "", "id")), 16L)
  expect_length(unique(vapply(report$sections, `[[`, "", "id")), 5L)
  expect_length(unique(vapply(groups, `[[`, "", "id")), 4L)
})


test_that("browser summary report preserves component-local evaluation identity", {
  dat <- summary_report_test_data()
  report <- rtichoke:::summary_report_browser_spec(dat$probs, dat$reals)
  components <- summary_report_components(report)

  components_with_evaluations <- Filter(
    function(component) length(component$spec$evaluations) > 0,
    components
  )

  evaluation_ids <- vapply(
    components_with_evaluations,
    function(component) component$spec$evaluations[[1]]$id,
    character(1)
  )

  expect_identical(
    evaluation_ids,
    rep("evaluation-1", length(components_with_evaluations))
  )
  expect_false("evaluations" %in% names(report))
  expect_identical(
    summary_report_component(report, "roc")$spec$evaluations[[1]]$population,
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

  threshold_table <- summary_report_component(report, "performance-table")$spec
  ppcr_table <- summary_report_component(report, "performance-table-2")$spec
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
    vapply(
      summary_report_component(report, "roc")$spec$data,
      `[[`,
      0,
      "cutoff"
    ),
    threshold_data$probability_threshold
  )
  expect_equal(
    vapply(
      summary_report_component(report, "roc-2")$spec$data,
      `[[`,
      0,
      "cutoff"
    ),
    unname(ppcr_data$probability_threshold)
  )
  expect_equal(
    vapply(
      summary_report_component(report, "gains-2")$spec$data,
      `[[`,
      0,
      "ppcr"
    ),
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
  expect_identical(
    roc,
    rtichoke:::rtichoke_viz_roc_v2_spec(
      performance_data,
      metadata
    )
  )
})


test_that("structured summary embeds the authoritative standalone specs", {
  dat <- summary_report_test_data()
  performance_data <- prepare_performance_data(dat$probs, dat$reals)
  ppcr_data <- prepare_performance_data(
    dat$probs,
    dat$reals,
    stratified_by = "ppcr"
  )
  metadata <- rtichoke:::build_evaluation_metadata(dat$probs, dat$reals)
  calibration_data <- rtichoke:::create_calibration_curve_list(
    dat$probs,
    dat$reals
  )
  ia_data <- rtichoke:::add_static_interventions_avoided_metric(
    performance_data
  )
  report <- rtichoke:::summary_report_browser_spec(dat$probs, dat$reals)

  expected <- list(
    "prevalence-summary" = rtichoke:::rtichoke_viz_summary_metrics_prevalence_spec(
      performance_data,
      metadata
    ),
    "calibration-smooth" = rtichoke:::rtichoke_viz_calibration_v2_spec(
      calibration_data,
      metadata,
      method = "smooth"
    ),
    "calibration" = rtichoke:::rtichoke_viz_calibration_v2_spec(
      calibration_data,
      metadata,
      method = "discrete"
    ),
    "auroc" = rtichoke:::rtichoke_viz_summary_metrics_auroc_spec(
      dat$probs,
      dat$reals,
      metadata
    ),
    "performance-table" = rtichoke:::rtichoke_viz_performance_table_v2_spec(
      performance_data,
      metadata
    ),
    "roc" = rtichoke:::rtichoke_viz_roc_v2_spec(performance_data, metadata),
    "precision-recall" = rtichoke:::rtichoke_viz_precision_recall_v2_spec(
      performance_data,
      metadata
    ),
    "gains" = rtichoke:::rtichoke_viz_gains_v2_spec(
      performance_data,
      metadata
    ),
    "lift" = rtichoke:::rtichoke_viz_lift_v2_spec(
      performance_data,
      metadata
    ),
    "decision-curve" = rtichoke:::rtichoke_viz_decision_curve_v2_spec(
      performance_data,
      metadata
    ),
    "interventions-avoided" = rtichoke:::rtichoke_viz_interventions_avoided_v2_spec(
      ia_data,
      metadata
    ),
    "performance-table-2" = rtichoke:::rtichoke_viz_performance_table_v2_spec(
      ppcr_data,
      metadata,
      stratified_by = "ppcr"
    ),
    "roc-2" = rtichoke:::rtichoke_viz_roc_v2_spec(ppcr_data, metadata),
    "precision-recall-2" = rtichoke:::rtichoke_viz_precision_recall_v2_spec(
      ppcr_data,
      metadata
    ),
    "gains-2" = rtichoke:::rtichoke_viz_gains_v2_spec(ppcr_data, metadata),
    "lift-2" = rtichoke:::rtichoke_viz_lift_v2_spec(ppcr_data, metadata)
  )

  for (id in names(expected)) {
    expect_identical(summary_report_component(report, id)$spec, expected[[id]])
  }
  expect_false(identical(
    summary_report_component(report, "decision-curve")$spec,
    summary_report_component(report, "interventions-avoided")$spec
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

  components_with_evaluations <- Filter(
    function(component) length(component$spec$evaluations) > 0,
    summary_report_components(report)
  )

  for (component in components_with_evaluations) {
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
  expect_match(html, "rtichoke-viz-0.14.0", fixed = TRUE)
  expect_match(html, '"id":"prevalence-summary"', fixed = TRUE)
  expect_match(html, '"id":"calibration-smooth"', fixed = TRUE)
  expect_match(html, '"id":"calibration"', fixed = TRUE)
  expect_match(html, '"id":"auroc"', fixed = TRUE)
  expect_match(html, '"id":"performance-table"', fixed = TRUE)
  expect_match(html, '"id":"roc"', fixed = TRUE)
  expect_match(html, '"id":"precision-recall"', fixed = TRUE)
  expect_match(html, '"id":"decision-curve"', fixed = TRUE)
  expect_match(html, '"id":"interventions-avoided"', fixed = TRUE)
  expect_match(html, '"id":"performance-table-2"', fixed = TRUE)
  expect_false(grepl("import { renderReport } from", html, fixed = TRUE))
  expect_false(grepl(
    'src="lib/rtichoke-viz-0.14.0/rtichoke-viz.js"',
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
      "--allow-file-access-from-files",
      "--disable-gpu",
      "--disable-dev-shm-usage",
      "--virtual-time-budget=5000",
      "--run-all-tasks",
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
      "prevalence-summary",
      "rtichoke-summary-metrics"
    ),
    info = browser_stderr
  )
  expect_true(
    component_contains(
      dom,
      "auroc",
      "rtichoke-summary-metrics"
    ),
    info = browser_stderr
  )
  expect_true(
    component_contains(
      dom,
      "calibration-smooth",
      "<svg"
    ),
    info = browser_stderr
  )
  expect_true(
    component_contains(
      dom,
      "calibration",
      "<svg"
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
})
