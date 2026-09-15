summary_report_test_data <- function() {
  list(
    probs = list("Model A" = seq(0.01, 0.99, length.out = 100)),
    reals = list("Population A" = rep(c(0, 1), 50))
  )
}

find_headless_browser <- function() {
  if (isTRUE(as.logical(Sys.getenv("NOT_CRAN", "false")))) {
    return("")
  }
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
    '"(?:(?!</section>)[\\s\\S])*?',
    pattern
  )
  grepl(component_pattern, dom, perl = TRUE)
}


summary_report_component_types <- c(
  "summary_metrics",
  "calibration",
  "calibration",
  "summary_metrics",
  "prediction_distribution",
  "roc",
  "lift",
  "precision_recall",
  "gains",
  "prediction_distribution",
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
  "prediction-distribution",
  "roc",
  "lift",
  "precision-recall",
  "gains",
  "prediction-distribution-2",
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
      c("Prediction Distribution", "ROC", "Lift", "Precision-Recall", "Gains")
    )
  }
  expect_identical(
    vapply(discrimination$items[[2]]$components, `[[`, "", "id"),
    c("prediction-distribution", "roc", "lift", "precision-recall", "gains")
  )
  expect_identical(
    vapply(discrimination$items[[3]]$components, `[[`, "", "id"),
    c("prediction-distribution-2", "roc-2", "lift-2", "precision-recall-2", "gains-2")
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
  expect_length(unique(vapply(components, `[[`, "", "id")), 18L)
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

  threshold_dist_data <- prepare_probs_distribution_data(
    dat$probs,
    dat$reals,
    by = 0.01,
    stratified_by = "probability_threshold"
  )
  ppcr_dist_data <- prepare_probs_distribution_data(
    dat$probs,
    dat$reals,
    by = 0.01,
    stratified_by = "ppcr"
  )

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
    "prediction-distribution" = rtichoke:::rtichoke_viz_prediction_distribution_spec(
      distribution_data = threshold_dist_data,
      performance_data = performance_data
    ),
    "performance-table" = rtichoke:::rtichoke_viz_performance_table_v2_spec(
      performance_data,
      metadata
    ),
    "roc" = rtichoke:::rtichoke_viz_roc_v2_spec(
      performance_data,
      metadata,
      operating_point = "probability_threshold"
    ),
    "precision-recall" = rtichoke:::rtichoke_viz_precision_recall_v2_spec(
      performance_data,
      metadata,
      operating_point = "probability_threshold"
    ),
    "gains" = rtichoke:::rtichoke_viz_gains_v2_spec(
      performance_data,
      metadata,
      operating_point = "probability_threshold"
    ),
    "lift" = rtichoke:::rtichoke_viz_lift_v2_spec(
      performance_data,
      metadata,
      operating_point = "probability_threshold"
    ),
    "decision-curve" = rtichoke:::rtichoke_viz_decision_curve_v2_spec(
      performance_data,
      metadata
    ),
    "interventions-avoided" = rtichoke:::rtichoke_viz_interventions_avoided_v2_spec(
      ia_data,
      metadata
    ),
    "prediction-distribution-2" = rtichoke:::rtichoke_viz_prediction_distribution_spec(
      distribution_data = ppcr_dist_data,
      performance_data = ppcr_data
    ),
    "performance-table-2" = rtichoke:::rtichoke_viz_performance_table_v2_spec(
      ppcr_data,
      metadata,
      stratified_by = "ppcr"
    ),
    "roc-2" = rtichoke:::rtichoke_viz_roc_v2_spec(
      ppcr_data,
      metadata,
      operating_point = "ppcr"
    ),
    "precision-recall-2" = rtichoke:::rtichoke_viz_precision_recall_v2_spec(
      ppcr_data,
      metadata,
      operating_point = "ppcr"
    ),
    "gains-2" = rtichoke:::rtichoke_viz_gains_v2_spec(
      ppcr_data,
      metadata,
      operating_point = "ppcr"
    ),
    "lift-2" = rtichoke:::rtichoke_viz_lift_v2_spec(
      ppcr_data,
      metadata,
      operating_point = "ppcr"
    )
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
  expect_match(html, ".rtichoke-report", fixed = TRUE)
  expect_match(html, ".rtichoke-viz-chart", fixed = TRUE)
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
    'rtichoke-viz.js',
    html,
    fixed = TRUE
  ))
  expect_false(grepl(
    'rtichoke-viz.css',
    html,
    fixed = TRUE
  ))

  # Verify no dependency directory was created
  output_files <- list.files(output_dir, recursive = TRUE)
  expect_identical(output_files, "browser_report.html")
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

  # Check tab structure & accessibility attributes
  expect_match(dom, 'role="tablist"', fixed = TRUE)
  expect_match(dom, 'role="tab"', fixed = TRUE)
  expect_match(dom, 'role="tabpanel"', fixed = TRUE)
  expect_match(dom, 'aria-selected="true"', fixed = TRUE)
  expect_match(dom, 'aria-controls=', fixed = TRUE)

  # Calibration tabs: Smooth and Discrete in correct order
  smooth_pos <- regexpr('id="tab-calibration-calibration-smooth"', dom)[[1]]
  discrete_pos <- regexpr('id="tab-calibration-calibration"', dom)[[1]]
  expect_true(smooth_pos > 0L)
  expect_true(discrete_pos > 0L)
  expect_true(smooth_pos < discrete_pos)

  # Discrimination group tabs & curve tabs in exact order ROC -> Lift -> Precision-Recall -> Gains
  disc_thresh_pos <- regexpr(
    'id="section-group-tab-discrimination-discrimination-probability-threshold"',
    dom
  )[[1]]
  disc_ppcr_pos <- regexpr(
    'id="section-group-tab-discrimination-discrimination-ppcr"',
    dom
  )[[1]]
  expect_true(disc_thresh_pos > 0L)
  expect_true(disc_ppcr_pos > 0L)

  roc_pos <- regexpr('id="tab-discrimination-probability-threshold-roc"', dom)[[
    1
  ]]
  lift_pos <- regexpr(
    'id="tab-discrimination-probability-threshold-lift"',
    dom
  )[[1]]
  pr_pos <- regexpr(
    'id="tab-discrimination-probability-threshold-precision-recall"',
    dom
  )[[1]]
  gains_pos <- regexpr(
    'id="tab-discrimination-probability-threshold-gains"',
    dom
  )[[1]]

  expect_true(roc_pos > 0L)
  expect_true(lift_pos > 0L)
  expect_true(pr_pos > 0L)
  expect_true(gains_pos > 0L)
  expect_true(roc_pos < lift_pos)
  expect_true(lift_pos < pr_pos)
  expect_true(pr_pos < gains_pos)

  # Utility direct sibling tabs
  dc_pos <- regexpr('id="tab-utility-decision-curve"', dom)[[1]]
  ia_pos <- regexpr('id="tab-utility-interventions-avoided"', dom)[[1]]
  expect_true(dc_pos > 0L)
  expect_true(ia_pos > 0L)
  expect_true(dc_pos < ia_pos)

  # Density layout styles assertions
  expect_match(dom, "max-width: 1040px;", fixed = TRUE)
  expect_match(dom, "min-height: 500px;", fixed = TRUE)
  expect_match(dom, "min-height: 550px;", fixed = TRUE)
  expect_match(
    dom,
    ".rtichoke-report__tabpanel .rtichoke-report__component-title",
    fixed = TRUE
  )
  expect_match(dom, ".rtichoke-report .rtichoke-summary-metrics", fixed = TRUE)
})


test_that("browser summary report exposes expandable Confusion Matrix detail for threshold and PPCR tables", {
  skip_on_os("windows")
  browser <- find_headless_browser()
  skip_if(!nzchar(browser), "No headless Chromium/Chrome available")

  output_dir <- tempfile("rtichoke-summary-confusion-")
  create_summary_report(
    probs = list("Model A" = c(0.1, 0.2, 0.8, 0.9)),
    reals = list("Population A" = c(0, 0, 1, 1)),
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

  js_script <- '
    (() => {
      const component = id => document.querySelector(`[data-component-id="${id}"]`);
      const threshTable = component("performance-table");
      const ppcrTable = component("performance-table-2");

      if (!threshTable || !ppcrTable) return "TABLES_NOT_FOUND";

      const testTable = (tableNode, expectedOpType) => {
        const toggleBtn = tableNode.querySelector(".rtichoke-performance-table__toggle-btn");
        if (!toggleBtn) return "TOGGLE_BTN_NOT_FOUND";

        const detailContainer = tableNode.querySelector(".rtichoke-performance-table__confusion-container");
        if (!detailContainer) return "DETAIL_CONTAINER_NOT_FOUND";

        const titleNode = detailContainer.querySelector(".rtichoke-performance-table__confusion-title");
        const titleText = titleNode ? titleNode.textContent : "";

        const opType = detailContainer.getAttribute("data-operating-point-type");

        const detailRow = tableNode.querySelector(".rtichoke-performance-table__detail-row");
        const initialHidden = detailRow ? detailRow.hidden : null;

        toggleBtn.click();
        const expandedHidden = detailRow ? detailRow.hidden : null;

        return {
          hasToggleBtn: true,
          opType: opType,
          titleText: titleText,
          initialHidden: initialHidden,
          expandedHidden: expandedHidden
        };
      };

      return JSON.stringify({
        threshold: testTable(threshTable, "probability_threshold"),
        ppcr: testTable(ppcrTable, "ppcr")
      });
    })()
  '

  node_script <- sprintf(
    '
    const puppeteer = require("puppeteer");
    (async () => {
      const browser = await puppeteer.launch({ headless: "new", args: ["--no-sandbox", "--disable-gpu", "--allow-file-access-from-files"] });
      const page = await browser.newPage();
      await page.goto("%s");
      await page.waitForSelector("[data-component-id=\'performance-table\']");
      const result = await page.evaluate(() => {
        const component = id => document.querySelector(`[data-component-id="${id}"]`);
        const threshTable = component("performance-table");
        const ppcrTable = component("performance-table-2");
        if (!threshTable || !ppcrTable) return "TABLES_NOT_FOUND";

        const testTable = (tableNode) => {
          const toggleBtn = tableNode.querySelector(".rtichoke-performance-table__toggle-btn");
          if (!toggleBtn) return { hasToggleBtn: false };

          const detailContainer = tableNode.querySelector(".rtichoke-performance-table__confusion-container");
          if (!detailContainer) return { hasToggleBtn: true, hasDetail: false };

          const titleNode = detailContainer.querySelector(".rtichoke-performance-table__confusion-title");
          const titleText = titleNode ? titleNode.textContent : "";
          const opType = detailContainer.getAttribute("data-operating-point-type");
          const detailRow = tableNode.querySelector(".rtichoke-performance-table__detail-row");
          const initialHidden = detailRow ? detailRow.hidden : null;

          toggleBtn.click();
          const expandedHidden = detailRow ? detailRow.hidden : null;

          return {
            hasToggleBtn: true,
            hasDetail: true,
            opType: opType,
            titleText: titleText,
            initialHidden: initialHidden,
            expandedHidden: expandedHidden
          };
        };

        return {
          threshold: testTable(threshTable),
          ppcr: testTable(ppcrTable)
        };
      });
      console.log(JSON.stringify(result));
      await browser.close();
    })();
  ',
    url
  )

  dom_lines <- system2(
    browser,
    args = c(
      "--headless=new",
      "--no-sandbox",
      "--allow-file-access-from-files",
      "--disable-gpu",
      "--disable-dev-shm-usage",
      "--virtual-time-budget=5000",
      "--dump-dom",
      shQuote(url)
    ),
    stdout = TRUE,
    stderr = stderr_file,
    timeout = 20
  )
  dom <- paste(dom_lines, collapse = "\n")

  expect_match(dom, "rtichoke-performance-table__toggle-btn", fixed = TRUE)
  expect_match(
    dom,
    "rtichoke-performance-table__confusion-container",
    fixed = TRUE
  )
  expect_match(dom, "rtichoke-performance-table__confusion-title", fixed = TRUE)
  expect_match(
    dom,
    '<div class="rtichoke-performance-table__confusion-title">Confusion Matrix</div>',
    fixed = TRUE
  )
  expect_false(grepl(
    '<div class="rtichoke-performance-table__confusion-title">Estimated Confusion Matrix</div>',
    dom,
    fixed = TRUE
  ))
  expect_match(
    dom,
    'data-operating-point-type="probability_threshold"',
    fixed = TRUE
  )
  expect_match(dom, 'data-operating-point-type="ppcr"', fixed = TRUE)
})


test_that("browser summary report omits confusion detail when confusion metrics are incomplete", {
  incomplete_spec <- list(
    schemaVersion = "2.0",
    type = "performance_table",
    evaluations = list(list(
      id = "evaluation-1",
      population = "Pop A",
      model = "Model A"
    )),
    metrics = list(
      list(id = "sensitivity", label = "Sensitivity", format = "decimal")
    ),
    rows = list(
      list(
        evaluationId = "evaluation-1",
        operatingPoint = list(type = "probability_threshold", value = 0.5),
        values = list(
          list(metricId = "sensitivity", estimate = 0.8)
        )
      )
    )
  )

  report <- rtichoke:::rtichoke_viz_report_spec(incomplete_spec)
  report_widget <- rtichoke:::render_rtichoke_viz_report_browser(report)
  report_html <- as.character(report_widget)
  expect_match(report_html, '"type":"performance_table"', fixed = TRUE)
})


test_that("browser summary report includes the Performance Metrics Cheat Sheet", {
  dat <- summary_report_test_data()
  output_dir <- tempfile("rtichoke-summary-cheatsheet-")

  create_summary_report(
    probs = dat$probs,
    reals = dat$reals,
    renderer = "browser",
    output_file = "browser_report.html",
    output_dir = output_dir
  )

  rendered_file <- file.path(output_dir, "browser_report.html")
  expect_true(file.exists(rendered_file))

  html <- paste(readLines(rendered_file, warn = FALSE), collapse = "\n")

  cheat_sheet_occurrences <- length(gregexpr(
    "Performance Metrics Cheat Sheet",
    html
  )[[1]])
  expect_equal(cheat_sheet_occurrences, 1)

  expect_match(
    html,
    "<details class=\\\"rtichoke-cheat-sheet\\\">",
    fixed = TRUE
  )
  expect_match(
    html,
    "<summary>Performance Metrics Cheat Sheet<\\/summary>",
    fixed = TRUE
  )

  # Confusion matrix labels
  expect_match(html, "Real Positive", fixed = TRUE)
  expect_match(html, "Real Negative", fixed = TRUE)
  expect_match(html, "Predicted +", fixed = TRUE)
  expect_match(html, "Predicted -", fixed = TRUE)
  expect_match(html, "<td>TP<\\/td>", fixed = TRUE)
  expect_match(html, "<td>FP<\\/td>", fixed = TRUE)
  expect_match(html, "<td>TN<\\/td>", fixed = TRUE)
  expect_match(html, "<td>FN<\\/td>", fixed = TRUE)

  # Metric names
  expect_match(html, "<dt>Prevalence<\\/dt>", fixed = TRUE)
  expect_match(html, "<dt>PPCR<\\/dt>", fixed = TRUE)
  expect_match(html, "Sensitivity", fixed = TRUE)
  expect_match(html, "Specificity", fixed = TRUE)
  expect_match(html, "Precision", fixed = TRUE)
  expect_match(html, "<dt>NPV<\\/dt>", fixed = TRUE)
  expect_match(html, "<dt>Lift<\\/dt>", fixed = TRUE)
  expect_match(html, "<dt>Net Benefit<\\/dt>", fixed = TRUE)

  # Metric formulas
  expect_match(html, "(TP + FN) / (TP + FP + TN + FN)", fixed = TRUE)
  expect_match(html, "(TP + FP) / (TP + FP + TN + FN)", fixed = TRUE)
  expect_match(html, "TP / (TP + FN)", fixed = TRUE)
  expect_match(html, "TP / Real Positives", fixed = TRUE)
  expect_match(html, "P(Predicted Positive | Real Positive)", fixed = TRUE)
  expect_match(html, "TN / (TN + FP)", fixed = TRUE)
  expect_match(html, "TN / Real Negatives", fixed = TRUE)
  expect_match(html, "P(Predicted Negative | Real Negative)", fixed = TRUE)
  expect_match(html, "TP / (TP + FP)", fixed = TRUE)
  expect_match(html, "TP / Predicted Positives", fixed = TRUE)
  expect_match(html, "P(Real Positive | Predicted Positive)", fixed = TRUE)
  expect_match(html, "TN / (TN + FN)", fixed = TRUE)
  expect_match(html, "TN / Predicted Negatives", fixed = TRUE)
  expect_match(html, "P(Real Negative | Predicted Negative)", fixed = TRUE)
  expect_match(html, "PPV / Prevalence", fixed = TRUE)
  expect_match(html, "TP / N - FP / N * p_t / (1 - p_t)", fixed = TRUE)
  expect_match(html, "N = TP + FP + TN + FN", fixed = TRUE)

  # DOM placement relative ordering
  expect_match(
    html,
    "insertBefore(cheatSheetNode, headerNode.nextSibling)",
    fixed = TRUE
  )

  # Presentation options
  expect_match(
    html,
    "sectionGroupPresentation: 'tabs'",
    fixed = TRUE
  )
  expect_match(
    html,
    "groupPresentation: 'tabs'",
    fixed = TRUE
  )
  expect_match(
    html,
    "sectionComponentPresentation: 'tabs'",
    fixed = TRUE
  )

  browser <- find_headless_browser()
  if (nzchar(browser)) {
    rendered_file_norm <- normalizePath(
      rendered_file,
      winslash = "/",
      mustWork = TRUE
    )
    url <- paste0("file://", rendered_file_norm)
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
        "--dump-dom",
        shQuote(url)
      ),
      stdout = TRUE,
      stderr = stderr_file,
      timeout = 20
    )
    dom <- paste(dom_lines, collapse = "\n")

    header_pos <- regexpr("class=\"rtichoke-report__header\"", dom)[1]
    cs_pos <- regexpr("class=\"rtichoke-cheat-sheet\"", dom)[1]
    nav_pos <- regexpr("class=\"rtichoke-report__nav\"", dom)[1]

    expect_true(header_pos > 0, info = "Header node found")
    expect_true(cs_pos > 0, info = "Cheat sheet node found")
    expect_true(nav_pos > 0, info = "Nav node found")

    expect_true(
      header_pos < cs_pos,
      info = "Cheat sheet is placed AFTER header"
    )
    expect_true(cs_pos < nav_pos, info = "Cheat sheet is placed BEFORE nav")
  }
})

test_that("browser summary report prediction distribution components match standalone spec identity", {
  dat <- summary_report_test_data()
  report <- rtichoke:::summary_report_browser_spec(dat$probs, dat$reals)

  # Threshold standalone vs embedded using report default grid by = 0.01
  thresh_dist_data <- prepare_probs_distribution_data(
    dat$probs,
    dat$reals,
    by = 0.01,
    stratified_by = "probability_threshold"
  )
  thresh_perf_data <- prepare_performance_data(
    dat$probs,
    dat$reals,
    by = 0.01,
    stratified_by = "probability_threshold"
  )
  standalone_thresh_spec <- rtichoke:::rtichoke_viz_prediction_distribution_spec(
    distribution_data = thresh_dist_data,
    performance_data = thresh_perf_data
  )
  embedded_thresh_spec <- summary_report_component(report, "prediction-distribution")$spec

  expect_identical(embedded_thresh_spec$schemaVersion, standalone_thresh_spec$schemaVersion)
  expect_identical(embedded_thresh_spec$type, standalone_thresh_spec$type)
  expect_identical(embedded_thresh_spec$evaluations, standalone_thresh_spec$evaluations)
  expect_identical(embedded_thresh_spec$operatingPoint, standalone_thresh_spec$operatingPoint)
  expect_identical(embedded_thresh_spec$bins, standalone_thresh_spec$bins)
  expect_identical(embedded_thresh_spec$rankBins, standalone_thresh_spec$rankBins)
  expect_identical(embedded_thresh_spec$operatingPoints, standalone_thresh_spec$operatingPoints)
  expect_identical(embedded_thresh_spec, standalone_thresh_spec)

  # PPCR standalone vs embedded using report default grid by = 0.01
  ppcr_dist_data <- prepare_probs_distribution_data(
    dat$probs,
    dat$reals,
    by = 0.01,
    stratified_by = "ppcr"
  )
  ppcr_perf_data <- prepare_performance_data(
    dat$probs,
    dat$reals,
    by = 0.01,
    stratified_by = "ppcr"
  )
  standalone_ppcr_spec <- rtichoke:::rtichoke_viz_prediction_distribution_spec(
    distribution_data = ppcr_dist_data,
    performance_data = ppcr_perf_data
  )
  embedded_ppcr_spec <- summary_report_component(report, "prediction-distribution-2")$spec

  expect_identical(embedded_ppcr_spec$schemaVersion, standalone_ppcr_spec$schemaVersion)
  expect_identical(embedded_ppcr_spec$type, standalone_ppcr_spec$type)
  expect_identical(embedded_ppcr_spec$evaluations, standalone_ppcr_spec$evaluations)
  expect_identical(embedded_ppcr_spec$operatingPoint, standalone_ppcr_spec$operatingPoint)
  expect_identical(embedded_ppcr_spec$bins, standalone_ppcr_spec$bins)
  expect_identical(embedded_ppcr_spec$rankBins, standalone_ppcr_spec$rankBins)
  expect_identical(embedded_ppcr_spec$operatingPoints, standalone_ppcr_spec$operatingPoints)
  expect_identical(embedded_ppcr_spec, standalone_ppcr_spec)
})


test_that("browser summary report prediction distribution satisfies frozen tied fixture oracle", {
  tied_probs <- list("Model Tied" = c(0.00, 0.15, 0.30, 0.50, 0.50, 0.50, 0.65, 0.80, 1.00))
  tied_reals <- list("Pop Tied" = c(0, 1, 0, 1, 0, 1, 1, 0, 1))

  # Test internal producer/spec for PPCR with by = 0.20
  ppcr_dist_data <- prepare_probs_distribution_data(
    probs = tied_probs,
    reals = tied_reals,
    by = 0.20,
    stratified_by = "ppcr"
  )
  ppcr_perf_data <- prepare_performance_data(
    probs = tied_probs,
    reals = tied_reals,
    by = 0.20,
    stratified_by = "ppcr"
  )

  ppcr_spec <- rtichoke:::rtichoke_viz_prediction_distribution_spec(
    distribution_data = ppcr_dist_data,
    performance_data = ppcr_perf_data
  )

  # Check rank bins against frozen literal expected values
  expected_rank_bins <- list(
    list(evaluationId = "evaluation-1", rankLower = 0.00, rankUpper = 0.20, positiveMass = 1L, negativeMass = 1L),
    list(evaluationId = "evaluation-1", rankLower = 0.20, rankUpper = 0.40, positiveMass = 2L, negativeMass = 2L),
    list(evaluationId = "evaluation-1", rankLower = 0.40, rankUpper = 0.60, positiveMass = 0L, negativeMass = 0L),
    list(evaluationId = "evaluation-1", rankLower = 0.60, rankUpper = 0.80, positiveMass = 1L, negativeMass = 0L),
    list(evaluationId = "evaluation-1", rankLower = 0.80, rankUpper = 1.00, positiveMass = 1L, negativeMass = 1L)
  )
  expect_identical(ppcr_spec$rankBins, expected_rank_bins)

  # Check PPCR operating points against frozen literal expected values
  expected_ops <- list(
    list(value = 0.00, cutoff = 1.00, realizedPpcr = 0 / 9, tp = 0L, fp = 0L, tn = 4L, fn = 5L),
    list(value = 0.20, cutoff = 0.71, realizedPpcr = 2 / 9, tp = 1L, fp = 1L, tn = 3L, fn = 4L),
    list(value = 0.40, cutoff = 0.50, realizedPpcr = 3 / 9, tp = 2L, fp = 1L, tn = 3L, fn = 3L),
    list(value = 0.60, cutoff = 0.50, realizedPpcr = 3 / 9, tp = 2L, fp = 1L, tn = 3L, fn = 3L),
    list(value = 0.80, cutoff = 0.24, realizedPpcr = 7 / 9, tp = 4L, fp = 3L, tn = 1L, fn = 1L),
    list(value = 1.00, cutoff = 0.00, realizedPpcr = 9 / 9, tp = 5L, fp = 4L, tn = 0L, fn = 0L)
  )

  expect_length(ppcr_spec$operatingPoints, length(expected_ops))
  for (i in seq_along(expected_ops)) {
    op <- ppcr_spec$operatingPoints[[i]]
    exp_op <- expected_ops[[i]]

    expect_equal(op$value, exp_op$value, tolerance = 1e-4)
    expect_equal(op$cutoff, exp_op$cutoff, tolerance = 1e-2)
    expect_equal(op$realizedPpcr, exp_op$realizedPpcr, tolerance = 1e-4)

    metrics <- stats::setNames(
      lapply(op$performance, `[[`, "estimate"),
      vapply(op$performance, `[[`, "", "metricId")
    )

    expect_identical(metrics$true_positives, exp_op$tp)
    expect_identical(metrics$false_positives, exp_op$fp)
    expect_identical(metrics$true_negatives, exp_op$tn)
    expect_identical(metrics$false_negatives, exp_op$fn)
  }
})


test_that("browser summary report prediction distribution preserves producer-owned metrics", {
  dat <- summary_report_test_data()
  report <- rtichoke:::summary_report_browser_spec(dat$probs, dat$reals)

  thresh_spec <- summary_report_component(report, "prediction-distribution")$spec
  perf_data <- prepare_performance_data(dat$probs, dat$reals)

  # Check first operating point metrics match producer performance data exactly
  op_1 <- thresh_spec$operatingPoints[[1]]
  perf_row_1 <- perf_data[1, ]

  metrics_1 <- stats::setNames(
    lapply(op_1$performance, `[[`, "estimate"),
    vapply(op_1$performance, `[[`, "", "metricId")
  )

  metric_cols <- list(
    true_positives = "TP",
    true_negatives = "TN",
    false_positives = "FP",
    false_negatives = "FN",
    sensitivity = "sensitivity",
    specificity = "specificity",
    ppv = "PPV",
    npv = "NPV",
    lift = "lift"
  )

  for (metric_id in names(metric_cols)) {
    raw_col <- metric_cols[[metric_id]]
    raw_val <- perf_row_1[[raw_col]][[1]]
    if (is.null(raw_val) || is.na(raw_val) || !is.finite(raw_val)) {
      expect_null(metrics_1[[metric_id]])
    } else if (metric_id %in% c("true_positives", "true_negatives", "false_positives", "false_negatives")) {
      expect_identical(metrics_1[[metric_id]], as.integer(raw_val))
    } else {
      expect_equal(metrics_1[[metric_id]], as.numeric(raw_val))
    }
  }
})


test_that("browser summary report prediction distribution supports multiple evaluations", {
  probs <- list(
    "Model 1" = seq(0.1, 0.9, length.out = 50),
    "Model 2" = seq(0.2, 0.8, length.out = 50)
  )
  reals <- list(
    "Pop 1" = rep(c(0, 1), 25)
  )

  report <- rtichoke:::summary_report_browser_spec(probs, reals)
  thresh_spec <- summary_report_component(report, "prediction-distribution")$spec

  expect_length(thresh_spec$evaluations, 2L)
  expect_identical(thresh_spec$evaluations[[1]]$id, "evaluation-1")
  expect_identical(thresh_spec$evaluations[[1]]$model, "Model 1")
  expect_identical(thresh_spec$evaluations[[1]]$population, "Pop 1")
  expect_identical(thresh_spec$evaluations[[2]]$id, "evaluation-2")
  expect_identical(thresh_spec$evaluations[[2]]$model, "Model 2")
  expect_identical(thresh_spec$evaluations[[2]]$population, "Pop 1")

  eval_ids_in_bins <- unique(vapply(thresh_spec$bins, `[[`, "", "evaluationId"))
  expect_setequal(eval_ids_in_bins, c("evaluation-1", "evaluation-2"))

  eval_ids_in_rank_bins <- unique(vapply(thresh_spec$rankBins, `[[`, "", "evaluationId"))
  expect_setequal(eval_ids_in_rank_bins, c("evaluation-1", "evaluation-2"))

  eval_ids_in_ops <- unique(vapply(thresh_spec$operatingPoints, `[[`, "", "evaluationId"))
  expect_setequal(eval_ids_in_ops, c("evaluation-1", "evaluation-2"))
})


test_that("browser summary report and prediction distribution component specs pass authoritative schema validation", {
  skip_if_not_installed("jsonvalidate")

  dat <- summary_report_test_data()
  report_spec <- rtichoke:::summary_report_browser_spec(dat$probs, dat$reals)

  report_schema_path <- system.file("rtichoke-viz", "rtichoke-viz-report.schema.json", package = "rtichoke")
  v2_schema_path <- system.file("rtichoke-viz", "rtichoke-viz-v2.schema.json", package = "rtichoke")

  expect_true(file.exists(report_schema_path))
  expect_true(file.exists(v2_schema_path))

  report_validator <- jsonvalidate::json_validator(report_schema_path)
  v2_validator <- jsonvalidate::json_validator(v2_schema_path, engine = "ajv")

  report_json <- jsonlite::toJSON(report_spec, auto_unbox = TRUE, null = "null", digits = NA)
  expect_true(report_validator(report_json, verbose = TRUE))

  thresh_component_spec <- summary_report_component(report_spec, "prediction-distribution")$spec
  thresh_json <- jsonlite::toJSON(thresh_component_spec, auto_unbox = TRUE, null = "null", digits = NA)
  expect_true(v2_validator(thresh_json, verbose = TRUE))

  ppcr_component_spec <- summary_report_component(report_spec, "prediction-distribution-2")$spec
  ppcr_json <- jsonlite::toJSON(ppcr_component_spec, auto_unbox = TRUE, null = "null", digits = NA)
  expect_true(v2_validator(ppcr_json, verbose = TRUE))
})


test_that("browser acceptance verifies prediction distribution rendering and interactivity in report", {
  skip_on_os("windows")
  browser <- find_headless_browser()
  skip_if(!nzchar(browser), "No headless Chromium/Chrome available")

  output_dir <- tempfile("rtichoke-summary-pred-dist-")
  create_summary_report(
    probs = list("Model A" = seq(0.01, 0.99, length.out = 100)),
    reals = list("Population A" = rep(c(0, 1), 50)),
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

  # Check both Prediction Distribution components are rendered with SVG chart containers
  expect_true(
    component_contains(dom, "prediction-distribution", "<svg"),
    info = browser_stderr
  )
  expect_true(
    component_contains(dom, "prediction-distribution-2", "<svg"),
    info = browser_stderr
  )
})


test_that("resolve_render_report_identifier resolves various JS export formats", {
  vendor <- system.file("rtichoke-viz", package = "rtichoke")
  bundle <- paste(
    readLines(file.path(vendor, "rtichoke-viz.js"), warn = FALSE),
    collapse = "\n"
  )
  resolved <- resolve_render_report_identifier(bundle)
  expect_match(
    resolved,
    "^[A-Za-z_$][A-Za-z0-9_$]*$"
  )

  expect_equal(
    resolve_render_report_identifier(
      "var customFn = function(){}; export { customFn as renderReport };"
    ),
    "customFn"
  )

  expect_equal(
    resolve_render_report_identifier("export{a1 as renderReport,b2 as foo}"),
    "a1"
  )

  expect_equal(
    resolve_render_report_identifier(
      "function renderReport(){}; export { renderReport };"
    ),
    "renderReport"
  )

  expect_equal(
    resolve_render_report_identifier("export function renderReport(){};"),
    "renderReport"
  )

  expect_equal(
    resolve_render_report_identifier("function renderReport(){};"),
    "renderReport"
  )

  expect_error(
    resolve_render_report_identifier("function foo(){}; export { foo };"),
    "Could not resolve renderReport export from rtichoke-viz bundle"
  )

  expect_error(
    resolve_render_report_identifier(NULL),
    "Could not resolve renderReport export from rtichoke-viz bundle"
  )

  expect_error(
    resolve_render_report_identifier(NA_character_),
    "Could not resolve renderReport export from rtichoke-viz bundle"
  )
})
