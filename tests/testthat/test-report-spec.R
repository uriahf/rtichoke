report_test_specs <- function(
  model_known = TRUE,
  multiple_populations = FALSE
) {
  if (multiple_populations) {
    metadata <- data.frame(
      model = c(NA_character_, NA_character_),
      population = c("Population A", "Population B"),
      evaluation = c("Population A", "Population B")
    )
    performance_data <- tibble::tibble(
      population = rep(c("Population A", "Population B"), each = 2),
      probability_threshold = rep(c(0.25, 0.75), 2),
      sensitivity = c(0.9, 0.5, 0.8, 0.4),
      specificity = c(0.4, 0.9, 0.5, 0.85),
      TP = c(9, 5, 8, 4)
    )
  } else {
    metadata <- data.frame(
      model = if (model_known) {
        "Model A"
      } else {
        NA_character_
      },
      population = "Population A",
      evaluation = if (model_known) {
        "Model A"
      } else {
        "Population A"
      }
    )
    performance_data <- tibble::tibble(
      probability_threshold = c(0.25, 0.75),
      sensitivity = c(0.9, 0.5),
      specificity = c(0.4, 0.9),
      TP = c(9, 5)
    )
    if (model_known) {
      performance_data$model <- "Model A"
    } else {
      performance_data$population <- "Population A"
    }
  }

  list(
    performance_table = rtichoke:::rtichoke_viz_performance_table_v2_spec(
      performance_data,
      metadata
    ),
    roc = rtichoke:::rtichoke_viz_roc_v2_spec(
      performance_data,
      metadata
    )
  )
}

report_test_calibration_spec <- function() {
  list(
    schemaVersion = "2.0",
    type = "calibration",
    evaluations = list(list(
      id = "evaluation-1",
      model = "Model A",
      population = "Population A"
    )),
    series = list(list(
      id = "series-1",
      evaluationId = "evaluation-1",
      display = list(
        label = "Model A",
        group = "Model A",
        role = "model"
      )
    )),
    data = list(list(
      seriesId = "series-1",
      predicted = 0.4,
      observed = 0.5,
      method = "discrete",
      events = 5,
      total = 10
    )),
    x = "predicted",
    y = "observed",
    xAxis = list(label = "Predicted probability", domain = c(0, 1)),
    yAxis = list(label = "Observed probability", domain = c(0, 1)),
    references = list(list(type = "identity", scope = "global"))
  )
}

test_that("report assembler composes performance table, ROC, and calibration", {
  specs <- report_test_specs()
  calibration <- report_test_calibration_spec()

  report <- rtichoke:::rtichoke_viz_report_spec(
    specs$performance_table,
    specs$roc,
    calibration,
    title = "Model performance"
  )

  expect_identical(report$schemaVersion, "1.0")
  expect_identical(report$type, "report")
  expect_identical(report$title, "Model performance")
  expect_identical(
    vapply(report$components, `[[`, "", "id"),
    c("performance-table", "roc", "calibration")
  )
  expect_identical(report$components[[1]]$spec, specs$performance_table)
  expect_identical(report$components[[2]]$spec, specs$roc)
  expect_identical(report$components[[3]]$spec, calibration)
})

test_that("component IDs are deterministic, unique, and independent of series IDs", {
  specs <- report_test_specs()
  first <- rtichoke:::rtichoke_viz_report_spec(
    specs$roc,
    specs$roc,
    specs$performance_table
  )
  second <- rtichoke:::rtichoke_viz_report_spec(
    specs$roc,
    specs$roc,
    specs$performance_table
  )

  ids <- vapply(first$components, `[[`, "", "id")
  expect_identical(ids, c("roc", "roc-2", "performance-table"))
  expect_identical(ids, vapply(second$components, `[[`, "", "id"))
  expect_length(unique(ids), length(ids))
  expect_false(any(ids %in% c("series-1", "series-2")))
})

test_that("component order and optional titles are presentation metadata", {
  specs <- report_test_specs()
  report <- rtichoke:::rtichoke_viz_report_spec(
    specs$roc,
    specs$performance_table,
    component_titles = list("ROC curve", NULL)
  )

  expect_identical(
    vapply(report$components, `[[`, "", "id"),
    c("roc", "performance-table")
  )
  expect_identical(report$components[[1]]$title, "ROC curve")
  expect_false("title" %in% names(report$components[[2]]))
})

test_that("evaluation IDs remain component-local", {
  specs <- report_test_specs()
  calibration <- report_test_calibration_spec()
  report <- rtichoke:::rtichoke_viz_report_spec(
    specs$performance_table,
    specs$roc,
    calibration
  )

  expect_identical(
    vapply(
      report$components,
      function(component) {
        component$spec$evaluations[[1]]$id
      },
      character(1)
    ),
    rep("evaluation-1", 3)
  )
  expect_false("evaluations" %in% names(report))
})

test_that("model-known semantics survive report assembly unchanged", {
  specs <- report_test_specs(model_known = TRUE)
  report <- rtichoke:::rtichoke_viz_report_spec(
    specs$performance_table,
    specs$roc
  )

  expect_identical(
    report$components[[1]]$spec$evaluations[[1]]$model,
    "Model A"
  )
  expect_identical(
    report$components[[2]]$spec$evaluations[[1]]$population,
    "Population A"
  )
})

test_that("model-unknown semantics survive report assembly unchanged", {
  specs <- report_test_specs(model_known = FALSE)
  report <- rtichoke:::rtichoke_viz_report_spec(
    specs$performance_table,
    specs$roc
  )

  expect_false(
    "model" %in% names(report$components[[1]]$spec$evaluations[[1]])
  )
  expect_false(
    "model" %in% names(report$components[[2]]$spec$evaluations[[1]])
  )
})

test_that("multiple populations remain component-local semantic metadata", {
  specs <- report_test_specs(multiple_populations = TRUE)
  report <- rtichoke:::rtichoke_viz_report_spec(
    specs$performance_table,
    specs$roc
  )

  for (component in report$components) {
    expect_identical(
      vapply(component$spec$evaluations, `[[`, "", "population"),
      c("Population A", "Population B")
    )
  }
})

test_that("report assembler does not recompute or normalize component specs", {
  specs <- report_test_specs()
  specs$roc$data[[1]]$sensitivity <- 0.123456
  specs$performance_table$rows[[1]]$values[[1]]$estimate <- 987

  report <- rtichoke:::rtichoke_viz_report_spec(
    specs$performance_table,
    specs$roc
  )

  expect_identical(report$components[[1]]$spec, specs$performance_table)
  expect_identical(report$components[[2]]$spec, specs$roc)
  expect_identical(report$components[[2]]$spec$data[[1]]$sensitivity, 0.123456)
})

test_that("report assembler rejects invalid report-level inputs", {
  specs <- report_test_specs()

  expect_error(
    rtichoke:::rtichoke_viz_report_spec(),
    "at least one component"
  )
  expect_error(
    rtichoke:::rtichoke_viz_report_spec(
      specs$roc,
      component_titles = list("one", "two")
    ),
    "one entry per component"
  )
  expect_error(
    rtichoke:::rtichoke_viz_report_spec(list(
      schemaVersion = "1.0",
      type = "roc"
    )),
    "schemaVersion 2.0"
  )
})

test_that("structured report assembler validates v1.1 wrappers and ids", {
  specs <- report_test_specs()
  component <- list(
    type = "component",
    id = "roc",
    title = "ROC",
    spec = specs$roc
  )
  group <- list(
    type = "group",
    id = "discrimination-threshold",
    title = "By Probability Threshold",
    components = list(component)
  )
  section <- list(
    id = "discrimination",
    title = "Discrimination",
    items = list(group)
  )

  report <- rtichoke:::rtichoke_viz_report_spec_v1_1(
    section,
    title = "Summary Report"
  )
  expect_identical(report$schemaVersion, "1.1")
  expect_identical(report$sections[[1]], section)

  invalid_component <- component
  invalid_component$type <- NULL
  invalid_section <- section
  invalid_section$items[[1]]$components[[1]] <- invalid_component
  expect_error(
    rtichoke:::rtichoke_viz_report_spec_v1_1(invalid_section),
    'type = "component"'
  )

  duplicate_group_section <- section
  duplicate_group_section$items <- list(group, group)
  expect_error(
    rtichoke:::rtichoke_viz_report_spec_v1_1(duplicate_group_section),
    "group ids must be unique"
  )

  duplicate_component_group <- group
  duplicate_component_group$components <- list(component, component)
  duplicate_component_section <- section
  duplicate_component_section$items <- list(duplicate_component_group)
  expect_error(
    rtichoke:::rtichoke_viz_report_spec_v1_1(duplicate_component_section),
    "component ids must be unique"
  )
})
