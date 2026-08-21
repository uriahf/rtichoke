sem_probs_a <- c(0.05, 0.15, 0.35, 0.55, 0.75, 0.95, 0.25, 0.85)
sem_probs_b <- c(0.10, 0.20, 0.40, 0.60, 0.70, 0.90, 0.30, 0.80)
sem_reals_equal <- c(0, 0, 0, 0, 1, 1, 1, 1)
sem_reals_low <- c(0, 0, 0, 0, 0, 0, 1, 1)
sem_reals_high <- c(0, 0, 1, 1, 1, 1, 1, 1)

sem_curves <- c(
  "roc",
  "precision recall",
  "gains",
  "lift",
  "decision",
  "interventions avoided"
)

sem_reference_groups <- function(performance_data, curve) {
  curve_list <- rtichoke:::create_rtichoke_curve_list(
    performance_data,
    curve = curve
  )
  unique(curve_list$reference_data$reference_group)
}

sem_plotted_groups <- function(performance_data, curve) {
  curve_list <- rtichoke:::create_rtichoke_curve_list(
    performance_data,
    curve = curve
  )
  unique(curve_list$performance_data_ready_for_curve$reference_group)
}

test_that("one model in one population is one plotted evaluation", {
  performance_data <- prepare_performance_data(
    probs = list(sem_probs_a),
    reals = list(sem_reals_equal),
    by = 0.25
  )

  expect_identical(
    rtichoke:::check_performance_data_type_for_plotly(performance_data),
    "one model"
  )

  purrr::walk(sem_curves, function(curve) {
    expect_identical(sem_plotted_groups(performance_data, curve), "model")
  })

  curve_list <- rtichoke:::create_rtichoke_curve_list(
    performance_data,
    curve = "roc"
  )
  expect_identical(unname(curve_list$group_colors_vec$model), "black")
})

test_that("multiple models share one population context", {
  performance_data <- prepare_performance_data(
    probs = list(
      "Model A" = sem_probs_a,
      "Model B" = sem_probs_b
    ),
    reals = list(sem_reals_equal),
    by = 0.25
  )

  expect_identical(
    rtichoke:::check_performance_data_type_for_plotly(performance_data),
    "several models"
  )
  expect_setequal(unique(performance_data$model), c("Model A", "Model B"))

  purrr::walk(sem_curves, function(curve) {
    expect_setequal(
      sem_plotted_groups(performance_data, curve),
      c("Model A", "Model B")
    )
  })

  expect_identical(sem_reference_groups(performance_data, "roc"), "reference_line")
  expect_identical(
    sem_reference_groups(performance_data, "precision recall"),
    "reference_line"
  )
  expect_setequal(
    sem_reference_groups(performance_data, "gains"),
    c("reference_line", "reference_line_perfect_model")
  )
  expect_setequal(
    sem_reference_groups(performance_data, "lift"),
    c("reference_line", "reference_line_perfect_model")
  )
  expect_setequal(
    sem_reference_groups(performance_data, "decision"),
    c("reference_line", "reference_line_treat_all")
  )
  expect_setequal(
    sem_reference_groups(performance_data, "interventions avoided"),
    c("reference_line", "reference_line_treat_none")
  )
})

test_that("different-prevalence populations own distinct references", {
  performance_data <- prepare_performance_data(
    probs = list(
      "Population low" = sem_probs_a,
      "Population high" = sem_probs_a
    ),
    reals = list(
      "Population low" = sem_reals_low,
      "Population high" = sem_reals_high
    ),
    by = 0.25
  )

  expect_identical(
    rtichoke:::check_performance_data_type_for_plotly(performance_data),
    "several populations"
  )
  expect_setequal(
    unique(performance_data$population),
    c("Population low", "Population high")
  )

  purrr::walk(sem_curves, function(curve) {
    expect_setequal(
      sem_plotted_groups(performance_data, curve),
      c("Population low", "Population high")
    )
  })

  expect_identical(sem_reference_groups(performance_data, "roc"), "reference_line")
  expect_setequal(
    sem_reference_groups(performance_data, "precision recall"),
    c("Population low", "Population high")
  )
  expect_setequal(
    sem_reference_groups(performance_data, "gains"),
    c(
      "reference_line",
      "reference_line_perfect_model_Population low",
      "reference_line_perfect_model_Population high"
    )
  )
  expect_setequal(
    sem_reference_groups(performance_data, "lift"),
    c(
      "reference_line",
      "reference_line_perfect_model_Population low",
      "reference_line_perfect_model_Population high"
    )
  )
  expect_setequal(
    sem_reference_groups(performance_data, "decision"),
    c(
      "reference_line",
      "reference_line_treat_all_Population low",
      "reference_line_treat_all_Population high"
    )
  )
  expect_setequal(
    sem_reference_groups(performance_data, "interventions avoided"),
    c(
      "reference_line",
      "reference_line_treat_none_Population low",
      "reference_line_treat_none_Population high"
    )
  )
})

test_that("equal-prevalence populations remain distinct contexts", {
  performance_data <- prepare_performance_data(
    probs = list(
      "Population A" = sem_probs_a,
      "Population B" = sem_probs_b
    ),
    reals = list(
      "Population A" = sem_reals_equal,
      "Population B" = rev(sem_reals_equal)
    ),
    by = 0.25
  )

  expect_setequal(
    unique(performance_data$population),
    c("Population A", "Population B")
  )

  pr_references <- rtichoke:::create_rtichoke_curve_list(
    performance_data,
    curve = "precision recall"
  )$reference_data

  expect_setequal(
    unique(pr_references$reference_group),
    c("Population A", "Population B")
  )
  expect_equal(
    unique(pr_references$y[pr_references$reference_group == "Population A"]),
    unique(pr_references$y[pr_references$reference_group == "Population B"])
  )
})

test_that("paired inputs are represented as population-labelled evaluations", {
  pair_names <- c("Model A @ Population A", "Model B @ Population B")
  performance_data <- prepare_performance_data(
    probs = stats::setNames(list(sem_probs_a, sem_probs_b), pair_names),
    reals = stats::setNames(list(sem_reals_low, sem_reals_high), pair_names),
    by = 0.25
  )

  expect_identical(names(performance_data)[1], "population")
  expect_setequal(unique(performance_data$population), pair_names)
  expect_false("model" %in% names(performance_data))
})

test_that("calibration keeps grouping and one global identity line", {
  multi_model <- create_calibration_curve_list(
    probs = list(
      "Model A" = sem_probs_a,
      "Model B" = sem_probs_b
    ),
    reals = list(sem_reals_equal)
  )

  expect_identical(multi_model$performance_type, "several models")
  expect_setequal(
    unique(multi_model$deciles_dat$reference_group),
    c("Model A", "Model B")
  )
  expect_identical(
    unique(multi_model$reference_data$reference_group),
    "reference_line"
  )

  multi_population <- create_calibration_curve_list(
    probs = list(
      "Population low" = sem_probs_a,
      "Population high" = sem_probs_b
    ),
    reals = list(
      "Population low" = sem_reals_low,
      "Population high" = sem_reals_high
    )
  )

  expect_identical(multi_population$performance_type, "several populations")
  expect_setequal(
    unique(multi_population$deciles_dat$reference_group),
    c("Population low", "Population high")
  )
  expect_identical(
    unique(multi_population$reference_data$reference_group),
    "reference_line"
  )
})

test_that("performance-table semantics follow model versus population shape", {
  models <- prepare_performance_data(
    probs = list(
      "Model A" = sem_probs_a,
      "Model B" = sem_probs_b
    ),
    reals = list(sem_reals_equal),
    by = 0.25
  )
  populations <- prepare_performance_data(
    probs = list(
      "Population A" = sem_probs_a,
      "Population B" = sem_probs_b
    ),
    reals = list(
      "Population A" = sem_reals_low,
      "Population B" = sem_reals_high
    ),
    by = 0.25
  )

  expect_identical(names(models)[1], "model")
  expect_setequal(unique(models$model), c("Model A", "Model B"))
  expect_identical(names(populations)[1], "population")
  expect_setequal(
    unique(populations$population),
    c("Population A", "Population B")
  )
})
