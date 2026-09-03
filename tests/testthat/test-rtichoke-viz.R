test_that("real rtichoke ROC output maps to the canonical viz spec", {
  performance_data <- prepare_performance_data(
    probs = list(
      "Estimated model" = example_dat$estimated_probabilities,
      "Random guess" = example_dat$random_guess
    ),
    reals = list(example_dat$outcome)
  )

  spec <- rtichoke_viz_roc_spec(performance_data)

  expect_identical(spec$schemaVersion, "1.0")
  expect_identical(spec$type, "roc")
  expect_identical(spec$x, "false_positive_rate")
  expect_identical(spec$y, "sensitivity")
  expect_length(spec$data, nrow(performance_data))
  expect_setequal(
    unique(vapply(spec$data, `[[`, character(1), "model")),
    c("Estimated model", "Random guess")
  )

  expect_identical(
    spec$data[[1]]$cutoff,
    performance_data$probability_threshold[[1]]
  )
  expect_identical(
    spec$data[[1]]$sensitivity,
    performance_data$sensitivity[[1]]
  )
  expect_identical(
    spec$data[[1]]$specificity,
    performance_data$specificity[[1]]
  )
})

test_that("ROC spec rejects incomplete performance data", {
  expect_error(
    rtichoke_viz_roc_spec(data.frame(sensitivity = 1)),
    "probability_threshold, specificity"
  )
})

test_that("real calibration output maps to the canonical viz spec", {
  calibration_curve_list <- create_calibration_curve_list(
    probs = list(
      "Estimated model" = example_dat$estimated_probabilities,
      "Random guess" = example_dat$random_guess
    ),
    reals = list(example_dat$outcome)
  )

  spec <- rtichoke_viz_calibration_spec(calibration_curve_list)

  expect_identical(spec$schemaVersion, "1.0")
  expect_identical(spec$type, "calibration")
  expect_identical(spec$x, "predicted")
  expect_identical(spec$y, "observed")
  expect_length(
    spec$data,
    nrow(calibration_curve_list$calibration_bins_dat)
  )
  expect_length(
    spec$distribution,
    nrow(calibration_curve_list$histogram_for_calibration)
  )
  expect_setequal(
    unique(vapply(spec$data, `[[`, character(1), "model")),
    c("Estimated model", "Random guess")
  )

  first_bin <- calibration_curve_list$calibration_bins_dat[1, ]
  expect_identical(spec$data[[1]]$predicted, first_bin$x[[1]])
  expect_identical(spec$data[[1]]$observed, first_bin$y[[1]])
  expect_identical(spec$data[[1]]$events, first_bin$sum_reals[[1]])
  expect_identical(spec$data[[1]]$total, first_bin$total_obs[[1]])
})
