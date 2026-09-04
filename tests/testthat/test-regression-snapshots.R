test_that("single-model performance data stays stable", {
  performance_data <- prepare_performance_data(
    probs = list(c(0.1, 0.4, 0.6, 0.9)),
    reals = list(c(0, 1, 0, 1)),
    by = 0.25
  )

  middle_threshold <- dplyr::filter(
    performance_data,
    probability_threshold == 0.5
  )

  expect_equal(middle_threshold$TP, 1)
  expect_equal(middle_threshold$TN, 1)
  expect_equal(middle_threshold$FN, 1)
  expect_equal(middle_threshold$FP, 1)
  expect_equal(middle_threshold$sensitivity, 0.5)
  expect_equal(middle_threshold$specificity, 0.5)
  expect_equal(middle_threshold$PPV, 0.5)
  expect_equal(middle_threshold$NPV, 0.5)
  expect_equal(middle_threshold$NB, 0)
  expect_equal(middle_threshold$ppcr, 0.5)

  expect_snapshot_value(
    dplyr::filter(
      performance_data,
      probability_threshold %in% c(0.25, 0.5, 0.75)
    ),
    style = "json2"
  )
})


test_that("multi-model performance data stays stable", {
  performance_data <- prepare_performance_data(
    probs = list(
      "Model A" = c(0.1, 0.4, 0.6, 0.9),
      "Model B" = c(0.2, 0.3, 0.7, 0.8)
    ),
    reals = list(c(0, 1, 0, 1)),
    by = 0.5
  )

  expect_identical(unique(performance_data$model), c("Model A", "Model B"))
  expect_snapshot_value(
    dplyr::filter(performance_data, probability_threshold == 0.5),
    style = "json2"
  )
})


test_that("multi-population performance data stays stable", {
  performance_data <- prepare_performance_data(
    probs = list(
      "Population A" = c(0.1, 0.4, 0.6, 0.9),
      "Population B" = c(0.2, 0.3, 0.7, 0.8)
    ),
    reals = list(
      "Population A" = c(0, 1, 0, 1),
      "Population B" = c(0, 0, 1, 1)
    ),
    by = 0.5
  )

  expect_identical(
    unique(performance_data$population),
    c("Population A", "Population B")
  )
  expect_snapshot_value(
    dplyr::filter(performance_data, probability_threshold == 0.5),
    style = "json2"
  )
})


test_that("ppcr stratification stays stable", {
  performance_data <- prepare_performance_data(
    probs = list(c(0.1, 0.4, 0.6, 0.9)),
    reals = list(c(0, 1, 0, 1)),
    by = 0.25,
    stratified_by = "ppcr"
  )

  expect_snapshot_value(performance_data, style = "json2")
})


test_that("calibration deciles stay stable", {
  calibration_bins_dat <- rtichoke:::make_calibration_bins_dat(
    probs = example_dat$estimated_probabilities,
    reals = example_dat$outcome,
    n_bins = 10
  )

  expect_snapshot_value(calibration_bins_dat, style = "json2")
})
