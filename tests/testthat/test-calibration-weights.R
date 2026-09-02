test_that("unweighted private calibration bins preserve established factual semantics", {
  probs <- seq(0.05, 0.95, length.out = 20)
  reals <- rep(c(0, 1), 10)

  expected <- rtichoke:::make_deciles_dat(probs, reals)
  actual <- rtichoke:::prepare_calibration_bins(probs, reals)

  expect_equal(actual$quintile, expected$quintile)
  expect_equal(actual$x, expected$x)
  expect_equal(actual$y, expected$y)
  expect_equal(actual$sum_reals, expected$sum_reals)
  expect_equal(actual$total_obs, expected$total_obs)
  expect_equal(actual$outcome_weight_sum, expected$total_obs)
})


test_that("outcome weights change observed calibration but not bins or predicted means", {
  probs <- c(0.10, 0.20, 0.30, 0.40, 0.60, 0.70, 0.80, 0.90)
  reals <- c(0, 1, 0, 1, 1, 0, 1, 0)
  weights <- c(1, 3, 1, 1, 2, 1, 4, 1)

  weighted <- rtichoke:::prepare_calibration_bins(
    probs,
    reals,
    outcome_weights = weights,
    n_bins = 2
  )
  unweighted_two_bins <- data.frame(probs, reals) |>
    dplyr::mutate(quintile = dplyr::ntile(probs, 2)) |>
    dplyr::group_by(quintile) |>
    dplyr::summarise(x = mean(probs), .groups = "drop")

  expect_equal(weighted$quintile, unweighted_two_bins$quintile)
  expect_equal(weighted$x, unweighted_two_bins$x)
  expect_equal(weighted$y, c(4 / 6, 6 / 8))
  expect_equal(weighted$outcome_weight_sum, c(6, 8))
  expect_equal(weighted$weighted_sum_reals, c(4, 6))
})


test_that("all-one outcome weights reproduce unweighted bin estimates", {
  probs <- seq(0.05, 0.95, length.out = 20)
  reals <- rep(c(0, 1), 10)

  unweighted <- rtichoke:::prepare_calibration_bins(probs, reals)
  weighted <- rtichoke:::prepare_calibration_bins(
    probs,
    reals,
    outcome_weights = rep(1, length(reals))
  )

  expect_equal(weighted$x, unweighted$x)
  expect_equal(weighted$y, unweighted$y)
  expect_equal(weighted$sum_reals, unweighted$sum_reals)
  expect_equal(weighted$total_obs, unweighted$total_obs)
})


test_that("private weighted calibration bins validate weights", {
  probs <- c(0.1, 0.2, 0.8, 0.9)
  reals <- c(0, 1, 1, 0)

  expect_error(
    rtichoke:::prepare_calibration_bins(
      probs,
      reals,
      outcome_weights = c(1, 1)
    ),
    "same length"
  )
  expect_error(
    rtichoke:::prepare_calibration_bins(
      probs,
      reals,
      outcome_weights = c(1, -1, 1, 1),
      n_bins = 2
    ),
    "finite, non-negative"
  )
  expect_error(
    rtichoke:::prepare_calibration_bins(
      probs,
      reals,
      outcome_weights = c(1, Inf, 1, 1),
      n_bins = 2
    ),
    "finite, non-negative"
  )
  expect_error(
    rtichoke:::prepare_calibration_bins(
      probs,
      reals,
      outcome_weights = c(1, 1, 0, 0),
      n_bins = 2
    ),
    "positive total"
  )
})
