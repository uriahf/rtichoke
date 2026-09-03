test_that("make_calibration_bins_dat default n_bins = 10 numerical behavior unchanged", {
  calibration_bins_dat <- rtichoke:::make_calibration_bins_dat(
    probs = example_dat$estimated_probabilities,
    reals = example_dat$outcome,
    n_bins = 10
  )

  expect_identical(calibration_bins_dat$bin, 1:10)
  expect_identical(
    names(calibration_bins_dat),
    c("bin", "y", "x", "sum_reals", "total_obs")
  )
  expect_equal(nrow(calibration_bins_dat), 10)
  expect_equal(sum(calibration_bins_dat$total_obs), length(example_dat$outcome))
  expect_equal(sum(calibration_bins_dat$sum_reals), sum(example_dat$outcome))
})

test_that("limits of calibration curve", {
  limits_calibration_curve <- rtichoke:::make_calibration_bins_dat(
    probs = example_dat$estimated_probabilities,
    reals = example_dat$outcome
  ) |>
    rtichoke:::define_limits_for_calibration_plot()

  expect_equal(length(limits_calibration_curve), 2)
})

test_that("calibration validates n_bins input", {
  probs <- example_dat$estimated_probabilities
  reals <- example_dat$outcome

  invalid_n_bins <- list(
    NULL,
    NA,
    NaN,
    Inf,
    -1,
    0,
    2.5,
    c(5, 10),
    "10",
    TRUE
  )

  for (val in invalid_n_bins) {
    expect_error(
      rtichoke:::make_calibration_bins_dat(probs, reals, n_bins = val),
      "`n_bins` must be a single positive whole number.",
      fixed = TRUE
    )
  }

  # Valid integer / numeric
  expect_no_error(rtichoke:::make_calibration_bins_dat(
    probs,
    reals,
    n_bins = 5L
  ))
  expect_no_error(rtichoke:::make_calibration_bins_dat(
    probs,
    reals,
    n_bins = 5
  ))
})

test_that("n_bins validation occurs before all-identical shortcut", {
  probs_identical <- rep(0.5, 10)
  reals <- rep(c(0, 1), 5)

  expect_error(
    rtichoke:::make_calibration_bins_dat(probs_identical, reals, n_bins = -2),
    "`n_bins` must be a single positive whole number.",
    fixed = TRUE
  )
})

test_that("n_bins = 8, n_bins = 1, n_bins > n, n = 5/11/12 with n_bins = 10", {
  probs_100 <- seq(0.01, 1, length.out = 100)
  reals_100 <- rep(c(0, 1), 50)

  # n_bins = 8
  dat_8 <- rtichoke:::make_calibration_bins_dat(
    probs_100,
    reals_100,
    n_bins = 8
  )
  expect_equal(nrow(dat_8), 8)
  expect_identical(dat_8$bin, 1:8)

  # n_bins = 1
  dat_1 <- rtichoke:::make_calibration_bins_dat(
    probs_100,
    reals_100,
    n_bins = 1
  )
  expect_equal(nrow(dat_1), 1)
  expect_identical(dat_1$bin, 1L)
  expect_equal(dat_1$x, mean(probs_100))
  expect_equal(dat_1$y, mean(reals_100))

  # n = 11, n_bins = 10
  probs_11 <- seq(0.1, 0.9, length.out = 11)
  reals_11 <- c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1)
  dat_11 <- rtichoke:::make_calibration_bins_dat(
    probs_11,
    reals_11,
    n_bins = 10
  )
  expect_equal(nrow(dat_11), 10)
  expect_equal(sum(dat_11$total_obs), 11)

  # n = 12, n_bins = 10
  probs_12 <- seq(0.1, 0.9, length.out = 12)
  reals_12 <- c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1)
  dat_12 <- rtichoke:::make_calibration_bins_dat(
    probs_12,
    reals_12,
    n_bins = 10
  )
  expect_equal(nrow(dat_12), 10)
  expect_equal(sum(dat_12$total_obs), 12)

  # n = 5, n_bins = 10
  probs_5 <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  reals_5 <- c(0, 0, 1, 1, 1)
  dat_5 <- rtichoke:::make_calibration_bins_dat(probs_5, reals_5, n_bins = 10)
  expect_equal(nrow(dat_5), 5)
  expect_equal(sum(dat_5$total_obs), 5)
  expect_false(any(dat_5$total_obs == 0)) # No empty bins materialized

  # n_bins > n (n = 5, n_bins = 20)
  dat_gt_n <- rtichoke:::make_calibration_bins_dat(
    probs_5,
    reals_5,
    n_bins = 20
  )
  expect_equal(nrow(dat_gt_n), 5)
  expect_equal(sum(dat_gt_n$total_obs), 5)
  expect_false(any(dat_gt_n$total_obs == 0))
})

test_that("all probabilities identical produces one aggregate bin", {
  probs <- rep(0.42, 20)
  reals <- c(rep(0, 15), rep(1, 5))

  dat <- rtichoke:::make_calibration_bins_dat(probs, reals, n_bins = 10)
  expect_equal(nrow(dat), 1)
  expect_identical(dat$bin, 1)
  expect_equal(dat$x, 0.42)
  expect_equal(dat$y, 0.25)
  expect_equal(dat$sum_reals, 5)
  expect_equal(dat$total_obs, 20)
})

test_that("partial ties crossing a bin boundary are handled correctly", {
  probs <- c(0.1, 0.2, 0.2, 0.2, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)
  reals <- c(0, 0, 1, 0, 1, 1, 0, 1, 1, 1)

  dat <- rtichoke:::make_calibration_bins_dat(probs, reals, n_bins = 5)
  expect_equal(sum(dat$total_obs), 10)
  expect_equal(sum(dat$sum_reals), sum(reals))
  expect_identical(dat$bin, 1:5)
})

test_that("create_calibration_curve and list with multiple models and populations", {
  # Multiple models
  probs_models <- list(
    "Model 1" = example_dat$estimated_probabilities,
    "Model 2" = example_dat$random_guess
  )
  reals_models <- list(example_dat$outcome)

  curve_list_m <- create_calibration_curve_list(
    probs = probs_models,
    reals = reals_models,
    n_bins = 8
  )
  expect_equal(nrow(curve_list_m$calibration_bins_dat), 16)
  expect_setequal(
    unique(curve_list_m$calibration_bins_dat$reference_group),
    c("Model 1", "Model 2")
  )

  # Multiple populations
  probs_pops <- list(
    "train" = example_dat |>
      dplyr::filter(type_of_set == "train") |>
      dplyr::pull(estimated_probabilities),
    "test" = example_dat |>
      dplyr::filter(type_of_set == "test") |>
      dplyr::pull(estimated_probabilities)
  )
  reals_pops <- list(
    "train" = example_dat |>
      dplyr::filter(type_of_set == "train") |>
      dplyr::pull(outcome),
    "test" = example_dat |>
      dplyr::filter(type_of_set == "test") |>
      dplyr::pull(outcome)
  )

  curve_list_p <- create_calibration_curve_list(
    probs = probs_pops,
    reals = reals_pops,
    n_bins = 6
  )
  expect_equal(nrow(curve_list_p$calibration_bins_dat), 12)
  expect_setequal(
    unique(curve_list_p$calibration_bins_dat$reference_group),
    c("train", "test")
  )
})

test_that("reference_group identity remains unchanged", {
  curve_list <- create_calibration_curve_list(
    probs = list("Model A" = example_dat$estimated_probabilities),
    reals = list(example_dat$outcome),
    n_bins = 5
  )
  expect_identical(
    unique(curve_list$calibration_bins_dat$reference_group),
    "Model A"
  )
  expect_identical(
    unique(curve_list$reference_data$reference_group),
    "reference_line"
  )
})

test_that("smooth output unaffected by non-default n_bins in create_calibration_curve", {
  probs <- list(example_dat$estimated_probabilities)
  reals <- list(example_dat$outcome)

  curve_smooth_def <- create_calibration_curve(
    probs = probs,
    reals = reals,
    type = "smooth",
    n_bins = 10,
    interactive = FALSE
  )

  curve_smooth_custom <- create_calibration_curve(
    probs = probs,
    reals = reals,
    type = "smooth",
    n_bins = 5,
    interactive = FALSE
  )

  # Compare ggplot output / axes limits / structure
  expect_equal(
    curve_smooth_def$patches$plots[[1]]$coordinates$limits,
    curve_smooth_custom$patches$plots[[1]]$coordinates$limits
  )
})

test_that("Plotly discrete rendering works with n_bins", {
  plotly_curve <- create_calibration_curve(
    probs = list(example_dat$estimated_probabilities),
    reals = list(example_dat$outcome),
    type = "discrete",
    n_bins = 8,
    interactive = TRUE
  )
  expect_s3_class(plotly_curve, "plotly")
})

test_that("ggplot discrete rendering works with n_bins", {
  gg_curve <- create_calibration_curve(
    probs = list(example_dat$estimated_probabilities),
    reals = list(example_dat$outcome),
    type = "discrete",
    n_bins = 8,
    interactive = FALSE
  )
  expect_s3_class(gg_curve, "patchwork")
})

test_that("canonical v1 and v2 adapters consume calibration_bins_dat", {
  curve_list <- create_calibration_curve_list(
    probs = list("Model A" = example_dat$estimated_probabilities),
    reals = list(example_dat$outcome),
    n_bins = 8
  )

  v1_spec <- rtichoke:::rtichoke_viz_calibration_spec(curve_list)
  expect_equal(v1_spec$schemaVersion, "1.0")
  expect_equal(length(v1_spec$data), 8)

  metadata <- rtichoke:::build_evaluation_metadata(
    probs = list("Model A" = example_dat$estimated_probabilities),
    reals = list(example_dat$outcome)
  )

  v2_spec <- rtichoke:::rtichoke_viz_calibration_v2_spec(
    curve_list,
    metadata,
    method = "discrete"
  )
  expect_equal(v2_spec$schemaVersion, "2.0")
  expect_equal(length(v2_spec$data), 8)
  # Ensure CalibrationSpec does not contain n_bins or bin
  expect_false("n_bins" %in% names(v2_spec$data[[1]]))
  expect_false("bin" %in% names(v2_spec$data[[1]]))
})

test_that("browser summary report path succeeds with calibration_bins_dat", {
  probs <- list("Model A" = example_dat$estimated_probabilities)
  reals <- list(example_dat$outcome)
  metadata <- rtichoke:::build_evaluation_metadata(probs, reals)
  curve_list <- create_calibration_curve_list(probs, reals, n_bins = 10)

  calib_v2 <- rtichoke:::rtichoke_viz_calibration_v2_spec(
    curve_list,
    metadata,
    method = "discrete"
  )
  report_spec <- rtichoke:::rtichoke_viz_report_spec(calib_v2)
  browser_tag <- rtichoke:::render_rtichoke_viz_report_browser(report_spec)

  expect_s3_class(browser_tag, "shiny.tag.list")
  expect_match(as.character(browser_tag), "renderReport", fixed = TRUE)
})

test_that("calibration validates probability and outcome inputs", {
  expect_error(
    create_calibration_curve(
      probs = c(example_dat$estimated_probabilities, -0.2),
      reals = c(example_dat$outcome, 1)
    ),
    "Estimated Probabilities are out of the range"
  )

  expect_error(
    create_calibration_curve(
      probs = list(example_dat$estimated_probabilities),
      reals = list(replace(example_dat$outcome, 1, 2))
    ),
    "Outcomes are out of the range"
  )
})
