test_that("test deciles dat", {
  deciles_dat <- rtichoke:::make_deciles_dat(
    probs = example_dat$estimated_probabilities,
    real = example_dat$outcome
  )

  expect_identical(deciles_dat$quintile, 1:10)
  expect_identical(
    names(deciles_dat),
    c("quintile", "y", "x", "sum_reals", "total_obs")
  )
})


test_that("limits of calibration curve", {
  limits_calibration_curve <- rtichoke:::make_deciles_dat(
    probs = example_dat$estimated_probabilities,
    real = example_dat$outcome
  ) %>%
    rtichoke:::define_limits_for_calibration_plot()

  expect_equal(length(limits_calibration_curve), 2)
})


test_that("checking probs and real inputs", {
  expect_error(
    create_calibration_curve(
      probs = c(example_dat$estimated_probabilities, -0.2),
      real = c(example_dat$outcome, 1)
    )
  )

  # expect_error(
  #   create_calibration_curve(
  #     probs = list(
  #       "train" = example_dat %>%
  #         dplyr::filter(type_of_set == "train") %>%
  #         dplyr::pull(estimated_probabilities),
  #       "test" = c(example_dat %>% dplyr::filter(type_of_set == "test") %>%
  #         dplyr::pull(estimated_probabilities), 0.3)
  #     ),
  #     real = list(
  #       "train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
  #         dplyr::pull(outcome),
  #       "test" = c(example_dat %>% dplyr::filter(type_of_set == "test") %>%
  #         dplyr::pull(outcome), 2)
  #     )
  #   )
  # )
})
