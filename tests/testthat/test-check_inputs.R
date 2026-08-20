test_that("probs must be in range of [0,1]", {
  expect_error(
    check_probs_input(c(example_dat$estimated_probabilities, -0.1)),
    "Estimated Probabilities are out of the range"
  )
  expect_error(
    check_probs_input(c(example_dat$estimated_probabilities, 1.1)),
    "Estimated Probabilities are out of the range"
  )

  expect_error(
    list(
      "train" = example_dat %>%
        dplyr::filter(type_of_set == "train") %>%
        dplyr::pull(estimated_probabilities),
      "test" = c(
        example_dat %>%
          dplyr::filter(type_of_set == "test") %>%
          dplyr::pull(estimated_probabilities),
        -0.2
      )
    ) %>%
      check_probs_input(),
    "Estimated Probabilities are out of the range"
  )
})


test_that("real must be 0 or 1", {
  expect_error(
    rtichoke:::check_real_input(c(example_dat$outcome, 0.1)),
    "Outcomes are out of the range"
  )
  expect_error(
    rtichoke:::check_real_input(c(example_dat$outcome, 0.9)),
    "Outcomes are out of the range"
  )

  expect_error(
    list(
      "train" = example_dat %>%
        dplyr::filter(type_of_set == "train") %>%
        dplyr::pull(outcome),
      "test" = c(
        example_dat %>%
          dplyr::filter(type_of_set == "test") %>%
          dplyr::pull(outcome),
        0.2
      )
    ) %>%
      rtichoke:::check_real_input(),
    "Outcomes are out of the range"
  )
})


test_that("public curve builders reject out-of-range probabilities", {
  invalid_probs <- list(c(example_dat$estimated_probabilities, -0.1))
  reals <- list(c(example_dat$outcome, 1))

  expect_error(
    prepare_performance_data(probs = invalid_probs, reals = reals),
    "Estimated Probabilities are out of the range"
  )
  expect_error(
    create_roc_curve(probs = invalid_probs, reals = reals),
    "Estimated Probabilities are out of the range"
  )
  expect_error(
    create_lift_curve(probs = invalid_probs, reals = reals),
    "Estimated Probabilities are out of the range"
  )
  expect_error(
    create_precision_recall_curve(probs = invalid_probs, reals = reals),
    "Estimated Probabilities are out of the range"
  )
})


test_that("plot functions reject incompatible stratification options", {
  expect_error(
    train_and_test_sets %>%
      plot_roc_curve(
        interactive = FALSE,
        stratified_by = "ppcr"
      )
  )

  expect_error(
    train_and_test_sets %>%
      plot_lift_curve(
        interactive = FALSE,
        stratified_by = "ppcr"
      )
  )

  expect_error(
    train_and_test_sets %>%
      plot_precision_recall_curve(
        interactive = FALSE,
        main_slider = "ppcr"
      )
  )

  expect_error(
    train_and_test_sets %>%
      plot_gains_curve(
        interactive = FALSE,
        main_slider = "ppcr"
      )
  )

  expect_error(
    train_and_test_sets_enforced_percentiles_symmetry %>%
      plot_gains_curve(stratified_by = "ppcr")
  )

  expect_error(
    train_and_test_sets %>%
      plot_decision_curve(
        interactive = FALSE,
        main_slider = "ppcr"
      )
  )

  expect_error(
    train_and_test_sets_enforced_percentiles_symmetry %>%
      plot_decision_curve(interactive = FALSE)
  )
})
