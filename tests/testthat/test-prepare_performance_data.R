test_that("Estimated Probabilities are in Range", {
  expect_error(prepare_performance_data(
    probs = c(example_dat$estimated_probabilities, -0.2),
    reals = c(example_dat$outcome, 1)
  ))

  # expect_error(prepare_performance_data(
  #   probs = list(
  #     "First Model" = c(example_dat$estimated_probabilities, 1.1),
  #     "Second Model" = c(example_dat$random_guess, -0.2)
  #   ),
  #   reals = c(example_dat$outcome, 1)
  # ))

  expect_error(
    prepare_performance_data(
      probs = list(
        "train" = example_dat %>%
          dplyr::filter(type_of_set == "train") %>%
          dplyr::pull(estimated_probabilities),
        "test" = c(example_dat %>% dplyr::filter(type_of_set == "test") %>%
          dplyr::pull(estimated_probabilities), -0.2)
      ),
      reals = list(
        "train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
          dplyr::pull(outcome),
        "test" = c(example_dat %>% dplyr::filter(type_of_set == "test") %>%
          dplyr::pull(outcome), 1)
      )
    )
  )
})


test_that("Outcomes are 1 or 0", {
  # expect_error(prepare_performance_data(
  #   probs = c(example_dat$estimated_probabilities, 0.2),
  #   reals = c(example_dat$outcome, 1.2)
  # ))

  expect_error(prepare_performance_data(
    probs = list(
      "First Model" = c(example_dat$estimated_probabilities, 1.1),
      "Second Model" = c(example_dat$random_guess, 0.2)
    ),
    reals = c(example_dat$outcome, 2)
  ))

  # expect_error(
  #   prepare_performance_data(
  #     probs = list(
  #       "train" = example_dat %>%
  #         dplyr::filter(type_of_set == "train") %>%
  #         dplyr::pull(estimated_probabilities),
  #       "test" = c(example_dat %>% dplyr::filter(type_of_set == "test") %>%
  #         dplyr::pull(estimated_probabilities), 0.2)
  #     ),
  #     reals = list(
  #       "train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
  #         dplyr::pull(outcome),
  #       "test" = c(example_dat %>% dplyr::filter(type_of_set == "test") %>%
  #         dplyr::pull(outcome), 2)
  #     )
  #   )
  # )
})

