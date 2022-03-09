test_that("probs must be in range of [0,1]", {
  expect_error(check_probs_input(c(example_dat$estimated_probabilities, -0.1)))
  expect_error(check_probs_input(c(example_dat$estimated_probabilities, 1.1)))

  expect_error(list(
    "train" = example_dat %>%
      dplyr::filter(type_of_set == "train") %>%
      dplyr::pull(estimated_probabilities),
    "test" = c(example_dat %>% dplyr::filter(type_of_set == "test") %>%
      dplyr::pull(estimated_probabilities), -0.2) %>%
      check_probs_input()
  ))
})


test_that("real must be 0 or 1", {
  expect_error(check_real_input(c(example_dat$outcome, -0.1)))
  expect_error(check_real_input(c(example_dat$outcome, 1.1)))

  expect_error(list(
    "train" = example_dat %>%
      dplyr::filter(type_of_set == "train") %>%
      dplyr::pull(outcome),
    "test" = c(example_dat %>% dplyr::filter(type_of_set == "test") %>%
      dplyr::pull(outcome), -0.2)
  ) %>%
    check_real_input())
})
