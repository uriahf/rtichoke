## code to prepare `multiple_populations` dataset goes here

multiple_populations <- prepare_performance_data(
  probs = list(
    "train" = example_dat %>%
      dplyr::filter(type_of_set == "train") %>%
      dplyr::pull(estimated_probabilities),
    "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
      dplyr::pull(estimated_probabilities)
  ),
  reals = list(
    "train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
      dplyr::pull(outcome),
    "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
      dplyr::pull(outcome)
  )
)


usethis::use_data(multiple_populations, overwrite = TRUE)
