## code to prepare `multiple_populations_by_ppcr` dataset goes here

multiple_populations_by_ppcr <- prepare_performance_data(
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
  ),
  stratified_by = "ppcr"
)

usethis::use_data(multiple_populations_by_ppcr, overwrite = TRUE)
