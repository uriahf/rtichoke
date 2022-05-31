## code to prepare `multiple_models_by_ppcr` dataset goes here

multiple_models_by_ppcr <- prepare_performance_data(
  probs = list(
    "First Model" = example_dat$estimated_probabilities,
    "Second Model" = example_dat$random_guess
  ),
  reals = list(example_dat$outcome),
  stratified_by = "ppcr"
)

usethis::use_data(multiple_models_by_ppcr, overwrite = TRUE)
