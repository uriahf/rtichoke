## code to prepare `multiple_models` dataset goes here

multiple_models <- prepare_performance_data(
  probs = list(
    "First Model" = example_dat$estimated_probabilities,
    "Second Model" = example_dat$random_guess
  ),
  reals = list(example_dat$outcome)
)

usethis::use_data(multiple_models, overwrite = TRUE)
