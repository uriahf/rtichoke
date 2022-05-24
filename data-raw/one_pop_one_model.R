## code to prepare `one_pop_one_model` dataset goes here

one_pop_one_model <- prepare_performance_data(
  probs = list(example_dat$estimated_probabilities),
  reals = list(example_dat$outcome)
)

usethis::use_data(one_pop_one_model, overwrite = TRUE)
