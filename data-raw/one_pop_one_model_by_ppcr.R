## code to prepare `one_pop_one_model_by_ppcr` dataset goes here

one_pop_one_model_by_ppcr <- prepare_performance_data(
  probs = list(example_dat$estimated_probabilities),
  reals = list(example_dat$outcome),
  stratified_by = "ppcr"
)

usethis::use_data(one_pop_one_model_by_ppcr, overwrite = TRUE)
