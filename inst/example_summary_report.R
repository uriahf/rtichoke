library(rtichoke)


create_summary_report(
  probs = example_dat$estimated_probabilities,
  real = example_dat$outcome,
  interactive = TRUE,
  output_file = "one_model.html"
)


create_summary_report(
  probs = list(
    "Good Model" = example_dat$estimated_probabilities,
    "Bad Model" = example_dat$bad_model,
    "Random Guess" = sample(example_dat$random_guess)
  ),
  real = example_dat$outcome,
  interactive = TRUE,
  output_file = "several_models.html"
)


create_summary_report(
  probs = list(
    "Train" = example_dat %>%
      dplyr::filter(type_of_set == "train") %>%
      dplyr::pull(estimated_probabilities),
    "Test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
      dplyr::pull(estimated_probabilities)
  ),
  real = list(
    "Train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
      dplyr::pull(outcome),
    "Test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
      dplyr::pull(outcome)
  ),
  interactive = TRUE,
  output_file = "train_test_val.html"
)
