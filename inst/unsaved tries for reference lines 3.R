performance_table_one_model <- create_performance_table(
  probs = example_dat$estimated_probabilities,
  real = example_dat$outcome)


performance_table_one_model_probs_as_list <- create_performance_table(
  probs = list("glm" = example_dat$estimated_probabilities),
  real = example_dat$outcome)


# And you can create performance table for more than one model
performance_table_for_four_models <- create_performance_table(
  probs = list("First Model" = example_dat$estimated_probabilities,
               "Second Model" = example_dat$random_guess,
               "Third Model" = sample(example_dat$estimated_probabilities)),
  real = example_dat$outcome)


performance_table_for_train_and_test_sets <- create_performance_table(
  probs = list("train" = example_dat %>%
                 dplyr::filter(type_of_set == "train") %>%
                 dplyr::pull(estimated_probabilities),
               "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
                 dplyr::pull(estimated_probabilities)),
  real = list("train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
                dplyr::pull(outcome),
              "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
                dplyr::pull(outcome))
)

names(performance_table_for_four_models)


names(performance_table_for_train_and_test_sets)

performance_table_for_train_and_test_sets %>%
  rtichoke::create_plotly_for_performance_metrics(FPR, sensitivity) %>%
plotly::layout(paper_bgcolor='#fdf9f1',
      plot_bgcolor='#fdf9f1')



roc_example_plot_interactive <- performance_table_for_train_and_test_sets %>%
  rtichoke::plot_roc_curve(interactive = T)




add_reference_lines_to_plotly(roc_example_plot_interactive , c(0.5,0.5), c(0.25, 0.75))



