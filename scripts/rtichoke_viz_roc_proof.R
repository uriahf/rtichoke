library(rtichoke)

performance_data <- prepare_performance_data(
  probs = list(
    "Estimated model" = example_dat$estimated_probabilities,
    "Random guess" = example_dat$random_guess
  ),
  reals = list(example_dat$outcome)
)

rtichoke:::write_rtichoke_viz_roc_proof(
  performance_data,
  file.path("docs", "rtichoke-viz-roc")
)
