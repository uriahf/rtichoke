library(rtichoke)

calibration_curve_list <- create_calibration_curve_list(
  probs = list(
    "Estimated model" = example_dat$estimated_probabilities,
    "Random guess" = example_dat$random_guess
  ),
  reals = list(example_dat$outcome)
)

rtichoke:::write_rtichoke_viz_calib_proof(
  calibration_curve_list,
  file.path("docs", "rtichoke-viz-calibration")
)
