test_that("decision type is validated", {
  expect_error(
    create_decision_curve(
      probs = list(example_dat$estimated_probabilities),
      reals = list(example_dat$outcome),
      type = "decision"
    ),
    "should be one of"
  )

  expect_error(
    one_pop_one_model %>%
      plot_decision_curve(type = "decision"),
    "should be one of"
  )
})
