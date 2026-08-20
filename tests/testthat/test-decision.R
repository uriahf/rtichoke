test_that("decision type is validated", {
  expect_error(
    create_decision_curve(
      probs = list(example_dat$estimated_probabilities),
      reals = list(example_dat$outcome),
      type = "decision"
    ),
    "should be one of"
  )
})
