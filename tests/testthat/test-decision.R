test_that("type assert works", {
  expect_error(
    create_decision_curve(
      probs = list(example_dat$estimated_probabilities),
      reals = list(example_dat$outcome),
      type = "decision"
    )
  )

  expect_error(
    one_pop_one_model %>%
      plot_decision_curve(type = "decision")
  )
})
