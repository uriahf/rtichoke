test_that("Performance Table names are correct", {
  expect_equal(
    names(create_performance_table(
      probs = list(
        "First Model" = example_dat$estimated_probabilities,
        "Second Model" = example_dat$random_guess
      ),
      real = example_dat$outcome
    )),
    c(
      "model", "threshold", "TP", "TN", "FN", "FP", "sensitivity",
      "FPR", "lift", "specificity", "PPV", "NPV", "positives", "NB",
      "predicted_positives_percent"
    )
  )
  
  expect_equal(
    names(create_performance_table(
      probs = example_dat$estimated_probabilities,
      real = example_dat$outcome
    )),
    c(
      "threshold", "TP", "TN", "FN", "FP", "sensitivity",
      "FPR", "lift", "specificity", "PPV", "NPV", "positives", "NB",
      "predicted_positives_percent"
    )
  )
})