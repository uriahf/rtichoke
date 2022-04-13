test_that("Performance Table names are correct", {
  expect_equal(
    names(prepare_performance_data(
      probs = list(
        "First Model" = example_dat$estimated_probabilities,
        "Second Model" = example_dat$random_guess
      ),
      reals = list(example_dat$outcome)
    )),
    c(
      "model", "threshold", "TP", "TN", "FN", "FP", "sensitivity",
      "FPR", "specificity", "PPV", "NPV", "lift", "predicted_positives", "NB",
      "ppcr"
    )
  )

  expect_equal(
    names(prepare_performance_data(
      probs = list(example_dat$estimated_probabilities),
      reals = list(example_dat$outcome)
    )),
    c(
      "threshold", "TP", "TN", "FN", "FP", "sensitivity",
      "FPR", "specificity", "PPV", "NPV", "lift", "predicted_positives", "NB",
      "ppcr"
    )
  )

  expect_equal(
    names(prepare_performance_data(
      probs = list(
        "train" = example_dat %>%
          dplyr::filter(type_of_set == "train") %>%
          dplyr::pull(estimated_probabilities),
        "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
          dplyr::pull(estimated_probabilities)
      ),
      real = list(
        "train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
          dplyr::pull(outcome),
        "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
          dplyr::pull(outcome)
      )
    )),
    c(
      "population", "threshold", "TP", "TN", "FN", "FP", "sensitivity",
      "FPR", "specificity", "PPV", "NPV", "lift", "predicted_positives", "NB",
      "ppcr"
    )
  )
})
