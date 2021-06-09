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
  
  expect_equal(
    names(create_performance_table(
      probs = list("train" = example_dat %>%
                     dplyr::filter(type_of_set == "train") %>%
                     dplyr::pull(estimated_probabilities),
                   "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
                     dplyr::pull(estimated_probabilities)),
      real = list("train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
                    dplyr::pull(outcome),
                  "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
                    dplyr::pull(outcome))
    )),
    c("population", "threshold", "TP", "TN", "FN", "FP", "sensitivity", 
      "FPR", "lift", "specificity", "PPV", "NPV", "positives", "NB", 
      "predicted_positives_percent")
  )
})
