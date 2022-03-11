test_that("probs must be in range of [0,1]", {
  expect_error(check_probs_input(c(example_dat$estimated_probabilities, -0.1)))
  expect_error(check_probs_input(c(example_dat$estimated_probabilities, 1.1)))

  expect_error(list(
    "train" = example_dat %>%
      dplyr::filter(type_of_set == "train") %>%
      dplyr::pull(estimated_probabilities),
    "test" = c(example_dat %>% dplyr::filter(type_of_set == "test") %>%
      dplyr::pull(estimated_probabilities), -0.2) %>%
      check_probs_input()
  ))
})


test_that("real must be 0 or 1", {
  expect_error(check_real_input(c(example_dat$outcome, 0.1)))
  expect_error(check_real_input(c(example_dat$outcome, 0.9)))

  expect_error(list(
    "train" = example_dat %>%
      dplyr::filter(type_of_set == "train") %>%
      dplyr::pull(outcome),
    "test" = c(example_dat %>% dplyr::filter(type_of_set == "test") %>%
      dplyr::pull(outcome), 0.2)
  ) %>%
    check_real_input())
})


# Test inputs for prepare_performance_data() and create_*_curve() functions

test_that("input checks should return error", {
  expect_error(
    prepare_performance_data(
      probs = c(example_dat$estimated_probabilities, -0.1),
      real = c(example_dat$outcome, 1)
    )
  )
  
  expect_error(
    create_roc_curve(
      probs = c(example_dat$estimated_probabilities, -0.1),
      real = c(example_dat$outcome, 1)
    )
)
    
  expect_error(
    create_lift_curve(
      probs = c(example_dat$estimated_probabilities, -0.1),
      real = c(example_dat$outcome, 1)
    )
  )
  
  expect_error(
    create_precision_recall_curve(
      probs = c(example_dat$estimated_probabilities, -0.1),
      real = c(example_dat$outcome, 1)
    )
  )
  
  expect_error(
    prepare_performance_data(
      probs = c(example_dat$estimated_probabilities, -0.1),
      real = c(example_dat$outcome, 1)
    )
  )
  
  expect_error(
    prepare_performance_data(
      probs = c(example_dat$estimated_probabilities, -0.1),
      real = c(example_dat$outcome, 1)
    )
  )
    
  expect_error(
    prepare_performance_data(
      probs = c(example_dat$estimated_probabilities, -0.1),
      real = c(example_dat$outcome, 1)
    )
  )
    
})

# Test consistency for performance_data and plot_*_curve functions





test_that("input checks should return error", {
  
  expect_error(
    train_and_test_sets %>%
      plot_roc_curve(interactive = FALSE, 
                     main_slider = "ppcr")
  )
  
  expect_error(
    train_and_test_sets_enforced_percentiles_symmetry %>%
      plot_roc_curve(interactive = FALSE)
  )
  
  expect_error(
    train_and_test_sets %>%
      plot_lift_curve(interactive = FALSE, 
                     main_slider = "ppcr")
  )
  
  expect_error(
    train_and_test_sets_enforced_percentiles_symmetry %>%
      plot_lift_curve(interactive = FALSE)
  )
  
  expect_error(
    train_and_test_sets %>%
      plot_precision_recall_curve(interactive = FALSE, 
                      main_slider = "ppcr")
  )
  
  expect_error(
    train_and_test_sets_enforced_percentiles_symmetry %>%
      plot_precision_recall_curve(interactive = FALSE)
  )
  
  
  expect_error(
    train_and_test_sets %>%
      plot_gains_curve(interactive = FALSE, 
                      main_slider = "ppcr")
  )
  
  expect_error(
    train_and_test_sets_enforced_percentiles_symmetry %>%
      plot_gains_curve(interactive = FALSE)
  )
  
  
  expect_error(
    train_and_test_sets %>%
      plot_decision_curve(interactive = FALSE, 
                      main_slider = "ppcr")
  )
  
  expect_error(
    train_and_test_sets_enforced_percentiles_symmetry %>%
      plot_decision_curve(interactive = FALSE)
  )
  
  
})

