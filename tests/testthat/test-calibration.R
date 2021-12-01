test_that("test deciles dat", {
    deciles_dat <- make_deciles_dat(
    probs = example_dat$estimated_probabilities,
    real = example_dat$outcome
  )
    
  expect_identical(deciles_dat$quintile, 1:10)  
  expect_identical(names(deciles_dat), c("quintile", "phaty", "phatx"))  
})


test_that("limits of calibration curve", {
  limits_calibration_curve <- make_deciles_dat(
  probs = example_dat$estimated_probabilities,
  real = example_dat$outcome
  ) %>%
  define_limits_for_calibration_plot()
  
  expect_equal(length(limits_calibration_curve), 2)  
})

test_that("limits of calibration curve", {
  probs_long_format <-  arrange_estimated_probabilities_to_long_format(
     probs = list("First Model" = example_dat$estimated_probabilities)
    )
  
  expect_identical(names(probs_long_format), c("model", "probs"))  
})

test_that("checking probs and real inputs", {
  expect_error(
    
    create_calibration_curve(
      probs = c(example_dat$estimated_probabilities, -0.2),
      real = c(example_dat$outcome, 1)
    )
  ) 
  
  expect_error(
  create_calibration_curve(
    probs = list(
      "train" = example_dat %>%
        dplyr::filter(type_of_set == "train") %>%
        dplyr::pull(estimated_probabilities),
      "test" = c(example_dat %>% dplyr::filter(type_of_set == "test") %>%
        dplyr::pull(estimated_probabilities), 0.3)
    ),
    real = list(
      "train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
        dplyr::pull(outcome),
      "test" = c(example_dat %>% dplyr::filter(type_of_set == "test") %>%
        dplyr::pull(outcome), 2)
    )
  )
  )
  
})


