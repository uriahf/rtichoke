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
     probs = list("First Model" = example_dat$estimated_probabilities),
     real = example_dat$outcome
    )
  
  expect_identical(names(probs_long_format), c("model", "probs"))  
})


