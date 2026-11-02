# Deterministic continuous longitudinal example for function documentation.
# Generate the package dataset by running this file from the package root.

continuous_outcomes_steps <- data.frame(
  id = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6),
  step = c(0, 1, 3, 0, 1, 3, 0, 1, 3, 0, 1, 3, 0, 1, 3, 0, 1),
  reals = c(10, 15, 20, 12, 17, 25, 14, NA, 30, 16, 20, NA, 18, 22, 35, 20, 24)
)

stopifnot(
  !anyDuplicated(continuous_outcomes_steps[c("id", "step")]),
  identical(sort(unique(continuous_outcomes_steps$step)), c(0, 1, 3)),
  nrow(continuous_outcomes_steps) == 17L
)

usethis::use_data(continuous_outcomes_steps, overwrite = TRUE)
