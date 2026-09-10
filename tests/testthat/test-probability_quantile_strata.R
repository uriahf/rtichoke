test_that("assign_probability_quantile_strata works for mostly continuous probabilities", {
  probs <- seq(0.01, 1.00, length.out = 100)
  strata <- assign_probability_quantile_strata(probs, by = 0.20)

  expect_s3_class(strata, "ordered")
  expect_equal(levels(strata), c("0.2", "0.4", "0.6", "0.8", "1.0"))
  expect_equal(length(strata), 100)
  expect_equal(as.character(table(strata)), rep("20", 5))
})

test_that("assign_probability_quantile_strata handles deterministic Example A parity", {
  probs <- c(0.00, 0.10, 0.20, 0.20, 0.40, 0.50, 0.50, 0.70, 0.80, 0.90, 1.00)
  strata <- assign_probability_quantile_strata(probs, by = 0.20)

  expect_s3_class(strata, "ordered")
  expect_equal(levels(strata), c("0.2", "0.4", "0.6", "0.8", "1.0"))

  expected_assignments <- factor(
    c("0.2", "0.2", "0.2", "0.2", "0.4", "0.6", "0.6", "0.8", "0.8", "1.0", "1.0"),
    levels = c("0.2", "0.4", "0.6", "0.8", "1.0"),
    ordered = TRUE
  )
  expect_equal(strata, expected_assignments)

  counts <- table(strata)
  expect_equal(as.numeric(counts["0.2"]), 4)
  expect_equal(as.numeric(counts["0.4"]), 1)
  expect_equal(as.numeric(counts["0.6"]), 2)
  expect_equal(as.numeric(counts["0.8"]), 2)
  expect_equal(as.numeric(counts["1.0"]), 2)
})

test_that("assign_probability_quantile_strata handles Example B (substantial ties and unused levels)", {
  probs <- c(0.0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 1.0)
  strata <- assign_probability_quantile_strata(probs, by = 0.20)

  expect_s3_class(strata, "ordered")
  expect_equal(levels(strata), c("0.2", "0.4", "0.6", "0.8", "1.0"))

  expected_assignments <- factor(
    c("0.2", "0.2", "0.2", "0.2", "0.2", "0.2", "0.2", "0.2", "0.2", "1.0"),
    levels = c("0.2", "0.4", "0.6", "0.8", "1.0"),
    ordered = TRUE
  )
  expect_equal(strata, expected_assignments)

  counts <- table(strata)
  expect_equal(as.numeric(counts["0.2"]), 9)
  expect_equal(as.numeric(counts["0.4"]), 0)
  expect_equal(as.numeric(counts["0.6"]), 0)
  expect_equal(as.numeric(counts["0.8"]), 0)
  expect_equal(as.numeric(counts["1.0"]), 1)
})

test_that("assign_probability_quantile_strata handles observations exactly on internal quantile boundaries", {
  # probs where internal quantiles are 0.2, 0.4, 0.6, 0.8
  probs <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
  strata <- assign_probability_quantile_strata(probs, by = 0.20)

  # Python digitize(..., right=True):
  # 0.0 <= 0.2 -> 0.2
  # 0.2 <= 0.2 -> 0.2
  # 0.4 <= 0.4 -> 0.4
  # 0.6 <= 0.6 -> 0.6
  # 0.8 <= 0.8 -> 0.8
  # 1.0 -> 1.0
  expected_assignments <- factor(
    c("0.2", "0.2", "0.4", "0.6", "0.8", "1.0"),
    levels = c("0.2", "0.4", "0.6", "0.8", "1.0"),
    ordered = TRUE
  )
  expect_equal(strata, expected_assignments)
})

test_that("assign_probability_quantile_strata handles N not divisible by number of strata", {
  # N = 11, by = 0.25 (4 strata: 0.25, 0.50, 0.75, 1.00)
  probs <- seq(0, 1, length.out = 11)
  strata <- assign_probability_quantile_strata(probs, by = 0.25)

  expect_s3_class(strata, "ordered")
  expect_equal(levels(strata), c("0.25", "0.50", "0.75", "1.00"))
  expect_equal(length(strata), 11)
})

test_that("assign_probability_quantile_strata handles exact 0.0 and exact 1.0 probabilities", {
  probs <- c(0.0, 0.0, 1.0, 1.0)
  strata <- assign_probability_quantile_strata(probs, by = 0.50)

  expect_s3_class(strata, "ordered")
  expect_equal(levels(strata), c("0.5", "1.0"))
  # edges: [0, 0.5, 1.0], internal bin: 0.5
  # 0.0 <= 0.5 -> "0.5"
  # 1.0 > 0.5 -> "1.0"
  expect_equal(as.character(strata), c("0.5", "0.5", "1.0", "1.0"))
})

test_that("prepare_performance_data PPCR operating point behavior remains completely unchanged", {
  probs <- list("model1" = c(0.1, 0.2, 0.3, 0.4, 0.5))
  reals <- list(c(0, 0, 1, 0, 1))

  perf_data_ppcr <- prepare_performance_data(
    probs = probs,
    reals = reals,
    by = 0.2,
    stratified_by = "ppcr"
  )

  # Verify existing R prepare_performance_data(..., stratified_by = "ppcr") behavior
  expect_equal(perf_data_ppcr$ppcr, c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0))
  expect_equal(as.numeric(perf_data_ppcr$probability_threshold), c(0.50, 0.42, 0.34, 0.26, 0.18, 0.10))
  expect_equal(as.numeric(perf_data_ppcr$TP), c(0, 1, 1, 2, 2, 2))
  expect_equal(as.numeric(perf_data_ppcr$TN), c(3, 3, 2, 2, 1, 0))
})
