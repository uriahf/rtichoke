# Future R/Python Statistical Contract
# The golden fixtures and invariant tests in this file define the exact semantics
# for static binary probability distribution data preparation.
# Python implementations must reproduce these exact boundaries, counts, and
# realized PPCR calculations.

test_that("golden threshold fixture matches expected static classification counts", {
  # CROSS-LANGUAGE STATISTICAL CONTRACT FIXTURE
  scores <- c(0.0, 0.2, 0.5, 0.5, 0.8, 1.0)
  outcomes <- c(0, 1, 1, 0, 1, 0)

  res <- prepare_probs_distribution_data(
    probs = list(scores),
    reals = list(outcomes),
    by = 0.1,
    stratified_by = "probability_threshold"
  )

  ops <- res$operating_points
  bins <- res$bins

  target_cutoffs <- c(0.0, 0.2, 0.5, 1.0)
  sub_ops <- dplyr::filter(ops, cutoff %in% target_cutoffs)

  expect_equal(sub_ops$cutoff, target_cutoffs)
  expect_equal(sub_ops$realized_ppcr, c(1.0, 2 / 3, 1 / 3, 0.0), tolerance = 1e-5)

  # Reconstruct TP, FP, TN, FN from bins for each target cutoff
  expected_counts <- list(
    "0"   = c(TP = 3, FP = 3, TN = 0, FN = 0),
    "0.2" = c(TP = 2, FP = 2, TN = 1, FN = 1),
    "0.5" = c(TP = 1, FP = 1, TN = 2, FN = 2),
    "1"   = c(TP = 0, FP = 0, TN = 3, FN = 3)
  )

  for (c_val in target_cutoffs) {
    if (c_val == 0) {
      tp <- sum(bins$n_positive)
      fp <- sum(bins$n_negative)
      tn <- 0
      fn <- 0
    } else {
      tn <- sum(bins$n_negative[bins$upper <= c_val])
      fn <- sum(bins$n_positive[bins$upper <= c_val])
      tp <- sum(bins$n_positive[bins$upper > c_val])
      fp <- sum(bins$n_negative[bins$upper > c_val])
    }

    key <- as.character(c_val)
    exp <- expected_counts[[key]]
    expect_equal(c(TP = tp, FP = fp, TN = tn, FN = fn), exp)
  }
})


test_that("important PPCR tie fixture preserves requested value and calculates realized PPCR", {
  # CROSS-LANGUAGE STATISTICAL CONTRACT PPCR FIXTURE
  scores <- c(0.1, 0.2, 0.5, 0.5, 0.8, 0.9)
  outcomes <- c(0, 0, 1, 0, 1, 1)

  res <- prepare_probs_distribution_data(
    probs = list(scores),
    reals = list(outcomes),
    by = 0.5,
    stratified_by = "ppcr"
  )

  ops <- res$operating_points
  ppcr_05_row <- dplyr::filter(ops, value == 0.5)

  expect_equal(nrow(ppcr_05_row), 1)
  expect_equal(ppcr_05_row$value, 0.5)
  expect_equal(ppcr_05_row$cutoff, 0.5)
  expect_equal(ppcr_05_row$realized_ppcr, 1 / 3, tolerance = 1e-5)
})


# Helper to verify exact equivalence between reconstructed counts from bins and
# prepare_performance_data() rows for all operating points in an evaluation result
expect_bins_reconstruction_equals_perf_data <- function(probs, reals, by = 0.01, stratified_by = "probability_threshold") {
  res <- prepare_probs_distribution_data(
    probs = probs,
    reals = reals,
    by = by,
    stratified_by = stratified_by
  )

  perf <- prepare_performance_data(
    probs = probs,
    reals = reals,
    by = by,
    stratified_by = stratified_by
  )

  bins <- res$bins
  ops <- res$operating_points

  # Process per evaluation
  evals <- unique(ops$evaluation)

  for (e in evals) {
    ops_e <- dplyr::filter(ops, evaluation == e)
    bins_e <- dplyr::filter(bins, evaluation == e)
    perf_e <- if ("model" %in% names(perf) && e %in% perf$model) {
      dplyr::filter(perf, model == e)
    } else if ("population" %in% names(perf) && e %in% perf$population) {
      dplyr::filter(perf, population == e)
    } else {
      perf
    }

    # Verify length of ops_e matches perf_e
    expect_equal(nrow(ops_e), nrow(perf_e))

    for (k in seq_len(nrow(ops_e))) {
      op_row <- ops_e[k, ]
      perf_row <- perf_e[k, ]

      c_val <- op_row$cutoff
      req_val <- op_row$value

      if (stratified_by == "probability_threshold") {
        if (c_val == 0) {
          tp <- sum(bins_e$n_positive)
          fp <- sum(bins_e$n_negative)
          tn <- 0
          fn <- 0
        } else {
          tn <- sum(bins_e$n_negative[bins_e$upper <= c_val])
          fn <- sum(bins_e$n_positive[bins_e$upper <= c_val])
          tp <- sum(bins_e$n_positive[bins_e$upper > c_val])
          fp <- sum(bins_e$n_negative[bins_e$upper > c_val])
        }
      } else {
        # PPCR stratification
        if (req_val == 1 || c_val == 0) {
          tp <- sum(bins_e$n_positive)
          fp <- sum(bins_e$n_negative)
          tn <- 0
          fn <- 0
        } else {
          tn <- sum(bins_e$n_negative[bins_e$upper <= c_val])
          fn <- sum(bins_e$n_positive[bins_e$upper <= c_val])
          tp <- sum(bins_e$n_positive[bins_e$upper > c_val])
          fp <- sum(bins_e$n_negative[bins_e$upper > c_val])
        }
      }

      expect_equal(tp, unname(perf_row$TP), label = sprintf("TP for eval %s at cutoff %g", e, c_val))
      expect_equal(fp, unname(perf_row$FP), label = sprintf("FP for eval %s at cutoff %g", e, c_val))
      expect_equal(tn, unname(perf_row$TN), label = sprintf("TN for eval %s at cutoff %g", e, c_val))
      expect_equal(fn, unname(perf_row$FN), label = sprintf("FN for eval %s at cutoff %g", e, c_val))
    }
  }
}


test_that("statistical scenario 1: distinct scores with mixed outcomes", {
  p <- list(c(0.1, 0.3, 0.6, 0.8))
  r <- list(c(0, 1, 0, 1))
  expect_bins_reconstruction_equals_perf_data(p, r, by = 0.1)
})


test_that("statistical scenario 2: threshold exactly equal to an observed score", {
  p <- list(c(0.2, 0.5, 0.8))
  r <- list(c(0, 1, 1))
  res <- prepare_probs_distribution_data(p, r, by = 0.1)
  expect_bins_reconstruction_equals_perf_data(p, r, by = 0.1)
  # At threshold 0.5, score 0.5 is predicted negative
  op_05 <- dplyr::filter(res$operating_points, cutoff == 0.5)
  expect_equal(op_05$realized_ppcr, 1 / 3)
})


test_that("statistical scenario 3: partially tied scores containing both outcomes", {
  p <- list(c(0.1, 0.4, 0.4, 0.4, 0.9))
  r <- list(c(0, 1, 0, 1, 1))
  expect_bins_reconstruction_equals_perf_data(p, r, by = 0.1)
  expect_bins_reconstruction_equals_perf_data(p, r, by = 0.2, stratified_by = "ppcr")
})


test_that("statistical scenario 4: all scores tied", {
  p <- list(c(0.5, 0.5, 0.5, 0.5))
  r <- list(c(0, 1, 0, 1))
  expect_bins_reconstruction_equals_perf_data(p, r, by = 0.1)
  expect_bins_reconstruction_equals_perf_data(p, r, by = 0.25, stratified_by = "ppcr")
})


test_that("statistical scenario 5: scores equal to zero", {
  p <- list(c(0.0, 0.0, 0.2, 0.7))
  r <- list(c(0, 1, 0, 1))
  res <- prepare_probs_distribution_data(p, r, by = 0.1)
  expect_bins_reconstruction_equals_perf_data(p, r, by = 0.1)

  # Check zero-mass interval [0, 0]
  b0 <- res$bins[1, ]
  expect_equal(b0$lower, 0)
  expect_equal(b0$upper, 0)
  expect_true(b0$include_lower)
  expect_true(b0$include_upper)
  expect_equal(b0$n_positive, 1)
  expect_equal(b0$n_negative, 1)
})


test_that("statistical scenario 6: scores equal to one", {
  p <- list(c(0.1, 0.5, 1.0, 1.0))
  r <- list(c(0, 1, 0, 1))
  res <- prepare_probs_distribution_data(p, r, by = 0.1)
  expect_bins_reconstruction_equals_perf_data(p, r, by = 0.1)

  # Check interval ending at 1.0 contains the score=1 observations
  last_bin <- res$bins[nrow(res$bins), ]
  expect_equal(last_bin$upper, 1.0)
})


test_that("statistical scenario 7: cutoff zero handling", {
  p <- list(c(0.0, 0.3, 0.8))
  r <- list(c(1, 0, 1))
  res <- prepare_probs_distribution_data(p, r, by = 0.1)
  op_0 <- dplyr::filter(res$operating_points, cutoff == 0)
  expect_equal(op_0$realized_ppcr, 1.0)
  expect_bins_reconstruction_equals_perf_data(p, r, by = 0.1)
})


test_that("statistical scenario 8: cutoff one when it exists in current grid", {
  p <- list(c(0.2, 0.6, 0.9))
  r <- list(c(0, 1, 1))
  res <- prepare_probs_distribution_data(p, r, by = 0.1)
  op_1 <- dplyr::filter(res$operating_points, cutoff == 1.0)
  expect_equal(nrow(op_1), 1)
  expect_equal(op_1$realized_ppcr, 0.0)
  expect_bins_reconstruction_equals_perf_data(p, r, by = 0.1)
})


test_that("statistical scenario 9: all-positive outcomes", {
  p <- list(c(0.1, 0.4, 0.7))
  r <- list(c(1, 1, 1))
  expect_bins_reconstruction_equals_perf_data(p, r, by = 0.1)
  expect_bins_reconstruction_equals_perf_data(p, r, by = 0.25, stratified_by = "ppcr")
})


test_that("statistical scenario 10: all-negative outcomes", {
  p <- list(c(0.2, 0.5, 0.8))
  r <- list(c(0, 0, 0))
  expect_bins_reconstruction_equals_perf_data(p, r, by = 0.1)
  expect_bins_reconstruction_equals_perf_data(p, r, by = 0.25, stratified_by = "ppcr")
})


test_that("statistical scenario 11: one unnamed model/population", {
  p <- list(c(0.1, 0.5, 0.9))
  r <- list(c(0, 1, 1))
  res <- prepare_probs_distribution_data(p, r)

  expect_equal(res$bins$evaluation[1], "model")
  expect_equal(res$bins$model[1], "model")
  expect_equal(res$bins$population[1], "population")
  expect_bins_reconstruction_equals_perf_data(p, r)
})


test_that("statistical scenario 12: several named models sharing one outcome vector", {
  p <- list(
    "Model Alpha" = c(0.1, 0.4, 0.8),
    "Model Beta"  = c(0.2, 0.3, 0.9)
  )
  r <- list(c(0, 1, 1))
  res <- prepare_probs_distribution_data(p, r)

  expect_equal(unique(res$bins$evaluation), c("Model Alpha", "Model Beta"))
  expect_equal(unique(res$bins$model), c("Model Alpha", "Model Beta"))
  expect_equal(unique(res$bins$population), "population")
  expect_bins_reconstruction_equals_perf_data(p, r)
})


test_that("statistical scenario 13: several named populations with matched outcome vectors", {
  p <- list(
    "Train" = c(0.1, 0.4, 0.8),
    "Test"  = c(0.2, 0.3, 0.9)
  )
  r <- list(
    "Train" = c(0, 1, 1),
    "Test"  = c(1, 0, 1)
  )
  res <- prepare_probs_distribution_data(p, r)

  expect_equal(unique(res$bins$evaluation), c("Train", "Test"))
  expect_equal(unique(res$bins$population), c("Train", "Test"))
  expect_bins_reconstruction_equals_perf_data(p, r)
})


test_that("statistical scenario 14: by = 0.5", {
  p <- list(c(0.1, 0.3, 0.7, 0.9))
  r <- list(c(0, 1, 0, 1))
  expect_bins_reconstruction_equals_perf_data(p, r, by = 0.5)
  expect_bins_reconstruction_equals_perf_data(p, r, by = 0.5, stratified_by = "ppcr")
})


test_that("statistical scenario 15: default by = 0.01", {
  p <- list(example_dat$estimated_probabilities)
  r <- list(example_dat$outcome)
  expect_bins_reconstruction_equals_perf_data(p, r, by = 0.01)
  expect_bins_reconstruction_equals_perf_data(p, r, by = 0.01, stratified_by = "ppcr")
})


test_that("statistical scenario 16: by that does not divide 1.0 exactly", {
  p <- list(c(0.1, 0.4, 0.7, 0.9))
  r <- list(c(0, 1, 0, 1))
  expect_bins_reconstruction_equals_perf_data(p, r, by = 0.3)
  expect_bins_reconstruction_equals_perf_data(p, r, by = 0.3, stratified_by = "ppcr")
})


test_that("statistical scenario 17: requested versus realized PPCR under ties", {
  p <- list(c(0.2, 0.5, 0.5, 0.5, 0.8))
  r <- list(c(0, 1, 0, 1, 1))
  res <- prepare_probs_distribution_data(p, r, by = 0.2, stratified_by = "ppcr")

  op <- res$operating_points
  # Find row where value requested is 0.4
  row_04 <- dplyr::filter(op, value == 0.4)
  expect_equal(row_04$cutoff, 0.5)
  expect_equal(row_04$realized_ppcr, 1 / 5) # only score 0.8 > 0.5
  expect_bins_reconstruction_equals_perf_data(p, r, by = 0.2, stratified_by = "ppcr")
})


test_that("statistical scenario 18: repeated quantile cutoffs in PPCR stratification", {
  p <- list(c(0.1, 0.1, 0.1, 0.9))
  r <- list(c(0, 0, 1, 1))
  res <- prepare_probs_distribution_data(p, r, by = 0.25, stratified_by = "ppcr")

  expect_bins_reconstruction_equals_perf_data(p, r, by = 0.25, stratified_by = "ppcr")
})


test_that("statistical scenario 19: totals across bins preserve exact sample totals", {
  p <- list(example_dat$estimated_probabilities)
  r <- list(example_dat$outcome)
  res <- prepare_probs_distribution_data(p, r, by = 0.05)

  bins <- res$bins
  expect_equal(sum(bins$n_positive), sum(r[[1]]))
  expect_equal(sum(bins$n_negative), sum(r[[1]] == 0))
  expect_equal(sum(bins$n_positive + bins$n_negative), length(p[[1]]))
})


test_that("statistical scenario 20: deterministic row ordering in returned list", {
  p <- list(
    "M1" = c(0.1, 0.4, 0.9),
    "M2" = c(0.2, 0.5, 0.8)
  )
  r <- list(c(0, 1, 1))

  res1 <- prepare_probs_distribution_data(p, r, by = 0.1)
  res2 <- prepare_probs_distribution_data(p, r, by = 0.1)

  expect_identical(res1, res2)
  expect_equal(res1$bins$evaluation[1], "M1")
  expect_equal(res1$operating_points$evaluation[1], "M1")
})


test_that("input validation stops on invalid probabilities or arguments", {
  expect_error(
    prepare_probs_distribution_data(list(c(0.1, 1.2)), list(c(0, 1))),
    "out of the range"
  )

  expect_error(
    prepare_probs_distribution_data(list(c(0.1, 0.5)), list(c(0, 1)), stratified_by = "invalid"),
    "'arg' should be one of"
  )
})
