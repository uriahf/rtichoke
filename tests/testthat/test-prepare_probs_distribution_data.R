# Future R/Python Statistical Contract
# The golden fixtures and invariant tests in this file define the exact semantics
# for static binary probability distribution data preparation.
# Python implementations must reproduce these exact boundaries, counts, and
# realized PPCR calculations.

# Reusable Golden Fixture Objects
GOLDEN_THRESHOLD_SCORES <- c(0.0, 0.2, 0.5, 0.5, 0.8, 1.0)
GOLDEN_THRESHOLD_OUTCOMES <- c(0, 1, 1, 0, 1, 0)

GOLDEN_PPCR_SCORES <- c(0.1, 0.2, 0.5, 0.5, 0.8, 0.9)
GOLDEN_PPCR_OUTCOMES <- c(0, 0, 1, 0, 1, 1)


test_that("golden threshold fixture matches expected static classification counts", {
  # CROSS-LANGUAGE STATISTICAL CONTRACT FIXTURE
  res <- prepare_probs_distribution_data(
    probs = list(GOLDEN_THRESHOLD_SCORES),
    reals = list(GOLDEN_THRESHOLD_OUTCOMES),
    by = 0.1,
    stratified_by = "probability_threshold"
  )

  operating_points <- res$operating_points
  bins <- res$bins

  target_cutoffs <- c(0.0, 0.2, 0.5, 1.0)
  sub_ops <- dplyr::filter(operating_points, .data$cutoff %in% target_cutoffs)

  expect_equal(sub_ops$cutoff, target_cutoffs)
  expect_equal(
    sub_ops$realized_ppcr,
    c(1.0, 2 / 3, 1 / 3, 0.0),
    tolerance = 1e-5
  )

  # Reconstruct TP, FP, TN, FN from bins for each target cutoff
  expected_counts <- list(
    "0" = c(TP = 3, FP = 3, TN = 0, FN = 0),
    "0.2" = c(TP = 2, FP = 2, TN = 1, FN = 1),
    "0.5" = c(TP = 1, FP = 1, TN = 2, FN = 2),
    "1" = c(TP = 0, FP = 0, TN = 3, FN = 3)
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
  res <- prepare_probs_distribution_data(
    probs = list(GOLDEN_PPCR_SCORES),
    reals = list(GOLDEN_PPCR_OUTCOMES),
    by = 0.5,
    stratified_by = "ppcr"
  )

  operating_points <- res$operating_points
  ppcr_05_row <- dplyr::filter(operating_points, .data$value == 0.5)

  expect_equal(nrow(ppcr_05_row), 1)
  expect_equal(ppcr_05_row$value, 0.5)
  expect_equal(ppcr_05_row$cutoff, 0.5)
  expect_equal(ppcr_05_row$realized_ppcr, 1 / 3, tolerance = 1e-5)
})


# Helper to verify exact equivalence between reconstructed counts from bins and
# prepare_performance_data() rows for all operating points matching by explicit identity
expect_bins_reconstruction_equals_perf_data <- function(
  probs,
  reals,
  by = 0.01,
  stratified_by = "probability_threshold"
) {
  distribution_data <- prepare_probs_distribution_data(
    probs = probs,
    reals = reals,
    by = by,
    stratified_by = stratified_by
  )

  performance_data <- prepare_performance_data(
    probs = probs,
    reals = reals,
    by = by,
    stratified_by = stratified_by
  )

  bins <- distribution_data$bins
  operating_points <- distribution_data$operating_points

  unique_evaluations <- unique(operating_points$evaluation)

  for (eval_id in unique_evaluations) {
    eval_ops <- dplyr::filter(operating_points, .data$evaluation == eval_id)
    eval_bins <- dplyr::filter(bins, .data$evaluation == eval_id)

    eval_perf <- if (
      "model" %in%
        names(performance_data) &&
        eval_id %in% performance_data$model
    ) {
      dplyr::filter(performance_data, .data$model == eval_id)
    } else if (
      "population" %in%
        names(performance_data) &&
        eval_id %in% performance_data$population
    ) {
      dplyr::filter(performance_data, .data$population == eval_id)
    } else {
      performance_data
    }

    expect_equal(nrow(eval_ops), nrow(eval_perf))

    for (k in seq_len(nrow(eval_ops))) {
      op_row <- eval_ops[k, ]
      effective_cutoff <- op_row$cutoff
      requested_value <- op_row$value

      # Explicit identity join with production performance row
      perf_row <- if (stratified_by == "probability_threshold") {
        dplyr::filter(
          eval_perf,
          .data$probability_threshold == effective_cutoff
        )
      } else {
        dplyr::filter(eval_perf, .data$ppcr == requested_value)
      }

      expect_equal(nrow(perf_row), 1L)

      if (stratified_by == "probability_threshold") {
        if (effective_cutoff == 0) {
          tp <- sum(eval_bins$n_positive)
          fp <- sum(eval_bins$n_negative)
          tn <- 0
          fn <- 0
        } else {
          tn <- sum(eval_bins$n_negative[eval_bins$upper <= effective_cutoff])
          fn <- sum(eval_bins$n_positive[eval_bins$upper <= effective_cutoff])
          tp <- sum(eval_bins$n_positive[eval_bins$upper > effective_cutoff])
          fp <- sum(eval_bins$n_negative[eval_bins$upper > effective_cutoff])
        }
      } else {
        # PPCR stratification
        if (requested_value == 1 || effective_cutoff == 0) {
          tp <- sum(eval_bins$n_positive)
          fp <- sum(eval_bins$n_negative)
          tn <- 0
          fn <- 0
        } else {
          tn <- sum(eval_bins$n_negative[eval_bins$upper <= effective_cutoff])
          fn <- sum(eval_bins$n_positive[eval_bins$upper <= effective_cutoff])
          tp <- sum(eval_bins$n_positive[eval_bins$upper > effective_cutoff])
          fp <- sum(eval_bins$n_negative[eval_bins$upper > effective_cutoff])
        }
      }

      expect_equal(
        tp,
        unname(perf_row$TP),
        label = sprintf(
          "TP for eval %s at cutoff %g (value %g)",
          eval_id,
          effective_cutoff,
          requested_value
        )
      )
      expect_equal(
        fp,
        unname(perf_row$FP),
        label = sprintf(
          "FP for eval %s at cutoff %g (value %g)",
          eval_id,
          effective_cutoff,
          requested_value
        )
      )
      expect_equal(
        tn,
        unname(perf_row$TN),
        label = sprintf(
          "TN for eval %s at cutoff %g (value %g)",
          eval_id,
          effective_cutoff,
          requested_value
        )
      )
      expect_equal(
        fn,
        unname(perf_row$FN),
        label = sprintf(
          "FN for eval %s at cutoff %g (value %g)",
          eval_id,
          effective_cutoff,
          requested_value
        )
      )
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
  op_05 <- dplyr::filter(res$operating_points, .data$cutoff == 0.5)
  expect_equal(op_05$realized_ppcr, 1 / 3)
})


test_that("statistical scenario 3: partially tied scores containing both outcomes", {
  p <- list(c(0.1, 0.4, 0.4, 0.4, 0.9))
  r <- list(c(0, 1, 0, 1, 1))
  expect_bins_reconstruction_equals_perf_data(p, r, by = 0.1)
  expect_bins_reconstruction_equals_perf_data(
    p,
    r,
    by = 0.2,
    stratified_by = "ppcr"
  )
})


test_that("statistical scenario 4: all scores tied", {
  p <- list(c(0.5, 0.5, 0.5, 0.5))
  r <- list(c(0, 1, 0, 1))
  expect_bins_reconstruction_equals_perf_data(p, r, by = 0.1)
  expect_bins_reconstruction_equals_perf_data(
    p,
    r,
    by = 0.25,
    stratified_by = "ppcr"
  )
})


test_that("statistical scenario 5: scores equal to zero", {
  p <- list(c(0.0, 0.0, 0.2, 0.7))
  r <- list(c(0, 1, 0, 1))
  res <- prepare_probs_distribution_data(p, r, by = 0.1)
  expect_bins_reconstruction_equals_perf_data(p, r, by = 0.1)

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

  last_bin <- res$bins[nrow(res$bins), ]
  expect_equal(last_bin$upper, 1.0)
})


test_that("statistical scenario 7: cutoff zero handling", {
  p <- list(c(0.0, 0.3, 0.8))
  r <- list(c(1, 0, 1))
  res <- prepare_probs_distribution_data(p, r, by = 0.1)
  op_0 <- dplyr::filter(res$operating_points, .data$cutoff == 0)
  expect_equal(op_0$realized_ppcr, 1.0)
  expect_bins_reconstruction_equals_perf_data(p, r, by = 0.1)
})


test_that("statistical scenario 8: cutoff one when it exists in current grid", {
  p <- list(c(0.2, 0.6, 0.9))
  r <- list(c(0, 1, 1))
  res <- prepare_probs_distribution_data(p, r, by = 0.1)
  op_1 <- dplyr::filter(res$operating_points, .data$cutoff == 1.0)
  expect_equal(nrow(op_1), 1)
  expect_equal(op_1$realized_ppcr, 0.0)
  expect_bins_reconstruction_equals_perf_data(p, r, by = 0.1)
})


test_that("statistical scenario 9: all-positive outcomes", {
  p <- list(c(0.1, 0.4, 0.7))
  r <- list(c(1, 1, 1))
  expect_bins_reconstruction_equals_perf_data(p, r, by = 0.1)
  expect_bins_reconstruction_equals_perf_data(
    p,
    r,
    by = 0.25,
    stratified_by = "ppcr"
  )
})


test_that("statistical scenario 10: all-negative outcomes", {
  p <- list(c(0.2, 0.5, 0.8))
  r <- list(c(0, 0, 0))
  expect_bins_reconstruction_equals_perf_data(p, r, by = 0.1)
  expect_bins_reconstruction_equals_perf_data(
    p,
    r,
    by = 0.25,
    stratified_by = "ppcr"
  )
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
    "Model Beta" = c(0.2, 0.3, 0.9)
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
    "Test" = c(0.2, 0.3, 0.9)
  )
  r <- list(
    "Train" = c(0, 1, 1),
    "Test" = c(1, 0, 1)
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
  expect_bins_reconstruction_equals_perf_data(
    p,
    r,
    by = 0.5,
    stratified_by = "ppcr"
  )
})


test_that("statistical scenario 15: default by = 0.01", {
  p <- list(example_dat$estimated_probabilities)
  r <- list(example_dat$outcome)
  expect_bins_reconstruction_equals_perf_data(p, r, by = 0.01)
  expect_bins_reconstruction_equals_perf_data(
    p,
    r,
    by = 0.01,
    stratified_by = "ppcr"
  )
})


test_that("statistical scenario 16: by that does not divide 1.0 exactly", {
  p <- list(c(0.1, 0.4, 0.7, 0.9))
  r <- list(c(0, 1, 0, 1))
  expect_bins_reconstruction_equals_perf_data(p, r, by = 0.3)
  expect_bins_reconstruction_equals_perf_data(
    p,
    r,
    by = 0.3,
    stratified_by = "ppcr"
  )
})


test_that("statistical scenario 17: requested versus realized PPCR under ties", {
  p <- list(c(0.2, 0.5, 0.5, 0.5, 0.8))
  r <- list(c(0, 1, 0, 1, 1))
  res <- prepare_probs_distribution_data(p, r, by = 0.2, stratified_by = "ppcr")

  op <- res$operating_points
  row_04 <- dplyr::filter(op, .data$value == 0.4)
  expect_equal(row_04$cutoff, 0.5)
  expect_equal(row_04$realized_ppcr, 1 / 5)
  expect_bins_reconstruction_equals_perf_data(
    p,
    r,
    by = 0.2,
    stratified_by = "ppcr"
  )
})


test_that("statistical scenario 18: repeated quantile cutoffs in PPCR stratification", {
  p <- list(c(0.1, 0.1, 0.1, 0.9))
  r <- list(c(0, 0, 1, 1))
  expect_bins_reconstruction_equals_perf_data(
    p,
    r,
    by = 0.25,
    stratified_by = "ppcr"
  )
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


test_that("duplicated evaluation names throw clear error", {
  # Duplicated model names produce duplicate evaluation identities
  probs_dup <- list(
    "Model_1" = c(0.1, 0.4),
    "Model_1" = c(0.2, 0.5)
  )
  reals_dup <- list(c(0, 1))

  expect_error(
    prepare_probs_distribution_data(probs_dup, reals_dup),
    "Evaluation names must be unique across models/populations."
  )
})


test_that("input validation stops on invalid probabilities or arguments", {
  expect_error(
    prepare_probs_distribution_data(list(c(0.1, 1.2)), list(c(0, 1))),
    "out of the range"
  )

  expect_error(
    prepare_probs_distribution_data(
      list(c(0.1, 0.5)),
      list(c(0, 1)),
      stratified_by = "invalid"
    ),
    "'arg' should be one of"
  )
})


# -----------------------------------------------------------------------------
# PRODUCER-OWNED rank_bins STATISTICAL CONTRACT & INVARIANCE TESTS
# -----------------------------------------------------------------------------

test_that("primary golden fixture matches exact statistical rank_bins table", {
  # INDEPENDENT PRIMARY GOLDEN FIXTURE (N = 9, by = 0.20)
  p <- list(c(0.00, 0.15, 0.30, 0.50, 0.50, 0.50, 0.65, 0.80, 1.00))
  r <- list(c(0, 1, 0, 1, 0, 1, 1, 0, 1))
  by <- 0.20

  res <- prepare_probs_distribution_data(p, r, by = by)

  expect_true("rank_bins" %in% names(res))
  rb <- res$rank_bins

  expect_equal(
    names(rb),
    c(
      "evaluation",
      "model",
      "population",
      "rank_lower",
      "rank_upper",
      "n_positive",
      "n_negative"
    )
  )
  expect_equal(nrow(rb), 5L)
  expect_equal(rb$rank_lower, c(0.0, 0.2, 0.4, 0.6, 0.8))
  expect_equal(rb$rank_upper, c(0.2, 0.4, 0.6, 0.8, 1.0))

  # Assert complete exact table, including empty [0.4, 0.6] stratum
  expect_equal(rb$n_positive, c(1L, 2L, 0L, 1L, 1L))
  expect_equal(rb$n_negative, c(1L, 2L, 0L, 0L, 1L))

  # Mass conservation
  expect_equal(sum(rb$n_positive), 5L)
  expect_equal(sum(rb$n_negative), 4L)
  expect_equal(sum(rb$n_positive + rb$n_negative), 9L)
})

test_that("secondary golden fixture N < q matches exact statistical rank_bins table", {
  # INDEPENDENT SECONDARY GOLDEN FIXTURE (N = 3, by = 0.20)
  p <- list(c(0.10, 0.50, 0.90))
  r <- list(c(0, 1, 1))
  by <- 0.20

  res <- prepare_probs_distribution_data(p, r, by = by)
  rb <- res$rank_bins

  expect_equal(nrow(rb), 5L)
  expect_equal(rb$rank_lower, c(0.0, 0.2, 0.4, 0.6, 0.8))
  expect_equal(rb$rank_upper, c(0.2, 0.4, 0.6, 0.8, 1.0))

  # Assert complete table including both empty strata [0.2, 0.4] and [0.6, 0.8]
  expect_equal(rb$n_positive, c(0L, 0L, 1L, 0L, 1L))
  expect_equal(rb$n_negative, c(1L, 0L, 0L, 0L, 0L))

  # Mass conservation
  expect_equal(sum(rb$n_positive), 2L)
  expect_equal(sum(rb$n_negative), 1L)
  expect_equal(sum(rb$n_positive + rb$n_negative), 3L)
})

test_that("rank_bins demonstrates row permutation order invariance including tied score group", {
  # Primary fixture with observation permutation permuting score=0.50 tied group
  p <- c(0.00, 0.15, 0.30, 0.50, 0.50, 0.50, 0.65, 0.80, 1.00)
  r <- c(0, 1, 0, 1, 0, 1, 1, 0, 1)
  by <- 0.20

  res_orig <- prepare_probs_distribution_data(list(p), list(r), by = by)

  # Reorder rows: shuffle tied group (indices 4, 5, 6 -> 5, 4, 6) and other elements
  perm_idx <- c(5, 1, 9, 4, 6, 2, 7, 3, 8)
  res_perm <- prepare_probs_distribution_data(
    list(p[perm_idx]),
    list(r[perm_idx]),
    by = by
  )

  expect_identical(res_orig$rank_bins, res_perm$rank_bins)
})

test_that("rank_bins output schema and mass conservation for continuous predictions", {
  p <- list(c(0.05, 0.15, 0.25, 0.35, 0.45, 0.55, 0.65, 0.75, 0.85, 0.95))
  r <- list(c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1))
  by <- 0.2

  res <- prepare_probs_distribution_data(p, r, by = by)

  expect_true("rank_bins" %in% names(res))
  rb <- res$rank_bins

  expect_equal(
    names(rb),
    c(
      "evaluation",
      "model",
      "population",
      "rank_lower",
      "rank_upper",
      "n_positive",
      "n_negative"
    )
  )
  expect_equal(nrow(rb), 5L)
  expect_equal(rb$rank_lower, c(0.0, 0.2, 0.4, 0.6, 0.8))
  expect_equal(rb$rank_upper, c(0.2, 0.4, 0.6, 0.8, 1.0))

  # Mass conservation
  expect_equal(sum(rb$n_positive), sum(r[[1]] == 1))
  expect_equal(sum(rb$n_negative), sum(r[[1]] == 0))
  expect_equal(sum(rb$n_positive + rb$n_negative), length(p[[1]]))
})

test_that("rank_bins stratification-invariance across probability_threshold and ppcr modes", {
  p <- list(c(0.1, 0.2, 0.5, 0.5, 0.8, 0.9))
  r <- list(c(0, 0, 1, 0, 1, 1))
  by <- 0.2

  res_thresh <- prepare_probs_distribution_data(
    p,
    r,
    by = by,
    stratified_by = "probability_threshold"
  )
  res_ppcr <- prepare_probs_distribution_data(
    p,
    r,
    by = by,
    stratified_by = "ppcr"
  )

  expect_identical(res_thresh$rank_bins, res_ppcr$rank_bins)
})

test_that("rank_bins handles ties, repeated boundaries, empty strata, and zero/one scores", {
  # Deterministic fixture exercising ties, 0, 1, repeated cutoffs, empty strata
  p <- list(c(0.0, 0.1, 0.1, 0.1, 0.5, 0.9, 1.0))
  r <- list(c(0, 1, 0, 1, 1, 0, 1))
  by <- 0.2

  res <- prepare_probs_distribution_data(p, r, by = by)
  rb <- res$rank_bins

  # Complete requested grid retained (5 strata for by = 0.2)
  expect_equal(nrow(rb), 5L)
  expect_equal(rb$rank_lower, c(0.0, 0.2, 0.4, 0.6, 0.8))
  expect_equal(rb$rank_upper, c(0.2, 0.4, 0.6, 0.8, 1.0))

  # Strata 2 (0.2-0.4) and 3 (0.4-0.6) receive 0 observations due to repeated quantile boundary
  expect_equal(rb$n_positive[2], 0L)
  expect_equal(rb$n_negative[2], 0L)
  expect_equal(rb$n_positive[3], 0L)
  expect_equal(rb$n_negative[3], 0L)

  # Identical scores 0.1 are all assigned to stratum 1 (0.0-0.2) and never split
  expect_equal(rb$n_positive[1], 2L)
  expect_equal(rb$n_negative[1], 2L)

  # Exact score 0.0 is assigned to stratum 1
  # Exact score 1.0 is assigned to stratum 5 (0.8-1.0)
  expect_equal(rb$n_positive[5], 1L)
  expect_equal(rb$n_negative[5], 1L)

  # Mass conservation
  expect_equal(sum(rb$n_positive), 4L)
  expect_equal(sum(rb$n_negative), 3L)
  expect_equal(sum(rb$n_positive + rb$n_negative), 7L)
})

test_that("backward-compatibility: existing bins and operating_points outputs remain unchanged", {
  p <- list(GOLDEN_THRESHOLD_SCORES)
  r <- list(GOLDEN_THRESHOLD_OUTCOMES)
  by <- 0.1

  res_thresh <- prepare_probs_distribution_data(
    p,
    r,
    by = by,
    stratified_by = "probability_threshold"
  )
  res_ppcr <- prepare_probs_distribution_data(
    p,
    r,
    by = by,
    stratified_by = "ppcr"
  )

  # Existing outputs must be identical to pre-rankBins assertions
  expect_equal(
    names(res_thresh$bins),
    c(
      "evaluation",
      "model",
      "population",
      "lower",
      "upper",
      "include_lower",
      "include_upper",
      "n_positive",
      "n_negative"
    )
  )
  expect_equal(
    names(res_thresh$operating_points),
    c(
      "evaluation",
      "model",
      "population",
      "type",
      "value",
      "cutoff",
      "realized_ppcr"
    )
  )

  target_cutoffs <- c(0.0, 0.2, 0.5, 1.0)
  sub_ops <- dplyr::filter(
    res_thresh$operating_points,
    .data$cutoff %in% target_cutoffs
  )
  expect_equal(sub_ops$cutoff, target_cutoffs)
  expect_equal(
    sub_ops$realized_ppcr,
    c(1.0, 2 / 3, 1 / 3, 0.0),
    tolerance = 1e-5
  )
})
