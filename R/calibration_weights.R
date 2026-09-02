#' Prepare calibration bins with optional outcome weights
#'
#' Private vector-level primitive for calibration preparation. Prediction bins
#' and their displayed predicted values are always defined from the target
#' population predictions. Optional `outcome_weights` affect only the observed
#' outcome estimate within each bin.
#'
#' When `outcome_weights` is `NULL`, this delegates to the established
#' `make_deciles_dat()` implementation so factual calibration semantics remain
#' exactly unchanged.
#'
#' @param probs Numeric vector of predicted probabilities.
#' @param reals Numeric vector of binary outcomes.
#' @param outcome_weights Optional non-negative finite weights used only for the
#'   observed outcome mean.
#' @param n_bins Number of equal-frequency prediction bins. The historical
#'   unweighted path currently supports 10 bins only.
#'
#' @return A tibble with the established calibration-bin columns plus
#'   `outcome_weight_sum`. For weighted calls, `y` is the weighted observed
#'   outcome mean while `x` remains the unweighted mean prediction.
#' @noRd
prepare_calibration_bins <- function(
  probs,
  reals,
  outcome_weights = NULL,
  n_bins = 10
) {
  if (length(probs) != length(reals)) {
    stop("`probs` and `reals` must have the same length.", call. = FALSE)
  }

  if (is.null(outcome_weights)) {
    if (!identical(n_bins, 10L) && !identical(n_bins, 10)) {
      stop(
        "The established unweighted calibration path currently supports `n_bins = 10` only.",
        call. = FALSE
      )
    }

    return(
      make_deciles_dat(probs, reals) |>
        dplyr::mutate(outcome_weight_sum = total_obs)
    )
  }

  if (length(outcome_weights) != length(reals)) {
    stop(
      "`outcome_weights` must have the same length as `reals`.",
      call. = FALSE
    )
  }
  if (
    length(n_bins) != 1 ||
      is.na(n_bins) ||
      !is.numeric(n_bins) ||
      n_bins < 1 ||
      n_bins != as.integer(n_bins)
  ) {
    stop("`n_bins` must be a positive integer.", call. = FALSE)
  }
  if (
    any(!is.finite(outcome_weights)) ||
      any(outcome_weights < 0)
  ) {
    stop(
      "`outcome_weights` must contain only finite, non-negative values.",
      call. = FALSE
    )
  }

  dat <- data.frame(
    probs = probs,
    reals = reals,
    outcome_weights = outcome_weights
  )

  if (length(unique(probs)) == 1) {
    dat$quintile <- 1L
  } else {
    dat <- dat |>
      dplyr::mutate(quintile = dplyr::ntile(probs, as.integer(n_bins)))
  }

  bins <- dat |>
    dplyr::group_by(quintile) |>
    dplyr::summarise(
      x = mean(probs),
      outcome_weight_sum = sum(outcome_weights),
      weighted_sum_reals = sum(outcome_weights * reals),
      sum_reals = sum(reals),
      total_obs = dplyr::n(),
      .groups = "drop"
    )

  if (any(bins$outcome_weight_sum <= 0)) {
    stop(
      "Every calibration bin must have positive total `outcome_weights`.",
      call. = FALSE
    )
  }

  bins |>
    dplyr::mutate(
      y = weighted_sum_reals / outcome_weight_sum
    ) |>
    dplyr::select(
      quintile,
      x,
      y,
      sum_reals,
      total_obs,
      outcome_weight_sum,
      weighted_sum_reals
    )
}
