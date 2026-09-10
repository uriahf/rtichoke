#' Assign Observations to Probability Quantile Strata
#'
#' Assigns individual observations to empirical score-quantile strata
#' ordered from low to high predicted probability.
#'
#' @param probs Numeric vector of predicted probabilities.
#' @param by Numeric scalar representing the step size for quantile binning (e.g. 0.01 or 0.20).
#'
#' @details
#' PROBABILITY-QUANTILE STRATUM vs PPCR OPERATING POINT:
#' An observation-level probability quantile stratum represents an individual
#' observation's location within the empirical prediction-score distribution.
#' In contrast, a PPCR operating point is a requested population classification
#' operating point used by prepare_performance_data(). They are related through
#' the score distribution but are distinct concepts.
#'
#' @return An ordered factor of stratum labels ordered low to high predicted probability.
#' @keywords internal
assign_probability_quantile_strata <- function(probs, by) {
  q <- as.integer(round(1 / by))

  probs_vec <- as.numeric(probs)
  probs_seq <- seq(0, 1, length.out = q + 1)
  edges <- as.numeric(stats::quantile(
    probs_vec,
    probs = probs_seq,
    type = 7,
    names = FALSE
  ))

  edges <- cummax(edges)
  edges[1] <- 0.0
  edges[length(edges)] <- 1.0

  internal_bins <- edges[2:(length(edges) - 1)]

  # Reproduce np.digitize(probs, bins=internal_bins, right=True) semantics:
  # Counts how many internal bin cutoffs the probability strictly exceeds
  bin_idx <- vapply(probs_vec, function(x) sum(x > internal_bins), integer(1))

  by_str <- as.character(by)
  decimals <- if (grepl("\\.", by_str)) nchar(sub(".*\\.", "", by_str)) else 0

  grid_vals <- seq(by, 1.0, length.out = q)
  labels <- sprintf(paste0("%.", decimals, "f"), grid_vals)

  assigned_labels <- labels[bin_idx + 1]

  factor(assigned_labels, levels = labels, ordered = TRUE)
}
