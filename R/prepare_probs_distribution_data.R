#' Prepare Prediction Distribution Data
#'
#' Internal helper to prepare exact prediction distribution score intervals (`bins`)
#' and operating points (`operating_points`) for static binary outcomes. Reuses production
#' `prepare_performance_data()` as the authoritative source for cutoff grids and metrics.
#'
#' @inheritParams prepare_performance_data
#'
#' @return A named list with two tidy tibbles:
#'   \item{bins}{Exact score intervals covering score space 0 to 1. Includes zero-mass
#'     interval `[0, 0]` and right-closed intervals `(lower, upper]` aligned to effective
#'     cutoffs. Columns: `evaluation`, `model`, `population`, `lower`, `upper`,
#'     `include_lower`, `include_upper`, `n_positive`, `n_negative`.}
#'   \item{operating_points}{Selectable operating points. Columns: `evaluation`,
#'     `model`, `population`, `type`, `value` (requested metric value), `cutoff`
#'     (effective score cutoff), `realized_ppcr` (actual predicted positives fraction).}
#'
#' @details
#' This internal function is used to prepare static prediction distribution data
#' before rendering or contract serialization.
#'
#' @keywords internal
#'
#' @examples
#' # Single model with probability threshold stratification
#' res_single <- rtichoke:::prepare_probs_distribution_data(
#'   probs = list(example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome),
#'   by = 0.1
#' )
#' res_single$operating_points
#' res_single$bins
#'
#' # Multiple models sharing one outcome vector
#' res_multi <- rtichoke:::prepare_probs_distribution_data(
#'   probs = list(
#'     "Model A" = example_dat$estimated_probabilities,
#'     "Model B" = example_dat$random_guess
#'   ),
#'   reals = list(example_dat$outcome),
#'   by = 0.2
#' )
#' res_multi$operating_points
#'
#' # PPCR stratification with tied scores showing requested vs realized PPCR
#' res_ppcr <- rtichoke:::prepare_probs_distribution_data(
#'   probs = list(c(0.1, 0.2, 0.5, 0.5, 0.8, 0.9)),
#'   reals = list(c(0, 0, 1, 0, 1, 1)),
#'   by = 0.5,
#'   stratified_by = "ppcr"
#' )
#' res_ppcr$operating_points
prepare_probs_distribution_data <- function(
  probs,
  reals,
  by = 0.01,
  stratified_by = "probability_threshold"
) {
  check_probs_input(probs)
  check_real_input(reals)

  match.arg(stratified_by, c("probability_threshold", "ppcr"))

  if (any(purrr::map_lgl(probs, ~ any(.x > 1)))) {
    stop("Probabilities mustn't be greater than one ")
  }

  evaluation_metadata <- build_evaluation_metadata(probs, reals)

  if (any(duplicated(evaluation_metadata$evaluation))) {
    stop("Evaluation names must be unique across models/populations.")
  }

  n_evaluations <- nrow(evaluation_metadata)

  eval_results <- purrr::map(
    seq_len(n_evaluations),
    function(i) {
      probabilities <- probs[[i]]
      outcomes <- if (length(reals) == 1L) reals[[1L]] else reals[[i]]
      n_observations <- length(probabilities)
      current_eval_metadata <- evaluation_metadata[i, , drop = FALSE]

      perf_eval <- prepare_performance_data(
        probs = list(probabilities),
        reals = list(outcomes),
        by = by,
        stratified_by = stratified_by
      )

      # 1. Operating Points table
      if (stratified_by == "probability_threshold") {
        operating_point_values <- unname(perf_eval$probability_threshold)
        effective_cutoffs <- unname(perf_eval$probability_threshold)
      } else {
        operating_point_values <- unname(perf_eval$ppcr)
        effective_cutoffs <- unname(perf_eval$probability_threshold)
      }

      realized_ppcr <- unname(perf_eval$predicted_positives / n_observations)

      operating_points <- tibble::tibble(
        evaluation = current_eval_metadata$evaluation,
        model = current_eval_metadata$model,
        population = current_eval_metadata$population,
        type = stratified_by,
        value = operating_point_values,
        cutoff = effective_cutoffs,
        realized_ppcr = realized_ppcr
      )

      # 2. Bins table
      cutoffs <- unname(perf_eval$probability_threshold)
      boundaries <- sort(unique(c(0, cutoffs, 1)))
      n_boundaries <- length(boundaries)

      interval_grid <- tibble::tibble(
        interval_id = 0L:(n_boundaries - 1L),
        lower = c(0, boundaries[-n_boundaries]),
        upper = c(0, boundaries[-1L]),
        include_lower = c(TRUE, rep(FALSE, n_boundaries - 1L)),
        include_upper = rep(TRUE, n_boundaries)
      )

      zero_mask <- probabilities == 0
      obs_interval_id <- integer(n_observations)
      obs_interval_id[zero_mask] <- 0L

      if (any(!zero_mask)) {
        obs_interval_id[!zero_mask] <- as.integer(
          cut(
            probabilities[!zero_mask],
            breaks = boundaries,
            include.lowest = FALSE,
            right = TRUE,
            labels = FALSE
          )
        )
      }

      obs_df <- tibble::tibble(
        interval_id = obs_interval_id,
        outcome = outcomes
      )

      counts_df <- obs_df |>
        dplyr::group_by(.data$interval_id) |>
        dplyr::summarise(
          n_positive = as.integer(sum(.data$outcome == 1)),
          n_negative = as.integer(sum(.data$outcome == 0)),
          .groups = "drop"
        )

      bins <- interval_grid |>
        dplyr::left_join(counts_df, by = "interval_id") |>
        dplyr::mutate(
          n_positive = dplyr::coalesce(.data$n_positive, 0L),
          n_negative = dplyr::coalesce(.data$n_negative, 0L),
          evaluation = current_eval_metadata$evaluation,
          model = current_eval_metadata$model,
          population = current_eval_metadata$population
        ) |>
        dplyr::select(
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

      list(bins = bins, operating_points = operating_points)
    }
  )

  list(
    bins = dplyr::bind_rows(purrr::map(eval_results, "bins")),
    operating_points = dplyr::bind_rows(purrr::map(
      eval_results,
      "operating_points"
    ))
  )
}
