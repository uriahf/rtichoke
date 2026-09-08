#' Internal Static Prediction Distribution Preparation
#'
#' Prepares exact prediction score distribution data (bins and operating points)
#' for static binary outcome models and populations. Reuses production
#' `prepare_performance_data()` to ensure exact cutoff and PPCR alignment.
#'
#' @param probs A list of numeric vectors of estimated probabilities (one vector per
#'   model or population).
#' @param reals A list of numeric vectors of binary outcome indicators (0 or 1).
#' @param by Increment of the threshold or PPCR evaluation sequence (default 0.01).
#' @param stratified_by Operating point stratification metric: `"probability_threshold"`
#'   or `"ppcr"`.
#'
#' @return A named list with two tidy tibbles:
#'   \item{bins}{Exact aggregate score interval counts for each evaluation.}
#'   \item{operating_points}{Selectable operating points with effective cutoffs and
#'     realized PPCR.}
#' @keywords internal
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

  eval_meta <- build_evaluation_metadata(probs, reals)
  n_evals <- nrow(eval_meta)

  bins_list <- vector("list", n_evals)
  op_list <- vector("list", n_evals)

  for (i in seq_len(n_evals)) {
    p <- probs[[i]]
    r <- if (length(reals) == 1L) reals[[1L]] else reals[[i]]
    m_info <- eval_meta[i, , drop = FALSE]

    perf_eval <- prepare_performance_data(
      probs = list(p),
      reals = list(r),
      by = by,
      stratified_by = stratified_by
    )

    N <- length(p)

    # 1. Operating Points table construction
    if (stratified_by == "probability_threshold") {
      val <- unname(perf_eval$probability_threshold)
      cut <- unname(perf_eval$probability_threshold)
      # Realized PPCR using current static rule:
      # cutoff == 0 -> everyone positive (realized_ppcr = 1)
      # cutoff > 0 -> score > cutoff
      realized_ppcr <- unname(perf_eval$ppcr)
    } else {
      val <- unname(perf_eval$ppcr)
      cut <- unname(perf_eval$probability_threshold)
      realized_ppcr <- purrr::map2_dbl(
        val,
        cut,
        function(v, c) {
          if (v == 1 || c == 0) {
            1.0
          } else {
            sum(p > c) / N
          }
        }
      )
    }

    op_df <- tibble::tibble(
      evaluation = m_info$evaluation,
      model = m_info$model,
      population = m_info$population,
      type = stratified_by,
      value = val,
      cutoff = cut,
      realized_ppcr = realized_ppcr
    )

    # 2. Bins table construction
    cutoffs <- unname(perf_eval$probability_threshold)
    boundaries <- sort(unique(c(0, cutoffs, 1)))
    m_bounds <- length(boundaries)

    # Zero-mass interval [0, 0]
    b0_pos <- as.integer(sum(p == 0 & r == 1))
    b0_neg <- as.integer(sum(p == 0 & r == 0))

    lower_vec <- c(0)
    upper_vec <- c(0)
    inc_lower_vec <- c(TRUE)
    inc_upper_vec <- c(TRUE)
    n_pos_vec <- c(b0_pos)
    n_neg_vec <- c(b0_neg)

    # Subsequent right-closed intervals (boundaries[j], boundaries[j + 1]]
    if (m_bounds > 1L) {
      for (j in seq_len(m_bounds - 1L)) {
        l_val <- boundaries[j]
        u_val <- boundaries[j + 1L]

        in_bin <- p > l_val & p <= u_val
        pos_cnt <- as.integer(sum(in_bin & r == 1))
        neg_cnt <- as.integer(sum(in_bin & r == 0))

        lower_vec <- c(lower_vec, l_val)
        upper_vec <- c(upper_vec, u_val)
        inc_lower_vec <- c(inc_lower_vec, FALSE)
        inc_upper_vec <- c(inc_upper_vec, TRUE)
        n_pos_vec <- c(n_pos_vec, pos_cnt)
        n_neg_vec <- c(n_neg_vec, neg_cnt)
      }
    }

    bins_df <- tibble::tibble(
      evaluation = m_info$evaluation,
      model = m_info$model,
      population = m_info$population,
      lower = lower_vec,
      upper = upper_vec,
      include_lower = inc_lower_vec,
      include_upper = inc_upper_vec,
      n_positive = n_pos_vec,
      n_negative = n_neg_vec
    )

    op_list[[i]] <- op_df
    bins_list[[i]] <- bins_df
  }

  list(
    bins = dplyr::bind_rows(bins_list),
    operating_points = dplyr::bind_rows(op_list)
  )
}
