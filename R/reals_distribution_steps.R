#' Create an interactive outcome distribution visualization across discrete steps
#'
#' Prepares observed state counts or censoring-adjusted (Aalen-Johansen) state
#' occupancy probabilities across discrete steps and renders an interactive
#' Outcome Distribution component in the browser using \code{rtichoke_viz}.
#'
#' @param data A data.frame, tibble, or data.table containing subject-step panel data.
#' @param id_col Character string specifying the subject identifier column.
#' @param step_col Character string specifying the discrete step column.
#' @param reals_col Character string specifying the observed outcome/state column.
#' @param reals_order Vector specifying category ordering. If \code{NULL},
#'   ordering defaults to factor levels or sorted observed categories.
#' @param reals_scale Character string specifying the state scale (\code{"auto"},
#'   \code{"nominal"}, or \code{"ordinal"}).
#' @param censoring_heuristic Character string specifying the estimation mode:
#'   \code{"excluded"} (raw observed counts per step) or \code{"adjusted"}
#'   (Aalen-Johansen state occupancy probabilities under right censoring).
#' @param title Character string specifying the chart title.
#' @param renderer Character string specifying the rendering engine. Currently,
#'   only \code{"browser"} is supported.
#'
#' @return A browsable htmltools tag list rendering the Outcome Distribution chart.
#' @export
#'
#' @examples
#' \dontrun{
#' create_reals_distribution_steps(
#'   clinical_states_steps,
#'   censoring_heuristic = "excluded"
#' )
#' }
create_reals_distribution_steps <- function(
  data,
  id_col = "id",
  step_col = "step",
  reals_col = "reals",
  reals_order = NULL,
  reals_scale = "auto",
  censoring_heuristic = "excluded",
  title = "Outcome Distribution",
  renderer = "browser"
) {
  if (!identical(renderer, "browser")) {
    stop(
      "Only `renderer = \"browser\"` is supported for `create_reals_distribution_steps()`.",
      call. = FALSE
    )
  }

  spec <- rtichoke_viz_outcome_distribution_steps_v2_spec(
    data = data,
    id_col = id_col,
    step_col = step_col,
    reals_col = reals_col,
    reals_order = reals_order,
    reals_scale = reals_scale,
    censoring_heuristic = censoring_heuristic,
    title = title
  )

  render_rtichoke_viz_browser(spec)
}

#' Build a canonical OutcomeDistributionSpec v2 for observed steps
#'
#' Internal helper to prepare discrete step outcome distribution data and
#' serialize it into the canonical rtichoke_viz v2 contract.
#'
#' @noRd
rtichoke_viz_outcome_distribution_steps_v2_spec <- function(
  data,
  id_col = "id",
  step_col = "step",
  reals_col = "reals",
  reals_order = NULL,
  reals_scale = "auto",
  censoring_heuristic = "excluded",
  title = "Outcome Distribution"
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.", call. = FALSE)
  }

  required_cols <- c(id_col, step_col, reals_col)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0L) {
    stop(
      "`data` is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (
    !is.character(censoring_heuristic) ||
      length(censoring_heuristic) != 1L ||
      !(censoring_heuristic %in% c("excluded", "adjusted"))
  ) {
    stop(
      '`censoring_heuristic` must be either "excluded" or "adjusted".',
      call. = FALSE
    )
  }

  if (
    !is.character(reals_scale) ||
      length(reals_scale) != 1L ||
      !(reals_scale %in% c("auto", "nominal", "ordinal"))
  ) {
    stop(
      '`reals_scale` must be "auto", "nominal", or "ordinal".',
      call. = FALSE
    )
  }

  ids <- data[[id_col]]
  if (any(is.na(ids))) {
    stop("`id_col` contains missing values.", call. = FALSE)
  }

  steps <- data[[step_col]]
  if (
    !is.numeric(steps) ||
      any(is.na(steps)) ||
      any(!is.finite(steps)) ||
      any(steps < 0) ||
      any(steps != floor(steps))
  ) {
    stop(
      "`step_col` must contain non-missing, non-negative, finite integer values.",
      call. = FALSE
    )
  }

  if (any(duplicated(data[, c(id_col, step_col)]))) {
    stop(
      "Each subject-step combination in `data` must be unique.",
      call. = FALSE
    )
  }

  r_vec <- data[[reals_col]]

  effective_scale <- if (reals_scale == "auto") {
    if (is.ordered(r_vec) || is.numeric(r_vec)) "ordinal" else "nominal"
  } else {
    reals_scale
  }

  if (!is.null(reals_order)) {
    all_categories <- as.character(reals_order)
    obs_vals <- as.character(stats::na.omit(r_vec))
    invalid_vals <- setdiff(obs_vals, all_categories)
    if (length(invalid_vals) > 0L) {
      stop(
        "`reals_col` contains values not present in `reals_order`: ",
        paste(invalid_vals, collapse = ", "),
        call. = FALSE
      )
    }
  } else if (is.factor(r_vec)) {
    all_categories <- levels(r_vec)
  } else if (is.numeric(r_vec)) {
    all_categories <- as.character(sort(unique(stats::na.omit(r_vec))))
  } else {
    all_categories <- as.character(sort(unique(stats::na.omit(r_vec))))
  }

  if (length(all_categories) == 0L) {
    stop("No non-missing outcome categories found in `data`.", call. = FALSE)
  }

  all_steps <- sort(unique(steps))

  color_palette <- c(
    "#1b9e77",
    "#d95f02",
    "#7570b3",
    "#e7298a",
    "#07004D",
    "#E6AB02",
    "#FE5F55",
    "#54494B",
    "#006E90",
    "#BC96E6"
  )

  states_def <- lapply(seq_along(all_categories), function(idx) {
    cat_val <- all_categories[[idx]]
    color <- color_palette[[(idx - 1L) %% length(color_palette) + 1L]]
    list(
      id = cat_val,
      label = cat_val,
      color = color
    )
  })

  evaluations <- list(list(
    id = "evaluation-1",
    population = "population"
  ))

  series <- list(list(
    id = "series-1",
    evaluationId = "evaluation-1",
    display = list(
      label = "population",
      group = "population",
      role = "population"
    )
  ))

  state_distributions <- list()

  if (censoring_heuristic == "excluded") {
    for (s in all_steps) {
      step_data <- data[data[[step_col]] == s, , drop = FALSE]
      obs_r <- as.character(stats::na.omit(step_data[[reals_col]]))
      n_obs <- length(obs_r)

      states_data <- lapply(all_categories, function(cat_val) {
        cnt <- as.integer(sum(obs_r == cat_val))
        est <- if (n_obs > 0L) cnt / n_obs else NULL
        list(
          stateId = cat_val,
          estimate = est,
          count = cnt
        )
      })

      state_distributions[[length(state_distributions) + 1L]] <- list(
        evaluationId = "evaluation-1",
        horizon = as.numeric(s),
        estimator = "raw",
        estimateOrigin = "observed_step",
        states = states_data
      )
    }
  } else {
    # Adjusted mode: Aalen-Johansen state occupancy via survival::survfit
    sorted_df <- data[
      order(data[[id_col]], data[[step_col]]),
      ,
      drop = FALSE
    ]

    # Reject observation gaps / missing intermediate steps per subject
    split_check <- split(sorted_df, sorted_df[[id_col]])
    for (sub_df in split_check) {
      valid_sub <- sub_df[!is.na(sub_df[[reals_col]]), , drop = FALSE]
      if (nrow(valid_sub) >= 2L) {
        sub_steps <- valid_sub[[step_col]]
        idx_sub <- match(sub_steps, all_steps)
        if (any(diff(idx_sub) != 1L)) {
          stop(
            "Observation gaps or missing intermediate steps per subject are not supported in adjusted mode.",
            call. = FALSE
          )
        }
      }
    }

    min_step <- all_steps[1L]
    base_obs <- sorted_df[
      sorted_df[[step_col]] == min_step & !is.na(sorted_df[[reals_col]]),
      ,
      drop = FALSE
    ]
    base_reals <- factor(
      as.character(base_obs[[reals_col]]),
      levels = all_categories
    )

    if (length(base_reals) == 0L) {
      stop("No baseline observations found at step ", min_step, call. = FALSE)
    }

    base_counts <- table(base_reals)
    p0 <- as.numeric(base_counts / sum(base_counts))
    names(p0) <- all_categories

    tstart_vec <- numeric(0)
    tstop_vec <- numeric(0)
    id_vec <- character(0)
    istate_vec <- character(0)
    event_vec <- character(0)

    censor_code <- "censor_no_event"
    event_levels <- c(censor_code, all_categories)

    for (sub_df in split_check) {
      valid_sub <- sub_df[!is.na(sub_df[[reals_col]]), , drop = FALSE]
      m <- nrow(valid_sub)
      if (m == 0L) {
        next
      }

      sub_steps <- valid_sub[[step_col]]
      sub_reals <- as.character(valid_sub[[reals_col]])
      sub_id <- as.character(valid_sub[[id_col]][1L])

      if (m >= 2L) {
        for (j in seq_len(m - 1L)) {
          s_curr <- sub_steps[j]
          s_next <- sub_steps[j + 1L]
          r_curr <- sub_reals[j]
          r_next <- sub_reals[j + 1L]

          tstart_vec <- c(tstart_vec, s_curr)
          tstop_vec <- c(tstop_vec, s_next)
          id_vec <- c(id_vec, sub_id)
          istate_vec <- c(istate_vec, r_curr)
          event_vec <- c(
            event_vec,
            if (r_next == r_curr) censor_code else r_next
          )
        }
      }

      last_step <- sub_steps[m]
      idx_last <- match(last_step, all_steps)
      if (!is.na(idx_last) && idx_last < length(all_steps)) {
        next_grid_step <- all_steps[idx_last + 1L]
        censor_time <- (last_step + next_grid_step) / 2
        last_real <- sub_reals[m]

        tstart_vec <- c(tstart_vec, last_step)
        tstop_vec <- c(tstop_vec, censor_time)
        id_vec <- c(id_vec, sub_id)
        istate_vec <- c(istate_vec, last_real)
        event_vec <- c(event_vec, censor_code)
      }
    }

    estimates_by_step <- list()
    estimates_by_step[[as.character(min_step)]] <- p0

    if (length(tstart_vec) > 0L) {
      cp_df <- data.frame(
        id = id_vec,
        tstart = tstart_vec,
        tstop = tstop_vec,
        istate = factor(istate_vec, levels = all_categories),
        event = factor(event_vec, levels = event_levels)
      )

      fit <- survival::survfit(
        survival::Surv(tstart, tstop, event) ~ 1,
        data = cp_df,
        id = id,
        istate = istate,
        p0 = p0
      )

      fit_times <- fit$time
      fit_states <- fit$states
      fit_pstate <- fit$pstate

      if (!is.null(fit_times) && length(fit_times) > 0L) {
        if (!is.matrix(fit_pstate)) {
          fit_pstate <- matrix(
            fit_pstate,
            nrow = length(fit_times),
            ncol = length(fit_states),
            byrow = FALSE
          )
        }
        colnames(fit_pstate) <- fit_states

        for (s in all_steps[-1L]) {
          prior_indices <- which(fit_times <= s)
          if (length(prior_indices) == 0L) {
            full_vals <- p0
          } else {
            row_idx <- max(prior_indices)
            vals <- fit_pstate[row_idx, ]
            full_vals <- stats::setNames(
              numeric(length(all_categories)),
              all_categories
            )
            full_vals[names(vals)] <- as.numeric(vals)
          }
          estimates_by_step[[as.character(s)]] <- full_vals
        }
      } else {
        for (s in all_steps[-1L]) {
          estimates_by_step[[as.character(s)]] <- p0
        }
      }
    } else {
      for (s in all_steps[-1L]) {
        estimates_by_step[[as.character(s)]] <- p0
      }
    }

    for (s in all_steps) {
      step_p <- estimates_by_step[[as.character(s)]]

      states_data <- lapply(all_categories, function(cat_val) {
        est <- if (cat_val %in% names(step_p)) {
          unname(as.numeric(step_p[[cat_val]]))
        } else {
          0
        }
        list(
          stateId = cat_val,
          estimate = est
        )
      })

      state_distributions[[length(state_distributions) + 1L]] <- list(
        evaluationId = "evaluation-1",
        horizon = as.numeric(s),
        estimator = "aalen_johansen",
        estimateOrigin = "observed_step",
        states = states_data
      )
    }
  }

  list(
    schemaVersion = "2.0",
    type = "outcome_distribution",
    title = title,
    evaluations = evaluations,
    series = series,
    xAxisKind = "step",
    stateScale = effective_scale,
    xAxis = list(
      label = "Step",
      domain = c(min(all_steps), max(all_steps))
    ),
    yAxis = list(
      label = if (censoring_heuristic == "adjusted") {
        "Probability"
      } else {
        "Proportion"
      },
      domain = c(0, 1)
    ),
    states = states_def,
    stateDistributions = state_distributions
  )
}

#' Prepare continuous longitudinal outcome distribution data across discrete steps
#'
#' Pure numerical preparation helper for continuous outcomes measured at discrete
#' longitudinal steps. Computes step x bin counts, observed denominators,
#' unobserved subject counts, and per-bin proportions over shared equal-width
#' or explicit bin boundaries.
#'
#' @param data A data.frame or tibble containing subject-step panel data.
#' @param id_col Character string specifying the subject identifier column.
#' @param step_col Character string specifying the discrete step column.
#' @param reals_col Character string specifying the continuous outcome column.
#' @param bins A single positive integer specifying the number of equal-width
#'   bins, or a numeric vector of at least two strictly increasing, finite
#'   bin edges.
#'
#' @return A named list containing:
#'   \item{steps}{Numeric or integer vector of sorted unique steps present in \code{data}.}
#'   \item{bin_edges}{Numeric vector of length K + 1 giving bin boundaries.}
#'   \item{counts}{Integer matrix of size S x K containing observed counts per step x bin.}
#'   \item{observed_denominators}{Integer vector of length S giving valid observed outcome counts at each step.}
#'   \item{unobserved_counts}{Integer vector of length S giving unobserved subject counts at each step.}
#'   \item{proportions}{Numeric matrix of size S x K giving per-bin proportions (NA_real_ if denominator is 0).}
#'   \item{n_reference_subjects}{Integer scalar giving total unique subjects in \code{data}.}
#'
#' @noRd
prepare_continuous_reals_distribution_steps_data <- function(
  data,
  id_col = "id",
  step_col = "step",
  reals_col = "reals",
  bins = 10
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.", call. = FALSE)
  }

  required_cols <- c(id_col, step_col, reals_col)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0L) {
    stop(
      "`data` is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  ids <- data[[id_col]]
  if (any(is.na(ids))) {
    stop("`id_col` contains missing values.", call. = FALSE)
  }
  n_reference_subjects <- length(unique(ids))

  steps <- data[[step_col]]
  if (
    !is.numeric(steps) ||
      any(is.na(steps)) ||
      any(!is.finite(steps)) ||
      any(steps < 0) ||
      any(steps != floor(steps))
  ) {
    stop(
      "`step_col` must contain non-missing, non-negative, finite integer values.",
      call. = FALSE
    )
  }

  if (any(duplicated(data[, c(id_col, step_col)]))) {
    stop(
      "Each subject-step combination in `data` must be unique.",
      call. = FALSE
    )
  }

  r_vec <- data[[reals_col]]
  if (!is.numeric(r_vec) && !all(is.na(r_vec))) {
    stop("`reals_col` must be numeric.", call. = FALSE)
  }

  if (any(is.infinite(r_vec), na.rm = TRUE)) {
    stop("`reals_col` contains infinite values (`Inf` or `-Inf`).", call. = FALSE)
  }

  valid_mask <- !is.na(r_vec)
  valid_obs <- r_vec[valid_mask]

  if (length(valid_obs) == 0L) {
    stop("All outcomes in `data` are missing.", call. = FALSE)
  }

  if (!is.numeric(bins)) {
    stop("`bins` must be a numeric bin count or a numeric vector of bin edges.", call. = FALSE)
  }

  if (length(bins) == 1L) {
    if (is.na(bins) || !is.finite(bins) || bins <= 0 || bins != floor(bins)) {
      stop("Scalar `bins` must be a single positive integer.", call. = FALSE)
    }
    K <- as.integer(bins)

    min_val <- min(valid_obs)
    max_val <- max(valid_obs)

    if (min_val == max_val) {
      lower_b <- min_val - 0.5
      upper_b <- max_val + 0.5
      bin_edges <- seq(lower_b, upper_b, length.out = K + 1L)
    } else {
      bin_edges <- seq(min_val, max_val, length.out = K + 1L)
    }
  } else {
    if (any(is.na(bins)) || any(!is.finite(bins))) {
      stop("`bins` vector contains NA or infinite values.", call. = FALSE)
    }
    if (length(bins) < 2L) {
      stop("`bins` vector must contain at least two bin edges.", call. = FALSE)
    }
    if (any(diff(bins) <= 0)) {
      stop("`bins` vector must be strictly increasing.", call. = FALSE)
    }

    bin_edges <- as.numeric(bins)
    K <- length(bin_edges) - 1L

    if (min(valid_obs) < bin_edges[1L] || max(valid_obs) > bin_edges[length(bin_edges)]) {
      stop("Observed outcomes fall outside the explicit `bins` range.", call. = FALSE)
    }
  }

  all_steps <- sort(unique(steps))
  S <- length(all_steps)

  counts <- matrix(0L, nrow = S, ncol = K)
  rownames(counts) <- as.character(all_steps)
  colnames(counts) <- paste0("bin_", seq_len(K))

  proportions <- matrix(NA_real_, nrow = S, ncol = K)
  rownames(proportions) <- as.character(all_steps)
  colnames(proportions) <- paste0("bin_", seq_len(K))

  observed_denominators <- integer(S)
  names(observed_denominators) <- as.character(all_steps)

  unobserved_counts <- integer(S)
  names(unobserved_counts) <- as.character(all_steps)

  for (i in seq_along(all_steps)) {
    s <- all_steps[i]
    step_data <- data[data[[step_col]] == s, , drop = FALSE]
    obs_s <- step_data[[reals_col]][!is.na(step_data[[reals_col]])]
    n_obs <- length(obs_s)

    observed_denominators[i] <- as.integer(n_obs)
    unobserved_counts[i] <- as.integer(n_reference_subjects - n_obs)

    if (n_obs > 0L) {
      bin_indices <- findInterval(
        obs_s,
        bin_edges,
        rightmost.closed = TRUE,
        left.open = FALSE
      )
      tab_counts <- table(factor(bin_indices, levels = seq_len(K)))
      step_counts <- as.integer(tab_counts)
      counts[i, ] <- step_counts
      proportions[i, ] <- step_counts / n_obs
    }
  }

  list(
    steps = all_steps,
    bin_edges = bin_edges,
    counts = counts,
    observed_denominators = observed_denominators,
    unobserved_counts = unobserved_counts,
    proportions = proportions,
    n_reference_subjects = n_reference_subjects
  )
}
