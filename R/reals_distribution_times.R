#' Build a canonical OutcomeDistributionSpec v2
#'
#' Internal helper to prepare outcome distribution data and serialize it
#' into the canonical rtichoke_viz v2 contract.
#'
#' @param reals A numeric vector or list of numeric vectors containing outcome labels (0, 1, or 2).
#' @param times A numeric vector or list of numeric vectors containing follow-up times.
#' @param fixed_time_horizons A numeric vector of evaluation time horizons.
#'
#' @return A nested list representing a canonical OutcomeDistributionSpec v2.
#' @noRd
rtichoke_viz_outcome_distribution_v2_spec <- function(
  reals,
  times,
  fixed_time_horizons
) {
  # Normalize list inputs vs atomic vector inputs
  if (is.list(reals) || is.list(times)) {
    if (!is.list(reals) || !is.list(times)) {
      stop(
        "Both `reals` and `times` must be lists if one of them is a list.",
        call. = FALSE
      )
    }
    if (length(reals) != length(times)) {
      stop(
        "`reals` and `times` lists must have the same length.",
        call. = FALSE
      )
    }
    reals_names <- names(reals)
    times_names <- names(times)
    if (!is.null(reals_names) || !is.null(times_names)) {
      if (
        is.null(reals_names) ||
          is.null(times_names) ||
          !identical(reals_names, times_names)
      ) {
        stop(
          "`reals` and `times` list names must match exactly.",
          call. = FALSE
        )
      }
    }
    reals_list <- reals
    times_list <- times
    eval_labels <- if (!is.null(reals_names)) {
      reals_names
    } else {
      paste0("evaluation-", seq_along(reals_list))
    }
  } else {
    reals_list <- list(evaluation = reals)
    times_list <- list(evaluation = times)
    eval_labels <- "evaluation"
  }

  n_evals <- length(reals_list)

  # Validate inputs per evaluation
  for (i in seq_len(n_evals)) {
    r_vec <- reals_list[[i]]
    t_vec <- times_list[[i]]

    if (!is.numeric(r_vec) || !is.numeric(t_vec)) {
      stop("`reals` and `times` must be numeric vectors.", call. = FALSE)
    }
    if (length(r_vec) != length(t_vec)) {
      stop(
        "`reals` and `times` vectors must have identical lengths.",
        call. = FALSE
      )
    }
    if (any(is.na(r_vec)) || !all(r_vec %in% c(0, 1, 2))) {
      stop("`reals` must contain only 0, 1, or 2.", call. = FALSE)
    }
    if (any(is.na(t_vec)) || any(!is.finite(t_vec)) || any(t_vec < 0)) {
      stop("`times` must contain finite non-negative numbers.", call. = FALSE)
    }
  }

  # Validate fixed_time_horizons
  if (
    !is.numeric(fixed_time_horizons) ||
      any(is.na(fixed_time_horizons)) ||
      any(!is.finite(fixed_time_horizons)) ||
      any(fixed_time_horizons < 0)
  ) {
    stop(
      "`fixed_time_horizons` must contain finite non-negative numbers.",
      call. = FALSE
    )
  }

  # Normalize horizons: sort and deduplicate with 0 included
  horizons <- sort(unique(c(0, as.numeric(fixed_time_horizons))))

  # Build evaluations array
  evaluation_ids <- stats::setNames(
    paste0("evaluation-", seq_len(n_evals)),
    eval_labels
  )

  evaluations <- lapply(seq_len(n_evals), function(i) {
    label <- eval_labels[[i]]
    list(
      id = unname(evaluation_ids[[label]]),
      population = label
    )
  })

  fixed_state_definitions <- list(
    list(stateId = "real_positive", label = "Target event"),
    list(stateId = "real_competing", label = "Competing outcome"),
    list(stateId = "real_negative", label = "No target event"),
    list(stateId = "real_censored", label = "Unknown / excluded")
  )

  state_distributions <- list()

  for (i in seq_len(n_evals)) {
    r_vec <- reals_list[[i]]
    t_vec <- times_list[[i]]
    eval_id <- unname(evaluation_ids[[eval_labels[[i]]]])

    for (h in horizons) {
      if (h == 0) {
        c_pos <- 0L
        c_comp <- 0L
        c_cens <- 0L
        c_neg <- length(r_vec)
      } else {
        c_pos <- as.integer(sum(r_vec == 1 & t_vec <= h))
        c_comp <- as.integer(sum(r_vec == 2 & t_vec <= h))
        c_cens <- as.integer(sum(r_vec == 0 & t_vec < h))
        c_neg <- as.integer(length(r_vec) - c_pos - c_comp - c_cens)
      }

      counts <- list(
        real_positive = c_pos,
        real_competing = c_comp,
        real_negative = c_neg,
        real_censored = c_cens
      )

      states <- lapply(fixed_state_definitions, function(st_def) {
        list(
          stateId = st_def$stateId,
          label = st_def$label,
          count = unname(counts[[st_def$stateId]])
        )
      })

      state_distributions[[length(state_distributions) + 1L]] <- list(
        evaluationId = eval_id,
        horizon = as.numeric(h),
        estimator = "raw",
        estimateOrigin = "fixed_time_horizon",
        states = states
      )
    }
  }

  list(
    schemaVersion = "2.0",
    type = "outcome_distribution",
    title = "Outcome Distribution",
    evaluations = evaluations,
    stateDistributions = state_distributions
  )
}

#' Reals Distribution Over Time
#'
#' Summarize observed outcomes over fixed time horizons as an outcome distribution spec.
#'
#' @param reals A numeric vector or list of numeric vectors containing outcome labels (0, 1, or 2).
#' @param times A numeric vector or list of numeric vectors containing follow-up times.
#' @param fixed_time_horizons A numeric vector of evaluation time horizons.
#' @param renderer Rendering backend. Only `"browser"` is supported.
#'
#' @return A browsable HTML tag object when `renderer = "browser"`.
#' @export
#'
#' @examples
#' times <- c(24.1, 9.7, 49.9, 18.6, 34.8, 14.2, 39.2, 46.0, 31.5, 4.3)
#' reals <- c(1, 1, 1, 1, 0, 2, 1, 2, 0, 1)
#' fixed_time_horizons <- c(10, 20, 30, 40, 50)
#'
#' create_reals_distribution_times(
#'   reals = reals,
#'   times = times,
#'   fixed_time_horizons = fixed_time_horizons,
#'   renderer = "browser"
#' )
create_reals_distribution_times <- function(
  reals,
  times,
  fixed_time_horizons,
  renderer = "browser"
) {
  if (!identical(renderer, "browser")) {
    stop(
      "`renderer` must be 'browser' for time-dependent outcome distribution.",
      call. = FALSE
    )
  }

  spec <- rtichoke_viz_outcome_distribution_v2_spec(
    reals = reals,
    times = times,
    fixed_time_horizons = fixed_time_horizons
  )

  render_rtichoke_viz_self_contained_browser(spec)
}
