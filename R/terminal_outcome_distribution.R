# Internal terminal outcome population preparation and status-aware serialization.
# No public entry point or report uses this module yet.

.TERMINAL_HEURISTIC_PAIRS <- list(
  c("excluded", "excluded"),
  c("excluded", "adjusted_as_censored"),
  c("excluded", "adjusted_as_negative"),
  c("excluded", "adjusted_as_composite"),
  c("adjusted", "excluded"),
  c("adjusted", "adjusted_as_censored"),
  c("adjusted", "adjusted_as_negative"),
  c("adjusted", "adjusted_as_composite")
)

#' Prepare internal terminal outcome population distributions
#'
#' @param populations A named list of data frames or tibbles.
#'   Each data frame must contain non-empty `id`, `reals`, and `times`.
#' @param fixed_time_horizons A numeric vector of evaluation time horizons.
#'
#' @return A list of internal terminal outcome distribution records.
#' @noRd
prepare_terminal_outcome_distributions <- function(
  populations,
  fixed_time_horizons
) {
  if (
    !is.list(populations) ||
      length(populations) == 0L ||
      is.data.frame(populations)
  ) {
    stop(
      "`populations` must be a non-empty named list of data frames.",
      call. = FALSE
    )
  }
  pop_names <- names(populations)
  if (is.null(pop_names) || any(is.na(pop_names)) || any(pop_names == "")) {
    stop(
      "`populations` must be a named list with non-empty string names.",
      call. = FALSE
    )
  }
  if (length(unique(pop_names)) != length(pop_names)) {
    stop("`populations` must have unique names.", call. = FALSE)
  }

  if (
    !is.numeric(fixed_time_horizons) ||
      length(fixed_time_horizons) == 0L ||
      any(is.na(fixed_time_horizons)) ||
      any(!is.finite(fixed_time_horizons)) ||
      any(fixed_time_horizons < 0)
  ) {
    stop(
      "`fixed_time_horizons` must be a non-empty sequence of finite non-negative numbers.",
      call. = FALSE
    )
  }
  ordered_horizons <- sort(unique(as.numeric(fixed_time_horizons)))

  result <- list()

  for (idx in seq_along(populations)) {
    pop_name <- pop_names[[idx]]
    df <- populations[[idx]]

    if (!is.data.frame(df) || nrow(df) == 0L) {
      stop(
        sprintf("Population '%s' must be a non-empty data frame.", pop_name),
        call. = FALSE
      )
    }

    req_cols <- c("id", "reals", "times")
    if (!all(req_cols %in% names(df))) {
      stop(
        sprintf(
          "Population '%s' must contain 'id', 'reals', and 'times' columns.",
          pop_name
        ),
        call. = FALSE
      )
    }

    ids <- df[["id"]]
    reals <- df[["reals"]]
    times <- df[["times"]]

    if (any(is.na(ids))) {
      stop(
        sprintf("Subject IDs must be non-null in population '%s'.", pop_name),
        call. = FALSE
      )
    }
    if (length(unique(ids)) != length(ids)) {
      stop(
        sprintf("Subject IDs must be unique within population '%s'.", pop_name),
        call. = FALSE
      )
    }

    # Strict check on reals: must be numeric or integer, strictly in 0, 1, 2, no factors/logicals
    if (
      is.factor(reals) ||
        is.logical(reals) ||
        !is.numeric(reals) ||
        any(is.na(reals))
    ) {
      stop(
        sprintf(
          "`reals` must be numeric containing only 0, 1, or 2 in population '%s'.",
          pop_name
        ),
        call. = FALSE
      )
    }
    if (!all(reals %in% c(0, 1, 2))) {
      stop(
        sprintf(
          "`reals` must contain only 0, 1, or 2 in population '%s'.",
          pop_name
        ),
        call. = FALSE
      )
    }

    if (
      !is.numeric(times) ||
        any(is.na(times)) ||
        any(!is.finite(times)) ||
        any(times < 0)
    ) {
      stop(
        sprintf(
          "`times` must contain finite non-negative numbers in population '%s'.",
          pop_name
        ),
        call. = FALSE
      )
    }

    original_df <- data.frame(
      id = ids,
      reals = as.integer(reals),
      times = as.numeric(times),
      stringsAsFactors = FALSE
    )
    original_n <- nrow(original_df)
    eval_id <- paste0("evaluation-", idx)

    for (pair in .TERMINAL_HEURISTIC_PAIRS) {
      censoring <- pair[[1]]
      competing <- pair[[2]]

      for (h in ordered_horizons) {
        # Both exclusion masks refer to original outcomes before recoding
        censor_mask <- (original_df$reals == 0L) &
          (original_df$times < h) &
          (censoring == "excluded")
        competing_mask <- (original_df$reals == 2L) &
          (original_df$times <= h) &
          (competing == "excluded")

        excluded_mask <- censor_mask | competing_mask
        retained_df <- original_df[!excluded_mask, , drop = FALSE]

        censor_ids <- original_df$id[censor_mask]
        competing_ids <- original_df$id[competing_mask]
        retained_ids <- retained_df$id

        censor_count <- length(censor_ids)
        competing_count <- length(competing_ids)
        retained_n <- nrow(retained_df)

        states <- if (competing == "adjusted_as_negative") {
          c("primary", "competing", "event_free")
        } else if (competing == "adjusted_as_composite") {
          c("composite", "event_free")
        } else {
          c("primary", "event_free")
        }

        estimator <- if (competing == "adjusted_as_negative") {
          "aalen_johansen"
        } else {
          "kaplan_meier"
        }

        probabilities <- stats::setNames(
          rep(list(NULL), length(states)),
          states
        )
        status <- "undefined_empty_population"

        if (retained_n > 0L) {
          adj_df <- retained_df
          if (competing == "adjusted_as_censored") {
            adj_df$reals[adj_df$reals == 2L] <- 0L
          } else if (competing == "adjusted_as_composite") {
            adj_df$reals[adj_df$reals == 2L] <- 1L
          }

          if (competing == "adjusted_as_negative") {
            observed_causes <- sort(unique(adj_df$reals[adj_df$reals > 0L]))
            if (length(observed_causes) == 0L) {
              # No transitions: survival cannot fit multistate with no transitions.
              # Fit degenerate binary survival; missing causes have structural 0.
              fit <- survival::survfit(
                survival::Surv(times, reals == 1L) ~ 1,
                data = adj_df
              )
              s <- if (length(fit$surv)) utils::tail(fit$surv, 1) else 1
              probabilities <- list(
                primary = 0,
                competing = 0,
                event_free = as.numeric(s)
              )
            } else {
              adj_df$status <- factor(
                adj_df$reals,
                levels = c(0L, observed_causes),
                labels = c("censor", c("primary", "competing")[observed_causes])
              )
              fit <- survival::survfit(
                survival::Surv(times, status) ~ 1,
                data = adj_df
              )
              idx_h <- which(fit$time <= h)
              p <- if (length(idx_h)) {
                fit$pstate[max(idx_h), ]
              } else {
                stats::setNames(
                  c(1, rep(0, length(observed_causes))),
                  fit$states
                )
              }
              names(p) <- fit$states
              probs_vec <- c(
                primary = 0,
                competing = 0,
                event_free = unname(p[["(s0)"]])
              )
              for (st in intersect(names(p), c("primary", "competing"))) {
                probs_vec[[st]] <- unname(p[[st]])
              }
              probabilities <- as.list(probs_vec)
            }
          } else {
            fit <- survival::survfit(
              survival::Surv(times, reals == 1L) ~ 1,
              data = adj_df
            )
            idx_h <- which(fit$time <= h)
            s <- if (length(idx_h)) fit$surv[max(idx_h)] else 1
            probabilities <- as.list(stats::setNames(c(1 - s, s), states))
          }

          event_free <- probabilities[["event_free"]]
          status <- if (
            h > max(retained_df$times) && !is.null(event_free) && event_free > 0
          ) {
            "beyond_support_plateau"
          } else {
            "defined"
          }
        }

        record <- list(
          evaluation_id = eval_id,
          population = pop_name,
          horizon = as.numeric(h),
          censoring = censoring,
          competing = competing,
          original_n = original_n,
          retained_ids = retained_ids,
          retained_n = retained_n,
          censored_excluded_ids = censor_ids,
          censored_excluded_count = censor_count,
          censored_excluded_fraction = censor_count / original_n,
          competing_excluded_ids = competing_ids,
          competing_excluded_count = competing_count,
          competing_excluded_fraction = competing_count / original_n,
          estimator = estimator,
          status = status,
          probabilities = probabilities
        )

        result[[length(result) + 1L]] <- record
      }
    }
  }

  result
}

#' Bound machine-scale floating point endpoint roundoff
#'
#' @param value Numeric estimate or NULL.
#' @return Numeric probability in [0, 1].
#' @noRd
.canonical_probability <- function(value) {
  if (is.null(value)) {
    stop("A defined canonical estimate must not be NULL.", call. = FALSE)
  }
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
    stop(
      "A defined canonical estimate must be a finite probability in [0, 1].",
      call. = FALSE
    )
  }
  tolerance <- 8 * .Machine$double.eps
  if (value < -tolerance || value > 1 + tolerance) {
    stop(
      "A defined canonical estimate must be a finite probability in [0, 1].",
      call. = FALSE
    )
  }
  min(1.0, max(0.0, value))
}

#' Serialize canonical OutcomeDistributionSpec v2 from prepared distributions
#'
#' @param prepared A list of records returned by `prepare_terminal_outcome_distributions()`.
#'
#' @return A nested list representing canonical OutcomeDistributionSpec v2.
#' @noRd
rtichoke_viz_terminal_outcome_distribution_v2_spec <- function(prepared) {
  if (!is.list(prepared) || length(prepared) == 0L) {
    stop("Prepared distributions must not be empty.", call. = FALSE)
  }

  eval_map <- list()
  for (r in prepared) {
    eval_map[[r$evaluation_id]] <- list(
      id = r$evaluation_id,
      population = r$population
    )
  }
  evaluations <- unname(eval_map)

  # Validate all eight unique keys against the complete expected Cartesian product
  row_keys <- sapply(prepared, function(r) {
    paste(r$evaluation_id, r$horizon, r$censoring, r$competing, sep = "::")
  })
  unique_row_keys <- unique(row_keys)
  contexts <- unique(sapply(prepared, function(r) {
    paste(r$evaluation_id, r$horizon, sep = "::")
  }))
  expected_keys <- unlist(lapply(contexts, function(ctx) {
    sapply(.TERMINAL_HEURISTIC_PAIRS, function(pair) {
      paste(ctx, pair[[1]], pair[[2]], sep = "::")
    })
  }))

  if (
    length(unique_row_keys) != length(prepared) ||
      !setequal(unique_row_keys, expected_keys)
  ) {
    stop(
      "Serialization requires exactly eight pairs per population and horizon.",
      call. = FALSE
    )
  }

  state_labels <- list(
    primary = "Primary event",
    competing = "Competing event",
    composite = "Composite event",
    event_free = "Event-free"
  )

  variants <- list()

  for (pair in .TERMINAL_HEURISTIC_PAIRS) {
    censoring <- pair[[1]]
    competing <- pair[[2]]
    variant_key <- paste(censoring, competing, sep = ":")

    rows <- Filter(
      function(r) r$censoring == censoring && r$competing == competing,
      prepared
    )

    distributions <- lapply(rows, function(r) {
      states_list <- lapply(names(r$probabilities), function(state_name) {
        val <- r$probabilities[[state_name]]
        est <- if (r$status == "undefined_empty_population" && is.null(val)) {
          NULL
        } else {
          .canonical_probability(val)
        }
        list(
          stateId = state_name,
          label = state_labels[[state_name]],
          estimate = est
        )
      })

      list(
        evaluationId = r$evaluation_id,
        horizon = r$horizon,
        estimator = r$estimator,
        estimateOrigin = "fixed_time_horizon",
        estimateStatus = r$status,
        censoringHeuristic = r$censoring,
        competingHeuristic = r$competing,
        states = states_list
      )
    })

    exclusions <- lapply(evaluations, function(ev) {
      ev_rows <- Filter(function(r) r$evaluation_id == ev$id, rows)
      original_n <- ev_rows[[1]]$original_n

      excluded_counts <- lapply(ev_rows, function(r) {
        list(
          horizon = r$horizon,
          censoredExcludedCount = as.integer(r$censored_excluded_count),
          competingExcludedCount = as.integer(r$competing_excluded_count)
        )
      })

      list(
        evaluationId = ev$id,
        originalSampleSize = as.integer(original_n),
        excludedCounts = excluded_counts
      )
    })

    variants[[variant_key]] <- list(
      status = "available",
      stateDistributions = distributions,
      exclusions = exclusions
    )
  }

  # The required base payload is adjusted:adjusted_as_negative
  base <- variants[["adjusted:adjusted_as_negative"]]

  list(
    schemaVersion = "2.0",
    type = "outcome_distribution",
    title = "Outcome Distribution",
    evaluations = evaluations,
    stateDistributions = base$stateDistributions,
    exclusions = base$exclusions,
    variants = variants
  )
}
