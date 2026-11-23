#' Create a minimal time-dependent summary report
#'
#' Creates a self-contained HTML report of outcome distributions over time
#' horizons. By default, displays raw observed outcome accounting (target events,
#' competing events, censored, and non-events).
#'
#' With \code{heuristics = FALSE}, censored observations are displayed as their
#' own observed status and no censoring adjustment is applied.
#'
#' With \code{heuristics = TRUE}, report controls select among eight combinations
#' of censoring and competing-event treatment, using Kaplan-Meier or
#' Aalen-Johansen estimates. Exclusions define a retained population separately
#' at each horizon; exclusion percentages use the original population size.
#' Empty retained populations have unavailable estimates. Beyond observed
#' follow-up support, displayed estimates retain the estimator's last plateau.
#' Censoring adjustment relies on the relevant independent-censoring assumptions.
#'
#' Supply \code{probs} to add target-event calibration. The calibration tabs
#' use Aalen-Johansen estimates within prediction quantile groups, independently
#' of the outcome-distribution controls.
#'
#' @inheritParams create_reals_distribution_times
#' @param output_file The name of the output HTML file. Defaults to
#'   \code{"summary_report_times.html"}.
#' @param output_dir The directory where the report HTML file will be saved.
#'   Defaults to the current working directory.
#' @param renderer Character string specifying the rendering engine. Currently,
#'   only \code{"browser"} is supported.
#' @param heuristics A logical value indicating whether to enable analysis
#'   heuristic controls (censoring and competing events). Defaults to \code{FALSE}.
#'
#' @param probs Optional numeric matrix or named list of matrices, one per
#'   model in the same population. Rows match \code{reals}. Use numeric horizon values as column names to match
#'   the distinct positive \code{fixed_time_horizons} in any order. Unnamed
#'   columns follow ascending horizon order. Values are predicted target-event
#'   cumulative incidence probabilities.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' times <- c(24.1, 9.7, 49.9, 18.6, 34.8, 14.2, 39.2, 46.0, 31.5, 4.3)
#' reals <- c(1, 1, 1, 1, 0, 2, 1, 2, 0, 1)
#' fixed_time_horizons <- c(10, 20, 30, 40, 50)
#'
#' create_summary_report_times(
#'   reals = reals,
#'   times = times,
#'   fixed_time_horizons = fixed_time_horizons,
#'   output_file = "time_summary_report.html"
#' )
#' }
create_summary_report_times <- function(
  reals,
  times,
  fixed_time_horizons,
  output_file = "summary_report_times.html",
  output_dir = getwd(),
  renderer = "browser",
  heuristics = FALSE,
  probs = NULL
) {
  if (!identical(renderer, "browser")) {
    stop(
      "Only `renderer = \"browser\"` is supported for `create_summary_report_times()`.",
      call. = FALSE
    )
  }

  if (
    !is.logical(heuristics) ||
      length(heuristics) != 1L ||
      is.na(heuristics)
  ) {
    stop(
      "`heuristics` must be a single non-missing logical value.",
      call. = FALSE
    )
  }

  if (isTRUE(heuristics)) {
    if (
      !is.numeric(fixed_time_horizons) ||
        length(fixed_time_horizons) == 0L ||
        any(!is.finite(fixed_time_horizons)) ||
        any(fixed_time_horizons < 0)
    ) {
      stop(
        "`fixed_time_horizons` must be a non-empty sequence of finite non-negative numbers.",
        call. = FALSE
      )
    }
    fixed_time_horizons <- sort(unique(c(0, fixed_time_horizons)))

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

    populations <- stats::setNames(
      lapply(seq_along(reals_list), function(i) {
        r <- reals_list[[i]]
        t <- times_list[[i]]
        if (length(r) != length(t)) {
          stop(
            "`reals` and `times` vectors must have identical lengths.",
            call. = FALSE
          )
        }
        validate_outcome_event_times(r, t)
        data.frame(
          id = seq_along(r),
          reals = r,
          times = t,
          stringsAsFactors = FALSE
        )
      }),
      eval_labels
    )

    prepared <- prepare_terminal_outcome_distributions(
      populations = populations,
      fixed_time_horizons = fixed_time_horizons
    )

    outcome_dist_spec <- rtichoke_viz_terminal_outcome_distribution_v2_spec(
      prepared
    )

    report_heuristics <- list(
      active = "adjusted:adjusted_as_negative",
      available = as.list(names(outcome_dist_spec$variants))
    )

    header_note <- time_dependent_outcome_distribution_heuristics_note_html()
  } else {
    outcome_dist_spec <- rtichoke_viz_outcome_distribution_v2_spec(
      reals = reals,
      times = times,
      fixed_time_horizons = fixed_time_horizons,
      state_labels = c(
        real_positive = "Event",
        real_competing = "Competing event",
        real_negative = "Event-free",
        real_censored = "Censored"
      )
    )

    report_heuristics <- NULL
    header_note <- time_dependent_outcome_distribution_note_html()
  }

  sections <- list(list(
    id = "time-dependent-outcomes",
    title = "Time-dependent outcomes",
    items = list(list(
      type = "component",
      id = "outcome-distribution",
      title = "Outcome Distribution",
      spec = outcome_dist_spec
    ))
  ))
  if (!is.null(probs)) {
    calibration <- prepare_time_report_calibration(
      probs,
      reals,
      times,
      fixed_time_horizons
    )
    sections <- c(sections, list(calibration$section))
    header_note <- paste(header_note, calibration$note, sep = "\n")
  }
  report_spec <- do.call(
    rtichoke_viz_report_spec_v1_1,
    c(
      sections,
      list(
        title = "Time-dependent Summary Report",
        heuristics = report_heuristics
      )
    )
  )

  report <- render_rtichoke_viz_report_browser(
    report_spec,
    header_annotation_html = header_note
  )

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  htmltools::save_html(
    report,
    file = file.path(output_dir, basename(output_file)),
    libdir = "lib"
  )

  print(glue::glue("{output_file} was rendered in {output_dir}"))
}
