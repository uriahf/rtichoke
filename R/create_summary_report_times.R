#' Create a minimal time-dependent summary report
#'
#' Prepares observed outcome status over time horizons and creates a self-contained
#' HTML report containing raw observed outcome accounting (target events,
#' competing events, censored, and non-events).
#'
#' Censored observations are displayed as their own observed status. No
#' Aalen-Johansen (AJ), Inverse Probability of Censoring Weighting (IPCW), or
#' other censoring adjustment is applied. This report contains only raw observed
#' outcome status over time and does not include predicted probabilities,
#' calibration, discrimination metrics, threshold-based metrics, or decision-curve
#' analysis.
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
  heuristics = FALSE
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

    report_spec <- rtichoke_viz_report_spec_v1_1(
      list(
        id = "time-dependent-outcomes",
        title = "Time-dependent outcomes",
        items = list(
          list(
            type = "component",
            id = "outcome-distribution",
            title = "Outcome Distribution",
            spec = outcome_dist_spec
          )
        )
      ),
      title = "Time-dependent Summary Report",
      heuristics = list(
        active = "adjusted:adjusted_as_negative",
        available = as.list(names(outcome_dist_spec$variants))
      )
    )

    header_note <- time_dependent_outcome_distribution_heuristics_note_html()
  } else {
    outcome_dist_spec <- rtichoke_viz_outcome_distribution_v2_spec(
      reals = reals,
      times = times,
      fixed_time_horizons = fixed_time_horizons
    )

    report_spec <- rtichoke_viz_report_spec_v1_1(
      list(
        id = "time-dependent-outcomes",
        title = "Time-dependent outcomes",
        items = list(
          list(
            type = "component",
            id = "outcome-distribution",
            title = "Outcome Distribution",
            spec = outcome_dist_spec
          )
        )
      ),
      title = "Time-dependent Summary Report"
    )

    header_note <- time_dependent_outcome_distribution_note_html()
  }

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
