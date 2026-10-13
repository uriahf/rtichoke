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
  renderer = "browser"
) {
  if (!identical(renderer, "browser")) {
    stop(
      "Only `renderer = \"browser\"` is supported for `create_summary_report_times()`.",
      call. = FALSE
    )
  }

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

  report <- render_rtichoke_viz_report_browser(
    report_spec,
    header_annotation_html = time_dependent_outcome_distribution_note_html()
  )

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  htmltools::save_html(
    report,
    file = file.path(output_dir, basename(output_file)),
    libdir = "lib"
  )

  print(glue::glue("{output_file} was rendered in {output_dir}"))
}
