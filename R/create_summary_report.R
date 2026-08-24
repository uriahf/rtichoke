#' Create a summary report
#'
#' @inheritParams create_roc_curve
#' @inheritParams rmarkdown::render
#' @param output_file The name of the output file.
#' @param renderer Summary-report rendering backend. `"rmarkdown"` preserves
#'   the existing report and remains the default. `"browser"` renders the
#'   canonical PerformanceTable, ROC, and discrete calibration components via
#'   the vendored rtichoke_viz ReportSpec renderer.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' create_summary_report(
#'   probs = list(example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome)
#' )
#'
#' create_summary_report(
#'   probs = list(example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome),
#'   renderer = "browser"
#' )
#'
#' create_summary_report(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   reals = list(example_dat$outcome)
#' )
#'
#' create_summary_report(
#'   probs = list(
#'     "train" = example_dat |>
#'       dplyr::filter(type_of_set == "train") |>
#'       dplyr::pull(estimated_probabilities),
#'     "test" = example_dat |> dplyr::filter(type_of_set == "test") |>
#'       dplyr::pull(estimated_probabilities)
#'   ),
#'   reals = list(
#'     "train" = example_dat |> dplyr::filter(type_of_set == "train") |>
#'       dplyr::pull(outcome),
#'     "test" = example_dat |> dplyr::filter(type_of_set == "test") |>
#'       dplyr::pull(outcome)
#'   )
#' )
#' }
create_summary_report <- function(
  probs,
  reals,
  interactive = TRUE,
  output_file = "summary_report.html",
  output_dir = getwd(),
  renderer = c("rmarkdown", "browser")
) {
  renderer <- match.arg(renderer)

  if (renderer == "rmarkdown") {
    rmarkdown::render(
      file.path(
        system.file(package = "rtichoke"),
        "summary_report_template.Rmd"
      ),
      params = list(
        probs = probs,
        reals = reals,
        interactive = interactive
      ),
      output_file = output_file,
      output_dir = output_dir
    )
  } else {
    report_spec <- summary_report_browser_spec(probs, reals)
    report <- render_rtichoke_viz_report_browser(report_spec)

    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    htmltools::save_html(
      report,
      file = file.path(output_dir, output_file),
      libdir = "lib"
    )
  }

  print(glue::glue("{output_file} was rendered in {output_dir}"))
}


#' Build the canonical first public browser summary report
#'
#' Reuse the package's existing statistical preparation and canonical component
#' builders, then assemble the resulting complete standalone specs into a
#' ReportSpec. This helper does not dispatch report components or alter their
#' identities.
#'
#' @param probs A list of vectors of estimated probabilities.
#' @param reals A list of vectors of binary outcomes.
#'
#' @return A canonical ReportSpec.
#' @noRd
summary_report_browser_spec <- function(probs, reals) {
  check_probs_input(probs)
  check_real_input(reals)

  performance_data <- prepare_performance_data(
    probs = probs,
    reals = reals
  )
  evaluation_metadata <- build_evaluation_metadata(probs, reals)
  calibration_curve_list <- create_calibration_curve_list(
    probs = probs,
    reals = reals
  )

  performance_table <- rtichoke_viz_performance_table_v2_spec(
    performance_data,
    evaluation_metadata
  )
  roc <- rtichoke_viz_roc_v2_spec(
    performance_data,
    evaluation_metadata
  )
  calibration <- rtichoke_viz_calibration_v2_spec(
    calibration_curve_list,
    evaluation_metadata,
    method = "discrete"
  )

  rtichoke_viz_report_spec(
    performance_table,
    roc,
    calibration,
    title = "Summary Report",
    component_titles = list(
      "Performance Table",
      "ROC",
      "Calibration"
    )
  )
}
