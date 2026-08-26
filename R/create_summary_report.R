#' Create a summary report
#'
#' @inheritParams create_roc_curve
#' @inheritParams rmarkdown::render
#' @param output_file The name of the output file.
#' @param renderer Summary-report rendering backend. `"rmarkdown"` preserves
#'   the existing report and remains the default. `"browser"` renders the
#'   existing canonical static report components via the vendored rtichoke_viz
#'   ReportSpec renderer.
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
    render_summary_report_rmarkdown(
      probs = probs,
      reals = reals,
      interactive = interactive,
      output_file = output_file,
      output_dir = output_dir
    )
  } else {
    report_spec <- summary_report_browser_spec(probs, reals)
    report <- render_rtichoke_viz_report_browser(report_spec)

    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    htmltools::save_html(
      report,
      file = file.path(output_dir, basename(output_file)),
      libdir = "lib"
    )
  }

  print(glue::glue("{output_file} was rendered in {output_dir}"))
}


#' Render the historical RMarkdown summary report
#'
#' Keep the pre-existing summary-report lifecycle isolated so the public
#' renderer switch cannot accidentally migrate the default backend.
#'
#' @inheritParams create_summary_report
#' @noRd
render_summary_report_rmarkdown <- function(
  probs,
  reals,
  interactive,
  output_file,
  output_dir
) {
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
}


#' Build the canonical static browser summary report
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

  threshold_performance_data <- prepare_performance_data(
    probs = probs,
    reals = reals
  )
  ppcr_performance_data <- prepare_performance_data(
    probs = probs,
    reals = reals,
    stratified_by = "ppcr"
  )
  evaluation_metadata <- build_evaluation_metadata(probs, reals)
  calibration_curve_list <- create_calibration_curve_list(
    probs = probs,
    reals = reals
  )
  interventions_avoided_data <- add_static_interventions_avoided_metric(
    threshold_performance_data
  )

  threshold_performance_table <- rtichoke_viz_performance_table_v2_spec(
    threshold_performance_data,
    evaluation_metadata
  )
  smooth_calibration <- rtichoke_viz_calibration_v2_spec(
    calibration_curve_list,
    evaluation_metadata,
    method = "smooth"
  )
  discrete_calibration <- rtichoke_viz_calibration_v2_spec(
    calibration_curve_list,
    evaluation_metadata,
    method = "discrete"
  )
  threshold_roc <- rtichoke_viz_roc_v2_spec(
    threshold_performance_data,
    evaluation_metadata
  )
  threshold_precision_recall <- rtichoke_viz_precision_recall_v2_spec(
    threshold_performance_data,
    evaluation_metadata
  )
  threshold_gains <- rtichoke_viz_gains_v2_spec(
    threshold_performance_data,
    evaluation_metadata
  )
  threshold_lift <- rtichoke_viz_lift_v2_spec(
    threshold_performance_data,
    evaluation_metadata
  )
  decision_curve <- rtichoke_viz_decision_curve_v2_spec(
    threshold_performance_data,
    evaluation_metadata
  )
  interventions_avoided <- rtichoke_viz_interventions_avoided_v2_spec(
    interventions_avoided_data,
    evaluation_metadata
  )
  ppcr_performance_table <- rtichoke_viz_performance_table_v2_spec(
    ppcr_performance_data,
    evaluation_metadata,
    stratified_by = "ppcr"
  )
  ppcr_roc <- rtichoke_viz_roc_v2_spec(
    ppcr_performance_data,
    evaluation_metadata
  )
  ppcr_precision_recall <- rtichoke_viz_precision_recall_v2_spec(
    ppcr_performance_data,
    evaluation_metadata
  )
  ppcr_gains <- rtichoke_viz_gains_v2_spec(
    ppcr_performance_data,
    evaluation_metadata
  )
  ppcr_lift <- rtichoke_viz_lift_v2_spec(
    ppcr_performance_data,
    evaluation_metadata
  )

  rtichoke_viz_report_spec(
    threshold_performance_table,
    smooth_calibration,
    discrete_calibration,
    threshold_roc,
    threshold_precision_recall,
    threshold_gains,
    threshold_lift,
    decision_curve,
    interventions_avoided,
    ppcr_performance_table,
    ppcr_roc,
    ppcr_precision_recall,
    ppcr_gains,
    ppcr_lift,
    title = "Summary Report",
    component_titles = list(
      "Performance Table \u2014 Probability Threshold",
      "Calibration \u2014 Smooth",
      "Calibration \u2014 Discrete",
      "ROC \u2014 Probability Threshold",
      "Precision-Recall \u2014 Probability Threshold",
      "Gains \u2014 Probability Threshold",
      "Lift \u2014 Probability Threshold",
      "Decision Curve",
      "Interventions Avoided",
      "Performance Table \u2014 PPCR",
      "ROC \u2014 PPCR",
      "Precision-Recall \u2014 PPCR",
      "Gains \u2014 PPCR",
      "Lift \u2014 PPCR"
    )
  )
}
