# ROC Curve ---------------------------------------------------------------

#' Create a ROC Curve
#'
#' This is a generic function to create an interactive or static ROC curve
#' to visualize model performance for binary classification problems.
#'
#' @param x An object containing prediction and outcome data.
#' @param ... Additional arguments passed to specific methods.
#'
#' @return A `plotly` or `ggplot2` object.
#'
create_roc_curve <- function(x, ...) {
  UseMethod("create_roc_curve")
}

#' @rdname create_roc_curve
#' @exportS3Method
#' @param reals A list of numeric vectors containing the binary outcomes (0 or 1).
#' @param by The increment for the probability threshold sequence.
#' @param stratified_by A character string specifying the stratification method.
#' @param chosen_threshold A numeric value for a specific threshold to highlight.
#' @param interactive A logical indicating whether to create an interactive `plotly` chart.
#' @param color_values A character vector of hex codes for the color palette.
#' @param title_included A logical indicating whether to include a title.
#' @param size A numeric value for the line size.
#' @examples
#' \dontrun{
#' create_roc_curve(
#'   x = list(example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome)
#' )
#'
#' create_roc_curve(
#'   x = list(example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome),
#'   stratified_by = "ppcr"
#' )
#'
#' create_roc_curve(
#'   x = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   reals = list(example_dat$outcome)
#' )
#'
#'
#' create_roc_curve(
#'   x = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   reals = list(example_dat$outcome),
#'   stratified_by = "ppcr"
#' )
#'
#'
#' create_roc_curve(
#'   x = list(
#'     "train" = example_dat %>%
#'       dplyr::filter(type_of_set == "train") %>%
#'       dplyr::pull(estimated_probabilities),
#'     "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'       dplyr::pull(estimated_probabilities)
#'   ),
#'   reals = list(
#'     "train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
#'       dplyr::pull(outcome),
#'     "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'       dplyr::pull(outcome)
#'   )
#' )
#'
#' create_roc_curve(
#'   x = list(
#'     "train" = example_dat %>%
#'       dplyr::filter(type_of_set == "train") %>%
#'       dplyr::pull(estimated_probabilities),
#'     "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'       dplyr::pull(estimated_probabilities)
#'   ),
#'   reals = list(
#'     "train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
#'       dplyr::pull(outcome),
#'     "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'       dplyr::pull(outcome)
#'   ),
#'   stratified_by = "ppcr"
#' )
#' }
create_roc_curve.default <- function(x, reals, by = 0.01,
                                     stratified_by = "probability_threshold",
                                     chosen_threshold = NA,
                                     interactive = TRUE,
                                     color_values = c(
                                       "#1b9e77", "#d95f02",
                                       "#7570b3", "#e7298a",
                                       "#07004D", "#E6AB02",
                                       "#FE5F55", "#54494B",
                                       "#006E90", "#BC96E6",
                                       "#52050A", "#1F271B",
                                       "#BE7C4D", "#63768D",
                                       "#08A045", "#320A28",
                                       "#82FF9E", "#2176FF",
                                       "#D1603D", "#585123"
                                     ),
                                     title_included = FALSE,
                                     size = NULL, ...) {
  check_probs_input(x)
  # check_real_input(reals)

  if (!is.na(chosen_threshold)) {
    check_chosen_threshold_input(chosen_threshold)
  }

  prepare_performance_data(
    probs = x,
    reals = reals,
    by = by,
    stratified_by = stratified_by
  ) %>%
    plot_roc_curve(
      chosen_threshold = chosen_threshold,
      interactive = interactive,
      color_values = color_values,
      title_included = FALSE,
      size = size
    )
}


#' ROC Curve from Performance Data
#'
#' Plot a ROC Curve
#'
#' @inheritParams create_roc_curve
#' @param performance_data an rtichoke Performance Data
#'
#' @examples
#' \dontrun{
#'
#' one_pop_one_model %>%
#'   plot_roc_curve()
#'
#' one_pop_one_model_by_ppcr %>%
#'   plot_roc_curve()
#'
#' multiple_models %>%
#'   plot_roc_curve()
#'
#' multiple_models_by_ppcr %>%
#'   plot_roc_curve()
#'
#' multiple_populations %>%
#'   plot_roc_curve()
#'
#' multiple_populations_by_ppcr %>%
#'   plot_roc_curve()
#' }
#'
#' @export
plot_roc_curve <- function(performance_data,
                           chosen_threshold = NA,
                           interactive = TRUE,
                           color_values = c(
                             "#1b9e77", "#d95f02",
                             "#7570b3", "#e7298a",
                             "#07004D", "#E6AB02",
                             "#FE5F55", "#54494B",
                             "#006E90", "#BC96E6",
                             "#52050A", "#1F271B",
                             "#BE7C4D", "#63768D",
                             "#08A045", "#320A28",
                             "#82FF9E", "#2176FF",
                             "#D1603D", "#585123"
                           ),
                           title_included = FALSE,
                           size = NULL) {
  rtichoke_curve_list <- performance_data |>
    create_rtichoke_curve_list("roc", size = size, color_values = color_values)

  if (!is.na(chosen_threshold)) {
    check_chosen_threshold_input(chosen_threshold)
  }


  stratified_by <- check_performance_data_stratification(
    performance_data
  )


  perf_dat_type <-
    check_performance_data_type_for_plotly(performance_data = performance_data)

  prevalence <-
    get_prevalence_from_performance_data(performance_data, perf_dat_type)

  if (interactive == FALSE) {
    reference_lines <- create_reference_lines_data_frame("roc")

    roc_curve <- performance_data %>%
      create_ggplot_for_performance_metrics(
        "FPR",
        "sensitivity", color_values
      ) %>%
      add_reference_lines_to_ggplot(reference_lines) +
      ggplot2::xlab("1 - Specificity") +
      ggplot2::ylab("Sensitivity")
  }

  if (interactive == TRUE) {
    roc_curve <- rtichoke_curve_list |>
      create_plotly_curve()
  }

  return(roc_curve)
}
