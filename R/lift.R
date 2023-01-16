# Lift --------------------------------------------------

#' Lift Curve
#'
#' Create a Lift Curve
#'
#' @inheritParams create_roc_curve
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' create_lift_curve(
#'   probs = list(example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome)
#' )
#'
#' create_lift_curve(
#'   probs = list(example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome),
#'   stratified_by = "ppcr"
#' )
#'
#' create_lift_curve(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   reals = list(example_dat$outcome)
#' )
#'
#'
#' create_lift_curve(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   reals = list(example_dat$outcome),
#'   stratified_by = "ppcr"
#' )
#'
#'
#' create_lift_curve(
#'   probs = list(
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
#' create_lift_curve(
#'   probs = list(
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
create_lift_curve <- function(probs, reals, by = 0.01,
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
                              size = NULL) {
  if (!is.na(chosen_threshold)) {
    check_chosen_threshold_input(chosen_threshold)
  }

  prepare_performance_data(
    probs = probs,
    reals = reals,
    by = by,
    stratified_by = stratified_by
  ) %>%
    plot_lift_curve(
      chosen_threshold = chosen_threshold,
      interactive = interactive,
      color_values = color_values,
      size = size
    )
}


#' LIFT Curve from Performance Data
#'
#' Plot a LIFT Curve
#'
#' @inheritParams plot_roc_curve
#'
#' @examples
#' \dontrun{
#'
#' one_pop_one_model %>%
#'   plot_lift_curve()
#'
#' one_pop_one_model_by_ppcr %>%
#'   plot_lift_curve()
#'
#' multiple_models %>%
#'   plot_lift_curve()
#'
#' multiple_models_by_ppcr %>%
#'   plot_lift_curve()
#'
#' multiple_populations %>%
#'   plot_lift_curve()
#'
#' multiple_populations_by_ppcr %>%
#'   plot_lift_curve()
#' }
#'
#' @export
plot_lift_curve <- function(performance_data,
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
                            size = NULL) {
  rtichoke_curve_list <- performance_data |>
    create_rtichoke_curve_list("lift", size = size, color_values = color_values)

  if (!is.na(chosen_threshold)) {
    check_chosen_threshold_input(chosen_threshold)
  }

  stratified_by <- check_performance_data_stratification(
    performance_data
  )

  perf_dat_type <- check_performance_data_type_for_plotly(
    performance_data =
      performance_data
  )
  prevalence <- get_prevalence_from_performance_data(
    performance_data,
    perf_dat_type
  )

  if (interactive == FALSE) {
    reference_lines <- create_reference_lines_data_frame("lift")

    lift_curve <- performance_data %>%
      create_ggplot_for_performance_metrics("ppcr", "lift", color_values) %>%
      add_reference_lines_to_ggplot(reference_lines) %>%
      set_lift_curve_limits() +
      ggplot2::xlab("Predicted Positives (Rate)") +
      ggplot2::ylab("Lift")
  }

  if (interactive == TRUE) {
    lift_curve <- rtichoke_curve_list |>
      create_plotly_curve()
  }

  return(lift_curve)
}


#' Set the limits for lift curve
#'
#' @param lift_curve a ggplot object of lift curve
#' @keywords internal
set_lift_curve_limits <- function(lift_curve) {
  lift_curve +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, NA)
}
