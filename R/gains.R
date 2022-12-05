# Gains --------------------------------------------------

#' Gains Curve
#'
#' Create a Gains Curve
#'
#' @inheritParams create_roc_curve
#'
#' @export
#'
#' @examples
#' 
#' 
#' \dontrun{
#'
#' create_gains_curve(
#'   probs = list(example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome)
#' )
#' 
#' create_gains_curve(
#'   probs = list(example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome),
#'   stratified_by = "ppcr"
#' )
#'
#' create_gains_curve(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   reals = list(example_dat$outcome)
#' )
#'
#'
#' create_gains_curve(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   reals = list(example_dat$outcome),
#'   stratified_by = "ppcr"
#' )
#'
#'
#' create_gains_curve(
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
#' create_gains_curve(
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
#' 
#' }
create_gains_curve <- function(probs, reals, by = 0.01,
                               stratified_by = "probability_threshold",
                               chosen_threshold = NA,
                               interactive = TRUE,
                               col_values = c("#1b9e77", "#d95f02", 
                                              "#7570b3", "#e7298a", 
                                              "#07004D", "#E6AB02", 
                                              "#FE5F55", "#54494B", 
                                              "#006E90" , "#BC96E6",
                                              "#52050A", "#1F271B", 
                                              "#BE7C4D", "#63768D", 
                                              "#08A045", "#320A28", 
                                              "#82FF9E", "#2176FF", 
                                              "#D1603D", "#585123"),
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
    plot_gains_curve(
      chosen_threshold = chosen_threshold,
      interactive = interactive,
      col_values = col_values,
      size = size
    )
}

#' Gains Curve from Performance Data
#'
#' Plot a Gains Curve
#'
#' @inheritParams plot_roc_curve
#'
#' @examples
#' \dontrun{
#' 
#' one_pop_one_model %>%
#'   plot_gains_curve()
#'
#' one_pop_one_model_by_ppcr %>%
#'   plot_gains_curve()
#'
#' multiple_models %>%
#'   plot_gains_curve()
#'
#' multiple_models_by_ppcr %>%
#'   plot_gains_curve()
#'
#' multiple_populations %>%
#'   plot_gains_curve()
#'
#' multiple_populations_by_ppcr %>%
#'   plot_gains_curve()
#'   
#' }
#'
#' @export

plot_gains_curve <- function(performance_data,
                             chosen_threshold = NA,
                             interactive = TRUE,
                             col_values = c("#1b9e77", "#d95f02", 
                                            "#7570b3", "#e7298a", 
                                            "#07004D", "#E6AB02", 
                                            "#FE5F55", "#54494B", 
                                            "#006E90" , "#BC96E6",
                                            "#52050A", "#1F271B", 
                                            "#BE7C4D", "#63768D", 
                                            "#08A045", "#320A28", 
                                            "#82FF9E", "#2176FF", 
                                            "#D1603D", "#585123"),
                             size = NULL) {
  
  rtichoke_curve_list <- performance_data |>
    create_rtichoke_curve_list("gains", size = size, col_values = col_values)
  
  if (!is.na(chosen_threshold)) {
    check_chosen_threshold_input(chosen_threshold)
  }

  stratified_by <- check_performance_data_stratification(
    performance_data
  )

  perf_dat_type <- check_performance_data_type_for_plotly(
    performance_data = performance_data
  )
  prevalence <- get_prevalence_from_performance_data(
    performance_data,
    perf_dat_type
  )

  if (interactive == FALSE) {
    reference_lines <- create_reference_lines_data_frame("gains", prevalence)

    gains_curve <- performance_data %>%
      create_ggplot_for_performance_metrics(
        "ppcr", "sensitivity",
        col_values
      ) %>%
      add_reference_lines_to_ggplot(reference_lines) %>%
      set_gains_curve_limits() +
      ggplot2::xlab("Predicted Positives (Rate)") +
      ggplot2::ylab("Sensitivity")
  }

  if (interactive == TRUE) {
    
    gains_curve <- rtichoke_curve_list |>
      create_plotly_curve()

  }

  return(gains_curve)
}

#' Add reference lines to Gains Curve
#'
#' @param gains_curve a ggplot object of a Gains Curve
#' @param prevalence the prevalence of the outcome
#' @keywords internal
add_gains_curve_reference_lines <- function(gains_curve, prevalence) {
  gains_curve$layers <- c(
    ggplot2::geom_segment(x = 0, y = 0, xend = 1, yend = 1, color = "grey"),
    purrr::map2(
      prevalence, c(
        "#5BC0BE",
        "#FC8D62",
        "#8DA0CB",
        "#E78AC3",
        "#A4243B"
      )[seq_len(length(prevalence))],
      add_prevalence_layers_to_gains_curve
    ) %>% unlist(),
    gains_curve$layers
  )
  gains_curve
}


#' Set the limits for Gains Curve
#'
#' @param gains_curve a ggplot object of a Gains Curve
#' @keywords internal
set_gains_curve_limits <- function(gains_curve) {
  gains_curve +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1)
}
