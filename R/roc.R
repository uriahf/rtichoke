# ROC Curve ---------------------------------------------------------------


#' ROC Curve
#'
#' Create a ROC Curve
#'
#' @inheritParams prepare_performance_data
#' @param col_values color palette
#' @param chosen_threshold a chosen threshold to display (for non-interactive)
#' @param interactive whether the plot should be interactive
#' plots
#' @param col_values color palette
#' @param title_included add title to the curve
#' @param size the size of the curve
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' create_roc_curve(
#'   probs = list(example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome)
#' )
#' 
#' create_roc_curve(
#'   probs = list(example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome),
#'   stratified_by = "ppcr"
#' )
#'
#' create_roc_curve(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   reals = list(example_dat$outcome)
#' )
#'
#'
#' create_roc_curve(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   reals = list(example_dat$outcome),
#'   stratified_by = "ppcr"
#' )
#'
#'
#' create_roc_curve(
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
#' create_roc_curve(
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
#'
#' }
create_roc_curve <- function(probs, reals, by = 0.01,
                             stratified_by = "probability_threshold",
                             chosen_threshold = NA,
                             interactive = TRUE,
                             col_values = c(
                               "#5BC0BE",
                               "#FC8D62",
                               "#8DA0CB",
                               "#E78AC3",
                               "#A4243B"
                             ),
                             title_included = FALSE,
                             size = NULL) {
  check_probs_input(probs)
  # check_real_input(reals)

  if (!is.na(chosen_threshold)) {
    check_chosen_threshold_input(chosen_threshold)
  }

  prepare_performance_data(
    probs = probs,
    reals = reals,
    by = by,
    stratified_by = stratified_by
  ) %>%
    plot_roc_curve(
      chosen_threshold = chosen_threshold,
      interactive = interactive,
      col_values = col_values,
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
#'
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
                           col_values = c(
                             "#5BC0BE",
                             "#FC8D62",
                             "#8DA0CB",
                             "#E78AC3",
                             "#A4243B"
                           ),
                           title_included = FALSE,
                           size = NULL) {
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
        "sensitivity", col_values
      ) %>%
      add_reference_lines_to_ggplot(reference_lines) +
      ggplot2::xlab("1 - Specificity") +
      ggplot2::ylab("Sensitivity")
  }

  if (interactive == TRUE) {
    perf_dat_type <- check_performance_data_type_for_plotly(performance_data)

    performance_data <- performance_data %>%
      add_hover_text_to_performance_data(perf_dat_type, 
                                         curve = "roc",
                                         stratified_by = stratified_by)


    if (perf_dat_type %in% c("one model with model column", "one model")) {
      roc_curve <- create_reference_lines_for_plotly(
        perf_dat_type, "roc",
        size = size
      ) %>%
        add_lines_and_markers_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          FPR,
          sensitivity
        ) %>%
        add_interactive_marker_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          FPR,
          sensitivity,
          stratified_by
        ) %>%
        set_styling_for_rtichoke("roc") %>% 
        plotly::animation_slider(
          currentvalue = list(prefix = ifelse(
            stratified_by == "probability_threshold",
            "Prob. Threshold: ",
            "Predicted Positives (Rate): "
          ),
          font = list(color="black"),
          xanchor = "left"),
          pad = list(t = 50)
        )
    }

    if (perf_dat_type == "several models") {
      performance_data <- performance_data %>%
        mutate(model = forcats::fct_inorder(factor(model)))

      roc_curve <- create_reference_lines_for_plotly(perf_dat_type,
        "roc",
        population_color_vector =
          col_values[seq_len(length(prevalence))],
        size = size
      ) %>%
        add_lines_and_markers_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          FPR,
          sensitivity,
          col_values = col_values
        ) %>%
        add_interactive_marker_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          FPR,
          sensitivity,
          stratified_by
        ) %>%
        set_styling_for_rtichoke("roc") %>% 
        plotly::animation_slider(
          currentvalue = list(prefix = ifelse(
            stratified_by == "probability_threshold",
            "Prob. Threshold: ",
            "Predicted Positives (Rate): "
          ),
          font = list(color="black"),
          xanchor = "left"),
          pad = list(t = 50)
        )
    }

    if (perf_dat_type == "several populations") {
      performance_data <- performance_data %>%
        mutate(population = forcats::fct_inorder(factor(population)))

      roc_curve <- create_reference_lines_for_plotly(perf_dat_type,
        "roc",
        population_color_vector =
          col_values[seq_len(length(prevalence))],
        size = size
      ) %>%
        add_lines_and_markers_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          FPR,
          sensitivity
        ) %>%
        add_interactive_marker_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          FPR,
          sensitivity,
          stratified_by
        ) %>%
        set_styling_for_rtichoke("roc") %>% 
        plotly::animation_slider(
          currentvalue = list(prefix = ifelse(
            stratified_by == "probability_threshold",
            "Prob. Threshold: ",
            "Predicted Positives (Rate): "
          ),
          font = list(color="black"),
          xanchor = "left"),
          pad = list(t = 50)
        )
    }
  }

  return(roc_curve)
}
