# Precision Recall --------------------------------------------------

#' Precision Recall Curve
#'
#' Create a Precision Recall Curve
#'
#' @inheritParams create_roc_curve
#'
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' create_precision_recall_curve(
#'   probs = list(example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome)
#' )
#' 
#' create_precision_recall_curve(
#'   probs = list(example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome),
#'   stratified_by = "ppcr"
#' )
#'
#' create_precision_recall_curve(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   reals = list(example_dat$outcome)
#' )
#'
#'
#' create_precision_recall_curve(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   reals = list(example_dat$outcome),
#'   stratified_by = "ppcr"
#' )
#'
#'
#' create_precision_recall_curve(
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
#' create_precision_recall_curve(
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
create_precision_recall_curve <- function(probs,
                                          reals,
                                          by = 0.01,
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
  prepare_performance_data(
    probs = probs,
    reals = reals,
    by = by,
    stratified_by = stratified_by
  ) %>%
    plot_precision_recall_curve(
      chosen_threshold = chosen_threshold,
      interactive = interactive,
      col_values = col_values,
      size = size
    )
}



#' Precision Recall Curve from Performance Data
#'
#' Plot a Precision Recall Curve
#'
#' @inheritParams plot_roc_curve
#'
#' @examples
#'
#' \dontrun{
#'
#' one_pop_one_model %>%
#'   plot_precision_recall_curve()
#'
#' one_pop_one_model_by_ppcr %>%
#'   plot_precision_recall_curve()
#'
#' multiple_models %>%
#'   plot_precision_recall_curve()
#'
#' multiple_models_by_ppcr %>%
#'   plot_precision_recall_curve()
#'
#' multiple_populations %>%
#'   plot_precision_recall_curve()
#'
#' multiple_populations_by_ppcr %>%
#'   plot_precision_recall_curve()
#' }
#'
#' @export

plot_precision_recall_curve <- function(performance_data,
                                        chosen_threshold = NA,
                                        interactive = FALSE,
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
  perf_dat_type <- check_performance_data_type_for_plotly(performance_data)
  prevalence <- get_prevalence_from_performance_data(
    performance_data,
    perf_dat_type
  )

  stratified_by <- check_performance_data_stratification(
    performance_data
  )


  if (interactive == FALSE) {
    reference_lines <- create_reference_lines_data_frame(
      "precision recall",
      prevalence
    )

    precision_recall_curve <- performance_data %>%
      create_ggplot_for_performance_metrics(
        "sensitivity", "PPV",
        col_values
      ) %>%
      add_reference_lines_to_ggplot(reference_lines) %>%
      set_precision_recall_curve_limits() +
      ggplot2::xlab("Sensitivity") +
      ggplot2::ylab("PPV")
  }

  if (interactive == TRUE) {
    performance_data$fake_PPV <- performance_data$PPV
    performance_data$fake_PPV[is.nan(performance_data$PPV)] <- -1

    performance_data <- performance_data %>%
      add_hover_text_to_performance_data(perf_dat_type,
        curve = "precision recall",
        stratified_by = stratified_by
      )

    if (perf_dat_type %in% c("one model with model column", "one model")) {
      precision_recall_curve <- create_reference_lines_for_plotly(perf_dat_type,
        "precision recall",
        prevalence = prevalence,
        size = size
      ) %>%
        add_lines_and_markers_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          sensitivity,
          PPV
        ) %>%
        add_interactive_marker_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          sensitivity,
          fake_PPV,
          stratified_by = stratified_by
        ) %>%
        set_styling_for_rtichoke("precision recall") %>% 
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
      precision_recall_curve <- create_reference_lines_for_plotly(perf_dat_type,
        "precision recall",
        prevalence = prevalence[1],
        population_color_vector =
          col_values[seq_len(length(prevalence))],
        size = size
      ) %>%
        add_lines_and_markers_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          sensitivity,
          PPV,
          col_values = col_values
        ) %>%
        add_interactive_marker_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          sensitivity,
          fake_PPV,
          stratified_by = stratified_by
        ) %>%
        set_styling_for_rtichoke("precision recall") %>% 
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
      precision_recall_curve <- create_reference_lines_for_plotly(perf_dat_type,
        "precision recall",
        prevalence = prevalence,
        population_color_vector = col_values[seq_len(length(prevalence))],
        size = size
      ) %>%
        add_lines_and_markers_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          sensitivity,
          PPV
        ) %>%
        add_interactive_marker_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          sensitivity,
          fake_PPV,
          stratified_by = stratified_by
        ) %>%
        set_styling_for_rtichoke("precision recall") %>% 
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
  return(precision_recall_curve)
}


#' Set the limits for percision recall curve
#'
#' @param precision_recall_curve a ggplot object of precision recall curve
#' @keywords internal
set_precision_recall_curve_limits <- function(precision_recall_curve) {
  precision_recall_curve +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1)
}
