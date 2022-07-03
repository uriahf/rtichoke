# Decision Curve ----------------------------------------------------------



#' Decision Curve
#'
#' Create a decision Curve
#'
#' @inheritParams create_roc_curve
#' @param type What type of Decision Curve, default choice is "conventional". 
#' Alternatives are "interventions avoided" and "combined" for both 
#' "conventional" and "interventions avoided" on the same view. 
#'
#' @export
#'
#' @examples
#' 
#' \dontrun{
#' 
#' 
#' create_decision_curve(
#'   probs = list(example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome)
#' )
#' 
#' create_decision_curve(
#'   probs = list(example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome,
#'   type = "interventions avoided")
#' )
#' 
#' create_decision_curve(
#'   probs = list(example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome,
#'   type = "combined")
#' )
#'
#' create_decision_curve(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   reals = list(example_dat$outcome)
#' )
#'
#' create_decision_curve(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   reals = list(example_dat$outcome),
#'   type = "interventions avoided"
#' )
#'
#' create_decision_curve(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   reals = list(example_dat$outcome),
#'   type = "combined"
#' )
#'
#' create_decision_curve(
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
#' create_decision_curve(
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
#'   type = "interventions avoided"
#' )
#' 
#'
#' create_decision_curve(
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
#'   type = "combined"
#' )
#'
#' }
create_decision_curve <- function(probs, reals, by = 0.01,
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
                                  size = NULL,
                                  type = "conventional") {
  
  match.arg(arg = type,
            choices = c("conventional", "interventions avoided", "combined"))
  
  if (!is.na(chosen_threshold)) {
    check_chosen_threshold_input(chosen_threshold)
  }

  prepare_performance_data(
    probs = probs,
    reals = reals,
    by = by,
    stratified_by = stratified_by
  ) %>%
    plot_decision_curve(
      chosen_threshold = chosen_threshold,
      interactive = interactive,
      col_values = col_values,
      size = size,
      type = type
    )
}



#' Decision Curve from Performance Data
#'
#' Plot a Precision decision Curve
#'
#' @inheritParams plot_roc_curve
#' @inheritParams create_decision_curve
#'
#' @examples
#' \dontrun{
#' 
#' 
#' one_pop_one_model %>%
#'   plot_decision_curve()
#'   
#' one_pop_one_model %>%
#'   plot_decision_curve(type = "interventions avoided")
#'   
#' one_pop_one_model %>%
#'   plot_decision_curve(type = "combined")
#'
#' multiple_models %>%
#'   plot_decision_curve()
#'   
#' multiple_models %>%
#'   plot_decision_curve(type = "interventions avoided")
#'   
#' multiple_models %>%
#'   plot_decision_curve(type = "combined")
#'
#' multiple_populations %>%
#'   plot_decision_curve()
#'   
#' multiple_populations %>%
#'   plot_decision_curve(type = "interventions avoided")
#'   
#' multiple_populations %>%
#'   plot_decision_curve(type = "combined")
#'   
#' }
#' @export

plot_decision_curve <- function(performance_data,
                                chosen_threshold = NA,
                                interactive = TRUE,
                                col_values = c(
                                  "#5BC0BE",
                                  "#FC8D62",
                                  "#8DA0CB",
                                  "#E78AC3",
                                  "#A4243B"
                                ),size = NULL,
                                type = "conventional") {
  
  
  if (!is.na(chosen_threshold)) {
    check_chosen_threshold_input(chosen_threshold)
  }
  
  if (interactive == FALSE) {
    
    print("decision reference lines from plotly")
    print(create_reference_lines_data_frame(curve,
                                            plotly = TRUE,
                                            prevalence,
                                            performance_data = performance_data
    ) )
    # 
    print("Creating decision curve")
    print(col_values)
    print(
      performance_data %>%
        create_ggplot_for_performance_metrics("threshold", "NB", col_values)
    )
    
    decision_curve <- performance_data %>%
      create_ggplot_for_performance_metrics("threshold", "NB", col_values) %>%
      add_reference_lines_to_ggplot(
        create_reference_lines_data_frame("decision", prevalence)
      ) %>%
      set_decision_curve_limits() +
      ggplot2::xlab("Probability Threshold") +
      ggplot2::ylab("Net Benefit")
  }
  if (interactive == TRUE) {
    
    if (type == "conventional") {
    
      decision_curve <- plot_conventional_decision(
        performance_data,
        col_values = col_values,
        size = size)
    
    }
    
    if (type == "interventions avoided") {
      
      decision_curve <- plot_interventions_avoided(
        performance_data,
        col_values = col_values,
        size = size)
      
    }
    
    if (type == "combined") {
      
      interventions_avoided_annotation <- list(
        text = "Interventions Avoided (per 100)",
        font = list(
          size = 18,
          color = "black"),
        xref = "paper",
        yref = "paper",
        yanchor = "bottom",
        xanchor = "center",
        align = "center",
        x = 0.5,
        y = 1,
        showarrow = FALSE
      ) 
      
      conventional_decision_annotation <- list(
        text = "Net Benefit",
        font = list(
          size = 18,
          color = "black"),
        xref = "paper",
        yref = "paper",
        yanchor = "bottom",
        xanchor = "center",
        align = "center",
        x = 0.5,
        y = 1,
        showarrow = FALSE
      ) 
      
      
      decision_curve <- plotly::subplot(
        performance_data %>%
          plot_interventions_avoided(col_values = col_values,
                                     size = size) %>%
          plotly::layout(annotations = interventions_avoided_annotation),
        performance_data %>%
          rtichoke:::plot_conventional_decision(col_values = col_values,
                                                size = size) %>%
          plotly::layout(annotations = conventional_decision_annotation), 
        nrows = 2,
        shareX = TRUE,
        shareY = FALSE, heights = c(0.5, 0.5)
      )
      
    }
    
    decision_curve <- decision_curve %>% 
      plotly::animation_slider(
        currentvalue = list(
          prefix = "Prob. Threshold: ",
          font = list(color="black"),
          xanchor = "left"),
        pad = list(t = 50)
      )
    
  }
  
  return(decision_curve)
  
}


#' Set the limits for Decision Curve
#'
#' @param decision_curve a ggplot object of Decision Curve
#' @keywords internal
set_decision_curve_limits <- function(decision_curve) {
  decision_curve +
    ggplot2::xlim(0, 1)
}


#' Decision Curve from Performance Data (Interventions Avoided)
#'
#' Plot interventions Avoided
#'
#' @inheritParams plot_roc_curve
#'
#' @examples
#'
#' \dontrun{
#'
#' one_pop_one_model %>%
#'   rtichoke:::plot_interventions_avoided()
#'
#' multiple_models %>%
#'   rtichoke:::plot_interventions_avoided()
#'
#' multiple_populations %>%
#'   rtichoke:::plot_interventions_avoided()
#'   
#' }
#' @keywords internal
plot_interventions_avoided <- function(performance_data,
                                       chosen_threshold = NA,
                                       interactive = TRUE,
                                       col_values = c(
                                         "#5BC0BE",
                                         "#FC8D62",
                                         "#8DA0CB",
                                         "#E78AC3",
                                         "#A4243B"
                                       ),
                                       size = NULL){
  
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
    performance_data, perf_dat_type
  )
  
  performance_data <- performance_data  %>% 
    dplyr::mutate(
      N = TP +TN  + FP + FN,
      prevalence = (TP + FN) / N,
      NB_intervention_all =   prevalence - (1- prevalence) * 
        (probability_threshold) / (1 - probability_threshold),
      NB_treatment_avoided = 100 * (NB - NB_intervention_all) * 
        ( (1 - probability_threshold) / probability_threshold )
    ) %>% 
    add_hover_text_to_performance_data(perf_dat_type, 
                                       curve = "interventions avoided",
                                       stratified_by = stratified_by) %>%
    dplyr::filter(probability_threshold > 0 & probability_threshold < 1)
  
  
  if (perf_dat_type %in% c("one model with model column", "one model")) {

    
    interventions_avoided <- plotly::plot_ly(
      height = size,
      width = size
    ) %>%
      add_lines_and_markers_from_performance_data(
        performance_data = performance_data,
        performance_data_type = perf_dat_type,
        probability_threshold,
        NB_treatment_avoided
      ) %>%
      add_interactive_marker_from_performance_data(
        performance_data = performance_data,
        performance_data_type = perf_dat_type,
        probability_threshold,
        NB_treatment_avoided,
        stratified_by = stratified_by
      ) %>%
      set_styling_for_rtichoke(
        "interventions avoided",
        min_y_range = 
          min(-10, min(performance_data$NB_treatment_avoided) - 10),
        max_y_range = 
          max(performance_data$NB_treatment_avoided) + 10  
      )
  }
  
  if (perf_dat_type == "several models") {
    interventions_avoided <- plotly::plot_ly(
      height = size,
      width = size
    ) %>%
      add_lines_and_markers_from_performance_data(
        performance_data = performance_data,
        performance_data_type = perf_dat_type,
        probability_threshold,
        NB_treatment_avoided
      )  %>%
      add_interactive_marker_from_performance_data(
        performance_data = performance_data,
        performance_data_type = perf_dat_type,
        probability_threshold,
        NB_treatment_avoided
      ) %>%
      set_styling_for_rtichoke(
        "interventions avoided",
        min_y_range = 
          min(-10, min(performance_data$NB_treatment_avoided) - 10),
        max_y_range = 
          max(performance_data$NB_treatment_avoided) + 10  
      )
  }
  
  if (perf_dat_type == "several populations") {
    
    interventions_avoided <- plotly::plot_ly(
      height = size,
      width = size
    ) %>%
      add_lines_and_markers_from_performance_data(
        performance_data = performance_data,
        performance_data_type = perf_dat_type,
        probability_threshold,
        NB_treatment_avoided,
        col_values = col_values
      )  %>%
      add_interactive_marker_from_performance_data(
        performance_data = performance_data,
        performance_data_type = perf_dat_type,
        probability_threshold,
        NB_treatment_avoided
      )  %>%
      set_styling_for_rtichoke(
        "interventions avoided",
        min_y_range = 
          min(-10, min(performance_data$NB_treatment_avoided) - 10),
        max_y_range = 
          max(performance_data$NB_treatment_avoided) + 10  
      )
  }

  
  return(interventions_avoided)
  
}




#' Decision Curve from Performance Data (Conventional)
#'
#' Plot Conventional Decision Curve
#'
#' @inheritParams plot_roc_curve
#'
#' @examples
#'
#' \dontrun{
#'
#' one_pop_one_model %>%
#'   rtichoke:::plot_conventional_decision()
#'
#' multiple_models %>%
#'   rtichoke:::plot_conventional_decision()
#'
#' multiple_populations %>%
#'   rtichoke:::plot_conventional_decision()
#'   
#' }
#' @keywords internal
plot_conventional_decision <- function(performance_data,
                                       chosen_threshold = NA,
                                       interactive = TRUE,
                                       col_values = c(
                                         "#5BC0BE",
                                         "#FC8D62",
                                         "#8DA0CB",
                                         "#E78AC3",
                                         "#A4243B"
                                       ),
                                       size = NULL){
  
  
  stratified_by <- check_performance_data_stratification(
    performance_data
  )
  
  perf_dat_type <- check_performance_data_type_for_plotly(
    performance_data = performance_data
  )
  prevalence <- get_prevalence_from_performance_data(
    performance_data, perf_dat_type
  )
  
  performance_data <- performance_data %>%
    add_hover_text_to_performance_data(perf_dat_type, 
                                       curve = "decision",
                                       stratified_by = stratified_by) %>%
    dplyr::filter(probability_threshold > 0 & probability_threshold < 1)
  
  
  
  if (perf_dat_type %in% c("one model with model column", "one model")) {
    
    
    conventional_decision <- create_reference_lines_for_plotly(
      perf_dat_type,
      "decision",
      prevalence = prevalence,
      size = size,
      performance_data = performance_data
    ) %>%
      add_lines_and_markers_from_performance_data(
        performance_data = performance_data,
        performance_data_type = perf_dat_type,
        probability_threshold,
        NB
      ) %>%
      add_interactive_marker_from_performance_data(
        performance_data = performance_data,
        performance_data_type = perf_dat_type,
        probability_threshold,
        NB,
        stratified_by = stratified_by
      ) %>%
      set_styling_for_rtichoke(
        "decision",
        max_y_range = max(performance_data$NB,
                          na.rm = TRUE) + 
          diff(range(performance_data$NB, na.rm = TRUE)) * 0.1,
        min_y_range = min(performance_data$NB[performance_data$NB != -Inf],
                          na.rm = TRUE) - 
          diff(range(performance_data$NB, na.rm = TRUE)) * 0.1)
    
  }
  
  if (perf_dat_type == "several models") {
    
    conventional_decision <- create_reference_lines_for_plotly(
      perf_dat_type,
      "decision",
      prevalence = prevalence[1],
      population_color_vector = col_values[seq_len(length(prevalence))],
      size = size,
      performance_data = performance_data
    ) %>%
      add_lines_and_markers_from_performance_data(
        performance_data = performance_data,
        performance_data_type = perf_dat_type,
        probability_threshold,
        NB,
        col_values = col_values
      ) %>%
      add_interactive_marker_from_performance_data(
        performance_data = performance_data,
        performance_data_type = perf_dat_type,
        probability_threshold,
        NB,
        stratified_by = stratified_by
      ) %>%
      set_styling_for_rtichoke(
        "decision",
        max_y_range = max(performance_data$NB,
                          na.rm = TRUE) + 
          diff(range(performance_data$NB, na.rm = TRUE)) * 0.1,
        min_y_range = min(performance_data$NB[performance_data$NB != -Inf],
                          na.rm = TRUE) - 
          diff(range(performance_data$NB, na.rm = TRUE)) * 0.1)
    
  }
  
  if (perf_dat_type == "several populations") {
    
    conventional_decision <- create_reference_lines_for_plotly(perf_dat_type,
                                                        "decision",
                                                        prevalence = prevalence,
                                                        population_color_vector =
                                                          col_values[seq_len(length(prevalence))],
                                                        size = size,
                                                        performance_data = performance_data
    ) %>%
      add_lines_and_markers_from_performance_data(
        performance_data = performance_data,
        performance_data_type = perf_dat_type,
        probability_threshold,
        NB
      ) %>%
      add_interactive_marker_from_performance_data(
        performance_data = performance_data,
        performance_data_type = perf_dat_type,
        probability_threshold,
        NB,
        stratified_by = stratified_by
      ) %>%
      set_styling_for_rtichoke(
        "decision",
        max_y_range = max(performance_data$NB,
                          na.rm = TRUE) + 
          diff(range(performance_data$NB, na.rm = TRUE)) * 0.1,
        min_y_range = min(performance_data$NB[performance_data$NB != -Inf],
                          na.rm = TRUE) - 
          diff(range(performance_data$NB, na.rm = TRUE)) * 0.1)
  }
  
  
  return(conventional_decision)
  
}