# Decision Curve ----------------------------------------------------------



#' Decision Curve
#'
#' Create a Decision Curve
#'
#' @inheritParams create_roc_curve
#'
#' @export
#'
create_decision_curve <- function(probs, real, by = 0.01,
                                  enforce_percentiles_symmetry = F) {
  prepare_performance_data(
    probs = probs,
    real = real,
    by = by,
    enforce_percentiles_symmetry = enforce_percentiles_symmetry
  ) %>%
    plot_decision_curve()
}



#' Precision Recall Curve from Performance Data
#'
#' Plot a Precision Recall Curve
#'
#' @inheritParams plot_roc_curve
#'
#' @examples
#'
#' one_pop_one_model_as_a_vector %>%
#'   plot_decision_curve()
#'
#' one_pop_one_model_as_a_vector_enforced_percentiles_symmetry %>%
#'   plot_decision_curve()
#'
#' one_pop_one_model_as_a_list %>%
#'   plot_decision_curve()
#'
#' one_pop_one_model_as_a_list_enforced_percentiles_symmetry %>%
#'   plot_decision_curve()
#'
#' one_pop_three_models %>%
#'   plot_decision_curve()
#'
#' one_pop_three_models_enforced_percentiles_symmetry %>%
#'   plot_decision_curve()
#'
#' train_and_test_sets %>%
#'   plot_decision_curve()
#'
#' train_and_test_sets_enforced_percentiles_symmetry %>%
#'   plot_decision_curve()
#' \dontrun{
#'
#' one_pop_one_model_as_a_vector %>%
#'   plot_decision_curve(interactive = T)
#'
#' one_pop_one_model_as_a_vector_enforced_percentiles_symmetry %>%
#'   plot_decision_curve(interactive = T, main_slider = "predicted_positives_percent")
#'
#' one_pop_one_model_as_a_list %>%
#'   plot_decision_curve(interactive = T)
#'
#' one_pop_one_model_as_a_list_enforced_percentiles_symmetry %>%
#'   plot_decision_curve(interactive = T, main_slider = "predicted_positives_percent")
#'
#' one_pop_three_models %>%
#'   plot_decision_curve(interactive = T)
#'
#' one_pop_three_models_enforced_percentiles_symmetry %>%
#'   plot_decision_curve(interactive = T, main_slider = "predicted_positives_percent")
#'
#' train_and_test_sets %>%
#'   plot_decision_curve(interactive = T)
#'
#' train_and_test_sets_enforced_percentiles_symmetry %>%
#'   plot_decision_curve(interactive = T, main_slider = "predicted_positives_percent")
#' }
#' @export

plot_decision_curve <- function(performance_data,
                                chosen_threshold = NA,
                                interactive = F,
                                main_slider = "threshold",
                                col_values = c(
                                  "#5BC0BE",
                                  "#FC8D62",
                                  "#8DA0CB",
                                  "#E78AC3",
                                  "#A4243B"
                                )) {
  
  perf_dat_type <- check_performance_data_type_for_plotly(performance_data = performance_data)
  prevalence <- get_prevalence_from_performance_data(performance_data, perf_dat_type)
  
  if (interactive == F) {
    decision_curve <- performance_data %>%
      create_ggplot_for_performance_metrics("threshold", "NB") %>%
      add_reference_lines_to_ggplot(create_reference_lines_data_frame("decision", prevalence)) %>%
      set_decision_curve_limits()
  }
  if (interactive == T) {
    
    print(perf_dat_type)
    print(prevalence)
    
    performance_data <- performance_data %>% 
      add_hover_text_to_performance_data(perf_dat_type, curve = "decision")
    
    if (perf_dat_type %in% c("one model with model column", "one model")) {
      
      decision_curve <- create_reference_lines_for_plotly(perf_dat_type,
                                                          "decision",
                                                          prevalence = prevalence) %>% 
        add_lines_and_markers_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          threshold,
          NB, 
          main_slider
        ) %>%
        add_interactive_marker_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          threshold,
          NB, 
          main_slider
        ) %>%
        set_styling_for_rtichoke("decision")
    }
    
    if (perf_dat_type == "several models") {
      
      decision_curve <- create_reference_lines_for_plotly(perf_dat_type, 
                                                                  "decision",
                                                                  prevalence = prevalence,
                                                                  population_color_vector = col_values) %>% 
        add_lines_and_markers_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          threshold,
          NB, 
          col_values = col_values, 
          main_slider = main_slider
        )  %>%
        add_interactive_marker_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          threshold,
          NB, 
          main_slider = main_slider
        )  %>%
        set_styling_for_rtichoke("decision")
      
    }
    
    if (perf_dat_type == "several populations") {
      
      decision_curve <- create_reference_lines_for_plotly(perf_dat_type, 
                                                                  "decision",
                                                                  prevalence = prevalence,
                                                                  population_color_vector = col_values) %>% 
        add_lines_and_markers_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          threshold,
          NB, 
          main_slider = main_slider
        )  %>%
        add_interactive_marker_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          threshold,
          NB, 
          main_slider = main_slider
        )  %>%
        set_styling_for_rtichoke("decision")
      
    }
    
  }
  
  return(decision_curve)
}


#' Set the limits for Decision Curve
#'
#' @param decision_curve a ggplot object of Decision Curve
#'
set_decision_curve_limits <- function(decision_curve) {
  decision_curve +
    ggplot2::xlim(0, 1)
}
