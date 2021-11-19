# ROC Curve ---------------------------------------------------------------


#' ROC Curve
#'
#' Create a ROC Curve 
#'
#' @inheritParams prepare_performance_data
#' @param col_values color palette
#'
#' @export
#'
#'
create_roc_curve <- function(probs, real, by = 0.01,
                             enforce_percentiles_symmetry = F,
                             col_values = c(
                               "#5BC0BE",
                               "#FC8D62",
                               "#8DA0CB",
                               "#E78AC3",
                               "#A4243B"
                             )) {
  prepare_performance_data(
    probs = probs,
    real = real,
    by = by,
    enforce_percentiles_symmetry = enforce_percentiles_symmetry
  ) %>%
    plot_roc_curve()
}


#' ROC Curve from Performance Data
#'
#' Plot a ROC Curve
#'
#' @param performance_data an rtichoke Performance Data
#' @param chosen_threshold a chosen threshold to display
#' @param interactive whether the plot should be interactive
#' @param main_slider what is the main slider - threshold, percent positives or positives
#' @param col_values color palette
#'
#' @examples
#'
#' one_pop_one_model_as_a_vector %>%
#'   plot_roc_curve()
#'
#' one_pop_one_model_as_a_vector_enforced_percentiles_symmetry %>%
#'   plot_roc_curve()
#'
#' one_pop_one_model_as_a_list %>%
#'   plot_roc_curve()
#'
#' one_pop_one_model_as_a_list_enforced_percentiles_symmetry %>%
#'   plot_roc_curve()
#'
#' one_pop_three_models %>%
#'   plot_roc_curve()
#'
#' one_pop_three_models_enforced_percentiles_symmetry %>%
#'   plot_roc_curve()
#'
#' train_and_test_sets %>%
#'   plot_roc_curve()
#'
#' train_and_test_sets_enforced_percentiles_symmetry %>%
#'   plot_roc_curve()
#' \dontrun{
#'
#' one_pop_one_model_as_a_vector %>%
#'   plot_roc_curve(interactive = TRUE)
#'
#' one_pop_one_model_as_a_vector_enforced_percentiles_symmetry %>%
#'   plot_roc_curve(interactive = TRUE, main_slider = "predicted_positives_percent")
#'
#' one_pop_one_model_as_a_list %>%
#'   plot_roc_curve(interactive = TRUE)
#'
#' one_pop_one_model_as_a_list_enforced_percentiles_symmetry %>%
#'   plot_roc_curve(interactive = TRUE, main_slider = "predicted_positives_percent")
#'
#' one_pop_three_models %>%
#'   plot_roc_curve(interactive = TRUE)
#'
#' one_pop_three_models_enforced_percentiles_symmetry %>%
#'   plot_roc_curve(interactive = TRUE, main_slider = "predicted_positives_percent")
#'
#' train_and_test_sets %>%
#'   plot_roc_curve(interactive = TRUE)
#'
#' train_and_test_sets_enforced_percentiles_symmetry %>%
#'   plot_roc_curve(interactive = TRUE, main_slider = "predicted_positives_percent")
#' }
#'
#' @export
plot_roc_curve <- function(performance_data,
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
  
  if (interactive == FALSE) {
    
    reference_lines <- create_reference_lines_data_frame("roc")
    
    roc_curve <- performance_data %>%
      create_ggplot_for_performance_metrics("FPR", "sensitivity") %>%
      add_reference_lines_to_ggplot(reference_lines) +
      ggplot2::xlab("1 - Specificity") +
      ggplot2::ylab("Sensitivity")
  }
  
  if (interactive == TRUE) {
    
    perf_dat_type <- check_performance_data_type_for_plotly(performance_data)
    
    performance_data <- performance_data %>% 
      add_hover_text_to_performance_data(perf_dat_type, curve = "roc")
    
    
    if (perf_dat_type %in% c("one model with model column", "one model")) {
    
      roc_curve <- create_reference_lines_for_plotly(perf_dat_type, "roc") %>% 
      add_lines_and_markers_from_performance_data(
        performance_data = performance_data,
        performance_data_type = perf_dat_type,
        FPR,
        sensitivity, 
        main_slider
      ) %>%
      add_interactive_marker_from_performance_data(
        performance_data = performance_data,
        performance_data_type = perf_dat_type,
        FPR,
        sensitivity,
        main_slider
      ) %>%
      set_styling_for_rtichoke("roc")
    }
    
    if (perf_dat_type == "several models") {
      
      roc_curve <- create_reference_lines_for_plotly(perf_dat_type, 
                                        "roc", 
                                        population_color_vector = col_values) %>% 
        add_lines_and_markers_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          FPR,
          sensitivity,
          col_values = col_values, 
          main_slider = main_slider
        )  %>%
        add_interactive_marker_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          FPR,
          sensitivity,
          main_slider = main_slider
        )  %>%
        set_styling_for_rtichoke("roc")
      
    }
    
    if (perf_dat_type == "several populations") {
      
      roc_curve <- create_reference_lines_for_plotly(perf_dat_type, 
                                        "roc", 
                                        population_color_vector = col_values) %>% 
        add_lines_and_markers_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          FPR,
          sensitivity,
          main_slider = main_slider
        )  %>%
        add_interactive_marker_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          FPR,
          sensitivity,
          main_slider = main_slider
        )  %>%
        set_styling_for_rtichoke("roc")
      
    }
  }
  
  return(roc_curve)
}


