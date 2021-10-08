# LIFT Curve ---------------------------------------------------------------


#' LIFT Curve
#'
#' Create a LIFT Curve
#'
#' @inheritParams prepare_performance_data
#'
#' @export
#'
#'
create_lift_curve <- function(probs, real, by = 0.01,
                             enforce_percentiles_symmetry = F,
                             color_palette = c(
                               "#21DACD",
                               "#B6C174",
                               "#A7DA2E",
                               "#C2C172",
                               "#FFD700"
                             )) {
  prepare_performance_data(
    probs = probs,
    real = real,
    by = by,
    enforce_percentiles_symmetry = enforce_percentiles_symmetry
  ) %>%
    plot_lift_curve()
}


#' LIFT Curve from Performance Data
#'
#' Plot a LIFT Curve
#'
#' @param performance_data an rtichoke Performance Data
#' @param chosen_threshold a chosen threshold to display
#' @param interactive whether the plot should be interactive
#' @param main_slider what is the main slider - threshold, percent positives or positives
#' @param color_palette color palette for the curves
#'
#' @examples
#'
#' one_pop_one_model_as_a_vector %>%
#'   plot_lift_curve()
#'
#' one_pop_one_model_as_a_vector_enforced_percentiles_symmetry %>%
#'   plot_lift_curve()
#'
#' one_pop_one_model_as_a_list %>%
#'   plot_lift_curve()
#'
#' one_pop_one_model_as_a_list_enforced_percentiles_symmetry %>%
#'   plot_lift_curve()
#'
#' one_pop_three_models %>%
#'   plot_lift_curve()
#'
#' one_pop_three_models_enforced_percentiles_symmetry %>%
#'   plot_lift_curve()
#'
#' train_and_test_sets %>%
#'   plot_lift_curve()
#'
#' train_and_test_sets_enforced_percentiles_symmetry %>%
#'   plot_lift_curve()
#' \dontrun{
#'
#' one_pop_one_model_as_a_vector %>%
#'   plot_lift_curve(interactive = T)
#'
#' one_pop_one_model_as_a_vector_enforced_percentiles_symmetry %>%
#'   plot_lift_curve(interactive = T, main_slider = "predicted_positives_percent")
#'
#' one_pop_one_model_as_a_list %>%
#'   plot_lift_curve(interactive = T)
#'
#' one_pop_one_model_as_a_list_enforced_percentiles_symmetry %>%
#'   plot_lift_curve(interactive = T, main_slider = "predicted_positives_percent")
#'
#' one_pop_three_models %>%
#'   plot_lift_curve(interactive = T)
#'
#' one_pop_three_models_enforced_percentiles_symmetry %>%
#'   plot_lift_curve(interactive = T, main_slider = "predicted_positives_percent")
#'
#' train_and_test_sets %>%
#'   plot_lift_curve(interactive = T)
#'
#' train_and_test_sets_enforced_percentiles_symmetry %>%
#'   plot_lift_curve(interactive = T, main_slider = "predicted_positives_percent")
#' }
#'
#' @export
plot_lift_curve <- function(performance_data,
                           chosen_threshold = NA,
                           interactive = F,
                           main_slider = "threshold",
                           color_palette = c(
                             "#21DACD",
                             "#B6C174",
                             "#A7DA2E",
                             "#C2C172",
                             "#FFD700"
                           )) {
  
  if (interactive == F) {
    
    reference_lines <- create_reference_lines_data_frame("lift")
    
    lift_curve <- performance_data %>%
      create_ggplot_for_performance_metrics("FPR", "sensitivity") %>%
      add_reference_lines_to_ggplot(reference_lines)
  }
  
  if (interactive == T) {
    
    perf_dat_type <- rtichoke::check_performance_data_type_for_plotly(performance_data)
    
    performance_data <- performance_data %>% 
      mutate(fake_dot = 0) 
    
    if (perf_dat_type %in% c("one model with model column", "one model")) {
      
      lift_curve <- create_reference_lines_for_plotly(perf_dat_type, "lift") %>% 
        add_lines_and_markers_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          predicted_positives_percent,
          lift, 
          main_slider
        ) %>%
        add_interactive_marker_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          predicted_positives_percent,
          lift,
          main_slider
        ) %>%
         set_styling_for_rtichoke("lift")
    }
    
    if (perf_dat_type == "several models") {
      
      lift_curve <- create_reference_lines_for_plotly(perf_dat_type, 
                                                     "lift", 
                                                     population_color_vector = color_palette) %>% 
        add_lines_and_markers_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          predicted_positives_percent,
          lift, 
          col_values = color_palette, 
          main_slider = main_slider
        )  %>%
        add_interactive_marker_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          predicted_positives_percent,
          lift, 
          main_slider = main_slider
        )  %>%
        set_styling_for_rtichoke("lift")
      
    }
    
    if (perf_dat_type == "several populations") {
      
      lift_curve <- create_reference_lines_for_plotly(perf_dat_type, 
                                                     "lift", 
                                                     population_color_vector = color_palette) %>% 
        add_lines_and_markers_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          predicted_positives_percent,
          lift, 
          main_slider = main_slider
        )  %>%
        add_interactive_marker_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          predicted_positives_percent,
          lift, 
          main_slider = main_slider
        )  %>%
        set_styling_for_rtichoke("lift")
      
    }
  }
  
  return(lift_curve)
}


