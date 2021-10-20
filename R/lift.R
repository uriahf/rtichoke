# LIFT Curve ---------------------------------------------------------------


#' LIFT Curve
#'
#' Create a LIFT Curve
#'
#' @inheritParams prepare_performance_data
#' @param col_values color palette
#'
#' @export
#'
#'
create_lift_curve <- function(probs, real, by = 0.01,
                             enforce_percentiles_symmetry = F,
                             col_values = c(
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
#' @param col_values color palette
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
                           col_values = c(
                             "#21DACD",
                             "#B6C174",
                             "#A7DA2E",
                             "#C2C172",
                             "#FFD700"
                           )) {
  
  if (interactive == F) {
    
    reference_lines <- create_reference_lines_data_frame("lift")
    
    lift_curve <- performance_data %>%
      create_ggplot_for_performance_metrics("predicted_positives_percent", "lift") %>%
      add_reference_lines_to_ggplot(reference_lines) %>% 
      set_lift_curve_limits()
  }
  
  if (interactive == T) {

    performance_data$fake_lift <- performance_data$lift
    performance_data$fake_lift[is.nan(performance_data$lift)] <- -1  
    
    perf_dat_type <- check_performance_data_type_for_plotly(performance_data)
    
    performance_data <- performance_data %>% 
      add_hover_text_to_performance_data(perf_dat_type, curve = "lift")
    
    print(perf_dat_type)

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
          fake_lift,
          main_slider
        ) %>%
         set_styling_for_rtichoke("lift", max_y_range = max(performance_data$lift, na.rm = T) + 0.1)
    }

    if (perf_dat_type == "several models") {

      lift_curve <- create_reference_lines_for_plotly(perf_dat_type,
                                                     "lift",
                                                     population_color_vector = col_values) %>%
        add_lines_and_markers_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          predicted_positives_percent,
          lift,
          col_values = col_values,
          main_slider = main_slider
        )  %>%
        add_interactive_marker_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          predicted_positives_percent,
          fake_lift,
          main_slider = main_slider
        )  %>%
        set_styling_for_rtichoke("lift", max_y_range = max(performance_data$lift, na.rm = T) + 0.1)

    }

    if (perf_dat_type == "several populations") {

      lift_curve <- create_reference_lines_for_plotly(perf_dat_type,
                                                     "lift",
                                                     population_color_vector = col_values) %>%
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
          fake_lift,
          main_slider = main_slider
        )  %>%
        set_styling_for_rtichoke("lift", max_y_range = max(performance_data$lift, na.rm = T) + 0.1)

    }
  }
  
  return(lift_curve)
}


#' Set the limits for lift curve
#'
#' @param lift_curve a ggplot object of lift curve
#'
set_lift_curve_limits <- function(lift_curve) {
  lift_curve +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, NA)
}