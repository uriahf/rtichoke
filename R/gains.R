# Gains Curve -------------------------------------------------------------

#' Gains Curve
#'
#' Create a Gains Curve
#'
#' @inheritParams create_roc_curve
#'
#' @export
#'
create_gains_curve <- function(probs, real, by = 0.01,
                               enforce_percentiles_symmetry = F) {
  prepare_performance_data(
    probs = probs,
    real = real,
    by = by,
    enforce_percentiles_symmetry = enforce_percentiles_symmetry
  ) %>%
    plot_gains_curve()
}


#' Gains Curve from Performance Data
#'
#' Plot a Gains Curve
#'
#' @inheritParams plot_roc_curve
#'
#' @examples
#'
#' one_pop_one_model_as_a_vector %>%
#'   plot_gains_curve()
#'
#' one_pop_one_model_as_a_vector_enforced_percentiles_symmetry %>%
#'   plot_gains_curve()
#'
#' one_pop_one_model_as_a_list %>%
#'   plot_gains_curve()
#'
#' one_pop_one_model_as_a_list_enforced_percentiles_symmetry %>%
#'   plot_gains_curve()
#'
#' one_pop_three_models %>%
#'   plot_gains_curve()
#'
#' one_pop_three_models_enforced_percentiles_symmetry %>%
#'   plot_gains_curve()
#'
#' train_and_test_sets %>%
#'   plot_gains_curve()
#'
#' train_and_test_sets_enforced_percentiles_symmetry %>%
#'   plot_gains_curve()
#' \dontrun{
#'
#' one_pop_one_model_as_a_vector %>%
#'   plot_gains_curve(interactive = T)
#'
#' one_pop_one_model_as_a_vector_enforced_percentiles_symmetry %>%
#'   plot_gains_curve(interactive = T, main_slider = "predicted_positives_percent")
#'
#' one_pop_one_model_as_a_list %>%
#'   plot_gains_curve(interactive = T)
#'
#' one_pop_one_model_as_a_list_enforced_percentiles_symmetry %>%
#'   plot_gains_curve(interactive = T, main_slider = "predicted_positives_percent")
#'
#' one_pop_three_models %>%
#'   plot_gains_curve(interactive = T)
#'
#' one_pop_three_models_enforced_percentiles_symmetry %>%
#'   plot_gains_curve(interactive = T, main_slider = "predicted_positives_percent")
#'
#' train_and_test_sets %>%
#'   plot_gains_curve(interactive = T)
#'
#' train_and_test_sets_enforced_percentiles_symmetry %>%
#'   plot_gains_curve(interactive = T, main_slider = "predicted_positives_percent")
#' }
#'
#' @export

plot_gains_curve <- function(performance_data,
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
  
  perf_dat_type <- check_performance_data_type_for_plotly(performance_data = performance_data)
  prevalence <- get_prevalence_from_performance_data(performance_data, perf_dat_type)
  
  if (interactive == F) {
  reference_lines <- create_reference_lines_data_frame("gains", prevalence)

    gains_curve <- performance_data %>%
      create_ggplot_for_performance_metrics("predicted_positives_percent", "sensitivity") %>%
      add_reference_lines_to_ggplot(reference_lines) %>%
      set_gains_curve_limits()
  }

  if (interactive == T) {


    print(perf_dat_type)
    print(prevalence)
    
    performance_data <- performance_data %>% 
      add_hover_text_to_performance_data(perf_dat_type, curve = "gains")
    
    if (perf_dat_type %in% c("one model with model column", "one model")) {
      
      gains_curve <- create_reference_lines_for_plotly(perf_dat_type, 
                                                                  "gains",
                                                                  prevalence = prevalence) %>% 
        add_lines_and_markers_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          predicted_positives_percent,
          sensitivity, 
          main_slider
        ) %>%
        add_interactive_marker_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          predicted_positives_percent,
          sensitivity, 
          main_slider
        ) %>%
        set_styling_for_rtichoke("gains")
    }
    
    if (perf_dat_type == "several models") {
      
      gains_curve <- create_reference_lines_for_plotly(perf_dat_type, 
                                                                  "gains",
                                                                  prevalence = prevalence,
                                                                  population_color_vector = col_values) %>% 
        add_lines_and_markers_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          predicted_positives_percent,
          sensitivity, 
          col_values = col_values, 
          main_slider = main_slider
        )  %>%
        add_interactive_marker_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          predicted_positives_percent,
          sensitivity, 
          main_slider = main_slider
        )  %>%
        set_styling_for_rtichoke("gains")
      
    }
    
    if (perf_dat_type == "several populations") {
      
      gains_curve <- create_reference_lines_for_plotly(perf_dat_type,
                                                       "gains",
                                                       prevalence = prevalence,
                                                       population_color_vector = col_values) %>% 
        add_lines_and_markers_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          predicted_positives_percent,
          sensitivity, 
          main_slider = main_slider
        )  %>%
        add_interactive_marker_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          predicted_positives_percent,
          sensitivity, 
          main_slider = main_slider
        )  %>%
        set_styling_for_rtichoke("gains")
      
    }
    
  }
  
  return(gains_curve)
  
}

#' Add reference lines to Gains Curve
#'
#' @param gains_curve a ggplot object of a Gains Curve
#' @param prevalence the prevalence of the outcome
add_gains_curve_reference_lines <- function(gains_curve, prevalence) {
  gains_curve$layers <- c(
    ggplot2::geom_segment(x = 0, y = 0, xend = 1, yend = 1, color = "grey"),
    purrr::map2(
      prevalence, c(
        "#5E7F9A",
        "#931B53",
        "#F7DC2E",
        "#C6C174",
        "#75DBCD"
      )[1:length(prevalence)],
      add_prevalence_layers_to_gains_curve
    ) %>% unlist(),
    gains_curve$layers
  )
  gains_curve
}


#' Set the limits for Gains Curve
#'
#' @param gains_curve a ggplot object of a Gains Curve
#'
set_gains_curve_limits <- function(gains_curve) {
  gains_curve +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1)
}