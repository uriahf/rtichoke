# Precision Recall Curve --------------------------------------------------


#' Precision Recall Curve
#'
#' Create a Precision Recall Curve
#'
#' @inheritParams create_roc_curve
#'
#' @export
#'
create_precision_recall_curve <- function(probs, real, by = 0.01,
                                          enforce_percentiles_symmetry = F) {
  prepare_performance_data(
    probs = probs,
    real = real,
    by = by,
    enforce_percentiles_symmetry = enforce_percentiles_symmetry
  ) %>%
    plot_precision_recall_curve()
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
#'   plot_precision_recall_curve()
#'
#' one_pop_one_model_as_a_vector_enforced_percentiles_symmetry %>%
#'   plot_precision_recall_curve()
#'
#' one_pop_one_model_as_a_list %>%
#'   plot_precision_recall_curve()
#'
#' one_pop_one_model_as_a_list_enforced_percentiles_symmetry %>%
#'   plot_precision_recall_curve()
#'
#' one_pop_three_models %>%
#'   plot_precision_recall_curve()
#'
#' one_pop_three_models_enforced_percentiles_symmetry %>%
#'   plot_precision_recall_curve()
#'
#' train_and_test_sets %>%
#'   plot_precision_recall_curve()
#'
#' train_and_test_sets_enforced_percentiles_symmetry %>%
#'   plot_precision_recall_curve()
#' \dontrun{
#'
#' one_pop_one_model_as_a_vector %>%
#'   plot_precision_recall_curve(interactive = T)
#'
#' one_pop_one_model_as_a_vector_enforced_percentiles_symmetry %>%
#'   plot_precision_recall_curve(interactive = T, main_slider = "predicted_positives_percent")
#'
#' one_pop_one_model_as_a_list %>%
#'   plot_precision_recall_curve(interactive = T)
#'
#' one_pop_one_model_as_a_list_enforced_percentiles_symmetry %>%
#'   plot_precision_recall_curve(interactive = T, main_slider = "predicted_positives_percent")
#'
#' one_pop_three_models %>%
#'   plot_precision_recall_curve(interactive = T)
#'
#' one_pop_three_models_enforced_percentiles_symmetry %>%
#'   plot_precision_recall_curve(interactive = T, main_slider = "predicted_positives_percent")
#'
#' train_and_test_sets %>%
#'   plot_precision_recall_curve(interactive = T)
#'
#' train_and_test_sets_enforced_percentiles_symmetry %>%
#'   plot_precision_recall_curve(interactive = T, main_slider = "predicted_positives_percent")
#' }
#'
#' @export

plot_precision_recall_curve <- function(performance_data,
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
  performance_data_type <- check_performance_data_type_for_plotly(performance_data)
  prevalence <- get_prevalence_from_performance_data(performance_data, performance_data_type)
  
  if (interactive == F) {
  
    reference_lines <- create_reference_lines_data_frame("precision recall", prevalence)
  
    precision_recall_curve <- performance_data %>%
      create_ggplot_for_performance_metrics("sensitivity", "PPV") %>%
      add_reference_lines_to_ggplot(reference_lines) %>%
      set_precision_recall_curve_limits()
  }
  
  if (interactive == T) {
    # precision_recall_curve <- performance_data %>%
    #   create_plotly_for_performance_metrics(sensitivity, PPV,
    #                                         reference_lines = reference_lines) %>%
    #   plotly::layout(
    #     xaxis = list(
    #       title = "Sensitivity",
    #       showgrid = F
    #     ),
    #     yaxis = list(
    #       title = "PPV",
    #       showgrid = F
    #     ),
    #     showlegend = FALSE
    #   ) %>%
    #   plotly::config(displayModeBar = F)
    
    perf_dat_type <- check_performance_data_type_for_plotly(performance_data = performance_data)
    prevalence <- get_prevalence_from_performance_data(performance_data, perf_dat_type)
    
    print(perf_dat_type)
    print(prevalence)
    
    if (perf_dat_type %in% c("one model with model column", "one model")) {
      
      precision_recall_curve <- create_reference_lines_for_plotly(perf_dat_type, 
                                                                  "precision recall",
                                                                  prevalence = prevalence) %>% 
        add_lines_and_markers_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          sensitivity,
          PPV, 
          main_slider
        ) %>%
        add_interactive_marker_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          sensitivity,
          PPV, 
          main_slider
        ) %>%
        set_styling_for_rtichoke("precision recall")
    }
    
    if (perf_dat_type == "several models") {
      
      precision_recall_curve <- create_reference_lines_for_plotly(perf_dat_type, 
                                                                  "precision recall",
                                                                  prevalence = prevalence,
                                                     population_color_vector = col_values) %>% 
        add_lines_and_markers_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          sensitivity,
          PPV, 
          col_values = col_values, 
          main_slider = main_slider
        )  %>%
        add_interactive_marker_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          sensitivity,
          PPV, 
          main_slider = main_slider
        )  %>%
        set_styling_for_rtichoke("precision recall")
      
    }
    
    if (perf_dat_type == "several populations") {
      
      precision_recall_curve <- create_reference_lines_for_plotly(perf_dat_type, 
                                                                  "precision recall",
                                                                  prevalence = prevalence,
                                                     population_color_vector = col_values) %>% 
        add_lines_and_markers_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          sensitivity,
          PPV, 
          main_slider = main_slider
        )  %>%
        add_interactive_marker_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          sensitivity,
          PPV, 
          main_slider = main_slider
        )  %>%
        set_styling_for_rtichoke("precision recall")
      
    }
    
  }
  return(precision_recall_curve)
}


#' Set the limits for percision recall curve
#'
#' @param precision_recall_curve a ggplot object of precision recall curve
#'
set_precision_recall_curve_limits <- function(precision_recall_curve) {
  precision_recall_curve +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1)
}


