# ROC Curve ---------------------------------------------------------------


#' ROC Curve
#'
#' Create a ROC Curve
#'
#' @inheritParams prepare_performance_data
#'
#' @export
#'
#'
create_roc_curve <- function(probs, real, by = 0.01,
                             enforce_percentiles_symmetry = F) {
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
#'
#'
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
#'   plot_roc_curve(interactive = T)
#'
#' one_pop_one_model_as_a_vector_enforced_percentiles_symmetry %>%
#'   plot_roc_curve(interactive = T, main_slider = "predicted_positives_percent")
#'
#' one_pop_one_model_as_a_list %>%
#'   plot_roc_curve(interactive = T)
#'
#' one_pop_one_model_as_a_list_enforced_percentiles_symmetry %>%
#'   plot_roc_curve(interactive = T, main_slider = "predicted_positives_percent")
#'
#' one_pop_three_models %>%
#'   plot_roc_curve(interactive = T)
#'
#' one_pop_three_models_enforced_percentiles_symmetry %>%
#'   plot_roc_curve(interactive = T, main_slider = "predicted_positives_percent")
#'
#' train_and_test_sets %>%
#'   plot_roc_curve(interactive = T)
#'
#' train_and_test_sets_enforced_percentiles_symmetry %>%
#'   plot_roc_curve(interactive = T, main_slider = "predicted_positives_percent")
#' }
#'
#' @export


plot_roc_curve <- function(performance_data,
                           chosen_threshold = NA,
                           interactive = F,
                           main_slider = "threshold") {
  
  if (interactive == F) {
    
    reference_lines <- create_reference_lines_data_frame("roc")
    
    roc_curve <- performance_data %>%
      create_ggplot_for_performance_metrics("FPR", "sensitivity") %>%
      add_reference_lines_to_ggplot(reference_lines)
  }
  
  if (interactive == T) {
    
    reference_lines <- create_reference_lines_for_plotly(curve = "roc")
    
    reference_lines
    
    roc_curve <- performance_data %>%
      create_plotly_for_performance_metrics(FPR,
                                            sensitivity,
                                            reference_lines = reference_lines
      ) %>%
      plotly::layout(
        xaxis = list(
          title = "1 - Specificity"
        ),
        yaxis = list(
          title = "Sensitivity"
        ),
        showlegend = F
      ) %>%
      plotly::config(displayModeBar = F)
  }
  
  return(roc_curve)
}


