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
#' 
#' create_precision_recall_curve(
#'   probs = example_dat$estimated_probabilities,
#'   real = example_dat$outcome
#' )
#' 
#' create_precision_recall_curve(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   real = example_dat$outcome
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
#'   real = list(
#'     "train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
#'       dplyr::pull(outcome),
#'     "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'       dplyr::pull(outcome)
#'   )
#' )
#' 
#' \dontrun{
#' 
#' create_precision_recall_curve(
#'   probs = example_dat$estimated_probabilities,
#'   real = example_dat$outcome,
#'   interactive = TRUE
#' )
#' 
#' create_precision_recall_curve(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   real = example_dat$outcome,
#'   interactive = TRUE 
#'   )
#' 
#' create_precision_recall_curve(
#'   probs = list(
#'     "train" = example_dat %>%
#'       dplyr::filter(type_of_set == "train") %>%
#'       dplyr::pull(estimated_probabilities),
#'     "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'       dplyr::pull(estimated_probabilities)
#'   ),
#'   real = list(
#'     "train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
#'       dplyr::pull(outcome),
#'     "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'       dplyr::pull(outcome)
#'   ),
#'   interactive = TRUE   
#' )
#' }
create_precision_recall_curve <- function(probs, real, by = 0.01,
                             stratified_by = "probability_threshold",
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
  prepare_performance_data(
    probs = probs,
    real = real,
    by = by,
    stratified_by = stratified_by
  ) %>%
    plot_precision_recall_curve(chosen_threshold = chosen_threshold,
                   interactive = interactive,
                   main_slider = main_slider,
                   col_values = col_values)
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
#'   plot_precision_recall_curve(interactive = TRUE)
#'
#' one_pop_one_model_as_a_vector_enforced_percentiles_symmetry %>%
#'   plot_precision_recall_curve(interactive = TRUE, main_slider = "ppcr")
#'
#' one_pop_one_model_as_a_list %>%
#'   plot_precision_recall_curve(interactive = TRUE)
#'
#' one_pop_one_model_as_a_list_enforced_percentiles_symmetry %>%
#'   plot_precision_recall_curve(interactive = TRUE, main_slider = "ppcr")
#'
#' one_pop_three_models %>%
#'   plot_precision_recall_curve(interactive = TRUE)
#'
#' one_pop_three_models_enforced_percentiles_symmetry %>%
#'   plot_precision_recall_curve(interactive = TRUE, main_slider = "ppcr")
#'
#' train_and_test_sets %>%
#'   plot_precision_recall_curve(interactive = TRUE)
#'
#' train_and_test_sets_enforced_percentiles_symmetry %>%
#'   plot_precision_recall_curve(interactive = TRUE, main_slider = "ppcr")
#' }
#'
#' @export

plot_precision_recall_curve <- function(performance_data,
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
  perf_dat_type <- check_performance_data_type_for_plotly(performance_data)
  prevalence <- get_prevalence_from_performance_data(performance_data, perf_dat_type)
  
  if (interactive == FALSE) {
  
    reference_lines <- create_reference_lines_data_frame("precision recall", prevalence)
  
    precision_recall_curve <- performance_data %>%
      create_ggplot_for_performance_metrics("sensitivity", "PPV", col_values) %>%
      add_reference_lines_to_ggplot(reference_lines) %>%
      set_precision_recall_curve_limits() +
      ggplot2::xlab("Sensitivity") +
      ggplot2::ylab("PPV")
  }
  
  if (interactive == TRUE) {
    
    performance_data$fake_PPV <- performance_data$PPV
    performance_data$fake_PPV[is.nan(performance_data$PPV)] <- -1  

    performance_data <- performance_data %>% 
      add_hover_text_to_performance_data(perf_dat_type, curve = "precision recall")

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
          fake_PPV, 
          main_slider
        ) %>%
        set_styling_for_rtichoke("precision recall")
    }
    
    if (perf_dat_type == "several models") {
      
      precision_recall_curve <- create_reference_lines_for_plotly(perf_dat_type, 
                                                                  "precision recall",
                                                                  prevalence = prevalence[1], 
                                                                  population_color_vector = 
                                                                    col_values[1:length(prevalence)]) %>% 
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
          fake_PPV, 
          main_slider = main_slider
        )  %>%
        set_styling_for_rtichoke("precision recall")
      
    }
    
    if (perf_dat_type == "several populations") {
      
      precision_recall_curve <- create_reference_lines_for_plotly(perf_dat_type, 
                                                                  "precision recall",
                                                                  prevalence = prevalence,
                                                     population_color_vector = col_values[1:length(prevalence)]) %>% 
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
          fake_PPV, 
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
#' @keywords internal
set_precision_recall_curve_limits <- function(precision_recall_curve) {
  precision_recall_curve +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1)
}


