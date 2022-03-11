# ROC Curve ---------------------------------------------------------------


#' ROC Curve
#'
#' Create a ROC Curve
#'
#' @inheritParams prepare_performance_data
#' @param col_values color palette
#' @param chosen_threshold a chosen threshold to display (for non-interactive)
#' @param interactive whether the plot should be interactive
#' plots
#' @param main_slider what is the main slider - threshold, percent positives 
#' or positives
#' @param col_values color palette
#' @param title_included add title to the curve
#' @param size the size of the curve
#'
#'
#' @export
#'
#' @examples
#'
#' create_roc_curve(
#'   probs = example_dat$estimated_probabilities,
#'   real = example_dat$outcome
#' )
#'
#' create_roc_curve(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   real = example_dat$outcome
#' )
#'
#' create_roc_curve(
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
#' \dontrun{
#'
#' create_roc_curve(
#'   probs = example_dat$estimated_probabilities,
#'   real = example_dat$outcome,
#'   interactive = TRUE
#' )
#'
#' create_roc_curve(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   real = example_dat$outcome,
#'   interactive = TRUE
#' )
#'
#' create_roc_curve(
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
create_roc_curve <- function(probs, real, by = 0.01,
                             stratified_by = "probability_threshold",
                             chosen_threshold = NA,
                             interactive = FALSE,
                             main_slider = "threshold",
                             col_values = c(
                               "#5BC0BE",
                               "#FC8D62",
                               "#8DA0CB",
                               "#E78AC3",
                               "#A4243B"
                             ),
                             title_included = FALSE,
                             size = NULL) {
  
  check_probs_input(probs)
  check_real_input(real)
  
  if(!is.na(chosen_threshold)) {
    check_chosen_threshold_input(chosen_threshold)
  }
  
  prepare_performance_data(
    probs = probs,
    real = real,
    by = by,
    stratified_by = stratified_by
  ) %>%
    plot_roc_curve(
      chosen_threshold = chosen_threshold,
      interactive = interactive,
      main_slider = main_slider,
      col_values = col_values,
      title_included = FALSE,
      size = size
    )
}


#' ROC Curve from Performance Data
#'
#' Plot a ROC Curve
#'
#' @inheritParams create_roc_curve
#' @param performance_data an rtichoke Performance Data
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
#'   plot_roc_curve(interactive = TRUE, main_slider = "ppcr")
#'
#' one_pop_one_model_as_a_list %>%
#'   plot_roc_curve(interactive = TRUE)
#'
#' one_pop_one_model_as_a_list_enforced_percentiles_symmetry %>%
#'   plot_roc_curve(interactive = TRUE, main_slider = "ppcr")
#'
#' one_pop_three_models %>%
#'   plot_roc_curve(interactive = TRUE)
#'
#' one_pop_three_models_enforced_percentiles_symmetry %>%
#'   plot_roc_curve(interactive = TRUE, main_slider = "ppcr")
#'
#' train_and_test_sets %>%
#'   plot_roc_curve(interactive = TRUE)
#'
#' train_and_test_sets_enforced_percentiles_symmetry %>%
#'   plot_roc_curve(interactive = TRUE, main_slider = "ppcr")
#' }
#'
#' @export
plot_roc_curve <- function(performance_data,
                           chosen_threshold = NA,
                           interactive = FALSE,
                           main_slider = "threshold",
                           col_values = c(
                             "#5BC0BE",
                             "#FC8D62",
                             "#8DA0CB",
                             "#E78AC3",
                             "#A4243B"
                           ),
                           title_included = FALSE,
                           size = NULL) {
  
  if(!is.na(chosen_threshold)) {
  check_chosen_threshold_input(chosen_threshold)
  }
    
    
  performance_data_stratification <- check_performance_data_stratification(
      performance_data
      )
  
  if (((performance_data_stratification == "ppcr") &
       (main_slider != "ppcr")) | 
      ((performance_data_stratification != "ppcr") &
       (main_slider == "ppcr"))
  )
      { stop("Performance data and Main Slider are not consistent") }
  
  
  perf_dat_type <- 
    check_performance_data_type_for_plotly(performance_data = performance_data)
  prevalence <- 
    get_prevalence_from_performance_data(performance_data, perf_dat_type)

  if (interactive == FALSE) {
    reference_lines <- create_reference_lines_data_frame("roc")

    roc_curve <- performance_data %>%
      create_ggplot_for_performance_metrics("FPR", 
                                            "sensitivity", col_values) %>%
      add_reference_lines_to_ggplot(reference_lines) +
      ggplot2::xlab("1 - Specificity") +
      ggplot2::ylab("Sensitivity")
  }

  if (interactive == TRUE) {
    perf_dat_type <- check_performance_data_type_for_plotly(performance_data)

    performance_data <- performance_data %>%
      add_hover_text_to_performance_data(perf_dat_type, curve = "roc")


    if (perf_dat_type %in% c("one model with model column", "one model")) {
      roc_curve <- create_reference_lines_for_plotly(perf_dat_type, "roc",
        size = size
      ) %>%
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
      performance_data <- performance_data %>%
        mutate(model = forcats::fct_inorder(factor(model)))

      roc_curve <- create_reference_lines_for_plotly(perf_dat_type,
        "roc",
        population_color_vector =
          col_values[seq_len(length(prevalence))],
        size = size
      ) %>%
        add_lines_and_markers_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          FPR,
          sensitivity,
          col_values = col_values,
          main_slider = main_slider
        ) %>%
        add_interactive_marker_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          FPR,
          sensitivity,
          main_slider = main_slider
        ) %>%
        set_styling_for_rtichoke("roc")
    }

    if (perf_dat_type == "several populations") {
      performance_data <- performance_data %>%
        mutate(population = forcats::fct_inorder(factor(population)))

      roc_curve <- create_reference_lines_for_plotly(perf_dat_type,
        "roc",
        population_color_vector =
          col_values[seq_len(length(prevalence))],
        size = size
      ) %>%
        add_lines_and_markers_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          FPR,
          sensitivity,
          main_slider = main_slider
        ) %>%
        add_interactive_marker_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          FPR,
          sensitivity,
          main_slider = main_slider
        ) %>%
        set_styling_for_rtichoke("roc")
    }
  }

  return(roc_curve)
}
