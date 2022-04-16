# Decision Curve ----------------------------------------------------------



#' Decision Curve
#'
#' Create a decision Curve
#'
#' @inheritParams create_roc_curve
#'
#' @export
#'
#' @examples
#' 
#' \dontrun{
#' create_decision_curve(
#'   probs = list(example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome)
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
#' }
create_decision_curve <- function(probs, reals, by = 0.01,
                                  stratified_by = "probability_threshold",
                                  chosen_threshold = NA,
                                  interactive = TRUE,
                                  main_slider = "threshold",
                                  col_values = c(
                                    "#5BC0BE",
                                    "#FC8D62",
                                    "#8DA0CB",
                                    "#E78AC3",
                                    "#A4243B"
                                  ),
                                  size = NULL) {
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
      main_slider = main_slider,
      col_values = col_values,
      size = size
    )
}



#' Decision Curve from Performance Data
#'
#' Plot a Precision decision Curve
#'
#' @inheritParams plot_roc_curve
#'
#' @examples
#'
#' one_pop_one_model_as_a_vector %>%
#'   plot_decision_curve()
#'
#' one_pop_one_model_as_a_vector_enforced_percentiles_symmetry %>%
#'   plot_decision_curve(main_slider = "ppcr")
#'
#' one_pop_one_model_as_a_list %>%
#'   plot_decision_curve()
#'
#' one_pop_one_model_as_a_list_enforced_percentiles_symmetry %>%
#'   plot_decision_curve(main_slider = "ppcr")
#'
#' one_pop_three_models %>%
#'   plot_decision_curve()
#'
#' one_pop_three_models_enforced_percentiles_symmetry %>%
#'   plot_decision_curve(main_slider = "ppcr")
#'
#' train_and_test_sets %>%
#'   plot_decision_curve()
#'
#' train_and_test_sets_enforced_percentiles_symmetry %>%
#'   plot_decision_curve(main_slider = "ppcr")
#' \dontrun{
#'
#' one_pop_one_model_as_a_vector %>%
#'   plot_decision_curve(interactive = TRUE)
#'
#' one_pop_one_model_as_a_vector_enforced_percentiles_symmetry %>%
#'   plot_decision_curve(interactive = TRUE, main_slider = "ppcr")
#'
#' one_pop_one_model_as_a_list %>%
#'   plot_decision_curve(interactive = TRUE)
#'
#' one_pop_one_model_as_a_list_enforced_percentiles_symmetry %>%
#'   plot_decision_curve(interactive = TRUE, main_slider = "ppcr")
#'
#' one_pop_three_models %>%
#'   plot_decision_curve(interactive = TRUE)
#'
#' one_pop_three_models_enforced_percentiles_symmetry %>%
#'   plot_decision_curve(interactive = TRUE, main_slider = "ppcr")
#'
#' train_and_test_sets %>%
#'   plot_decision_curve(interactive = TRUE)
#'
#' train_and_test_sets_enforced_percentiles_symmetry %>%
#'   plot_decision_curve(interactive = TRUE, main_slider = "ppcr")
#' }
#' @export

plot_decision_curve <- function(performance_data,
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
                                size = NULL) {
  if (!is.na(chosen_threshold)) {
    check_chosen_threshold_input(chosen_threshold)
  }

  performance_data_stratification <- check_performance_data_stratification(
    performance_data
  )

  if (((performance_data_stratification == "ppcr") &
    (main_slider != "ppcr")) |
    ((performance_data_stratification != "ppcr") &
      (main_slider == "ppcr"))
  ) {
    stop("Performance data and Main Slider are not consistent")
  }

  perf_dat_type <- check_performance_data_type_for_plotly(
    performance_data = performance_data
  )
  prevalence <- get_prevalence_from_performance_data(
    performance_data, perf_dat_type
  )

  if (interactive == FALSE) {
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
    performance_data <- performance_data %>%
      add_hover_text_to_performance_data(perf_dat_type, curve = "decision")

    if (perf_dat_type %in% c("one model with model column", "one model")) {
      decision_curve <- create_reference_lines_for_plotly(
        perf_dat_type,
        "decision",
        prevalence = prevalence,
        size = size
      ) %>%
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
      decision_curve <- create_reference_lines_for_plotly(
        perf_dat_type,
        "decision",
        prevalence = prevalence[1],
        population_color_vector = col_values[seq_len(length(prevalence))],
        size = size
      ) %>%
        add_lines_and_markers_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          threshold,
          NB,
          col_values = col_values,
          main_slider = main_slider
        ) %>%
        add_interactive_marker_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          threshold,
          NB,
          main_slider = main_slider
        ) %>%
        set_styling_for_rtichoke("decision")
    }

    if (perf_dat_type == "several populations") {
      decision_curve <- create_reference_lines_for_plotly(perf_dat_type,
        "decision",
        prevalence = prevalence,
        population_color_vector =
          col_values[seq_len(length(prevalence))],
        size = size
      ) %>%
        add_lines_and_markers_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          threshold,
          NB,
          main_slider = main_slider
        ) %>%
        add_interactive_marker_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          threshold,
          NB,
          main_slider = main_slider
        ) %>%
        set_styling_for_rtichoke("decision")
    }
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
