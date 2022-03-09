# Gains --------------------------------------------------

#' Gains Curve
#'
#' Create a Gains Curve
#'
#' @inheritParams create_roc_curve
#'
#' @export
#'
#' @examples
#'
#' create_gains_curve(
#'   probs = example_dat$estimated_probabilities,
#'   real = example_dat$outcome
#' )
#'
#' create_gains_curve(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   real = example_dat$outcome
#' )
#'
#' create_gains_curve(
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
#' create_gains_curve(
#'   probs = example_dat$estimated_probabilities,
#'   real = example_dat$outcome,
#'   interactive = TRUE
#' )
#'
#' create_gains_curve(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   real = example_dat$outcome,
#'   interactive = TRUE
#' )
#'
#' create_gains_curve(
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
create_gains_curve <- function(probs, real, by = 0.01,
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
                               ),
                               size = NULL) {
  prepare_performance_data(
    probs = probs,
    real = real,
    by = by,
    stratified_by = stratified_by
  ) %>%
    plot_gains_curve(
      chosen_threshold = chosen_threshold,
      interactive = interactive,
      main_slider = main_slider,
      col_values = col_values,
      size = size
    )
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
#'   plot_gains_curve(interactive = TRUE)
#'
#' one_pop_one_model_as_a_vector_enforced_percentiles_symmetry %>%
#'   plot_gains_curve(interactive = TRUE, main_slider = "ppcr")
#'
#' one_pop_one_model_as_a_list %>%
#'   plot_gains_curve(interactive = TRUE)
#'
#' one_pop_one_model_as_a_list_enforced_percentiles_symmetry %>%
#'   plot_gains_curve(interactive = TRUE, main_slider = "ppcr")
#'
#' one_pop_three_models %>%
#'   plot_gains_curve(interactive = TRUE)
#'
#' one_pop_three_models_enforced_percentiles_symmetry %>%
#'   plot_gains_curve(interactive = TRUE, main_slider = "ppcr")
#'
#' train_and_test_sets %>%
#'   plot_gains_curve(interactive = TRUE)
#'
#' train_and_test_sets_enforced_percentiles_symmetry %>%
#'   plot_gains_curve(interactive = TRUE, main_slider = "ppcr")
#' }
#'
#' @export

plot_gains_curve <- function(performance_data,
                             chosen_threshold = NA,
                             interactive = F,
                             main_slider = "threshold",
                             col_values = c(
                               "#5BC0BE",
                               "#FC8D62",
                               "#8DA0CB",
                               "#E78AC3",
                               "#A4243B"
                             ),
                             size = NULL) {
  perf_dat_type <- check_performance_data_type_for_plotly(performance_data = performance_data)
  prevalence <- get_prevalence_from_performance_data(performance_data, perf_dat_type)

  if (interactive == FALSE) {
    reference_lines <- create_reference_lines_data_frame("gains", prevalence)

    gains_curve <- performance_data %>%
      create_ggplot_for_performance_metrics("ppcr", "sensitivity", col_values) %>%
      add_reference_lines_to_ggplot(reference_lines) %>%
      set_gains_curve_limits() +
      ggplot2::xlab("Predicted Positives (Rate)") +
      ggplot2::ylab("Sensitivity")
  }

  if (interactive == TRUE) {
    performance_data <- performance_data %>%
      add_hover_text_to_performance_data(perf_dat_type, curve = "gains")

    if (perf_dat_type %in% c("one model with model column", "one model")) {
      gains_curve <- create_reference_lines_for_plotly(perf_dat_type,
        "gains",
        prevalence = prevalence,
        size = size
      ) %>%
        add_lines_and_markers_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          ppcr,
          sensitivity,
          main_slider
        ) %>%
        add_interactive_marker_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          ppcr,
          sensitivity,
          main_slider
        ) %>%
        set_styling_for_rtichoke("gains")
    }

    if (perf_dat_type == "several models") {
      gains_curve <- create_reference_lines_for_plotly(perf_dat_type,
        "gains",
        prevalence = prevalence[1],
        population_color_vector =
          col_values[seq_len(length(prevalence))],
        size = size
      ) %>%
        add_lines_and_markers_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          ppcr,
          sensitivity,
          col_values = col_values,
          main_slider = main_slider
        ) %>%
        add_interactive_marker_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          ppcr,
          sensitivity,
          main_slider = main_slider
        ) %>%
        set_styling_for_rtichoke("gains")
    }

    if (perf_dat_type == "several populations") {
      gains_curve <- create_reference_lines_for_plotly(perf_dat_type,
        "gains",
        prevalence = prevalence,
        population_color_vector =
          col_values[seq_len(length(prevalence))],
        size = size
      ) %>%
        add_lines_and_markers_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          ppcr,
          sensitivity,
          main_slider = main_slider
        ) %>%
        add_interactive_marker_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          ppcr,
          sensitivity,
          main_slider = main_slider
        ) %>%
        set_styling_for_rtichoke("gains")
    }
  }

  return(gains_curve)
}

#' Add reference lines to Gains Curve
#'
#' @param gains_curve a ggplot object of a Gains Curve
#' @param prevalence the prevalence of the outcome
#' @keywords internal
add_gains_curve_reference_lines <- function(gains_curve, prevalence) {
  gains_curve$layers <- c(
    ggplot2::geom_segment(x = 0, y = 0, xend = 1, yend = 1, color = "grey"),
    purrr::map2(
      prevalence, c(
        "#5BC0BE",
        "#FC8D62",
        "#8DA0CB",
        "#E78AC3",
        "#A4243B"
      )[seq_len(length(prevalence))],
      add_prevalence_layers_to_gains_curve
    ) %>% unlist(),
    gains_curve$layers
  )
  gains_curve
}


#' Set the limits for Gains Curve
#'
#' @param gains_curve a ggplot object of a Gains Curve
#' @keywords internal
set_gains_curve_limits <- function(gains_curve) {
  gains_curve +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1)
}
