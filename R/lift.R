# Lift --------------------------------------------------

#' Lift Curve
#'
#' Create a Lift Curve
#'
#' @inheritParams create_roc_curve
#'
#' @export
#'
#' @examples
#'
#' create_lift_curve(
#'   probs = example_dat$estimated_probabilities,
#'   real = example_dat$outcome
#' )
#'
#' create_lift_curve(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   real = example_dat$outcome
#' )
#'
#' create_lift_curve(
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
#' create_lift_curve(
#'   probs = example_dat$estimated_probabilities,
#'   real = example_dat$outcome,
#'   interactive = TRUE
#' )
#'
#' create_lift_curve(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   real = example_dat$outcome,
#'   interactive = TRUE
#' )
#'
#' create_lift_curve(
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
create_lift_curve <- function(probs, real, by = 0.01,
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
                              size = NULL) {
  if (!is.na(chosen_threshold)) {
    check_chosen_threshold_input(chosen_threshold)
  }

  prepare_performance_data(
    probs = probs,
    real = real,
    by = by,
    stratified_by = stratified_by
  ) %>%
    plot_lift_curve(
      chosen_threshold = chosen_threshold,
      interactive = interactive,
      main_slider = main_slider,
      col_values = col_values,
      size = size
    )
}


#' LIFT Curve from Performance Data
#'
#' Plot a LIFT Curve
#'
#' @inheritParams plot_roc_curve
#'
#' @examples
#'
#' one_pop_one_model_as_a_vector %>%
#'   plot_lift_curve()
#'
#' one_pop_one_model_as_a_vector_enforced_percentiles_symmetry %>%
#'   plot_lift_curve(main_slider = "ppcr")
#'
#' one_pop_one_model_as_a_list %>%
#'   plot_lift_curve()
#'
#' one_pop_one_model_as_a_list_enforced_percentiles_symmetry %>%
#'   plot_lift_curve(main_slider = "ppcr")
#'
#' one_pop_three_models %>%
#'   plot_lift_curve()
#'
#' one_pop_three_models_enforced_percentiles_symmetry %>%
#'   plot_lift_curve(main_slider = "ppcr")
#'
#' train_and_test_sets %>%
#'   plot_lift_curve()
#'
#' train_and_test_sets_enforced_percentiles_symmetry %>%
#'   plot_lift_curve(main_slider = "ppcr")
#' \dontrun{
#'
#' one_pop_one_model_as_a_vector %>%
#'   plot_lift_curve(interactive = TRUE)
#'
#' one_pop_one_model_as_a_vector_enforced_percentiles_symmetry %>%
#'   plot_lift_curve(interactive = TRUE, main_slider = "ppcr")
#'
#' one_pop_one_model_as_a_list %>%
#'   plot_lift_curve(interactive = TRUE)
#'
#' one_pop_one_model_as_a_list_enforced_percentiles_symmetry %>%
#'   plot_lift_curve(interactive = TRUE, main_slider = "ppcr")
#'
#' one_pop_three_models %>%
#'   plot_lift_curve(interactive = TRUE)
#'
#' one_pop_three_models_enforced_percentiles_symmetry %>%
#'   plot_lift_curve(interactive = TRUE, main_slider = "ppcr")
#'
#' train_and_test_sets %>%
#'   plot_lift_curve(interactive = TRUE)
#'
#' train_and_test_sets_enforced_percentiles_symmetry %>%
#'   plot_lift_curve(interactive = TRUE, main_slider = "ppcr")
#' }
#'
#' @export
plot_lift_curve <- function(performance_data,
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
    performance_data =
      performance_data
  )
  prevalence <- get_prevalence_from_performance_data(
    performance_data,
    perf_dat_type
  )

  if (interactive == FALSE) {
    reference_lines <- create_reference_lines_data_frame("lift")

    lift_curve <- performance_data %>%
      create_ggplot_for_performance_metrics("ppcr", "lift", col_values) %>%
      add_reference_lines_to_ggplot(reference_lines) %>%
      set_lift_curve_limits() +
      ggplot2::xlab("Predicted Positives (Rate)") +
      ggplot2::ylab("Lift")
  }

  if (interactive == TRUE) {
    performance_data$fake_lift <- performance_data$lift
    performance_data$fake_lift[is.nan(performance_data$lift)] <- -1

    perf_dat_type <- check_performance_data_type_for_plotly(performance_data)

    performance_data <- performance_data %>%
      add_hover_text_to_performance_data(perf_dat_type, curve = "lift")

    if (perf_dat_type %in% c("one model with model column", "one model")) {
      lift_curve <- create_reference_lines_for_plotly(perf_dat_type, "lift",
        size = size
      ) %>%
        add_lines_and_markers_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          ppcr,
          lift,
          main_slider
        ) %>%
        add_interactive_marker_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          ppcr,
          fake_lift,
          main_slider
        ) %>%
        set_styling_for_rtichoke("lift",
          max_y_range = max(performance_data$lift,
            na.rm = TRUE
          ) + 0.1
        )
    }

    if (perf_dat_type == "several models") {
      lift_curve <- create_reference_lines_for_plotly(perf_dat_type,
        "lift",
        population_color_vector =
          col_values[seq_len(length(prevalence))],
        size = size
      ) %>%
        add_lines_and_markers_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          ppcr,
          lift,
          col_values = col_values,
          main_slider = main_slider
        ) %>%
        add_interactive_marker_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          ppcr,
          fake_lift,
          main_slider = main_slider
        ) %>%
        set_styling_for_rtichoke("lift",
          max_y_range = max(performance_data$lift,
            na.rm = TRUE
          ) + 0.1
        )
    }

    if (perf_dat_type == "several populations") {
      lift_curve <- create_reference_lines_for_plotly(perf_dat_type,
        "lift",
        population_color_vector =
          col_values[seq_len(length(prevalence))],
        size = size
      ) %>%
        add_lines_and_markers_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          ppcr,
          lift,
          main_slider = main_slider
        ) %>%
        add_interactive_marker_from_performance_data(
          performance_data = performance_data,
          performance_data_type = perf_dat_type,
          ppcr,
          fake_lift,
          main_slider = main_slider
        ) %>%
        set_styling_for_rtichoke("lift",
          max_y_range = max(performance_data$lift,
            na.rm = TRUE
          ) + 0.1
        )
    }
  }

  return(lift_curve)
}


#' Set the limits for lift curve
#'
#' @param lift_curve a ggplot object of lift curve
#' @keywords internal
set_lift_curve_limits <- function(lift_curve) {
  lift_curve +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, NA)
}
