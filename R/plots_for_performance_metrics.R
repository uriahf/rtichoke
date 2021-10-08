#' Create ggplot for Performance Metrics
#'
#' Makes a ggplot for the metrices
#'
#' @param performance_data an rtichoke Performance Data
#' @param x_perf_metric a performance metrice for the x axis
#' @param y_perf_metric a performance metrice for the y axis
#' @param col_values color palette
#'


create_ggplot_for_performance_metrics <- function(performance_data,
                                                  x_perf_metric,
                                                  y_perf_metric,
                                                  col_values = c(
                                                    "#5E7F9A",
                                                    "#931B53",
                                                    "#F7DC2E",
                                                    "#C6C174",
                                                    "#75DBCD"
                                                  )) {
  if (!(names(performance_data)[1] %in% c("population", "model"))) {
    col_values_vec <- "black"

    ggplot_for_performance_metrics <- ggplot2::ggplot(
      performance_data,
      ggplot2::aes_string(
        x = x_perf_metric,
        y = y_perf_metric
      )
    )
  } else {
    col_values_vec <- col_values[1:length(unique(performance_data[, 1]))]

    if (length(unique(performance_data[, 1])) == 1) {
      col_values_vec <- "black"
    }

    if (length(unique(performance_data[, 1])) > 1) {
      names(col_values_vec) <- unique(performance_data[, 1])
    }

    ggplot_for_performance_metrics <- ggplot2::ggplot(
      performance_data,
      ggplot2::aes_string(
        x = x_perf_metric,
        y = y_perf_metric,
        group = names(performance_data)[1],
        color = names(performance_data)[1]
      )
    )
  }

  ggplot_for_performance_metrics +
    ggplot2::geom_point(size = 1) +
    ggplot2::geom_path(size = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::scale_color_manual(values = col_values_vec)
}

#' Create plotly for Performance Metrics
#'
#' Makes a plotly for the Performance Metrics
#'
#'
#'
#' @inheritParams create_ggplot_for_performance_metrics
#' @param main_slider what is the main slider - threshold, percent positives or positives
#' @param reference_lines a list of reference lines

create_plotly_for_performance_metrics <- function(performance_data,
                                                  x_perf_metric,
                                                  y_perf_metric,
                                                  col_values = c(
                                                    "#5E7F9A",
                                                    "#931B53",
                                                    "#F7DC2E",
                                                    "#C6C174",
                                                    "#75DBCD"
                                                  ),
                                                  main_slider = "threshold",
                                                  reference_lines = NA) {
  x_perf_metric <- enquo(x_perf_metric)
  y_perf_metric <- enquo(y_perf_metric)


  performance_data_type <- check_performance_data_type_for_plotly(performance_data)

  if (performance_data_type %in% c("one model", "one model with model column")) {
    col_values_vec <- "black"
  } else {
    col_values_vec <- col_values[1:length(unique(performance_data[, 1]))]
    names(col_values_vec) <- unique(performance_data[, 1])
  }

  print(performance_data_type)
  print(col_values_vec)

  plotly_for_performance_metrics <- performance_data %>%
    create_plotly_base(x_perf_metric,
      y_perf_metric,
      performance_data_type = performance_data_type,
      col_values = col_values_vec
    )

  if (is.data.frame(reference_lines)) {
    plotly_for_performance_metrics <- plotly_for_performance_metrics %>%
      add_reference_lines_to_plotly(reference_lines, performance_data_type)
  }


  plotly_for_performance_metrics <- plotly_for_performance_metrics %>%
    add_markers_and_lines_to_plotly(performance_data_type = performance_data_type) %>%
    add_interactive_marker_to_plotly() %>%
    remove_grid_lines_from_plotly()

  plotly_for_performance_metrics
}




#' remove_grid_lines_from_plotly
#'
#' @param plotly_object a plotly plot for performance metrics
remove_grid_lines_from_plotly <- function(plotly_object) {
  plotly_object %>%
    plotly::layout(
      xaxis = list(showgrid = F),
      yaxis = list(showgrid = F)
    )
}


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
                             main_slider = "threshold") {
  prevalence <- get_prevalence_from_performance_data(performance_data)
  reference_lines <- create_reference_lines_data_frame("gains", prevalence)

  if (interactive == F) {
    gains_curve <- performance_data %>%
      create_ggplot_for_performance_metrics("predicted_positives_percent", "sensitivity") %>%
      add_reference_lines_to_ggplot(reference_lines) %>%
      set_gains_curve_limits()
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

# Decision Curve ----------------------------------------------------------



#' Decision Curve
#'
#' Create a Decision Curve
#'
#' @inheritParams create_roc_curve
#'
#' @export
#'
create_decision_curve <- function(probs, real, by = 0.01,
                                  enforce_percentiles_symmetry = F) {
  prepare_performance_data(
    probs = probs,
    real = real,
    by = by,
    enforce_percentiles_symmetry = enforce_percentiles_symmetry
  ) %>%
    plot_decision_curve()
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
#'   plot_decision_curve()
#'
#' one_pop_one_model_as_a_vector_enforced_percentiles_symmetry %>%
#'   plot_decision_curve()
#'
#' one_pop_one_model_as_a_list %>%
#'   plot_decision_curve()
#'
#' one_pop_one_model_as_a_list_enforced_percentiles_symmetry %>%
#'   plot_decision_curve()
#'
#' one_pop_three_models %>%
#'   plot_decision_curve()
#'
#' one_pop_three_models_enforced_percentiles_symmetry %>%
#'   plot_decision_curve()
#'
#' train_and_test_sets %>%
#'   plot_decision_curve()
#'
#' train_and_test_sets_enforced_percentiles_symmetry %>%
#'   plot_decision_curve()
#' \dontrun{
#'
#' one_pop_one_model_as_a_vector %>%
#'   plot_decision_curve(interactive = T)
#'
#' one_pop_one_model_as_a_vector_enforced_percentiles_symmetry %>%
#'   plot_decision_curve(interactive = T, main_slider = "predicted_positives_percent")
#'
#' one_pop_one_model_as_a_list %>%
#'   plot_decision_curve(interactive = T)
#'
#' one_pop_one_model_as_a_list_enforced_percentiles_symmetry %>%
#'   plot_decision_curve(interactive = T, main_slider = "predicted_positives_percent")
#'
#' one_pop_three_models %>%
#'   plot_decision_curve(interactive = T)
#'
#' one_pop_three_models_enforced_percentiles_symmetry %>%
#'   plot_decision_curve(interactive = T, main_slider = "predicted_positives_percent")
#'
#' train_and_test_sets %>%
#'   plot_decision_curve(interactive = T)
#'
#' train_and_test_sets_enforced_percentiles_symmetry %>%
#'   plot_decision_curve(interactive = T, main_slider = "predicted_positives_percent")
#' }
#' @export

plot_decision_curve <- function(performance_data,
                                chosen_threshold = NA,
                                interactive = F,
                                main_slider = "threshold") {
  prevalence <- get_prevalence_from_performance_data(performance_data)

  if (interactive == F) {
    decision_curve <- performance_data %>%
      create_ggplot_for_performance_metrics("threshold", "NB") %>%
      add_reference_lines_to_ggplot(create_reference_lines_data_frame("decision", prevalence)) %>%
      set_decision_curve_limits()
  }
  return(decision_curve)
}


#' Set the limits for Decision Curve
#'
#' @param decision_curve a ggplot object of Decision Curve
#'
set_decision_curve_limits <- function(decision_curve) {
  decision_curve +
    ggplot2::xlim(0, 1)
}
