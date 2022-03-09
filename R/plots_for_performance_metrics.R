#' Create ggplot for Performance Metrics
#'
#' Makes a ggplot for the metrices
#'
#' @param performance_data an rtichoke Performance Data
#' @param x_perf_metric a performance metrice for the x axis
#' @param y_perf_metric a performance metrice for the y axis
#' @param col_values color palette
#'
#' @keywords internal
create_ggplot_for_performance_metrics <- function(performance_data,
                                                  x_perf_metric,
                                                  y_perf_metric,
                                                  col_values = c(
                                                    "#5BC0BE",
                                                    "#FC8D62",
                                                    "#8DA0CB",
                                                    "#E78AC3",
                                                    "#A4243B"
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
    col_values_vec <- col_values[
      seq_len(length(unique(performance_data %>% 
                              dplyr::pull(1))))]

    if (length(unique(performance_data %>% dplyr::pull(1))) == 1) {
      col_values_vec <- "black"
    }

    if (length(unique(performance_data %>% dplyr::pull(1))) > 1) {
      names(col_values_vec) <- unique(performance_data %>% dplyr::pull(1))
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
    ggplot2::geom_path(size = 1) +
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
#' @param main_slider what is the main slider - threshold, 
#' percent positives or positives
#' @param reference_lines a list of reference lines
#' @keywords internal

create_plotly_for_performance_metrics <- function(performance_data,
                                                  x_perf_metric,
                                                  y_perf_metric,
                                                  col_values = c(
                                                    "#5BC0BE",
                                                    "#FC8D62",
                                                    "#8DA0CB",
                                                    "#E78AC3",
                                                    "#A4243B"
                                                  ),
                                                  main_slider = "threshold",
                                                  reference_lines = NA) {
  x_perf_metric <- enquo(x_perf_metric)
  y_perf_metric <- enquo(y_perf_metric)


  performance_data_type <- check_performance_data_type_for_plotly(
    performance_data)

  if (performance_data_type %in% c("one model", 
                                   "one model with model column")) {
    col_values_vec <- "black"
  } else {
    col_values_vec <- col_values[seq_len(length(unique(performance_data %>% 
                                                         dplyr::pull(1))))]
    names(col_values_vec) <- unique(performance_data %>% dplyr::pull(1))
  }

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
    add_markers_and_lines_to_plotly(
      performance_data_type = performance_data_type) %>%
    add_interactive_marker_to_plotly() %>%
    remove_grid_lines_from_plotly()

  plotly_for_performance_metrics
}




#' remove_grid_lines_from_plotly
#'
#' @param plotly_object a plotly plot for performance metrics
#' @keywords internal
remove_grid_lines_from_plotly <- function(plotly_object) {
  plotly_object %>%
    plotly::layout(
      xaxis = list(showgrid = FALSE),
      yaxis = list(showgrid = FALSE)
    )
}
