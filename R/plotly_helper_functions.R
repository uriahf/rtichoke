#' Check Performance type for plotly
#'
#' @param performance_data an rtichoke Performance Data
#' @keywords internal

check_performance_data_type_for_plotly <- function(performance_data) {
  if (!(names(performance_data)[1] %in% c("population", "model"))) {
    performance_data_type <- "one model"
  }
  if ((names(performance_data)[1] == "model") & (
    length(unique(performance_data %>% pull(1))) == 1)) {
    performance_data_type <- "one model with model column"
  }
  if ((names(performance_data)[1] == "model") & (
    length(unique(performance_data %>% pull(1))) > 1)) {
    performance_data_type <- "several models"
  }
  if (names(performance_data)[1] == "population") {
    performance_data_type <- "several populations"
  }
  performance_data_type
}


#' Adding markers and lines to plotly
#'
#' @param plotly_object a plotly object
#' @param performance_data_type the type of the performance data
#'
#' @keywords internal
add_markers_and_lines_to_plotly <- function(plotly_object,
                                            performance_data_type) {
  if (performance_data_type %in%
    c("one model", "one model with model column")) {
    plotly_with_markers_and_lines <- plotly_object %>%
      plotly::add_trace(
        hoverinfo = "text",
        text = ~ paste(
          "TPR (Sensitivity):", round(sensitivity, digits = 3), "<br>",
          "FPR:", round(FPR, digits = 3), "<br>",
          "Specificity", round(specificity, digits = 3), "<br>",
          "Lift", round(lift, digits = 3), "<br>",
          "PPV", round(PPV, digits = 3), "<br>",
          "NPV", round(NPV, digits = 3), "<br>",
          "TP:", TP, "<br>",
          "TN:", TN, "<br>",
          "FP:", FP, "<br>",
          "FN:", FN
        ),
        type = "scatter",
        mode = "markers+lines",
        color = I("black")
      )
  }

  if (performance_data_type == "several models") {
    plotly_with_markers_and_lines <- plotly_object %>%
      plotly::add_trace(
        hoverinfo = "text",
        text = ~ paste(
          "Model:", model, "<br>",
          "TPR (Sensitivity):", round(sensitivity, digits = 3), "<br>",
          "FPR:", round(FPR, digits = 3), "<br>",
          "Specificity", round(specificity, digits = 3), "<br>",
          "LIFT", round(lift, digits = 3), "<br>",
          "PPV", round(PPV, digits = 3), "<br>",
          "NPV", round(NPV, digits = 3), "<br>",
          "TP:", TP, "<br>",
          "TN:", TN, "<br>",
          "FP:", FP, "<br>",
          "FN:", FN
        ),
        type = "scatter",
        mode = "markers+lines"
      )
  }

  if (performance_data_type == "several populations") {
    plotly_with_markers_and_lines <- plotly_object %>%
      plotly::add_trace(
        hoverinfo = "text",
        text = ~ paste(
          "Population:", population, "<br>",
          "TPR (Sensitivity):", round(sensitivity, digits = 3), "<br>",
          "FPR:", round(FPR, digits = 3), "<br>",
          "Specificity", round(specificity, digits = 3), "<br>",
          "Lift", round(lift, digits = 3), "<br>",
          "PPV", round(PPV, digits = 3), "<br>",
          "NPV", round(NPV, digits = 3), "<br>",
          "TP:", TP, "<br>",
          "TN:", TN, "<br>",
          "FP:", FP, "<br>",
          "FN:", FN
        ),
        type = "scatter",
        mode = "markers+lines"
      )
  }
  plotly_with_markers_and_lines
}



#' Add Reference Lines to Plotly Object
#'
#' @param plotly_object a plotly plot for performance metrics
#' @param performance_data_type the type of the Performance Data
#' @param reference_lines dataframe of reference lines
#' @keywords internal
add_reference_lines_to_plotly <- function(plotly_object,
                                          reference_lines,
                                          performance_data_type = "one model") {
  if (performance_data_type == "several populations") {
    reference_lines %>%
      split(seq_len(nrow(.))) %>%
      purrr::reduce(add_reference_lines_to_plotly,
        .init = plotly_object
      ) %>%
      add_markers() %>%
      add_lines()
  } else {
    plotly_object %>%
      plotly::add_lines(
        x = ~ c(reference_lines$x, reference_lines$xend),
        y = ~ c(reference_lines$y, reference_lines$yend),
        mode = "lines",
        color = I(reference_lines$col)
      )
  }
}




#' Set styling for rtichoke plotly
#'
#' @param plotly_object a plotly object
#' @param curve the required curve
#' @param min_y_range the minimum value of y range (for decision curve)
#' @param max_y_range the maximum value of y range (for lift and decision curve)
#' @param min_x_range the minimum value of x range (for decision curve)
#' @param max_x_range the maximum value of x range (for decision curve)
#' @keywords internal
set_styling_for_rtichoke <- function(plotly_object,
                                     curve,
                                     min_y_range = NA,
                                     max_y_range = NA,
                                     min_x_range = NA,
                                     max_x_range = NA) {
  plotly_object %>%
    remove_grid_lines_from_plotly() %>%
    set_axis_titles(curve,
      max_y_range = max_y_range,
      min_y_range = min_y_range,
      min_x_range = min_x_range,
      max_x_range = max_x_range
    ) %>%
    plotly::config(displayModeBar = FALSE)
}



#' Set Titles for x and y axis in plotly objects
#'
#' @inheritParams set_styling_for_rtichoke
#' @keywords internal
set_axis_titles <- function(plotly_object,
                            curve,
                            max_y_range = NA,
                            min_y_range = NA,
                            min_x_range = NA,
                            max_x_range = NA) {
  if (curve == "roc") {
    plotly_obj <- plotly_object %>%
      plotly::layout(
        xaxis = list(
          title = "1 - Specificity",
          fixedrange = TRUE
        ),
        yaxis = list(
          title = "Sensitivity",
          fixedrange = TRUE
        ),
        showlegend = FALSE
      )
  }

  if (curve == "lift") {
    plotly_obj <- plotly_object %>%
      plotly::layout(
        xaxis = list(
          title = "Predicted Positives (Rate)",
          range = c(-0.1, 1.1),
          fixedrange = TRUE
        ),
        yaxis = list(
          title = "Lift",
          range = c(-0.1, max_y_range),
          fixedrange = TRUE
        ),
        showlegend = FALSE
      )
  }

  if (curve == "precision recall") {
    plotly_obj <- plotly_object %>%
      plotly::layout(
        xaxis = list(
          title = "Sensitivity",
          range = c(-0.1, 1.1),
          fixedrange = TRUE
        ),
        yaxis = list(
          title = "PPV",
          range = c(-0.1, 1.1),
          fixedrange = TRUE
        ),
        showlegend = FALSE
      )
  }

  if (curve == "gains") {
    plotly_obj <- plotly_object %>%
      plotly::layout(
        xaxis = list(
          title = "Predicted Positives (Rate)",
          range = c(-0.1, 1.1),
          fixedrange = TRUE
        ),
        yaxis = list(
          title = "Sensitivity",
          range = c(-0.1, 1.1),
          fixedrange = TRUE
        ),
        showlegend = FALSE
      )
  }

  if (curve == "decision") {
    plotly_obj <- plotly_object %>%
      plotly::layout(
        xaxis = list(
          title = "Probability Threshold",
          range = c(min_x_range, max_x_range),
          fixedrange = TRUE
        ),
        yaxis = list(
          title = "Net Benefit",
          range = c(min_y_range, max_y_range),
          fixedrange = TRUE
        ),
        showlegend = FALSE
      )
  }

  if (curve == "interventions avoided") {
    plotly_obj <- plotly_object %>%
      plotly::layout(
        xaxis = list(
          title = "Probability Threshold",
          range = c(min_x_range, max_x_range),
          fixedrange = TRUE
        ),
        yaxis = list(
          title = "Interventions Avoided (per 100)",
          range = c(min_y_range, max_y_range),
          fixedrange = TRUE
        ),
        showlegend = FALSE
      )
  }

  plotly_obj
}





#' Add interactive marker based on performance data
#'
#' @inheritParams add_lines_and_markers_from_performance_data
#' @inheritParams create_roc_curve
#' @keywords internal
add_interactive_marker_from_performance_data <- function(plotly_object,
                                                         performance_data,
                                                         performance_data_type,
                                                         x_perf_metric,
                                                         y_perf_metric,
                                                         stratified_by = "probability_threshold") {
  x_perf_metric <- enquo(x_perf_metric)
  y_perf_metric <- enquo(y_perf_metric)

  if (performance_data_type %in% c(
    "one model",
    "one model with model column"
  )) {
    plotly_plot <- plotly_object %>%
      plotly::add_markers(
        data = performance_data,
        x = x_perf_metric,
        y = y_perf_metric,
        frame = as.formula(paste0("~", stratified_by)),
        marker = list(
          size = 12,
          line = list(
            width = 3,
            color = I("black")
          ),
          color = "#f6e3be"
        ),
        hoverinfo = "text",
        text = ~text
      )
  }

  if (performance_data_type == "several models") {
    plotly_plot <- plotly_object %>%
      plotly::add_markers(
        data = performance_data,
        x = x_perf_metric,
        y = y_perf_metric,
        frame = as.formula(paste0("~", stratified_by)),
        color = ~model,
        marker = list(
          size = 12,
          line = list(
            width = 3,
            color = I("black")
          )
        ),
        hoverinfo = "text",
        text = ~text
      )
  }

  if (performance_data_type == "several populations") {
    plotly_plot <- plotly_object %>%
      plotly::add_markers(
        data = performance_data,
        x = x_perf_metric,
        y = y_perf_metric,
        frame = as.formula(paste0("~", stratified_by)),
        color = ~population,
        marker = list(
          size = 12,
          line = list(
            width = 3,
            color = I("black")
          )
        ),
        hoverinfo = "text",
        text = ~text
      )
  }

  plotly_plot
}





#' Add lines and markers based on performance data
#'
#' @param plotly_object a previous plotly object
#' @param performance_data the performance data for the plot
#' @param performance_data_type the type of the performance data
#' @param x_perf_metric performance metric for the x axis
#' @param y_perf_metric performance metric for the y axis
#' @param color_values color palette
#' @keywords internal
add_lines_and_markers_from_performance_data <- function(plotly_object,
                                                        performance_data,
                                                        performance_data_type,
                                                        x_perf_metric,
                                                        y_perf_metric,
                                                        color_values = c(
                                                          "#5BC0BE",
                                                          "#FC8D62",
                                                          "#8DA0CB",
                                                          "#E78AC3",
                                                          "#A4243B"
                                                        )) {
  x_perf_metric <- enquo(x_perf_metric)
  y_perf_metric <- enquo(y_perf_metric)

  if (performance_data_type %in% c(
    "one model",
    "one model with model column"
  )) {
    color_values_vec <- "black"
  } else {
    color_values_vec <- color_values[
      seq_len(
        length(unique(performance_data %>%
          pull(1)))
      )
    ]
    names(color_values_vec) <- unique(performance_data %>% pull(1))
  }


  if (performance_data_type %in% c(
    "one model",
    "one model with model column"
  )) {
    plotly_base <- plotly_object %>%
      plotly::add_trace(
        data = performance_data,
        x = x_perf_metric,
        y = y_perf_metric,
        type = "scatter",
        mode = "markers+lines",
        color = I("black"),
        hoverinfo = "text",
        text = ~text
      )
  }

  if (performance_data_type == "several models") {
    plotly_base <- plotly_object %>%
      plotly::add_trace(
        data = performance_data,
        x = x_perf_metric,
        y = y_perf_metric,
        type = "scatter",
        mode = "markers+lines",
        color = ~model,
        colors = color_values_vec,
        hoverinfo = "text",
        text = ~text
      )
  }

  if (performance_data_type == "several populations") {
    plotly_base <- plotly_object %>%
      plotly::add_trace(
        data = performance_data,
        x = x_perf_metric,
        y = y_perf_metric,
        type = "scatter",
        mode = "markers+lines",
        color = ~population,
        colors = color_values_vec,
        hoverinfo = "text",
        text = ~text
      )
  }

  plotly_base
}





#' Create reference lines plotly as the first stage of interactive plot
#'
#' @inheritParams create_roc_curve
#'
#' @param performance_table_type the type of the performance table
#' @param curve the required curve
#' @param prevalence the prevalence of the population
#' @param population_color_vector color values
#' @param size the size of the curve
#' @keywords internal
create_reference_lines_for_plotly <- function(performance_table_type,
                                              curve,
                                              prevalence = NA,
                                              population_color_vector = NA,
                                              size = NULL,
                                              performance_data = NULL) {
  size_height <- switch(is.null(size) + 1,
    size + 50,
    NULL
  )


  if ((curve %in% c("roc", "lift")) || ((performance_table_type !=
    "several populations"))) {
    if (curve %in% c("gains", "decision")) {
      reference_lines_for_plotly <- create_reference_lines_data_frame(curve,
        plotly = TRUE,
        prevalence,
        performance_data = performance_data
      ) %>%
        plotly::plot_ly(
          x = ~x,
          y = ~y,
          height = size_height,
          width = size
        ) %>%
        plotly::add_lines(
          color = I("grey"),
          colors = population_color_vector,
          line = list(width = 1.75),
          linetype = ~population,
          hoverinfo = "text",
          text = ~text
        )
    } else {
      reference_lines_for_plotly <- create_reference_lines_data_frame(curve,
        plotly = TRUE,
        prevalence
      ) %>%
        plotly::plot_ly(
          x = ~x, y = ~y,
          height = size_height,
          width = size
        ) %>%
        plotly::add_lines(
          color = I("grey"),
          colors = population_color_vector,
          line = list(width = 1.75)
        )
    }
  } else {
    if (curve == "precision recall") {
      reference_lines_for_plotly <- create_reference_lines_data_frame(
        "precision recall",
        plotly = TRUE,
        prevalence
      ) %>%
        plotly::plot_ly(
          x = ~x,
          y = ~y,
          color = ~population,
          colors = population_color_vector,
          height = size_height,
          width = size
        ) %>%
        plotly::add_lines(line = list(dash = "dash", width = 1.75))
    }

    if (curve == "gains") {
      if (length(prevalence) == 1) {
        color_values <- "grey"
      }
      if (length(prevalence) > 1) {
        color_values <- population_color_vector[seq_len(length(prevalence))]
      }

      names(color_values) <- names(prevalence)

      population_color_reference_vector <- color_values %>%
        create_color_reference_lines_vector("gains")


      population_linetype_reference_vector <- color_values %>%
        create_linetype_reference_vector("gains")


      reference_lines_for_plotly <- create_reference_lines_data_frame("gains",
        plotly = TRUE,
        prevalence
      ) %>%
        # dplyr::left_join(color_values_dat) %>%
        plotly::plot_ly(
          x = ~x,
          y = ~y,
          color = ~population,
          colors = population_color_reference_vector,
          height = size_height,
          width = size
        ) %>%
        plotly::add_lines(
          line = list(width = 1.75),
          linetype = ~population,
          linetypes = population_linetype_reference_vector
        )
    }

    if (curve == "decision") {
      if (length(prevalence) == 1) {
        color_values <- "grey"
      }
      if (length(prevalence) > 1) {
        color_values <- population_color_vector[seq_len(length(prevalence))]
      }

      names(color_values) <- names(prevalence)

      population_color_reference_vector <- color_values %>%
        create_color_reference_lines_vector("decision")

      population_linetype_reference_vector <- color_values %>%
        create_linetype_reference_vector("decision")

      reference_lines_for_plotly <- create_reference_lines_data_frame(
        "decision",
        plotly = TRUE,
        prevalence,
        performance_data = performance_data
      ) %>%
        plotly::plot_ly(
          x = ~x,
          y = ~y,
          color = ~population,
          colors = population_color_reference_vector,
          height = size_height,
          width = size,
          hoverinfo = "text",
          text = ~text
        ) %>%
        plotly::add_lines(
          line = list(width = 1.75),
          linetype = ~population,
          linetypes = population_linetype_reference_vector
        )
    }
  }

  reference_lines_for_plotly
}







#' Creating color reference lines vector
#'
#' @param color_populations_vector color population vector
#' @param curve a curve
#' @keywords internal
create_color_reference_lines_vector <- function(color_populations_vector,
                                                curve) {
  if (curve == "gains") {
    color_populations_vector <- c(color_populations_vector, random = "grey")
  }
  if (curve == "precision recall") {
    color_populations_vector <- color_populations_vector
  }
  if (curve == "decision") {
    color_populations_vector <- c(color_populations_vector,
      treat_none = "grey"
    )
  }
  color_populations_vector
}



#' Creating linetype reference lines vector
#'
#' @param color_populations_vector color population vector
#' @param curve a curve
#' @keywords internal
create_linetype_reference_vector <- function(color_populations_vector, curve) {
  col_populations_vec <- rep("dash", length(color_populations_vector))
  names(col_populations_vec) <- names(color_populations_vector)
  if (curve == "gains") {
    col_populations_vec <- c(col_populations_vec, random = "solid")
  }
  if (curve == "decision") {
    col_populations_vec <- c(col_populations_vec, treat_none = "solid")
  }
  col_populations_vec
}
