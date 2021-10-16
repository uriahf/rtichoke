#' Check Performance type for plotly
#'
#' @param performance_data an rtichoke Performance Data

check_performance_data_type_for_plotly <- function(performance_data) {
  if (!(names(performance_data)[1] %in% c("population", "model"))) {
    performance_data_type <- "one model"
  }
  if ((names(performance_data)[1] == "model") & (length(unique(performance_data[, 1])) == 1)) {
    performance_data_type <- "one model with model column"
  }
  if ((names(performance_data)[1] == "model") & (length(unique(performance_data[, 1])) > 1)) {
    performance_data_type <- "several models"
  }
  if (names(performance_data)[1] == "population") {
    performance_data_type <- "several populations"
  }
  performance_data_type
}


#' Create basic plotly for Performance Metrics
#'
#' Makes a basic plotly for the metrices
#'
#' @inheritParams create_ggplot_for_performance_metrics
#' @param performance_data_type the type of the Performance Data
#' @param col_values palette

create_plotly_base <- function(performance_data,
                               x_perf_metric,
                               y_perf_metric,
                               performance_data_type = "one model",
                               col_values = c(
                                 "#5E7F9A",
                                 "#931B53",
                                 "#F7DC2E",
                                 "#C6C174",
                                 "#75DBCD"
                               )) {
  if (performance_data_type %in% c("one model", "one model with model column")) {
    plotly_base <- performance_data %>%
      plotly::plot_ly(
        x = x_perf_metric,
        y = y_perf_metric
      )
  }

  if (performance_data_type == "several models") {
    plotly_base <- performance_data %>%
      plotly::plot_ly(
        x = x_perf_metric,
        y = y_perf_metric,
        color = ~model,
        colors = col_values
      )
  }

  if (performance_data_type == "several populations") {
    plotly_base <- performance_data %>%
      plotly::plot_ly(
        x = x_perf_metric,
        y = y_perf_metric,
        color = ~population,
        colors = col_values
      )
  }

  plotly_base
}


add_markers_and_lines_to_plotly <- function(plotly_object, performance_data_type) {
  if (performance_data_type %in% c("one model", "one model with model column")) {
    plotly_with_markers_and_lines <- plotly_object %>%
      plotly::add_trace(
        hoverinfo = "text",
        text = ~ paste(
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
  plotly_with_markers_and_lines
}


#' Add Interactive Marker to Plotly
#'
#' @param plotly_object a plotly plot for performance metrics
#' @param main_slider what is the main slider - threshold, percent positives or positives

add_interactive_marker_to_plotly <- function(plotly_object,
                                             main_slider = "threshold") {
  plotly_object %>%
    plotly::add_markers(
      frame = as.formula(paste0("~", main_slider)),
      marker = list(
        size = 12,
        line = list(
          width = 3,
          color = I("black")
        )
      ),
      hoverinfo = "text",
      text = ~ paste(
        # "Model:", model,
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
      )
    )
}


#' Add Reference Lines to Plotly Object
#'
#' @param plotly_object a plotly plot for performance metrics
#' @param performance_data_type the type of the Performance Data
#' @param reference_lines dataframe of reference lines

add_reference_lines_to_plotly <- function(plotly_object,
                                          reference_lines,
                                          performance_data_type = "one model") {
  print(reference_lines)
  print(c(reference_lines$x, reference_lines$xend))
  print(c(reference_lines$y, reference_lines$yend))

  if (performance_data_type == "several populations") {
    # plotly_object %>%
    #   add_lines(
    #     data = reference_lines %>%
    #       mutate(population = unique(performance_data_for_train_and_test_sets$population)) %>%
    #       tidyr::pivot_longer(cols = c(x, xend), values_to = "x") %>%
    #       select(-name) %>%
    #       tidyr::pivot_longer(cols = c(y, yend), values_to = "y") %>%
    #       select(-name) %>%
    #       distinct(),
    #     x =~ x,
    #     y =~ y
    #   )
    #
    # fake_base_plotly <- fake_plotly_base(perf_table, sensitivity, PPV, performance_data_type = "several populations")
    # reference_lines <- create_reference_lines_data_frame("precision recall", prevalence)
    #
    # # works!
    #
    # fake_base_plotly %>%
    #   add_reference_lines_to_plotly(reference_lines[1,]) %>%
    #   add_reference_lines_to_plotly(reference_lines[2,]) %>%
    #   add_reference_lines_to_plotly(reference_lines[3,]) %>%
    #   add_reference_lines_to_plotly(reference_lines[4,]) %>%
    #   add_reference_lines_to_plotly(reference_lines[5,]) %>%
    #   add_markers() %>%
    #   add_lines()
    #
    # # doesn't work!
    #
    reference_lines %>%
      split(1:nrow(.)) %>%
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
#' @param max_y_range the maximum value of y range (for lift curve)
#'
#' @return
set_styling_for_rtichoke <- function(plotly_object, curve, max_y_range = NA) {
  plotly_object %>% 
    remove_grid_lines_from_plotly() %>% 
    set_axis_titles(curve, max_y_range = max_y_range) %>% 
    plotly::config(displayModeBar = F) 
}



#' Set Titles for x and y axis in plotly objects
#'
#' @param plotly_object a plotly object 
#' @param curve the required curve
#' @param max_y_range the maximum value for y range
#'
#' @return
set_axis_titles <- function(plotly_object, curve, max_y_range = NA){
  if ( curve == "roc" ) {
    plotly_obj <- plotly_object %>% 
      plotly::layout(
        xaxis = list(
          title = "1 - Specificity"
        ),
        yaxis = list(
          title = "Sensitivity"
        ),
        showlegend = F
      )
  }
  
  if ( curve == "lift" ) {
    plotly_obj <- plotly_object %>% 
      plotly::layout(
        xaxis = list(
          title = "Predicted Positives Percent",
          range = c(-0.1,1.1)
        ),
        yaxis = list(
          title = "Lift",
          range = c(-0.1,max_y_range)
        ),
        showlegend = F
      )
  }
  
  if ( curve == "precision recall" ) {
    plotly_obj <- plotly_object %>% 
      plotly::layout(
        xaxis = list(
          title = "Sensitivity",
          range = c(-0.1,1.1),
          fixedrange = TRUE
        ),
        yaxis = list(
          title = "PPV",
          range = c(-0.1,1.1),
          fixedrange = TRUE
        ),
        showlegend = F
      )
  }
  
  if ( curve == "gains" ) {
    plotly_obj <- plotly_object %>% 
      plotly::layout(
        xaxis = list(
          title = "predicted_positives_percent",
          range = c(-0.1,1.1),
          fixedrange = TRUE
        ),
        yaxis = list(
          title = "Sensitivity",
          range = c(-0.1,1.1),
          fixedrange = TRUE
        ),
        showlegend = F
      )
  }
  plotly_obj 
}





#' Add interactive marker based on performance data
#'
#' @inheritParams add_lines_and_markers_from_performance_data
#'
#' @return
add_interactive_marker_from_performance_data <- function(plotly_object,
                                                         performance_data,
                                                         performance_data_type,
                                                         x_perf_metric,
                                                         y_perf_metric,
                                                         main_slider = "threshold"){
  x_perf_metric <- enquo(x_perf_metric)
  y_perf_metric <- enquo(y_perf_metric)
  
  if (performance_data_type %in% c("one model", "one model with model column")) {
    
    plotly_plot <- plotly_object %>%
      plotly::add_markers(
        data = performance_data,
        x = x_perf_metric,
        y = y_perf_metric,
        frame = as.formula(paste0("~", main_slider)),
        marker = list(
          size = 12,
          line = list(
            width = 3,
            color = I("black")
          ),
          color = "#f6e3be"
        ),
        hoverinfo = "text",
        text = ~ paste(
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
        )
      )
  }
  if (performance_data_type == "several models") {
    plotly_plot <- plotly_object %>%
      plotly::add_markers(
        data = performance_data,
        x = x_perf_metric,
        y = y_perf_metric,
        frame = as.formula(paste0("~", main_slider)),
        color =~ model,
        marker = list(
          size = 12,
          line = list(
            width = 3,
            color = I("black")
          )
        ),
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
        )
      )
  }
  if (performance_data_type == "several populations") {
    plotly_plot <- plotly_object %>%
      plotly::add_markers(
        data = performance_data,
        x = x_perf_metric,
        y = y_perf_metric,
        frame = as.formula(paste0("~", main_slider)),
        color =~ population,
        marker = list(
          size = 12,
          line = list(
            width = 3,
            color = I("black")
          )
        ),
        hoverinfo = "text",
        text = ~ paste(
          "Population:", population, "<br>",
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
        )
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
#' @param col_values color palette
#' @param main_slider the main slider for interactivity
#'
#' @return
add_lines_and_markers_from_performance_data <- function(plotly_object,
                                                        performance_data,
                                                        performance_data_type,
                                                        x_perf_metric,
                                                        y_perf_metric,
                                                        col_values = c(
                                                          "#5E7F9A",
                                                          "#931B53",
                                                          "#F7DC2E",
                                                          "#C6C174",
                                                          "#75DBCD"
                                                        ),
                                                        main_slider = "threshold") {
  
  x_perf_metric <- enquo(x_perf_metric)
  y_perf_metric <- enquo(y_perf_metric)
  
  # print(x_perf_metric)
  # print(typeof(x_perf_metric))  
  
  if (performance_data_type %in% c("one model", "one model with model column")) {
    col_values_vec <- "black"
  } else {
    col_values_vec <- col_values[1:length(unique(performance_data[, 1]))]
    names(col_values_vec) <- unique(performance_data[, 1])
  }
  
  if (performance_data_type %in% c("one model", "one model with model column")) {
    plotly_base <- plotly_object %>%
      plotly::add_trace(
        data = performance_data,
        x = x_perf_metric,
        y = y_perf_metric,
        type = "scatter",
        mode = "markers+lines",
        color = I("black"),
        hoverinfo = "text",
        text = ~ paste(
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
        )
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
        colors = col_values_vec,
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
        )
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
        colors = col_values_vec,
        hoverinfo = "text",
        text = ~ paste(
          "Population:", population, "<br>",
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
        )
      )
  }
  
  plotly_base
}





#' Create reference lines plotly as the first stage of interactive plot
#'
#' @param performance_table_type the type of the performance table
#' @param curve the required curve
#' @param prevalence the prevalence of the population
#' @param population_color_vector color values
#'
#' @return
create_reference_lines_for_plotly <- function(performance_table_type, 
                                              curve, 
                                              prevalence = NA, 
                                              population_color_vector = NA){
  if (curve %in% c("roc", "lift") || performance_table_type != "several populations" ) {

    reference_lines_for_plotly <- create_reference_lines_data_frame(curve, 
                                                                    plotly = T, 
                                                                    prevalence) %>%
      plotly::plot_ly(x =~ x ,y =~y)  %>%
      plotly::add_lines(color = I("grey"), colors = population_color_vector, line = list(width = 1.75))
    
  } else {
    
    if (curve == "precision recall") {
      
      
      reference_lines_for_plotly <- create_reference_lines_data_frame("precision recall", 
                                                                      plotly = T, 
                                                                      prevalence) %>%
        plotly::plot_ly(x =~ x ,
                        y =~y, 
                        color =~ population,
                        colors =  population_color_vector) %>%
        plotly::add_lines(line = list(dash = 'dash',  width = 1.75))
      
    }
    
    if (curve == "gains") {
      
      if (length(prevalence) == 1) {
        col_values <- "grey"
      }
      if (length(prevalence) > 1) {
        col_values <- c(
          "#5E7F9A",
          "#931B53",
          "#F7DC2E",
          "#C6C174",
          "#75DBCD"
        )[1:length(prevalence)]
      }
      
      # col_values_dat <- data.frame(
      #     population = c(names(prevalence), "random"),
      #     population_color = c(col_values, "grey"),
      #     linetype = c(rep("solid", length(prevalence)), "dashed")
      # )
      
      
      names(col_values) <- names(prevalence)

      population_color_reference_vector <- col_values %>%
        create_color_reference_lines_vector("gains")
      print(population_color_reference_vector)


      population_linetype_reference_vector <- col_values %>%
        create_linetype_reference_vector("gains")
      print(population_linetype_reference_vector)

      
      # reference_lines_for_plotly <- create_reference_lines_data_frame("gains", 
      #                                                                 plotly = T, 
      #                                                                 prevalence) %>%
      #   plotly::plot_ly(x =~ x,
      #                   y =~y, 
      #                   color =~ population,
      #                   colors =  population_color_vector) %>%

      # 
      

      reference_lines_for_plotly <- create_reference_lines_data_frame("gains", 
                                                                      plotly = T, 
                                                                      prevalence)  %>% 
        dplyr::left_join(col_values_dat) %>% 
        plotly::plot_ly(x =~ x,
                        y =~ y, 
                        color =~ population,
                        colors = population_color_reference_vector) %>%
        plotly::add_lines(line = list(width = 1.75),
                          linetype =~ population,
                          linetypes = population_linetype_reference_vector)
      
    }
    
    if (curve == "decision") {
      
      population_color_reference_vector <- population_color_vector %>%
        create_color_reference_lines_vector("decision")
      
      population_linetype_reference_vector <- population_color_vector %>%
        create_linetype_reference_vector("decision")
      
      reference_lines_for_plotly <- create_reference_lines_data_frame("decision", 
                                                                      plotly = T, 
                                                                      prevalence) %>%
        plotly::plot_ly(x =~ x,
                        y =~y,
                        color =~ population,
                        colors =  population_color_reference_vector) %>%
        plotly::add_lines(line = list(width = 1.75),
                          linetype =~ population,
                          linetypes = population_linetype_reference_vector)
      
    }
    
  }
  
  reference_lines_for_plotly
  
}
