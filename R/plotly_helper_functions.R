#' Check Performance type for plotly
#'
#' @param performance_table an rtichoke performance table

check_performance_table_type_for_plotly <- function(performance_table){
  if (!(names(performance_table)[1] %in% c("population", "model"))) {performance_table_type <- "one model"}
  if ((names(performance_table)[1] == "model") & (length(unique(performance_table[,1])) == 1)) 
  {performance_table_type <- "one model with model column"}
  if ((names(performance_table)[1] == "model") & (length(unique(performance_table[,1])) > 1)) {performance_table_type <- "several models"}
  if (names(performance_table)[1] == "population" ) {performance_table_type <- "several populations"}
  performance_table_type
}


#' Create basic plotly for Performance Metrics
#' 
#' Makes a basic plotly for the metrices
#'
#' @inheritParams create_ggplot_for_performance_metrics
#' @param performance_table_type the type of the performance table
#' @param col_values palette

create_plotly_base <- function(performance_table, 
                               x_perf_metric, 
                               y_perf_metric, 
                               performance_table_type = "one model",
                               col_values = c("#5E7F9A", 
                                              "#931B53", 
                                              "#F7DC2E", 
                                              "#C6C174", 
                                              "#75DBCD")) {
  
  
  if(performance_table_type %in% c("one model", "one model with model column")){
    plotly_base <- performance_table %>%
      plotly::plot_ly(
        x = x_perf_metric,
        y = y_perf_metric
      )
  } 
  
  if(performance_table_type == "several models"){
    plotly_base <- performance_table %>%
      plotly::plot_ly(
        x = x_perf_metric,
        y = y_perf_metric,
        color =~  model,
        colors = col_values
      )
  }
  
  if(performance_table_type == "several populations"){
    plotly_base <- performance_table %>%
      plotly::plot_ly(
        x = x_perf_metric,
        y = y_perf_metric,
        color =~  population,
        colors = col_values
      )
  }
  
  plotly_base
  
}


add_markers_and_lines_to_plotly <- function(plotly_object, performance_table_type){
  if(performance_table_type %in% c("one model", "one model with model column")){
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
  
  if (performance_table_type == "several models") {
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
  
  if (performance_table_type == "several populations") {
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
                                             main_slider = 'threshold'){
  plotly_object  %>%
    plotly::add_markers(
      frame = as.formula(paste0("~", main_slider)),
      marker =     list(
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
#' @param reference_lines dataframe of reference lines

add_reference_lines_to_plotly <- function(plotly_object, 
                                          reference_lines, 
                                          performance_table_type = "one model"){
  print(reference_lines)
  print(c(reference_lines$x, reference_lines$xend))
  print(c(reference_lines$y, reference_lines$yend))
  
  if (performance_table_type == "several populations") {
    # plotly_object %>%
    #   add_lines(
    #     data = reference_lines %>% 
    #       mutate(population = unique(performance_table_for_train_and_test_sets$population)) %>%
    #       tidyr::pivot_longer(cols = c(x, xend), values_to = "x") %>%
    #       select(-name) %>%
    #       tidyr::pivot_longer(cols = c(y, yend), values_to = "y") %>%
    #       select(-name) %>%
    #       distinct(),
    #     x =~ x,
    #     y =~ y
    #   )
    # 
    # fake_base_plotly <- fake_plotly_base(perf_table, sensitivity, PPV, performance_table_type = "several populations")
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
    # reference_lines  %>%
    #   split(1:nrow(.)) %>%
    #   purrr::reduce(add_reference_lines_to_plotly,
    #                 .init = fake_base_plotly) %>%
    #   add_markers() %>%
    #   add_lines()
    
    
  } else {
  
  plotly_object %>%
    plotly::add_lines(
      x = ~ c(reference_lines$x, reference_lines$xend),
      y = ~ c(reference_lines$y, reference_lines$yend),
      mode = "lines",
      color = I(reference_lines$col))
    }
}


