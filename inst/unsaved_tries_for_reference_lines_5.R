add_lines_and_markers_from_performance_data <- function(plotly_reference_lines,
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
                                                        main_slider = "threshold",
                                                        reference_lines = NA) {

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
    plotly_base <- plotly_reference_lines %>%
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
    plotly_base <- plotly_reference_lines %>%
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

    plotly_base <- plotly_reference_lines %>%
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


plot_metrics_curve <- function(reference_lines_plotly,
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
                       )){
  
  # x_perf_metric <- enquo(x_perf_metric)
  # y_perf_metric <- enquo(y_perf_metric)
  
  reference_lines_plotly %>% 
    plotly::add_lines_and_markers_from_performance_data(
      performance_data = performance_data,
      performance_data_type = performance_data_type,
      x_perf_metric,
      y_perf_metric
    ) %>%
    add_interactive_marker_from_performance_data(
      performance_data = one_pop_one_model_as_a_list,
      performance_data_type = perf_dat_type,
      x_perf_metric,
      y_perf_metric
    ) %>% 
    remove_grid_lines_from_plotly() %>%
    plotly::config(displayModeBar = F)
}







  


# roc one model one pop

perf_dat_type <- rtichoke::check_performance_data_type_for_plotly(one_pop_one_model_as_a_list)

create_reference_lines_for_plotly(perf_dat_type, "roc") %>% 
  add_lines_and_markers_from_performance_data(
    performance_data = one_pop_one_model_as_a_list,
    performance_data_type = perf_dat_type,
    FPR,
    sensitivity
  ) %>%
  add_interactive_marker_from_performance_data(
    performance_data = one_pop_one_model_as_a_list,
    performance_data_type = perf_dat_type,
    FPR,
    sensitivity
  ) %>%
  remove_grid_lines_from_plotly() %>%
  plotly::layout(
    xaxis = list(
      title = "1 - Specificity"
    ),
    yaxis = list(
      title = "Sensitivity"
    ),
    showlegend = F
  ) %>%
  plotly::config(displayModeBar = F)
  
  
create_reference_lines_for_plotly(perf_dat_type, "roc") %>% 
  add_lines_and_markers_from_performance_data(
    performance_data = one_pop_one_model_as_a_vector_enforced_percentiles_symmetry,
    performance_data_type = perf_dat_type,
    FPR,
    sensitivity
  ) %>%
  add_interactive_marker_from_performance_data(
    performance_data = one_pop_one_model_as_a_vector_enforced_percentiles_symmetry,
    performance_data_type = perf_dat_type,
    FPR,
    sensitivity, 
    main_slider = "predicted_positives_percent"
  ) %>%
  remove_grid_lines_from_plotly() %>%
  plotly::layout(
    xaxis = list(
      title = "1 - Specificity"
    ),
    yaxis = list(
      title = "Sensitivity"
    ),
    showlegend = F
  ) %>%
  plotly::config(displayModeBar = F)


create_reference_lines_for_plotly(perf_dat_type, "roc") %>% 
  add_lines_and_markers_from_performance_data(
    performance_data = one_pop_one_model_as_a_list,
    performance_data_type = perf_dat_type,
    FPR,
    sensitivity
  ) %>%
  add_interactive_marker_from_performance_data(
    performance_data = one_pop_one_model_as_a_list,
    performance_data_type = perf_dat_type,
    FPR,
    sensitivity
  ) %>%
  remove_grid_lines_from_plotly() %>%
  plotly::layout(
    xaxis = list(
      title = "1 - Specificity"
    ),
    yaxis = list(
      title = "Sensitivity"
    ),
    showlegend = F
  ) %>%
  plotly::config(displayModeBar = F)

create_reference_lines_for_plotly(perf_dat_type, "roc") %>% 
  add_lines_and_markers_from_performance_data(
    performance_data = one_pop_one_model_as_a_list_enforced_percentiles_symmetry,
    performance_data_type = perf_dat_type,
    FPR,
    sensitivity
  ) %>%
  add_interactive_marker_from_performance_data(
    performance_data = one_pop_one_model_as_a_list_enforced_percentiles_symmetry,
    performance_data_type = perf_dat_type,
    FPR,
    sensitivity,
    main_slider = "predicted_positives_percent"
  ) %>%
  remove_grid_lines_from_plotly() %>%
  plotly::layout(
    xaxis = list(
      title = "1 - Specificity"
    ),
    yaxis = list(
      title = "Sensitivity"
    ),
    showlegend = F
  ) %>%
  plotly::config(displayModeBar = F)


# roc several models

perf_dat_type <- rtichoke::check_performance_data_type_for_plotly(rtichoke::one_pop_three_models)

create_reference_lines_for_plotly(perf_dat_type, "roc", population_color_vector = c(
  "#21DACD",
  "#B6C174",
  "#A7DA2E",
  "#C2C172",
  "#FFD700"
)) %>% 
  add_lines_and_markers_from_performance_data(
    performance_data = one_pop_three_models,
    performance_data_type = perf_dat_type,
    FPR,
    sensitivity
  )  %>%
  add_interactive_marker_from_performance_data(
    performance_data = one_pop_three_models,
    performance_data_type = perf_dat_type,
    FPR,
    sensitivity
  )  %>%
  remove_grid_lines_from_plotly() %>%
  plotly::layout(
    xaxis = list(
      title = "1 - Specificity"
    ),
    yaxis = list(
      title = "Sensitivity"
    ),
    showlegend = F
  ) %>%
  plotly::config(displayModeBar = F)


create_reference_lines_for_plotly(perf_dat_type, "roc", population_color_vector = c(
  "#21DACD",
  "#B6C174",
  "#A7DA2E",
  "#C2C172",
  "#FFD700"
)) %>% 
  add_lines_and_markers_from_performance_data(
    performance_data = one_pop_three_models_enforced_percentiles_symmetry,
    performance_data_type = perf_dat_type,
    FPR,
    sensitivity
  )  %>%
  add_interactive_marker_from_performance_data(
    performance_data = one_pop_three_models_enforced_percentiles_symmetry,
    performance_data_type = perf_dat_type,
    FPR,
    sensitivity
  )  %>%
  remove_grid_lines_from_plotly() %>%
  plotly::layout(
    xaxis = list(
      title = "1 - Specificity"
    ),
    yaxis = list(
      title = "Sensitivity"
    ),
    showlegend = F
  ) %>%
  plotly::config(displayModeBar = F)


# roc several populations

perf_dat_type <- rtichoke::check_performance_data_type_for_plotly(rtichoke::train_and_test_sets)

create_reference_lines_for_plotly(perf_dat_type, "roc", population_color_vector = c(
  "#21DACD",
  "#B6C174",
  "#A7DA2E",
  "#C2C172",
  "#FFD700"
)) %>% 
  add_lines_and_markers_from_performance_data(
    performance_data = train_and_test_sets,
    performance_data_type = perf_dat_type,
    FPR,
    sensitivity
  )  %>%
  add_interactive_marker_from_performance_data(
    performance_data = train_and_test_sets,
    performance_data_type = perf_dat_type,
    FPR,
    sensitivity
  )  %>%
  remove_grid_lines_from_plotly() %>%
  plotly::layout(
    xaxis = list(
      title = "1 - Specificity"
    ),
    yaxis = list(
      title = "Sensitivity"
    ),
    showlegend = F
  ) %>%
  plotly::config(displayModeBar = F)


create_reference_lines_for_plotly(perf_dat_type, "roc", population_color_vector = c(
  "#21DACD",
  "#B6C174",
  "#A7DA2E",
  "#C2C172",
  "#FFD700"
)) %>% 
  add_lines_and_markers_from_performance_data(
    performance_data = train_and_test_sets_enforced_percentiles_symmetry,
    performance_data_type = perf_dat_type,
    FPR,
    sensitivity
  )  %>%
  add_interactive_marker_from_performance_data(
    performance_data = train_and_test_sets_enforced_percentiles_symmetry,
    performance_data_type = perf_dat_type,
    FPR,
    sensitivity, 
    main_slider = "predicted_positives_percent"
  )  %>%
  remove_grid_lines_from_plotly() %>%
  plotly::layout(
    xaxis = list(
      title = "1 - Specificity"
    ),
    yaxis = list(
      title = "Sensitivity"
    ),
    showlegend = F
  ) %>%
  plotly::config(displayModeBar = F)























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