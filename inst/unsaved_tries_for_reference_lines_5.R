




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
  set_styling_for_rtichoke("roc")


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
  set_styling_for_rtichoke("roc")























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