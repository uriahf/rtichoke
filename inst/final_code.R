## Check Inputs

check_curve_input <- function(curve) {
  rlang::arg_match(curve, c(
    "roc", "lift", "precision recall", "gains",
    "decision", "interventions avoided"
  ))
}
check_stratified_by_input <- function(stratified_by) {
  rlang::arg_match(stratified_by, c("probability_threshold", "ppcr"))
}


# Plot Curve

create_rtichoke_curve_list <- function(performance_data,
                                       curve, 
                                       min_p_threshold = 0, 
                                       max_p_threshold = 1, 
                                       size = NULL){
  
  rtichoke_curve_list <- list()
  
  stratified_by <- rtichoke:::check_performance_data_stratification(
    performance_data
  )
  
  rtichoke_curve_list$perf_dat_type <- rtichoke:::check_performance_data_type_for_plotly(
    performance_data = performance_data)
  
  
  rtichoke_curve_list$group_colors_vec <- performance_data |>
    extract_reference_groups_from_performance_data(rtichoke_curve_list$perf_dat_type) |>
    create_reference_group_color_vector(rtichoke_curve_list$perf_dat_type, col_values = c(
      "#1b9e77", "#d95f02",
      "#7570b3", "#e7298a",
      "#07004D", "#E6AB02",
      "#FE5F55", "#54494B",
      "#006E90", "#BC96E6",
      "#52050A", "#1F271B",
      "#BE7C4D", "#63768D",
      "#08A045", "#320A28",
      "#82FF9E", "#2176FF",
      "#D1603D", "#585123"
    )) |> 
    as.list()
  
  prevalence_from_performance_data <- rtichoke:::get_prevalence_from_performance_data(performance_data) |> 
    as.list()
  
  rtichoke_curve_list$reference_data <- create_reference_lines_data(
    curve, 
    prevalence_from_performance_data,
    rtichoke_curve_list$perf_dat_type,
    min_p_threshold = min_p_threshold,
    max_p_threshold = max_p_threshold)
  
  if (curve == "roc") {
    
    x_performance_metric <- "FPR"
    y_performance_metric <- "sensitivity"
    
    rtichoke_curve_list$axes_labels$xaxis <- "1 - Specificity"
    rtichoke_curve_list$axes_labels$yaxis <- "Sensitivity"
    
  } else if (curve == "precision recall") {
    
    x_performance_metric <- "sensitivity"
    y_performance_metric <- "PPV"
    
    rtichoke_curve_list$axes_labels$xaxis <- "Sensitivity"
    rtichoke_curve_list$axes_labels$yaxis <- "PPV"
    
  } else if (curve == "lift") {
    
    x_performance_metric <- "ppcr"
    y_performance_metric <- "lift"
    
    rtichoke_curve_list$axes_labels$xaxis <- "Predicted Positives (Rate)"
    rtichoke_curve_list$axes_labels$yaxis <- "Lift"
    
  } else if (curve == "gains") {
    
    x_performance_metric <- "ppcr"
    y_performance_metric <- "sensitivity"
    
    rtichoke_curve_list$axes_labels$xaxis <- "Predicted Positives (Rate)"
    rtichoke_curve_list$axes_labels$yaxis <- "Sensitivity"
    
  } else if (curve == "decision") {
    
    x_performance_metric <- "probability_threshold"
    y_performance_metric <- "NB"
    
    rtichoke_curve_list$axes_labels$xaxis <- "Probability Threshold"
    rtichoke_curve_list$axes_labels$yaxis <- "Net Benefit"
    
  } else if (curve == "interventions avoided") {
    
    x_performance_metric <- "probability_threshold"
    y_performance_metric <- "NB_treatment_avoided"
    
    rtichoke_curve_list$axes_labels$xaxis <- "Probability Threshold"
    rtichoke_curve_list$axes_labels$yaxis <- "Interventions Avoided (per 100)"
    
  }
  
  rtichoke_curve_list$performance_data_ready_for_curve <- performance_data |>
    prepare_performance_data_for_curve(
      x_performance_metric,
      y_performance_metric,
      stratified_by,
      rtichoke_curve_list$perf_dat_type, 
    )
  
  rtichoke_curve_list$axes_ranges <- extract_axes_ranges(rtichoke_curve_list$performance_data_ready_for_curve, 
                                     curve,
                                     min_p_threshold = min_p_threshold,
                                     max_p_threshold = max_p_threshold) |> 
    as.list()
  
  rtichoke_curve_list
  
}





create_plotly_curve <- function(rtichoke_curve_list){
  
  size_height <- switch(is.null(rtichoke_curve_list$size) +1, rtichoke_curve_list$size + 50, NULL)
  
  interactive_marker <- list(
    size = 12,
    line = list(
      width = 3,
      color = I("black")
    )
  )
  
  if ( ! (rtichoke_curve_list$perf_dat_type %in% c("several models", "several populations")) ) {
    
    interactive_marker$color <- "#f6e3be"
    
  }
  
  plotly::plot_ly(
    x = ~x,
    y = ~y,
    height = size_height,
    width = rtichoke_curve_list$size,
    hoverinfo = "text",
    text = ~text,
    color = ~reference_group,
    colors = unlist(rtichoke_curve_list$group_colors_vec)
  ) |>
    plotly::add_lines(
      data = rtichoke_curve_list$reference_data,
      line = list(
        dash = "dot"
      )
    ) |>
    plotly::add_trace(
      data = rtichoke_curve_list$performance_data_ready_for_curve,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(dash = 'solid')
    ) |>
    plotly::add_markers(
      data = rtichoke_curve_list$performance_data_ready_for_curve,
      frame =~ stratified_by,
      marker = interactive_marker
    ) |>
    plotly::layout(
      xaxis = list(showgrid = FALSE, fixedrange = TRUE,
                   range = rtichoke_curve_list$axes_ranges$xaxis, title = rtichoke_curve_list$axes_labels$xaxis),
      yaxis = list(showgrid = FALSE, fixedrange = TRUE,
                   range = rtichoke_curve_list$axes_ranges$yaxis, title = rtichoke_curve_list$axes_labels$xaxis),
      showlegend = FALSE
    ) |>
    plotly::config(displayModeBar = FALSE)
  
}


plot_rtichoke_curve <- function(performance_data, curve, col_values = c(
  "#1b9e77", "#d95f02",
  "#7570b3", "#e7298a",
  "#07004D", "#E6AB02",
  "#FE5F55", "#54494B",
  "#006E90", "#BC96E6",
  "#52050A", "#1F271B",
  "#BE7C4D", "#63768D",
  "#08A045", "#320A28",
  "#82FF9E", "#2176FF",
  "#D1603D", "#585123"
), min_p_threshold = 0, max_p_threshold = 1, size = NULL) {
  check_curve_input(curve)
  
  stratified_by <- rtichoke:::check_performance_data_stratification(
    performance_data
  )

  prevalence_from_performance_data <- rtichoke:::get_prevalence_from_performance_data(performance_data)
  perf_dat_type <- rtichoke:::check_performance_data_type_for_plotly(performance_data = performance_data)

  reference_group_colors_vec <- performance_data |>
    extract_reference_groups_from_performance_data(perf_dat_type) |>
    create_reference_group_color_vector(perf_dat_type, col_values = col_values)
  
  print(paste("reference_group_color_vector", 
              reference_group_colors_vec))
  

  print(reference_group_colors_vec)
  
  if (curve == "roc") {
    x_performance_metric <- "FPR"
    y_performance_metric <- "sensitivity"
  } else if (curve == "precision recall") {
    x_performance_metric <- "sensitivity"
    y_performance_metric <- "PPV"
  } else if (curve == "lift") {
    x_performance_metric <- "ppcr"
    y_performance_metric <- "lift"
  } else if (curve == "gains") {
    x_performance_metric <- "ppcr"
    y_performance_metric <- "sensitivity"
  } else if (curve == "decision") {
    x_performance_metric <- "probability_threshold"
    y_performance_metric <- "NB"
  } else if (curve == "interventions avoided") {
    x_performance_metric <- "probability_threshold"
    y_performance_metric <- "NB_treatment_avoided"
  }
  
  
  performance_data_ready_for_curve <- performance_data |>
    prepare_performance_data_for_curve(
      x_performance_metric,
      y_performance_metric,
      stratified_by,
      perf_dat_type, 
      min_p_threshold, 
      max_p_threshold
    )
  
  axis_ranges <- extract_axes_ranges(performance_data_ready_for_curve, 
                                     curve,
                                     min_p_threshold,
                                     max_p_threshold)
  
  print(axis_ranges)
  
  create_reference_lines_for_plotly_new(
    curve,
    prevalence_from_performance_data,
    reference_group_colors = reference_group_colors_vec,
    perf_dat_type,
    size,
    min_p_threshold,
    max_p_threshold
  ) |>
    add_markers_and_lines_for_plotly_reference_object(
      performance_data_ready_for_curve, perf_dat_type) |>
    rtichoke:::set_styling_for_rtichoke(curve, 
                                        min_x_range = axis_ranges$xaxis[1], 
                                        max_x_range = axis_ranges$xaxis[2], 
                                        min_y_range = axis_ranges$yaxis[1], 
                                        max_y_range = axis_ranges$yaxis[2]) |>
    
    plotly::animation_slider(
      currentvalue = list(
        prefix = ifelse(stratified_by == "probability_threshold",
               "Prob. Threshold: ",
               "Predicted Positives (Rate):"),
        font = list(color = "black"),
        xanchor = "left"
      ),
      pad = list(t = 50)
    )
}

##  helpers

extract_reference_groups_from_performance_data <- function(performance_data, perf_data_type) {
  if (perf_data_type == "several models") {
    reference_groups <- unique(performance_data$model)
  } else if (perf_data_type == "several populations") {
    reference_groups <- unique(performance_data$population)
  } else {
    reference_groups <- "model"
  }

  reference_groups
}


create_reference_group_color_vector <- function(reference_groups,
                                                perf_dat_type,
                                                col_values) {
  
  if (!(perf_dat_type %in% c("several populations", "several models")) ) {
  
    col_values <- "black"
    
  } 
  
  reference_group_color_vector <- c(
    "#BEBEBE", 
    "#BEBEBE",
    "#BEBEBE",
    col_values[1:length(reference_groups)], 
    col_values[1:length(reference_groups)], # fix to grey when one population
    col_values[1:length(reference_groups)]
  )
  
  names(reference_group_color_vector) <- c(
    "reference_line",
    "reference_line_perfect_model",
    "reference_line_treat_all",
    paste0("reference_line_treat_all_", reference_groups),
    paste0("reference_line_perfect_model_", reference_groups),
    reference_groups
  )

  print(paste(
    "reference_group_color_vector:", reference_group_color_vector))
  
  reference_group_color_vector
}

# TODO check with blog examples interventions avoided


prepare_performance_data_for_curve <- function(
    performance_data,
    x_performance_metric,
    y_performance_metric,
    stratified_by,
    perf_dat_type, 
    min_p_threshold = 0, 
    max_p_threshold = 1){
  
  
  performance_data %>%
    {
      if (y_performance_metric == "NB_treatment_avoided") {
        dplyr::mutate(., N = TP +TN  + FP + FN,
                      prevalence = (TP + FN) / N,
                      NB_intervention_all =   prevalence - (1- prevalence) * 
                        (probability_threshold) / (1 - probability_threshold),
                      NB_treatment_avoided = 100 * (NB - NB_intervention_all) * 
                        ( (1 - probability_threshold) / probability_threshold ))
      } else {
        .
      }
    } %>%
    {
      if (stratified_by == "probability_threshold") {
        dplyr::filter(., probability_threshold <= max_p_threshold) |> 
          dplyr::filter(probability_threshold >= min_p_threshold)
      } else {
        .
      }
    }|> 
    add_hover_text_to_performance_data_new(
      x_performance_metric, y_performance_metric, stratified_by, perf_dat_type)|> 
    select_and_rename_necessary_variables(
      x_performance_metric, 
      y_performance_metric, 
      stratified_by, 
      perf_dat_type) |> 
    dplyr::filter(!is.nan(y))  
  
}


add_hover_text_to_performance_data_new <- function(
    performance_data,
    performance_metric_x,
    performance_metric_y,
    stratified_by,
    perf_dat_type){
  
  hover_text <- create_hover_text(
    stratified_by = stratified_by,
    interventions_avoided = (performance_metric_x == "NB_treatment_avoided")) |> 
    rtichoke:::make_two_performance_metrics_bold(
      performance_metric_x, 
      performance_metric_y) %>% 
    {
      if (perf_dat_type == "several models") {
        rtichoke:::add_models_for_text_for_hover(.)
      } else {
        .
      }
    } %>% 
    {
      if (perf_dat_type == "several populations") {
        rtichoke:::add_population_for_text_for_hover(.)
      } else {
        .
      }
    }
  
  print(hover_text)
  
  performance_data %>%
    dplyr::mutate(
      dplyr::across(where(is.numeric), round, 3),
      text = glue::glue(hover_text),
      text = stringr::str_replace_all(text, pattern = "NaN", "")
    )
  
}

create_hover_text <- function(stratified_by, interventions_avoided = FALSE) {
  
  if (interventions_avoided == FALSE) {
    
    text_for_hover <- paste0(
      "Prob. Threshold: {probability_threshold}<br>\\
Sensitivity: {sensitivity}<br>\\
1 - Specificity (FPR): {FPR}<br>\\
Specificity: {specificity}<br>\\
Lift: {lift}<br>\\
PPV: {PPV}<br>\\
NPV: {NPV}<br>",
      ifelse ( stratified_by == "probability_threshold", 
               "NB: {NB}<br>", 
               "" ),
      "Predicted Positives: {predicted_positives} ({100 * ppcr}%)
TP: {TP}<br>\\
TN: {TN}<br>\\
FP: {FP}<br>\\
FN: {FN}"
    ) 
    
  } else {
    
    text_for_hover <- "Prob. Threshold: {probability_threshold}
Interventions Avoided (per 100): {NB_treatment_avoided}<br>\\
NB: {NB}<br>\\
Predicted Positives: {predicted_positives} ({100 * ppcr}%)<br>\\
TN: {TN}<br>\\
FN: {FN}"
    
  }
  
  text_for_hover
  
}


select_and_rename_necessary_variables <- function(performance_data, 
                                                  x_perf_metric, 
                                                  y_perf_metric, 
                                                  stratified_by, 
                                                  perf_dat_type){
  
  x_perf_metric <- sym(x_perf_metric)
  y_perf_metric <- sym(y_perf_metric)
  stratified_by <- sym(stratified_by)

  
  if (!(perf_dat_type %in% c("several populations", "several models")) ) {
    
    performance_data <- performance_data |> 
      dplyr::mutate(model = "model")
    
  }
  
  if ( perf_dat_type == "several populations" ) {
    
    reference_group <- sym("population")
    
  } else {
    
    reference_group <- sym("model")
    
  }
  
  
  performance_data |>
    dplyr::select(
      reference_group = {{reference_group}},
      x = {{x_perf_metric}},
      y = {{y_perf_metric}},
      stratified_by = {{stratified_by}},
      text
    ) 
  
  }
  
add_markers_and_lines_for_plotly_reference_object <- function(
    plotly_object, 
    performance_data_ready,
    perf_dat_type){
  
  interactive_marker <- list(
    size = 12,
    line = list(
      width = 3,
      color = I("black")
    )
  )
  
  if ( ! (perf_dat_type %in% c("several models", "several populations")) ) {
    
    interactive_marker$color <- "#f6e3be"
    
  }
  
  plotly_object |> 
    add_trace(
      data = performance_data_ready,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(dash = 'solid')
    ) |> 
    add_markers(
      data = performance_data_ready,
      frame =~ stratified_by,
      marker = interactive_marker
    )
  
}

create_reference_lines_for_plotly_new <- function(curve,
                                                  prevalence,
                                                  reference_group_colors,
                                                  perf_dat_type,
                                                  size,
                                                  min_p_threshold, 
                                                  max_p_threshold) {
  
  size_height <- switch(is.null(size) +1, size + 50, NULL)
  
  plot_ly(
    x = ~x,
    y = ~y,
    height = size_height,
    width = size,
    hoverinfo = "text",
    text = ~text,
    color = ~reference_group,
    colors = reference_group_colors
  )|>
    add_lines(
      data = create_reference_lines_data(
        curve, prevalence,
        perf_dat_type,
        min_p_threshold, 
        max_p_threshold),
      line = list(
        dash = "dot"
      )
    )
  
}

create_reference_lines_data <- function(curve, prevalence,
                                        perf_dat_type, 
                                        min_p_threshold, 
                                        max_p_threshold){
  
  check_curve_input(curve)
  
  if (curve == "roc") {
    
    reference_lines_data <- data.frame(
      reference_group = "reference_line",
      x = seq(0, 1, by = 0.01), 
      y = seq(0, 1, by = 0.01)) |> 
      dplyr::mutate(
        text = 
          glue::glue(
            "<b>Random Guess</b><br>Sensitivity: {y}<br>1 - Specificity: {x}")
      )
    
  }
  
  if (curve == "lift") {
    
    reference_lines_data <- data.frame(
      reference_group = "reference_line",
      x = seq(0.01, 1, by = 0.01), 
      y = rep(1, 100)) |> 
      dplyr::mutate(
        text = 
          glue::glue(
            "<b>Random Guess</b><br>Lift: {y}<br>Predicted Positives: {100*x}%")
            )

  }
  
  if (curve == "precision recall") {
    
    if (perf_dat_type == "several populations") {
      
      reference_group <- rep(names(prevalence), each = 100)
      reference_line_x_values <- rep(seq(0.01, 1, by = 0.01), times = length(prevalence))
      reference_line_y_values <- rep(prevalence, each = 100)
      hover_text <- "<b>Random Guess ({reference_group})</b><br>PPV: {round(y, digits = 3)}<br>Sensitivity: {x}"
      
      
    } else {
      
      reference_group <- "reference_line"
      reference_line_x_values <- seq(0.01, 1, by = 0.01)
      reference_line_y_values <- rep(unique(prevalence), 100)
      hover_text <- "<b>Random Guess</b><br>PPV: {round(y, digits = 3)}<br>Sensitivity: {x}"
      
    }
    
    
    reference_lines_data <- data.frame(
      reference_group = reference_group,
      x = reference_line_x_values,
      y = reference_line_y_values
    ) |> 
      dplyr::mutate(text = glue::glue(hover_text))
    
    print(reference_lines_data)
    
  }
  
  if (curve == "gains") {
    
    hover_text_random <- "<b>Random Guess</b><br>PPV: {round(y, digits = 3)}<br>Sensitivity: {x}"
    
    if (perf_dat_type == "several populations") {
      
      reference_group <- rep(c("reference_line", paste0("reference_line_perfect_model_", names(prevalence))), each = 101)
      reference_line_x_values <- rep(seq(0, 1, by = 0.01), times = (length(prevalence) + 1) )
      reference_line_y_values <- c(
        seq(0, 1, by = 0.01),
        prevalence |> 
          purrr::map(~return_perfect_prediction_gains_y_values(.x)) |>
          unlist())
      hover_text_perfect <- "<b>Perfect Prediction ({reference_group})</b><br>PPV: {round(y, digits = 3)}<br>Sensitivity: {x}"
      
      
    } else {
      
      reference_group <- rep(c("reference_line", "reference_line_perfect_model"), each = 101)
      reference_line_x_values <- rep(seq(0, 1, by = 0.01), times = 2)
      reference_line_y_values <- c(
          seq(0, 1, by = 0.01),
          return_perfect_prediction_gains_y_values(unique(prevalence))
        )
      
      hover_text_perfect <- "<b>Perfect Prediction</b><br>PPV: {round(y, digits = 3)}<br>Sensitivity: {x}"
      
    }
      
    reference_lines_data <- data.frame(
      reference_group = reference_group,
      x = reference_line_x_values,
      y = reference_line_y_values
    ) |> 
      dplyr::mutate(text = dplyr::case_when(
        reference_group == "reference_line" ~ glue::glue(hover_text_random),
        TRUE ~ glue::glue(hover_text_perfect)),
        text = stringr::str_replace(text, "reference_line_perfect_model_", ""))
    
  }
  
  if (curve == "decision") {
    
    if (perf_dat_type == "several populations") {
      
      reference_group <- rep(c("reference_line", 
                               paste0("reference_line_perfect_model_", names(prevalence))), 
                             each = 100)
      reference_line_x_values <- rep(seq(0, 0.99, by = 0.01), times = (length(prevalence) + 1) )
      reference_line_y_values <- c(
        rep(0, 100),
        prevalence |> 
          purrr::map(~return_treat_all_y_values(.x)) |>
          unlist())
      
      hover_text_treat_all <- "<b>Treat All ({reference_group})</b><br>NB: {round(y, digits = 3)}<br>Probability Threshold: {x}"
      
      
    } else {
      
      reference_group <- rep(c("reference_line", "reference_line_treat_all"), each = 100)
      reference_line_x_values <- rep(seq(0, 0.99, by = 0.01), times = 2)
      reference_line_y_values <- c(rep(0, 100), c(unique(prevalence) - (1 - unique(prevalence)) * 
                                                    (seq(0, 0.99, by = 0.01))  / (1 - seq(0, 0.99, by = 0.01) )))
      hover_text_treat_all <- "<b>Treat All</b><br>NB: {round(y, digits = 3)}<br>Probability Threshold: {x}"
      
    }
    
      hover_text_treat_none <- "<b>Treat None</b><br>NB: 0<br>Probability Threshold: {x}"
    
    
      reference_lines_data <- data.frame(
        reference_group = reference_group,
        x = reference_line_x_values,
        y = reference_line_y_values
      ) |> 
        dplyr::mutate(text = dplyr::case_when(
          reference_group == "reference_line" ~ glue::glue(hover_text_treat_none),
          TRUE ~ glue::glue(hover_text_treat_all)),
          text = stringr::str_replace(text, "reference_line_perfect_model_", "")) |> 
        dplyr::filter(x >= min_p_threshold, x<= max_p_threshold)
      
    
  }
  
  if (curve == "interventions avoided") {
    
    reference_lines_data <- data.frame(
      x = NA, 
      y = NA, 
      reference_group = NA,
      text = NA)
  }
  
  
  reference_lines_data
  
}


return_perfect_prediction_gains_y_values <- function(prevalence) {
  
  c(
    seq(0, 1, length.out = (100 * (round(prevalence, digits = 3)) + 1)), 
    rep(1, (100 - 100 * round(prevalence, digits = 3)))
  )
  
}



extract_axes_ranges <- function(performance_data_ready, curve,
                                min_p_threshold,
                                max_p_threshold){

  if (curve %in% c("lift", "decision", "interventions avoided")) {
    
    max_y_range <- max(performance_data_ready$y)
    
  }
  
  if (curve %in% c("decision", "interventions avoided")) {
    
    min_x_range <- min_p_threshold
    max_x_range <- max_p_threshold
    
    min_y_range <- min(c(performance_data_ready$y), 0)
    
  }
  
  
  
  if (curve == "roc") { curve_axis_range <- list(xaxis = c(0, 1), yaxis = c(0, 1))}
  if (curve == "lift") { curve_axis_range <- list(xaxis = c(0, 1), yaxis = c(0, max_y_range))}
  if (curve == "precision recall") { curve_axis_range <- list(xaxis = c(0, 1), yaxis = c(0, 1))}
  if (curve == "gains") { curve_axis_range <- list(xaxis = c(0, 1), yaxis = c(0, 1))}
  if (curve == "decision") { curve_axis_range <- list(xaxis = c(min_x_range, 
                                                          max_x_range), 
                                                yaxis = c(min_y_range, 
                                                          max_y_range))}
  if (curve == "interventions avoided") { curve_axis_range <- list(xaxis = c(min_x_range, 
                                                          max_x_range), 
                                                yaxis = c(min(0, min_y_range), 
                                                          100))}
  
  curve_axis_range |>  purrr::map(~extand_axis_range(.x))
  
}

extand_axis_range <- function(axis_range, extand_range_by = 1.1) {
  
  margin <- (extand_range_by - 1) * diff(axis_range)
  
  c(axis_range[1] - margin, axis_range[2] + margin)
  
}


return_treat_all_y_values <- function(prevalence) {
  
  c(prevalence - (1 - unique(prevalence)) * 
      (seq(0, 0.99, by = 0.01))  / (1 - seq(0, 0.99, by = 0.01) ))
  
}


