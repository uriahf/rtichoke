#' Create ggplot for Performance Metrics
#'
#' Makes a ggplot for the metrices
#'
#' @param performance_table an rtichoke performance table
#' @param x_perf_metric a performance metrice for the x axis
#' @param y_perf_metric a performance metrice for the y axis
#' @param col_values color palette
#'


create_ggplot_for_performance_metrics <- function(performance_table,
                                                  x_perf_metric,
                                                  y_perf_metric,
                                                  col_values = c("#5E7F9A", 
                                                                 "#931B53", 
                                                                 "#F7DC2E", 
                                                                 "#C6C174", 
                                                                 "#75DBCD")) {
  
  if (!(names(performance_table)[1] %in% c("population", "model"))) {
    col_values_vec <- "black"
    
    ggplot_for_performance_metrics <- ggplot2::ggplot(
      performance_table,
      ggplot2::aes_string(
        x = x_perf_metric,
        y = y_perf_metric
      )
    )
    
  } else {
    
    col_values_vec <- col_values[1:length(unique(performance_table[, 1]))]

    if (length(unique(performance_table[, 1])) == 1) {
      col_values_vec <- "black"
    }
    
    if (length(unique(performance_table[, 1])) > 1) {
      names(col_values_vec) <- unique(performance_table[, 1])
    }
    
    ggplot_for_performance_metrics <- ggplot2::ggplot(
      performance_table,
      ggplot2::aes_string(
        x = x_perf_metric,
        y = y_perf_metric,
        group = names(performance_table)[1],
        color = names(performance_table)[1]
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

create_plotly_for_performance_metrics <- function(performance_table,
                                                  x_perf_metric,
                                                  y_perf_metric,
                                                  col_values = c("#5E7F9A", 
                                                                 "#931B53", 
                                                                 "#F7DC2E", 
                                                                 "#C6C174", 
                                                                 "#75DBCD"),
                                                  main_slider = "threshold",
                                                  reference_lines = NA) {
    
  x_perf_metric <- enquo(x_perf_metric)
  y_perf_metric <- enquo(y_perf_metric)
  
  
  performance_table_type <- check_performance_table_type_for_plotly(performance_table)
  
  if(performance_table_type %in% c("one model", "one model with model column")){
    col_values_vec <- "black"
  } else {
    col_values_vec <- col_values[1:length(unique(performance_table[, 1]))]
    names(col_values_vec) <- unique(performance_table[, 1])
  }
  
  print(performance_table_type)
  print(col_values_vec)
  
  plotly_for_performance_metrics <- performance_table %>%
    create_plotly_base(x_perf_metric,
                       y_perf_metric,
                       performance_table_type = performance_table_type,
                       col_values = col_values_vec) 
  
  if(is.data.frame(reference_lines) ){
    plotly_for_performance_metrics <- plotly_for_performance_metrics %>%
      add_reference_lines_to_plotly(reference_lines, performance_table_type)
  }
  
  
  plotly_for_performance_metrics <- plotly_for_performance_metrics %>%
    add_markers_and_lines_to_plotly(performance_table_type = performance_table_type) %>%
    add_interactive_marker_to_plotly() %>%
    remove_grid_lines_from_plotly()
  
  plotly_for_performance_metrics
}




#' remove_grid_lines_from_plotly
#'
#' @param plotly_object a plotly plot for performance metrics 
remove_grid_lines_from_plotly <- function(plotly_object){
  plotly_object %>%
    plotly::layout(xaxis = list(showgrid = F),
           yaxis = list(showgrid = F))
}

# ROC Curve ---------------------------------------------------------------


#' ROC Curve
#' 
#' Create a ROC Curve
#'
#' @inheritParams create_performance_table
#'
#' @export
#' 
#'
create_roc_curve <- function(probs, real, by = 0.01, 
                             enforce_percentiles_symmetry = F){
    create_performance_table(probs = probs, 
                             real = real,
                             by = by, 
                             enforce_percentiles_symmetry = enforce_percentiles_symmetry) %>%
        plot_roc_curve()
}


#' ROC Curve from Performance Table
#'
#' Plot a ROC Curve
#'
#' @param performance_table an rtichoke performance table
#' @param chosen_threshold a chosen threshold to display
#' @param interactive whether the plot should be interactive
#' @param main_slider what is the main slider - threshold, percent positives or positives
#'
#' 
#' 
#' @examples
#'
#' one_pop_one_model_as_a_vector %>%
#'   plot_roc_curve()
#'
#' one_pop_one_model_as_a_vector_enforced_percentiles_symmetry %>%
#'   plot_roc_curve()
#'
#' one_pop_one_model_as_a_list %>%
#'   plot_roc_curve()
#'
#' one_pop_one_model_as_a_list_enforced_percentiles_symmetry %>%
#'   plot_roc_curve()
#'
#' one_pop_three_models %>%
#'   plot_roc_curve()
#'
#' one_pop_three_models_enforced_percentiles_symmetry %>%
#'   plot_roc_curve()
#'
#' train_and_test_sets %>%
#'   plot_roc_curve()
#'
#' train_and_test_sets_enforced_percentiles_symmetry %>%
#'   plot_roc_curve()
#'
#'\dontrun{
#'
#' one_pop_one_model_as_a_vector %>%
#'   plot_roc_curve(interactive = T)
#'
#' one_pop_one_model_as_a_vector_enforced_percentiles_symmetry %>%
#'   plot_roc_curve(interactive = T, main_slider = "predicted_positives_percent")
#'
#' one_pop_one_model_as_a_list %>%
#'   plot_roc_curve(interactive = T)
#'
#' one_pop_one_model_as_a_list_enforced_percentiles_symmetry %>%
#'   plot_roc_curve(interactive = T, main_slider = "predicted_positives_percent")
#'
#' one_pop_three_models %>%
#'   plot_roc_curve(interactive = T)
#'
#' one_pop_three_models_enforced_percentiles_symmetry %>%
#'   plot_roc_curve(interactive = T, main_slider = "predicted_positives_percent")
#'
#' train_and_test_sets %>%
#'   plot_roc_curve(interactive = T)
#'
#' train_and_test_sets_enforced_percentiles_symmetry %>%
#'   plot_roc_curve(interactive = T, main_slider = "predicted_positives_percent")
#'   }
#'   
#' @export
  

plot_roc_curve <- function(performance_table,
                           chosen_threshold = NA,
                           interactive = F,
                           main_slider = "threshold") {
  
  reference_lines <- create_reference_lines_data_frame("roc")
  
  if (interactive == F) {
    roc_curve <- performance_table %>%
      create_ggplot_for_performance_metrics("FPR", "sensitivity") %>%
      add_reference_lines_to_ggplot(reference_lines)
  }
  
  if (interactive == T) {
    roc_curve <- performance_table %>%
      create_plotly_for_performance_metrics(FPR, 
                                            sensitivity,
                                            reference_lines = reference_lines) %>%
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
  }
    
  return(roc_curve)
}



# Lift Curve --------------------------------------------------------------



#' Lift Curve
#' 
#' Create a Lift Curve
#'
#' @inheritParams create_roc_curve
#'
#' @export
#'
create_lift_curve <- function(probs, real, by = 0.01, 
                             enforce_percentiles_symmetry = F){
    create_performance_table(probs = probs, 
                             real = real,
                             by = by, 
                             enforce_percentiles_symmetry = enforce_percentiles_symmetry) %>%
        plot_lift_curve()
}


#' Lift Curve from Performance Table
#'
#' Plot a Lift Curve
#'
#' @inheritParams plot_roc_curve
#' 
#' @examples
#'
#' one_pop_one_model_as_a_vector %>%
#'   plot_lift_curve()
#'
#' one_pop_one_model_as_a_vector_enforced_percentiles_symmetry %>%
#'   plot_lift_curve()
#'
#' one_pop_one_model_as_a_list %>%
#'   plot_lift_curve()
#'
#' one_pop_one_model_as_a_list_enforced_percentiles_symmetry %>%
#'   plot_lift_curve()
#'
#' one_pop_three_models %>%
#'   plot_lift_curve()
#'
#' one_pop_three_models_enforced_percentiles_symmetry %>%
#'   plot_lift_curve()
#'
#' train_and_test_sets %>%
#'   plot_lift_curve()
#'
#' train_and_test_sets_enforced_percentiles_symmetry %>%
#'   plot_lift_curve()
#'
#'
#'\dontrun{
#' one_pop_one_model_as_a_vector %>%
#'   plot_lift_curve(interactive = T)
#'
#' one_pop_one_model_as_a_vector_enforced_percentiles_symmetry %>%
#'   plot_lift_curve(interactive = T, main_slider = "predicted_positives_percent")
#'
#' one_pop_one_model_as_a_list %>%
#'   plot_lift_curve(interactive = T)
#'
#' one_pop_one_model_as_a_list_enforced_percentiles_symmetry %>%
#'   plot_lift_curve(interactive = T, main_slider = "predicted_positives_percent")
#'
#' one_pop_three_models %>%
#'   plot_lift_curve(interactive = T)
#'
#' one_pop_three_models_enforced_percentiles_symmetry %>%
#'   plot_lift_curve(interactive = T, main_slider = "predicted_positives_percent")
#'
#' train_and_test_sets %>%
#'   plot_lift_curve(interactive = T)
#'
#' train_and_test_sets_enforced_percentiles_symmetry %>%
#'   plot_lift_curve(interactive = T, main_slider = "predicted_positives_percent")
#'   }
#' @export

plot_lift_curve <- function(performance_table,
                           chosen_threshold = NA,
                           interactive = F,
                           main_slider = "threshold") {
  
  reference_lines <- create_reference_lines_data_frame("lift", prevalence)

  
    if (interactive == F) {
        lift_curve <- performance_table %>%
            create_ggplot_for_performance_metrics("predicted_positives_percent","lift") %>%
            # add_lift_curve_reference_lines() %>%
            add_reference_lines_to_ggplot(reference_lines) %>%
            set_lift_limits()
    }
  
  if (interactive == T) {
    lift_curve <- performance_table %>%
      mutate(fake_dot = 0) %>%
      create_plotly_for_performance_metrics(predicted_positives_percent, lift,
                                            reference_lines = reference_lines) %>%
      plotly::add_markers(
        x = ~predicted_positives_percent,
        y = ~fake_dot,
        frame = as.formula(paste0("~", main_slider)),
        marker = list(
          color = "rgba(0,0,0,0)",
          size = 16,
          line = list(
            width = 4,
            color = "rgba(0,0,0,0)"
          )
        )
      ) %>%
      plotly::layout(
        xaxis = list(
          title = "Predicted Positives Percent",
          showgrid = F
        ),
        yaxis = list(
          title = "LIFT",
          showgrid = F
        ),
        showlegend = FALSE
      ) %>%
      plotly::config(displayModeBar = F)
  }
  
    return(lift_curve)
}


#' Set the limits for Lift Curve
#' 
#' @param lift_curve a ggplot object of a Lift Curve
#'
set_lift_limits <- function(lift_curve){
  lift_curve  +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, NA)
}




# Precision Recall Curve --------------------------------------------------


#' Precision Recall Curve
#' 
#' Create a Precision Recall Curve
#'
#' @inheritParams create_roc_curve
#'
#' @export
#'
create_precision_recall_curve <- function(probs, real, by = 0.01, 
                               enforce_percentiles_symmetry = F){
  create_performance_table(probs = probs, 
                           real = real,
                           by = by, 
                           enforce_percentiles_symmetry = enforce_percentiles_symmetry) %>%
    plot_precision_recall_curve()
}



#' Precision Recall Curve from Performance Table
#'
#' Plot a Precision Recall Curve
#'
#' @inheritParams plot_roc_curve
#' 
#' @examples
#'
#' one_pop_one_model_as_a_vector %>%
#'   plot_precision_recall_curve()
#'
#' one_pop_one_model_as_a_vector_enforced_percentiles_symmetry %>%
#'   plot_precision_recall_curve()
#'
#' one_pop_one_model_as_a_list %>%
#'   plot_precision_recall_curve()
#'
#' one_pop_one_model_as_a_list_enforced_percentiles_symmetry %>%
#'   plot_precision_recall_curve()
#'
#' one_pop_three_models %>%
#'   plot_precision_recall_curve()
#'
#' one_pop_three_models_enforced_percentiles_symmetry %>%
#'   plot_precision_recall_curve()
#'
#' train_and_test_sets %>%
#'   plot_precision_recall_curve()
#'
#' train_and_test_sets_enforced_percentiles_symmetry %>%
#'   plot_precision_recall_curve()
#'
#'\dontrun{
#'
#' one_pop_one_model_as_a_vector %>%
#'   plot_precision_recall_curve(interactive = T)
#'
#' one_pop_one_model_as_a_vector_enforced_percentiles_symmetry %>%
#'   plot_precision_recall_curve(interactive = T, main_slider = "predicted_positives_percent")
#'
#' one_pop_one_model_as_a_list %>%
#'   plot_precision_recall_curve(interactive = T)
#'
#' one_pop_one_model_as_a_list_enforced_percentiles_symmetry %>%
#'   plot_precision_recall_curve(interactive = T, main_slider = "predicted_positives_percent")
#'
#' one_pop_three_models %>%
#'   plot_precision_recall_curve(interactive = T)
#'
#' one_pop_three_models_enforced_percentiles_symmetry %>%
#'   plot_precision_recall_curve(interactive = T, main_slider = "predicted_positives_percent")
#'
#' train_and_test_sets %>%
#'   plot_precision_recall_curve(interactive = T)
#'
#' train_and_test_sets_enforced_percentiles_symmetry %>%
#'   plot_precision_recall_curve(interactive = T, main_slider = "predicted_positives_percent")
#'   }
#'   
#' @export

plot_precision_recall_curve <- function(performance_table,
                             chosen_threshold = NA,
                             interactive = F,
                             main_slider = "threshold") {
  
  performance_table_type <- check_performance_table_type_for_plotly(performance_table)
  prevalence <- get_prevalence_from_performance_table(performance_table, performance_table_type)
  reference_lines <- create_reference_lines_data_frame("precision recall", prevalence)
  
  if (interactive == F) {
    precision_recall_curve <- performance_table %>%
      create_ggplot_for_performance_metrics("sensitivity", "PPV") %>%
      add_reference_lines_to_ggplot(reference_lines) %>%
      set_precision_recall_curve_limits()
  }

  if (interactive == T) {
    # precision_recall_curve <- performance_table %>%
    #   create_plotly_for_performance_metrics(sensitivity, PPV,
    #                                         reference_lines = reference_lines) %>%
    #   plotly::layout(
    #     xaxis = list(
    #       title = "Sensitivity",
    #       showgrid = F
    #     ),
    #     yaxis = list(
    #       title = "PPV",
    #       showgrid = F
    #     ),
    #     showlegend = FALSE
    #   ) %>%
    #   plotly::config(displayModeBar = F)
    
    
    
    
  }
  return(precision_recall_curve)
}


#' Set the limits for percision recall curve
#' 
#' @param precision_recall_curve a ggplot object of precision recall curve
#'
set_precision_recall_curve_limits <- function(precision_recall_curve){
  precision_recall_curve  +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1)
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
                              enforce_percentiles_symmetry = F){
  create_performance_table(probs = probs, 
                           real = real,
                           by = by, 
                           enforce_percentiles_symmetry = enforce_percentiles_symmetry) %>%
    plot_gains_curve()
}


#' Gains Curve from Performance Table
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
#'
#'\dontrun{
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

plot_gains_curve <- function(performance_table,
                            chosen_threshold = NA,
                            interactive = F,
                            main_slider = "threshold") {
  
  prevalence <- get_prevalence_from_performance_table(performance_table)
  reference_lines <- create_reference_lines_data_frame("gains", prevalence)
  
  if (interactive == F) {
    gains_curve <- performance_table %>%
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
    purrr::map2(prevalence, c("#5E7F9A", 
                              "#931B53", 
                              "#F7DC2E", 
                              "#C6C174", 
                              "#75DBCD")[1:length(prevalence)] , 
                add_prevalence_layers_to_gains_curve) %>% unlist(),
    gains_curve$layers
  )
  gains_curve
}


#' Set the limits for Gains Curve
#' 
#' @param gains_curve a ggplot object of a Gains Curve
#'
set_gains_curve_limits <- function(gains_curve){
  gains_curve  +
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
                                          enforce_percentiles_symmetry = F){
  create_performance_table(probs = probs, 
                           real = real,
                           by = by, 
                           enforce_percentiles_symmetry = enforce_percentiles_symmetry) %>%
    plot_decision_curve()
}



#' Precision Recall Curve from Performance Table
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
#'
#'\dontrun{
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
#'   }
#' @export

plot_decision_curve <- function(performance_table,
                                        chosen_threshold = NA,
                                        interactive = F,
                                        main_slider = "threshold") {
  
  prevalence <- get_prevalence_from_performance_table(performance_table)
  
  if (interactive == F) {
    decision_curve <- performance_table %>%
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
set_decision_curve_limits <- function(decision_curve){
  decision_curve  +
    ggplot2::xlim(0, 1) 
}
