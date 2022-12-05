# Decision Curve ----------------------------------------------------------



#' Decision Curve
#'
#' Create a decision Curve
#'
#' @inheritParams create_roc_curve
#' @param type What type of Decision Curve, default choice is "conventional". 
#' Alternatives are "interventions avoided" and "combined" for both 
#' "conventional" and "interventions avoided" on the same view. 
#' @param min_p_threshold The minimum Probability Threshold value to be 
#' displayed
#' @param max_p_threshold The maximum Probability Threshold value to be 
#' displayed 
#'
#' @export
#'
#' @examples
#' 
#' \dontrun{
#' 
#' 
#' create_decision_curve(
#'   probs = list(example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome)
#' )
#' 
#' create_decision_curve(
#'   probs = list(example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome,
#'   type = "interventions avoided")
#' )
#' 
#' create_decision_curve(
#'   probs = list(example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome,
#'   type = "combined")
#' )
#'
#' create_decision_curve(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   reals = list(example_dat$outcome)
#' )
#'
#' create_decision_curve(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   reals = list(example_dat$outcome),
#'   type = "interventions avoided"
#' )
#'
#' create_decision_curve(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   reals = list(example_dat$outcome),
#'   type = "combined"
#' )
#'
#' create_decision_curve(
#'   probs = list(
#'     "train" = example_dat %>%
#'       dplyr::filter(type_of_set == "train") %>%
#'       dplyr::pull(estimated_probabilities),
#'     "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'       dplyr::pull(estimated_probabilities)
#'   ),
#'   reals = list(
#'     "train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
#'       dplyr::pull(outcome),
#'     "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'       dplyr::pull(outcome)
#'   )
#' )
#' 
#' create_decision_curve(
#'   probs = list(
#'     "train" = example_dat %>%
#'       dplyr::filter(type_of_set == "train") %>%
#'       dplyr::pull(estimated_probabilities),
#'     "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'       dplyr::pull(estimated_probabilities)
#'   ),
#'   reals = list(
#'     "train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
#'       dplyr::pull(outcome),
#'     "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'       dplyr::pull(outcome)
#'   ),
#'   type = "interventions avoided"
#' )
#' 
#'
#' create_decision_curve(
#'   probs = list(
#'     "train" = example_dat %>%
#'       dplyr::filter(type_of_set == "train") %>%
#'       dplyr::pull(estimated_probabilities),
#'     "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'       dplyr::pull(estimated_probabilities)
#'   ),
#'   reals = list(
#'     "train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
#'       dplyr::pull(outcome),
#'     "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'       dplyr::pull(outcome)
#'   ),
#'   type = "combined"
#' )
#'
#' }
create_decision_curve <- function(probs, reals, by = 0.01,
                                  stratified_by = "probability_threshold",
                                  chosen_threshold = NA,
                                  interactive = TRUE,
                                  col_values = c("#1b9e77", "#d95f02", 
                                                 "#7570b3", "#e7298a", 
                                                 "#07004D", "#E6AB02", 
                                                 "#FE5F55", "#54494B", 
                                                 "#006E90" , "#BC96E6",
                                                 "#52050A", "#1F271B", 
                                                 "#BE7C4D", "#63768D", 
                                                 "#08A045", "#320A28", 
                                                 "#82FF9E", "#2176FF", 
                                                 "#D1603D", "#585123"),
                                  size = NULL,
                                  type = "conventional",
                                  min_p_threshold = 0,
                                  max_p_threshold = 1) {
  
  match.arg(arg = type,
            choices = c("conventional", "interventions avoided", "combined"))
  
  if (!is.na(chosen_threshold)) {
    check_chosen_threshold_input(chosen_threshold)
  }

  prepare_performance_data(
    probs = probs,
    reals = reals,
    by = by,
    stratified_by = stratified_by
  ) %>%
    plot_decision_curve(
      chosen_threshold = chosen_threshold,
      interactive = interactive,
      col_values = col_values,
      size = size,
      type = type,
      min_p_threshold = min_p_threshold,
      max_p_threshold = max_p_threshold
    )
}



#' Decision Curve from Performance Data
#'
#' Plot a Precision decision Curve
#'
#' @inheritParams plot_roc_curve
#' @inheritParams create_decision_curve
#'
#' @examples
#' \dontrun{
#' 
#' 
#' one_pop_one_model %>%
#'   plot_decision_curve()
#'   
#' one_pop_one_model %>%
#'   plot_decision_curve(type = "interventions avoided")
#'   
#' one_pop_one_model %>%
#'   plot_decision_curve(type = "combined")
#'
#' multiple_models %>%
#'   plot_decision_curve()
#'   
#' multiple_models %>%
#'   plot_decision_curve(type = "interventions avoided")
#'   
#' multiple_models %>%
#'   plot_decision_curve(type = "combined")
#'
#' multiple_populations %>%
#'   plot_decision_curve()
#'   
#' multiple_populations %>%
#'   plot_decision_curve(type = "interventions avoided")
#'   
#' multiple_populations %>%
#'   plot_decision_curve(type = "combined")
#'   
#' }
#' @export

plot_decision_curve <- function(performance_data,
                                chosen_threshold = NA,
                                interactive = TRUE,
                                col_values = c("#1b9e77", "#d95f02", 
                                               "#7570b3", "#e7298a", 
                                               "#07004D", "#E6AB02", 
                                               "#FE5F55", "#54494B", 
                                               "#006E90" , "#BC96E6",
                                               "#52050A", "#1F271B", 
                                               "#BE7C4D", "#63768D", 
                                               "#08A045", "#320A28", 
                                               "#82FF9E", "#2176FF", 
                                               "#D1603D", "#585123"),
                                size = NULL,
                                type = "conventional",
                                min_p_threshold = 0,
                                max_p_threshold = 1) {
  
  
  if (!is.na(chosen_threshold)) {
    check_chosen_threshold_input(chosen_threshold)
  }
  
  if (interactive == FALSE) {
    
    decision_curve <- performance_data %>%
      create_ggplot_for_performance_metrics("threshold", "NB", col_values) %>%
      add_reference_lines_to_ggplot(
        create_reference_lines_data_frame("decision", prevalence)
      ) %>%
      set_decision_curve_limits() +
      ggplot2::xlab("Probability Threshold") +
      ggplot2::ylab("Net Benefit")
  }
  if (interactive == TRUE) {
    
    if (type == "conventional") {
    
      decision_curve <- performance_data |> 
        create_rtichoke_curve_list(
          "decision", size = size, col_values = col_values,
          min_p_threshold = min_p_threshold,
          max_p_threshold = max_p_threshold) |>
        create_plotly_curve()
    
    }
    
    if (type == "interventions avoided") {
      
      decision_curve <- decision_curve <- performance_data |> 
        create_rtichoke_curve_list(
          "interventions avoided", 
          size = size, col_values = col_values,
          min_p_threshold = min_p_threshold,
          max_p_threshold = max_p_threshold) |>
        create_plotly_curve()
      
    }
    
    if (type == "combined") {
      
      decision_curve <- create_rtichoke_combined_decision_curve_list(
        performance_data = performance_data, 
        size = size, 
        col_values = col_values,
        min_p_threshold = min_p_threshold,
        max_p_threshold = max_p_threshold) 
        
    }
    
  }
  
  return(decision_curve)
  
}

plot_decision_combined_curve <- function(rtichoke_decision_combined_curve_list){
  
  
  interactive_marker <- list(
    size = 12,
    line = list(
      width = 3,
      color = I("black")
    )
  )
  
  if ( ! (rtichoke_decision_combined_curve_list$perf_dat_type %in% 
          c("several models", "several populations")) ) {
    
    interactive_marker$color <- "#f6e3be"
    
  }
  
  combined_decision <- rtichoke_decision_combined_curve_list$performance_data_ready_for_curve$conventional |> 
    dplyr::inner_join(
      rtichoke_decision_combined_curve_list$performance_data_ready_for_curve$`interventions avoided`,
      by = c("reference_group", "stratified_by"),
      suffix = c("_conventional", "_interventions_avoided")
    )
  
  size_height <- switch(is.null(rtichoke_decision_combined_curve_list$size) +1, 
                        1.25*rtichoke_decision_combined_curve_list$size + 50, NULL)
  
  
  conventional_decision_curve <- plotly::plot_ly(
    height = size_height,
    width = rtichoke_decision_combined_curve_list$size
  ) |>
    plotly::add_lines(
      data = rtichoke_decision_combined_curve_list$reference_data$conventional,
      x = ~x,
      y = ~y,
      text = ~text,
      line = list(
        dash = "dot"
      ),
      hoverinfo = "text",
      color = ~reference_group,
      colors = unlist(rtichoke_decision_combined_curve_list$group_colors_vec)
    ) |>
    plotly::add_trace(
      data = combined_decision,
      x = ~x_conventional,
      y = ~ y_conventional,
      text =~ text_conventional,
      hoverinfo = "text",
      type = 'scatter',
      mode = 'lines+markers',
      line = list(dash = 'solid'),
      color =~ reference_group,
      colors = unlist(rtichoke_decision_combined_curve_list$group_colors_vec)
    ) |>
    plotly::add_markers(
      data = combined_decision,
      x = ~x_conventional,
      y = ~ y_conventional,
      text =~ text_conventional,
      hoverinfo = "text",
      type = 'scatter',
      color =~ reference_group,
      colors = unlist(rtichoke_decision_combined_curve_list$group_colors_vec),
      frame =~ stratified_by,
      marker = interactive_marker
    ) |>
    plotly::layout(
      xaxis = list(showgrid = FALSE, fixedrange = TRUE,
                   range = rtichoke_decision_combined_curve_list$axes_ranges$conventional$xaxis, 
                   title = rtichoke_decision_combined_curve_list$axes_labels$conventional$xaxis),
      yaxis = list(showgrid = FALSE, fixedrange = TRUE,
                   range = rtichoke_decision_combined_curve_list$axes_ranges$conventional$yaxis)
    )
  
  interventions_avoided_curve <- plotly::plot_ly(
    height = size_height,
    width = rtichoke_decision_combined_curve_list$size
  ) |>
    plotly::add_lines(
      data = rtichoke_decision_combined_curve_list$reference_data$`interventions avoided`,
      x = ~x,
      y = ~y,
      text = ~text,
      line = list(
        dash = "dot"
      ),
      hoverinfo = "text",
      color = ~reference_group,
      colors = unlist(rtichoke_decision_combined_curve_list$group_colors_vec)
    ) |>
    plotly::add_trace(
      x = ~x_interventions_avoided ,
      y = ~y_interventions_avoided,
      data = combined_decision,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(dash = 'solid'),
      color = ~reference_group,
      colors = unlist(rtichoke_decision_combined_curve_list$group_colors_vec),
      text = ~text_interventions_avoided,
      hoverinfo = "text"
    ) |>
    plotly::add_markers(
      data = combined_decision,
      x = ~x_interventions_avoided,
      y = ~y_interventions_avoided,
      frame =~ stratified_by,
      color = ~reference_group,
      colors = unlist(rtichoke_decision_combined_curve_list$group_colors_vec),
      marker = interactive_marker,
      text = ~text_interventions_avoided,
      hoverinfo = "text"
    ) |>
    plotly::layout(
      xaxis = list(showgrid = FALSE, fixedrange = TRUE,
                   range = rtichoke_decision_combined_curve_list$axes_ranges$`interventions avoided`$xaxis),
      yaxis = list(showgrid = FALSE, fixedrange = TRUE,
                   range = rtichoke_decision_combined_curve_list$axes_ranges$`interventions avoided`$yaxis)
    )
  
  interventions_avoided_annotation <- list(
    text = "Interventions Avoided (per 100)",
    font = list(
      size = 18,
      color = "black"),
    xref = "paper",
    yref = "paper",
    yanchor = "bottom",
    xanchor = "center",
    align = "center",
    x = 0.5,
    y = 1,
    showarrow = FALSE
  )
  
  conventional_decision_annotation <- list(
    text = "Net Benefit",
    font = list(
      size = 18,
      color = "black"),
    xref = "paper",
    yref = "paper",
    yanchor = "bottom",
    xanchor = "center",
    align = "center",
    x = 0.5,
    y = 1,
    showarrow = FALSE
  )
  
  plotly::subplot(
    interventions_avoided_curve |> plotly::layout(annotations = interventions_avoided_annotation), 
    conventional_decision_curve |> plotly::layout(annotations = conventional_decision_annotation),
    nrows = 2,
    shareX = TRUE,
    shareY = FALSE, 
    heights = c(0.5, 0.5)
  ) |>
    plotly::config(displayModeBar = FALSE) |>  
    plotly::animation_button(visible=FALSE) |>
    plotly::animation_slider(
      currentvalue = list(
        prefix = rtichoke_decision_combined_curve_list$animation_slider_prefix,
        font = list(color = "black"),
        xanchor = "left"
      ),
      pad = list(t = 50)
    ) |> 
    plotly::layout(
      showlegend = FALSE
    )
  
  
}




check_equality_between_decision_curve_lists <- function(
    decision_conventional_list, 
    decision_interventions_avoided_list) {
  
  stopifnot(
    
    all.equal(
      decision_conventional_list[c("size", "animation_slider_prefix",
                                   "perf_dat_type", "group_colors_vec")],
      decision_interventions_avoided_list[c("size", "animation_slider_prefix",
                                            "perf_dat_type", "group_colors_vec")]
    )
    
  )
  
}

unify_decision_curve_lists_for_combined_decision_curve_list <- function(
    rtichoke_decision_curve_lists) {
  
  check_equality_between_decision_curve_lists(
    rtichoke_decision_curve_lists$conventional,  
    rtichoke_decision_curve_lists$`interventions avoided`
  )
  
  
  
  rtichoke_decision_combined_curve_list <- rtichoke_decision_curve_lists$conventional[
    c("animation_slider_prefix", "perf_dat_type", "group_colors_vec")]
  
  rtichoke_decision_combined_curve_list$size <- rtichoke_decision_curve_lists$conventional$size
  
  rtichoke_decision_combined_curve_list$reference_data <- list(
    "conventional" = rtichoke_decision_curve_lists$conventional$reference_data,
    "interventions avoided" = rtichoke_decision_curve_lists$`interventions avoided`$reference_data
  )
  
  rtichoke_decision_combined_curve_list$axes_labels <- list(
    "conventional" = rtichoke_decision_curve_lists$conventional$axes_labels,
    "interventions avoided" = rtichoke_decision_curve_lists$`interventions avoided`$axes_labels
  )
  
  rtichoke_decision_combined_curve_list$performance_data_ready_for_curve <- list(
    "conventional" = rtichoke_decision_curve_lists$conventional$performance_data_ready_for_curve,
    "interventions avoided" = rtichoke_decision_curve_lists$`interventions avoided`$performance_data_ready_for_curve
  )
  
  rtichoke_decision_combined_curve_list$axes_ranges <- list(
    "conventional" = rtichoke_decision_curve_lists$conventional$axes_ranges,
    "interventions avoided" = rtichoke_decision_curve_lists$`interventions avoided`$axes_ranges
  )
  
  rtichoke_decision_combined_curve_list
  
}


create_rtichoke_combined_decision_curve_list <- function(
    performance_data,
    curve, 
    min_p_threshold = 0.01, 
    max_p_threshold = 0.99, 
    size = NULL, 
    col_values = c(
      "#1b9e77", "#d95f02",
      "#7570b3", "#e7298a",
      "#07004D", "#E6AB02",
      "#FE5F55", "#54494B",
      "#006E90", "#BC96E6",
      "#52050A", "#1F271B",
      "#BE7C4D", "#63768D",
      "#08A045", "#320A28",
      "#82FF9E", "#2176FF",
      "#D1603D", "#585123")) {
  
  rtichoke_decision_curve_lists <- c("decision", "interventions avoided") |>
    purrr::map(~create_rtichoke_curve_list(performance_data , 
                                           curve = .x, 
                                           min_p_threshold = min_p_threshold, 
                                           max_p_threshold = max_p_threshold, 
                                           size = size, 
                                           col_values = col_values)) |> 
    purrr::set_names("conventional", "interventions avoided")
  
  
  rtichoke_decision_curve_lists |> 
    unify_decision_curve_lists_for_combined_decision_curve_list() |> 
    plot_decision_combined_curve()                         
  
}


#' Set the limits for Decision Curve
#'
#' @param decision_curve a ggplot object of Decision Curve
#' @keywords internal
set_decision_curve_limits <- function(decision_curve) {
  decision_curve +
    ggplot2::xlim(0, 1)
}


#' Decision Curve from Performance Data (Interventions Avoided)
#'
#' Plot interventions Avoided
#'
#' @inheritParams plot_roc_curve
#' @inheritParams create_decision_curve
#'
#' @examples
#'
#' \dontrun{
#'
#' one_pop_one_model %>%
#'   rtichoke:::plot_interventions_avoided()
#'
#' multiple_models %>%
#'   rtichoke:::plot_interventions_avoided()
#'
#' multiple_populations %>%
#'   rtichoke:::plot_interventions_avoided()
#'   
#' }
#' @keywords internal
plot_interventions_avoided <- function(performance_data,
                                       chosen_threshold = NA,
                                       interactive = TRUE,
                                       col_values = c(
                                         "#5BC0BE",
                                         "#FC8D62",
                                         "#8DA0CB",
                                         "#E78AC3",
                                         "#A4243B"
                                       ),
                                       size = NULL,
                                       min_p_threshold = 0,
                                       max_p_threshold = 1){
  
  if (!is.na(chosen_threshold)) {
    check_chosen_threshold_input(chosen_threshold)
  }
  
  stratified_by <- check_performance_data_stratification(
    performance_data
  )
  
  perf_dat_type <- check_performance_data_type_for_plotly(
    performance_data = performance_data
  )
  prevalence <- get_prevalence_from_performance_data(
    performance_data, perf_dat_type
  )
  
  performance_data <- performance_data  %>% 
    dplyr::mutate(
      N = TP +TN  + FP + FN,
      prevalence = (TP + FN) / N,
      NB_intervention_all =   prevalence - (1- prevalence) * 
        (probability_threshold) / (1 - probability_threshold),
      NB_treatment_avoided = 100 * (NB - NB_intervention_all) * 
        ( (1 - probability_threshold) / probability_threshold )
    ) %>% 
    add_hover_text_to_performance_data(perf_dat_type, 
                                       curve = "interventions avoided",
                                       stratified_by = stratified_by)  %>%
    dplyr::filter(probability_threshold > 0 & 
                    probability_threshold < 1) %>%
    dplyr::filter(probability_threshold >= min_p_threshold & 
                    probability_threshold <= max_p_threshold)
  
  
  if (perf_dat_type %in% c("one model with model column", "one model")) {

    
    interventions_avoided <- plotly::plot_ly(
      height = size,
      width = size
    ) %>%
      add_lines_and_markers_from_performance_data(
        performance_data = performance_data,
        performance_data_type = perf_dat_type,
        probability_threshold,
        NB_treatment_avoided
      ) %>%
      add_interactive_marker_from_performance_data(
        performance_data = performance_data,
        performance_data_type = perf_dat_type,
        probability_threshold,
        NB_treatment_avoided,
        stratified_by = stratified_by
      ) %>%
      set_styling_for_rtichoke(
        "interventions avoided",
        min_y_range = 
          min(-10, min(performance_data$NB_treatment_avoided) - 10),
        max_y_range = 
          max(performance_data$NB_treatment_avoided) + 10,
        max_x_range = max_p_threshold + 
          diff(c(min_p_threshold, max_p_threshold), na.rm = TRUE) * 0.1,
        min_x_range = min_p_threshold - 
          diff(c(min_p_threshold, max_p_threshold), na.rm = TRUE) * 0.1)
  }
  
  if (perf_dat_type == "several models") {
    interventions_avoided <- plotly::plot_ly(
      height = size,
      width = size
    ) %>%
      add_lines_and_markers_from_performance_data(
        performance_data = performance_data,
        performance_data_type = perf_dat_type,
        probability_threshold,
        NB_treatment_avoided, 
        col_values = col_values
      )  %>%
      add_interactive_marker_from_performance_data(
        performance_data = performance_data,
        performance_data_type = perf_dat_type,
        probability_threshold,
        NB_treatment_avoided
      ) %>%
      set_styling_for_rtichoke(
        "interventions avoided",
        min_y_range = 
          min(-10, min(performance_data$NB_treatment_avoided) - 10),
        max_y_range = 
          max(performance_data$NB_treatment_avoided) + 10,
        max_x_range = max_p_threshold + 
          diff(c(min_p_threshold, max_p_threshold), na.rm = TRUE) * 0.1,
        min_x_range = min_p_threshold - 
          diff(c(min_p_threshold, max_p_threshold), na.rm = TRUE) * 0.1)
  }
  
  if (perf_dat_type == "several populations") {
    
    interventions_avoided <- plotly::plot_ly(
      height = size,
      width = size
    ) %>%
      add_lines_and_markers_from_performance_data(
        performance_data = performance_data,
        performance_data_type = perf_dat_type,
        probability_threshold,
        NB_treatment_avoided,
        col_values = col_values
      )  %>%
      add_interactive_marker_from_performance_data(
        performance_data = performance_data,
        performance_data_type = perf_dat_type,
        probability_threshold,
        NB_treatment_avoided
      )  %>%
      set_styling_for_rtichoke(
        "interventions avoided",
        min_y_range = 
          min(-10, min(performance_data$NB_treatment_avoided) - 10),
        max_y_range = 
          max(performance_data$NB_treatment_avoided) + 10,
        max_x_range = max_p_threshold + 
          diff(c(min_p_threshold, max_p_threshold), na.rm = TRUE) * 0.1,
        min_x_range = min_p_threshold - 
          diff(c(min_p_threshold, max_p_threshold), na.rm = TRUE) * 0.1)
  }

  
  return(interventions_avoided)
  
}




#' Decision Curve from Performance Data (Conventional)
#'
#' Plot Conventional Decision Curve
#'
#' @inheritParams plot_roc_curve
#' @inheritParams create_decision_curve
#'
#' @examples
#'
#' \dontrun{
#'
#' one_pop_one_model %>%
#'   rtichoke:::plot_conventional_decision()
#'
#' multiple_models %>%
#'   rtichoke:::plot_conventional_decision()
#'
#' multiple_populations %>%
#'   rtichoke:::plot_conventional_decision()
#'   
#' }
#' @keywords internal
plot_conventional_decision <- function(performance_data,
                                       chosen_threshold = NA,
                                       interactive = TRUE,
                                       col_values = c("#1b9e77", "#d95f02", 
                                                      "#7570b3", "#e7298a", 
                                                      "#07004D", "#E6AB02", 
                                                      "#FE5F55", "#54494B", 
                                                      "#006E90" , "#BC96E6",
                                                      "#52050A", "#1F271B", 
                                                      "#BE7C4D", "#63768D", 
                                                      "#08A045", "#320A28", 
                                                      "#82FF9E", "#2176FF", 
                                                      "#D1603D", "#585123"),
                                       size = NULL,
                                       min_p_threshold = 0,
                                       max_p_threshold = 1){
  
  
  stratified_by <- check_performance_data_stratification(
    performance_data
  )
  
  perf_dat_type <- check_performance_data_type_for_plotly(
    performance_data = performance_data
  )
  prevalence <- get_prevalence_from_performance_data(
    performance_data, perf_dat_type
  )
  
  performance_data <- performance_data %>%
    add_hover_text_to_performance_data(perf_dat_type, 
                                       curve = "decision",
                                       stratified_by = stratified_by) %>%
    dplyr::filter(probability_threshold > 0 & 
                    probability_threshold < 1) %>%
    dplyr::filter(probability_threshold >= min_p_threshold & 
                    probability_threshold <= max_p_threshold)
  
  
  
  if (perf_dat_type %in% c("one model with model column", "one model")) {
    
    
    conventional_decision <- create_reference_lines_for_plotly(
      perf_dat_type,
      "decision",
      prevalence = prevalence,
      size = size,
      performance_data = performance_data
    ) %>%
      add_lines_and_markers_from_performance_data(
        performance_data = performance_data,
        performance_data_type = perf_dat_type,
        probability_threshold,
        NB
      ) %>%
      add_interactive_marker_from_performance_data(
        performance_data = performance_data,
        performance_data_type = perf_dat_type,
        probability_threshold,
        NB,
        stratified_by = stratified_by
      ) %>%
      set_styling_for_rtichoke(
        "decision",
        max_y_range = max(performance_data$NB,
                          na.rm = TRUE) + 
          diff(range(performance_data$NB, na.rm = TRUE)) * 0.25,
        min_y_range = 
          min(min(performance_data$NB[performance_data$NB != -Inf],
                  na.rm = TRUE), 0) - 
          diff(range(performance_data$NB, na.rm = TRUE)) * 0.25,
        max_x_range = max_p_threshold + 
          diff(c(min_p_threshold, max_p_threshold), na.rm = TRUE) * 0.1,
        min_x_range = min_p_threshold - 
          diff(c(min_p_threshold, max_p_threshold), na.rm = TRUE) * 0.1)
    
  }
  
  if (perf_dat_type == "several models") {
    
    conventional_decision <- create_reference_lines_for_plotly(
      perf_dat_type,
      "decision",
      prevalence = prevalence[1],
      population_color_vector = col_values[seq_len(length(prevalence))],
      size = size,
      performance_data = performance_data
    ) %>%
      add_lines_and_markers_from_performance_data(
        performance_data = performance_data,
        performance_data_type = perf_dat_type,
        probability_threshold,
        NB,
        col_values = col_values
      ) %>%
      add_interactive_marker_from_performance_data(
        performance_data = performance_data,
        performance_data_type = perf_dat_type,
        probability_threshold,
        NB,
        stratified_by = stratified_by
      ) %>%
      set_styling_for_rtichoke(
        "decision",
        max_y_range = max(performance_data$NB,
                          na.rm = TRUE) + 
          diff(range(performance_data$NB, na.rm = TRUE)) * 0.25,
        min_y_range = 
          min(min(performance_data$NB[performance_data$NB != -Inf],
                          na.rm = TRUE), 0) - 
          diff(range(performance_data$NB, na.rm = TRUE)) * 0.25,
        max_x_range = max_p_threshold + 
          diff(c(min_p_threshold, max_p_threshold), na.rm = TRUE) * 0.1,
        min_x_range = min_p_threshold - 
          diff(c(min_p_threshold, max_p_threshold), na.rm = TRUE) * 0.1)
    
  }
  
  if (perf_dat_type == "several populations") {
    
    conventional_decision <- create_reference_lines_for_plotly(perf_dat_type,
                                                        "decision",
                                                        prevalence = prevalence,
                                                        population_color_vector =
                                                          col_values[seq_len(length(prevalence))],
                                                        size = size,
                                                        performance_data = performance_data
    ) %>%
      add_lines_and_markers_from_performance_data(
        performance_data = performance_data,
        performance_data_type = perf_dat_type,
        probability_threshold,
        NB
      ) %>%
      add_interactive_marker_from_performance_data(
        performance_data = performance_data,
        performance_data_type = perf_dat_type,
        probability_threshold,
        NB,
        stratified_by = stratified_by
      ) %>%
      set_styling_for_rtichoke(
        "decision",
        max_y_range = max(performance_data$NB,
                          na.rm = TRUE) + 
          diff(range(performance_data$NB, na.rm = TRUE)) * 0.25,
        min_y_range = 
          min(min(performance_data$NB[performance_data$NB != -Inf],
                  na.rm = TRUE), 0) - 
          diff(range(performance_data$NB, na.rm = TRUE)) * 0.25,
        max_x_range = max_p_threshold + 
          diff(c(min_p_threshold, max_p_threshold), na.rm = TRUE) * 0.1,
        min_x_range = min_p_threshold - 
          diff(c(min_p_threshold, max_p_threshold), na.rm = TRUE) * 0.1)
  }
  
  
  return(conventional_decision)
  
}