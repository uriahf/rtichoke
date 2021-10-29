# Performance Table ----------------------------------------------------------



#' Performance Table
#'
#' Create a Performance Table
#'
#' @inheritParams create_roc_curve
#'
#'
create_performance_table <- function(probs, real, by = 0.01,
                                     enforce_percentiles_symmetry = F) {
  prepare_performance_data(
    probs = probs,
    real = real,
    by = by,
    enforce_percentiles_symmetry = enforce_percentiles_symmetry
  ) %>%
    render_performance_table()
}


#' Performance Table
#'
#' Create a Performance Table
#'
#' @inheritParams plot_roc_curve
#'
#' @examples
#'
#' \dontrun{
#'
#' one_pop_one_model_as_a_vector %>%
#'   render_performance_table()
#'
#' one_pop_one_model_as_a_vector_enforced_percentiles_symmetry %>%
#'   render_performance_table()
#'
#' one_pop_one_model_as_a_list %>%
#'   render_performance_table()
#'
#' one_pop_one_model_as_a_list_enforced_percentiles_symmetry %>%
#'   render_performance_table()
#'
#' one_pop_three_models %>%
#'   render_performance_table()
#'
#' one_pop_three_models_enforced_percentiles_symmetry %>%
#'   render_performance_table()
#'
#' train_and_test_sets %>%
#'   render_performance_table()
#'
#' train_and_test_sets_enforced_percentiles_symmetry %>%
#'   render_performance_table()
#'
#' one_pop_one_model_as_a_vector %>%
#'   render_performance_table(interactive = T)
#'
#' one_pop_one_model_as_a_vector_enforced_percentiles_symmetry %>%
#'   render_performance_table(interactive = T)
#'
#' one_pop_one_model_as_a_list %>%
#'   render_performance_table(interactive = T)
#'
#' one_pop_one_model_as_a_list_enforced_percentiles_symmetry %>%
#'   render_performance_table(interactive = T)
#'
#' one_pop_three_models %>%
#'   render_performance_table(interactive = T)
#'
#' one_pop_three_models_enforced_percentiles_symmetry %>%
#'   render_performance_table(interactive = T)
#'
#' train_and_test_sets %>%
#'   render_performance_table(interactive = T)
#'
#' train_and_test_sets_enforced_percentiles_symmetry %>%
#'   render_performance_table(interactive = T)
#' }
#'
#' @export
render_performance_table <- function(performance_data,
                                     chosen_threshold = NA,
                                     interactive = F,
                                     main_slider = "threshold",
                                     col_values = c(
                                       "#5BC0BE",
                                       "#FC8D62",
                                       "#8DA0CB",
                                       "#E78AC3",
                                       "#A4243B"
                                     )) {
  
  perf_dat_type <- check_performance_data_type_for_plotly(performance_data = performance_data)
  prevalence <- get_prevalence_from_performance_data(performance_data, perf_dat_type)

  if (interactive == TRUE) {
    performance_data %>%
      reactable::reactable()
  } else {
    performance_data %>%
      prepare_performance_data_for_gt(main_slider) %>%  
      render_and_format_gt(main_slider, perf_dat_type, prevalence)
      
  }
}



#' Preparing Performance Data for gt
#'
#' @return
#' @keywords internal
#' @inheritParams plot_roc_curve
#'
prepare_performance_data_for_gt <- function(performance_data, 
                                            main_slider) {
  
  performance_data_ready_for_gt <- performance_data %>%
    replace_nan_with_na() %>%
    add_colors_to_performance_dat() 

  
  
  if (main_slider != "threshold") {
    performance_data_ready_for_gt <- performance_data_ready_for_gt %>%
      dplyr::relocate(plot_predicted_positives,
        .after = threshold
      ) %>%
      dplyr::arrange(predicted_positives_percent) %>% 
      dplyr::select(-threshold)
  } else {
    performance_data_ready_for_gt <- performance_data_ready_for_gt %>%
      dplyr::arrange(threshold)
  }

  performance_data_ready_for_gt %>%
    dplyr::select(-c(
      predicted_positives_percent,
      positives,
      display_predicted_postivies,
      FPR
    ))
}


#' Rendering and Formatting gt
#'
#' @return
#' @keywords internal
#' @inheritParams plot_roc_curve
#' @param prevalence the prevalence of the populations

render_and_format_gt <- function(performance_data, 
                                 main_slider, 
                                 perf_dat_type,
                                 prevalence){
  performance_data %>% 
  gt::gt() %>%
    gt::fmt_missing(
      columns = everything(),
      rows = everything(),
      missing_text = ""
    ) %>%
    gt::cols_align(
      align = "left",
      columns = c(
        TP, FP, TN, FN,
        sensitivity, lift, specificity, PPV, NPV, NB,
        plot_predicted_positives
      )
    ) %>%
    gt::cols_width(
      c(TP, TN, FP, FN, 
        sensitivity, lift, specificity, PPV, NPV, NB,
        plot_predicted_positives) ~ px(100)
    ) %>%
    gt::tab_spanner(
      label = "Confusion Matrix",
      columns = c(
        TP, FP, TN, FN,
        sensitivity, lift, specificity, PPV, NPV, NB
      )
    ) %>%
    gt::tab_spanner(
      label = "Performance Metrics",
      columns = c(
        sensitivity, lift, specificity,
        PPV, NPV, NB
      )
    ) %>%
    gt::cols_label(
      sensitivity = "Sens",
      lift = "Lift",
      specificity = "Spec",
      plot_predicted_positives = "Predicted Positives" # ,
    ) %>%
    gt::tab_options(
      table.background.color = "#FFFBF3"
    ) %>%
    gt::tab_header(
      title = gt::md(creating_title_for_gt(main_slider)),
      subtitle = gt::md(creating_subtitle_for_gt(perf_dat_type,
                                          prevalence = prevalence))
    )
}



#' Creating Title for gt performance table
#'
#' @return
#' @keywords internal
#' @inheritParams plot_roc_curve
creating_title_for_gt <- function(main_slider){
  
  if (main_slider == "threshold") {
    gt::md("**Performanc Metrics for Different Thresholds**")
  } else {
    gt::md("**Performanc Metrics by Predicted Positives Rate**")
  }
}



#' Creating Subtitle for gt performance table
#'
#' @return
#' @keywords internal
#' @inheritParams plot_roc_curve
#' @param models_names the names of the different models
creating_subtitle_for_gt <- function(
  perf_dat_type,
  col_values = NA,
  prevalence = NA,
  models_names = NA){
  
  if (perf_dat_type == "one model"){
    subtitle_for_gt <- glue::glue("prevalence: {round(prevalence, digits = 2)}")
  }
  
  if (perf_dat_type == "one model with model column"){
    subtitle_for_gt <- glue::glue("**{names(prevalence)}** model (prevalence: {round(prevalence, digits = 2)})")
  }
  
  # if (perf_dat_type == "several models"){
  #   
  #   names(col_values) <- names(prevalence)
  #   
  #   
  #   subtitle_for_gt <- glue::glue("**{names(prevalence)}** model (prevalence: {round(prevalence, digits = 2)})")
  # }

  if (perf_dat_type == "several populations"){
    
    subtitle_for_gt <- purrr::pmap(
      list(names(prevalence),
           col_values,
           prevalence),
      create_subtitle_for_one_population
    ) %>% 
      glue::glue_collapse(", ", last = " and ")
  }
  
  
  subtitle_for_gt
}





#' Creating Subtitle for One Population
#'
#' @param pop_name the name of the population
#' @param pop_color the color of the population
#' @param pop_prevalence the prevalence of the population
#'
#' @return
#' @keywords internal
create_subtitle_for_one_population <- function(pop_name, 
                                               pop_color, 
                                               pop_prevalence) {
  glue::glue("<b><span style=\"color: {pop_color};\">{pop_name}</span></b> population (prevalence: {round(pop_prevalence, digits = 2)})")
}