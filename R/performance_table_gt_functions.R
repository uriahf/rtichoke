#' Creating html bar
#'
#' @param value original value
#' @param display displayed value
#' @param color color of the bar
#' @param digits number of digits
#' @param no_round no rounding
#'
#' @return
#' @keywords internal
bar_chart <- function(value, 
                      display, 
                      color = "red", 
                      digits = 0, 
                      no_round = FALSE){
  
  if (is.na(value) | is.nan(value)) {
    NA
  } else {
  if (no_round) {
    display_rounded <- display
  } else {
  display_rounded <- round(display, digits = digits) %>% 
    format(nsmall = digits)
  }
  
  glue::glue("<span style=\"display: inline-block;direction: ltr;
             background-color: {color}; color: black;
             width: {value}%\">{display_rounded}</span>") %>% 
    as.character() %>% 
    gt::html()
  }
}



#' Adding color to the confusion metric
#'
#' @param performance_dat original performance data
#' @param metric the required metric
#' @param color the required color
#'
#' @return
#' @keywords internal
add_color_to_confusion_metric <- function(performance_dat, 
                                metric,
                                color){
  performance_dat %>% 
    dplyr::mutate(
      metric_plot = 100 * {{metric}} / max( {{metric}} ),
      metric_plot = purrr::map2(
        metric_plot, {{ metric }}, 
        .f = ~bar_chart(value = .x, 
                        display = .y, 
                        color = color,
                        digits = 0))
    ) %>%  
    dplyr::mutate( {{ metric }} := metric_plot ) %>% 
    dplyr::select(-metric_plot)
}


#' Adding color to performance metric
#'
#' @param performance_dat the original performance data 
#' @param metric the required metric
#' @param color the required color
#'
#' @return
#' @keywords internal
add_color_to_performance_metric <- function(performance_dat, 
                                          metric,
                                          color){
  performance_dat %>% 
    dplyr::mutate(
      metric_plot = 100 * {{metric}}  ,
      metric_plot = purrr::map2(
        metric_plot, {{ metric }}, 
        .f = ~bar_chart(value = .x, 
                        display = .y, 
                        color = color,
                        digits = 2))
    ) %>%  
    dplyr::mutate( {{ metric }} := metric_plot ) %>% 
    dplyr::select(-metric_plot)
}

#' Adding color to the lift metric
#'
#' @param performance_dat the original performance data
#' @param metric the required metric
#' @param color the required color
#'
#' @return
#' @keywords internal
add_color_to_lift <- function(performance_dat, 
                                          metric,
                                          color){
  performance_dat %>% 
    dplyr::mutate(
      metric_plot = 100 * {{metric}} / max( {{metric}} , na.rm = T),
      metric_plot = purrr::map2(
        metric_plot, {{ metric }}, 
        .f = ~bar_chart(value = .x, 
                        display = .y, 
                        color = color,
                        digits = 2))
    ) %>%  
    dplyr::mutate( {{ metric }} := metric_plot ) %>% 
    dplyr::select(-metric_plot)
}

add_color_to_predicted_positives <- function(performance_dat) {
  performance_dat %>% 
    mutate(display_predicted_postivies = glue::glue("{positives} ({round(predicted_positives_percent * 100, digits = 1)}%)"),
           plot_predicted_positives = 100 * predicted_positives_percent,
           plot_predicted_positives = purrr::map2(
             plot_predicted_positives, display_predicted_postivies, 
             .f = ~bar_chart(value = .x, 
                             display = .y, 
                             color = "lightgrey",
                             no_round = T))) 
}


#' Replancing NaN with NA
#'
#' @param performance_dat the original performance data 
#'
#' @return
#' @keywords internal
replace_nan_with_na <- function(performance_dat) {
  
  performance_dat$PPV[is.nan(performance_dat$PPV)] <- NA
  performance_dat$NPV[is.nan(performance_dat$NPV)] <- NA
  performance_dat$lift[is.nan(performance_dat$lift)] <- NA
  performance_dat$NB[is.nan(performance_dat$NB)] <- NA
  
  
  performance_dat
}


#' Adding color to Performance Data
#'
#' @param performance_dat the original performance data
#'
#' @return
#' @keywords internal
add_colors_to_performance_dat <- function(performance_dat){
performance_dat %>% 
  add_color_to_confusion_metric(TP, "lightgreen") %>% 
  add_color_to_confusion_metric(TN, "lightgreen") %>% 
  add_color_to_confusion_metric(FP, "pink") %>% 
  add_color_to_confusion_metric(FN, "pink") %>% 
  add_color_to_performance_metric(sensitivity, "lightgreen") %>% 
  add_color_to_lift(lift, "lightgreen") %>% 
  add_color_to_predicted_positives() %>% 
  add_color_to_performance_metric(specificity, "lightgreen") %>% 
  add_color_to_performance_metric(PPV, "lightgreen") %>% 
  add_color_to_performance_metric(NPV, "pink") %>% 
  add_color_to_performance_metric(NB, "lightgreen")
}