#' Create ggplot for performance metrics
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
  
    col_values_vec <- col_values[1:length(unique(performance_table[, 1]))]
  
  if (length(unique(performance_table[, 1])) == 1) {
    col_values_vec <- "black"
  }
  
  if (length(unique(performance_table[, 1])) > 1) {
    names(col_values_vec) <- unique(performance_table[, 1])
  }

  ggplot2::ggplot(
    performance_table,
    ggplot2::aes_string(
      x = x_perf_metric,
      y = y_perf_metric,
      group = names(performance_table)[1],
      color = names(performance_table)[1]
    )
  ) +
    ggplot2::geom_point(size = 1) +
    ggplot2::geom_path(size = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::scale_color_manual(values = col_values_vec)
}

#' ROC Curve
#' 
#' Create a ROC Curve
#'
#' @inheritParams create_performance_table
#'
#' @export
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
#' @export

plot_roc_curve <- function(performance_table,
                           chosen_threshold = NA,
                           interactive = F,
                           main_slider = "threshold") {
  if (interactive == F) {
    roc_curve <- performance_table %>%
      create_ggplot_for_performance_metrics("FPR", "sensitivity") %>%
      add_roc_curve_reference_lines()
  }
  return(roc_curve)
}

#' Add reference lines to ROC CURVE
#'
#' @param roc_curve a ggplot object of roc curve
add_roc_curve_reference_lines <- function(roc_curve) {
  roc_curve$layers <- c(
    ggplot2::geom_segment(x = 0, y = 0, xend = 1, yend = 1, color = "grey"),
    roc_curve$layers
  )
  roc_curve
}


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
#' @export

plot_lift_curve <- function(performance_table,
                           chosen_threshold = NA,
                           interactive = F,
                           main_slider = "threshold") {
    if (interactive == F) {
        lift_curve <- performance_table %>%
            create_ggplot_for_performance_metrics("predicted_positives_percent","lift") %>%
            add_lift_curve_reference_lines()
    }
    return(lift_curve)
}

#' Add reference lines to ROC CURVE
#'
#' @param lift_curve a ggplot object of lift curve
add_lift_curve_reference_lines <- function(lift_curve) {
    lift_curve$layers <- c(
        ggplot2::geom_segment(x = 0, y = 1, xend = 1, yend = 1, color = "grey"),
        lift_curve$layers
    )
    lift_curve
}

## Gains

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
#' @export

plot_gains_curve <- function(performance_table,
                            chosen_threshold = NA,
                            interactive = F,
                            main_slider = "threshold") {
  if (interactive == F) {
    gains_curve <- performance_table %>%
      create_ggplot_for_performance_metrics("predicted_positives_percent", "sensitivity") %>%
      add_gains_curve_reference_lines(get_prevalence_from_performance_table(performance_table))
  }
  return(gains_curve)
}

#' Add reference lines to Gains Curve
#'
#' @param gains_curve a ggplot object of lift curve
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


#' Title
#'
#' @param prevalence 
#' @param col_value 
#'
add_prevalence_layers_to_gains_curve <- function(prevalence, col_value){
  c(ggplot2::geom_segment(x = 0, y = 0, xend = prevalence, yend = 1, color = col_value, 
                          linetype= "dotted"),
  ggplot2::geom_segment(x = prevalence, y = 1, xend = 1, yend = 1, color = col_value, 
                        linetype= "dotted"))
  }




