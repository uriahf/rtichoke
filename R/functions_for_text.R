#' Title
#'
#' @param text_for_hover text
#' @param curve curve type
#'
#' @return
make_performance_metrics_bold <- function(text_for_hover, curve) {
  if (curve == "roc") {
    text_for_hover <- text_for_hover %>%
      make_two_performance_metrics_bold("Sensitivity", "FPR")
  }

  if (curve == "lift") {
    text_for_hover <- text_for_hover %>%
      make_two_performance_metrics_bold("lift", "predicted_positives_percent")
  }

  if (curve == "precision recall") {
    text_for_hover <- text_for_hover %>%
      make_two_performance_metrics_bold("Sensitivity", "PPV")
  }

  if (curve == "gains") {
    text_for_hover <- text_for_hover %>%
      make_two_performance_metrics_bold("predicted_positives_percent", "Sensitivity")
  }

  if (curve == "decision") {
    text_for_hover <- text_for_hover %>%
      make_performance_metric_bold("NB")
  }

  text_for_hover
}

#' Title
#'
#' @param performance_data_type performance data type
#' @param curve curve type
#'
#' @return
create_text_for_hover <- function(performance_data_type, curve) {
  text_for_hover <- "Sensitivity (sensitivity): {sensitivity}
1 - Specificity (FPR): {FPR}
Specificity: {specificity}
Lift: {lift}
PPV: {PPV}
NPV: {NPV}
NB: {NB}
Fraction of Predicted Positives: {predicted_positives_percent}
TP: {TP}
TN: {TN}
FP: {FP}
FN: {FN}"
  text_for_hover <- make_performance_metrics_bold(text_for_hover, curve)
  if (performance_data_type == "several models") {
    text_for_hover <- add_models_for_text_for_hover(text_for_hover)
  }
  if (performance_data_type == "several populations") {
    text_for_hover <- add_population_for_text_for_hover(text_for_hover)
  }
  text_for_hover
}

#' Title
#'
#' @param text_for_hover text
#'
#' @return
add_models_for_text_for_hover <- function(text_for_hover) {
  paste("<b>Model: {model}</b>",
    text_for_hover,
    sep = "\n"
  )
}

#' Title
#'
#' @param text_for_hover 
#'
#' @return
add_population_for_text_for_hover <- function(text_for_hover) {
  paste("<b>Population: {population}</b>",
    text_for_hover,
    sep = "\n"
  )
}

#' Title
#'
#' @param text_for_hover text
#' @param performance_metric_x  x
#' @param performance_metric_y  y
#'
#' @return
make_two_performance_metrics_bold <- function(text_for_hover, performance_metric_x, performance_metric_y) {
  text_for_hover %>%
    make_performance_metric_bold(performance_metric_x) %>%
    make_performance_metric_bold(performance_metric_y)
}

#' Title
#'
#' @param hover_text  text 
#' @param performance_metric perf
#'
#' @return
make_performance_metric_bold <- function(hover_text, performance_metric) {
  performance_metrics_text_hover <- unlist(str_split(hover_text, "\n"))

  performance_metrics_text_hover[performance_metrics_text_hover %>%
    stringr::str_detect(performance_metric)] <- paste0(
    "<b>",
    performance_metrics_text_hover[performance_metrics_text_hover %>%
      stringr::str_detect(performance_metric)],
    "</b>"
  )

  updated_text_hover <- paste(unlist(str_split(performance_metrics_text_hover, "\n")), collapse = "\n")



  updated_text_hover
}



#' Title
#'
#' @param performance_data perf dat
#' @param performance_data_type perf dat type
#' @param curve 
#'
#' @return
add_hover_text_to_performance_data <- function(performance_data,
                                               performance_data_type,
                                               curve) {
  text_for_hover <- create_text_for_hover(performance_data_type, curve)
  
  performance_data %>%
    dplyr::mutate(dplyr::across(where(is.numeric), round, 3),
           text = glue::glue(text_for_hover)
    )
}
