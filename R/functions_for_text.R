#' Making performance metrics bold
#'
#' @param text_for_hover text
#' @param curve curve type
#'
#' @keywords internal
make_performance_metrics_bold <- function(text_for_hover, curve) {
  if (curve == "roc") {
    text_for_hover <- text_for_hover %>%
      make_two_performance_metrics_bold("Sensitivity", "FPR")
  }

  if (curve == "lift") {
    text_for_hover <- text_for_hover %>%
      make_two_performance_metrics_bold("lift", "ppcr")
  }

  if (curve == "precision recall") {
    text_for_hover <- text_for_hover %>%
      make_two_performance_metrics_bold("Sensitivity", "PPV")
  }

  if (curve == "gains") {
    text_for_hover <- text_for_hover %>%
      make_two_performance_metrics_bold("ppcr", "Sensitivity")
  }

  if (curve == "decision") {
    text_for_hover <- text_for_hover %>%
      make_two_performance_metrics_bold("NB", "probability_threshold")
  }
  if (curve == "interventions avoided") {
    text_for_hover <- text_for_hover %>%
      make_two_performance_metrics_bold(
        "Interventions Avoided",
        "probability_threshold"
      )
  }

  text_for_hover
}

#' Creating text to hover
#'
#' @param performance_data_type performance data type
#' @param curve curve type
#'
#' @inheritParams create_roc_curve
#'
#' @keywords internal
create_text_for_hover <- function(performance_data_type,
                                  curve,
                                  stratified_by = "probability_threshold") {
  if (curve != "interventions avoided") {
    text_for_hover <- paste0(
      "Prob. Threshold: {probability_threshold}
Sensitivity: {sensitivity}
1 - Specificity (FPR): {FPR}
Specificity: {specificity}
Lift: {lift}
PPV: {PPV}
NPV: {NPV}\n",
      ifelse(stratified_by == "probability_threshold",
        "NB: {NB}\n",
        ""
      ),
      "Predicted Positives: {predicted_positives} ({100 * ppcr}%)
TP: {TP}
TN: {TN}
FP: {FP}
FN: {FN}"
    )
  } else {
    text_for_hover <- "Prob. Threshold: {probability_threshold}
Interventions Avoided (per 100): {NB_treatment_avoided}
NB: {NB}
Predicted Positives: {predicted_positives} ({100 * ppcr}%)
TN: {TN}
FN: {FN}"
  }
  text_for_hover <- make_performance_metrics_bold(text_for_hover, curve)
  if (performance_data_type == "several models") {
    text_for_hover <- add_models_for_text_for_hover(text_for_hover)
  }
  if (performance_data_type == "several populations") {
    text_for_hover <- add_population_for_text_for_hover(text_for_hover)
  }
  text_for_hover
}

#' Adding models for text to hover
#'
#' @param text_for_hover text
#'
#' @keywords internal
add_models_for_text_for_hover <- function(text_for_hover) {
  paste("<b>Model: {model}</b>",
    text_for_hover,
    sep = "\n"
  )
}

#' Adding population for text to hover
#'
#' @param text_for_hover text
#'
#' @keywords internal
add_population_for_text_for_hover <- function(text_for_hover) {
  paste("<b>Population: {population}</b>",
    text_for_hover,
    sep = "\n"
  )
}

#' Making two performance metrics bold
#'
#' @param text_for_hover text
#' @param performance_metric_x  x
#' @param performance_metric_y  y
#'
#' @keywords internal
make_two_performance_metrics_bold <- function(text_for_hover,
                                              performance_metric_x,
                                              performance_metric_y) {
  text_for_hover %>%
    make_performance_metric_bold(performance_metric_x) %>%
    make_performance_metric_bold(performance_metric_y)
}

#' Making performance metric bold
#'
#' @param hover_text  text
#' @param performance_metric perf
#'
#' @keywords internal
make_performance_metric_bold <- function(hover_text, performance_metric) {
  performance_metrics_text_hover <- unlist(stringr::str_split(hover_text, "<br>"))

  performance_metrics_text_hover[performance_metrics_text_hover %>%
    stringr::str_detect(performance_metric)] <- paste0(
    "<b>",
    performance_metrics_text_hover[performance_metrics_text_hover %>%
      stringr::str_detect(performance_metric)],
    "</b>"
  )

  updated_text_hover <- paste(
    unlist(
      stringr::str_split(
        performance_metrics_text_hover, "<br>"
      )
    ),
    collapse = "<br>"
  )



  updated_text_hover
}



#' Adding hover text to performance data
#'
#' @param performance_data perf dat
#' @param performance_data_type perf dat type
#' @param curve curve
#'
#' @inheritParams create_roc_curve
#'
#' @keywords internal
add_hover_text_to_performance_data <- function(performance_data,
                                               performance_data_type,
                                               curve,
                                               stratified_by = "probability_threshold") {
  text_for_hover <- create_text_for_hover(
    performance_data_type,
    curve,
    stratified_by
  )

  performance_data %>%
    dplyr::mutate(
      dplyr::across(where(is.numeric), round, 3),
      text = glue::glue(text_for_hover),
      text = stringr::str_replace_all(.data$text, pattern = "NaN", "")
    )
}
