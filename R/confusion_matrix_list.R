
#' Create a list of confusion matrices
#'
#' @inheritParams plot_roc_curve
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'
#' one_pop_one_model %>%
#'   create_conf_mat_list()
#'
#' one_pop_one_model_by_ppcr %>%
#'   create_conf_mat_list()
#'
#' multiple_models %>%
#'   create_conf_mat_list()
#'
#' multiple_models_by_ppcr %>%
#'   create_conf_mat_list()
#'
#' multiple_populations %>%
#'   create_conf_mat_list()
#'
#' multiple_populations_by_ppcr %>%
#'   create_conf_mat_list()
#' }
create_conf_mat_list <- function(performance_table,
                                 stratified_by = "probability_threshold") {
  if (stratified_by != "probability_threshold") {
    performance_table <- performance_table %>%
      dplyr::arrange(.data$ppcr)
  } else {
    performance_table <- performance_table %>%
      dplyr::arrange(.data$probability_threshold)
  }

  matrix_list <- performance_table %>%
    dplyr::mutate(Predicted_Positive = .data$predicted_positives) %>%
    dplyr::mutate(
      Predicted_Negative = .data$FN + .data$TN,
      Real_Positive = .data$TP + .data$FN,
      Real_Negative = .data$TN +.data$ FP,
      N = .data$TP + .data$FP + .data$FN + .data$TN
    ) %>%
    dplyr::select(
      .data$probability_threshold,
      .data$ppcr,
      .data$TP,
      .data$FP,
      .data$Predicted_Positive,
      .data$FN,
      .data$TN,
      .data$Predicted_Negative,
      .data$Real_Positive,
      .data$Real_Negative,
      .data$N
    ) %>%
    mutate(idx = 1:n()) %>%
    split(f = .["idx"]) %>%
    purrr::map(~ dplyr::select(., -.data$probability_threshold, -.data$ppcr, -.data$idx)) %>%
    purrr::map(~ matrix(., nrow = 3, byrow = TRUE)) %>%
    purrr::map(~ magrittr::set_rownames(., c(
      "Predicted Positive",
      "Predicted Negative", " "
    ))) %>%
    purrr::map(~ magrittr::set_colnames(., c(
      "Real Positive",
      "Real Negative", " "
    )))

  matrix_list %>%
    purrr::map(~ render_reactable_confusion_matrix(.))
}

#' Render a reactable confusion matrix
#'
#' @param confusion_matrix a confusion matrix
#'
#' @keywords internal
render_reactable_confusion_matrix <- function(confusion_matrix) {
  N <- as.numeric(confusion_matrix[3, 3])

  reactable::reactable(confusion_matrix,
    fullWidth = FALSE,
    sortable = FALSE,
    columns = list(
      "Real Positive" = reactable::colDef(
        align = "left",
        style = function(value, index) {
          bar_style_perf(
            width = value / N,
            c(
              "lightgreen", "pink",
              "lightgrey"
            )[index]
          )
        },
        cell = function(value, index) {
          glue::glue(
            "{value} ({round(value / N * 100, digits = 2)}%) "
          )
        }
      ),
      "Real Negative" = reactable::colDef(
        align = "left",
        style = function(value, index) {
          bar_style_perf(
            width = value / N,
            c(
              "pink",
              "lightgreen",
              "lightgrey"
            )[index]
          )
        },
        cell = function(value, index) {
          glue::glue(
            "{value} ({round(value / N * 100, digits = 2)}%)
                               "
          )
        }
      ),
      " " = reactable::colDef(
        align = "left",
        style = function(value, index) {
          bar_style_perf(width = value / N, "lightgrey")
        },
        cell = function(value, index) {
          glue::glue(
            "{value} ({round(value / N * 100, digits = 2)}%) "
          )
        }
      ),
      ".rownames" = reactable::colDef(
        style = list(fontWeight = "bold")
        # width = 70
      )
    )
  )
}
