
#' Create a list of confusion matrices
#'
#' @inheritParams plot_roc_curve
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' one_pop_one_model_as_a_vector %>%
#'   create_conf_mat_list()
#'
#' one_pop_one_model_as_a_vector_enforced_percentiles_symmetry %>%
#'   create_conf_mat_list(main_slider = "ppcr")
#'
#' one_pop_one_model_as_a_list %>%
#'   create_conf_mat_list()
#'
#' one_pop_one_model_as_a_list_enforced_percentiles_symmetry %>%
#'   create_conf_mat_list(main_slider = "ppcr")
#'
#' one_pop_three_models %>%
#'   create_conf_mat_list()
#'
#' one_pop_three_models_enforced_percentiles_symmetry %>%
#'   create_conf_mat_list(main_slider = "ppcr")
#'
#' train_and_test_sets %>%
#'   create_conf_mat_list()
#'
#' train_and_test_sets_enforced_percentiles_symmetry %>%
#'   create_conf_mat_list(main_slider = "ppcr")
#'
#' one_pop_one_model_as_a_vector %>%
#'   create_conf_mat_list()
#'
#' one_pop_one_model_as_a_vector_enforced_percentiles_symmetry %>%
#'   create_conf_mat_list(main_slider = "ppcr")
#' }
create_conf_mat_list <- function(performance_table,
                                 stratified_by = "probability_threshold") {
  if (stratified_by != "probability_threshold") {
    performance_table <- performance_table %>%
      dplyr::arrange(ppcr)
  } else {
    performance_table <- performance_table %>%
      dplyr::arrange(probability_threshold)
  }

  matrix_list <- performance_table %>%
    dplyr::mutate(Predicted_Positive = predicted_positives) %>%
    dplyr::mutate(
      Predicted_Negative = FN + TN,
      Real_Positive = TP + FN,
      Real_Negative = TN + FP,
      N = TP + FP + FN + TN
    ) %>%
    dplyr::select(
      probability_threshold,
      ppcr,
      TP,
      FP,
      Predicted_Positive,
      FN,
      TN,
      Predicted_Negative,
      Real_Positive,
      Real_Negative,
      N
    ) %>%
    mutate(idx = 1:n()) %>%
    split(f = .["idx"]) %>%
    purrr::map(~ dplyr::select(., -probability_threshold, -ppcr, -idx)) %>%
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
