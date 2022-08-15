#' Create Table for AUC
#'
#' @inheritParams create_roc_curve
#' @keywords internal
#' 
#' @examples 
#' 
#' rtichoke:::create_table_for_auc(
#'     probs = list(example_dat$estimated_probabilities),
#'     reals = list(example_dat$outcome)
#' )
#'
#' 
#' rtichoke:::create_table_for_auc(
#' probs = list(
#' "First Model" = example_dat$estimated_probabilities,
#' "Second Model" = example_dat$random_guess
#' ),
#' reals = list(example_dat$outcome)
#' )
#' 
#' 
#' rtichoke:::create_table_for_auc(
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
#'
#' @keywords internal
create_table_for_auc <- function(probs, 
                                 reals,
                                 col_values = c("#1b9e77", "#d95f02", 
                                                "#7570b3", "#e7298a", 
                                                "#07004D", "#E6AB02", 
                                                "#FE5F55", "#54494B", 
                                                "#006E90" , "#BC96E6",
                                                "#52050A", "#1F271B", 
                                                "#BE7C4D", "#63768D", 
                                                "#08A045", "#320A28", 
                                                "#82FF9E", "#2176FF", 
                                                "#D1603D", "#585123")) {
  
  if (length(probs) == 1) {
    names(probs) <- "Model 1"
  }
  
    data_for_auc <- purrr::map2_dbl(
      .x = reals,
      .y = probs,
      .f = ~ as.numeric(
        pROC::auc(
          .x, .y
        )
      )
    ) %>%
      tibble::tibble(
        population = names(probs),
        auc = .
      ) %>%
      dplyr::mutate(population = forcats::fct_inorder(population))

    table_for_auc <- data_for_auc %>%
      reactable::reactable(
        sortable = FALSE,
        fullWidth = FALSE,
        columns = list(
          auc = reactable::colDef(
            name = "AUROC",
            minWidth = 300,
            align = "left",
            cell = function(value) {
              width <- paste0(value * 100, "%")
              bar_chart_with_background(format(round(value, digits = 2),
                nsmall = 2
              ),
              width = width,
              fill = "green",
              background = "#e1e1e1"
              )
            }
          ),
          population = reactable::colDef(
            show = length(probs) != 1,
            name = ifelse(length(reals) == 1, "Model", "Population"),
            minWidth = 300,
            cell = function(value, index) {
              n_levels <- length(levels(value))

              key_num <- index %% n_levels
              if (key_num == 0) {
                key_num <- n_levels
              }
              key_num <- as.character(key_num)

              color <- switch(as.character(key_num),
                              "1" = col_values[1],
                              "2" = col_values[2],
                              "3" = col_values[3],
                              "4" = col_values[4],
                              "5" = col_values[5],
                              "6" = col_values[6],
                              "7" = col_values[7],
                              "8" = col_values[8],
                              "9" = col_values[9],
                              "10" = col_values[10],
                              "11" = col_values[11],
                              "12" = col_values[12],
                              "13" = col_values[13],
                              "14" = col_values[14],
                              "15" = col_values[15],
                              "16" = col_values[16],
                              "17" = col_values[17],
                              "18" = col_values[18],
                              "19" = col_values[19],
                              "20" = col_values[20]
              )

              badge <- status_badge(color = color)
              tagList(badge, value)
            }
          )
        )
      )

  table_for_auc
}



create_table_for_brier_score <- function(probs, real) {
  if (is.list(probs) & !is.list(real)) {
    if (length(probs) == 1) {
      create_table_for_brier_score(unlist(probs), real)
    }
  }

  if (!is.list(probs) & !is.list(real)) {
    data_for_brier_score <- tibble::tibble(
      brier_score = sum((probs - real)^2) / length(probs)
    )

    table_for_brier_score <- data_for_brier_score %>%
      reactable(
        sortable = FALSE,
        fullWidth = FALSE,
        columns = list(
          brier_score = reactable::colDef(
            name = "Brier Score",
            minWidth = 300,
            align = "left",
            cell = function(value) {
              width <- paste0(value * 100, "%")
              bar_chart_with_background(format(round(value, digits = 2),
                nsmall = 2
              ),
              width = width,
              fill = "red",
              background = "#e1e1e1"
              )
            }
          )
        )
      )
  }

  table_for_brier_score
}


bar_chart_with_background <- function(label, width = "100%", height = "16px",
                                      fill = "#00bfc4", background = NULL) {
  bar <- htmltools::div(style = list(
    background = fill, width = width,
    height = height
  ))
  chart <- htmltools::div(style = list(
    flexGrow = 1, marginLeft = "8px",
    background = background
  ), bar)
  htmltools::div(
    style = list(display = "flex", alignItems = "center"),
    as.character(label), chart
  )
}
