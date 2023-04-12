#' Create Data for Prevalence
#'
#' @inheritParams plot_roc_curve
#'
#' @keywords internal
create_table_for_prevalence <- function(performance_data,
                                        color_values = c(
                                          "#1b9e77", "#d95f02",
                                          "#7570b3", "#e7298a",
                                          "#07004D", "#E6AB02",
                                          "#FE5F55", "#54494B",
                                          "#006E90", "#BC96E6",
                                          "#52050A", "#1F271B",
                                          "#BE7C4D", "#63768D",
                                          "#08A045", "#320A28",
                                          "#82FF9E", "#2176FF",
                                          "#D1603D", "#585123"
                                        )) {
  perf_dat_type <- check_performance_data_type_for_plotly(
    performance_data = performance_data
  )

  if (perf_dat_type == "several populations") {
    data_for_prevalence <- dplyr::bind_cols(
      performance_data %>%
        get_prevalence_from_performance_data() %>%
        tibble(population = names(.), prevalence = .),
      performance_data %>%
        get_real_positives_from_performance_data() %>%
        tibble(real_positives = .),
      performance_data %>%
        dplyr::rename(any_of(c(
          "Model" = "model",
          "Population" = "population",
          "Threshold" = "threshold"
        ))) %>%
        get_n_from_performance_data() %>%
        dplyr::select(n_obs)
    ) %>%
      dplyr::mutate(population = forcats::fct_inorder(population))

    table_for_prevalence <- data_for_prevalence %>%
      reactable::reactable(
        sortable = FALSE,
        columns = list(
          prevalence = reactable::colDef(
            name = "Prevalence",
            minWidth = 300,
            align = "left",
            cell = function(value) {
              width <- paste0(value * 100, "%")
              bar_chart_with_background(
                format(round(value, digits = 2),
                  nsmall = 2
                ),
                width = width,
                fill = "grey",
                background = "#e1e1e1"
              )
            }
          ),
          n_obs = reactable::colDef(show = FALSE),
          real_positives = reactable::colDef(show = FALSE),
          population = reactable::colDef(
            minWidth = 300,
            cell = function(value, index) {
              n_levels <- length(levels(value))

              key_num <- index %% n_levels
              if (key_num == 0) {
                key_num <- n_levels
              }
              key_num <- as.character(key_num)

              color <- switch(as.character(key_num),
                "1" = color_values[1],
                "2" = color_values[2],
                "3" = color_values[3],
                "4" = color_values[4],
                "5" = color_values[5],
                "6" = color_values[6],
                "7" = color_values[7],
                "8" = color_values[8],
                "9" = color_values[9],
                "10" = color_values[10],
                "11" = color_values[11],
                "12" = color_values[12],
                "13" = color_values[13],
                "14" = color_values[14],
                "15" = color_values[15],
                "16" = color_values[16],
                "17" = color_values[17],
                "18" = color_values[18],
                "19" = color_values[19],
                "20" = color_values[20]
              )

              badge <- status_badge(color = color)
              tagList(badge, value)
            }
          )
        ), fullWidth = FALSE,
        details = function(index) {
          htmltools::div(
            "Real Positives = ",
            as.numeric(data_for_prevalence$real_positives[index]), ", ",
            " Total Population =  ",
            as.numeric(data_for_prevalence$n_obs[index])
          )
        }
      )
  } else {
    data_for_prevalence <- tibble::tibble(
      real_positives =
        get_real_positives_from_performance_data(performance_data)[1],
      prevalence = get_prevalence_from_performance_data(performance_data)[1],
      n_obs = as.numeric(get_n_from_performance_data(performance_data))
    )

    table_for_prevalence <- data_for_prevalence %>%
      reactable::reactable(
        sortable = FALSE,
        columns = list(
          prevalence = reactable::colDef(
            name = "Prevalence",
            align = "left",
            minWidth = 300,
            cell = function(value) {
              width <- paste0(value * 100, "%")
              bar_chart_with_background(
                format(round(value, digits = 2),
                  nsmall = 2
                ),
                width = width,
                fill = "grey",
                background = "#e1e1e1"
              )
            }
          ),
          n_obs = reactable::colDef(show = FALSE),
          real_positives = reactable::colDef(show = FALSE)
        ), fullWidth = FALSE,
        details = function(index) {
          htmltools::div(
            "Real Positives = ",
            as.numeric(data_for_prevalence$real_positives[index]), ", ",
            " Total Population =  ",
            as.numeric(data_for_prevalence$n_obs[index])
          )
        }
      )
  }

  table_for_prevalence
}



status_badge <- function(color = "#aaa", width = "9px", height = width) {
  span(style = list(
    display = "inline-block",
    marginRight = "8px",
    width = width,
    height = height,
    backgroundColor = color,
    borderRadius = "50%"
  ))
}
