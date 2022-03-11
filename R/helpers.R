#' Real Positives
#'
#' Get the real positives out of Performance Data
#'
#' @param performance_data an rtichoke Performance Data
#' @param performance_data_type the type of the Performance Data
#'
#' @keywords internal
get_real_positives_from_performance_data <- function(performance_data,
                                                     performance_data_type =
                                                       "not important") {
  real_positives <- real_positives <- NULL

  real_positives <- performance_data %>%
    mutate(real_positives = TP + FN) %>%
    dplyr::filter(ppcr == 1) %>%
    dplyr::select(dplyr::any_of(c("model", "population", "real_positives"))) %>%
    distinct() %>%
    dplyr::pull(real_positives, name = 1)

  real_positives
}

#' prevalence
#'
#' Get the prevalence out of Performance Data
#'
#' @param performance_data an rtichoke Performance Data
#' @param performance_data_type the type of the Performance Data
#'
#' @keywords internal
get_prevalence_from_performance_data <- function(performance_data,
                                                 performance_data_type =
                                                   "not important") {
  PPV <- ppcr <- NULL

  prevalence <- performance_data %>%
    dplyr::filter(ppcr == 1) %>%
    dplyr::select(dplyr::any_of(c("model", "population", "PPV"))) %>%
    distinct() %>%
    dplyr::pull(PPV, name = 1)

  prevalence
}

#' n
#'
#' Get the prevalence out of Performance Data
#'
#' @param performance_data an rtichoke Performance Data
#' @param performance_data_type the type of the Performance Data
#'
#' @keywords internal
get_n_from_performance_data <- function(performance_data,
                                        performance_data_type =
                                          "not important") {
  predicted_positives <- ppcr <- NULL

  # print(performance_data)

  real_positives <- performance_data %>%
    dplyr::filter(ppcr == 1) %>%
    dplyr::select(dplyr::any_of(
      c("Model", "Population", "predicted_positives")
    )) %>%
    distinct() %>%
    rename("n_obs" = predicted_positives) %>%
    select(1, "n_obs")
  # dplyr::pull(predicted_positives , name = 1)

  real_positives
}


#' Creating Reference Lines Data Frame
#'
#' @param curve the specified curve for the reference lines
#' @param prevalence the prevalence of the outcome
#' @param plotly should the reference lines data frame be
#' competible with plotly
#' @param multiple_pop should the reference lines data frame should be
#' adjusted to multiple populations
#' @param color the required color
#' @keywords internal

create_reference_lines_data_frame <- function(curve,
                                              prevalence = NA,
                                              color = NA,
                                              plotly = FALSE,
                                              multiple_pop = FALSE) {
  if (curve == "roc") {
    if (plotly == FALSE) {
      reference_lines_data_frame <- data.frame(
        x = 0, xend = 1, y = 0,
        yend = 1, col = "grey",
        linetype = "solid"
      )
    } else {
      reference_lines_data_frame <- data.frame(x = c(0, 1), y = c(0, 1))
    }
  }

  if (curve == "lift") {
    if (plotly == FALSE) {
      reference_lines_data_frame <- data.frame(
        x = 0, xend = 1,
        y = 1, yend = 1,
        col = "grey",
        linetype = "solid"
      )
    } else {
      reference_lines_data_frame <- data.frame(x = c(0, 1), y = c(1, 1))
    }
  }

  if (curve == "precision recall") {
    if (length(prevalence) == 1) {
      col_values <- "grey"
    }
    if (length(prevalence) > 1) {
      col_values <- c(
        "#5BC0BE",
        "#FC8D62",
        "#8DA0CB",
        "#E78AC3",
        "#A4243B"
      )[seq_len(length(prevalence))]
    }
    if (plotly == FALSE) {
      reference_lines_data_frame <- data.frame(
        x = 0, xend = 1, y = prevalence, yend = prevalence, col = col_values,
        linetype = "dotted"
      )
    } else {
      reference_lines_data_frame <- data.frame(
        population = rep(names(prevalence), each = 2),
        x = c(0, 1),
        y = rep(prevalence, each = 2)
      )
    }
  }

  if (curve == "gains") {
    if (length(prevalence) == 1) {
      col_values <- "grey"
    }
    if (length(prevalence) > 1) {
      col_values <- c(
        "#5BC0BE",
        "#FC8D62",
        "#8DA0CB",
        "#E78AC3",
        "#A4243B"
      )[seq_len(length(prevalence))]
    }

    if (plotly == FALSE) {
      reference_lines_data_frame <- purrr::map2_df(
        prevalence,
        col_values,
        function(x, y) {
          data.frame(
            x = c(0, x),
            xend = c(x, 1),
            y = c(0, 1),
            yend = c(1, 1),
            col = c(y, y),
            linetype = "dotted"
          )
        }
      ) %>%
        bind_rows(
          data.frame(
            x = 0, xend = 1, y = 0, yend = 1, col = "grey",
            linetype = "dotted"
          )
        )
    } else {
      reference_lines_data_frame <- data.frame(
        population = names(prevalence),
        x = prevalence,
        y = 1,
        row.names = NULL
      ) %>%
        bind_rows(
          data.frame(
            population = rep(names(prevalence), each = 2),
            x = c(0, 1),
            y = c(0, 1)
          )
        ) %>%
        arrange(population, x, y) %>%
        bind_rows(
          data.frame(population = "random", x = c(0, 1), y = c(0, 1))
        )
    }
  }

  if (curve == "decision") {
    if (plotly == FALSE) {
      reference_lines_data_frame <- rbind(
        create_reference_lines_data_frame("decision treat all", prevalence),
        create_reference_lines_data_frame("decision treat none")
      )
    } else {
      reference_lines_data_frame <- bind_rows(
        data.frame(
          population = names(prevalence), x = prevalence, y = 0,
          row.names = NULL
        ),
        data.frame(
          population = names(prevalence), x = 0, y = prevalence,
          row.names = NULL
        ),
        data.frame(population = "treat_none", x = c(0, 1), y = c(0, 0))
      )
    }
  }

  if (curve == "decision treat all") {
    if (length(prevalence) == 1) {
      col_values <- "grey"
    }
    if (length(prevalence) > 1) {
      col_values <- c(
        "#5BC0BE",
        "#FC8D62",
        "#8DA0CB",
        "#E78AC3",
        "#A4243B"
      )[seq_len(length(prevalence))]
    }

    reference_lines_data_frame <- data.frame(
      x = 0, xend = prevalence, y = prevalence, yend = 0, col = col_values,
      linetype = "dotted"
    )
  }

  if (curve == "decision treat none") {
    reference_lines_data_frame <- data.frame(
      x = 0, xend = 1, y = 0, yend = 0, col = "grey",
      linetype = "solid"
    )
  }

  reference_lines_data_frame
}


create_segment_for_reference_line <- function(reference_line) {
  ggplot2::geom_segment(
    x = reference_line$x,
    y = reference_line$y,
    xend = reference_line$xend,
    yend = reference_line$yend,
    color = reference_line$col,
    size = 1,
    linetype = reference_line$linetype
  )
}


#' Add reference lines to ggplot curve
#'
#' @param ggplot_curve a non interactive ggplot curve
#' @param reference_lines dataframe of reference lines
#' @keywords internal

add_reference_lines_to_ggplot <- function(ggplot_curve, reference_lines) {
  ggplot_curve$layers <- c(
    purrr::map(reference_lines %>%
      split(seq_len(nrow(.))), ~ create_segment_for_reference_line(.x)),
    ggplot_curve$layers
  )
  ggplot_curve
}



#' Creating subtitle for ggplot2
#'
#' @inheritParams create_roc_curve
#' @param probs_names the names of the probs
#' @keywords internal
#' @examples
#' \dontrun{
#' create_subtitle_for_ggplot(
#'   probs_names = c(
#'     "First Model", "Second Model", "Third Model"
#'   )
#' )
#' }
create_subtitle_for_ggplot <- function(probs_names, col_values = c(
                                         "#5BC0BE",
                                         "#FC8D62",
                                         "#8DA0CB",
                                         "#E78AC3",
                                         "#A4243B"
                                       )) {
  subtitle <- glue::glue("{probs_names},
                         {col_values[1:length(probs_names)]}")
  subtitle
}
