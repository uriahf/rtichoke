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
                                              multiple_pop = FALSE,
                                              performance_data = NULL) {
  if (curve == "roc") {
    if (plotly == FALSE) {
      reference_lines_data_frame <- data.frame(
        x = 0, xend = 1, y = 0,
        yend = 1, col = "grey",
        linetype = "solid"
      )
    } else {
      reference_lines_data_frame <- data.frame(
        x = c(0, 1),
        y = c(0, 1),
        text = c("Random Guess")
      )
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
        dplyr::arrange(population, x, y) %>%
        dplyr::bind_rows(
          data.frame(population = "random", x = c(0, 1), y = c(0, 1))
        ) %>%
        dplyr::mutate(
          text = dplyr::case_when(
            population != "random" ~ glue::glue(
              "<b>Sensitivity of Perfect Prediction:</b> \\
                              {round(y, digits = 3)} <br>\\
                              <b>PPCR:</b> {round(x, digits = 3)}"
            ),
            TRUE ~ "<b>Random Guess"
          )
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
      if (length(prevalence) == 1) {
        reference_lines_data_frame <- performance_data %>%
          dplyr::mutate(
            population = "treat_all",
            x = probability_threshold,
            y = prevalence - (1 - prevalence) *
              (probability_threshold / (1 - probability_threshold))
          ) %>%
          dplyr::select(population, x, y) %>%
          dplyr::bind_rows(
            data.frame(population = "treat_none", x = c(0, 1), y = c(0, 0))
          ) %>%
          dplyr::mutate(
            text = dplyr::case_when(
              population != "treat_none" ~ glue::glue("<b>NB Treat All:</b> \\
                              {round(y, digits = 3)} <br>\\
                              <b>Prob. Threshold:</b> \\
                              {round(x, digits = 3)} "),
              TRUE ~ "<b>NB Treat None:</b> 0"
            )
          )
      } else {
        reference_lines_data_frame <- performance_data %>%
          dplyr::left_join(
            data.frame(prevalence) %>%
              tibble::rownames_to_column("population")
          ) %>%
          dplyr::mutate(
            x = probability_threshold,
            y = prevalence - (1 - prevalence) *
              (probability_threshold / (1 - probability_threshold)),
            linetype = "solid"
          ) %>%
          dplyr::select(population, x, y, linetype) %>%
          dplyr::bind_rows(
            data.frame(
              population = "treat_none", x = c(0, 1), y = c(0, 0),
              linetype = "dotted"
            )
          ) %>%
          dplyr::mutate(
            text = dplyr::case_when(
              population != "treat_none" ~ glue::glue(
                "<b>NB Treat All ({population}):</b> \\
                              {round(y, digits = 3)} <br>\\
                              <b>Prob. Threshold:</b> \\
                              {round(x, digits = 3)} "
              ),
              TRUE ~ "<b>NB Treat None:</b> 0"
            )
          )
      }
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

  if (curve == "interventions avoided") {
    reference_lines_data_frame <- data.frame(
      x = numeric(), y = numeric()
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





#' Creating rtichoke curve list
#'
#' @inheritParams plot_roc_curve
#' @inheritParams check_curve_input
#' @inheritParams create_decision_curve
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' one_pop_one_model |>
#'   create_rtichoke_curve_list("roc")
#'
#' one_pop_one_model_by_ppcr |>
#'   create_rtichoke_curve_list("roc")
#'
#' multiple_models |>
#'   create_rtichoke_curve_list("roc")
#'
#' multiple_models_by_ppcr |>
#'   create_rtichoke_curve_list("roc")
#'
#' multiple_populations |>
#'   create_rtichoke_curve_list("roc")
#'
#' multiple_populations_by_ppcr |>
#'   create_rtichoke_curve_list("roc")
#' }
create_rtichoke_curve_list <- function(performance_data,
                                       curve,
                                       min_p_threshold = 0,
                                       max_p_threshold = 1,
                                       size = NULL, col_values = c(
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
  rtichoke_curve_list <- list()

  rtichoke_curve_list$size <- size

  stratified_by <- check_performance_data_stratification(
    performance_data
  )
  
  rtichoke_curve_list$animation_slider_prefix <- ifelse(stratified_by == "probability_threshold",
    "Prob. Threshold: ",
    "Predicted Positives (Rate):"
  )

  rtichoke_curve_list$perf_dat_type <- check_performance_data_type_for_plotly(
    performance_data = performance_data
  )


  rtichoke_curve_list$group_colors_vec <- performance_data |>
    extract_reference_groups_from_performance_data(rtichoke_curve_list$perf_dat_type) |>
    create_reference_group_color_vector(rtichoke_curve_list$perf_dat_type, col_values = col_values) |>
    as.list()

  prevalence_from_performance_data <- get_prevalence_from_performance_data(performance_data) |>
    as.list()

  rtichoke_curve_list$reference_data <- create_reference_lines_data(
    curve,
    prevalence_from_performance_data,
    rtichoke_curve_list$perf_dat_type,
    min_p_threshold = min_p_threshold,
    max_p_threshold = max_p_threshold
  )

  if (curve == "roc") {
    x_performance_metric <- "FPR"
    y_performance_metric <- "sensitivity"

    rtichoke_curve_list$axes_labels$xaxis <- "1 - Specificity"
    rtichoke_curve_list$axes_labels$yaxis <- "Sensitivity"
  } else if (curve == "precision recall") {
    x_performance_metric <- "sensitivity"
    y_performance_metric <- "PPV"

    rtichoke_curve_list$axes_labels$xaxis <- "Sensitivity"
    rtichoke_curve_list$axes_labels$yaxis <- "PPV"
  } else if (curve == "lift") {
    x_performance_metric <- "ppcr"
    y_performance_metric <- "lift"

    rtichoke_curve_list$axes_labels$xaxis <- "Predicted Positives (Rate)"
    rtichoke_curve_list$axes_labels$yaxis <- "Lift"
  } else if (curve == "gains") {
    x_performance_metric <- "ppcr"
    y_performance_metric <- "sensitivity"

    rtichoke_curve_list$axes_labels$xaxis <- "Predicted Positives (Rate)"
    rtichoke_curve_list$axes_labels$yaxis <- "Sensitivity"
  } else if (curve == "decision") {
    x_performance_metric <- "probability_threshold"
    y_performance_metric <- "NB"

    rtichoke_curve_list$axes_labels$xaxis <- "Probability Threshold"
    rtichoke_curve_list$axes_labels$yaxis <- "Net Benefit"
  } else if (curve == "interventions avoided") {
    x_performance_metric <- "probability_threshold"
    y_performance_metric <- "NB_interventions_avoided"

    rtichoke_curve_list$axes_labels$xaxis <- "Probability Threshold"
    rtichoke_curve_list$axes_labels$yaxis <- "Interventions Avoided (per 100)"
  }

  rtichoke_curve_list$performance_data_ready_for_curve <- performance_data |>
    prepare_performance_data_for_curve(
      x_performance_metric,
      y_performance_metric,
      stratified_by,
      rtichoke_curve_list$perf_dat_type,
      min_p_threshold = min_p_threshold,
      max_p_threshold = max_p_threshold
    )
  
  
  rtichoke_curve_list$performance_data_for_interactive_marker <- prepare_performance_data_for_interactive_marker(
    rtichoke_curve_list$performance_data_ready_for_curve,
    rtichoke_curve_list$perf_dat_type)


  rtichoke_curve_list$axes_ranges <- extract_axes_ranges(rtichoke_curve_list$performance_data_ready_for_curve,
    curve,
    min_p_threshold = min_p_threshold,
    max_p_threshold = max_p_threshold
  ) |>
    as.list()

  rtichoke_curve_list
}


#' Creating rtichoke plotly curve
#'
#' @param rtichoke_curve_list rtichoke curve list
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' one_pop_one_model |>
#'   create_rtichoke_curve_list("roc") |>
#'   create_plotly_curve()
#'
#' one_pop_one_model_by_ppcr |>
#'   create_rtichoke_curve_list("roc") |>
#'   create_plotly_curve()
#'
#' multiple_models |>
#'   create_rtichoke_curve_list("roc") |>
#'   create_plotly_curve()
#'
#' multiple_models_by_ppcr |>
#'   create_rtichoke_curve_list("roc") |>
#'   create_plotly_curve()
#'
#' multiple_populations |>
#'   create_rtichoke_curve_list("roc") |>
#'   create_plotly_curve()
#'
#' multiple_populations_by_ppcr |>
#'   create_rtichoke_curve_list("roc") |>
#'   create_plotly_curve()
#' }
create_plotly_curve <- function(rtichoke_curve_list) {
  size_height <- switch(is.null(rtichoke_curve_list$size) + 1,
    rtichoke_curve_list$size + 50,
    NULL
  )

  interactive_marker <- list(
    size = 12,
    line = list(
      width = 3,
      color = I("black")
    )
  )

  if (!(rtichoke_curve_list$perf_dat_type %in% c("several models", "several populations"))) {
    interactive_marker$color <- "#f6e3be"
  }
  
  plotly::plot_ly(
    x = ~x,
    y = ~y,
    height = size_height,
    width = rtichoke_curve_list$size,
    hoverinfo = "text",
    text = ~text,
    color = ~reference_group,
    colors = unlist(rtichoke_curve_list$group_colors_vec)
  ) |>
    plotly::add_lines(
      data = rtichoke_curve_list$reference_data,
      line = list(
        dash = "dot"
      )
    ) |>
    plotly::add_trace(
      data = rtichoke_curve_list$performance_data_ready_for_curve,
      type = "scatter",
      mode = "lines+markers",
      line = list(dash = "solid")
    ) |>
    plotly::add_markers(
      data = rtichoke_curve_list$performance_data_for_interactive_marker,
      frame = ~stratified_by,
      marker = interactive_marker
    ) |>
    plotly::layout(
      xaxis = list(
        showgrid = FALSE, fixedrange = TRUE,
        range = rtichoke_curve_list$axes_ranges$xaxis,
        title = rtichoke_curve_list$axes_labels$xaxis
      ),
      yaxis = list(
        showgrid = FALSE, fixedrange = TRUE,
        range = rtichoke_curve_list$axes_ranges$yaxis,
        title = rtichoke_curve_list$axes_labels$yaxis
      ),
      showlegend = FALSE,
      plot_bgcolor = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)"
    ) |>
    plotly::animation_slider(
      currentvalue = list(
        prefix = rtichoke_curve_list$animation_slider_prefix,
        font = list(color = "black"),
        xanchor = "left"
      ),
      pad = list(t = 50)
    ) |>
    plotly::config(displayModeBar = FALSE) |>
    plotly::animation_button(visible = FALSE)
}



#' Plot rtichoke curve
#'
#' @inheritParams create_roc_curve
#' @inheritParams plot_roc_curve
#' @inheritParams plot_decision_curve
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' one_pop_one_model |>
#'   plot_rtichoke_curve("roc")
#'
#' one_pop_one_model_by_ppcr |>
#'   plot_rtichoke_curve("roc")
#'
#' multiple_models |>
#'   plot_rtichoke_curve("roc")
#'
#' multiple_models_by_ppcr |>
#'   plot_rtichoke_curve("roc")
#'
#' multiple_populations |>
#'   plot_rtichoke_curve("roc")
#'
#' multiple_populations_by_ppcr |>
#'   plot_rtichoke_curve("roc")
#' }
plot_rtichoke_curve <- function(performance_data, curve, col_values = c(
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
                                ), min_p_threshold = 0, max_p_threshold = 1, size = NULL) {
  check_curve_input(curve)

  stratified_by <- check_performance_data_stratification(
    performance_data
  )

  prevalence_from_performance_data <- get_prevalence_from_performance_data(performance_data)
  perf_dat_type <- check_performance_data_type_for_plotly(performance_data = performance_data)

  reference_group_colors_vec <- performance_data |>
    extract_reference_groups_from_performance_data(perf_dat_type) |>
    create_reference_group_color_vector(perf_dat_type, col_values = col_values)

  if (curve == "roc") {
    x_performance_metric <- "FPR"
    y_performance_metric <- "sensitivity"
  } else if (curve == "precision recall") {
    x_performance_metric <- "sensitivity"
    y_performance_metric <- "PPV"
  } else if (curve == "lift") {
    x_performance_metric <- "ppcr"
    y_performance_metric <- "lift"
  } else if (curve == "gains") {
    x_performance_metric <- "ppcr"
    y_performance_metric <- "sensitivity"
  } else if (curve == "decision") {
    x_performance_metric <- "probability_threshold"
    y_performance_metric <- "NB"
  } else if (curve == "interventions avoided") {
    x_performance_metric <- "probability_threshold"
    y_performance_metric <- "NB_interventions_avoided"
  }


  performance_data_ready_for_curve <- performance_data |>
    prepare_performance_data_for_curve(
      x_performance_metric,
      y_performance_metric,
      stratified_by,
      perf_dat_type,
      min_p_threshold,
      max_p_threshold
    )

  axis_ranges <- extract_axes_ranges(
    performance_data_ready_for_curve,
    curve,
    min_p_threshold,
    max_p_threshold
  )

  create_reference_lines_for_plotly_new(
    curve,
    prevalence_from_performance_data,
    reference_group_colors = reference_group_colors_vec,
    perf_dat_type,
    size,
    min_p_threshold,
    max_p_threshold
  ) |>
    add_markers_and_lines_for_plotly_reference_object(
      performance_data_ready_for_curve, perf_dat_type
    ) |>
    set_styling_for_rtichoke(curve,
      min_x_range = axis_ranges$xaxis[1],
      max_x_range = axis_ranges$xaxis[2],
      min_y_range = axis_ranges$yaxis[1],
      max_y_range = axis_ranges$yaxis[2]
    ) |>
    plotly::animation_slider(
      currentvalue = list(
        prefix = ifelse(stratified_by == "probability_threshold",
          "Prob. Threshold: ",
          "Predicted Positives (Rate):"
        ),
        font = list(color = "black"),
        xanchor = "left"
      ),
      pad = list(t = 50)
    )
}



check_performance_type_by_probs_and_reals <- function(probs, reals) {
  if ((length(probs) == 1) & (length(reals) == 1)) {
    performance_type <- "one model"
  } else if ((length(probs) > 1) & (length(reals) == 1)) {
    performance_type <- "several models"
  } else {
    performance_type <- "several populations"
  }

  performance_type
}


extract_reference_groups_from_performance_data <- function(performance_data, perf_data_type) {
  if (perf_data_type == "several models") {
    reference_groups <- unique(performance_data$model)
  } else if (perf_data_type == "several populations") {
    reference_groups <- unique(performance_data$population)
  } else {
    reference_groups <- "model"
  }

  reference_groups
}





make_deciles_dat_new <- function(probs, reals) {
  data.frame(probs, reals) %>%
    dplyr::mutate(quintile = dplyr::ntile(probs, 10)) %>%
    dplyr::group_by(quintile) %>%
    dplyr::summarise(y = sum(reals) / n(), x = mean(probs), sum_reals = sum(reals), total_obs = n()) %>%
    dplyr::ungroup()
}

create_reference_group_color_vector <- function(reference_groups,
                                                perf_dat_type,
                                                col_values) {
  if (!(perf_dat_type %in% c("several populations", "several models"))) {
    col_values <- "black"
  }

  reference_group_color_vector <- c(
    "#BEBEBE",
    "#BEBEBE",
    "#BEBEBE",
    "#BEBEBE",
    col_values[1:length(reference_groups)],
    col_values[1:length(reference_groups)],
    col_values[1:length(reference_groups)], # fix to grey when one population
    col_values[1:length(reference_groups)]
  )

  names(reference_group_color_vector) <- c(
    "reference_line",
    "reference_line_perfect_model",
    "reference_line_treat_none",
    "reference_line_treat_all",
    paste0("reference_line_treat_none_", reference_groups),
    paste0("reference_line_treat_all_", reference_groups),
    paste0("reference_line_perfect_model_", reference_groups),
    reference_groups
  )

  reference_group_color_vector
}

prepare_performance_data_for_curve <- function(performance_data,
                                               x_performance_metric,
                                               y_performance_metric,
                                               stratified_by,
                                               perf_dat_type,
                                               min_p_threshold = 0,
                                               max_p_threshold = 1) {
  performance_data %>%
    {
      if (y_performance_metric == "NB_interventions_avoided") {
        dplyr::mutate(.,
          N = TP + TN + FP + FN,
          prevalence = (TP + FN) / N,
          NB_intervention_all = prevalence - (1 - prevalence) *
            (probability_threshold) / (1 - probability_threshold),
          NB_interventions_avoided = 100 * (NB - NB_intervention_all) *
            ((1 - probability_threshold) / probability_threshold)
        )
      } else {
        .
      }
    } %>%
    {
      if (stratified_by == "probability_threshold") {
        dplyr::filter(., probability_threshold <= max_p_threshold) |>
          dplyr::filter(probability_threshold >= min_p_threshold)
      } else {
        .
      }
    } |>
    add_hover_text_to_performance_data_new(
      x_performance_metric, y_performance_metric, stratified_by, perf_dat_type
    ) |>
    select_and_rename_necessary_variables(
      x_performance_metric,
      y_performance_metric,
      stratified_by,
      perf_dat_type
    ) %>%
    {
      if (y_performance_metric %in% c("NB", "NB_interventions_avoided")) {
        dplyr::filter(., !is.nan(y))
      } else {
        .
      }
    }
}


add_hover_text_to_performance_data_new <- function(performance_data,
                                                   performance_metric_x,
                                                   performance_metric_y,
                                                   stratified_by,
                                                   perf_dat_type) {
  hover_text <- create_hover_text(
    stratified_by = stratified_by,
    interventions_avoided = (performance_metric_y == "NB_interventions_avoided")
  ) |>
    make_two_performance_metrics_bold(
      performance_metric_x,
      performance_metric_y
    ) %>%
    {
      if (perf_dat_type == "several models") {
        add_models_for_text_for_hover_new(.)
      } else {
        .
      }
    } %>%
    {
      if (perf_dat_type == "several populations") {
        add_population_for_text_for_hover_new(.)
      } else {
        .
      }
    }

  performance_data %>%
    dplyr::mutate(
      dplyr::across(where(is.numeric), round, 3),
      text = glue::glue(hover_text),
      text = stringr::str_replace_all(text, pattern = "NaN", "")
    )
}

add_models_for_text_for_hover_new <- function(text_for_hover) {
  paste("<b>Model: {model}</b>",
    text_for_hover,
    sep = "<br>"
  )
}

add_population_for_text_for_hover_new <- function(text_for_hover) {
  paste("<b>Population: {population}</b>",
    text_for_hover,
    sep = "<br>"
  )
}


create_hover_text <- function(stratified_by, interventions_avoided = FALSE) {
  if (interventions_avoided == FALSE) {
    text_for_hover <- paste0(
      "Prob. Threshold: {probability_threshold}<br>\\
Sensitivity: {sensitivity}<br>\\
1 - Specificity (FPR): {FPR}<br>\\
Specificity: {specificity}<br>\\
Lift: {lift}<br>\\
PPV: {PPV}<br>\\
NPV: {NPV}<br>",
      ifelse(stratified_by == "probability_threshold",
        "NB: {NB}<br>\\
Odds of Prob. Threshold: 1:{round((1 - probability_threshold) / probability_threshold, digits = 2)}<br>",
        ""
      ),
      "Predicted Positives: {predicted_positives} ({100 * ppcr}%)<br>\\
TP: {TP}<br>\\
TN: {TN}<br>\\
FP: {FP}<br>\\
FN: {FN}"
    )
  } else {
    text_for_hover <- "Prob. Threshold: {probability_threshold}<br>\\
Odds of Prob. Threshold: 1:{round((1 - probability_threshold) / probability_threshold, digits = 2)}<br>\\
Interventions Avoided (per 100): {NB_interventions_avoided}<br>\\
NB: {NB}<br>\\
Predicted Positives: {predicted_positives} ({100 * ppcr}%)<br>\\
TN: {TN}<br>\\
FN: {FN}"
  }

  text_for_hover
}


select_and_rename_necessary_variables <- function(performance_data,
                                                  x_perf_metric,
                                                  y_perf_metric,
                                                  stratified_by,
                                                  perf_dat_type) {
  x_perf_metric <- sym(x_perf_metric)
  y_perf_metric <- sym(y_perf_metric)
  stratified_by <- sym(stratified_by)


  if (!(perf_dat_type %in% c("several populations", "several models"))) {
    performance_data <- performance_data |>
      dplyr::mutate(model = "model")
  }

  if (perf_dat_type == "several populations") {
    reference_group <- sym("population")
  } else {
    reference_group <- sym("model")
  }


  performance_data |>
    dplyr::select(
      reference_group = {{ reference_group }},
      x = {{ x_perf_metric }},
      y = {{ y_perf_metric }},
      stratified_by = {{ stratified_by }},
      text
    )
}

add_markers_and_lines_for_plotly_reference_object <- function(plotly_object,
                                                              performance_data_ready,
                                                              perf_dat_type) {
  interactive_marker <- list(
    size = 12,
    line = list(
      width = 3,
      color = I("black")
    )
  )

  if (!(perf_dat_type %in% c("several models", "several populations"))) {
    interactive_marker$color <- "#f6e3be"
  }

  plotly_object |>
    plotly::add_trace(
      data = performance_data_ready,
      type = "scatter",
      mode = "lines+markers",
      line = list(dash = "solid")
    ) |>
    plotly::add_markers(
      data = performance_data_ready,
      frame = ~stratified_by,
      marker = interactive_marker
    )
}

create_reference_lines_for_plotly_new <- function(curve,
                                                  prevalence,
                                                  reference_group_colors,
                                                  perf_dat_type,
                                                  size,
                                                  min_p_threshold,
                                                  max_p_threshold) {
  size_height <- switch(is.null(size) + 1,
    size + 50,
    NULL
  )

  plot_ly(
    x = ~x,
    y = ~y,
    height = size_height,
    width = size,
    hoverinfo = "text",
    text = ~text,
    color = ~reference_group,
    colors = reference_group_colors
  ) |>
    plotly::add_lines(
      data = create_reference_lines_data(
        curve, prevalence,
        perf_dat_type,
        min_p_threshold,
        max_p_threshold
      ),
      line = list(
        dash = "dot"
      )
    )
}

create_reference_lines_data <- function(curve, prevalence,
                                        perf_dat_type,
                                        min_p_threshold,
                                        max_p_threshold) {
  check_curve_input(curve)

  if (curve == "roc") {
    reference_lines_data <- data.frame(
      reference_group = "reference_line",
      x = seq(0, 1, by = 0.01),
      y = seq(0, 1, by = 0.01)
    ) |>
      dplyr::mutate(
        text =
          glue::glue(
            "<b>Random Guess</b><br>Sensitivity: {y}<br>1 - Specificity: {x}"
          )
      )
  }

  if (curve == "lift") {
    reference_lines_data <- data.frame(
      reference_group = "reference_line",
      x = seq(0.01, 1, by = 0.01),
      y = rep(1, 100)
    ) |>
      dplyr::mutate(
        text =
          glue::glue(
            "<b>Random Guess</b><br>Lift: {y}<br>Predicted Positives: {100*x}%"
          )
      )
  }

  if (curve == "precision recall") {
    if (perf_dat_type == "several populations") {
      reference_group <- rep(names(prevalence), each = 100)
      reference_line_x_values <- rep(seq(0.01, 1, by = 0.01), times = length(prevalence))
      reference_line_y_values <- rep(unlist(prevalence), each = 100)
      hover_text <- "<b>Random Guess ({reference_group})</b><br>PPV: {round(y, digits = 3)}<br>Sensitivity: {x}"
    } else {
      reference_group <- "reference_line"
      reference_line_x_values <- seq(0.01, 1, by = 0.01)
      reference_line_y_values <- rep(unique(unlist(prevalence)), 100)
      hover_text <- "<b>Random Guess</b><br>PPV: {round(y, digits = 3)}<br>Sensitivity: {x}"
    }


    reference_lines_data <- data.frame(
      reference_group = reference_group,
      x = reference_line_x_values,
      y = reference_line_y_values
    ) |>
      dplyr::mutate(text = glue::glue(hover_text))
  }

  if (curve == "gains") {
    hover_text_random <- "<b>Random Guess</b><br>PPV: {round(y, digits = 3)}<br>Sensitivity: {x}"

    if (perf_dat_type == "several populations") {
      reference_group <- rep(c("reference_line", paste0("reference_line_perfect_model_", names(prevalence))), each = 101)
      reference_line_x_values <- rep(seq(0, 1, by = 0.01), times = (length(prevalence) + 1))
      reference_line_y_values <- c(
        seq(0, 1, by = 0.01),
        prevalence |>
          purrr::map(~ return_perfect_prediction_gains_y_values(.x)) |>
          unlist()
      )
      hover_text_perfect <- "<b>Perfect Prediction ({reference_group})</b><br>PPV: {round(y, digits = 3)}<br>Sensitivity: {x}"
    } else {
      reference_group <- rep(c("reference_line", "reference_line_perfect_model"), each = 101)
      reference_line_x_values <- rep(seq(0, 1, by = 0.01), times = 2)
      reference_line_y_values <- c(
        seq(0, 1, by = 0.01),
        return_perfect_prediction_gains_y_values(unique(unlist(prevalence)))
      )

      hover_text_perfect <- "<b>Perfect Prediction</b><br>PPV: {round(y, digits = 3)}<br>Sensitivity: {x}"
    }

    reference_lines_data <- data.frame(
      reference_group = reference_group,
      x = reference_line_x_values,
      y = reference_line_y_values
    ) |>
      dplyr::mutate(
        text = dplyr::case_when(
          reference_group == "reference_line" ~ glue::glue(hover_text_random),
          TRUE ~ glue::glue(hover_text_perfect)
        ),
        text = stringr::str_replace(.data$text, "reference_line_perfect_model_", "")
      )
  }

  if (curve == "decision") {
    if (perf_dat_type == "several populations") {
      reference_group <- rep(
        c(
          "reference_line",
          paste0("reference_line_treat_all_", names(prevalence))
        ),
        each = 100
      )
      reference_line_x_values <- rep(seq(0, 0.99, by = 0.01), times = (length(prevalence) + 1))
      reference_line_y_values <- c(
        rep(0, 100),
        prevalence |>
          purrr::map(~ return_treat_all_y_values(.x)) |>
          unlist()
      )

      hover_text_treat_all <- "<b>Treat All ({reference_group})</b><br>NB: {round(y, digits = 3)}<br>Probability Threshold: {x}<br>Odds of Prob. Threshold: 1:{round((1 - x) / x, digits = 2)}"
    } else {
      reference_group <- rep(c("reference_line", "reference_line_treat_all"), each = 100)
      reference_line_x_values <- rep(seq(0, 0.99, by = 0.01), times = 2)
      reference_line_y_values <- c(rep(0, 100), c(unique(unlist(prevalence)) - (1 - unique(unlist(prevalence))) *
        (seq(0, 0.99, by = 0.01)) / (1 - seq(0, 0.99, by = 0.01))))
      hover_text_treat_all <- "<b>Treat All</b><br>NB: {round(y, digits = 3)}<br>Probability Threshold: {x}<br>Odds of Prob. Threshold: 1:{round((1 - x) / x, digits = 2)}"
    }

    hover_text_treat_none <- "<b>Treat None</b><br>NB: 0<br>Probability Threshold: {x}<br>Odds of Prob. Threshold: 1:{round((1 - x) / x, digits = 2)}"


    reference_lines_data <- data.frame(
      reference_group = reference_group,
      x = reference_line_x_values,
      y = reference_line_y_values
    ) |>
      dplyr::mutate(
        text = dplyr::case_when(
          reference_group == "reference_line" ~ glue::glue(hover_text_treat_none),
          TRUE ~ glue::glue(hover_text_treat_all)
        ),
        text = stringr::str_replace(.data$text, "reference_line_treat_all_", "")
      ) |>
      dplyr::filter(x >= min_p_threshold, x <= max_p_threshold)
  }

  if (curve == "interventions avoided") {
    if (perf_dat_type == "several populations") {
      reference_group <- rep(
        c(
          "reference_line",
          paste0("reference_line_treat_none_", names(prevalence))
        ),
        each = 99
      )
      reference_line_x_values <- rep(seq(0.01, 0.99, by = 0.01), times = (length(prevalence) + 1))
      reference_line_y_values <- 100 * c(
        rep(0, 99),
        prevalence |>
          purrr::map(~ return_treat_none_y_values(.x)) |>
          unlist()
      )

      hover_text_treat_none <- "<b>Treat None ({reference_group})</b><br>Interventions Avoided (per 100): {round(y, digits = 3)}<br>Probability Threshold: {x}<br>Odds of Prob. Threshold: 1:{round((1 - x) / x, digits = 2)}"
    } else {
      reference_group <- rep(c("reference_line", "reference_line_treat_none"), each = 99)
      reference_line_x_values <- rep(seq(0.01, 0.99, by = 0.01), times = 2)
      reference_line_y_values <- 100 * c(rep(0, 99), c(1 - unique(unlist(prevalence)) - unique(unlist(prevalence)) *
        (1 - seq(0.01, 0.99, by = 0.01)) / (seq(0.01, 0.99, by = 0.01))))
      hover_text_treat_none <- "<b>Treat None</b><br>Interventions Avoided (per 100): {round(y, digits = 3)}<br>Probability Threshold: {x}<br>Odds of Prob. Threshold: 1:{round((1 - x) / x, digits = 2)}"
    }

    hover_text_treat_all <- "<b>Treat All</b><br>Interventions Avoided (per 100): 0<br>Probability Threshold: {x}<br>Odds of Prob. Threshold: 1:{round((1 - x) / x, digits = 2)}"

    reference_lines_data <- data.frame(
      reference_group = reference_group,
      x = reference_line_x_values,
      y = reference_line_y_values
    ) |>
      dplyr::mutate(
        text = dplyr::case_when(
          reference_group == "reference_line" ~ glue::glue(hover_text_treat_all),
          TRUE ~ glue::glue(hover_text_treat_none)
        ),
        text = stringr::str_replace(.data$text, "reference_line_treat_none_", "")
      ) |>
      dplyr::filter(.data$x >= min_p_threshold, .data$x <= max_p_threshold)
  }


  reference_lines_data
}


return_perfect_prediction_gains_y_values <- function(prevalence) {
  c(
    seq(0, 1, length.out = (100 * (round(prevalence, digits = 3)) + 1)),
    rep(1, (100 - 100 * round(prevalence, digits = 3)))
  )
}



extract_axes_ranges <- function(performance_data_ready, curve,
                                min_p_threshold,
                                max_p_threshold) {
  if (curve %in% c("lift", "decision", "interventions avoided")) {
    max_y_range <- max(performance_data_ready$y, na.rm = TRUE)
  }

  if (curve %in% c("decision", "interventions avoided")) {
    min_x_range <- min_p_threshold
    max_x_range <- max_p_threshold

    min_y_range <- min(c(performance_data_ready$y), 0)
  }



  if (curve == "roc") {
    curve_axis_range <- list(xaxis = c(0, 1), yaxis = c(0, 1))
  }
  if (curve == "lift") {
    curve_axis_range <- list(xaxis = c(0, 1), yaxis = c(0, max_y_range))
  }
  if (curve == "precision recall") {
    curve_axis_range <- list(xaxis = c(0, 1), yaxis = c(0, 1))
  }
  if (curve == "gains") {
    curve_axis_range <- list(xaxis = c(0, 1), yaxis = c(0, 1))
  }
  if (curve == "decision") {
    curve_axis_range <- list(
      xaxis = c(
        min_x_range,
        max_x_range
      ),
      yaxis = c(
        min_y_range,
        max_y_range
      )
    )
  }
  if (curve == "interventions avoided") {
    curve_axis_range <- list(
      xaxis = c(
        min_x_range,
        max_x_range
      ),
      yaxis = c(
        min(0, min_y_range),
        100
      )
    )
  }

  curve_axis_range |> purrr::map(~ extand_axis_range(.x))
}

extand_axis_range <- function(axis_range, extand_range_by = 1.1) {
  margin <- (extand_range_by - 1) * diff(axis_range)

  c(axis_range[1] - margin, axis_range[2] + margin)
}


return_treat_all_y_values <- function(prevalence) {
  c(prevalence - (1 - unique(prevalence)) *
    (seq(0, 0.99, by = 0.01)) / (1 - seq(0, 0.99, by = 0.01)))
}

return_treat_none_y_values <- function(prevalence) {
  c(1 - prevalence - (unique(prevalence)) *
    (1 - seq(0.01, 0.99, by = 0.01)) / (seq(0.01, 0.99, by = 0.01)))
}


prepare_performance_data_for_interactive_marker <- function(
    performance_data_ready_for_curve, perf_dat_type) {
  
  performance_data_for_interactive_marker <- performance_data_ready_for_curve
  
  performance_data_for_interactive_marker$y[is.nan(performance_data_for_interactive_marker$y)] <- -1
  performance_data_for_interactive_marker$x[is.nan(performance_data_for_interactive_marker$x)] <- -1
  
  if ( perf_dat_type %in% c("several models", "several populations") ) {
    
    performance_data_for_interactive_marker |> 
      split(~reference_group) |> 
      purrr::map_df(check_zero_variance_for_sub_population)
    
  } else {
    
    performance_data_for_interactive_marker
    
  }
  
}


check_zero_variance_for_sub_population <- function(performance_data_sup_population) {
  
  if ( nrow(performance_data_sup_population) == 2 ) {
    
    dplyr::bind_rows(
      performance_data_sup_population[1, ],
      data.frame(
        reference_group = rep(
          unique(performance_data_sup_population$reference_group), 
          1 / 0.01 - 1),
        x = seq(0 + 0.01, 1 - 0.01, by = 0.01),
        y = rep(-1, 1 / 0.01 - 1)
      ) |> 
        dplyr::mutate(
          stratified_by = x,
          text = NA
        ),
      performance_data_sup_population[2, ]
    )
    
  } else {
    
    performance_data_sup_population
    
  }
  
}