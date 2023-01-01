#' Creating html bar
#'
#' @param value original value
#' @param display displayed value
#' @param color color of the bar
#' @param digits number of digits
#' @param no_round no rounding
#'
#' @keywords internal
bar_chart <- function(value,
                      display,
                      color = "red",
                      digits = 0,
                      no_round = FALSE) {
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

bar_style_nb <- function(width, display) {
  if (is.na(width) | is.nan(width)) {
    NA
  } else {
    position <- paste0((0.5 + width / 2) * 100, "%")

    display_rounded <- round(display, digits = 2) %>%
      format(nsmall = 2)

    if (width <= 0) {
      fill <- "pink"
      html_code <- glue::glue("<span style=\"display: \\
      inline-block; background: linear-gradient(90deg,
             transparent {position}, {fill} {position}, {fill} 50%,
             transparent 50%) center center / 98% 88% no-repeat; \\
             border-radius: 4px;
             flex: 100 0 auto; width: 100px;\">{display_rounded}</span>")
    } else {
      fill <- "lightgreen"
      html_code <- glue::glue("<span style=\"display: \\
      inline-block;background: linear-gradient(90deg,
             transparent 50%, {fill} 50%, {fill} {position}, \\
             transparent {position}) center center / 98% 88% no-repeat;
             border-radius: 4px; flex: 100 0 auto; \\
                              width: 100px;\">{display_rounded}</span>")
    }

    html_code %>%
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
#' @keywords internal
add_color_to_confusion_metric <- function(performance_dat,
                                          metric,
                                          color) {
  performance_dat %>%
    dplyr::mutate(
      metric_plot = 100 * {{ metric }} / .data$n_obs,
      metric_plot = purrr::map2(
        metric_plot, {{ metric }},
        .f = ~ bar_chart(
          value = .x,
          display = .y,
          color = color,
          digits = 0
        )
      )
    ) %>%
    dplyr::mutate({{ metric }} := .data$metric_plot) %>%
    dplyr::select(-.data$metric_plot)
}


#' Adding color to performance metric
#'
#' @param performance_dat the original performance data
#' @param metric the required metric
#' @param color the required color
#'
#' @keywords internal
add_color_to_performance_metric <- function(performance_dat,
                                            metric,
                                            color) {
  performance_dat %>%
    dplyr::mutate(
      metric_plot = 100 * {{ metric }},
      metric_plot = purrr::map2(
        metric_plot, {{ metric }},
        .f = ~ bar_chart(
          value = .x,
          display = .y,
          color = color,
          digits = 2
        )
      )
    ) %>%
    dplyr::mutate({{ metric }} := metric_plot) %>%
    dplyr::select(-metric_plot)
}

#' Adding color to the lift metric
#'
#' @param performance_dat the original performance data
#' @param metric the required metric
#' @param color the required color
#'
#' @keywords internal
add_color_to_lift <- function(performance_dat,
                              metric,
                              color) {
  performance_dat %>%
    dplyr::mutate(
      metric_plot = 100 * {{ metric }} / max({{ metric }}, na.rm = TRUE),
      metric_plot = purrr::map2(
        metric_plot, {{ metric }},
        .f = ~ bar_chart(
          value = .x,
          display = .y,
          color = color,
          digits = 2
        )
      )
    ) %>%
    dplyr::mutate({{ metric }} := .data$metric_plot) %>%
    dplyr::select(-metric_plot)
}

add_color_to_predicted_positives <- function(performance_dat) {
  performance_dat %>%
    mutate(
      display_predicted_postivies =
        glue::glue("{predicted_positives} ({round(ppcr  * 100, digits = 1)}%)"),
      plot_predicted_positives = 100 * .data$ppcr,
      plot_predicted_positives = purrr::map2(
        plot_predicted_positives, display_predicted_postivies,
        .f = ~ bar_chart(
          value = .x,
          display = .y,
          color = "lightgrey",
          no_round = TRUE
        )
      )
    )
}


#' Adding Color to Net Benifit metric
#'
#' @keywords internal
#' @inheritParams plot_roc_curve
add_color_to_net_benifit <- function(performance_data) {
  performance_data %>%
    dplyr::mutate(
      NB_plot = .data$NB,
      NB_plot = purrr::map2(
        NB_plot, NB,
        .f = ~ bar_style_nb(width = .x, display = .y)
      )
    ) %>%
    dplyr::mutate(NB = .data$NB_plot) %>%
    dplyr::select(-.data$NB_plot)
}



#' Replancing NaN with NA
#'
#' @param performance_dat the original performance data
#'
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
#' @keywords internal
add_colors_to_performance_dat <- function(performance_dat) {
  n_obs_dat <- get_n_from_performance_data(performance_dat)

  # print(n_obs_dat)

  performance_dat %>%
    dplyr::mutate(key = "key") %>%
    dplyr::left_join(n_obs_dat %>% mutate(key = "key")) %>%
    dplyr::select(-.data$key) %>%
    add_color_to_confusion_metric(.data$TP, "lightgreen") %>%
    add_color_to_confusion_metric(.data$TN, "lightgreen") %>%
    add_color_to_confusion_metric(.data$FP, "pink") %>%
    add_color_to_confusion_metric(.data$FN, "pink") %>%
    select(-.data$n_obs) %>%
    add_color_to_performance_metric(.data$sensitivity, "lightgreen") %>%
    add_color_to_lift(.data$lift, "lightgreen") %>%
    add_color_to_predicted_positives() %>%
    add_color_to_performance_metric(.data$specificity, "lightgreen") %>%
    add_color_to_performance_metric(.data$PPV, "lightgreen") %>%
    add_color_to_performance_metric(.data$NPV, "lightgreen") %>%
    add_color_to_net_benifit()
}
