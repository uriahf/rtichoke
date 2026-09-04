#' Create a Calibration Curve
#'
#' @inheritParams create_roc_curve
#' @param type discrete or smooth
#' @param n_bins Number of calibration bins for binned calibration (default 10).
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' create_calibration_curve(
#'   probs = list(example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome), type = "discrete"
#' )
#'
#'
#' create_calibration_curve(
#'   probs = list(example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome), type = "smooth"
#' )
#'
#' # Several Models
#'
#' create_calibration_curve(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   reals = list(example_dat$outcome),
#'   type = "discrete"
#' )
#'
#'
#' create_calibration_curve(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   reals = list(example_dat$outcome),
#'   type = "smooth"
#' )
#'
#'
#' # Several Populations
#'
#' create_calibration_curve(
#'   probs = list(
#'     "train" = example_dat |>
#'       dplyr::filter(type_of_set == "train") |>
#'       dplyr::pull(estimated_probabilities),
#'     "test" = example_dat |> dplyr::filter(type_of_set == "test") |>
#'       dplyr::pull(estimated_probabilities)
#'   ),
#'   reals = list(
#'     "train" = example_dat |> dplyr::filter(type_of_set == "train") |>
#'       dplyr::pull(outcome),
#'     "test" = example_dat |> dplyr::filter(type_of_set == "test") |>
#'       dplyr::pull(outcome)
#'   ),
#'   type = "discrete"
#' )
#'
#'
#' create_calibration_curve(
#'   probs = list(
#'     "train" = example_dat |>
#'       dplyr::filter(type_of_set == "train") |>
#'       dplyr::pull(estimated_probabilities),
#'     "test" = example_dat |> dplyr::filter(type_of_set == "test") |>
#'       dplyr::pull(estimated_probabilities)
#'   ),
#'   reals = list(
#'     "train" = example_dat |> dplyr::filter(type_of_set == "train") |>
#'       dplyr::pull(outcome),
#'     "test" = example_dat |> dplyr::filter(type_of_set == "test") |>
#'       dplyr::pull(outcome)
#'   ),
#'   type = "smooth"
#' )
#' }
#'
create_calibration_curve <- function(
  probs,
  reals,
  interactive = TRUE,
  color_values = c(
    "#1b9e77",
    "#d95f02",
    "#7570b3",
    "#e7298a",
    "#07004D",
    "#E6AB02",
    "#FE5F55",
    "#54494B",
    "#006E90",
    "#BC96E6",
    "#52050A",
    "#1F271B",
    "#BE7C4D",
    "#63768D",
    "#08A045",
    "#320A28",
    "#82FF9E",
    "#2176FF",
    "#D1603D",
    "#585123"
  ),
  type = "discrete",
  size = NULL,
  n_bins = 10
) {
  check_probs_input(probs)
  check_real_input(reals)

  # Validate n_bins even for smooth calls
  if (
    !is.numeric(n_bins) ||
      length(n_bins) != 1L ||
      is.na(n_bins) ||
      !is.finite(n_bins) ||
      n_bins <= 0 ||
      (n_bins %% 1 != 0)
  ) {
    stop("`n_bins` must be a single positive whole number.", call. = FALSE)
  }

  effective_n_bins <- if (type == "discrete") n_bins else 10

  calibration_curve_list <- create_calibration_curve_list(
    probs = probs,
    reals = reals,
    color_values = color_values,
    size = size,
    n_bins = effective_n_bins
  )

  if (interactive == TRUE) {
    calibration_curve <- calibration_curve_list |>
      create_plotly_curve_from_calibration_curve_list(type = type)
  } else {
    calibration_curve <- calibration_curve_list |>
      create_ggplot_curve_from_calibration_curve_list(type = type)
  }

  calibration_curve
}


#' Define limits for Calibration Curve
#'
#' @param calibration_bins_dat A data frame containing bin-level calibration data.
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' make_calibration_bins_dat(
#'   probs = example_dat$estimated_probabilities,
#'   reals = example_dat$outcome
#' ) |>
#'   define_limits_for_calibration_plot()
#' }
define_limits_for_calibration_plot <- function(calibration_bins_dat) {
  if (nrow(calibration_bins_dat) == 1) {
    l <- 0
    u <- 1
  } else {
    l <- max(0, min(calibration_bins_dat$x, calibration_bins_dat$y))
    u <- max(calibration_bins_dat$x, calibration_bins_dat$y)
  }

  limits <- c(
    l - (u - l) * 0.05,
    u + (u - l) * 0.05
  )

  limits
}


#' Create a Calibration Curve List
#'
#' @inheritParams create_roc_curve
#' @param n_bins Number of calibration bins for binned calibration (default 10).
#'
#' @export
#'
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' create_calibration_curve_list(
#'   probs = list(example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome)
#' )
#'
#' # Several Models
#'
#' create_calibration_curve_list(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   reals = list(example_dat$outcome)
#' )
#'
#'
#' # Several Populations
#'
#'
#' create_calibration_curve_list(
#'   probs = list(
#'     "train" = example_dat |>
#'       dplyr::filter(type_of_set == "train") |>
#'       dplyr::pull(estimated_probabilities),
#'     "test" = example_dat |> dplyr::filter(type_of_set == "test") |>
#'       dplyr::pull(estimated_probabilities)
#'   ),
#'   reals = list(
#'     "train" = example_dat |> dplyr::filter(type_of_set == "train") |>
#'       dplyr::pull(outcome),
#'     "test" = example_dat |> dplyr::filter(type_of_set == "test") |>
#'       dplyr::pull(outcome)
#'   )
#' )
#' }
create_calibration_curve_list <- function(
  probs,
  reals,
  color_values = c(
    "#1b9e77",
    "#d95f02",
    "#7570b3",
    "#e7298a",
    "#07004D",
    "#E6AB02",
    "#FE5F55",
    "#54494B",
    "#006E90",
    "#BC96E6",
    "#52050A",
    "#1F271B",
    "#BE7C4D",
    "#63768D",
    "#08A045",
    "#320A28",
    "#82FF9E",
    "#2176FF",
    "#D1603D",
    "#585123"
  ),
  size = NULL,
  n_bins = 10
) {
  check_probs_input(probs)
  check_real_input(reals)

  if (is.null(names(probs))) {
    names(probs) <- "model"
  }

  reference_groups <- names(probs)

  calibration_curve_list <- list()

  calibration_curve_list$performance_type <- check_performance_type_by_probs_and_reals(
    probs,
    reals
  )

  calibration_curve_list$size <- list(size)

  group_colors_vec <- create_reference_group_color_vector(
    reference_groups,
    calibration_curve_list$performance_type,
    color_values
  ) |>
    as.list()

  # Create Calibration Bins Dat

  if (calibration_curve_list$performance_type == "several populations") {
    calibration_curve_list$calibration_bins_dat <- purrr::map2_dfr(
      probs,
      reals,
      ~ make_calibration_bins_dat(.x, .y, n_bins = n_bins),
      .id = "reference_group"
    )

    calibration_curve_list$smooth_dat <- purrr::map2_dfr(
      probs,
      reals,
      function(x, y) {
        if (length(unique(x)) == 1) {
          list("x" = unique(x), y = mean(y))
        } else {
          lowess(x, y, iter = 0) |>
            approx(
              xout = seq(0, 1, by = 0.01),
              ties = mean
            )
        }
      },
      .id = "reference_group"
    ) |>
      stats::na.omit()
  } else {
    calibration_curve_list$calibration_bins_dat <- purrr::map_df(
      probs,
      ~ make_calibration_bins_dat(.x, reals[[1]], n_bins = n_bins),
      .id = "reference_group"
    )

    calibration_curve_list$smooth_dat <- purrr::map_df(
      probs,
      reals = reals,
      function(x, reals) {
        if (length(unique(x)) == 1) {
          list("x" = unique(x), y = mean(reals[[1]]))
        } else {
          lowess(x, reals[[1]], iter = 0) |>
            approx(
              xout = seq(0, 1, by = 0.01),
              ties = mean
            )
        }
      },
      .id = "reference_group"
    )
  }

  hover_text_for_discrete_calibration <- "Predicted: {round(x, digits = 3)}<br>Observed: {round(y, digits = 3)}"

  if (calibration_curve_list$performance_type != "one model") {
    hover_text_for_discrete_calibration <- paste0(
      "<b>{reference_group}</b><br>",
      hover_text_for_discrete_calibration
    )
  }

  calibration_curve_list$calibration_bins_dat <- calibration_curve_list$calibration_bins_dat |>
    dplyr::mutate(
      text = glue::glue(paste0(
        hover_text_for_discrete_calibration,
        " ( {sum_reals} / {total_obs} )"
      ))
    )

  calibration_curve_list$smooth_dat <- calibration_curve_list$smooth_dat |>
    dplyr::mutate(
      text = glue::glue(hover_text_for_discrete_calibration)
    )

  calibration_curve_list$group_colors_vec <- group_colors_vec

  limits <- define_limits_for_calibration_plot(
    calibration_curve_list$calibration_bins_dat
  )

  calibration_curve_list$axes_ranges <- list(xaxis = limits, yaxis = limits)

  calibration_curve_list$reference_data <- data.frame(
    reference_group = "reference_line",
    x = seq(0, 1, by = 0.01),
    y = seq(0, 1, by = 0.01)
  ) |>
    dplyr::mutate(
      text = glue::glue(
        "<b>Perfectly Calibrated</b><br>Predicted: {x}<br>Observed: {y}"
      )
    )

  calibration_curve_list$histogram_for_calibration <- probs |>
    purrr::map_df(
      ~ hist(
        .x,
        plot = FALSE,
        breaks = seq(0, 1, 0.01)
      ) |>
        (\(histogram) histogram[c("mids", "counts")])(),
      .id = "reference_group"
    ) |>
    dplyr::mutate(
      text_obs = glue::glue("{counts} observations in "),
      text_range = ifelse(
        mids == 0.005,
        "[0,0.01]",
        glue::glue("( {mids - 0.005} , {mids + 0.005} ]")
      ),
      text = glue::glue("{text_obs}{text_range}")
    )

  calibration_curve_list$histogram_opacity <- 1 / length(probs)

  calibration_curve_list
}


create_plotly_curve_from_calibration_curve_list <- function(
  calibration_curve_list,
  type = "discrete"
) {
  calibration_curve <- plotly::plot_ly(
    x = ~x,
    y = ~y,
    width = calibration_curve_list$size[[1]],
    height = calibration_curve_list$size[[1]],
    hoverinfo = "text",
    text = ~text,
    color = ~reference_group,
    colors = unlist(calibration_curve_list$group_colors_vec),
    legendgroup = ~reference_group
  ) |>
    plotly::add_lines(
      data = calibration_curve_list$reference_data,
      showlegend = FALSE,
      line = list(
        dash = "dot"
      )
    )

  if (type == "discrete") {
    calibration_curve <- calibration_curve |>
      plotly::add_trace(
        data = calibration_curve_list$calibration_bins_dat,
        type = "scatter",
        mode = "markers+lines",
        marker = list(
          size = 10
        ),
        showlegend = calibration_curve_list$performance_type != "one model"
      )
  } else {
    calibration_curve <- calibration_curve |>
      plotly::add_trace(
        data = calibration_curve_list$smooth_dat,
        type = "scatter",
        mode = "lines",
        showlegend = calibration_curve_list$performance_type != "one model"
      )
  }

  # Histogram

  histogram_for_calibration <- calibration_curve_list$histogram_for_calibration |>
    plotly::plot_ly() |>
    plotly::add_bars(
      x = ~mids,
      y = ~counts,
      showlegend = FALSE,
      opacity = calibration_curve_list$histogram_opacity,
      width = 0.01,
      color = ~reference_group,
      legendgroup = ~reference_group,
      colors = unlist(calibration_curve_list$group_colors_vec),
      text = ~text,
      hoverinfo = "text",
      textposition = "none"
    ) |>
    plotly::layout(
      barmode = "overlay",
      xaxis = list(showgrid = FALSE),
      yaxis = list(showgrid = FALSE),
      plot_bgcolor = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)"
    )

  plotly::subplot(
    calibration_curve,
    histogram_for_calibration,
    nrows = 2,
    shareX = TRUE,
    heights = c(0.8, 0.2)
  ) |>
    plotly::config(displayModeBar = FALSE) |>
    plotly::layout(
      xaxis = list(
        title = "Predicted",
        range = calibration_curve_list$axes_ranges$xaxis,
        showgrid = FALSE
      ),
      yaxis = list(
        title = "Observed",
        range = calibration_curve_list$axes_ranges$yaxis,
        showgrid = FALSE
      ),
      legend = list(
        orientation = "h",
        xanchor = "center",
        yanchor = "top",
        x = 0.5,
        y = 1.1
      )
    )
}


create_ggplot_curve_from_calibration_curve_list <- function(
  calibration_curve_list,
  type = "discrete"
) {
  if (type == "discrete") {
    calibration_curve <- ggplot2::ggplot(
      calibration_curve_list$calibration_bins_dat,
      ggplot2::aes(
        x = x,
        y = y,
        color = reference_group
      )
    ) +
      ggplot2::geom_abline(
        slope = 1,
        intercept = 0,
        linetype = "dashed",
        color = "grey"
      ) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::ylab("Observed") +
      ggplot2::labs(x = "Predicted") +
      ggplot2::theme_classic() +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      ) +
      ggplot2::coord_cartesian(
        xlim = calibration_curve_list$axes_ranges$xaxis,
        ylim = calibration_curve_list$axes_ranges$yaxis,
        expand = FALSE
      ) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::scale_colour_manual(
        values = unlist(calibration_curve_list$group_colors_vec)
      )
  } else {
    calibration_curve <- ggplot2::ggplot(
      calibration_curve_list$smooth_dat,
      ggplot2::aes(
        x = x,
        y = y,
        color = reference_group
      )
    ) +
      ggplot2::geom_abline(
        slope = 1,
        intercept = 0,
        linetype = "dashed",
        color = "grey"
      ) +
      ggplot2::geom_line() +
      ggplot2::ylab("Observed") +
      ggplot2::labs(x = "Predicted") +
      ggplot2::theme_classic() +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      ) +
      ggplot2::coord_cartesian(
        xlim = calibration_curve_list$axes_ranges$xaxis,
        ylim = calibration_curve_list$axes_ranges$yaxis,
        expand = FALSE
      ) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::scale_colour_manual(
        values = unlist(calibration_curve_list$group_colors_vec)
      )
  }

  histogram_for_calibration <- ggplot2::ggplot(
    data = calibration_curve_list$histogram_for_calibration,
    ggplot2::aes(x = mids, y = counts, fill = reference_group)
  ) +
    ggplot2::geom_col(
      position = "identity",
      alpha = calibration_curve_list$histogram_opacity
    ) +
    ggplot2::theme_classic() +
    ggplot2::coord_cartesian(
      xlim = calibration_curve_list$axes_ranges$xaxis,
      expand = FALSE
    ) +
    ggplot2::labs(x = "Predicted") +
    ggplot2::theme(axis.title.y = ggplot2::element_text(colour = "white")) +
    ggplot2::scale_fill_manual(
      values = unlist(calibration_curve_list$group_colors_vec)
    )

  patchwork::wrap_plots(
    calibration_curve +
      ggplot2::theme(
        legend.position = "none"
      ),
    histogram_for_calibration +
      ggplot2::theme(
        legend.position = "none"
      ),
    heights = c(3, 1)
  )
}


make_calibration_bins_dat <- function(
  probs,
  reals,
  n_bins = 10
) {
  if (
    !is.numeric(n_bins) ||
      length(n_bins) != 1L ||
      is.na(n_bins) ||
      !is.finite(n_bins) ||
      n_bins <= 0 ||
      (n_bins %% 1 != 0)
  ) {
    stop("`n_bins` must be a single positive whole number.", call. = FALSE)
  }

  if (length(unique(probs)) == 1) {
    tibble::tibble(
      bin = 1L,
      x = unique(probs),
      y = mean(reals),
      sum_reals = sum(reals),
      total_obs = length(reals)
    )
  } else {
    data.frame(probs, reals) |>
      dplyr::mutate(bin = dplyr::ntile(probs, n_bins)) |>
      dplyr::group_by(bin) |>
      dplyr::summarise(
        y = sum(reals) / n(),
        x = mean(probs),
        sum_reals = sum(reals),
        total_obs = n()
      ) |>
      dplyr::ungroup()
  }
}
