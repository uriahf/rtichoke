#' Create a Calibration Curve
#'
#' @inheritParams create_roc_curve
#' @param type discrete or smooth
#' @param histogram_included if TRUE a histogram will be added to the Calibration Curve
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
#'   ), 
#'   type = "discrete"
#' )
#'
#'
#' create_calibration_curve(
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
#'   ), 
#'   type = "smooth"
#' )
#' 
#' }
#' 
#' 
create_calibration_curve <- function(probs,
                                     reals,
                                     interactive = TRUE,
                                     col_values = c(
                                       "#5BC0BE",
                                       "#FC8D62",
                                       "#8DA0CB",
                                       "#E78AC3",
                                       "#A4243B"
                                     ),
                                     type = "discrete",
                                     size = NULL,
                                     histogram_included = TRUE) {
  quintile <- phatx <- phaty <- gam <- NULL

  check_probs_input(probs)
  # check_real_input(real)


  if (length(probs) == 1 & is.null(names(probs))) {
    names(probs) <- "model 1"
  }

  col_values <- col_values[seq_len(length(probs))]

  if (length(probs) >= 1 & length(reals) == 1) {
    
    
    deciles_dat <- tibble::tribble(
      ~model, ~quintile, ~phaty, ~phatx,
      "reference", NA, 0, 0,
      "reference", NA, 1, 1
    ) %>%
      dplyr::bind_rows(
        purrr::map_df(probs,
          ~ make_deciles_dat(.x, reals[[1]]),
          .id = "model"
        )
      ) %>%
      dplyr::mutate(model = forcats::fct_inorder(factor(model)))
    
  }


  if (length(probs) >= 1 & length(reals) > 1) {
    if (is.null(names(probs)) & is.null(names(reals))) {
      names(probs) <- paste("population", seq_len(length(probs)))
      names(reals) <- paste("population", seq_len(length(reals)))
    }

    deciles_dat <- tibble::tribble(
      ~population, ~quintile, ~phaty, ~phatx,
      "reference", NA, 0, 0,
      "reference", NA, 1, 1
    ) %>%
      dplyr::bind_rows(
        purrr::map2_dfr(probs,
          reals,
          ~ make_deciles_dat(.x, .y),
          .id = "population"
        )
      ) %>%
      dplyr::mutate(population = forcats::fct_inorder(factor(population)))
  }

  limits <- define_limits_for_calibration_plot(deciles_dat)

  if (type == "smooth") {

    smooth_dat <- create_dat_for_smooth_calibration(
      probs,
      reals = reals,
      deciles_dat
    )

    if (interactive == FALSE) {
      if ((length(probs) == 1) & (length(reals) == 1)) {
        cal_plot <- ggplot2::ggplot(
          smooth_dat,
          ggplot2::aes(
            x = x,
            y = y
          )
        ) +
          ggplot2::geom_abline(
            slope = 1,
            intercept = 0,
            color = "grey"
          ) +
          ggplot2::geom_line(size = 1) +
          ggplot2::ylab("Observed") +
          ggplot2::theme_classic() +
          ggplot2::theme(
            axis.title.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank()
          ) +
          ggplot2::coord_cartesian(
            xlim = limits,
            ylim = limits,
            expand = FALSE
          ) +
          ggplot2::theme(legend.position = "none")
      }

      if ((length(probs) > 1) & (length(reals) == 1)) {
        # print(head(smooth_dat))
        # print(col_values)
        #

        cal_plot <- ggplot2::ggplot(
          smooth_dat
        ) +
          ggplot2::geom_line(ggplot2::aes(
            x = x,
            y = y,
            color = model
          ),
          size = 1
          ) +
          ggplot2::ylab("Observed") +
          ggplot2::theme_classic() +
          ggplot2::theme(
            axis.title.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank()
          ) +
          ggplot2::coord_cartesian(
            xlim = limits,
            ylim = limits,
            expand = FALSE
          ) +
          ggplot2::scale_color_manual(values = c("grey", unname(col_values)))
      }

      if ((length(probs) > 1) & (length(reals) > 1)) {
        
        cal_plot <- ggplot2::ggplot(
          smooth_dat
        ) +
          ggplot2::geom_line(ggplot2::aes(
            x = x,
            y = y,
            color = population
          ),
          size = 1
          ) +
          ggplot2::ylab("Observed") +
          ggplot2::theme_classic() +
          ggplot2::theme(
            axis.title.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank()
          ) +
          ggplot2::coord_cartesian(
            xlim = limits,
            ylim = limits,
            expand = FALSE
          ) +
          ggplot2::scale_color_manual(values = c("grey", unname(col_values)))
        
        print(cal_plot)
        
      }
    }

    if (interactive == TRUE) {
      if ((length(probs) == 1) & (length(reals) == 1)) {

        # print("one population")

        cal_plot <- plotly::plot_ly(
          x = ~x,
          y = ~y,
          hovertemplate = paste0(
            "<b>%{xaxis.title.text}:</b> %{x:.2f}<br>",
            "<b>%{yaxis.title.text}:</b> %{y:.2f}",
            "<extra></extra>"
          ),
          color = ~model,
          colors = c("grey", I("black")),
          height = size,
          width = size
        ) %>%
          plotly::add_lines(
            data = smooth_dat
          ) %>%
          plotly::layout(
            xaxis = list(range = limits, showgrid = FALSE),
            yaxis = list(range = limits, showgrid = FALSE),
            showlegend = TRUE
          )
      }
      if ((length(probs) > 1) & (length(reals) == 1)) {

        # print("several models")
        # print(smooth_dat %>% na.omit())
        # print(col_values)

        cal_plot <- plotly::plot_ly(
          x = ~x,
          y = ~y,
          hovertemplate = paste0(
            "<b>%{xaxis.title.text}:</b> %{x:.2f}<br>",
            "<b>%{yaxis.title.text}:</b> %{y:.2f}"
          ),
          colors = c("grey", unname(col_values)),
          color = ~model,
          legendgroup = ~model,
          height = size,
          width = size
        ) %>%
          plotly::add_lines(
            data = smooth_dat %>% dplyr::filter(model == "reference"),
            showlegend = FALSE
          ) %>%
          plotly::add_lines(
            data = smooth_dat %>% dplyr::filter(model != "reference"),
            showlegend = TRUE
          ) %>%
          plotly::layout(
            legend = list(
              orientation = "h",
              xanchor = "center",
              yanchor = "top",
              x = 0.5,
              y = 1.1
            ),
            xaxis = list(range = limits, showgrid = FALSE),
            yaxis = list(range = limits, showgrid = FALSE)
          )

        # print(cal_plot)
      }
      if ((length(probs) > 1) & (length(reals) > 1)) {

        # print("several populations")

        cal_plot <- plotly::plot_ly(
          x = ~x,
          y = ~y,
          hovertemplate = paste0(
            "<b>%{xaxis.title.text}:</b> %{x:.2f}<br>",
            "<b>%{yaxis.title.text}:</b> %{y:.2f}"
          ),
          colors = c("grey", unname(col_values)),
          color = ~population,
          legendgroup = ~population,
          height = size,
          width = size
        ) %>%
          plotly::add_lines(
            data = smooth_dat %>% dplyr::filter(population == "reference"),
            showlegend = FALSE
          ) %>%
          plotly::add_lines(
            data = smooth_dat %>% dplyr::filter(population != "reference"),
            showlegend = TRUE
          ) %>%
          plotly::layout(
            legend = list(
              orientation = "h",
              xanchor = "center",
              yanchor = "top",
              x = 0.5,
              y = 1.1
            ),
            xaxis = list(range = limits, showgrid = FALSE),
            yaxis = list(range = limits, showgrid = FALSE)
          )

        # print(cal_plot)
      }
    }
  }

  if (type == "discrete") {
    if (interactive == FALSE) {
      if ((length(probs) == 1)) {
        cal_plot <- ggplot2::ggplot(
          deciles_dat,
          ggplot2::aes(x = phatx, y = phaty)
        ) +
          ggplot2::geom_abline(slope = 1, intercept = 0, color = "grey") +
          ggplot2::geom_line(size = 1) +
          ggplot2::geom_point() +
          ggplot2::theme_classic() +
          ggplot2::ylab("Observed") +
          ggplot2::theme(
            axis.title.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank()
          ) +
          ggplot2::coord_cartesian(
            xlim = limits,
            ylim = limits, expand = FALSE
          ) +
          ggplot2::labs(x = "Predicted")
        

      }

      if ((length(probs) > 1) & (length(reals) == 1)) {
        cal_plot <- ggplot2::ggplot(
          deciles_dat %>%
            dplyr::filter(model != "reference"),
          ggplot2::aes(
            x = phatx,
            y = phaty,
            color = model
          )
        ) +
          ggplot2::geom_abline(
            slope = 1,
            intercept = 0,
            color = "grey"
          ) +
          ggplot2::geom_line(size = 1) +
          ggplot2::geom_point() +
          ggplot2::theme_classic() +
          ggplot2::ylab("Observed") +
          ggplot2::theme(
            axis.title.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank()
          ) +
          ggplot2::coord_cartesian(
            xlim = limits,
            ylim = limits, expand = FALSE
          ) +
          ggplot2::scale_color_manual(values = unname(col_values)) #+
        ggplot2::theme(legend.position = "none")
      }

      if ((length(probs) > 1) & (length(reals) > 1)) {
        cal_plot <- ggplot2::ggplot(
          deciles_dat %>%
            dplyr::filter(population != "reference"),
          ggplot2::aes(
            x = phatx,
            y = phaty,
            color = population
          )
        ) +
          ggplot2::geom_abline(
            slope = 1,
            intercept = 0,
            color = "grey"
          ) +
          ggplot2::geom_line(size = 1) +
          ggplot2::geom_point() +
          ggplot2::theme_classic() +
          ggplot2::ylab("Observed") +
          ggplot2::theme(
            axis.title.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank()
          ) +
          ggplot2::coord_cartesian(
            xlim = limits,
            ylim = limits, expand = FALSE
          ) +
          ggplot2::scale_color_manual(values = unname(col_values)) #+
        ggplot2::theme(legend.position = "none")
      }
    }
    if (interactive == TRUE) {
      if ((length(probs) == 1) & (length(reals) == 1)) {

        # print("one population")
        #
        # print(names(deciles_dat))
        # print(deciles_dat)

        cal_plot <- plotly::plot_ly(
          x = ~phatx,
          y = ~phaty,
          color = ~model,
          legendgroup = ~model,
          colors = c("grey", "black"),
          height = size,
          width = size
        ) %>%
          plotly::add_markers(
            data = deciles_dat %>% dplyr::filter(model != "reference"),
            showlegend = TRUE,
            hovertemplate = paste0(
              "<b>%{xaxis.title.text}:</b> %{x:.2f}<br>",
              "<b>%{yaxis.title.text}:</b> %{y:.2f}",
              "<extra></extra>"
            )
          ) %>%
          plotly::add_lines(
            data = deciles_dat,
            line = list(width = 1),
          ) %>%
          plotly::layout(
            xaxis = list(range = limits, showgrid = FALSE),
            yaxis = list(range = limits, showgrid = FALSE),
            showlegend = FALSE
          )
      }
      if ((length(probs) > 1) & (length(reals) == 1)) {

        # print("Several Models")
        #
        # print(names(deciles_dat))
        # print(deciles_dat)
        # print(col_values)


        cal_plot <- plotly::plot_ly(
          x = ~phatx,
          y = ~phaty,
          color = ~model,
          legendgroup = ~model,
          colors = unname(c(I("grey"), col_values)),
          height = size,
          width = size
        ) %>%
          plotly::add_markers(
            data = deciles_dat %>% dplyr::filter(model != "reference"),
            showlegend = TRUE,
            hovertemplate = paste0(
              "<b>%{xaxis.title.text}:</b> %{x:.2f}<br>",
              "<b>%{yaxis.title.text}:</b> %{y:.2f}"
            )
          ) %>%
          plotly::add_lines(
            data = deciles_dat,
            showlegend = FALSE
          ) %>%
          plotly::layout(
            xaxis = list(range = limits, showgrid = FALSE),
            yaxis = list(range = limits, showgrid = FALSE)
          ) %>%
          plotly::layout(
            legend = list(
              orientation = "h",
              xanchor = "center",
              yanchor = "top",
              x = 0.5,
              y = 1.1
            ),
            xaxis = list(title = "Predicted"),
            yaxis = list(title = "Observed")
          )


        # print(cal_plot)
      }
      if ((length(probs) > 1) & (length(reals) > 1)) {

        # print("Several Populations")
        #
        # print(names(deciles_dat))
        # print(deciles_dat)
        # print(col_values)


        cal_plot <- plotly::plot_ly(
          x = ~phatx,
          y = ~phaty,
          color = ~population,
          legendgroup = ~population,
          colors = unname(c(I("grey"), col_values)),
          height = size,
          width = size
        ) %>%
          plotly::add_markers(
            data = deciles_dat %>% dplyr::filter(population != "reference"),
            showlegend = TRUE,
            hovertemplate = paste0(
              "<b>%{xaxis.title.text}:</b> %{x:.2f}<br>",
              "<b>%{yaxis.title.text}:</b> %{y:.2f}"
            )
          ) %>%
          plotly::add_lines(
            data = deciles_dat,
            showlegend = FALSE
          ) %>%
          plotly::layout(
            xaxis = list(range = limits, showgrid = FALSE),
            yaxis = list(range = limits, showgrid = FALSE)
          ) %>%
          plotly::layout(legend = list(
            orientation = "h",
            xanchor = "center",
            yanchor = "top",
            x = 0.5,
            y = 1.1
          ))
      }
    }
  }

  if (interactive == TRUE) {
    if ( histogram_included == TRUE ) {
    if ((length(probs) == 1)) {
      # print(make_histogram_for_calibration(probs, deciles_dat))

      histprobs <- make_histogram_for_calibration(
        probs,
        deciles_dat
      ) %>%
        plotly::plot_ly() %>%
        plotly::add_bars(
          x = ~mids,
          y = ~counts,
          width = 0.01,
          # color = I("black"),
          color = I("grey35"),
          text = ~text,
          hoverinfo = "text",
          textposition = "none"
        ) %>%
        plotly::layout(
          barmode = "overlay", xaxis = list(range = limits, showgrid = FALSE),
          yaxis = list(showgrid = FALSE),
          showlegend = FALSE
        )
    } else {

      # print(make_histogram_for_calibration(probs, deciles_dat))

      histprobs <- make_histogram_for_calibration(probs, deciles_dat) %>%
        plotly::plot_ly(
          color = as.formula(paste0("~", names(deciles_dat)[1])),
          legendgroup = as.formula(paste0("~", names(deciles_dat)[1])),
          x = ~mids,
          y = ~counts,
          opacity = 0.5,
          text = ~text,
          hoverinfo = "text"
        ) %>%
        plotly::add_bars(
          showlegend = FALSE,
          colors = c(unname(col_values)),
          textposition = "none"
        ) %>%
        plotly::layout(
          barmode = "overlay", xaxis = list(range = limits, showgrid = FALSE),
          yaxis = list(showgrid = FALSE)
        )
    }

    full_cal_plot <- plotly::subplot(
      cal_plot,
      histprobs,
      nrows = 2,
      shareX = TRUE,
      heights = c(0.8, 0.2)
    )
    
    } else {
      
      full_cal_plot <- cal_plot
      
    }
    
    full_cal_plot <- full_cal_plot %>%
      plotly::layout(
        xaxis = list(title = "Predicted", range = limits),
        yaxis = list(title = "Observed")
      ) %>%
      plotly::config(displayModeBar = FALSE)
    
  }


  if (interactive == FALSE) {
    if ((length(probs) == 1) & (length(reals) == 1)) {

      # print("one population")

      histprobs <- ggplot2::ggplot(
        data = make_histogram_for_calibration(probs, deciles_dat),
        ggplot2::aes(x = mids, y = counts)
      ) +
        ggplot2::geom_col() +
        ggplot2::theme_classic() +
        ggplot2::coord_cartesian(
          xlim = limits,
          expand = FALSE
        ) +
        ggplot2::labs(x = "Predicted") +
        ggplot2::theme(axis.title.y = ggplot2::element_text(colour = "white")) +
        ggplot2::scale_color_manual(values = col_values)
    }

    if ((length(probs) > 1) & (length(reals) == 1)) {

      # print("several models")
      # print(make_histogram_for_calibration(probs, deciles_dat))

      histprobs <- ggplot2::ggplot(
        data = make_histogram_for_calibration(probs, deciles_dat),
        ggplot2::aes(
          x = mids,
          y = counts,
          fill = model
        )
      ) +
        ggplot2::geom_col(position = "identity", alpha = 0.5) +
        ggplot2::theme_classic() +
        ggplot2::coord_cartesian(
          xlim = limits,
          expand = FALSE
        ) +
        ggplot2::labs(x = "Predicted") +
        ggplot2::theme(axis.title.y = ggplot2::element_text(colour = "white")) +
        ggplot2::scale_fill_manual(values = unname(col_values))
    }

    if ((length(probs) > 1) & (length(reals) > 1)) {
      # print((is.list(probs)) & ( is.list(real)) )
      # print("several populations")

      histprobs <- ggplot2::ggplot(
        data = make_histogram_for_calibration(probs, deciles_dat),
        ggplot2::aes(x = mids, y = counts, fill = population)
      ) +
        ggplot2::geom_col(position = "identity", alpha = 0.5) +
        ggplot2::theme_classic() +
        ggplot2::coord_cartesian(
          xlim = limits,
          expand = FALSE
        ) +
        ggplot2::labs(x = "Predicted") +
        ggplot2::scale_fill_manual(values = unname(col_values)) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(colour = "white"))

      # print(
      #   plotly::ggplotly(
      #     histprobs
      #   )
      # )
    }

    full_cal_plot <- patchwork::wrap_plots(cal_plot +
      ggplot2::theme(
        legend.position = "none"
      ),
    histprobs +
      ggplot2::theme(
        legend.position = "none"
      ),
    heights = c(3, 1)
    )
  }

  full_cal_plot
}



#' Make deciles dataframe
#'
#' @inheritParams create_roc_curve
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' make_deciles_dat(
#'   probs = example_dat$estimated_probabilities,
#'   real = example_dat$outcome
#' )
#' }
make_deciles_dat <- function(probs, real) {
  data.frame(probs, real) %>%
    dplyr::mutate(quintile = dplyr::ntile(probs, 10)) %>%
    dplyr::group_by(quintile) %>%
    dplyr::summarise(phaty = sum(real) / n(), phatx = mean(probs)) %>%
    dplyr::ungroup()
}



#' Define limits for Calibration Curve
#'
#' @param deciles_dat
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' make_deciles_dat(
#'   probs = example_dat$estimated_probabilities,
#'   real = example_dat$outcome
#' ) %>%
#'   define_limits_for_calibration_plot()
#' }
define_limits_for_calibration_plot <- function(deciles_dat) {
  deciles_dat <- deciles_dat %>% dplyr::filter(!is.na(quintile))

  l <- max(0, min(deciles_dat$phatx[1], deciles_dat$phaty[1]))
  u <- max(deciles_dat$phatx, deciles_dat$phaty)
  limits <- c(l - (u - l) * 0.05, u + (u - l) * 0.05)

  limits
}



#' Arrange estimated probabilities to long format
#'
#' @inheritParams create_roc_curve
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' arrange_estimated_probabilities_to_long_format(
#'   probs = list("First Model" = example_dat$estimated_probabilities)
#' )
#' }
arrange_estimated_probabilities_to_long_format <- function(probs) {
  purrr::map_df(probs,
    ~ data.frame(probs = .x),
    .id = "model"
  )
}



#' Creating dat for smooth calibration
#'
#' Arrange estimated probabilities to long format
#'
#' @inheritParams create_roc_curve
#' @param deciles_dat data of deciles
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # several models
#' deciles_dat <- purrr::map_df(list(
#'   "Model 1" = example_dat$estimated_probabilities
#' ),
#' ~ make_deciles_dat(.x, example_dat$outcome),
#' .id = "model"
#' ) %>%
#'   mutate(model = forcats::fct_inorder(factor(model)))
#'
#' create_dat_for_smooth_calibration(
#'   list("Model 1" = example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome),
#'   deciles_dat
#' )
#'
#' # several populations
#'
#' deciles_dat <- purrr::map2_dfr(list(
#'   "train" = example_dat %>%
#'     dplyr::filter(type_of_set == "train") %>%
#'     dplyr::pull(estimated_probabilities),
#'   "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'     dplyr::pull(estimated_probabilities)
#' ),
#' list(
#'   "train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
#'     dplyr::pull(outcome),
#'   "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'     dplyr::pull(outcome)
#' ),
#' ~ make_deciles_dat(.x, .y),
#' .id = "population"
#' ) %>%
#'   mutate(population = forcats::fct_inorder(factor(population)))
#'
#' create_dat_for_smooth_calibration(
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
#'   ),
#'   deciles_dat
#' )
#' }
create_dat_for_smooth_calibration <- function(probs,
                                              reals,
                                              deciles_dat) {
  if (length(probs) >= 1 & length(reals) == 1) {
    smooth_dat <- tibble::tribble(
      ~model, ~x, ~y,
      "reference", 0, 0,
      "reference", 1, 1
    ) %>%
      dplyr::bind_rows(
        probs %>%
          purrr::map_df(~ lowess(., reals[[1]], iter = 0) %>%
            approx(
              xout = seq(0, 1, by = 0.01),
              ties = mean
            ),
          .id = "model"
          )
      ) %>%
      dplyr::mutate(model = forcats::fct_inorder(factor(model))) %>%
      stats::na.omit()
  }

  if (length(probs) > 1 & length(reals) > 1) {
    smooth_dat <- tibble::tribble(
      ~population, ~x, ~y,
      "reference", 0, 0,
      "reference", 1, 1
    ) %>%
      dplyr::bind_rows(purrr::map2_dfr(probs,
        reals,
        ~ lowess(.x, .y, iter = 0) %>%
          approx(
            xout = seq(0, 1, by = 0.01),
            ties = mean
          ),
        .id = "population"
      )) %>%
      dplyr::mutate(population = forcats::fct_inorder(factor(population))) %>%
      stats::na.omit()
  }
  smooth_dat
}

#' Creating dat for histogram calibration
#'
#' @inheritParams create_dat_for_smooth_calibration
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # several models
#' deciles_dat <- purrr::map_df(list(
#'   "Model 1" = example_dat$estimated_probabilities
#' ),
#' ~ make_deciles_dat(.x, example_dat$outcome),
#' .id = "model"
#' ) %>%
#'   mutate(model = forcats::fct_inorder(factor(model)))
#'
#' make_histogram_for_calibration(
#'   list("Model 1" = example_dat$estimated_probabilities),
#'   deciles_dat
#' )
#'
#' # several populations
#'
#' deciles_dat <- purrr::map2_dfr(list(
#'   "train" = example_dat %>%
#'     dplyr::filter(type_of_set == "train") %>%
#'     dplyr::pull(estimated_probabilities),
#'   "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'     dplyr::pull(estimated_probabilities)
#' ),
#' list(
#'   "train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
#'     dplyr::pull(outcome),
#'   "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'     dplyr::pull(outcome)
#' ),
#' ~ make_deciles_dat(.x, .y),
#' .id = "population"
#' ) %>%
#'   mutate(population = forcats::fct_inorder(factor(population)))
#'
#' make_histogram_for_calibration(
#'   probs = list(
#'     "train" = example_dat %>%
#'       dplyr::filter(type_of_set == "train") %>%
#'       dplyr::pull(estimated_probabilities),
#'     "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'       dplyr::pull(estimated_probabilities)
#'   ),
#'   deciles_dat
#' )
#' }
make_histogram_for_calibration <- function(probs, deciles_dat) {
  probs %>%
    purrr::map_df(~ hist(.x, plot = FALSE, breaks = seq(0, 1, 0.01)) %>%
      .[c("mids", "counts")], .id = names(deciles_dat)[1]) %>%
    dplyr::mutate(
      text_obs = glue::glue("{counts} observations in "),
      text_range = ifelse(mids == 0.005, "[0,0.01]",
        glue::glue("({mids - 0.005},{mids + 0.005}]")
      ),
      text = glue::glue("{text_obs}{text_range}"),
      dplyr::across(
        dplyr::any_of(c("model", "population")),
        ~ forcats::fct_inorder(factor(.x))
      )
    )
}
