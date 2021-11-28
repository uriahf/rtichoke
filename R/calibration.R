#' Create a Calibration Curve
#'
#' @inheritParams create_roc_curve
#'
#' @return
#' calibration 
#' @export
#'
#' @examples
#' create_calibration_curve(
#'   probs = example_dat$estimated_probabilities,
#'   real = example_dat$outcome
#' )
#'
#' create_calibration_curve(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   real = example_dat$outcome
#' )
#'
#' create_calibration_curve(
#'   probs = list(
#'     "train" = example_dat %>%
#'       dplyr::filter(type_of_set == "train") %>%
#'       dplyr::pull(estimated_probabilities),
#'     "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'       dplyr::pull(estimated_probabilities)
#'   ),
#'   real = list(
#'     "train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
#'       dplyr::pull(outcome),
#'     "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'       dplyr::pull(outcome)
#'   )
#' )
create_calibration_curve <- function(probs,
                                     real,
                                     col_values = c(
                                       "#5BC0BE",
                                       "#FC8D62",
                                       "#8DA0CB",
                                       "#E78AC3",
                                       "#A4243B"
                                     ),
                                     type = "discrete") {
  quintile <- phatx <- phaty <- gam <- NULL
  
  if (!is.list( probs) ) {probs = list("model 1" = probs)}
  
  col_values <- col_values[1:length(probs)]
  
  if (is.list(probs) & !is.list(real)) {
    
  deciles_dat <- purrr::map_df(probs,
    ~ make_deciles_dat(.x, real),
    .id = "model"
  )%>% 
    mutate(model  = forcats::fct_inorder(factor(model)))
  }
  
  
  if (is.list(probs) & is.list(real)) {
    if (is.null(names(probs)) & is.null(names(real))) {
      names(probs) <- paste("population", 1:length(probs))
      names(real) <- paste("population", 1:length(real))
    }
    deciles_dat <- purrr::map2_dfr(probs,
                           real,
                           ~ make_deciles_dat(.x, .y),
                           .id = "population"
    ) %>% 
      mutate(population  = forcats::fct_inorder(factor(population))) 
  }

  limits <- define_limits_for_calibration_plot(deciles_dat)

  if (type == "smooth") {
    cal_plot <- probs %>%
      purrr::map_df(~ lowess(., real, iter = 0) %>%
        approx(xout = seq(0, 1, by = 0.01), ties = mean), .id = names(deciles_dat)[1]) %>%
      as.data.frame() %>%
      plotly::plot_ly(
        x = ~x,
        y = ~y,
        colors = col_values,
        opacity = length(probs)
      ) %>%
      plotly::layout(
        shapes = list(
          type = "line",
          x0 = 0,
          x1 = 1,
          y0 = 0,
          y1 = 1,
          line = list(color = "grey")
        ), xaxis = list(range = limits, showgrid = F),
        yaxis = list(range = limits, showgrid = F),
        showlegend = FALSE
      ) %>%
      plotly::add_lines(
        color = as.formula(paste0("~", names(deciles_dat)[1]))
      )
  }

  if (type == "discrete") {
    cal_plot <- deciles_dat %>%
      plotly::plot_ly(
        x = ~phatx,
        y = ~phaty,
        color = as.formula(paste0("~", names(deciles_dat)[1])),
        colors = col_values,
        opacity = length(probs)
      ) %>%
      plotly::layout(
        shapes = list(
          type = "line",
          x0 = 0,
          x1 = 1,
          y0 = 0,
          y1 = 1,
          line = list(color = "grey")
        )
      ) %>%
      plotly::add_trace(
        type = "scatter",
        mode = "lines+markers" # ,
      ) %>%
      plotly::layout(
        xaxis = list(range = limits, showgrid = F),
        yaxis = list(range = limits, showgrid = F),
        showlegend = FALSE
      ) %>%
      plotly::config(displayModeBar = F)
  }

  histprobs <- probs %>%
    purrr::map_df(~ hist(.x, plot = F, breaks = seq(0, 1, 0.01)) %>%
      .[c("mids", "counts")], .id = names(deciles_dat)[1]) %>%
    plotly::plot_ly(
      colors = col_values,
      opacity = length(probs)
    ) %>%
    plotly::add_bars(x = ~mids, 
                     y = ~counts, 
                     color = as.formula(paste0("~", names(deciles_dat)[1]))) %>%
    plotly::layout(
      barmode = "overlay", xaxis = list(range = limits, showgrid = F), 
      yaxis = list(showgrid = F),
      showlegend = FALSE
    ) %>%
    plotly::config(displayModeBar = F)

  full_cal_plot <- plotly::subplot(cal_plot,
    histprobs,
    nrows = 2,
    shareX = T,
    heights = c(0.8, 0.2)
  )
  full_cal_plot
}



#' Make deciles dataframe
#'
#' @inheritParams create_roc_curve
#'
#' @return
#' @keywords internal
#' @examples 
#' \dontrun{
#' make_deciles_dat(
#' probs = example_dat$estimated_probabilities,
#' real = example_dat$outcome
#')
#'}
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
#' @return
#' @keywords internal
#' @examples 
#' \dontrun{
#' make_deciles_dat(
#' probs = example_dat$estimated_probabilities,
#' real = example_dat$outcome
#' ) %>% 
#' define_limits_for_calibration_plot()
#' }
define_limits_for_calibration_plot <- function(deciles_dat) {
  l <- max(0, min(deciles_dat$phatx[1], deciles_dat$phaty[1]))
  u <- max(deciles_dat$phatx, deciles_dat$phaty)
  limits <- c(l - (u - l) * 0.05, u + (u - l) * 0.05)
  limits
}



#' Arrange estimated probabilities to long format
#'
#' @inheritParams create_roc_curve
#'
#' @return
#' @keywords internal
#' 
#' \dontrun{
#' arrange_estimated_probabilities_to_long_format(
#' probs = list("First Model" = example_dat$estimated_probabilities)
#' )
#' }
arrange_estimated_probabilities_to_long_format <- function(probs) {
  purrr::map_df(probs,
    ~ data.frame(probs = .x),
    .id = "model"
  )
}