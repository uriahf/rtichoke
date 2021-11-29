#' Create a Calibration Curve
#'
#' @inheritParams create_roc_curve
#' @param type discrete or smooth
#'
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
                                     interactive = F,
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
  ) %>% 
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

    print(real)
    smooth_dat <- create_dat_for_smooth_calibration(
      probs,
      real = real,
      deciles_dat
    )

    if(interactive == FALSE) {
      
      print(smooth_dat)
      
      cal_plot <- ggplot2::ggplot(
        smooth_dat,
        ggplot2::aes(x = x, y = y)
      )  +
        ggplot2::geom_abline(slope = 1, 
                            intercept = 0, 
                            color = "grey") +
        ggplot2::geom_line() +
        ggplot2::ylab("Observed") +
        ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
              axis.text.x = ggplot2::element_blank(), 
              axis.ticks.x = ggplot2::element_blank()) +
        ggplot2::coord_cartesian(xlim = limits, 
                        ylim = limits, 
                        expand = F)  +
        ggplot2::theme_classic() +
        ggplot2::theme(legend.position = "none")
    }
    
    if(interactive == TRUE) {
      
        
    cal_plot <- plotly::plot_ly(
      x =~ c(0,1),
      y = c(0,1),
      colors = col_values
    ) %>% 
      plotly::add_lines(
        color = I("grey"),
        line = list(width = 1.75),
       ) %>%
      plotly::add_lines(
        data = smooth_dat,
        type = "scatter",
        mode = "lines+markers" ,
        x = ~x,
        y = ~y,
        color = as.formula(paste0("~", names(deciles_dat)[1])),
        colors = col_values,
        opacity = length(probs)
      ) %>%
      plotly::layout(
        xaxis = list(range = limits, showgrid = F),
        yaxis = list(range = limits, showgrid = F),
        showlegend = FALSE
      ) %>%
      plotly::config(displayModeBar = F)
    }
  }

  if (type == "discrete") {
    cal_plot <- plotly::plot_ly(
      x =~ c(0,1),
      y = c(0,1),
      colors = col_values
    ) %>% 
      plotly::add_lines(
        color = I("grey"),
        line = list(width = 1.75),
      ) %>%
      plotly::add_trace(
        data = deciles_dat,
        type = "scatter",
        mode = "lines+markers" ,
        x = ~phatx,
        y = ~phaty,
        color = as.formula(paste0("~", names(deciles_dat)[1])),
        colors = col_values,
        opacity = length(probs)
      ) %>%
      plotly::layout(
        xaxis = list(range = limits, showgrid = F),
        yaxis = list(range = limits, showgrid = F),
        showlegend = FALSE
      ) %>%
      plotly::config(displayModeBar = F)
  }

  if(interactive == TRUE) {
    
    histprobs <- make_histogram_for_calibration(probs, deciles_dat)  %>%
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
  }
  
  if (interactive == FALSE) {
    if ((!is.list(probs)) | ( !is.list(real) & (length(probs) == 1)) & (is.list(probs)) ) { 
    
    histprobs <- ggplot2::ggplot(
      data = make_histogram_for_calibration(probs, deciles_dat),
      ggplot2::aes(x = mids, y = counts)) +
      ggplot2::geom_col() +
      ggplot2::theme_classic() +
      ggplot2::coord_cartesian(xlim = limits,
                               expand = F) +
      ggplot2::labs(x = "Predicted") +
      ggplot2::theme(axis.title.y = ggplot2::element_text(colour = "white"))
    
  }
  
  if ((is.list(probs)) | ( !is.list(real)) ) { 
  
  histprobs <- ggplot2::ggplot(
    data = make_histogram_for_calibration(probs, deciles_dat),
    ggplot2::aes(x = mids, y = counts, color = model)) +
    ggplot2::geom_col() +
    ggplot2::theme_classic() +
    ggplot2::coord_cartesian(xlim = limits,
                             expand = F) +
    ggplot2::labs(x = "Predicted") +
    ggplot2::theme(axis.title.y = ggplot2::element_text(colour = "white"))
  
}
  
    if ((is.list(probs)) & ( is.list(real)) ) { 
      print((is.list(probs)) & ( is.list(real)) )
      
      histprobs <- ggplot2::ggplot(
        data = make_histogram_for_calibration(probs, deciles_dat),
        ggplot2::aes(x = mids, y = counts, color = population)) +
        ggplot2::geom_col() +
        ggplot2::theme_classic() +
        ggplot2::coord_cartesian(xlim = limits,
                                 expand = F) +
        ggplot2::labs(x = "Predicted") +
        ggplot2::theme(axis.title.y = ggplot2::element_text(colour = "white"))
      
    }  
    
  full_cal_plot <- patchwork::wrap_plots(cal_plot, histprobs, heights = c(3,1))
  
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
#' @keywords internal
#' 
#' @examples
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
#' deciles_dat <- purrr::map_df(list("Model 1" = example_dat$estimated_probabilities),
#'                              ~ make_deciles_dat(.x, example_dat$outcome),
#'                              .id = "model"
#' ) %>% 
#'   mutate(model  = forcats::fct_inorder(factor(model)))
#' 
#' create_dat_for_smooth_calibration(
#'   list("Model 1" = example_dat$estimated_probabilities),
#'   real = example_dat$outcome,
#'   deciles_dat
#' )
#' 
#' # several populations
#' 
#' deciles_dat <- purrr::map2_dfr(list(
#' "train" = example_dat %>%
#'  dplyr::filter(type_of_set == "train") %>%
#'  dplyr::pull(estimated_probabilities),
#'  "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'  dplyr::pull(estimated_probabilities)
#'),
#'list(
#'  "train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
#'    dplyr::pull(outcome),
#'  "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'    dplyr::pull(outcome)
#'),
#'~ make_deciles_dat(.x, .y),
#'.id = "population"
#') %>% 
#'  mutate(population  = forcats::fct_inorder(factor(population)))
#'
#'create_dat_for_smooth_calibration(
#'  probs = list(
#'    "train" = example_dat %>%
#'      dplyr::filter(type_of_set == "train") %>%
#'      dplyr::pull(estimated_probabilities),
#'    "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'      dplyr::pull(estimated_probabilities)
#'  ),
#'  real = list(
#'    "train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
#'      dplyr::pull(outcome),
#'    "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'      dplyr::pull(outcome)
#'  ),
#'  deciles_dat
#')
#' }
create_dat_for_smooth_calibration <- function(probs, 
                                              real,
                                              deciles_dat){
  if (is.list(probs) & !is.list(real)) {
  smooth_dat <- probs %>%
    purrr::map_df(~ lowess(., real, iter = 0) %>%
                    approx(xout = seq(0, 1, by = 0.01), 
                           ties = mean),
                  .id = "model") %>%
    as.data.frame() 
  }
  
  if (is.list(probs) & is.list(real)) {

    smooth_dat <- purrr::map2_dfr(probs,
                                  real,
                                  ~ lowess(.x, .y, iter = 0) %>%
                                    approx(xout = seq(0, 1, by = 0.01),
                                           ties = mean),
                                     .id = "population"
      )
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
#' deciles_dat <- purrr::map_df(list("Model 1" = example_dat$estimated_probabilities),
#'                              ~ make_deciles_dat(.x, example_dat$outcome),
#'                              .id = "model"
#' ) %>% 
#'   mutate(model  = forcats::fct_inorder(factor(model)))
#' 
#' make_histogram_for_calibration(
#'   list("Model 1" = example_dat$estimated_probabilities),
#'   deciles_dat
#' )
#' 
#' # several populations
#' 
#' deciles_dat <- purrr::map2_dfr(list(
#' "train" = example_dat %>%
#'  dplyr::filter(type_of_set == "train") %>%
#'  dplyr::pull(estimated_probabilities),
#'  "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'  dplyr::pull(estimated_probabilities)
#'),
#'list(
#'  "train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
#'    dplyr::pull(outcome),
#'  "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'    dplyr::pull(outcome)
#'),
#'~ make_deciles_dat(.x, .y),
#'.id = "population"
#') %>% 
#'  mutate(population  = forcats::fct_inorder(factor(population)))
#'
#'make_histogram_for_calibration(
#'  probs = list(
#'    "train" = example_dat %>%
#'      dplyr::filter(type_of_set == "train") %>%
#'      dplyr::pull(estimated_probabilities),
#'    "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'      dplyr::pull(estimated_probabilities)
#'  ),
#'  deciles_dat
#')
#' }
make_histogram_for_calibration <- function(probs, deciles_dat){
  probs %>%
  purrr::map_df(~ hist(.x, plot = F, breaks = seq(0, 1, 0.01)) %>%
                  .[c("mids", "counts")], .id = names(deciles_dat)[1])
}