#' Create a Calibration Curve
#'
#' @inheritParams create_roc_curve
#' @param type discrete or smooth
#'
#' @export
#'
#' @examples
#' \dontrun{
#' create_calibration_curve(
#'   probs = example_dat$estimated_probabilities,
#'   real = example_dat$outcome, type = "discrete"
#' )
#' 
#' create_calibration_curve(
#'   probs = example_dat$estimated_probabilities,
#'   real = example_dat$outcome, type = "discrete", 
#'   interactive = TRUE
#' )
#' 
#' 
#' create_calibration_curve(
#'   probs = example_dat$estimated_probabilities,
#'   real = example_dat$outcome, type = "smooth"
#' )
#' 
#' create_calibration_curve(
#'   probs = list(example_dat$estimated_probabilities),
#'   real = example_dat$outcome, type = "smooth", 
#'   interactive = TRUE
#' )
#' 
#' # Several Models
#' 
#' create_calibration_curve(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   real = example_dat$outcome, type = "discrete"
#' ) 
#' 
#' 
#' create_calibration_curve(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   real = example_dat$outcome, 
#'   interactive = TRUE, 
#'   type = "discrete"
#' ) 
#' 
#' 
#' create_calibration_curve(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   real = example_dat$outcome, 
#'   interactive = FALSE, 
#'   type = "smooth"
#' ) 
#' 
#' create_calibration_curve(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess,
#'     "Third Model" = sample(example_dat$random_guess, replace = TRUE)
#'   ),
#'   real = example_dat$outcome, 
#'   interactive = TRUE, 
#'   type = "smooth"
#' ) 
#'   
#' 
#' create_calibration_curve(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess,
#'     "Third Model" = sample(example_dat$random_guess, replace = TRUE),
#'     "Fourth Model" = sample(example_dat$random_guess, replace = TRUE),
#'     "Fifth Model" = sample(example_dat$random_guess, replace = TRUE)
#'     
#'   ),
#'   real = example_dat$outcome, type = "smooth",
#'   interactive = TRUE
#' )
#' 
#' 
#' # several populations
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
#'   real = list(
#'     "train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
#'       dplyr::pull(outcome),
#'     "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'       dplyr::pull(outcome)
#'   )
#' )
#' 
#' 
#' create_calibration_curve(
#'   probs = list(
#'     "Train" = example_dat %>%
#'       dplyr::filter(type_of_set == "train") %>%
#'       dplyr::pull(estimated_probabilities),
#'     "Test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'       dplyr::pull(estimated_probabilities),
#'     "Val" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'       dplyr::pull(estimated_probabilities) %>% 
#'       sample(replace = TRUE)
#'   ),
#'   real = list(
#'     "Train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
#'       dplyr::pull(outcome),
#'     "Test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'       dplyr::pull(outcome),
#'     "Val" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'       dplyr::pull(outcome)
#'   ), interactive = TRUE,
#' )
#' }
create_calibration_curve <- function(probs,
                                     real,
                                     interactive = F,
                                     col_values = c(
                                       "#5BC0BE",
                                       "#FC8D62",
                                       "#8DA0CB",
                                       "#E78AC3",
                                       "#A4243B",
                                       title_included = F
                                     ),
                                     type = "discrete") {
  quintile <- phatx <- phaty <- gam <- NULL
  
  check_probs_input(probs)
  check_real_input(real)
  
  
  if (!is.list( probs) ) {probs = list("model 1" = probs)}
  
  col_values <- col_values[1:length(probs)]
  
  if (is.list(probs) & !is.list(real)) {
    
  deciles_dat <- tibble::tribble(
      ~model, ~quintile, ~phaty, ~phatx,
      "reference", NA, 0, 0,
      "reference", NA, 1, 1
    ) %>% 
  bind_rows(
    purrr::map_df(probs,
    ~ make_deciles_dat(.x, real),
    .id = "model"
  )) %>% 
    dplyr::mutate(model  = forcats::fct_inorder(factor(model)))
  }
  
  
  if (is.list(probs) & is.list(real)) {
    if (is.null(names(probs)) & is.null(names(real))) {
      names(probs) <- paste("population", 1:length(probs))
      names(real) <- paste("population", 1:length(real))
    }
    
    deciles_dat <- tibble::tribble(
      ~population, ~quintile, ~phaty, ~phatx,
      "reference", NA, 0, 0,
      "reference", NA, 1, 1
    ) %>% dplyr::bind_rows(
      purrr::map2_dfr(probs,
                           real,
                           ~ make_deciles_dat(.x, .y),
                           .id = "population"
    )) %>% 
      dplyr::mutate(population  = forcats::fct_inorder(factor(population)))
    
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

      if ((length(probs) == 1) & !is.list(real)) {

        
      cal_plot <- ggplot2::ggplot(
        smooth_dat,
        ggplot2::aes(x = x, 
                     y = y)
      )  +
        ggplot2::geom_abline(slope = 1, 
                            intercept = 0, 
                            color = "grey") +
        ggplot2::geom_line(size = 1) +
        ggplot2::ylab("Observed") +
        ggplot2::theme_classic() +
        ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
              axis.text.x = ggplot2::element_blank(), 
              axis.ticks.x = ggplot2::element_blank()) +
        ggplot2::coord_cartesian(xlim = limits, 
                        ylim = limits, 
                        expand = F)  +
        ggplot2::theme(legend.position = "none")
      
      
      }
      
      if ((length(probs) > 1) & !is.list(real)) {
        print(head(smooth_dat))
        print(col_values)
        
        
        cal_plot <- ggplot2::ggplot(
          smooth_dat
        )  +
          ggplot2::geom_line(ggplot2::aes(x = x, 
                                          y = y,
                                          color = model),
                             size = 1) +
          ggplot2::ylab("Observed") +
          ggplot2::theme_classic() +
          ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_blank(),
                         axis.ticks.x = ggplot2::element_blank()) +
          ggplot2::coord_cartesian(xlim = limits,
                                   ylim = limits,
                                   expand = F)  +
          ggplot2::scale_color_manual(values = c("grey", unname(col_values)))
        
      }
      
      if ((length(probs) > 1) & is.list(real)) {
        print(smooth_dat)
        
      }
      
    }
    
    if(interactive == TRUE) {
      
      if ((length(probs) == 1) & (!is.list(real)) ) { 
        
        print("one population")
        
    cal_plot <- plotly::plot_ly(
      x = ~x,
      y = ~y,
      hovertemplate = paste0("<b>%{xaxis.title.text}:</b> %{x:.2f}<br>",
                             "<b>%{yaxis.title.text}:</b> %{y:.2f}",
                             "<extra></extra>"
      ),
      color =~ model,
      colors = c("grey", I("black"))
    ) %>% 
      plotly::add_lines(
        data = smooth_dat
      ) %>%
      plotly::layout(
        xaxis = list(range = limits, showgrid = F),
        yaxis = list(range = limits, showgrid = F),
        showlegend = TRUE
      ) 
      }
      if ((length(probs) > 1) & (!is.list(real)) ) { 
        
        print("several models")
        print(smooth_dat %>% na.omit())
        print(col_values)
        
        cal_plot <- plotly::plot_ly(
          x = ~x,
          y = ~y,
          hovertemplate = paste0("<b>%{xaxis.title.text}:</b> %{x:.2f}<br>",
                                 "<b>%{yaxis.title.text}:</b> %{y:.2f}"
          ),
          colors = c("grey", unname(col_values)),
          color =~ model,
          legendgroup =~ model
        ) %>%
          plotly::add_lines(
            data = smooth_dat %>% dplyr::filter(model == "reference"),
            showlegend = FALSE
          ) %>% 
          plotly::add_lines(
            data = smooth_dat %>% dplyr::filter(model != "reference"),
            showlegend = TRUE
        ) %>%
          plotly::layout(legend = list(orientation = 'h',
                                       xanchor = "center",
                                       yanchor = "top",
                                       x = 0.5,
                                       y = 1.1
          ),
          xaxis = list(range = limits, showgrid = F),
          yaxis = list(range = limits, showgrid = F))
        
        print(cal_plot)
        
      }
      if ((length(probs) > 1) & (is.list(real)) ) { 
        
        print("several populations")
        
        cal_plot <- plotly::plot_ly(
          x = ~x,
          y = ~y,
          hovertemplate = paste0("<b>%{xaxis.title.text}:</b> %{x:.2f}<br>",
                                 "<b>%{yaxis.title.text}:</b> %{y:.2f}"
          ),
          colors = c("grey", unname(col_values)),
          color =~ population,
          legendgroup =~ population
        ) %>%
          plotly::add_lines(
            data = smooth_dat %>% dplyr::filter(population == "reference"),
            showlegend = FALSE
          ) %>% 
          plotly::add_lines(
            data = smooth_dat %>% dplyr::filter(population != "reference"),
            showlegend = TRUE
          ) %>%
          plotly::layout(legend = list(orientation = 'h',
                                       xanchor = "center",
                                       yanchor = "top",
                                       x = 0.5,
                                       y = 1.1
          ),
          xaxis = list(range = limits, showgrid = F),
          yaxis = list(range = limits, showgrid = F))
        
        print(cal_plot)
        
      }
      
      
    }
  }

  if (type == "discrete") {
    
    if(interactive == FALSE) {
      
      if ((length(probs) == 1) ) { 
      
      cal_plot <- ggplot2::ggplot(deciles_dat, 
                                  ggplot2::aes(x = phatx, y = phaty)) +
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
        ggplot2::coord_cartesian(xlim = limits, ylim = limits, expand = F)+
        ggplot2::labs(x = "Predicted")
      }
      
      if ((length(probs) > 1) & ( !is.list(real)) ) { 
        
        cal_plot <- ggplot2::ggplot(deciles_dat %>% 
                                      dplyr::filter(model!= "reference"), 
                                    ggplot2::aes(x = phatx, 
                                                 y = phaty,
                                                 color = model)) +
          ggplot2::geom_abline(slope = 1,
                               intercept = 0,
                               color = "grey") +
          ggplot2::geom_line(size = 1) +
          ggplot2::geom_point() +
          ggplot2::theme_classic() +
          ggplot2::ylab("Observed") +
          ggplot2::theme(
            axis.title.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank()
          ) +
          ggplot2::coord_cartesian(xlim = limits, ylim = limits, expand = F)  +
          ggplot2::scale_color_manual(values = unname(col_values)) #+
          ggplot2::theme(legend.position = "none")
        
      }
      
      if ((length(probs) > 1) & ( is.list(real)) ) { 
        
        cal_plot <- ggplot2::ggplot(deciles_dat %>% 
                                      dplyr::filter(population!= "reference"), 
                                    ggplot2::aes(x = phatx, 
                                                 y = phaty,
                                                 color = population)) +
          ggplot2::geom_abline(slope = 1,
                               intercept = 0,
                               color = "grey") +
          ggplot2::geom_line(size = 1) +
          ggplot2::geom_point() +
          ggplot2::theme_classic() +
          ggplot2::ylab("Observed") +
          ggplot2::theme(
            axis.title.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank()
          ) +
          ggplot2::coord_cartesian(xlim = limits, ylim = limits, expand = F)  +
          ggplot2::scale_color_manual(values = unname(col_values)) #+
        ggplot2::theme(legend.position = "none")
        
      }
      
      
      
    }
    if(interactive == TRUE) {
      
      if ((length(probs) == 1) & (!is.list(real)) ) { 
        
        print("one population")
        
        print(names(deciles_dat))
        print(deciles_dat)
        
        cal_plot <- plotly::plot_ly(
        x = ~phatx,
        y = ~phaty,
        color =~ model,
        legendgroup = ~model,
        colors = c("grey", "black")
      ) %>% 
        plotly::add_markers(
          data = deciles_dat %>% dplyr::filter(model != "reference"),
          showlegend = TRUE,
          hovertemplate = paste0("<b>%{xaxis.title.text}:</b> %{x:.2f}<br>",
                                 "<b>%{yaxis.title.text}:</b> %{y:.2f}",
                                 "<extra></extra>"
        )) %>% 
        plotly::add_lines(
          data = deciles_dat,
          line = list(width = 1),
        ) %>%
        plotly::layout(
          xaxis = list(range = limits, showgrid = F),
          yaxis = list(range = limits, showgrid = F),
          showlegend = FALSE
        ) 

      }
      if ((length(probs) > 1) & (!is.list(real)) ) { 
        
        print("Several Models")
        
        print(names(deciles_dat))
        print(deciles_dat)
        print(col_values)
          
        
        cal_plot <- plotly::plot_ly(
          x = ~phatx,
          y = ~phaty,
          color =~ model,
          legendgroup = ~model,
          colors = unname(c(I("grey"), col_values)),
        ) %>% 
          plotly::add_markers(
            data = deciles_dat %>% dplyr::filter(model != "reference"),
            showlegend = TRUE,
            hovertemplate = paste0("<b>%{xaxis.title.text}:</b> %{x:.2f}<br>",
                                   "<b>%{yaxis.title.text}:</b> %{y:.2f}"
            )
          ) %>% 
          plotly::add_lines(
            data = deciles_dat,
            showlegend = FALSE) %>%
          plotly::layout(
            xaxis = list(range = limits, showgrid = F),
            yaxis = list(range = limits, showgrid = F)
          ) %>%
          plotly::layout(legend = list(orientation = 'h',
                                       xanchor = "center",
                                       yanchor = "top",
                                       x = 0.5,
                                       y = 1.1
                                       ),
                         xaxis = list(title = 'Predicted'), 
                         yaxis = list(title = 'Observed'))
        

        print(cal_plot)
        
      }
      if ((length(probs) > 1) & (is.list(real)) ) { 
        
        print("Several Populations")
        
        print(names(deciles_dat))
        print(deciles_dat)
        print(col_values)
        
        
        cal_plot <- plotly::plot_ly(
          x = ~phatx,
          y = ~phaty,
          color =~ population,
          legendgroup = ~population,
          colors = unname(c(I("grey"), col_values)),
        ) %>% 
          plotly::add_markers(
            data = deciles_dat %>% dplyr::filter(population != "reference"),
            showlegend = TRUE,
            hovertemplate = paste0("<b>%{xaxis.title.text}:</b> %{x:.2f}<br>",
                                   "<b>%{yaxis.title.text}:</b> %{y:.2f}"
            )
          ) %>% 
          plotly::add_lines(
            data = deciles_dat,
            showlegend = FALSE) %>%
          plotly::layout(
            xaxis = list(range = limits, showgrid = F),
            yaxis = list(range = limits, showgrid = F)
          )  %>%
          plotly::layout(legend = list(orientation = 'h',
                                       xanchor = "center",
                                       yanchor = "top",
                                       x = 0.5,
                                       y = 1.1
          ))
        
      }
      
      
    }
  }

  if(interactive == TRUE) {
    
    if ((length(probs) == 1) ) { 
      print(make_histogram_for_calibration(probs, deciles_dat))
      
      histprobs <- make_histogram_for_calibration(probs, 
                                                  deciles_dat)  %>%
        plotly::plot_ly() %>%
         plotly::add_bars(x = ~mids, 
                          y = ~counts,
                          width = 0.01,
                          # color = I("black"),
                          color = I("grey35"),
                          text =~ text,
                          hoverinfo = "text"
                        ) %>%
         plotly::layout(
           barmode = "overlay", xaxis = list(range = limits, showgrid = F),
           yaxis = list(showgrid = F),
           showlegend = FALSE
         ) 
      
      
      
    } else {
      
      print(make_histogram_for_calibration(probs, deciles_dat))
    
    histprobs <- make_histogram_for_calibration(probs, deciles_dat)  %>%
    plotly::plot_ly(
      color = as.formula(paste0("~", names(deciles_dat)[1])),
      legendgroup = as.formula(paste0("~", names(deciles_dat)[1])),
      x = ~mids, 
      y = ~counts,
      opacity = 0.5,
      text =~ text,
      hoverinfo = "text"
    ) %>%
      plotly::add_bars(showlegend = FALSE, 
                       colors =  c(unname(col_values))) %>% 
    plotly::layout(
      barmode = "overlay", xaxis = list(range = limits, showgrid = F),
      yaxis = list(showgrid = F)
    ) 
    
    }
    
  full_cal_plot <- plotly::subplot(cal_plot,
    histprobs,
    nrows = 2,
    shareX = T,
    heights = c(0.8, 0.2)
  ) %>% 
    plotly::layout(xaxis = list(title = 'Predicted'), 
           yaxis = list(title = 'Observed')) %>%
    plotly::config(displayModeBar = F) 
  
  }
  
  if (interactive == FALSE) {
    if ((length(probs) == 1) & (is.list(probs)) ) { 
    
      print("one population")
      
    histprobs <- ggplot2::ggplot(
      data = make_histogram_for_calibration(probs, deciles_dat),
      ggplot2::aes(x = mids, y = counts)) +
      ggplot2::geom_col() +
      ggplot2::theme_classic() +
      ggplot2::coord_cartesian(xlim = limits,
                               expand = F) +
      ggplot2::labs(x = "Predicted") +
      ggplot2::theme(axis.title.y = ggplot2::element_text(colour = "white"))  +
      ggplot2::scale_color_manual(values = col_values)
    
  }
  
  if ((is.list(probs) &  (length(probs) > 1)) & ( !is.list(real)) ) { 
    
    print("several models")
    print(make_histogram_for_calibration(probs, deciles_dat))
  
  histprobs <- ggplot2::ggplot(
    data = make_histogram_for_calibration(probs, deciles_dat),
    ggplot2::aes(x = mids, 
                 y = counts, 
                 fill = model)) +
    ggplot2::geom_col(position = "identity", alpha = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::coord_cartesian(xlim = limits,
                             expand = F) +
    ggplot2::labs(x = "Predicted") +
    ggplot2::theme(axis.title.y = ggplot2::element_text(colour = "white"))  +
    ggplot2::scale_fill_manual(values = unname(col_values)) 
  
}
  
    if ((is.list(probs)) & ( is.list(real)) ) { 
      print((is.list(probs)) & ( is.list(real)) )
      print("several populations")
      
      histprobs <- ggplot2::ggplot(
        data = make_histogram_for_calibration(probs, deciles_dat),
        ggplot2::aes(x = mids, y = counts, fill = population)) +
        ggplot2::geom_col(position = "identity", alpha = 0.5) +
        ggplot2::theme_classic() +
        ggplot2::coord_cartesian(xlim = limits,
                                 expand = F) +
        ggplot2::labs(x = "Predicted") +
        ggplot2::scale_fill_manual(values = unname(col_values)) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(colour = "white"))
      
      print(
        plotly::ggplotly(
          histprobs
        )
      )
      
    }  
    
  full_cal_plot <- patchwork::wrap_plots(cal_plot+
                                           ggplot2::theme(legend.position = "none"), 
                                         histprobs+
                                           ggplot2::theme(legend.position = "none"), 
                                         heights = c(3,1))
  
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
  smooth_dat <- tibble::tribble(
    ~model, ~x, ~y,
    "reference", 0, 0,
    "reference", 1, 1
  ) %>% 
  dplyr::bind_rows(
  probs %>%
    purrr::map_df(~ lowess(., real, iter = 0) %>%
                    approx(xout = seq(0, 1, by = 0.01), 
                           ties = mean),
                  .id = "model") 
  ) %>%
    dplyr::mutate(model  = forcats::fct_inorder(factor(model))) %>% 
    na.omit()
  }
  
  if (is.list(probs) & is.list(real)) {
    smooth_dat <- tibble::tribble(
      ~population, ~x, ~y,
      "reference", 0, 0,
      "reference", 1, 1
    ) %>% 
      dplyr::bind_rows(purrr::map2_dfr(probs,
                                  real,
                                  ~ lowess(.x, .y, iter = 0) %>%
                                    approx(xout = seq(0, 1, by = 0.01),
                                           ties = mean),
                                     .id = "population"
      )) %>%
      dplyr::mutate(population  = forcats::fct_inorder(factor(population))) %>% 
      na.omit()
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
                  .[c("mids", "counts")], .id = names(deciles_dat)[1]) %>% 
    dplyr::mutate(
      text_obs = glue::glue("{counts} observations in "),
      text_range = ifelse(mids == 0.005, "[0,0.01]", glue::glue("({mids - 0.005},{mids + 0.005}]")),
      text = glue::glue("{text_obs}{text_range}"),
      dplyr::across(dplyr::any_of(c("model", "population")), 
                      ~forcats::fct_inorder(factor(.x)))
    )  
    
}