#' prevalence
#'
#' Get the prevalence out of performance table
#'
#' @param performance_table an rtichoke performance table
#'
#' @export



get_prevalence_from_performance_table <- function(performance_table) {
  PPV <- predicted_positives_percent <- NULL

  performance_table %>%
    dplyr::filter(predicted_positives_percent == 1) %>%
    dplyr::pull(PPV) %>%
    unique()
}


#' Title
#'
#' @param curve the specified curve for the reference lines
#' @param prevalence the prevalence of the outcome
#' @param color the required color

create_reference_lines_data_frame <- function(curve,
                                              prevalence = NA,
                                              color = NA) {
  

  
  if (curve == "roc") {
    reference_lines_data_frame <- data.frame(x = 0, xend = 1, y = 0, yend = 1, col = "grey", linetype = "solid")
  }

  if (curve == "lift") {
    reference_lines_data_frame <- data.frame(x = 0, xend = 1, y = 1, yend = 1, col = "grey", linetype = "solid")
  }

  if (curve == "precision recall") {
    
    if(length(prevalence) == 1) {col_values <- "grey" }
    if(length(prevalence) > 1) {col_values <- c("#5E7F9A", 
                                                "#931B53", 
                                                "#F7DC2E", 
                                                "#C6C174", 
                                                "#75DBCD")[1:length(prevalence)]}
    
    reference_lines_data_frame <- data.frame(x = 0, xend = 1, y = prevalence, yend = prevalence, col = col_values, 
                                             linetype = "dotted" )
  }

  if (curve == "gains") {
    
    if(length(prevalence) == 1) {col_values <- "grey" }
    if(length(prevalence) > 1) {col_values <- c("#5E7F9A", 
                                                "#931B53", 
                                                "#F7DC2E", 
                                                "#C6C174", 
                                                "#75DBCD")[1:length(prevalence)]}
  
    
    reference_lines_data_frame <- purrr::map2_df(
      prevalence,
      col_values,
      function(x, y)
        data.frame(
          x = c(0, x),
          xend = c(x, 1),
          y = c(0, 1),
          yend = c(1, 1),
          col = c(y, y),
          linetype = "dotted") 
    )
    
  }
  
  if (curve == "decision") {
    reference_lines_data_frame <- rbind(
      create_reference_lines_data_frame("decision treat all", prevalence),
      create_reference_lines_data_frame("decision treat none")
    )
  }

  if (curve == "decision treat all") {
    
    if(length(prevalence) == 1) {col_values <- "grey" }
    if(length(prevalence) > 1) {col_values <- c("#5E7F9A", 
                                                "#931B53", 
                                                "#F7DC2E", 
                                                "#C6C174", 
                                                "#75DBCD")[1:length(prevalence)]}
    
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


create_segment_for_reference_line <- function(reference_line){
  ggplot2::geom_segment(x = reference_line$x, 
                        y = reference_line$y, 
                        xend = reference_line$xend, 
                        yend = reference_line$yend, 
                        color = reference_line$col,
                        linetype = reference_line$linetype)
}


#' Add reference lines to ggplot curve
#'
#' @param ggplot_curve a non interactive ggplot curve
#' @param reference_lines dataframe of reference lines 

add_reference_lines_to_ggplot <- function(ggplot_curve, reference_lines){
  
  ggplot_curve$layers <- c(
    purrr::map(reference_lines  %>%
                 split(1:nrow(.)), ~create_segment_for_reference_line(.x)),
    ggplot_curve$layers
  )
  ggplot_curve
}





