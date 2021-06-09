#' Create ggplot for performance metrics
#'
#' Makes a ggplot for the metrices
#'
#' @param performance_table an rtichoke performance table
#' @param x_perf_metric a performance metrice for the x axis
#' @param y_perf_metric a performance metrice for the y axis
#' @param col_values color palette 
#'

create_ggplot_for_performance_metrics <- function(performance_table, 
                                                 x_perf_metric, 
                                                 y_perf_metric,
                                                 col_values =  c("#E69F00", 
                                                                 "#56B4E9",
                                                                 "#F0E442", 
                                                                 "#0072B2", 
                                                                 "#CC79A7")){

    if(length(unique(performance_table[,1]))==1){
     col_values <- "black"
    } 
    
    ggplot2::ggplot(performance_table,
                    ggplot2::aes_string( x = x_perf_metric, 
         y = y_perf_metric, 
         group = names(performance_table)[1], 
         color = names(performance_table)[1])) +
        ggplot2::geom_point(size = 1) +
        ggplot2::geom_path(size = 0.5) +
        ggplot2::theme_classic() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::scale_color_manual(values = col_values)
    
}






