#' prevalence
#'
#' Get the prevalence out of performance table
#'
#' @param performance_table an rtichoke performance table
#'
#' @export



get_prevalence_from_performance_table <- function(performance_table) {
  PPV <- predicted_positives_percent <-NULL 
  
  performance_table %>%
    dplyr::filter(predicted_positives_percent == 1) %>%
    dplyr::pull(PPV) %>%
    unique()
  
}
