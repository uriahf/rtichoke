#' Check probs input
#'
#' @inheritParams prepare_performance_data
#' @keywords internal
#' @examples
#' \dontrun{
#' check_probs_input(example_dat$estimated_probabilities)
#' 
#' list(
#' "train" = example_dat %>%
#'   dplyr::filter(type_of_set == "train") %>%
#'   dplyr::pull(estimated_probabilities),
#' "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'              dplyr::pull(estimated_probabilities
#' ) %>% 
#'   check_probs_input()
#' 
#' check_probs_input(c(example_dat$estimated_probabilities, -0.1))
#' check_probs_input(c(example_dat$estimated_probabilities,  1.1))
#' 
#' list(
#'   "train" = example_dat %>%
#'     dplyr::filter(type_of_set == "train") %>%
#'     dplyr::pull(estimated_probabilities),
#'   "test" = c(example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'                dplyr::pull(estimated_probabilities), -0.2)
#'   ) %>% 
#'     check_probs_input()
#' 
#' } 
check_probs_input <- function(probs){
  
  if (is.list(probs)){
    probs %>% 
      purrr::map(.f = ~check_probs_input(.x))
  }
  
  if (!is.list(probs)){
  if ( any(probs < 0)| any(probs > 1)) {
    stop("Estimated Probabilities are out of the range")
  }
  }
}



#' Check real input
#'
#' @inheritParams prepare_performance_data
#' @keywords internal
#' @examples
#' \dontrun{
#' check_real_input(example_dat$outcome)
#' 
#' list(
#' "train" = example_dat %>% 
#'   dplyr::filter(type_of_set == "train") %>%
#'   dplyr::pull(outcome),
#' "test" = example_dat %>% 
#'   dplyr::filter(type_of_set == "test") %>%
#'   dplyr::pull(outcome)
#' ) %>% 
#'   check_real_input()
#' 
#' check_real_input(c(example_dat$outcome, -0.1))
#' check_real_input(c(example_dat$outcome,  1.1))
#' 
#' list(
#'   "train" = example_dat %>%
#'     dplyr::filter(type_of_set == "train") %>%
#'     dplyr::pull(outcome),
#'   "test" = c(example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'                dplyr::pull(outcome), -0.2)
#'   ) %>% 
#'     check_real_input()
#' 
#' } 
check_real_input <- function(real){
  
  if (is.list(real)){
    real %>% 
      purrr::map(.f = ~check_real_input(.x))
  }
  
  if (!is.list(real)){
    if ( !all(real %in% c(0,1)) ) {
      stop("Outcomes are out of the range")
    }
  }
}