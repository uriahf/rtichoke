#' Create a Calibration Curve List
#'
#' @inheritParams create_roc_curve
#' 
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' prepare_events_per_strata_data(
#'   probs = list(example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome)
#' )
#'
#' # Several Models
#'
#' prepare_events_per_strata_data(
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
#' prepare_events_per_strata_data(
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
#'   )
#' )
#' }

prepare_events_per_strata_data <- function(
    probs, 
    reals = NA, 
    stratified_by = "probability_threshold",
    by = 0.01) {
  
  half_by <- by / 2
  
  if (is.null(names(probs))) {
    names(probs) <- "model"
  }
  
  reference_groups <- names(probs)
  
  hist_data <- probs |> 
    purrr::map_df(~ hist(
      .x,
      plot = FALSE, breaks = seq(0, 1, by)
    )  %>% 
      .[c("mids", "counts")], .id = "reference_group") |>
    dplyr::mutate(
      text_obs = glue::glue("{counts} observations in "),
      text_range = ifelse(mids == half_by, glue::glue("[0, {by}]"),
                          glue::glue("({mids - half_by}, {mids + half_by}]")
      ),
      text = glue::glue("{text_obs}{text_range}")
    )
  
  hist_data
  
}
