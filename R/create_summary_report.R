#' Create a summary report
#'
#' @inheritParams create_roc_curve
#' @param output_file The name of the output file
#'
#' @return
#' @export
#'
#' @examples
#' @examples
#' \dontrun{
#' create_summary_report(
#'   probs = example_dat$estimated_probabilities,
#'   real = example_dat$outcome
#' )
#'
#' create_summary_report(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   real = example_dat$outcome
#' )
#'
#' create_summary_report(
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
#' create_summary_report(
#'   probs = example_dat$estimated_probabilities,
#'   real = example_dat$outcome,
#'   interactive = TRUE
#' )
#'
#' create_summary_report(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   real = example_dat$outcome,
#'   interactive = TRUE
#' )
#'
#' create_summary_report(
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
#'   ),
#'   interactive = TRUE
#' )
#' }
create_summary_report <- function(probs, real, interactive = FALSE,
                                  output_file = "summary_report.html") {
  rmarkdown::render(
    file.path(
      system.file(package = "rtichoke"),
      "summary_report_template.Rmd"
    ),
    params = list(
      probs = probs,
      real = real,
      interactive = interactive
    ),
    output_file = output_file
  )
}
