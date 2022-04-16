#' Create a summary report
#'
#' @inheritParams create_roc_curve
#' @inheritParams rmarkdown::render
#' @param output_file The name of the output file
#'
#' @export
#'
#' @examples
#' \dontrun{
#' create_summary_report(
#'   probs = list(example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome)
#' )
#'
#' create_summary_report(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   reals = list(example_dat$outcome)
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
#'   reals = list(
#'     "train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
#'       dplyr::pull(outcome),
#'     "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'       dplyr::pull(outcome)
#'   )
#' )
#'
#' }
create_summary_report <- function(probs, reals, interactive = TRUE,
                                  output_file = "summary_report.html",
                                  output_dir = getwd()) {
  rmarkdown::render(
    file.path(
      system.file(package = "rtichoke"),
      "summary_report_template.Rmd"
    ),
    params = list(
      probs = probs,
      reals = reals,
      interactive = interactive
    ),
    output_file = output_file,
    output_dir = output_dir
  )

  print(glue::glue("{output_file} was rendered in {output_dir}"))
}
