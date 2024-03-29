% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_inputs.R
\name{check_probs_input}
\alias{check_probs_input}
\title{Check probs input}
\usage{
check_probs_input(probs)
}
\arguments{
\item{probs}{a list of vectors of estimated probabilities
(one for each model or one for each population)}
}
\description{
Check probs input
}
\examples{
\dontrun{
check_probs_input(example_dat$estimated_probabilities)

list(
  "train" = example_dat \%>\%
    dplyr::filter(type_of_set == "train") \%>\%
    dplyr::pull(estimated_probabilities),
  "test" = example_dat \%>\% dplyr::filter(type_of_set == "test") \%>\%
    dplyr::pull(estimated_probabilities)
) \%>\%
  check_probs_input()

check_probs_input(c(example_dat$estimated_probabilities, -0.1))
check_probs_input(c(example_dat$estimated_probabilities, 1.1))

list(
  "train" = example_dat \%>\%
    dplyr::filter(type_of_set == "train") \%>\%
    dplyr::pull(estimated_probabilities),
  "test" = c(example_dat \%>\% dplyr::filter(type_of_set == "test") \%>\%
    dplyr::pull(estimated_probabilities), -0.2)
) \%>\%
  check_probs_input()
}
}
\keyword{internal}
