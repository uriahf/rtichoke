% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calibration.R
\name{create_calibration_curve_list}
\alias{create_calibration_curve_list}
\title{Create a Calibration Curve List}
\usage{
create_calibration_curve_list(
  probs,
  reals,
  color_values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#07004D", "#E6AB02",
    "#FE5F55", "#54494B", "#006E90", "#BC96E6", "#52050A", "#1F271B", "#BE7C4D",
    "#63768D", "#08A045", "#320A28", "#82FF9E", "#2176FF", "#D1603D", "#585123"),
  size = NULL
)
}
\arguments{
\item{probs}{a list of vectors of estimated probabilities
(one for each model or one for each population)}

\item{reals}{a list of vectors of binary outcomes (one for each population)}

\item{color_values}{color palette}

\item{size}{the size of the curve}
}
\description{
Create a Calibration Curve List
}
\examples{
\dontrun{

create_calibration_curve_list(
  probs = list(example_dat$estimated_probabilities),
  reals = list(example_dat$outcome)
)

# Several Models

create_calibration_curve_list(
  probs = list(
    "First Model" = example_dat$estimated_probabilities,
    "Second Model" = example_dat$random_guess
  ),
  reals = list(example_dat$outcome)
)


# Several Populations


create_calibration_curve_list(
  probs = list(
    "train" = example_dat \%>\%
      dplyr::filter(type_of_set == "train") \%>\%
      dplyr::pull(estimated_probabilities),
    "test" = example_dat \%>\% dplyr::filter(type_of_set == "test") \%>\%
      dplyr::pull(estimated_probabilities)
  ),
  reals = list(
    "train" = example_dat \%>\% dplyr::filter(type_of_set == "train") \%>\%
      dplyr::pull(outcome),
    "test" = example_dat \%>\% dplyr::filter(type_of_set == "test") \%>\%
      dplyr::pull(outcome)
  )
)
}
}
\keyword{internal}
