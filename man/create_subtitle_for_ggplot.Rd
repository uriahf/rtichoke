% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{create_subtitle_for_ggplot}
\alias{create_subtitle_for_ggplot}
\title{Creating subtitle for ggplot2}
\usage{
create_subtitle_for_ggplot(
  probs_names,
  color_values = c("#5BC0BE", "#FC8D62", "#8DA0CB", "#E78AC3", "#A4243B")
)
}
\arguments{
\item{probs_names}{the names of the probs}

\item{color_values}{color palette}
}
\description{
Creating subtitle for ggplot2
}
\examples{
\dontrun{
create_subtitle_for_ggplot(
  probs_names = c(
    "First Model", "Second Model", "Third Model"
  )
)
}
}
\keyword{internal}
