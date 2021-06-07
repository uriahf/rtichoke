#' Example Data Frame for rtichoke package
#'
#' An example data frame that contains estimated probabilities and binary outcomes,
#' used as an input for rtichoke functions.
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{estimated_probabilities}{estimated probabilities from some model}
#'   \item{random_guess}{random guesses generated from unifrom distribution}
#'   \item{outcome}{binary outcome}
#'   \item{type_of_set}{the type of the set that the observation belongs to}
#'   ...
#' }
"example_dat"