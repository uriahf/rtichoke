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


#' Example performance table 
#'
#' An example performance table that contains train and test sets
#'
#' @format A data frame with 202 rows and 15 variables:
"performance_table_for_train_and_test_sets"

#' Example performance table 
#'
#' An example performance table that contains performance for two different models
#'
#' @format A data frame with 202 rows and 15 variables:
"performance_table_for_two_models"

#' Example performance table 
#'
#' An example performance table that contains one model for one population
#'
#' @format A data frame with 101 rows and 14 variables:
"performance_table_one_model"

