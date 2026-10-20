#' Example Data Frame for rtichoke package
#'
#' An example data frame that contains estimated probabilities and binary
#' outcomes, used as an input for rtichoke functions.
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{estimated_probabilities}{estimated probabilities from some model}
#'   \item{bad_model}{estimated probabilities from some bad model}
#'   \item{random_guess}{random guesses generated from unifrom distribution}
#'   \item{outcome}{binary outcome}
#'   \item{type_of_set}{the type of the set that the observation belongs to}
#'   ...
#' }
"example_dat"


#' Example Performance Data
#'
#' An example Performance Data that contains one model
#'
#' @format A data frame with 101 rows and 14 variables:
"one_pop_one_model_as_a_vector"


#' Example Performance Data
#'
#' An example Performance Data that contains one model
#'
#' @format A data frame with 101 rows and 14 variables:
"one_pop_one_model_as_a_vector_enforced_percentiles_symmetry"


#' Example Performance Data
#'
#' An example Performance Data that contains one model
#'
#' @format A data frame with 101 rows and 15 variables:
"one_pop_one_model_as_a_list"


#' Example Performance Data
#'
#' An example Performance Data that contains one model
#'
#' @format A data frame with 101 rows and 15 variables:
"one_pop_one_model_as_a_list_enforced_percentiles_symmetry"

#' Example Performance Data
#'
#' An example Performance Data that contains three models
#'
#' @format A data frame with 303 rows and 15 variables:
"one_pop_three_models"


#' Example Performance Data
#'
#' An example Performance Data that contains three models
#'
#' @format A data frame with 303 rows and 15 variables:
"one_pop_three_models_enforced_percentiles_symmetry"


#' Example Performance Data
#'
#' An example Performance Data that contains test and train sets
#'
#' @format A data frame with 202 rows and 15 variables:
"train_and_test_sets"


#' Example Performance Data
#'
#' An example Performance Data that contains test and train sets
#'
#' @format A data frame with 202 rows and 15 variables:
"train_and_test_sets_enforced_percentiles_symmetry"


#' Example Performance Data
#'
#' An example Performance Data that contains multiple models
#'
#' @format A data frame with 202 rows and 15 variables:
"multiple_models"

#' Example Performance Data
#'
#' An example Performance Data that contains multiple models
#' stratified by ppcr
#'
#' @format A data frame with 202 rows and 15 variables:
"multiple_models_by_ppcr"


#' Example Performance Data
#'
#' An example Performance Data that contains multiple populations
#'
#' @format A data frame with 202 rows and 15 variables:
"multiple_populations"

#' Example Performance Data
#'
#' An example Performance Data that contains multiple populations
#' stratified by ppcr
#'
#' @format A data frame with 202 rows and 15 variables:
"multiple_populations_by_ppcr"


#' Example Performance Data
#'
#' An example Performance Data that contains one model
#'
#' @format A data frame with 101 rows and 14 variables:
"one_pop_one_model"

#' Example Performance Data
#'
#' An example Performance Data that contains one model stratified by ppcr
#'
#' @format A data frame with 101 rows and 14 variables:
"one_pop_one_model_by_ppcr"


#' Synthetic Longitudinal Clinical Outcome Data
#'
#' A synthetic dataset containing 27 daily observations for 600 subjects across
#' four ordinal clinical outcome states ("Home", "Hospital", "Vent/ARDS", "Dead").
#'
#' @details
#' The dataset was generated using a first-order Markov simulation with
#' seed 2026, initial probabilities (0, 0.85, 0.15, 0), and transition matrix
#' \code{rbind(c(.97, .03, 0, 0), c(.10, .82, .06, .02), c(.01, .08, .84, .07), c(0, 0, 0, 1))}
#' where "Dead" is an absorbing state.
#'
#' This dataset draws inspiration from Frank Harrell's work on longitudinal
#' ordinal Markov and state-occupancy models and VIOLET 2 trial examples:
#' \url{https://hbiostat.org/r/hmisc/markov/sim#estimating-efficiency-from-the-study}.
#' It is independently simulated and is NOT Frank Harrell's original trial
#' data, fitted model, or an endorsed implementation.
#'
#' @format A data frame with 16,200 rows and 3 variables:
#' \describe{
#'   \item{id}{Subject identifier (integer 1-600)}
#'   \item{step}{Discrete step / day horizon (integer 1-27)}
#'   \item{reals}{Observed clinical state (ordered factor with levels "Home" < "Hospital" < "Vent/ARDS" < "Dead")}
#' }
"clinical_states_steps"
