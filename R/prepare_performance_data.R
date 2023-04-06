#' Prepare Performance Data
#'
#' The prepare_performance_data function makes a Performance Data that is made
#' of different cutoffs.
#' Each row represents a cutoff and each column stands for a performance metric.
#' It is possible to use this function for more than one model in order to
#' compare different models performance for the same population.
#' In this case the user should use a list that is made of vectors of estimated
#' probabilities, one for each model.
#'
#' Sometime instead of using a cutoff for the estimated probability it
#' is required to enforce a symmetry between the percentiles of the
#' probabilities, in medicine it is referred as "Risk Percentile" when the
#' outcome stands for something negative in essence such as a severe disease
#' or death: Let's say that we want to see the model performance for the top 5%
#' patients at risk for some well defined population, in this case the user
#' should change the parameter stratified_by from the default
#' "probability_threshold" to "predicted_positives" and the results will be
#' similar Performance Data,
#' only this time each row will represent some rounded percentile.
#'
#'
#' @param probs a list of vectors of estimated probabilities
#' (one for each model or one for each population)
#' @param reals a list of vectors of binary outcomes (one for each population)
#' @param by number: increment of the sequence.
#' @param stratified_by Performance Metrics can be stratified by Probability
#' Threshold or alternatively by Predicted Positives Condition Rate
#'
#' @examples
#' # You can prepare Performance Data for one model
#'
#' prepare_performance_data(
#'   probs = list(example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome)
#' )
#'
#' prepare_performance_data(
#'   probs = list(example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome),
#'   stratified_by = "ppcr"
#' )
#'
#' # Several Models
#'
#' prepare_performance_data(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   reals = list(example_dat$outcome)
#' )
#'
#' prepare_performance_data(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   reals = list(example_dat$outcome),
#'   stratified_by = "ppcr"
#' )
#'
#'
#' # Several Populations
#'
#' prepare_performance_data(
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
#' prepare_performance_data(
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
#'   ),
#'   stratified_by = "ppcr"
#' )
#'
#' @export
prepare_performance_data <- function(probs,
                                     reals,
                                     by = 0.01,
                                     stratified_by = "probability_threshold") {
  . <- probability_threshold <- NULL

  check_probs_input(probs)
  # check_real_input(reals)

  match.arg(stratified_by, c("probability_threshold", "ppcr"))

  if ((probs %>% purrr::map_lgl(~ any(.x > 1)) %>% any())) {
    stop("Probabilities mustn't be greater than one ")
  }

  if ((length(probs) > 1) & (length(reals) == 1)) {
    if (is.null(names(probs))) {
      names(probs) <- paste("model", seq_len(length(probs)))
    }

    return(
      probs %>%
        purrr::map(
          ~ prepare_performance_data(
            probs = list(.),
            reals = reals,
            by = by,
            stratified_by = stratified_by
          )
        ) %>%
        dplyr::bind_rows(.id = "model")
    )
  }

  if ((length(probs) > 1) & (length(reals) > 1)) {
    if (is.null(names(probs)) & is.null(names(reals))) {
      names(probs) <- paste("population", seq_len(length(probs)))
      names(reals) <- paste("population", seq_len(length(reals)))
    }
    return(purrr::map2_dfr(probs,
      reals,
      ~ prepare_performance_data(
        list(.x),
        list(.y),
        by = by,
        stratified_by = stratified_by
      ),
      .id = "population"
    ))
  }

  N <- TP <- TN <- FP <- FN <- NULL
  N <- length(probs[[1]])

  tibble::tibble(
    probability_threshold = if (stratified_by != "probability_threshold" &
                                length(unique(probs[[1]])) != 1) {
      stats::quantile(probs[[1]],
        probs = rev(seq(0, 1, by = by))
      )
    } else if (stratified_by != "probability_threshold" &
               length(unique(probs[[1]])) == 1) {
      
      c(0, 1)
      
    } else {
      round(
        seq(0, 1, by = by),
        digits = nchar(format(by, scientific = FALSE))
      )
    }
  ) %>%
    {
      if (stratified_by != "probability_threshold" &
          length(unique(probs[[1]])) != 1) {
        dplyr::mutate(.,
          ppcr = round(seq(0, 1, by = by),
            digits = nchar(format(by, scientific = FALSE))
          )
        )
      } else if (stratified_by != "probability_threshold" &
                 length(unique(probs[[1]])) == 1) {
        
        dplyr::mutate(.,
                      ppcr = c(1, 0)
        )
        
      } else {
        .
      }
    } %>%
    dplyr::mutate(
      TP = lapply(
        probability_threshold,
        function(x) {
          ifelse(x == 0,
            length(probs[[1]][reals[[1]] == 1]),
            sum(probs[[1]][reals[[1]] == 1] > x)
          )
        }
      ) %>%
        unlist(),
      TN = lapply(
        probability_threshold,
        function(x) {
          ifelse(x == 0, as.integer(0),
            sum(probs[[1]][reals[[1]] == 0] <= x)
          )
        }
      ) %>%
        unlist()
    ) %>%
    {
      if (stratified_by != "probability_threshold") {
        dplyr::mutate(., TN = dplyr::case_when(
          ppcr != 1 ~ TN,
          TRUE ~ as.integer(0)
        ))
      } else {
        .
      }
    } %>%
    {
      if (stratified_by != "probability_threshold") {
        dplyr::mutate(., TP = dplyr::case_when(
          ppcr != 1 ~ TP,
          TRUE ~ as.integer(sum(reals[[1]]))
        ))
      } else {
        .
      }
    } %>%
    dplyr::mutate(
      FN = sum(reals[[1]]) - TP,
      FP = N - sum(reals[[1]]) - TN,
      sensitivity = TP / (TP + FN),
      FPR = FP / (FP + TN),
      specificity = TN / (TN + FP),
      PPV = TP / (TP + FP),
      NPV = TN / (TN + FN),
      lift = (TP / (TP + FN)) / ((TP + FP) / N),
      predicted_positives = TP + FP
    ) %>%
    {
      if (stratified_by == "probability_threshold") {
        dplyr::mutate(., NB = TP / N - (FP / N) * (
          probability_threshold / (1 - probability_threshold)))
      } else {
        .
      }
    } %>%
    {
      if (stratified_by == "probability_threshold") dplyr::mutate(., ppcr = (TP + FP) / N) else .
    }
}
