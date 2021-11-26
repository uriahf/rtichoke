#' Prepare Performance Data
#'
#' The prepare_performance_data function makes a Performance Data that is made
#' of different cutoffs.
#' Each row represents a cutoff and each column stands for a performance metric.
#' It is possible to use this function for more than one model in order to compare
#' different models performance for the same population. In this case the user should
#' use a list that is made of vectors of estimated probabilities, one for each model.
#'
#' Sometime instead of using a cutoff for the estimated probability it is required
#' to enforce a symmetry between the percentiles of the probabilities, in medicine
#' it is referred as "Risk Percentile" when the outcome stands for something negative
#' in essence such as a severe disease or death: Let's say that we want to see the
#' model performance for the top 5% patients at risk for some well defined population,
#' in this case the user should change the parameter stratified_by
#' from the default "probability_threshold" to "predicted_positives" and the 
#' results will be similar Performance Data,
#' only this time each row will represent some rounded percentile.
#'
#'
#' @param probs a vector of estimated probabilities or a list of vectors of that
#' kind (one for each model)
#' @param real a vector of binary outcomes or a list of vectors of that
#' kind (one for each population)
#' @param by number: increment of the sequence.
#' @param stratified_by Performance Metrics can be stratified by Probability
#' Threshold or alternatively by Predicted Positives Condition Rate
#'
#' @examples
#' # You can prepare Performance Data for one model
#'
#' prepare_performance_data(
#'   probs = example_dat$estimated_probabilities,
#'   real = example_dat$outcome
#' )
#'
#' # And you can prepare Performance Data for more than one model
#' prepare_performance_data(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   real = example_dat$outcome
#' )
#'
#' # Notice that once you've put a list in the probs parameter you'll receive a new
#' # column in the Performance Data named "Model". If it's a named list (like in our
#' # example) the Model column will mention the names of each element in the probs-list
#' # as the name of the model, if it's unnamed list the Models will count "Model 1",
#' # "Model 2", etc.. according to the order of the estimated-probabilities vector in the
#' # list.
#'
#'
#' prepare_performance_data(
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
#' @export
prepare_performance_data <- function(probs, real, by = 0.01,
                                     stratified_by = "probability_threshold") {
  . <- threshold <- NULL

  if ((probs %>% purrr::map_lgl(~ any(.x > 1)) %>% any())) {
    stop("Probabilities mustn't be greater than one ")
  }

  if (is.list(probs) & !is.list(real)) {
    if (is.null(names(probs))) {
      names(probs) <- paste("model", 1:length(probs))
    }
    return(probs %>%
      purrr::map(~ prepare_performance_data(
        probs = .,
        real = real,
        by = by,
        stratified_by = stratified_by
      )) %>%
      dplyr::bind_rows(.id = "model"))
  }

  if (is.list(probs) & is.list(real)) {
    if (is.null(names(probs)) & is.null(names(real))) {
      names(probs) <- paste("population", 1:length(probs))
      names(real) <- paste("population", 1:length(real))
    }
    return(purrr::map2_dfr(probs,
      real,
      ~ prepare_performance_data(.x, .y,
                                 by = by,
                                 stratified_by = stratified_by),
      .id = "population"
    ))
  }

  N <- TP <- TN <- FP <- FN <- NULL
  N <- length(probs)

  tibble::tibble(
    threshold = if (stratified_by != "probability_threshold") stats::quantile(probs, probs = rev(seq(0, 1, by = by))) else round(seq(0, 1, by = by), digits = nchar(format(by, scientific = F)))
  ) %>%
    {
      if (stratified_by != "probability_threshold") dplyr::mutate(., ppcr  = round(seq(0, 1, by = by), digits = nchar(format(by, scientific = F)))) else .
    } %>%
    dplyr::mutate(
      TP = lapply(threshold, function(x) sum(probs[real == 1] > x)) %>%
        unlist(),
      TN = lapply(threshold, function(x) sum(probs[real == 0] <= x)) %>%
        unlist()
    ) %>%
    dplyr::mutate(
      FN = sum(real) - TP,
      FP = N - sum(real) - TN,
      sensitivity = TP / (TP + FN),
      FPR = FP / (FP + TN),
      lift = (TP / (TP + FN)) / ((TP + FP) / N),
      specificity = TN / (TN + FP),
      PPV = TP / (TP + FP),
      NPV = TN / (TN + FN),
      positives = TP + FP,
      NB = TP / N - (FP / N) * (threshold / (1 - threshold))
    ) %>%
    {
      if (stratified_by == "probability_threshold") dplyr::mutate(., ppcr  = (TP + FP) / N) else .
    }
}
