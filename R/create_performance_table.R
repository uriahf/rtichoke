#' Create Performance Table
#' Makes a Performance Table
#'
#' @param probs a vector of estimated probabilities or a list if vectors of that kind (one for each model)
#' @param real a vector of binary outcomes
#' @param by number: increment of the sequence.
#' @param enforce_riskpercentiles_symmetry in case a symmetry between the risk percentiles is desired
#'
#' @export



create_performance_table <- function(probs, real, by = 0.01, 
                                     enforce_riskpercentiles_symmetry = F) {
  . <- threshold <-NULL 
  
  if ((probs %>% purrr::map_lgl(~ any(.x > 1)) %>% any())) {
    stop("This is an error!")
  }

  if (is.list(probs)) {
    if (is.null(names(probs))) {
      names(probs) <- paste("model", 1:length(probs))
    }
    return(probs %>%
      purrr::map(~ create_performance_table(probs = ., 
                                            real = real, 
                                            by = by, 
                                            enforce_riskpercentiles_symmetry = enforce_riskpercentiles_symmetry)) %>%
      dplyr::bind_rows(.id = "model"))
  }

  N <- TP <- TN <- FP <- FN <- NULL
  N <- length(probs)

  data.frame(
    threshold = if (enforce_riskpercentiles_symmetry) stats::quantile(probs, probs = rev(seq(0, 1, by = by))) else round(seq(0, 1, by = by), digits = nchar(format(by, scientific = F)))
  ) %>%
    {
      if (enforce_riskpercentiles_symmetry) dplyr::mutate(., percentpositives = round(seq(0, 1, by = by), digits = nchar(format(by, scientific = F)))) else .
    } %>%
    dplyr::mutate(
      TP = lapply(threshold, function(x) sum(probs[real == 1] > x)) %>%
        unlist(),
      TN = lapply(threshold, function(x) sum(probs[real == 0] < x)) %>%
        unlist()
    ) %>%
    dplyr::mutate(
      FN = sum(real) - TP,
      FP = N - sum(real) - TN,
      TPR = TP / (TP + FN),
      FPR = FP / (FP + TN),
      lift = (TP / (TP + FN)) / ((TP + FP) / N),
      specificity = TN / (TN + FP),
      ppv = TP / (TP + FP),
      npv = TN / (TN + FN),
      positives = TP + FP,
      NB = TP / N - (FP / N) * (threshold / (1 - threshold))
    ) %>%
    {
      if (!enforce_riskpercentiles_symmetry) dplyr::mutate(., percentpositives = (TP + FP) / N) else .
    }
}
