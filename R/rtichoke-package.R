#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom graphics text
#' @importFrom pROC auc
#' @importFrom stats approx
#' @importFrom stats lowess
#' @importFrom tune collect_predictions
## usethis namespace: end
NULL

utils::globalVariables(c(
  ".", "FN", "FP", "Model", "N", "NB", "NB_intervention_all", "NB_plot", "NPV", "PPV", "Population", "TN",
  "TP", "Threshold", "add_lines", "add_markers",
  "add_prevalence_layers_to_gains_curve", "blockCheckbox", "counts",
  "display_predicted_postivies", "lift", "linetype", "main_slider", "metric_plot",
  "mids", "n_obs", "plot_ly", "plot_predicted_positives", "population", "ppcr",
  "prevalence", "probability_threshold", "quintile", "reactable", "reference_group",
  "sensitivity", "specificity", "stratified_by", "x", "y"
))
