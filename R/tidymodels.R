#' @rdname create_roc_curve
#' @exportS3Method create_roc_curve tune_results
create_roc_curve.tune_results <- function(x, ...) {
  predictions <- tune::collect_predictions(x)

  outcome_name <- x$outcomes
  outcome_levels <- levels(predictions[[outcome_name]])
  event_level <- outcome_levels[2]
  prob_col_name <- paste0(".pred_", event_level)

  # Group by model configuration
  model_configs <- unique(predictions$.config)

  probs <- list()
  for(config in model_configs) {
    probs[[config]] <- predictions[predictions$.config == config, ][[prob_col_name]]
  }

  # The reals are tricky because of the resampling.
  # We can group by model config, and then get the reals for each.
  # They should be in the same order as the probs.
  reals_per_model <- list()
  for(config in model_configs) {
    reals_per_model[[config]] <- as.numeric(predictions[predictions$.config == config, ][[outcome_name]] == event_level)
  }

  # Since we are comparing models, the 'reals' list should have one element
  # containing the true outcomes. We assume the order of outcomes is the same
  # for each model configuration, which it should be.
  reals <- list(reals_per_model[[1]])

  create_roc_curve.default(x = probs, reals = reals, ...)
}
