#' Build a canonical rtichoke_viz v2 performance-table specification
#'
#' Translate already-computed performance data plus explicit semantic
#' evaluation metadata into the canonical sibling PerformanceTableSpec.
#' This helper performs presentation normalization only: all statistics come
#' from `prepare_performance_data()`.
#'
#' @param performance_data Output from [prepare_performance_data()].
#' @param evaluation_metadata Output from [build_evaluation_metadata()].
#' @param stratified_by Operating-point semantics used to prepare the data.
#'
#' @return A nested list representing a canonical PerformanceTableSpec.
#' @noRd
rtichoke_viz_performance_table_v2_spec <- function(
  performance_data,
  evaluation_metadata,
  stratified_by = "probability_threshold"
) {
  stratified_by <- match.arg(
    stratified_by,
    c("probability_threshold", "ppcr")
  )
  required_metadata <- c("model", "population", "evaluation")
  missing_metadata <- setdiff(required_metadata, names(evaluation_metadata))
  if (length(missing_metadata) > 0L) {
    stop(
      "Performance-table evaluation metadata is missing columns: ",
      paste(missing_metadata, collapse = ", "),
      call. = FALSE
    )
  }

  compatibility_group <- roc_v2_compatibility_group(
    performance_data,
    evaluation_metadata
  )
  metadata_group <- as.character(evaluation_metadata$evaluation)
  missing_groups <- setdiff(unique(compatibility_group), metadata_group)
  if (length(missing_groups) > 0L) {
    stop(
      "Performance rows are missing evaluation metadata: ",
      paste(missing_groups, collapse = ", "),
      call. = FALSE
    )
  }

  used_metadata <- evaluation_metadata[
    evaluation_metadata$evaluation %in% unique(compatibility_group),
    ,
    drop = FALSE
  ]
  used_groups <- as.character(used_metadata$evaluation)
  evaluation_ids <- stats::setNames(
    paste0("evaluation-", seq_len(nrow(used_metadata))),
    used_groups
  )

  evaluations <- lapply(seq_len(nrow(used_metadata)), function(i) {
    metadata <- used_metadata[i, , drop = FALSE]
    evaluation <- list(
      id = unname(evaluation_ids[[as.character(metadata$evaluation)]]),
      population = as.character(metadata$population)
    )
    if (!is.na(metadata$model) && nzchar(metadata$model)) {
      evaluation$model <- as.character(metadata$model)
    }
    evaluation
  })

  metric_map <- c(
    TP = "true_positives",
    TN = "true_negatives",
    FP = "false_positives",
    FN = "false_negatives",
    sensitivity = "sensitivity",
    specificity = "specificity",
    FPR = "false_positive_rate",
    PPV = "ppv",
    NPV = "npv",
    lift = "lift",
    predicted_positives = "predicted_positives",
    ppcr = "ppcr",
    NB = "net_benefit"
  )
  metric_labels <- c(
    true_positives = "True Positives",
    true_negatives = "True Negatives",
    false_positives = "False Positives",
    false_negatives = "False Negatives",
    sensitivity = "Sensitivity",
    specificity = "Specificity",
    false_positive_rate = "False Positive Rate",
    ppv = "PPV",
    npv = "NPV",
    lift = "Lift",
    predicted_positives = "Predicted Positives",
    ppcr = "Predicted Positives Condition Rate",
    net_benefit = "Net Benefit"
  )
  source_metrics <- names(metric_map)[names(metric_map) %in% names(performance_data)]
  metrics <- lapply(source_metrics, function(source) {
    id <- unname(metric_map[[source]])
    list(id = id, label = unname(metric_labels[[id]]))
  })

  operating_column <- if (stratified_by == "ppcr") "ppcr" else {
    "probability_threshold"
  }
  if (!operating_column %in% names(performance_data)) {
    stop("Performance data is missing operating-point column: ", operating_column,
      call. = FALSE
    )
  }

  canonical_number <- function(value) {
    value <- as.numeric(value)
    if (length(value) == 0L || is.na(value) || is.nan(value)) NULL else value
  }

  rows <- lapply(seq_len(nrow(performance_data)), function(i) {
    group <- compatibility_group[[i]]
    values <- lapply(source_metrics, function(source) {
      list(
        metricId = unname(metric_map[[source]]),
        estimate = canonical_number(performance_data[[source]][[i]])
      )
    })
    list(
      evaluationId = unname(evaluation_ids[[group]]),
      operatingPoint = list(
        type = stratified_by,
        value = as.numeric(performance_data[[operating_column]][[i]])
      ),
      values = values
    )
  })

  list(
    schemaVersion = "2.0",
    type = "performance_table",
    evaluations = evaluations,
    metrics = metrics,
    rows = rows
  )
}
