#' Build a canonical rtichoke_viz v2 ROC specification
#'
#' Translate already-computed ROC performance data plus explicit semantic
#' evaluation metadata into the canonical rtichoke_viz v2 contract. This
#' helper is deliberately internal and is not wired into production rendering
#' yet.
#'
#' @param performance_data Output from [prepare_performance_data()].
#' @param evaluation_metadata Output from [build_evaluation_metadata()].
#'
#' @return A nested list representing a canonical ROC v2 specification.
#' @noRd
rtichoke_viz_roc_v2_spec <- function(
  performance_data,
  evaluation_metadata
) {
  required_columns <- c(
    "probability_threshold",
    "sensitivity",
    "specificity"
  )
  missing_columns <- setdiff(required_columns, names(performance_data))
  if (length(missing_columns) > 0L) {
    stop(
      "ROC performance data is missing columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  required_metadata <- c("model", "population", "evaluation")
  missing_metadata <- setdiff(required_metadata, names(evaluation_metadata))
  if (length(missing_metadata) > 0L) {
    stop(
      "ROC evaluation metadata is missing columns: ",
      paste(missing_metadata, collapse = ", "),
      call. = FALSE
    )
  }

  if (nrow(evaluation_metadata) == 0L) {
    stop(
      "ROC evaluation metadata must contain at least one evaluation",
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
      "ROC performance rows are missing evaluation metadata: ",
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
  series_ids <- stats::setNames(
    paste0("series-", seq_len(nrow(used_metadata))),
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

  series <- lapply(seq_len(nrow(used_metadata)), function(i) {
    metadata <- used_metadata[i, , drop = FALSE]
    group <- as.character(metadata$evaluation)
    if (!is.na(metadata$model) && nzchar(metadata$model)) {
      display_value <- as.character(metadata$model)
      display_role <- "model"
    } else {
      display_value <- as.character(metadata$population)
      display_role <- "population"
    }

    list(
      id = unname(series_ids[[group]]),
      evaluationId = unname(evaluation_ids[[group]]),
      display = list(
        label = display_value,
        group = display_value,
        role = display_role
      )
    )
  })

  data <- lapply(seq_len(nrow(performance_data)), function(i) {
    group <- compatibility_group[[i]]
    list(
      seriesId = unname(series_ids[[group]]),
      cutoff = as.numeric(performance_data$probability_threshold[[i]]),
      sensitivity = as.numeric(performance_data$sensitivity[[i]]),
      specificity = as.numeric(performance_data$specificity[[i]])
    )
  })

  list(
    schemaVersion = "2.0",
    type = "roc",
    evaluations = evaluations,
    series = series,
    data = data,
    x = "false_positive_rate",
    y = "sensitivity",
    xAxis = list(label = "1 - Specificity", domain = c(0, 1)),
    yAxis = list(label = "Sensitivity", domain = c(0, 1)),
    references = list(list(type = "identity", scope = "global"))
  )
}

roc_v2_compatibility_group <- function(
  performance_data,
  evaluation_metadata
) {
  model_known <- !is.na(evaluation_metadata$model) &
    nzchar(evaluation_metadata$model)

  if (all(model_known) && "model" %in% names(performance_data)) {
    return(as.character(performance_data$model))
  }

  if (all(!model_known) && "population" %in% names(performance_data)) {
    return(as.character(performance_data$population))
  }

  if (nrow(evaluation_metadata) == 1L) {
    return(rep(
      as.character(evaluation_metadata$evaluation[[1]]),
      nrow(performance_data)
    ))
  }

  stop(
    paste0(
      "ROC performance data cannot be joined to semantic evaluation metadata ",
      "without an explicit compatibility grouping column"
    ),
    call. = FALSE
  )
}
