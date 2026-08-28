#' Build a canonical rtichoke_viz v2 calibration specification
#'
#' Adapt already-computed calibration curve data plus explicit semantic
#' evaluation metadata into the canonical rtichoke_viz v2 contract. This
#' helper does not calculate calibration statistics and is not wired into
#' production rendering.
#'
#' @param calibration_curve_list Output from [create_calibration_curve_list()].
#' @param evaluation_metadata Output from [build_evaluation_metadata()].
#' @param method Calibration representation to embed: `"discrete"` or
#'   `"smooth"`.
#'
#' @return A nested list representing a canonical calibration v2 specification.
#' @noRd
rtichoke_viz_calibration_v2_spec <- function(
  calibration_curve_list,
  evaluation_metadata,
  method = c("discrete", "smooth")
) {
  method <- match.arg(method)
  required_metadata <- c("model", "population", "evaluation")
  missing_metadata <- setdiff(required_metadata, names(evaluation_metadata))
  if (length(missing_metadata) > 0L) {
    stop(
      "Calibration evaluation metadata is missing columns: ",
      paste(missing_metadata, collapse = ", "),
      call. = FALSE
    )
  }
  if (nrow(evaluation_metadata) == 0L) {
    stop(
      "Calibration evaluation metadata must contain at least one evaluation",
      call. = FALSE
    )
  }

  calibration_data <- if (method == "discrete") {
    calibration_curve_list$deciles_dat
  } else {
    calibration_curve_list$smooth_dat
  }
  valid_data_rows <- is.finite(as.numeric(calibration_data$x)) &
    is.finite(as.numeric(calibration_data$y))
  calibration_data <- calibration_data[valid_data_rows, , drop = FALSE]

  histogram <- calibration_curve_list$histogram_for_calibration

  required_data_columns <- c("reference_group", "x", "y")
  if (method == "discrete") {
    required_data_columns <- c(
      required_data_columns,
      "sum_reals",
      "total_obs"
    )
  }
  missing_data_columns <- setdiff(
    required_data_columns,
    names(calibration_data)
  )
  if (length(missing_data_columns) > 0L) {
    stop(
      "Calibration data is missing columns: ",
      paste(missing_data_columns, collapse = ", "),
      call. = FALSE
    )
  }

  required_histogram_columns <- c("reference_group", "mids", "counts")
  missing_histogram_columns <- setdiff(
    required_histogram_columns,
    names(histogram)
  )
  if (length(missing_histogram_columns) > 0L) {
    stop(
      "Calibration histogram is missing columns: ",
      paste(missing_histogram_columns, collapse = ", "),
      call. = FALSE
    )
  }

  groups <- unique(c(
    as.character(calibration_data$reference_group),
    as.character(histogram$reference_group)
  ))
  metadata_groups <- as.character(evaluation_metadata$evaluation)
  missing_groups <- setdiff(groups, metadata_groups)
  if (length(missing_groups) > 0L) {
    stop(
      "Calibration rows are missing evaluation metadata: ",
      paste(missing_groups, collapse = ", "),
      call. = FALSE
    )
  }

  used_metadata <- evaluation_metadata[
    evaluation_metadata$evaluation %in% groups,
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
    model_known <- !is.na(metadata$model) && nzchar(metadata$model)
    display_value <- if (model_known) {
      as.character(metadata$model)
    } else {
      as.character(metadata$population)
    }
    list(
      id = unname(series_ids[[group]]),
      evaluationId = unname(evaluation_ids[[group]]),
      display = list(
        label = display_value,
        group = display_value,
        role = if (model_known) "model" else "population"
      )
    )
  })

  data <- lapply(seq_len(nrow(calibration_data)), function(i) {
    group <- as.character(calibration_data$reference_group[[i]])
    datum <- list(
      seriesId = unname(series_ids[[group]]),
      predicted = as.numeric(calibration_data$x[[i]]),
      observed = as.numeric(calibration_data$y[[i]]),
      method = method
    )
    if (method == "discrete") {
      datum$events <- as.numeric(calibration_data$sum_reals[[i]])
      datum$total <- as.numeric(calibration_data$total_obs[[i]])
    }
    datum
  })

  distribution <- lapply(seq_len(nrow(histogram)), function(i) {
    group <- as.character(histogram$reference_group[[i]])
    list(
      seriesId = unname(series_ids[[group]]),
      midpoint = as.numeric(histogram$mids[[i]]),
      count = as.numeric(histogram$counts[[i]]),
      binWidth = 0.01
    )
  })

  list(
    schemaVersion = "2.0",
    type = "calibration",
    evaluations = evaluations,
    series = series,
    data = data,
    distribution = distribution,
    x = "predicted",
    y = "observed",
    xAxis = list(label = "Predicted probability", domain = c(0, 1)),
    yAxis = if (method == "smooth") {
      list(label = "Observed probability")
    } else {
      list(label = "Observed probability", domain = c(0, 1))
    },
    references = list(list(type = "identity", scope = "global"))
  )
}
