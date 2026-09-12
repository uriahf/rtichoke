#' Prediction Probability Histogram
#'
#' Create an interactive histogram of predicted probabilities, separated by
#' observed outcome. The browser controls can show the distribution relative
#' to either probability thresholds or predicted-positive classification rates
#' (PPCR).
#'
#' @inheritParams prepare_performance_data
#'
#' @return A browsable HTML object rendered by the vendored `rtichoke_viz`
#'   browser bundle. It can be displayed in the RStudio Viewer, embedded in
#'   R Markdown or Quarto, or saved as a single self-contained HTML file with
#'   [htmltools::save_html()].
#'
#' @export
#'
#' @examples
#' create_probs_histogram(
#'   probs = list(example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome),
#'   by = 0.1
#' )
#'
#' create_probs_histogram(
#'   probs = list(
#'     "Prediction Model" = example_dat$estimated_probabilities,
#'     "Random Guess" = example_dat$random_guess
#'   ),
#'   reals = list(example_dat$outcome),
#'   by = 0.1,
#'   stratified_by = "ppcr"
#' )
create_probs_histogram <- function(
  probs,
  reals,
  by = 0.01,
  stratified_by = "probability_threshold"
) {
  distribution_data <- prepare_probs_distribution_data(
    probs = probs,
    reals = reals,
    by = by,
    stratified_by = stratified_by
  )

  performance_data <- prepare_performance_data(
    probs = probs,
    reals = reals,
    by = by,
    stratified_by = stratified_by
  )

  render_rtichoke_viz_self_contained_browser(
    rtichoke_viz_prediction_distribution_spec(
      distribution_data = distribution_data,
      performance_data = performance_data
    )
  )
}

#' Render a self-contained canonical browser component
#'
#' Embed the vendored browser bundle and stylesheet directly so the saved HTML
#' can be opened from a local filesystem without a web server.
#'
#' @param spec A standalone canonical rtichoke_viz specification.
#'
#' @return A browsable self-contained HTML object.
#' @noRd
render_rtichoke_viz_self_contained_browser <- function(spec) {
  if (!identical(spec$type, "prediction_distribution")) {
    stop(
      "Self-contained browser rendering is not available for chart type: ",
      spec$type,
      call. = FALSE
    )
  }

  vendor_directory <- system.file("rtichoke-viz", package = "rtichoke")
  javascript_path <- file.path(vendor_directory, "rtichoke-viz.js")
  stylesheet_path <- file.path(vendor_directory, "rtichoke-viz.css")
  if (!file.exists(javascript_path) || !file.exists(stylesheet_path)) {
    stop("Vendored rtichoke_viz browser assets are unavailable", call. = FALSE)
  }

  component_id <- rtichoke_viz_browser_id()
  spec_json <- jsonlite::toJSON(
    spec,
    auto_unbox = TRUE,
    null = "null",
    digits = NA
  )
  javascript <- readChar(
    javascript_path,
    nchars = file.info(javascript_path)$size,
    useBytes = TRUE
  )
  Encoding(javascript) <- "UTF-8"
  javascript_json <- jsonlite::toJSON(
    javascript,
    auto_unbox = TRUE,
    pretty = FALSE
  )
  stylesheet <- readChar(
    stylesheet_path,
    nchars = file.info(stylesheet_path)$size,
    useBytes = TRUE
  )
  Encoding(stylesheet) <- "UTF-8"

  spec_json <- gsub("</", "<\\/", spec_json, fixed = TRUE)
  javascript_json <- gsub("</", "<\\/", javascript_json, fixed = TRUE)
  module_script <- paste0(
    "const source = JSON.parse(document.querySelector('#",
    component_id,
    "-bundle').textContent);\n",
    "const moduleUrl = URL.createObjectURL(new Blob([source], ",
    "{ type: 'text/javascript' }));\n",
    "const { renderPredictionDistribution } = await import(moduleUrl);\n",
    "const spec = JSON.parse(document.querySelector('#",
    component_id,
    "-spec').textContent);\n",
    "document.querySelector('#",
    component_id,
    "').append(renderPredictionDistribution(spec));\n",
    "URL.revokeObjectURL(moduleUrl);"
  )

  htmltools::browsable(htmltools::tagList(
    htmltools::tags$style(htmltools::HTML(stylesheet)),
    htmltools::tags$div(
      id = component_id,
      class = "rtichoke-viz-chart"
    ),
    htmltools::tags$script(
      id = paste0(component_id, "-spec"),
      type = "application/json",
      htmltools::HTML(spec_json)
    ),
    htmltools::tags$script(
      id = paste0(component_id, "-bundle"),
      type = "application/json",
      htmltools::HTML(javascript_json)
    ),
    htmltools::tags$script(
      type = "module",
      htmltools::HTML(module_script)
    )
  ))
}

#' Build a canonical PredictionDistributionSpec
#'
#' Translate already-computed static prediction-distribution data and
#' producer-owned operating-point performance data into the canonical
#' rtichoke_viz contract.
#'
#' @param distribution_data Output from [prepare_probs_distribution_data()].
#' @param performance_data Output from [prepare_performance_data()].
#'
#' @return A nested list representing a canonical PredictionDistributionSpec.
#' @noRd
rtichoke_viz_prediction_distribution_spec <- function(
  distribution_data,
  performance_data
) {
  bins <- distribution_data$bins
  rank_bins <- distribution_data$rank_bins
  operating_points <- distribution_data$operating_points

  evaluation_metadata <- unique(distribution_data$operating_points[,
    c("evaluation", "model", "population"),
    drop = FALSE
  ])
  evaluation_names <- as.character(evaluation_metadata$evaluation)
  evaluation_ids <- stats::setNames(
    paste0("evaluation-", seq_along(evaluation_names)),
    evaluation_names
  )

  evaluations <- lapply(seq_len(nrow(evaluation_metadata)), function(i) {
    row <- evaluation_metadata[i, , drop = FALSE]
    eval_name <- as.character(row$evaluation[[1]])
    evaluation <- list(
      id = unname(evaluation_ids[[eval_name]]),
      population = as.character(row$population[[1]])
    )
    model <- as.character(row$model[[1]])
    if (!is.na(model) && nzchar(model)) {
      evaluation$model <- model
    }
    evaluation
  })

  canonical_bins <- lapply(seq_len(nrow(bins)), function(row_index) {
    list(
      evaluationId = unname(evaluation_ids[[as.character(
        bins$evaluation[[row_index]]
      )]]),
      lower = as.numeric(bins$lower[[row_index]]),
      upper = as.numeric(bins$upper[[row_index]]),
      includeLower = isTRUE(bins$include_lower[[row_index]]),
      includeUpper = isTRUE(bins$include_upper[[row_index]]),
      nPositive = as.integer(bins$n_positive[[row_index]]),
      nNegative = as.integer(bins$n_negative[[row_index]])
    )
  })

  canonical_rank_bins <- lapply(seq_len(nrow(rank_bins)), function(row_index) {
    list(
      evaluationId = unname(evaluation_ids[[as.character(
        rank_bins$evaluation[[row_index]]
      )]]),
      rankLower = as.numeric(rank_bins$rank_lower[[row_index]]),
      rankUpper = as.numeric(rank_bins$rank_upper[[row_index]]),
      positiveMass = as.integer(rank_bins$n_positive[[row_index]]),
      negativeMass = as.integer(rank_bins$n_negative[[row_index]])
    )
  })

  # Normalize performance_data evaluation matching
  perf_eval_group <- if (
    "model" %in%
      names(performance_data) &&
      "population" %in% names(performance_data)
  ) {
    # Could be multiple models or populations
    if (length(unique(performance_data$model)) > 1) {
      as.character(performance_data$model)
    } else if (length(unique(performance_data$population)) > 1) {
      as.character(performance_data$population)
    } else {
      as.character(performance_data$model)
    }
  } else if ("model" %in% names(performance_data)) {
    as.character(performance_data$model)
  } else if ("population" %in% names(performance_data)) {
    as.character(performance_data$population)
  } else {
    rep(evaluation_names[[1]], nrow(performance_data))
  }

  metric_cols <- list(
    true_positives = "TP",
    true_negatives = "TN",
    false_positives = "FP",
    false_negatives = "FN",
    sensitivity = "sensitivity",
    specificity = "specificity",
    ppv = "PPV",
    npv = "NPV",
    lift = "lift"
  )

  canonical_operating_points <- lapply(
    seq_len(nrow(operating_points)),
    function(row_index) {
      op_row <- operating_points[row_index, , drop = FALSE]
      eval_name <- as.character(op_row$evaluation[[1]])
      op_type <- as.character(op_row$type[[1]])
      op_value <- as.numeric(op_row$value[[1]])

      # Match performance_data exactly 1-to-1
      perf_match_mask <- (perf_eval_group == eval_name)
      if (op_type == "probability_threshold") {
        perf_match_mask <- perf_match_mask &
          (as.numeric(performance_data$probability_threshold) == op_value)
      } else if (op_type == "ppcr") {
        perf_match_mask <- perf_match_mask &
          (as.numeric(performance_data$ppcr) == op_value)
      } else {
        stop("Unsupported operating point type: ", op_type, call. = FALSE)
      }

      matching_perf_rows <- performance_data[perf_match_mask, , drop = FALSE]
      if (nrow(matching_perf_rows) != 1L) {
        stop(
          sprintf(
            "Operating point join failed for evaluation '%s', type '%s', value %g: expected exactly 1 performance row, found %d",
            eval_name,
            op_type,
            op_value,
            nrow(matching_perf_rows)
          ),
          call. = FALSE
        )
      }

      perf_row <- matching_perf_rows[1, , drop = FALSE]

      perf_entries <- lapply(names(metric_cols), function(canonical_metric_id) {
        col_name <- metric_cols[[canonical_metric_id]]
        val <- perf_row[[col_name]][[1]]
        estimate <- if (is.null(val) || is.na(val) || !is.finite(val)) {
          NULL
        } else if (
          canonical_metric_id %in%
            c(
              "true_positives",
              "true_negatives",
              "false_positives",
              "false_negatives"
            )
        ) {
          as.integer(val)
        } else {
          as.numeric(val)
        }
        list(
          metricId = canonical_metric_id,
          estimate = estimate
        )
      })

      list(
        evaluationId = unname(evaluation_ids[[eval_name]]),
        type = op_type,
        value = op_value,
        cutoff = as.numeric(op_row$cutoff[[1]]),
        realizedPpcr = as.numeric(op_row$realized_ppcr[[1]]),
        performance = perf_entries
      )
    }
  )

  list(
    schemaVersion = "2.0",
    type = "prediction_distribution",
    evaluations = evaluations,
    operatingPoint = list(
      dimension = as.character(operating_points$type[[1]])
    ),
    bins = canonical_bins,
    rankBins = canonical_rank_bins,
    operatingPoints = canonical_operating_points
  )
}
