#' Prediction Probability Histogram
#'
#' Create an interactive histogram of predicted probabilities, separated by
#' observed outcome. The browser controls can show the distribution relative
#' to either probability thresholds or predicted-positive classification rates
#' (PPCR).
#'
#' @param probs A list of numeric vectors containing predicted probabilities.
#'   A named list identifies models or populations in the rendered output.
#'   Probabilities must be between zero and one.
#' @param reals A list of numeric vectors containing observed binary outcomes,
#'   coded as zero or one. Supply one vector to evaluate multiple models in the
#'   same population, or one outcome vector for each probability vector to
#'   evaluate multiple populations.
#' @param by Numeric increment used to construct selectable probability
#'   thresholds or PPCR values. The default is `0.01`.
#' @param stratified_by Operating-point dimension. Use
#'   `"probability_threshold"` for probability cutoffs or `"ppcr"` for
#'   predicted-positive classification rates.
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

  render_rtichoke_viz_self_contained_browser(
    rtichoke_viz_prediction_distribution_spec(distribution_data)
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
  spec_json <- jsonlite::toJSON(spec, auto_unbox = TRUE, digits = NA)
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
#' Translate already-computed static prediction-distribution data into the
#' canonical rtichoke_viz contract without recomputing statistical quantities.
#'
#' @param distribution_data Output from
#'   [prepare_probs_distribution_data()].
#'
#' @return A nested list representing a canonical PredictionDistributionSpec.
#' @noRd
rtichoke_viz_prediction_distribution_spec <- function(distribution_data) {
  bins <- distribution_data$bins
  operating_points <- distribution_data$operating_points
  evaluation_names <- unique(as.character(operating_points$evaluation))
  evaluation_ids <- stats::setNames(
    paste0("evaluation-", seq_along(evaluation_names)),
    evaluation_names
  )

  evaluations <- lapply(evaluation_names, function(evaluation_name) {
    evaluation_rows <- operating_points[
      operating_points$evaluation == evaluation_name,
      ,
      drop = FALSE
    ]
    evaluation <- list(
      id = unname(evaluation_ids[[evaluation_name]]),
      population = as.character(evaluation_rows$population[[1]])
    )
    model <- as.character(evaluation_rows$model[[1]])
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

  canonical_operating_points <- lapply(
    seq_len(nrow(operating_points)),
    function(row_index) {
      list(
        evaluationId = unname(evaluation_ids[[as.character(
          operating_points$evaluation[[row_index]]
        )]]),
        type = as.character(operating_points$type[[row_index]]),
        value = as.numeric(operating_points$value[[row_index]]),
        cutoff = as.numeric(operating_points$cutoff[[row_index]]),
        realizedPpcr = as.numeric(
          operating_points$realized_ppcr[[row_index]]
        )
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
    operatingPoints = canonical_operating_points
  )
}
