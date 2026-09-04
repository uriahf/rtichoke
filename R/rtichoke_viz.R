rtichoke_viz_roc_spec <- function(performance_data) {
  required_columns <- c(
    "probability_threshold",
    "sensitivity",
    "specificity"
  )
  missing_columns <- setdiff(required_columns, names(performance_data))

  if (length(missing_columns) > 0) {
    stop(
      "ROC performance data is missing columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  series <- if ("model" %in% names(performance_data)) {
    as.character(performance_data$model)
  } else if ("population" %in% names(performance_data)) {
    as.character(performance_data$population)
  } else {
    rep("model", nrow(performance_data))
  }

  data <- lapply(seq_len(nrow(performance_data)), function(i) {
    list(
      model = series[[i]],
      cutoff = performance_data$probability_threshold[[i]],
      sensitivity = performance_data$sensitivity[[i]],
      specificity = performance_data$specificity[[i]]
    )
  })

  list(
    schemaVersion = "1.0",
    type = "roc",
    data = data,
    x = "false_positive_rate",
    y = "sensitivity",
    xAxis = list(label = "1 - Specificity", domain = c(0, 1)),
    yAxis = list(label = "Sensitivity", domain = c(0, 1)),
    references = list(list(type = "identity"))
  )
}

rtichoke_viz_calibration_spec <- function(calibration_curve_list) {
  calibration_bins_dat <- calibration_curve_list$calibration_bins_dat
  histogram <- calibration_curve_list$histogram_for_calibration

  required_calibration_bin_columns <- c(
    "reference_group",
    "x",
    "y",
    "sum_reals",
    "total_obs"
  )
  missing_calibration_bin_columns <- setdiff(
    required_calibration_bin_columns,
    names(calibration_bins_dat)
  )
  if (length(missing_calibration_bin_columns) > 0) {
    stop(
      "Calibration data is missing columns: ",
      paste(missing_calibration_bin_columns, collapse = ", "),
      call. = FALSE
    )
  }

  required_histogram_columns <- c(
    "reference_group",
    "mids",
    "counts"
  )
  missing_histogram_columns <- setdiff(
    required_histogram_columns,
    names(histogram)
  )
  if (length(missing_histogram_columns) > 0) {
    stop(
      "Calibration histogram is missing columns: ",
      paste(missing_histogram_columns, collapse = ", "),
      call. = FALSE
    )
  }

  data <- lapply(seq_len(nrow(calibration_bins_dat)), function(i) {
    list(
      model = as.character(calibration_bins_dat$reference_group[[i]]),
      predicted = calibration_bins_dat$x[[i]],
      observed = calibration_bins_dat$y[[i]],
      method = "discrete",
      events = calibration_bins_dat$sum_reals[[i]],
      total = calibration_bins_dat$total_obs[[i]]
    )
  })

  distribution <- lapply(seq_len(nrow(histogram)), function(i) {
    list(
      model = as.character(histogram$reference_group[[i]]),
      midpoint = histogram$mids[[i]],
      count = histogram$counts[[i]],
      binWidth = 0.01
    )
  })

  list(
    schemaVersion = "1.0",
    type = "calibration",
    data = data,
    distribution = distribution,
    x = "predicted",
    y = "observed",
    xAxis = list(
      label = "Predicted probability",
      domain = c(0, 1)
    ),
    yAxis = list(
      label = "Observed probability",
      domain = c(0, 1)
    ),
    references = list(list(type = "identity"))
  )
}

write_rtichoke_viz_roc_proof <- function(performance_data, output_dir) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  spec_json <- jsonlite::toJSON(
    rtichoke_viz_roc_spec(performance_data),
    auto_unbox = TRUE,
    digits = NA
  )
  spec_json <- gsub("</", "<\\/", spec_json, fixed = TRUE)

  html <- paste0(
    "<!doctype html>\n",
    "<html lang=\"en\">\n",
    "<head>\n",
    "  <meta charset=\"utf-8\">\n",
    "  <meta name=\"viewport\" ",
    "content=\"width=device-width, initial-scale=1\">\n",
    "  <title>rtichoke_viz R ROC proof</title>\n",
    "  <link rel=\"stylesheet\" href=\"./rtichoke-viz.css\">\n",
    "</head>\n",
    "<body>\n",
    "  <div id=\"roc-chart\" class=\"rtichoke-viz-chart\"></div>\n",
    "  <script id=\"roc-spec\" type=\"application/json\">",
    spec_json,
    "</script>\n",
    "  <script type=\"module\">\n",
    "    import { renderRoc } from \"./rtichoke-viz.js\";\n",
    "    const spec = JSON.parse(\n",
    "      document.querySelector(\"#roc-spec\").textContent\n",
    "    );\n",
    "    document.querySelector(\"#roc-chart\").append(renderRoc(spec));\n",
    "  </script>\n",
    "</body>\n",
    "</html>\n"
  )

  output_path <- file.path(output_dir, "index.html")
  writeLines(html, output_path, useBytes = TRUE)
  invisible(output_path)
}

write_rtichoke_viz_calib_proof <- function(
  calibration_curve_list,
  output_dir
) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  spec_json <- jsonlite::toJSON(
    rtichoke_viz_calibration_spec(calibration_curve_list),
    auto_unbox = TRUE,
    digits = NA
  )
  spec_json <- gsub("</", "<\\/", spec_json, fixed = TRUE)

  html <- paste0(
    "<!doctype html>\n",
    "<html lang=\"en\">\n",
    "<head>\n",
    "  <meta charset=\"utf-8\">\n",
    "  <meta name=\"viewport\" ",
    "content=\"width=device-width, initial-scale=1\">\n",
    "  <title>rtichoke_viz R calibration proof</title>\n",
    "  <link rel=\"stylesheet\" href=\"./rtichoke-viz.css\">\n",
    "</head>\n",
    "<body>\n",
    "  <div id=\"calibration-chart\" ",
    "class=\"rtichoke-viz-chart\"></div>\n",
    "  <script id=\"calibration-spec\" ",
    "type=\"application/json\">",
    spec_json,
    "</script>\n",
    "  <script type=\"module\">\n",
    "    import { renderCalibration } ",
    "from \"./rtichoke-viz.js\";\n",
    "    const spec = JSON.parse(\n",
    "      document.querySelector(\"#calibration-spec\").textContent\n",
    "    );\n",
    "    document.querySelector(\"#calibration-chart\")\n",
    "      .append(renderCalibration(spec));\n",
    "  </script>\n",
    "</body>\n",
    "</html>\n"
  )

  output_path <- file.path(output_dir, "index.html")
  writeLines(html, output_path, useBytes = TRUE)
  invisible(output_path)
}
