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
  rtichoke_viz_curve_v2_spec(
    performance_data,
    evaluation_metadata,
    type = "roc"
  )
}

rtichoke_viz_renderer <- function(renderer, interactive) {
  renderer <- match.arg(
    renderer,
    c("default", "ggplot2", "plotly", "browser")
  )
  if (renderer == "default") {
    return(if (isTRUE(interactive)) "plotly" else "ggplot2")
  }
  renderer
}

rtichoke_viz_browser_id <- local({
  next_id <- 0L
  function() {
    next_id <<- next_id + 1L
    paste0("rtichoke-viz-", next_id)
  }
})

render_rtichoke_viz_browser <- function(spec) {
  renderers <- c(
    roc = "renderRocV2",
    gains = "renderGainsV2",
    lift = "renderLiftV2"
  )
  renderer <- if (spec$type %in% names(renderers)) {
    unname(renderers[[spec$type]])
  } else {
    NULL
  }
  if (is.null(renderer)) {
    stop(
      "Browser rendering is not available for chart type: ",
      spec$type,
      call. = FALSE
    )
  }

  id <- rtichoke_viz_browser_id()
  json <- jsonlite::toJSON(spec, auto_unbox = TRUE, digits = NA)
  json <- gsub("</", "<\\/", json, fixed = TRUE)
  dependency <- htmltools::htmlDependency(
    name = "rtichoke-viz",
    version = "0.5.0",
    src = c(file = system.file("rtichoke-viz", package = "rtichoke")),
    script = list(src = "rtichoke-viz.js", type = "module"),
    stylesheet = "rtichoke-viz.css"
  )
  script <- paste0(
    "import { ",
    renderer,
    " } from './lib/rtichoke-viz-0.5.0/rtichoke-viz.js';\n",
    "const spec = JSON.parse(document.querySelector('#",
    id,
    "-spec').textContent);\n",
    "document.querySelector('#",
    id,
    "').append(",
    renderer,
    "(spec));"
  )

  htmltools::browsable(htmltools::attachDependencies(
    htmltools::tagList(
      htmltools::tags$div(id = id, class = "rtichoke-viz-chart"),
      htmltools::tags$script(
        id = paste0(id, "-spec"),
        type = "application/json",
        htmltools::HTML(json)
      ),
      htmltools::tags$script(type = "module", htmltools::HTML(script))
    ),
    dependency
  ))
}

#' Build a canonical rtichoke_viz v2 lift specification
#'
#' Translate already-computed lift performance data plus explicit semantic
#' evaluation metadata into the canonical rtichoke_viz v2 contract. Perfect
#' model reference geometry is derived from the same production prevalence
#' values used by the existing lift renderer. This helper is deliberately
#' internal and is not wired into production rendering yet.
#'
#' @inheritParams rtichoke_viz_roc_v2_spec
#'
#' @return A nested list representing a canonical lift v2 specification.
#' @noRd
rtichoke_viz_lift_v2_spec <- function(
  performance_data,
  evaluation_metadata
) {
  spec <- rtichoke_viz_curve_v2_spec(
    performance_data,
    evaluation_metadata,
    type = "lift"
  )

  populations <- unique(vapply(
    spec$evaluations,
    `[[`,
    character(1),
    "population"
  ))
  prevalence <- v2_population_prevalence(
    performance_data,
    evaluation_metadata,
    populations
  )

  perfect_references <- lapply(populations, function(population) {
    p <- as.numeric(prevalence[[population]])
    inv_p <- 1 / p
    list(
      type = "path",
      scope = "population",
      population = population,
      label = "Perfect Model",
      points = list(
        list(x = 0, y = inv_p),
        list(x = p, y = inv_p),
        list(x = 1, y = 1)
      )
    )
  })

  spec$references <- c(
    list(list(
      type = "horizontal",
      value = 1,
      scope = "global",
      label = "Random"
    )),
    perfect_references
  )

  data_lifts <- vapply(spec$data, `[[`, numeric(1), "lift")
  perfect_lifts <- vapply(
    populations,
    function(pop) {
      1 / as.numeric(prevalence[[pop]])
    },
    numeric(1)
  )

  max_lift <- max(c(data_lifts, perfect_lifts, 1), na.rm = TRUE)

  spec$yAxis <- list(
    label = "Lift",
    domain = c(0, max_lift)
  )

  spec
}

#' Build a canonical rtichoke_viz v2 gains specification
#'
#' Translate already-computed gains performance data plus explicit semantic
#' evaluation metadata into the canonical rtichoke_viz v2 contract. Perfect
#' model reference geometry is derived from the same production prevalence
#' values used by the existing gains renderer. This helper is deliberately
#' internal and is not wired into production rendering yet.
#'
#' @inheritParams rtichoke_viz_roc_v2_spec
#'
#' @return A nested list representing a canonical gains v2 specification.
#' @noRd
rtichoke_viz_gains_v2_spec <- function(
  performance_data,
  evaluation_metadata
) {
  spec <- rtichoke_viz_curve_v2_spec(
    performance_data,
    evaluation_metadata,
    type = "gains"
  )

  populations <- unique(vapply(
    spec$evaluations,
    `[[`,
    character(1),
    "population"
  ))
  prevalence <- v2_population_prevalence(
    performance_data,
    evaluation_metadata,
    populations
  )

  perfect_references <- lapply(populations, function(population) {
    population_prevalence <- unname(prevalence[[population]])
    list(
      type = "path",
      scope = "population",
      population = population,
      label = "Perfect Model",
      points = list(
        list(x = 0, y = 0),
        list(x = as.numeric(population_prevalence), y = 1),
        list(x = 1, y = 1)
      )
    )
  })

  spec$references <- c(
    list(list(type = "identity", scope = "global", label = "Random")),
    perfect_references
  )
  spec
}

v2_population_prevalence <- function(
  performance_data,
  evaluation_metadata,
  populations
) {
  prevalence <- get_prevalence_from_performance_data(performance_data)
  model_known <- !is.na(evaluation_metadata$model) &
    nzchar(evaluation_metadata$model)

  if (all(model_known) && "model" %in% names(performance_data)) {
    population_prevalence <- vapply(
      populations,
      function(population) {
        models <- as.character(evaluation_metadata$model[
          evaluation_metadata$population == population
        ])
        values <- unique(unname(prevalence[names(prevalence) %in% models]))
        if (length(values) != 1L) {
          stop(
            "Performance data must have one prevalence per population: ",
            population,
            call. = FALSE
          )
        }
        values[[1]]
      },
      numeric(1)
    )
    return(stats::setNames(population_prevalence, populations))
  }

  if (length(prevalence) == 1L) {
    prevalence <- stats::setNames(prevalence, populations[[1]])
  }

  missing_populations <- setdiff(populations, names(prevalence))
  if (length(missing_populations) > 0L) {
    stop(
      "Performance data is missing prevalence for populations: ",
      paste(missing_populations, collapse = ", "),
      call. = FALSE
    )
  }

  prevalence[populations]
}

gains_v2_population_prevalence <- v2_population_prevalence

rtichoke_viz_curve_v2_spec <- function(
  performance_data,
  evaluation_metadata,
  type = c("roc", "gains", "lift")
) {
  type <- match.arg(type)
  required_columns <- switch(
    type,
    roc = c("probability_threshold", "sensitivity", "specificity"),
    gains = c("probability_threshold", "ppcr", "sensitivity"),
    lift = c("probability_threshold", "ppcr", "lift")
  )
  missing_columns <- setdiff(required_columns, names(performance_data))
  if (length(missing_columns) > 0L) {
    stop(
      toupper(type),
      " performance data is missing columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  required_metadata <- c("model", "population", "evaluation")
  missing_metadata <- setdiff(required_metadata, names(evaluation_metadata))
  if (length(missing_metadata) > 0L) {
    stop(
      toupper(type),
      " evaluation metadata is missing columns: ",
      paste(missing_metadata, collapse = ", "),
      call. = FALSE
    )
  }

  if (nrow(evaluation_metadata) == 0L) {
    stop(
      paste0(
        toupper(type),
        " evaluation metadata must contain at least one evaluation"
      ),
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
      toupper(type),
      " performance rows are missing evaluation metadata: ",
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
    datum <- list(
      seriesId = unname(series_ids[[group]]),
      cutoff = as.numeric(performance_data$probability_threshold[[i]])
    )
    if (type == "roc") {
      datum$sensitivity <- as.numeric(performance_data$sensitivity[[i]])
      datum$specificity <- as.numeric(performance_data$specificity[[i]])
    } else if (type == "gains") {
      datum$sensitivity <- as.numeric(performance_data$sensitivity[[i]])
      datum$ppcr <- as.numeric(performance_data$ppcr[[i]])
    } else if (type == "lift") {
      datum$ppcr <- as.numeric(performance_data$ppcr[[i]])
      datum$lift <- as.numeric(performance_data$lift[[i]])
    }
    datum
  })

  if (type == "roc") {
    return(list(
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
    ))
  }

  if (type == "gains") {
    return(list(
      schemaVersion = "2.0",
      type = "gains",
      evaluations = evaluations,
      series = series,
      data = data,
      x = "ppcr",
      y = "sensitivity",
      xAxis = list(label = "Predicted Positives (Rate)", domain = c(0, 1)),
      yAxis = list(label = "Sensitivity", domain = c(0, 1)),
      references = list()
    ))
  }

  list(
    schemaVersion = "2.0",
    type = "lift",
    evaluations = evaluations,
    series = series,
    data = data,
    x = "ppcr",
    y = "lift",
    xAxis = list(label = "Predicted Positives (Rate)", domain = c(0, 1)),
    yAxis = list(label = "Lift"),
    references = list()
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
