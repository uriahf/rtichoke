# Build one canonical calibration component per positive report horizon.
prepare_time_report_calibration <- function(probs, reals, times, horizons) {
  horizons <- sort(unique(horizons[horizons > 0]))
  if (!is.numeric(reals) || !is.numeric(times) || !length(reals)) {
    stop(
      "Calibration requires numeric outcomes and times for one population.",
      call. = FALSE
    )
  }
  if (is.matrix(probs)) {
    probs <- list(Model = probs)
  }
  models <- names(probs)
  if (
    !is.list(probs) ||
      !length(probs) ||
      is.null(models) ||
      anyNA(models) ||
      any(!nzchar(trimws(models))) ||
      anyDuplicated(models)
  ) {
    stop(
      "`probs` must be a probability matrix or a list with unique, non-empty model names.",
      call. = FALSE
    )
  }
  probs <- lapply(
    probs,
    normalize_time_calibration_probs,
    reals = reals,
    horizons = horizons
  )
  model_ids <- if (length(models) == 1L) {
    "calibration-model"
  } else {
    paste0("calibration-model-", seq_along(models))
  }
  evaluation_ids <- if (length(models) == 1L) {
    "calibration-evaluation"
  } else {
    paste0("calibration-evaluation-", seq_along(models))
  }
  evaluations <- lapply(seq_along(models), function(index) {
    list(
      id = evaluation_ids[[index]],
      model = models[[index]],
      population = "evaluation"
    )
  })
  series <- lapply(seq_along(models), function(index) {
    model <- models[[index]]
    list(
      id = model_ids[[index]],
      evaluationId = evaluation_ids[[index]],
      display = list(label = model, group = model, role = "model")
    )
  })

  cohort <- data.frame(id = seq_along(reals), reals = reals, times = times)
  calibration <- lapply(seq_along(horizons), function(i) {
    horizon <- horizons[[i]]
    model_calibration <- lapply(seq_along(models), function(index) {
      scores <- probs[[index]][, i]
      model <- models[[index]]
      series_id <- model_ids[[index]]
      groups <- split(
        cohort,
        assign_probability_quantile_strata(scores, by = 0.2),
        drop = TRUE
      )
      rows <- prepare_terminal_outcome_distributions(groups, horizon)
      rows <- Filter(
        function(row) {
          row$censoring == "adjusted" && row$competing == "adjusted_as_negative"
        },
        rows
      )

      points <- lapply(rows, function(row) {
        group <- groups[[row$population]]
        list(
          seriesId = series_id,
          method = "discrete",
          predicted = mean(scores[group$id]),
          observed = .canonical_probability(row$probabilities$primary),
          total = nrow(group)
        )
      })
      support_notes <- lapply(seq_along(rows), function(j) {
        row <- rows[[j]]
        if (row$status == "beyond_support_plateau") {
          message <- sprintf(
            paste(
              "Horizon %s, risk group %d (mean predicted risk %s%%).",
              "Follow-up ends at time %s. Last estimate carried forward."
            ),
            format(horizon, digits = 15, trim = TRUE),
            j,
            format(100 * points[[j]]$predicted, digits = 4, trim = TRUE),
            format(
              max(groups[[row$population]]$times),
              digits = 15,
              trim = TRUE
            )
          )
          if (length(models) > 1L) {
            message <- paste(model, message, sep = ": ")
          }
          htmltools::tags$li(message)
        }
      })
      list(
        points = points,
        support_notes = Filter(Negate(is.null), support_notes)
      )
    })
    component <- list(
      type = "component",
      id = paste0("calibration-horizon-", i),
      title = paste("Horizon", format(horizon, digits = 15, trim = TRUE)),
      spec = list(
        schemaVersion = "2.0",
        type = "calibration",
        heuristicPolicy = "independent",
        evaluations = evaluations,
        series = series,
        data = unlist(
          lapply(model_calibration, `[[`, "points"),
          recursive = FALSE
        ),
        x = "predicted",
        y = "observed",
        xAxis = list(
          label = "Mean predicted target-event risk",
          domain = c(0, 1)
        ),
        yAxis = list(
          label = "Estimated target-event risk (Aalen-Johansen)",
          domain = c(0, 1)
        ),
        references = list(list(type = "identity", scope = "global"))
      )
    )
    list(
      component = component,
      support_notes = unlist(
        lapply(model_calibration, `[[`, "support_notes"),
        recursive = FALSE
      )
    )
  })
  components <- lapply(calibration, `[[`, "component")
  support_notes <- unlist(
    lapply(calibration, `[[`, "support_notes"),
    recursive = FALSE
  )

  size_rows <- lapply(components, function(component) {
    lapply(seq_along(models), function(index) {
      points <- Filter(
        function(point) point$seriesId == model_ids[[index]],
        component$spec$data
      )
      counts <- vapply(points, function(point) point$total, 0L)
      label <- if (length(models) > 1L) {
        paste(component$title, models[[index]], sep = " / ")
      } else {
        component$title
      }
      htmltools::tags$li(label, ": ", paste(counts, collapse = ", "))
    })
  })
  note <- htmltools::tags$details(
    class = "rtichoke-cheat-sheet",
    htmltools::tags$summary("Calibration method and group sizes"),
    htmltools::tags$p(
      "Observed risks use Aalen-Johansen estimates with competing events.",
      "Estimates assume independent censoring within each risk group.",
      "Choose the horizon in Calibration. Outcome Distribution controls affect that chart only."
    ),
    htmltools::tags$p(
      "Each model defines its own risk groups. Group sizes are listed from lowest to highest predicted risk. Equal scores stay together."
    ),
    htmltools::tags$ul(size_rows)
  )

  if (length(support_notes)) {
    note <- htmltools::tagList(
      note,
      htmltools::tags$aside(
        class = "rtichoke-cheat-sheet",
        htmltools::tags$strong("Calibration follow-up limits"),
        htmltools::tags$ul(support_notes)
      )
    )
  }

  list(
    section = list(
      id = "calibration",
      title = "Calibration",
      items = components
    ),
    note = as.character(note)
  )
}

normalize_time_calibration_probs <- function(probs, reals, horizons) {
  if (
    !is.matrix(probs) ||
      !is.numeric(probs) ||
      nrow(probs) != length(reals) ||
      length(horizons) == 0L ||
      ncol(probs) != length(horizons) ||
      any(!is.finite(probs)) ||
      any(probs < 0 | probs > 1)
  ) {
    stop(
      "`probs` must be a probability matrix for one population, with one row per subject and one column per distinct positive horizon.",
      call. = FALSE
    )
  }

  if (!is.null(colnames(probs))) {
    prediction_horizons <- suppressWarnings(as.numeric(colnames(probs)))
    column_order <- match(horizons, prediction_horizons)
    if (anyNA(column_order)) {
      stop(
        "Named `probs` columns must match each distinct positive horizon exactly once.",
        call. = FALSE
      )
    }
    probs <- probs[, column_order, drop = FALSE]
  }

  probs
}
