# Build one canonical calibration component per positive report horizon.
prepare_time_report_calibration <- function(probs, reals, times, horizons) {
  horizons <- sort(unique(horizons[horizons > 0]))
  if (
    !is.numeric(reals) ||
      !is.numeric(times) ||
      length(reals) == 0L ||
      !is.matrix(probs) ||
      !is.numeric(probs) ||
      nrow(probs) != length(reals) ||
      length(horizons) == 0L ||
      ncol(probs) != length(horizons) ||
      any(!is.finite(probs)) ||
      any(probs < 0 | probs > 1)
  ) {
    stop(
      "`probs` must be a probability matrix for one population, with one row per subject and one column per sorted, distinct positive horizon.",
      call. = FALSE
    )
  }

  cohort <- data.frame(id = seq_along(reals), reals = reals, times = times)
  components <- lapply(seq_along(horizons), function(i) {
    horizon <- horizons[[i]]
    scores <- probs[, i]
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
        seriesId = "calibration-model",
        method = "discrete",
        predicted = mean(scores[group$id]),
        observed = .canonical_probability(row$probabilities$primary),
        total = nrow(group)
      )
    })
    list(
      type = "component",
      id = paste0("calibration-horizon-", i),
      title = paste("Horizon", format(horizon, digits = 15, trim = TRUE)),
      spec = list(
        schemaVersion = "2.0",
        type = "calibration",
        evaluations = list(list(
          id = "calibration-evaluation",
          model = "Model",
          population = "evaluation"
        )),
        series = list(list(
          id = "calibration-model",
          evaluationId = "calibration-evaluation",
          display = list(label = "Model", group = "Model", role = "model")
        )),
        data = points,
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
  })

  size_rows <- lapply(components, function(component) {
    counts <- vapply(component$spec$data, function(point) point$total, 0L)
    htmltools::tags$li(component$title, ": ", paste(counts, collapse = ", "))
  })
  note <- htmltools::tags$details(
    class = "rtichoke-cheat-sheet",
    htmltools::tags$summary("Calibration method and group sizes"),
    htmltools::tags$p(
      "Observed risks use Aalen-Johansen estimates with competing events.",
      "Estimates assume independent censoring within each risk group.",
      "Beyond observed follow-up, the estimate stays at its last value.",
      "Choose the horizon in Calibration. Outcome Distribution controls affect that chart only."
    ),
    htmltools::tags$p(
      "Group sizes are listed from lowest to highest predicted risk. Equal scores stay together."
    ),
    htmltools::tags$ul(size_rows)
  )

  list(
    section = list(
      id = "calibration",
      title = "Calibration",
      items = components
    ),
    note = as.character(note)
  )
}
