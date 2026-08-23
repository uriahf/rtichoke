test_that("ROC v2 represents one model in one population", {
  probs <- list("Model A" = c(0.05, 0.2, 0.7, 0.95))
  reals <- list("Population A" = c(0, 0, 1, 1))

  spec <- rtichoke:::rtichoke_viz_roc_v2_spec(
    prepare_performance_data(probs, reals, by = 0.25),
    rtichoke:::build_evaluation_metadata(probs, reals)
  )

  expect_identical(spec$schemaVersion, "2.0")
  expect_identical(spec$type, "roc")
  expect_identical(
    spec$evaluations,
    list(list(
      id = "evaluation-1",
      population = "Population A",
      model = "Model A"
    ))
  )
  expect_identical(
    spec$series,
    list(list(
      id = "series-1",
      evaluationId = "evaluation-1",
      display = list(
        label = "Model A",
        group = "Model A",
        role = "model"
      )
    ))
  )
  expect_identical(unique(vapply(spec$data, `[[`, "", "seriesId")), "series-1")
  expect_identical(
    spec$references,
    list(list(type = "identity", scope = "global"))
  )
})

test_that("ROC v2 keeps multiple models in one shared population distinct", {
  probs <- list(
    "Model A" = c(0.05, 0.2, 0.7, 0.95),
    "Model B" = c(0.1, 0.4, 0.6, 0.9)
  )
  reals <- list("Population A" = c(0, 0, 1, 1))

  spec <- rtichoke:::rtichoke_viz_roc_v2_spec(
    prepare_performance_data(probs, reals, by = 0.25),
    rtichoke:::build_evaluation_metadata(probs, reals)
  )

  expect_identical(
    vapply(spec$evaluations, `[[`, "", "id"),
    c("evaluation-1", "evaluation-2")
  )
  expect_identical(
    vapply(spec$evaluations, `[[`, "", "population"),
    c("Population A", "Population A")
  )
  expect_identical(
    vapply(spec$evaluations, `[[`, "", "model"),
    c("Model A", "Model B")
  )
  expect_identical(
    vapply(spec$series, `[[`, "", "id"),
    c("series-1", "series-2")
  )
  expect_setequal(
    vapply(spec$data, `[[`, "", "seriesId"),
    c("series-1", "series-2")
  )
})

test_that("ROC v2 preserves unknown model identity for keyed populations", {
  probs <- list(
    "Population A" = c(0.05, 0.2, 0.7, 0.95),
    "Population B" = c(0.1, 0.4, 0.6, 0.9)
  )
  reals <- list(
    "Population A" = c(0, 0, 1, 1),
    "Population B" = c(0, 1, 0, 1)
  )

  spec <- rtichoke:::rtichoke_viz_roc_v2_spec(
    prepare_performance_data(probs, reals, by = 0.25),
    rtichoke:::build_evaluation_metadata(probs, reals)
  )

  expect_identical(
    spec$evaluations,
    list(
      list(id = "evaluation-1", population = "Population A"),
      list(id = "evaluation-2", population = "Population B")
    )
  )
  expect_false(any(vapply(
    spec$evaluations,
    function(x) "model" %in% names(x),
    logical(1)
  )))
  expect_identical(
    lapply(spec$series, `[[`, "display"),
    list(
      list(
        label = "Population A",
        group = "Population A",
        role = "population"
      ),
      list(
        label = "Population B",
        group = "Population B",
        role = "population"
      )
    )
  )
  expect_setequal(
    vapply(spec$data, `[[`, "", "seriesId"),
    c("series-1", "series-2")
  )
})

test_that("ROC v2 IDs do not encode compatibility group labels", {
  probs <- list(
    "Population A" = c(0.05, 0.2, 0.7, 0.95),
    "Population B" = c(0.1, 0.4, 0.6, 0.9)
  )
  reals <- list(
    "Population A" = c(0, 0, 1, 1),
    "Population B" = c(0, 1, 0, 1)
  )
  spec <- rtichoke:::rtichoke_viz_roc_v2_spec(
    prepare_performance_data(probs, reals, by = 0.25),
    rtichoke:::build_evaluation_metadata(probs, reals)
  )

  renamed_probs <- stats::setNames(probs, c("Cohort X", "Cohort Y"))
  renamed_reals <- stats::setNames(reals, c("Cohort X", "Cohort Y"))
  renamed_spec <- rtichoke:::rtichoke_viz_roc_v2_spec(
    prepare_performance_data(renamed_probs, renamed_reals, by = 0.25),
    rtichoke:::build_evaluation_metadata(renamed_probs, renamed_reals)
  )

  expect_identical(
    vapply(spec$evaluations, `[[`, "", "id"),
    vapply(renamed_spec$evaluations, `[[`, "", "id")
  )
  expect_identical(
    vapply(spec$series, `[[`, "", "id"),
    vapply(renamed_spec$series, `[[`, "", "id")
  )
})

test_that("gains v2 uses production prevalence for perfect path", {
  probs <- list("Model A" = c(0.05, 0.2, 0.7, 0.95))
  reals <- list("Population A" = c(0, 0, 0, 1))
  performance_data <- prepare_performance_data(probs, reals, by = 0.25)

  spec <- rtichoke:::rtichoke_viz_gains_v2_spec(
    performance_data,
    rtichoke:::build_evaluation_metadata(probs, reals)
  )

  prevalence <- unname(
    rtichoke:::get_prevalence_from_performance_data(performance_data)
  )
  expect_identical(spec$type, "gains")
  expect_identical(spec$x, "ppcr")
  expect_identical(spec$y, "sensitivity")
  expect_identical(
    spec$references[[1]],
    list(type = "identity", scope = "global", label = "Random")
  )
  expect_identical(spec$references[[2]]$scope, "population")
  expect_identical(spec$references[[2]]$population, "Population A")
  expect_identical(
    spec$references[[2]]$points,
    list(
      list(x = 0, y = 0),
      list(x = prevalence, y = 1),
      list(x = 1, y = 1)
    )
  )
})

test_that("gains v2 shares one perfect path across models in one population", {
  probs <- list(
    "Model A" = c(0.05, 0.2, 0.7, 0.95),
    "Model B" = c(0.1, 0.4, 0.6, 0.9)
  )
  reals <- list("Population A" = c(0, 0, 1, 1))

  spec <- rtichoke:::rtichoke_viz_gains_v2_spec(
    prepare_performance_data(probs, reals, by = 0.25),
    rtichoke:::build_evaluation_metadata(probs, reals)
  )

  expect_length(spec$series, 2)
  expect_length(spec$references, 2)
  expect_identical(spec$references[[2]]$population, "Population A")
})

test_that("gains v2 keeps equal-prevalence populations as distinct owners", {
  probs <- list(
    "Population A" = c(0.05, 0.2, 0.7, 0.95),
    "Population B" = c(0.1, 0.4, 0.6, 0.9)
  )
  reals <- list(
    "Population A" = c(0, 0, 1, 1),
    "Population B" = c(0, 1, 0, 1)
  )

  spec <- rtichoke:::rtichoke_viz_gains_v2_spec(
    prepare_performance_data(probs, reals, by = 0.25),
    rtichoke:::build_evaluation_metadata(probs, reals)
  )

  perfect <- spec$references[-1]
  expect_length(perfect, 2)
  expect_identical(
    vapply(perfect, `[[`, "", "population"),
    c("Population A", "Population B")
  )
  expect_identical(perfect[[1]]$points, perfect[[2]]$points)
  expect_false(any(vapply(
    spec$evaluations,
    function(x) "model" %in% names(x),
    logical(1)
  )))
})

test_that("renderer selector preserves the legacy default", {
  expect_identical(
    rtichoke:::rtichoke_viz_renderer("default", TRUE),
    "plotly"
  )
  expect_identical(
    rtichoke:::rtichoke_viz_renderer("default", FALSE),
    "ggplot2"
  )
  expect_identical(
    rtichoke:::rtichoke_viz_renderer("browser", TRUE),
    "browser"
  )
  expect_error(rtichoke:::rtichoke_viz_renderer("unknown", TRUE), "arg")
})

test_that("ROC renderer choices dispatch without changing performance data", {
  probs <- list("Model A" = c(0.05, 0.2, 0.7, 0.95))
  reals <- list("Population A" = c(0, 0, 1, 1))
  performance_data <- prepare_performance_data(probs, reals, by = 0.25)
  metadata <- rtichoke:::build_evaluation_metadata(probs, reals)

  expect_s3_class(
    plot_roc_curve(performance_data, renderer = "ggplot2"),
    "ggplot"
  )
  expect_s3_class(
    plot_roc_curve(performance_data, renderer = "plotly"),
    "plotly"
  )
  browser <- plot_roc_curve(
    performance_data,
    renderer = "browser",
    evaluation_metadata = metadata
  )
  expect_s3_class(browser, "shiny.tag.list")
  expect_match(as.character(browser), "renderRocV2", fixed = TRUE)
  expect_error(
    plot_roc_curve(performance_data, renderer = "browser"),
    "explicit evaluation_metadata"
  )
})

test_that("gains browser renderer consumes the canonical v2 builder", {
  probs <- list("Model A" = c(0.05, 0.2, 0.7, 0.95))
  reals <- list("Population A" = c(0, 0, 1, 1))

  browser <- create_gains_curve(probs, reals, by = 0.25, renderer = "browser")

  expect_s3_class(browser, "shiny.tag.list")
  expect_match(as.character(browser), "renderGainsV2", fixed = TRUE)
  expect_match(as.character(browser), '"schemaVersion":"2.0"', fixed = TRUE)
})

test_that("Lift v2 represents one model in one population", {
  probs <- list("Model A" = c(0.05, 0.2, 0.7, 0.95))
  reals <- list("Population A" = c(0, 0, 0, 1))
  perf_dat <- prepare_performance_data(probs, reals, by = 0.25)
  meta <- rtichoke:::build_evaluation_metadata(probs, reals)

  spec <- rtichoke:::rtichoke_viz_lift_v2_spec(perf_dat, meta)

  expect_identical(spec$schemaVersion, "2.0")
  expect_identical(spec$type, "lift")
  expect_identical(spec$x, "ppcr")
  expect_identical(spec$y, "lift")

  expect_identical(
    spec$evaluations,
    list(list(
      id = "evaluation-1",
      population = "Population A",
      model = "Model A"
    ))
  )

  expect_identical(
    spec$series,
    list(list(
      id = "series-1",
      evaluationId = "evaluation-1",
      display = list(
        label = "Model A",
        group = "Model A",
        role = "model"
      )
    ))
  )

  expect_identical(unique(vapply(spec$data, `[[`, "", "seriesId")), "series-1")
  expect_true(all(c("seriesId", "cutoff", "ppcr", "lift") %in% names(spec$data[[1]])))

  expect_identical(
    spec$references[[1]],
    list(
      type = "horizontal",
      value = 1,
      scope = "global",
      label = "Random"
    )
  )

  prevalence <- unname(rtichoke:::get_prevalence_from_performance_data(perf_dat))
  expect_identical(spec$references[[2]]$scope, "population")
  expect_identical(spec$references[[2]]$population, "Population A")
  expect_identical(spec$references[[2]]$label, "Perfect Model")
  expect_identical(
    spec$references[[2]]$points,
    list(
      list(x = 0, y = 1 / prevalence),
      list(x = prevalence, y = 1 / prevalence),
      list(x = 1, y = 1)
    )
  )

  expect_identical(spec$xAxis, list(label = "Predicted Positives (Rate)", domain = c(0, 1)))
  expect_identical(spec$yAxis$label, "Lift")
  expect_equal(spec$yAxis$domain[[1]], 0)
  expect_true(spec$yAxis$domain[[2]] >= 1 / prevalence)
})

test_that("Lift v2 shares one perfect path across two models in one shared population", {
  probs <- list(
    "Model A" = c(0.05, 0.2, 0.7, 0.95),
    "Model B" = c(0.1, 0.4, 0.6, 0.9)
  )
  reals <- list("Population A" = c(0, 0, 1, 1))

  spec <- rtichoke:::rtichoke_viz_lift_v2_spec(
    prepare_performance_data(probs, reals, by = 0.25),
    rtichoke:::build_evaluation_metadata(probs, reals)
  )

  expect_length(spec$evaluations, 2)
  expect_length(spec$series, 2)
  expect_length(spec$references, 2)
  expect_identical(spec$references[[1]]$label, "Random")
  expect_identical(spec$references[[2]]$label, "Perfect Model")
  expect_identical(spec$references[[2]]$population, "Population A")
  expect_setequal(
    vapply(spec$data, `[[`, "", "seriesId"),
    c("series-1", "series-2")
  )
})

test_that("Lift v2 keeps distinct population-owned references for multiple populations", {
  probs <- list(
    "Population A" = c(0.05, 0.2, 0.7, 0.95),
    "Population B" = c(0.1, 0.4, 0.6, 0.9)
  )
  reals <- list(
    "Population A" = c(0, 0, 0, 1),
    "Population B" = c(0, 0, 1, 1)
  )

  spec <- rtichoke:::rtichoke_viz_lift_v2_spec(
    prepare_performance_data(probs, reals, by = 0.25),
    rtichoke:::build_evaluation_metadata(probs, reals)
  )

  perfect_refs <- spec$references[-1]
  expect_length(perfect_refs, 2)
  expect_identical(
    vapply(perfect_refs, `[[`, "", "population"),
    c("Population A", "Population B")
  )
  expect_false(identical(perfect_refs[[1]]$points, perfect_refs[[2]]$points))
})

test_that("Lift v2 keeps equal-prevalence distinct populations as separate reference owners", {
  probs <- list(
    "Population A" = c(0.05, 0.2, 0.7, 0.95),
    "Population B" = c(0.1, 0.4, 0.6, 0.9)
  )
  reals <- list(
    "Population A" = c(0, 0, 1, 1),
    "Population B" = c(0, 1, 0, 1)
  )

  spec <- rtichoke:::rtichoke_viz_lift_v2_spec(
    prepare_performance_data(probs, reals, by = 0.25),
    rtichoke:::build_evaluation_metadata(probs, reals)
  )

  perfect_refs <- spec$references[-1]
  expect_length(perfect_refs, 2)
  expect_identical(
    vapply(perfect_refs, `[[`, "", "population"),
    c("Population A", "Population B")
  )
  expect_identical(perfect_refs[[1]]$points, perfect_refs[[2]]$points)
})

test_that("Lift v2 generates deterministic evaluation and series IDs independent of group names", {
  probs <- list(
    "Population A" = c(0.05, 0.2, 0.7, 0.95),
    "Population B" = c(0.1, 0.4, 0.6, 0.9)
  )
  reals <- list(
    "Population A" = c(0, 0, 1, 1),
    "Population B" = c(0, 1, 0, 1)
  )
  spec1 <- rtichoke:::rtichoke_viz_lift_v2_spec(
    prepare_performance_data(probs, reals, by = 0.25),
    rtichoke:::build_evaluation_metadata(probs, reals)
  )

  renamed_probs <- stats::setNames(probs, c("Group X", "Group Y"))
  renamed_reals <- stats::setNames(reals, c("Group X", "Group Y"))
  spec2 <- rtichoke:::rtichoke_viz_lift_v2_spec(
    prepare_performance_data(renamed_probs, renamed_reals, by = 0.25),
    rtichoke:::build_evaluation_metadata(renamed_probs, renamed_reals)
  )

  expect_identical(
    vapply(spec1$evaluations, `[[`, "", "id"),
    vapply(spec2$evaluations, `[[`, "", "id")
  )
  expect_identical(
    vapply(spec1$series, `[[`, "", "id"),
    vapply(spec2$series, `[[`, "", "id")
  )
})

test_that("Lift v2 preserves unknown model semantics when model is unknown", {
  probs <- list(
    "Population A" = c(0.05, 0.2, 0.7, 0.95),
    "Population B" = c(0.1, 0.4, 0.6, 0.9)
  )
  reals <- list(
    "Population A" = c(0, 0, 1, 1),
    "Population B" = c(0, 1, 0, 1)
  )

  spec <- rtichoke:::rtichoke_viz_lift_v2_spec(
    prepare_performance_data(probs, reals, by = 0.25),
    rtichoke:::build_evaluation_metadata(probs, reals)
  )

  expect_false("model" %in% names(spec$evaluations[[1]]))
  expect_identical(spec$series[[1]]$display$role, "population")
  expect_identical(spec$series[[1]]$display$label, "Population A")
})

test_that("Lift default public Plotly behavior remains unchanged and browser Lift fails clearly", {
  probs <- list("Model A" = c(0.05, 0.2, 0.7, 0.95))
  reals <- list("Population A" = c(0, 0, 1, 1))

  plotly_plot <- create_lift_curve(probs, reals, by = 0.25)
  expect_s3_class(plotly_plot, "plotly")

  ggplot_plot <- create_lift_curve(probs, reals, by = 0.25, interactive = FALSE)
  expect_s3_class(ggplot_plot, "ggplot")

  expect_error(
    create_lift_curve(probs, reals, by = 0.25, renderer = "browser"),
    "Browser rendering is not available for chart type: lift"
  )
})
