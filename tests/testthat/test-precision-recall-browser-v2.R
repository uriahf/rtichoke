precision_recall_v2_fixture <- function() {
  probs <- list("Model A" = c(0.05, 0.2, 0.7, 0.95))
  reals <- list("Population A" = c(0, 0, 1, 1))
  list(
    probs = probs,
    reals = reals,
    performance_data = prepare_performance_data(probs, reals, by = 0.25),
    metadata = rtichoke:::build_evaluation_metadata(probs, reals)
  )
}

test_that("Precision-Recall v2 is a pure adapter over existing performance rows", {
  dat <- precision_recall_v2_fixture()
  spec <- rtichoke:::rtichoke_viz_precision_recall_v2_spec(
    dat$performance_data,
    dat$metadata
  )
  valid <- is.finite(as.numeric(dat$performance_data$probability_threshold)) &
    is.finite(as.numeric(dat$performance_data$sensitivity)) &
    is.finite(as.numeric(dat$performance_data$PPV))

  expect_identical(spec$schemaVersion, "2.0")
  expect_identical(spec$type, "precision_recall")
  expect_identical(spec$x, "sensitivity")
  expect_identical(spec$y, "ppv")
  expect_identical(
    vapply(spec$data, `[[`, numeric(1), "cutoff"),
    as.numeric(dat$performance_data$probability_threshold[valid])
  )
  expect_identical(
    vapply(spec$data, `[[`, numeric(1), "sensitivity"),
    as.numeric(dat$performance_data$sensitivity[valid])
  )
  expect_identical(
    vapply(spec$data, `[[`, numeric(1), "ppv"),
    as.numeric(dat$performance_data$PPV[valid])
  )
  expect_true(all(vapply(
    spec$data,
    function(x) all(is.finite(c(x$cutoff, x$sensitivity, x$ppv))),
    logical(1)
  )))
  expect_true(all(
    c("seriesId", "cutoff", "sensitivity", "ppv") %in% names(spec$data[[1]])
  ))
})

test_that("Precision-Recall v2 represents one model in one population", {
  dat <- precision_recall_v2_fixture()
  spec <- rtichoke:::rtichoke_viz_precision_recall_v2_spec(
    dat$performance_data,
    dat$metadata
  )

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
  expect_identical(
    spec$references,
    list(list(
      type = "horizontal",
      scope = "population",
      population = "Population A",
      value = 0.5
    ))
  )
})

test_that("Precision-Recall v2 shares one prevalence reference across models", {
  probs <- list(
    "Model A" = c(0.05, 0.2, 0.7, 0.95),
    "Model B" = c(0.1, 0.4, 0.6, 0.9)
  )
  reals <- list("Population A" = c(0, 0, 1, 1))

  spec <- rtichoke:::rtichoke_viz_precision_recall_v2_spec(
    prepare_performance_data(probs, reals, by = 0.25),
    rtichoke:::build_evaluation_metadata(probs, reals)
  )

  expect_identical(
    vapply(spec$evaluations, `[[`, "", "id"),
    c("evaluation-1", "evaluation-2")
  )
  expect_identical(
    vapply(spec$series, `[[`, "", "id"),
    c("series-1", "series-2")
  )
  expect_identical(
    vapply(spec$evaluations, `[[`, "", "population"),
    c("Population A", "Population A")
  )
  expect_length(spec$references, 1)
  expect_identical(spec$references[[1]]$population, "Population A")
  expect_identical(spec$references[[1]]$value, 0.5)
})

test_that("Precision-Recall v2 preserves population-shaped model-unknown semantics", {
  probs <- list(
    "Population A" = c(0.05, 0.2, 0.7, 0.95),
    "Population B" = c(0.1, 0.4, 0.6, 0.9)
  )
  reals <- list(
    "Population A" = c(0, 0, 0, 1),
    "Population B" = c(0, 0, 1, 1)
  )

  spec <- rtichoke:::rtichoke_viz_precision_recall_v2_spec(
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
  expect_identical(
    vapply(spec$references, `[[`, "", "population"),
    c("Population A", "Population B")
  )
  expect_identical(
    vapply(spec$references, `[[`, numeric(1), "value"),
    c(0.25, 0.5)
  )
})

test_that("Precision-Recall v2 keeps equal-prevalence populations as distinct owners", {
  probs <- list(
    "Population A" = c(0.05, 0.2, 0.7, 0.95),
    "Population B" = c(0.1, 0.4, 0.6, 0.9)
  )
  reals <- list(
    "Population A" = c(0, 0, 1, 1),
    "Population B" = c(0, 1, 0, 1)
  )

  spec <- rtichoke:::rtichoke_viz_precision_recall_v2_spec(
    prepare_performance_data(probs, reals, by = 0.25),
    rtichoke:::build_evaluation_metadata(probs, reals)
  )

  expect_length(spec$references, 2)
  expect_identical(
    vapply(spec$references, `[[`, "", "population"),
    c("Population A", "Population B")
  )
  expect_identical(
    vapply(spec$references, `[[`, numeric(1), "value"),
    c(0.5, 0.5)
  )
  expect_false(
    identical(
      spec$references[[1]]$population,
      spec$references[[2]]$population
    )
  )
})

test_that("Precision-Recall v2 IDs are deterministic and label-independent", {
  probs <- list(
    "Population A" = c(0.05, 0.2, 0.7, 0.95),
    "Population B" = c(0.1, 0.4, 0.6, 0.9)
  )
  reals <- list(
    "Population A" = c(0, 0, 1, 1),
    "Population B" = c(0, 1, 0, 1)
  )
  spec <- rtichoke:::rtichoke_viz_precision_recall_v2_spec(
    prepare_performance_data(probs, reals, by = 0.25),
    rtichoke:::build_evaluation_metadata(probs, reals)
  )

  renamed_probs <- stats::setNames(probs, c("Cohort X", "Cohort Y"))
  renamed_reals <- stats::setNames(reals, c("Cohort X", "Cohort Y"))
  renamed_spec <- rtichoke:::rtichoke_viz_precision_recall_v2_spec(
    prepare_performance_data(renamed_probs, renamed_reals, by = 0.25),
    rtichoke:::build_evaluation_metadata(renamed_probs, renamed_reals)
  )

  expect_identical(
    vapply(spec$evaluations, `[[`, "", "id"),
    c("evaluation-1", "evaluation-2")
  )
  expect_identical(
    vapply(spec$series, `[[`, "", "id"),
    c("series-1", "series-2")
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

test_that("Precision-Recall public renderers preserve historical defaults", {
  dat <- precision_recall_v2_fixture()

  plotly_plot <- create_precision_recall_curve(
    dat$probs,
    dat$reals,
    by = 0.25
  )
  expect_s3_class(plotly_plot, "plotly")

  ggplot_plot <- create_precision_recall_curve(
    dat$probs,
    dat$reals,
    by = 0.25,
    interactive = FALSE
  )
  expect_s3_class(ggplot_plot, "ggplot")

  expect_s3_class(
    plot_precision_recall_curve(dat$performance_data, renderer = "plotly"),
    "plotly"
  )
  expect_s3_class(
    plot_precision_recall_curve(dat$performance_data, renderer = "ggplot2"),
    "ggplot"
  )
})

test_that("Precision-Recall browser dispatch uses the canonical shared renderer", {
  dat <- precision_recall_v2_fixture()

  browser <- create_precision_recall_curve(
    dat$probs,
    dat$reals,
    by = 0.25,
    renderer = "browser"
  )
  html <- as.character(browser)

  expect_s3_class(browser, "shiny.tag.list")
  expect_match(html, "renderPrecisionRecallV2", fixed = TRUE)
  expect_match(html, '"schemaVersion":"2.0"', fixed = TRUE)
  expect_match(html, '"type":"precision_recall"', fixed = TRUE)
  expect_match(html, '"x":"sensitivity"', fixed = TRUE)
  expect_match(html, '"y":"ppv"', fixed = TRUE)

  precomputed <- plot_precision_recall_curve(
    dat$performance_data,
    renderer = "browser",
    evaluation_metadata = dat$metadata
  )
  expect_match(
    as.character(precomputed),
    "renderPrecisionRecallV2",
    fixed = TRUE
  )
  expect_error(
    plot_precision_recall_curve(dat$performance_data, renderer = "browser"),
    "explicit evaluation_metadata"
  )
})
