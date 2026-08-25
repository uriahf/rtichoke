# Static standalone Interventions Avoided consumer-adoption coverage.
ia_v2_fixture <- function() {
  probs <- list("Model A" = c(0.05, 0.2, 0.7, 0.95))
  reals <- list("Population A" = c(0, 0, 1, 1))
  performance_data <- prepare_performance_data(probs, reals, by = 0.25)
  list(
    probs = probs,
    reals = reals,
    performance_data = performance_data,
    ia_performance_data = rtichoke:::add_static_interventions_avoided_metric(
      performance_data
    ),
    metadata = rtichoke:::build_evaluation_metadata(probs, reals)
  )
}

ia_v2_spec <- function(performance_data, metadata) {
  rtichoke:::rtichoke_viz_interventions_avoided_v2_spec(
    performance_data,
    metadata
  )
}

test_that("Interventions Avoided v2 passes through existing thresholds and IA", {
  dat <- ia_v2_fixture()
  dat$ia_performance_data$NB_interventions_avoided[
    dat$ia_performance_data$probability_threshold == 0.25
  ] <- 37.25
  spec <- ia_v2_spec(dat$ia_performance_data, dat$metadata)
  thresholds <- vapply(spec$data, `[[`, numeric(1), "threshold")
  ia <- vapply(spec$data, `[[`, numeric(1), "interventionsAvoided")

  expect_identical(spec$schemaVersion, "2.0")
  expect_identical(spec$type, "interventions_avoided")
  expect_identical(spec$x, "threshold")
  expect_identical(spec$y, "interventionsAvoided")
  expect_equal(ia[thresholds == 0.25], 37.25)
})

test_that("Interventions Avoided production metric keeps established formula", {
  dat <- ia_v2_fixture()
  perf <- dat$ia_performance_data
  valid <- is.finite(perf$probability_threshold) &
    is.finite(perf$NB_interventions_avoided) &
    perf$probability_threshold > 0
  expected <- with(
    perf[valid, ],
    100 *
      (TN / N - FN / N * (1 - probability_threshold) / probability_threshold)
  )
  expect_equal(perf$NB_interventions_avoided[valid], expected)
})

test_that("Interventions Avoided ids are deterministic and independent", {
  dat <- ia_v2_fixture()
  spec <- ia_v2_spec(dat$ia_performance_data, dat$metadata)
  expect_identical(
    vapply(spec$evaluations, `[[`, character(1), "id"),
    "evaluation-1"
  )
  expect_identical(vapply(spec$series, `[[`, character(1), "id"), "series-1")
  expect_identical(spec$series[[1]]$evaluationId, "evaluation-1")
  expect_false(identical(spec$series[[1]]$id, spec$series[[1]]$evaluationId))
})

test_that("two models sharing one population share one Treat None", {
  probs <- list(
    "Model A" = c(0.05, 0.2, 0.7, 0.95),
    "Model B" = c(0.1, 0.3, 0.6, 0.9)
  )
  reals <- list("Population A" = c(0, 0, 1, 1))
  perf <- prepare_performance_data(probs, reals, by = 0.25) |>
    rtichoke:::add_static_interventions_avoided_metric()
  spec <- ia_v2_spec(perf, rtichoke:::build_evaluation_metadata(probs, reals))
  benchmarks <- vapply(spec$references, `[[`, character(1), "benchmark")

  expect_length(spec$evaluations, 2)
  expect_length(spec$series, 2)
  expect_equal(sum(benchmarks == "treat_all"), 1)
  expect_equal(sum(benchmarks == "treat_none"), 1)
  expect_identical(spec$references[[2]]$population, "Population A")
})

test_that("distinct populations keep population-owned Treat None references", {
  probs <- list(
    "Population A" = c(0.05, 0.2, 0.7, 0.95),
    "Population B" = c(0.1, 0.3, 0.6, 0.9)
  )
  reals <- list(
    "Population A" = c(0, 0, 1, 1),
    "Population B" = c(0, 0, 0, 1)
  )
  perf <- prepare_performance_data(probs, reals, by = 0.25) |>
    rtichoke:::add_static_interventions_avoided_metric()
  spec <- ia_v2_spec(perf, rtichoke:::build_evaluation_metadata(probs, reals))
  refs <- Filter(
    function(x) identical(x$benchmark, "treat_none"),
    spec$references
  )

  expect_length(refs, 2)
  expect_identical(
    vapply(refs, `[[`, character(1), "population"),
    c("Population A", "Population B")
  )
  expect_false(identical(refs[[1]]$points, refs[[2]]$points))
})

test_that("equal-prevalence populations remain distinct Treat None owners", {
  probs <- list(
    "Population A" = c(0.05, 0.2, 0.7, 0.95),
    "Population B" = c(0.1, 0.3, 0.6, 0.9)
  )
  reals <- list(
    "Population A" = c(0, 0, 1, 1),
    "Population B" = c(0, 1, 0, 1)
  )
  perf <- prepare_performance_data(probs, reals, by = 0.25) |>
    rtichoke:::add_static_interventions_avoided_metric()
  spec <- ia_v2_spec(perf, rtichoke:::build_evaluation_metadata(probs, reals))
  refs <- Filter(
    function(x) identical(x$benchmark, "treat_none"),
    spec$references
  )

  expect_length(refs, 2)
  expect_identical(refs[[1]]$points, refs[[2]]$points)
  expect_false(identical(refs[[1]]$population, refs[[2]]$population))
  expect_true(all(vapply(
    spec$evaluations,
    function(x) is.null(x$model),
    logical(1)
  )))
  expect_identical(
    vapply(spec$series, function(x) x$display$role, character(1)),
    c("population", "population")
  )
})

test_that("Treat All and Treat None geometry matches canonical IA semantics", {
  dat <- ia_v2_fixture()
  spec <- ia_v2_spec(dat$ia_performance_data, dat$metadata)
  all_ref <- Filter(
    function(x) identical(x$benchmark, "treat_all"),
    spec$references
  )[[1]]
  none_ref <- Filter(
    function(x) identical(x$benchmark, "treat_none"),
    spec$references
  )[[1]]
  point_25 <- Filter(function(x) identical(x$x, 0.25), none_ref$points)[[1]]
  point_50 <- Filter(function(x) identical(x$x, 0.5), none_ref$points)[[1]]

  expect_identical(all_ref$type, "horizontal")
  expect_identical(all_ref$scope, "global")
  expect_identical(all_ref$value, 0)
  expect_identical(none_ref$scope, "population")
  expect_equal(
    point_25$y,
    100 * (1 - 0.5 - 0.5 * (1 - 0.25) / 0.25)
  )
  expect_equal(
    point_50$y,
    100 * (1 - 0.5 - 0.5 * (1 - 0.5) / 0.5)
  )
})

test_that("static cutoff equality remains predicted negative", {
  perf <- prepare_performance_data(
    probs = list(c(0.5, 0.9)),
    reals = list(c(1, 1)),
    by = 0.5
  )
  row <- perf[perf$probability_threshold == 0.5, , drop = FALSE]
  expect_equal(row$TP, 1)
  expect_equal(row$FN, 1)
})

test_that("Interventions Avoided browser dispatch uses shared v0.7.0 renderer", {
  dat <- ia_v2_fixture()
  widget <- plot_decision_curve(
    dat$performance_data,
    type = "interventions avoided",
    renderer = "browser",
    evaluation_metadata = dat$metadata
  )
  html <- paste(as.character(widget), collapse = "\n")
  expect_match(html, "renderInterventionsAvoidedV2", fixed = TRUE)
  expect_match(html, "rtichoke-viz-0.7.0/rtichoke-viz.js", fixed = TRUE)
  expect_match(html, '"type":"interventions_avoided"', fixed = TRUE)
})

test_that("Interventions Avoided browser mode is opt-in and Plotly stays default", {
  dat <- ia_v2_fixture()
  expect_s3_class(
    create_decision_curve(
      dat$probs,
      dat$reals,
      by = 0.25,
      type = "interventions avoided"
    ),
    "plotly"
  )
  expect_s3_class(
    create_decision_curve(
      dat$probs,
      dat$reals,
      by = 0.25,
      type = "interventions avoided",
      renderer = "plotly"
    ),
    "plotly"
  )
})
