decision_v2_fixture <- function() {
  probs <- list("Model A" = c(0.05, 0.2, 0.7, 0.95))
  reals <- list("Population A" = c(0, 0, 1, 1))
  list(
    probs = probs,
    reals = reals,
    performance_data = prepare_performance_data(probs, reals, by = 0.25),
    metadata = rtichoke:::build_evaluation_metadata(probs, reals)
  )
}

test_that("Decision Curve v2 copies existing thresholds and net benefit", {
  dat <- decision_v2_fixture()
  spec <- rtichoke:::rtichoke_viz_decision_curve_v2_spec(
    dat$performance_data,
    dat$metadata
  )
  valid <- is.finite(as.numeric(dat$performance_data$probability_threshold)) &
    is.finite(as.numeric(dat$performance_data$NB))

  expect_identical(spec$schemaVersion, "2.0")
  expect_identical(spec$type, "decision_curve")
  expect_identical(spec$x, "threshold")
  expect_identical(spec$y, "netBenefit")
  expect_identical(
    vapply(spec$data, `[[`, numeric(1), "threshold"),
    as.numeric(dat$performance_data$probability_threshold[valid])
  )
  expect_identical(
    vapply(spec$data, `[[`, numeric(1), "netBenefit"),
    as.numeric(dat$performance_data$NB[valid])
  )
})

test_that("Decision Curve ids are deterministic and independent", {
  dat <- decision_v2_fixture()
  spec <- rtichoke:::rtichoke_viz_decision_curve_v2_spec(
    dat$performance_data,
    dat$metadata
  )
  expect_identical(
    vapply(spec$evaluations, `[[`, character(1), "id"),
    "evaluation-1"
  )
  expect_identical(
    vapply(spec$series, `[[`, character(1), "id"),
    "series-1"
  )
  expect_identical(spec$series[[1]]$evaluationId, "evaluation-1")
  expect_false(identical(spec$series[[1]]$id, spec$series[[1]]$evaluationId))
})

test_that("two models sharing one population share one Treat All", {
  probs <- list(
    "Model A" = c(0.05, 0.2, 0.7, 0.95),
    "Model B" = c(0.1, 0.3, 0.6, 0.9)
  )
  reals <- list("Population A" = c(0, 0, 1, 1))
  performance_data <- prepare_performance_data(probs, reals, by = 0.25)
  metadata <- rtichoke:::build_evaluation_metadata(probs, reals)
  spec <- rtichoke:::rtichoke_viz_decision_curve_v2_spec(
    performance_data,
    metadata
  )
  benchmarks <- vapply(spec$references, `[[`, character(1), "benchmark")

  expect_identical(
    vapply(spec$evaluations, `[[`, character(1), "id"),
    c("evaluation-1", "evaluation-2")
  )
  expect_identical(
    vapply(spec$series, `[[`, character(1), "id"),
    c("series-1", "series-2")
  )
  expect_equal(sum(benchmarks == "treat_none"), 1)
  expect_equal(sum(benchmarks == "treat_all"), 1)
  expect_identical(spec$references[[2]]$population, "Population A")
})

test_that("equal-prevalence populations remain distinct Treat All owners", {
  probs <- list(
    "Population A" = c(0.05, 0.2, 0.7, 0.95),
    "Population B" = c(0.1, 0.3, 0.6, 0.9)
  )
  reals <- list(
    "Population A" = c(0, 0, 1, 1),
    "Population B" = c(0, 1, 0, 1)
  )
  performance_data <- prepare_performance_data(probs, reals, by = 0.25)
  metadata <- rtichoke:::build_evaluation_metadata(probs, reals)
  spec <- rtichoke:::rtichoke_viz_decision_curve_v2_spec(
    performance_data,
    metadata
  )
  refs <- Filter(
    function(x) identical(x$benchmark, "treat_all"),
    spec$references
  )

  expect_length(refs, 2)
  expect_identical(
    vapply(refs, `[[`, character(1), "population"),
    c("Population A", "Population B")
  )
  expect_identical(refs[[1]]$points, refs[[2]]$points)
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

test_that("different-prevalence populations get different Treat All paths", {
  probs <- list(
    "Population A" = c(0.05, 0.2, 0.7, 0.95),
    "Population B" = c(0.1, 0.3, 0.6, 0.9)
  )
  reals <- list(
    "Population A" = c(0, 0, 1, 1),
    "Population B" = c(0, 0, 0, 1)
  )
  performance_data <- prepare_performance_data(probs, reals, by = 0.25)
  metadata <- rtichoke:::build_evaluation_metadata(probs, reals)
  spec <- rtichoke:::rtichoke_viz_decision_curve_v2_spec(
    performance_data,
    metadata
  )
  refs <- Filter(
    function(x) identical(x$benchmark, "treat_all"),
    spec$references
  )
  expect_false(identical(refs[[1]]$points, refs[[2]]$points))
})

test_that("Treat None and Treat All geometry matches canonical semantics", {
  dat <- decision_v2_fixture()
  spec <- rtichoke:::rtichoke_viz_decision_curve_v2_spec(
    dat$performance_data,
    dat$metadata
  )
  none <- Filter(
    function(x) identical(x$benchmark, "treat_none"),
    spec$references
  )[[1]]
  all <- Filter(
    function(x) identical(x$benchmark, "treat_all"),
    spec$references
  )[[1]]
  point <- Filter(function(x) identical(x$x, 0.25), all$points)[[1]]

  expect_identical(none$scope, "global")
  expect_identical(none$value, 0)
  expect_identical(all$scope, "population")
  expect_equal(point$y, 0.5 - 0.5 * 0.25 / 0.75)
})

test_that("Decision Curve browser dispatch uses shared v0.6.0 renderer", {
  dat <- decision_v2_fixture()
  widget <- plot_decision_curve(
    dat$performance_data,
    renderer = "browser",
    evaluation_metadata = dat$metadata
  )
  html <- paste(as.character(widget), collapse = "\n")
  expect_match(html, "renderDecisionCurveV2", fixed = TRUE)
  expect_match(html, "rtichoke-viz-0.6.0/rtichoke-viz.js", fixed = TRUE)
  expect_match(html, '"type":"decision_curve"', fixed = TRUE)
})

test_that("Decision Curve browser mode is opt-in and Plotly stays default", {
  dat <- decision_v2_fixture()
  expect_s3_class(
    create_decision_curve(dat$probs, dat$reals, by = 0.25),
    "plotly"
  )
  expect_s3_class(
    create_decision_curve(
      dat$probs,
      dat$reals,
      by = 0.25,
      renderer = "plotly"
    ),
    "plotly"
  )
  expect_error(
    plot_decision_curve(
      dat$performance_data,
      type = "interventions avoided",
      renderer = "browser",
      evaluation_metadata = dat$metadata
    ),
    "only for conventional static Decision Curves"
  )
})
