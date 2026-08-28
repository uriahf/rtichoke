test_that("SummaryMetrics validation accepts summary_metrics v1.0 and rejects invalid versions", {
  prevalence_spec <- list(
    schemaVersion = "1.0",
    type = "summary_metrics",
    evaluations = list(),
    populations = list(list(id = "population-1", label = "Population A")),
    metrics = list(list(
      metric = "prevalence",
      owner = list(type = "population", populationId = "population-1"),
      estimate = 0.25
    ))
  )

  # Accepted in v1.0 report spec
  report <- rtichoke:::rtichoke_viz_report_spec(prevalence_spec)
  expect_identical(report$components[[1]]$spec, prevalence_spec)

  # Accepted in v1.1 report spec
  section <- list(
    id = "prevalence",
    title = "Prevalence",
    items = list(list(
      type = "component",
      id = "prevalence-summary",
      title = "Prevalence summary",
      spec = prevalence_spec
    ))
  )
  report_v1_1 <- rtichoke:::rtichoke_viz_report_spec_v1_1(section)
  expect_identical(report_v1_1$sections[[1]]$items[[1]]$spec, prevalence_spec)

  # Wrong schemaVersion for summary_metrics rejected
  bad_version_spec <- prevalence_spec
  bad_version_spec$schemaVersion <- "2.0"
  expect_error(
    rtichoke:::rtichoke_viz_report_spec(bad_version_spec),
    "schemaVersion 1.0"
  )

  # Unsupported component type rejected
  unsupported_spec <- list(
    schemaVersion = "1.0",
    type = "unknown_type"
  )
  expect_error(
    rtichoke:::rtichoke_viz_report_spec(unsupported_spec),
    "unsupported type"
  )
})

test_that("prevalence SummaryMetrics is population-owned and handles single/multiple populations", {
  dat <- list(
    probs = list(
      "Model 1" = c(0.1, 0.4, 0.7, 0.9),
      "Model 2" = c(0.2, 0.3, 0.6, 0.8)
    ),
    reals = list("Population 1" = c(0, 1, 0, 1))
  )
  perf_data <- prepare_performance_data(dat$probs, dat$reals)
  metadata <- rtichoke:::build_evaluation_metadata(dat$probs, dat$reals)

  spec <- rtichoke:::rtichoke_viz_summary_metrics_prevalence_spec(
    perf_data,
    metadata
  )

  expect_identical(spec$schemaVersion, "1.0")
  expect_identical(spec$type, "summary_metrics")
  expect_length(spec$populations, 1L)
  expect_identical(
    spec$populations[[1]],
    list(id = "population-1", label = "Population 1")
  )
  expect_length(spec$metrics, 1L)
  expect_identical(spec$metrics[[1]]$metric, "prevalence")
  expect_identical(spec$metrics[[1]]$owner$type, "population")
  expect_identical(spec$metrics[[1]]$owner$populationId, "population-1")
  expect_equal(spec$metrics[[1]]$estimate, 0.5)

  # Serializes as expected JSON with null = "null"
  json <- jsonlite::toJSON(spec, auto_unbox = TRUE, null = "null")
  expect_match(json, '"schemaVersion":"1.0"', fixed = TRUE)
  expect_match(json, '"type":"summary_metrics"', fixed = TRUE)
  expect_match(json, '"metric":"prevalence"', fixed = TRUE)
})

test_that("prevalence SummaryMetrics keeps distinct populations separate even with similar labels", {
  probs <- list(
    "train" = seq(0.1, 0.9, length.out = 100),
    "test" = seq(0.1, 0.9, length.out = 100)
  )
  reals <- list(
    "train" = rep(c(0, 1), 50),
    "test" = rep(c(0, 0, 0, 1), 25)
  )
  perf_data <- prepare_performance_data(probs, reals)
  metadata <- rtichoke:::build_evaluation_metadata(probs, reals)

  spec <- rtichoke:::rtichoke_viz_summary_metrics_prevalence_spec(
    perf_data,
    metadata
  )

  expect_length(spec$populations, 2L)
  expect_identical(spec$populations[[1]]$label, "train")
  expect_identical(spec$populations[[2]]$label, "test")
  expect_equal(spec$metrics[[1]]$estimate, 0.5)
  expect_equal(spec$metrics[[2]]$estimate, 0.25)
})

test_that("AUROC SummaryMetrics is evaluation-owned and matches pROC::auc", {
  probs <- list(
    "Model A" = c(0.1, 0.2, 0.8, 0.9),
    "Model B" = c(0.1, 0.9, 0.2, 0.8)
  )
  reals <- list(c(0, 0, 1, 1))
  metadata <- rtichoke:::build_evaluation_metadata(probs, reals)

  spec <- rtichoke:::rtichoke_viz_summary_metrics_auroc_spec(
    probs,
    reals,
    metadata
  )

  expect_identical(spec$schemaVersion, "1.0")
  expect_identical(spec$type, "summary_metrics")
  expect_length(spec$evaluations, 2L)
  expect_identical(spec$evaluations[[1]]$id, "evaluation-1")
  expect_identical(spec$evaluations[[1]]$model, "Model A")
  expect_identical(spec$evaluations[[2]]$id, "evaluation-2")
  expect_identical(spec$evaluations[[2]]$model, "Model B")

  expect_length(spec$metrics, 2L)
  expect_identical(spec$metrics[[1]]$metric, "auroc")
  expect_identical(spec$metrics[[1]]$owner$type, "evaluation")
  expect_identical(spec$metrics[[1]]$owner$evaluationId, "evaluation-1")
  expect_equal(
    spec$metrics[[1]]$estimate,
    as.numeric(pROC::auc(reals[[1]], probs[[1]]))
  )

  expect_identical(spec$metrics[[2]]$metric, "auroc")
  expect_identical(spec$metrics[[2]]$owner$type, "evaluation")
  expect_identical(spec$metrics[[2]]$owner$evaluationId, "evaluation-2")
  expect_equal(
    spec$metrics[[2]]$estimate,
    as.numeric(pROC::auc(reals[[1]], probs[[2]]))
  )
})

test_that("AUROC SummaryMetrics maps undefined or single-class calculations to canonical null", {
  probs <- list("Single Class Model" = c(0.1, 0.5, 0.8))
  reals <- list(c(0, 0, 0)) # single-class outcome
  metadata <- rtichoke:::build_evaluation_metadata(probs, reals)

  spec <- rtichoke:::rtichoke_viz_summary_metrics_auroc_spec(
    probs,
    reals,
    metadata
  )

  expect_null(spec$metrics[[1]]$estimate)

  json <- jsonlite::toJSON(spec, auto_unbox = TRUE, null = "null")
  expect_match(json, '"estimate":null', fixed = TRUE)
})

test_that("AUROC SummaryMetrics correctly handles unlisted vector inputs and multi-population lists", {
  # Unlisted vectors
  p_vec <- c(0.1, 0.2, 0.8, 0.9)
  r_vec <- c(0, 0, 1, 1)
  meta_vec <- rtichoke:::build_evaluation_metadata(p_vec, r_vec)
  spec_vec <- rtichoke:::rtichoke_viz_summary_metrics_auroc_spec(
    p_vec,
    r_vec,
    meta_vec
  )
  expect_equal(
    spec_vec$metrics[[1]]$estimate,
    as.numeric(pROC::auc(r_vec, p_vec))
  )

  # Multi-population list
  probs_pop <- list(
    "train" = c(0.1, 0.2, 0.8, 0.9),
    "test" = c(0.2, 0.3, 0.7, 0.8)
  )
  reals_pop <- list(
    "train" = c(0, 0, 1, 1),
    "test" = c(0, 1, 0, 1)
  )
  meta_pop <- rtichoke:::build_evaluation_metadata(probs_pop, reals_pop)
  spec_pop <- rtichoke:::rtichoke_viz_summary_metrics_auroc_spec(
    probs_pop,
    reals_pop,
    meta_pop
  )
  expect_length(spec_pop$metrics, 2L)
  expect_equal(
    spec_pop$metrics[[1]]$estimate,
    as.numeric(pROC::auc(reals_pop$train, probs_pop$train))
  )
  expect_equal(
    spec_pop$metrics[[2]]$estimate,
    as.numeric(pROC::auc(reals_pop$test, probs_pop$test))
  )
})
