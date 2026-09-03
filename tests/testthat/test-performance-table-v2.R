performance_table_spec <- function(
  probs,
  reals,
  stratified_by = "probability_threshold"
) {
  performance_data <- prepare_performance_data(
    probs,
    reals,
    by = 0.5,
    stratified_by = stratified_by
  )
  rtichoke:::rtichoke_viz_performance_table_v2_spec(
    performance_data,
    rtichoke:::build_evaluation_metadata(probs, reals),
    stratified_by = stratified_by
  )
}

test_that("performance table v2 represents one model and population", {
  probs <- list("Model A" = c(0.1, 0.2, 0.8, 0.9))
  reals <- list("Population A" = c(0, 0, 1, 1))
  spec <- performance_table_spec(probs, reals)

  expect_identical(spec$schemaVersion, "2.0")
  expect_identical(spec$type, "performance_table")
  expect_identical(
    spec$evaluations,
    list(list(
      id = "evaluation-1",
      population = "Population A",
      model = "Model A"
    ))
  )
  expect_false(any(vapply(
    spec$rows,
    function(x) "seriesId" %in% names(x),
    logical(1)
  )))
})

test_that("performance table v2 shares population across models", {
  probs <- list(
    "Model A" = c(0.1, 0.2, 0.8, 0.9),
    "Model B" = c(0.2, 0.3, 0.7, 0.8)
  )
  reals <- list("Population A" = c(0, 0, 1, 1))
  spec <- performance_table_spec(probs, reals)

  expect_identical(
    vapply(spec$evaluations, `[[`, "", "population"),
    c("Population A", "Population A")
  )
  expect_identical(
    vapply(spec$evaluations, `[[`, "", "model"),
    c("Model A", "Model B")
  )
})

test_that("performance table v2 preserves unknown model populations", {
  probs <- list(
    "Population A" = c(0.1, 0.2, 0.8, 0.9),
    "Population B" = c(0.2, 0.7, 0.3, 0.8)
  )
  reals <- list(
    "Population A" = c(0, 0, 1, 1),
    "Population B" = c(0, 1, 0, 1)
  )
  spec <- performance_table_spec(probs, reals)

  expect_identical(
    vapply(spec$evaluations, `[[`, "", "population"),
    c("Population A", "Population B")
  )
  expect_false(any(vapply(
    spec$evaluations,
    function(x) "model" %in% names(x),
    logical(1)
  )))
})

test_that("performance table v2 evaluation IDs are deterministic", {
  probs <- list(
    "Model A" = c(0.1, 0.2, 0.8, 0.9),
    "Model B" = c(0.2, 0.3, 0.7, 0.8)
  )
  reals <- list("Population A" = c(0, 0, 1, 1))
  first <- performance_table_spec(probs, reals)
  second <- performance_table_spec(probs, reals)

  expect_identical(
    vapply(first$evaluations, `[[`, "", "id"),
    vapply(second$evaluations, `[[`, "", "id")
  )
})

test_that("performance table v2 maps operating points and long metrics", {
  probs <- list("Model A" = c(0.1, 0.2, 0.8, 0.9))
  reals <- list("Population A" = c(0, 0, 1, 1))
  threshold_spec <- performance_table_spec(probs, reals)
  ppcr_spec <- performance_table_spec(probs, reals, "ppcr")

  expect_identical(
    threshold_spec$rows[[1]]$operatingPoint$type,
    "probability_threshold"
  )
  expect_identical(ppcr_spec$rows[[1]]$operatingPoint$type, "ppcr")
  expect_true(all(vapply(
    threshold_spec$rows[[1]]$values,
    function(x) identical(sort(names(x)), c("estimate", "metricId")),
    logical(1)
  )))
  expect_true(
    "net_benefit" %in% vapply(threshold_spec$metrics, `[[`, "", "id")
  )
  expect_false(
    "net_benefit" %in% vapply(ppcr_spec$metrics, `[[`, "", "id")
  )
})

test_that("performance table v2 distinguishes zero and missing", {
  performance_data <- tibble::tibble(
    probability_threshold = c(0.2, 0.3),
    sensitivity = c(0, NA_real_),
    PPV = c(NaN, 0.5)
  )
  metadata <- data.frame(
    model = "Model A",
    population = "Population A",
    evaluation = "Model A"
  )
  spec <- rtichoke:::rtichoke_viz_performance_table_v2_spec(
    performance_data,
    metadata
  )

  first <- spec$rows[[1]]$values
  second <- spec$rows[[2]]$values
  expect_identical(first[[1]]$estimate, 0)
  expect_null(first[[2]]$estimate)
  expect_null(second[[1]]$estimate)
  expect_identical(second[[2]]$estimate, 0.5)
})

test_that("performance table v2 copies existing statistics without recomputation", {
  performance_data <- tibble::tibble(
    probability_threshold = 0.25,
    TP = 7,
    sensitivity = 0.123,
    specificity = 0.456,
    lift = 9.876,
    NB = -0.321
  )
  metadata <- data.frame(
    model = "Model A",
    population = "Population A",
    evaluation = "Model A"
  )
  spec <- rtichoke:::rtichoke_viz_performance_table_v2_spec(
    performance_data,
    metadata
  )
  estimates <- stats::setNames(
    vapply(spec$rows[[1]]$values, function(x) x$estimate, numeric(1)),
    vapply(spec$rows[[1]]$values, `[[`, "", "metricId")
  )

  expect_identical(unname(estimates[["true_positives"]]), 7)
  expect_identical(unname(estimates[["sensitivity"]]), 0.123)
  expect_identical(unname(estimates[["specificity"]]), 0.456)
  expect_identical(unname(estimates[["lift"]]), 9.876)
  expect_identical(unname(estimates[["net_benefit"]]), -0.321)
})

test_that("threshold and PPCR canonical performance table rows include all four confusion metrics", {
  probs <- list("Model A" = c(0.1, 0.2, 0.8, 0.9))
  reals <- list("Population A" = c(0, 0, 1, 1))

  thresh_spec <- performance_table_spec(
    probs,
    reals,
    stratified_by = "probability_threshold"
  )
  ppcr_spec <- performance_table_spec(probs, reals, stratified_by = "ppcr")

  confusion_metric_ids <- c(
    "true_positives",
    "true_negatives",
    "false_positives",
    "false_negatives"
  )

  for (spec in list(thresh_spec, ppcr_spec)) {
    spec_metric_ids <- vapply(spec$metrics, `[[`, character(1), "id")
    expect_true(all(confusion_metric_ids %in% spec_metric_ids))

    for (row in spec$rows) {
      row_metric_ids <- vapply(row$values, `[[`, character(1), "metricId")
      expect_true(all(confusion_metric_ids %in% row_metric_ids))
      for (cm_id in confusion_metric_ids) {
        val <- row$values[[match(cm_id, row_metric_ids)]]$estimate
        expect_true(is.numeric(val) && is.finite(val))
      }
    }
  }
})

test_that("existing performance table defaults are unchanged", {
  expect_identical(formals(create_performance_table)$output_type, "reactable")
  expect_identical(formals(render_performance_table)$output_type, "reactable")
})
