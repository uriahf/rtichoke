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
  expect_false(any(vapply(spec$evaluations, function(x) "model" %in% names(x), logical(1))))
  expect_identical(
    lapply(spec$series, `[[`, "display"),
    list(
      list(label = "Population A", group = "Population A", role = "population"),
      list(label = "Population B", group = "Population B", role = "population")
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
