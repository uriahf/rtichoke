test_that("one model and one population have one evaluation", {
  metadata <- rtichoke:::build_evaluation_metadata(
    probs = list(c(0.1, 0.9)),
    reals = list(c(0, 1))
  )

  expect_identical(metadata$model, "model")
  expect_identical(metadata$population, "population")
  expect_identical(metadata$evaluation, "model")
})

test_that("multiple models share stable population identity", {
  metadata <- rtichoke:::build_evaluation_metadata(
    probs = list(
      "Model A" = c(0.1, 0.9),
      "Model B" = c(0.2, 0.8)
    ),
    reals = list("Population A" = c(0, 1))
  )

  expect_identical(metadata$model, c("Model A", "Model B"))
  expect_identical(metadata$population, c("Population A", "Population A"))
  expect_identical(metadata$evaluation, c("Model A", "Model B"))
})

test_that("keyed populations retain distinct identities", {
  metadata <- rtichoke:::build_evaluation_metadata(
    probs = list(
      "Population A" = c(0.1, 0.9),
      "Population B" = c(0.2, 0.8)
    ),
    reals = list(
      "Population A" = c(0, 1),
      "Population B" = c(0, 1)
    )
  )

  expect_true(all(is.na(metadata$model)))
  expect_identical(metadata$population, c("Population A", "Population B"))
  expect_identical(metadata$evaluation, c("Population A", "Population B"))
})

test_that("paired inputs preserve evaluation labels without guessing model", {
  pair_names <- c("Model A @ Population A", "Model B @ Population B")
  metadata <- rtichoke:::build_evaluation_metadata(
    probs = stats::setNames(
      list(c(0.1, 0.9), c(0.2, 0.8)),
      pair_names
    ),
    reals = stats::setNames(
      list(c(0, 1), c(0, 1)),
      pair_names
    )
  )

  expect_true(all(is.na(metadata$model)))
  expect_identical(metadata$population, pair_names)
  expect_identical(metadata$evaluation, pair_names)
})
