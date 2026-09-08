test_that("prediction distribution adapter builds the exact canonical spec", {
  distribution_data <- prepare_probs_distribution_data(
    probs = list("Model A" = c(0, 0.2, 0.5, 1)),
    reals = list(c(0, 1, 0, 1)),
    by = 0.5
  )

  spec <- rtichoke_viz_prediction_distribution_spec(distribution_data)

  expect_identical(spec$schemaVersion, "2.0")
  expect_identical(spec$type, "prediction_distribution")
  expect_identical(spec$operatingPoint$dimension, "probability_threshold")
  expect_identical(
    spec$evaluations,
    list(list(
      id = "evaluation-1",
      population = "population",
      model = "Model A"
    ))
  )
  expect_named(
    spec$bins[[1]],
    c(
      "evaluationId",
      "lower",
      "upper",
      "includeLower",
      "includeUpper",
      "nPositive",
      "nNegative"
    )
  )
  expect_named(
    spec$operatingPoints[[1]],
    c("evaluationId", "type", "value", "cutoff", "realizedPpcr")
  )
  expect_true(all(vapply(
    spec$bins,
    function(bin) identical(bin$evaluationId, "evaluation-1"),
    logical(1)
  )))
})

test_that("cutoff zero preserves R all-positive semantics", {
  distribution_data <- prepare_probs_distribution_data(
    probs = list(c(0, 0.3, 0.8)),
    reals = list(c(1, 0, 1)),
    by = 0.1
  )
  spec <- rtichoke_viz_prediction_distribution_spec(distribution_data)

  cutoff_zero <- Filter(
    function(operating_point) operating_point$cutoff == 0,
    spec$operatingPoints
  )

  expect_length(cutoff_zero, 1L)
  expect_equal(cutoff_zero[[1]]$realizedPpcr, 1)
  expect_equal(sum(vapply(spec$bins, `[[`, integer(1), "nPositive")), 2L)
  expect_equal(sum(vapply(spec$bins, `[[`, integer(1), "nNegative")), 1L)
})

test_that("PPCR specs preserve requested and realized operating points", {
  distribution_data <- prepare_probs_distribution_data(
    probs = list(c(0.1, 0.2, 0.5, 0.5, 0.8, 0.9)),
    reals = list(c(0, 0, 1, 0, 1, 1)),
    by = 0.5,
    stratified_by = "ppcr"
  )
  spec <- rtichoke_viz_prediction_distribution_spec(distribution_data)
  ppcr_half <- Filter(
    function(operating_point) operating_point$value == 0.5,
    spec$operatingPoints
  )

  expect_identical(spec$operatingPoint$dimension, "ppcr")
  expect_length(ppcr_half, 1L)
  expect_equal(ppcr_half[[1]]$cutoff, 0.5)
  expect_equal(ppcr_half[[1]]$realizedPpcr, 1 / 3)
})

test_that("multiple evaluations retain model and population identity", {
  shared_population <- rtichoke_viz_prediction_distribution_spec(
    prepare_probs_distribution_data(
      probs = list(
        "Model A" = c(0.1, 0.5, 0.9),
        "Model B" = c(0.2, 0.4, 0.8)
      ),
      reals = list(c(0, 1, 1)),
      by = 0.5
    )
  )
  distinct_populations <- rtichoke_viz_prediction_distribution_spec(
    prepare_probs_distribution_data(
      probs = list(
        "Train" = c(0.1, 0.5, 0.9),
        "Test" = c(0.2, 0.4, 0.8)
      ),
      reals = list(
        "Train" = c(0, 1, 1),
        "Test" = c(1, 0, 1)
      ),
      by = 0.5
    )
  )

  expect_equal(
    vapply(shared_population$evaluations, `[[`, character(1), "model"),
    c("Model A", "Model B")
  )
  expect_equal(
    vapply(shared_population$evaluations, `[[`, character(1), "population"),
    c("population", "population")
  )
  expect_equal(
    vapply(
      distinct_populations$evaluations,
      `[[`,
      character(1),
      "population"
    ),
    c("Train", "Test")
  )
})

test_that("create_probs_histogram returns a standalone browser component", {
  histogram <- create_probs_histogram(
    probs = list(example_dat$estimated_probabilities),
    reals = list(example_dat$outcome),
    by = 0.1
  )
  html <- as.character(histogram)
  dependency <- htmltools::htmlDependencies(histogram)[[1]]

  expect_s3_class(histogram, "shiny.tag.list")
  expect_match(html, "renderPredictionDistribution", fixed = TRUE)
  expect_match(html, '"type":"prediction_distribution"', fixed = TRUE)
  expect_identical(dependency$name, "rtichoke-viz")
  expect_identical(dependency$version, "0.21.0")
})

test_that("prediction histogram browser dependencies save with standalone HTML", {
  output_dir <- tempfile("rtichoke-probs-histogram-")
  dir.create(output_dir)
  output_file <- file.path(output_dir, "probs-histogram.html")

  histogram <- create_probs_histogram(
    probs = list(c(0, 0.2, 0.5, 0.8, 1)),
    reals = list(c(0, 0, 1, 1, 1)),
    by = 0.25
  )
  htmltools::save_html(histogram, output_file)

  saved_html <- paste(readLines(output_file, warn = FALSE), collapse = "\n")
  expect_true(file.exists(output_file))
  expect_match(saved_html, "renderPredictionDistribution", fixed = TRUE)
  expect_true(file.exists(file.path(
    output_dir,
    "lib",
    "rtichoke-viz-0.21.0",
    "rtichoke-viz.js"
  )))
  expect_true(file.exists(file.path(
    output_dir,
    "lib",
    "rtichoke-viz-0.21.0",
    "rtichoke-viz.css"
  )))
})
