test_that("vendored rtichoke_viz v0.22.1 provenance and exports are intact", {
  vendor <- system.file("rtichoke-viz", package = "rtichoke")
  provenance <- readLines(file.path(vendor, "PROVENANCE"), warn = FALSE)

  expect_true("version: 0.22.1" %in% provenance)
  expect_true("archive: rtichoke-viz-0.22.1.tar.gz" %in% provenance)
  expect_true(
    "source_sha: 7cc8e8c23ee2f073ff3d8e4f32198eaac4785eb2" %in% provenance
  )
  expect_true(
    paste0(
      "archive_sha256: ",
      "b812f3fc283d404d3d5982cd1a84157c3cce1f13afcdc87c392d7c9ddc029189"
    ) %in%
      provenance
  )

  js <- paste(
    readLines(file.path(vendor, "rtichoke-viz.js"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(js, "renderRocV2", fixed = TRUE)
  expect_match(js, "renderGainsV2", fixed = TRUE)
  expect_match(js, "renderLiftV2", fixed = TRUE)
  expect_match(js, "renderDecisionCurveV2", fixed = TRUE)
  expect_match(js, "renderInterventionsAvoidedV2", fixed = TRUE)
  expect_match(js, "renderPerformanceTable", fixed = TRUE)
  expect_match(js, "renderPredictionDistribution", fixed = TRUE)
  expect_match(js, "renderReport", fixed = TRUE)
  expect_match(js, "ReportSpecV1_1Schema", fixed = TRUE)
  expect_match(js, "SummaryMetricsSpecSchema", fixed = TRUE)
  expect_match(js, "sectionComponentPresentation", fixed = TRUE)
})

test_that("vendored rtichoke_viz schemas preserve canonical ids", {
  vendor <- system.file("rtichoke-viz", package = "rtichoke")
  v1 <- paste(
    readLines(file.path(vendor, "rtichoke-viz.schema.json"), warn = FALSE),
    collapse = "\n"
  )
  v2 <- paste(
    readLines(file.path(vendor, "rtichoke-viz-v2.schema.json"), warn = FALSE),
    collapse = "\n"
  )
  report <- paste(
    readLines(
      file.path(vendor, "rtichoke-viz-report.schema.json"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(v1, "https://rtichoke.dev/schema/viz/1.0.json", fixed = TRUE)
  expect_match(v2, "https://rtichoke.dev/schema/viz/2.0.json", fixed = TRUE)
  expect_match(
    report,
    "https://rtichoke.dev/schema/viz/report.json",
    fixed = TRUE
  )
  expect_match(report, '"const": "1.1"', fixed = TRUE)
})

test_that("standalone v2 schema validates create_probs_histogram specs via jsonvalidate", {
  skip_if_not_installed("jsonvalidate")

  schema_path <- system.file(
    "rtichoke-viz",
    "rtichoke-viz-v2.schema.json",
    package = "rtichoke"
  )
  expect_true(file.exists(schema_path))

  validator <- jsonvalidate::json_validator(schema_path, engine = "ajv")

  # 1. Probability threshold spec via public create_probs_histogram()
  p_dist_thresh <- prepare_probs_distribution_data(
    probs = list(example_dat$estimated_probabilities),
    reals = list(example_dat$outcome),
    by = 0.1,
    stratified_by = "probability_threshold"
  )
  p_perf_thresh <- prepare_performance_data(
    probs = list(example_dat$estimated_probabilities),
    reals = list(example_dat$outcome),
    by = 0.1,
    stratified_by = "probability_threshold"
  )
  spec_thresh <- rtichoke_viz_prediction_distribution_spec(
    p_dist_thresh,
    p_perf_thresh
  )

  histogram_thresh <- create_probs_histogram(
    probs = list(example_dat$estimated_probabilities),
    reals = list(example_dat$outcome),
    by = 0.1,
    stratified_by = "probability_threshold"
  )
  spec_tag_thresh <- Filter(
    function(x) {
      is.list(x) && !is.null(x$attribs$id) && grepl("-spec$", x$attribs$id)
    },
    histogram_thresh
  )[[1]]
  json_thresh <- as.character(spec_tag_thresh$children[[1]])
  expect_true(validator(json_thresh))

  # 2. PPCR spec via public create_probs_histogram() using frozen tied N=9 fixture
  probs_tied <- list(c(0.00, 0.15, 0.30, 0.50, 0.50, 0.50, 0.65, 0.80, 1.00))
  reals_tied <- list(c(0, 1, 0, 1, 0, 1, 1, 0, 1))

  histogram_ppcr <- create_probs_histogram(
    probs = probs_tied,
    reals = reals_tied,
    by = 0.20,
    stratified_by = "ppcr"
  )
  spec_tag_ppcr <- Filter(
    function(x) {
      is.list(x) && !is.null(x$attribs$id) && grepl("-spec$", x$attribs$id)
    },
    histogram_ppcr
  )[[1]]
  json_ppcr <- as.character(spec_tag_ppcr$children[[1]])
  expect_true(validator(json_ppcr))

  # Assert emitted canonical spec retains the frozen rank-bin result and operating point identity
  spec_ppcr_obj <- jsonlite::fromJSON(json_ppcr, simplifyVector = FALSE)
  rank_df <- do.call(rbind, lapply(spec_ppcr_obj$rankBins, as.data.frame))
  expect_equal(rank_df$rankLower, c(0.00, 0.20, 0.40, 0.60, 0.80))
  expect_equal(rank_df$rankUpper, c(0.20, 0.40, 0.60, 0.80, 1.00))
  expect_equal(rank_df$positiveMass, c(1, 2, 0, 1, 1))
  expect_equal(rank_df$negativeMass, c(1, 2, 0, 0, 1))

  op_values <- vapply(
    spec_ppcr_obj$operatingPoints,
    `[[`,
    numeric(1),
    "value"
  )
  op_cutoffs <- vapply(
    spec_ppcr_obj$operatingPoints,
    `[[`,
    numeric(1),
    "cutoff"
  )
  op_realized <- vapply(
    spec_ppcr_obj$operatingPoints,
    `[[`,
    numeric(1),
    "realizedPpcr"
  )
  expect_equal(op_values, c(0.00, 0.20, 0.40, 0.60, 0.80, 1.00))
  expect_equal(
    op_cutoffs,
    c(1.00, 0.71, 0.50, 0.50, 0.24, 0.00),
    tolerance = 1e-2
  )
  expect_equal(
    op_realized,
    c(0 / 9, 2 / 9, 3 / 9, 3 / 9, 7 / 9, 9 / 9),
    tolerance = 1e-6
  )

  # 3. Missing required bins fails
  invalid_bins <- spec_thresh
  invalid_bins$bins <- NULL
  json_invalid_bins <- jsonlite::toJSON(
    invalid_bins,
    auto_unbox = TRUE,
    null = "null",
    digits = NA
  )
  expect_false(validator(json_invalid_bins))

  # 4. Unsupported component type fails
  invalid_type <- spec_thresh
  invalid_type$type <- "unsupported_type"
  json_invalid_type <- jsonlite::toJSON(
    invalid_type,
    auto_unbox = TRUE,
    null = "null",
    digits = NA
  )
  expect_false(validator(json_invalid_type))

  # 5. Schema contains all eight supported component branches
  schema_obj <- jsonlite::fromJSON(schema_path, simplifyVector = FALSE)
  expect_length(schema_obj$anyOf, 8L)
  branch_types <- vapply(
    schema_obj$anyOf,
    function(b) {
      if (!is.null(b$properties$type$const)) {
        b$properties$type$const
      } else if (!is.null(b$allOf)) {
        for (sub in b$allOf) {
          if (!is.null(sub$properties$type$const)) {
            return(sub$properties$type$const)
          }
        }
        NA_character_
      } else {
        NA_character_
      }
    },
    character(1)
  )
  expect_setequal(
    branch_types,
    c(
      "roc",
      "calibration",
      "precision_recall",
      "gains",
      "lift",
      "decision_curve",
      "interventions_avoided",
      "prediction_distribution"
    )
  )
})
