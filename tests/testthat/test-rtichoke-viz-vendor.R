test_that("vendored rtichoke_viz v0.21.0 provenance and exports are intact", {
  vendor <- system.file("rtichoke-viz", package = "rtichoke")
  provenance <- readLines(file.path(vendor, "PROVENANCE"), warn = FALSE)

  expect_true("version=0.21.0" %in% provenance)
  expect_true("archive=rtichoke-viz-0.21.0.tar.gz" %in% provenance)
  expect_true(
    "commit=e91435f88d337c9aa6086ac7ce5a45083975bac4" %in% provenance
  )
  expect_true(
    paste0(
      "sha256=",
      "4617aed5120fac07f7381062fc4c8dfecf562a6e578ad40092179d6e5accf7b8"
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
  expect_match(js, "PredictionDistributionSpecSchema", fixed = TRUE)
  expect_match(js, "renderPredictionDistribution", fixed = TRUE)
  expect_match(js, "renderPerformanceTable", fixed = TRUE)
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
  expect_true(grepl(
    "https://rtichoke.dev/schema/viz/report.json",
    report,
    fixed = TRUE
  ))
  expect_true(grepl('"const": "1.1"', report, fixed = TRUE))
})
