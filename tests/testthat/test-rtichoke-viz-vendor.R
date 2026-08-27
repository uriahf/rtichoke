test_that("vendored rtichoke_viz v0.11.0 provenance and exports are intact", {
  vendor <- system.file("rtichoke-viz", package = "rtichoke")
  provenance <- readLines(file.path(vendor, "PROVENANCE"), warn = FALSE)

  expect_true("version=0.11.0" %in% provenance)
  expect_true("archive=rtichoke-viz-0.11.0.tar.gz" %in% provenance)
  expect_true(
    "commit=f26e7000e17d1bded5e030dd42b44ca82128ed2f" %in% provenance
  )
  expect_true(
    paste0(
      "sha256=",
      "3d43c73b7c5f820c018dd2a5766400826b008bd8323db2d378f3b2901df10694"
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
  expect_match(js, "renderReport", fixed = TRUE)
  expect_match(js, "ReportSpecV1_1Schema", fixed = TRUE)
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
