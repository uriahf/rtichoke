test_that("vendored rtichoke_viz v0.7.0 provenance and exports are intact", {
  vendor <- system.file("rtichoke-viz", package = "rtichoke")
  provenance <- readLines(file.path(vendor, "PROVENANCE"), warn = FALSE)

  expect_true("version=0.7.0" %in% provenance)
  expect_true("archive=rtichoke-viz-0.7.0.tar.gz" %in% provenance)
  expect_true(
    "commit=b3564d2824ec1791f791fda406c99b3d7865a68f" %in% provenance
  )
  expect_true(
    paste0(
      "sha256=",
      "f09c30e231a8be39c2e89ba6ae39c90ed8cab67021213e17681a475066a9806e"
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

  expect_match(v1, "https://rtichoke.dev/schema/viz/1.0.json", fixed = TRUE)
  expect_match(v2, "https://rtichoke.dev/schema/viz/2.0.json", fixed = TRUE)
})
