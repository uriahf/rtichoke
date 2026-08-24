test_that("vendored rtichoke_viz v0.5.0 provenance and exports are intact", {
  vendor <- system.file("rtichoke-viz", package = "rtichoke")
  provenance <- readLines(file.path(vendor, "PROVENANCE"), warn = FALSE)

  expect_true("version=0.5.0" %in% provenance)
  expect_true("archive=rtichoke-viz-0.5.0.tar.gz" %in% provenance)
  expect_true(
    "commit=9c5a114ebe968e8cef4d2f14bf82ed552d2c8a17" %in% provenance
  )
  expect_true(
    paste0(
      "sha256=",
      "ab36ae71f9090b4de62da8f552ebe84ac35885ab04958667faaf070db2c98f65"
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
