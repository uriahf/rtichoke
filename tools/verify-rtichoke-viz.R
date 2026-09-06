archive <- "inst/rtichoke-viz/rtichoke-viz-0.20.2.tar.gz"
expected <- "2e4851159ceea0e3b2420c7e0aa22a566b94ec7057c27b13b7db7da88de34d11"
sha256 <- system2("sha256sum", archive, stdout = TRUE)
if (!length(sha256)) {
  stop("sha256sum did not return a digest", call. = FALSE)
}
actual <- strsplit(sha256[[1]], "[[:space:]]+")[[1]][[1]]
stopifnot(identical(actual, expected))

files <- utils::untar(archive, list = TRUE)
root <- "rtichoke-viz-0.20.2"
payload <- c(
  "MANIFEST",
  "rtichoke-viz.js",
  "rtichoke-viz.css",
  "rtichoke-viz.schema.json",
  "rtichoke-viz-v2.schema.json",
  "rtichoke-viz-report.schema.json"
)
stopifnot(all(file.path(root, payload) %in% files))

extracted <- tempfile("rtichoke-viz-")
dir.create(extracted)
utils::untar(archive, exdir = extracted)
release_dir <- file.path(extracted, root)
manifest <- readLines(file.path(release_dir, "MANIFEST"), warn = FALSE)
stopifnot(
  "version=0.20.2" %in% manifest,
  "commit=40748bdeff7d535f516744886b64056f3aaa518d" %in% manifest
)

read_raw <- function(path) {
  readBin(path, what = "raw", n = file.info(path)$size)
}
for (name in payload) {
  stopifnot(identical(
    read_raw(file.path(release_dir, name)),
    read_raw(file.path("inst/rtichoke-viz", name))
  ))
}

js <- paste(
  readLines(file.path(release_dir, "rtichoke-viz.js"), warn = FALSE),
  collapse = "\n"
)
stopifnot(
  grepl("renderRocV2", js, fixed = TRUE),
  grepl("renderGainsV2", js, fixed = TRUE),
  grepl("renderLiftV2", js, fixed = TRUE),
  grepl("renderDecisionCurveV2", js, fixed = TRUE),
  grepl("InterventionsAvoidedV2SpecSchema", js, fixed = TRUE),
  grepl("renderInterventionsAvoidedV2", js, fixed = TRUE),
  grepl("renderPerformanceTable", js, fixed = TRUE),
  grepl("renderReport", js, fixed = TRUE),
  grepl("ReportSpecV1_1Schema", js, fixed = TRUE),
  grepl("SummaryMetricsSpecSchema", js, fixed = TRUE),
  grepl("sectionComponentPresentation", js, fixed = TRUE)
)

schemas <- c(
  "rtichoke-viz.schema.json" = "https://rtichoke.dev/schema/viz/1.0.json",
  "rtichoke-viz-v2.schema.json" = "https://rtichoke.dev/schema/viz/2.0.json",
  "rtichoke-viz-report.schema.json" = "https://rtichoke.dev/schema/viz/report.json"
)
for (name in names(schemas)) {
  schema <- paste(
    readLines(file.path(release_dir, name), warn = FALSE),
    collapse = "\n"
  )
  stopifnot(grepl(unname(schemas[[name]]), schema, fixed = TRUE))
}
unlink(extracted, recursive = TRUE)
message("Verified rtichoke_viz v0.20.2: ", actual)
