archive <- "inst/rtichoke-viz/rtichoke-viz-0.5.0.tar.gz"
expected <- "ab36ae71f9090b4de62da8f552ebe84ac35885ab04958667faaf070db2c98f65"
sha256 <- system2("sha256sum", archive, stdout = TRUE)
if (!length(sha256)) {
  stop("sha256sum did not return a digest", call. = FALSE)
}
actual <- strsplit(sha256[[1]], "[[:space:]]+")[[1]][[1]]
stopifnot(identical(actual, expected))

files <- utils::untar(archive, list = TRUE)
root <- "rtichoke-viz-0.5.0"
payload <- c(
  "MANIFEST",
  "rtichoke-viz.js",
  "rtichoke-viz.css",
  "rtichoke-viz.schema.json",
  "rtichoke-viz-v2.schema.json"
)
stopifnot(all(file.path(root, payload) %in% files))

extracted <- tempfile("rtichoke-viz-")
dir.create(extracted)
utils::untar(archive, exdir = extracted)
release_dir <- file.path(extracted, root)
manifest <- readLines(file.path(release_dir, "MANIFEST"), warn = FALSE)
stopifnot(
  "version=0.5.0" %in% manifest,
  "commit=9c5a114ebe968e8cef4d2f14bf82ed552d2c8a17" %in% manifest
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
  grepl("renderPerformanceTable", js, fixed = TRUE),
  grepl("renderReport", js, fixed = TRUE)
)

schemas <- c(
  "rtichoke-viz.schema.json" = "https://rtichoke.dev/schema/viz/1.0.json",
  "rtichoke-viz-v2.schema.json" = "https://rtichoke.dev/schema/viz/2.0.json"
)
for (name in names(schemas)) {
  schema <- paste(
    readLines(file.path(release_dir, name), warn = FALSE),
    collapse = "\n"
  )
  stopifnot(grepl(unname(schemas[[name]]), schema, fixed = TRUE))
}
unlink(extracted, recursive = TRUE)
message("Verified rtichoke_viz v0.5.0: ", actual)
