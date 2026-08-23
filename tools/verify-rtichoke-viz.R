archive <- "inst/rtichoke-viz/rtichoke-viz-0.3.0.tar.gz"
expected <- "558f8d9e16f9544659b84e33f72511065163291a1b97a3c5511b61d1e1f0cac1"
sha256 <- system2("sha256sum", archive, stdout = TRUE)
if (!length(sha256)) {
  stop("sha256sum did not return a digest", call. = FALSE)
}
actual <- strsplit(sha256[[1]], "[[:space:]]+")[[1]][[1]]
stopifnot(identical(actual, expected))

files <- utils::untar(archive, list = TRUE)
required <- file.path(
  "rtichoke-viz-0.3.0",
  c(
    "MANIFEST",
    "rtichoke-viz.js",
    "rtichoke-viz.css",
    "rtichoke-viz.schema.json",
    "rtichoke-viz-v2.schema.json"
  )
)
stopifnot(all(required %in% files))
message("Verified rtichoke_viz v0.3.0: ", actual)
