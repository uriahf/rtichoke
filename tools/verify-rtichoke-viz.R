archive <- "inst/rtichoke-viz/rtichoke-viz-0.4.0.tar.gz"
expected <- "9a687cd938f1875d577e592664ca75447455166169f6132dd7f79406515e14e1"
sha256 <- system2("sha256sum", archive, stdout = TRUE)
if (!length(sha256)) {
  stop("sha256sum did not return a digest", call. = FALSE)
}
actual <- strsplit(sha256[[1]], "[[:space:]]+")[[1]][[1]]
stopifnot(identical(actual, expected))

files <- utils::untar(archive, list = TRUE)
required <- file.path(
  "rtichoke-viz-0.4.0",
  c(
    "MANIFEST",
    "rtichoke-viz.js",
    "rtichoke-viz.css",
    "rtichoke-viz.schema.json",
    "rtichoke-viz-v2.schema.json"
  )
)
stopifnot(all(required %in% files))
message("Verified rtichoke_viz v0.4.0: ", actual)
