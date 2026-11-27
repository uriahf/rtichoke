# Run from the repository root:
# Rscript scripts/rtichoke_fixed_horizon_calibration_proof.R /tmp/calibration.pdf
source("R/terminal_outcome_distribution.R")

horizon <- 3
subjects <- data.frame(
  id = seq_len(12),
  probs = c(
    0.06,
    0.10,
    0.14,
    0.18,
    0.24,
    0.30,
    0.36,
    0.42,
    0.54,
    0.62,
    0.70,
    0.78
  ),
  reals = c(0, 2, 0, 0, 0, 1, 2, 0, 0, 1, 1, 0),
  times = c(0.5, 2, 5, 5, 0.5, 2, 2, 5, 0.5, 2, 3, 5)
)
risk_band <- cut(
  subjects$probs,
  breaks = c(0, 0.2, 0.5, 1),
  include.lowest = TRUE
)
bands <- split(subjects, risk_band, drop = TRUE)
prepared <- prepare_terminal_outcome_distributions(bands, horizon)
aj_rows <- Filter(
  function(row) {
    row$censoring == "adjusted" && row$competing == "adjusted_as_negative"
  },
  prepared
)
stopifnot(identical(
  vapply(aj_rows, function(row) row$population, ""),
  names(bands)
))
stopifnot(all(vapply(aj_rows, function(row) row$status == "defined", TRUE)))

calibration <- data.frame(
  risk_band = names(bands),
  n = unname(vapply(bands, nrow, integer(1))),
  predicted = unname(vapply(bands, function(band) mean(band$probs), 0)),
  observed = vapply(aj_rows, function(row) row$probabilities$primary, 0)
)

# One early censor leaves three at risk in each band before its first event.
# The low band has no target events, the middle band has one target tied
# with a competing event, and the high band has two targets (one at h = 3).
# The high band's second AJ increment is (2/3) * (1/2) = 1/3.
stopifnot(
  isTRUE(all.equal(calibration$predicted, c(0.12, 0.33, 0.66))),
  isTRUE(all.equal(calibration$observed, c(0, 1 / 3, 2 / 3)))
)
print(calibration, row.names = FALSE)

args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1]] else tempfile(fileext = ".pdf")
grDevices::pdf(output, width = 6.25, height = 6.25)
graphics::plot(
  calibration$predicted,
  calibration$observed,
  xlim = c(0, 1),
  ylim = c(0, 1),
  asp = 1,
  xlab = "Mean predicted target-event risk",
  ylab = "Observed target-event risk (Aalen-Johansen)",
  main = sprintf("Calibration at horizon %s", horizon),
  sub = "Target-event cumulative incidence within each risk band",
  pch = 19,
  col = "#15616d",
  cex = 1.4
)
graphics::abline(0, 1, lty = 2, col = "grey55")
graphics::text(
  calibration$predicted,
  calibration$observed,
  labels = paste0("n = ", calibration$n),
  pos = 4,
  cex = 0.8
)
invisible(grDevices::dev.off())
message("Wrote ", output)
