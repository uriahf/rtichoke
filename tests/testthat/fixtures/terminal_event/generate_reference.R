#!/usr/bin/env Rscript
# Independent terminal-event oracle. No rtichoke or polarstate imports.
for (pkg in c("survival", "cmprsk", "jsonlite")) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Missing reference package: ", pkg)
}
cohort <- function(time, event) data.frame(
  id = LETTERS[seq_along(time)], time = time, event = event
)
cases <- list(
  fixture_a = list(data = cohort(c(2, 4, 5, 7, 10, 10, 12, 14), c(1, 2, 0, 0, 2, 1, 0, 1)),
                   horizons = c(0, 1, 4, 5, 7, 10, 12, 14, 15)),
  zero_ties = list(data = cohort(c(0, 0, 0, 2), c(1, 2, 0, 0)), horizons = c(0, 1, 2, 3)),
  all_censored = list(data = cohort(c(0, 2), c(0, 0)), horizons = c(0, 1, 2, 3)),
  no_competing = list(data = cohort(c(1, 2, 3), c(1, 0, 1)), horizons = c(0, 2, 3, 4)),
  no_target = list(data = cohort(c(1, 2, 3), c(2, 0, 2)), horizons = c(0, 2, 3, 4)),
  all_competing = list(data = cohort(c(1, 1), c(2, 2)), horizons = c(0, 1, 2)),
  tied_events_censor = list(data = cohort(c(1, 1, 1, 3), c(1, 2, 0, 0)), horizons = c(0, 1, 2, 3, 4))
)
reference_row <- function(original, h, censoring, competing) {
  censor_ids <- original$id[original$event == 0 & original$time < h & censoring == "excluded"]
  competing_ids <- original$id[original$event == 2 & original$time <= h & competing == "excluded"]
  d <- original[!original$id %in% c(censor_ids, competing_ids), ]
  states <- if (competing == "adjusted_as_negative") c("primary", "competing", "event_free") else
    c(if (competing == "adjusted_as_composite") "composite" else "primary", "event_free")
  result <- list(horizon = h, retained_ids = unname(d$id),
                 censored_excluded_ids = unname(censor_ids), competing_excluded_ids = unname(competing_ids),
                 original_n = nrow(original), retained_n = nrow(d),
                 censored_excluded_count = length(censor_ids), competing_excluded_count = length(competing_ids))
  if (!nrow(d)) return(c(result, list(status = "undefined_empty_population",
                                    probabilities = setNames(rep(list(NULL), length(states)), states),
                                    cuminc = NULL)))
  if (competing == "adjusted_as_censored") d$event[d$event == 2] <- 0
  if (competing == "adjusted_as_composite") d$event[d$event == 2] <- 1
  if (competing == "adjusted_as_negative") {
    observed_causes <- sort(unique(d$event[d$event > 0]))
    if (!length(observed_causes)) {
      # survival 3.8-12 cannot fit a multistate response with no transitions.
      # Fit the degenerate binary process; missing causes are structural zeros.
      fit <- survival::survfit(survival::Surv(time, event == 1) ~ 1, data = d)
      probabilities <- c(primary = 0, competing = 0, event_free = unname(tail(fit$surv, 1)))
    } else {
      d$status <- factor(d$event, levels = c(0, observed_causes),
                         labels = c("censor", c("primary", "competing")[observed_causes]))
      fit <- survival::survfit(survival::Surv(time, status) ~ 1, data = d)
      # Read right-continuous pstate; before first observation use initial occupancy.
      index <- which(fit$time <= h)
      p <- if (length(index)) fit$pstate[max(index), ] else c(1, rep(0, length(observed_causes)))
      names(p) <- fit$states
      probabilities <- c(primary = 0, competing = 0, event_free = p[["(s0)"]])
      for (state in intersect(names(p), c("primary", "competing"))) {
        probabilities[[state]] <- p[[state]]
      }
    }
    ci <- NULL
    if (any(d$event > 0)) {
      curves <- cmprsk::cuminc(d$time, d$event, cencode = 0)
      cause_risk <- function(cause) {
        curve <- curves[[paste("1", cause)]]
        if (is.null(curve)) return(0)
        indices <- which(curve$time <= h)
        if (length(indices)) curve$est[max(indices)] else 0
      }
      ci <- c(primary = cause_risk(1), competing = cause_risk(2))
      stopifnot(max(abs(ci - probabilities[c("primary", "competing")])) < 1e-12)
    }
  } else {
    fit <- survival::survfit(survival::Surv(time, event == 1) ~ 1, data = d)
    index <- which(fit$time <= h)
    s <- if (length(index)) fit$surv[max(index)] else 1
    probabilities <- setNames(c(1 - s, s), states)
    ci <- NULL
  }
  # Preserve package plateau values, but flag unsupported extrapolation separately.
  status <- if (h > max(d$time) && probabilities[["event_free"]] > 0)
    "beyond_support_plateau" else "defined"
  c(result, list(status = status, probabilities = as.list(probabilities), cuminc = if (is.null(ci)) NULL else as.list(ci)))
}
for (name in names(cases)) {
  case <- cases[[name]]
  variants <- list()
  for (censoring in c("excluded", "adjusted")) {
    for (competing in c("excluded", "adjusted_as_censored", "adjusted_as_negative", "adjusted_as_composite")) {
      key <- paste(censoring, competing, sep = ":")
      variants[[key]] <- lapply(case$horizons, function(h) reference_row(case$data, h, censoring, competing))
    }
  }
  cases[[name]]$variants <- variants
}
reference <- list(
  versions = list(R = as.character(getRversion()), survival = as.character(packageVersion("survival")),
                  cmprsk = as.character(packageVersion("cmprsk")), jsonlite = as.character(packageVersion("jsonlite"))),
  contract = list(events = "time <= horizon", censoring_exclusion = "original event == 0 and time < horizon",
                  competing_exclusion = "original event == 2 and time <= horizon",
                  zero = "ordinary right-continuous horizon, not synthetic pre-event baseline",
                  empty = "undefined: null probabilities, never zero risk",
                  beyond_support = "package plateau recorded and explicitly flagged; no claim of identified extrapolated risk"),
  cases = cases
)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) stop("Usage: Rscript generate_reference.R OUTPUT.json")
jsonlite::write_json(reference, args[[1]], auto_unbox = TRUE, pretty = TRUE, digits = 16, null = "null")
