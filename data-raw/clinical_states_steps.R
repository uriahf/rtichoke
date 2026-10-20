# Script to generate synthetic dataset: clinical_states_steps
#
# Provenance and Attribution:
# This dataset is independently simulated for demonstration and visualization purposes,
# drawing inspiration from Frank Harrell's work on longitudinal ordinal Markov /
# state-occupancy models and VIOLET 2 trial examples:
# https://hbiostat.org/r/hmisc/markov/sim#estimating-efficiency-from-the-study
#
# Note: This is an independently simulated dataset and is NOT Frank Harrell's original
# trial data, fitted model, or endorsed implementation.

set.seed(2026)

n_subjects <- 600L
days <- 1:27
states <- c("Home", "Hospital", "Vent/ARDS", "Dead")
p_initial <- c(0, 0.85, 0.15, 0)

P <- rbind(
  c(0.97, 0.03, 0, 0),
  c(0.10, 0.82, 0.06, 0.02),
  c(0.01, 0.08, 0.84, 0.07),
  c(0, 0, 0, 1)
)

records <- vector("list", n_subjects)

for (i in seq_len(n_subjects)) {
  subject_states <- integer(length(days))
  # Day 1 initial state sample
  subject_states[1] <- sample.int(4L, size = 1L, prob = p_initial)

  for (d in 2:length(days)) {
    prev_state <- subject_states[d - 1]
    subject_states[d] <- sample.int(4L, size = 1L, prob = P[prev_state, ])
  }

  records[[i]] <- data.frame(
    id = as.integer(i),
    step = as.integer(days),
    reals = factor(states[subject_states], levels = states, ordered = TRUE),
    stringsAsFactors = FALSE
  )
}

clinical_states_steps <- do.call(rbind, records)
# Ensure order by id then step
clinical_states_steps <- clinical_states_steps[
  order(clinical_states_steps$id, clinical_states_steps$step),
]
rownames(clinical_states_steps) <- NULL

usethis::use_data(clinical_states_steps, overwrite = TRUE)
