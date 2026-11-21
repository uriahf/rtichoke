# Synthetic Longitudinal Clinical Outcome Data

A synthetic dataset containing 27 daily observations for 600 subjects
across four ordinal clinical outcome states ("Home", "Hospital",
"Vent/ARDS", "Dead").

## Usage

``` r
clinical_states_steps
```

## Format

A data frame with 16,200 rows and 3 variables:

- id:

  Subject identifier (integer 1-600)

- step:

  Discrete step / day horizon (integer 1-27)

- reals:

  Observed clinical state (ordered factor with levels "Home" \<
  "Hospital" \< "Vent/ARDS" \< "Dead")

## Details

The dataset was generated using a first-order Markov simulation with
seed 2026, initial probabilities (0, 0.85, 0.15, 0), and transition
matrix
`rbind(c(.97, .03, 0, 0), c(.10, .82, .06, .02), c(.01, .08, .84, .07), c(0, 0, 0, 1))`
where "Dead" is an absorbing state.

This dataset draws inspiration from Frank Harrell's work on longitudinal
ordinal Markov and state-occupancy models and VIOLET 2 trial examples:
<https://hbiostat.org/r/hmisc/markov/sim#estimating-efficiency-from-the-study>.
It is independently simulated and is NOT Frank Harrell's original trial
data, fitted model, or an endorsed implementation.
