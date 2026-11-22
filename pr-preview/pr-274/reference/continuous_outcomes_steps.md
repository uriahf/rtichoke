# Continuous Longitudinal Outcomes Example

Deterministic six-subject panel with measurements at steps 0, 1 and 3.
The panel has 17 subject-step rows: one absent row at step 3 and two
explicit missing (`NA`) outcomes.

## Usage

``` r
continuous_outcomes_steps
```

## Format

A data frame with 17 rows and 3 variables:

- id:

  Subject identifier (1 to 6).

- step:

  Observed longitudinal step (0, 1 or 3).

- reals:

  Observed continuous outcome, possibly missing.

## Source

Generated in `data-raw/continuous_outcomes_steps.R` using
`usethis::use_data()`.
