# Assign Observations to Probability Quantile Strata

Assigns individual observations to empirical score-quantile strata
ordered from low to high predicted probability.

## Usage

``` r
assign_probability_quantile_strata(probs, by)
```

## Arguments

- probs:

  a list of vectors of estimated probabilities (one for each model or
  one for each population)

- by:

  number: increment of the sequence.

## Value

An ordered factor of stratum labels ordered low to high predicted
probability.

## Details

PROBABILITY-QUANTILE STRATUM vs PPCR OPERATING POINT: An
observation-level probability quantile stratum represents an individual
observation's location within the empirical prediction-score
distribution. In contrast, a PPCR operating point is a requested
population classification operating point used by
prepare_performance_data(). They are related through the score
distribution but are distinct concepts.
