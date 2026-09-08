# Internal Static Prediction Distribution Preparation

Prepares exact prediction score distribution data (bins and operating
points) for static binary outcome models and populations. Reuses
production
[`prepare_performance_data()`](https://uriahf.github.io/rtichoke/reference/prepare_performance_data.md)
to ensure exact cutoff and PPCR alignment.

## Usage

``` r
prepare_probs_distribution_data(
  probs,
  reals,
  by = 0.01,
  stratified_by = "probability_threshold"
)
```

## Arguments

- probs:

  A list of numeric vectors of estimated probabilities (one vector per
  model or population).

- reals:

  A list of numeric vectors of binary outcome indicators (0 or 1).

- by:

  Increment of the threshold or PPCR evaluation sequence (default 0.01).

- stratified_by:

  Operating point stratification metric: `"probability_threshold"` or
  `"ppcr"`.

## Value

A named list with two tidy tibbles:

- bins:

  Exact aggregate score interval counts for each evaluation.

- operating_points:

  Selectable operating points with effective cutoffs and realized PPCR.
