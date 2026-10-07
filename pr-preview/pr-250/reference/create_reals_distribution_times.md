# Reals Distribution Over Time

Summarize observed outcomes over fixed time horizons as an outcome
distribution spec.

## Usage

``` r
create_reals_distribution_times(
  reals,
  times,
  fixed_time_horizons,
  renderer = "browser"
)
```

## Arguments

- reals:

  A numeric vector or list of numeric vectors containing outcome labels
  (0, 1, or 2).

- times:

  A numeric vector or list of numeric vectors containing follow-up
  times.

- fixed_time_horizons:

  A numeric vector of evaluation time horizons.

- renderer:

  Rendering backend. Only `"browser"` is supported.

## Value

A browsable HTML tag object when `renderer = "browser"`.

## Examples

``` r
times <- c(24.1, 9.7, 49.9, 18.6, 34.8, 14.2, 39.2, 46.0, 31.5, 4.3)
reals <- c(1, 1, 1, 1, 0, 2, 1, 2, 0, 1)
fixed_time_horizons <- c(10, 20, 30, 40, 50)

create_reals_distribution_times(
  reals = reals,
  times = times,
  fixed_time_horizons = fixed_time_horizons,
  renderer = "browser"
)
#> Error: Browser rendering for 'outcome_distribution' is blocked on a vendored rtichoke_viz update exporting renderOutcomeDistribution.
```
