# Create an interactive outcome distribution visualization over time horizons

Prepares observed outcome counts (target events, competing events,
censored, and non-events) across time horizons and renders an
interactive Outcome Distribution component in the browser using
`rtichoke_viz`.

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

  A numeric vector (or list of numeric vectors) containing binary or
  multi-state outcome labels (0 = censored/no event, 1 = target event, 2
  = competing outcome).

- times:

  A numeric vector (or list of numeric vectors) containing follow-up
  times corresponding to `reals`.

- fixed_time_horizons:

  A numeric vector of fixed evaluation time horizons at which to display
  state distributions.

- renderer:

  Character string specifying the rendering engine. Currently, only
  `"browser"` is supported.

## Value

A browsable htmltools tag list rendering the Outcome Distribution chart.

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
#> Error: Browser rendering is not available for chart type: outcome_distribution
```
