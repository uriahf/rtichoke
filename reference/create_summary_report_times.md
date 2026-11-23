# Create a minimal time-dependent summary report

Creates a self-contained HTML report of outcome distributions over time
horizons. By default, displays raw observed outcome accounting (target
events, competing events, censored, and non-events).

## Usage

``` r
create_summary_report_times(
  reals,
  times,
  fixed_time_horizons,
  output_file = "summary_report_times.html",
  output_dir = getwd(),
  renderer = "browser",
  heuristics = FALSE,
  probs = NULL
)
```

## Arguments

- reals:

  A numeric vector (or list of numeric vectors) containing binary or
  multi-state outcome labels (0 = censored/no event, 1 = target event, 2
  = competing outcome).

- times:

  A numeric vector (or list of numeric vectors) containing follow-up
  times corresponding to `reals`. Target and competing events require
  positive times. Censoring may occur at time zero.

- fixed_time_horizons:

  A numeric vector of fixed evaluation time horizons at which to display
  state distributions.

- output_file:

  The name of the output HTML file. Defaults to
  `"summary_report_times.html"`.

- output_dir:

  The directory where the report HTML file will be saved. Defaults to
  the current working directory.

- renderer:

  Character string specifying the rendering engine. Currently, only
  `"browser"` is supported.

- heuristics:

  A logical value indicating whether to enable analysis heuristic
  controls (censoring and competing events). Defaults to `FALSE`.

- probs:

  Optional numeric matrix or named list of matrices, one per model in
  the same population. Rows match `reals`. Use numeric horizon values as
  column names to match the distinct positive `fixed_time_horizons` in
  any order. Unnamed columns follow ascending horizon order. Values are
  predicted target-event cumulative incidence probabilities.

## Details

With `heuristics = FALSE`, censored observations are displayed as their
own observed status and no censoring adjustment is applied.

With `heuristics = TRUE`, report controls select among eight
combinations of censoring and competing-event treatment, using
Kaplan-Meier or Aalen-Johansen estimates. Exclusions define a retained
population separately at each horizon; exclusion percentages use the
original population size. Empty retained populations have unavailable
estimates. Beyond observed follow-up support, displayed estimates retain
the estimator's last plateau. Censoring adjustment relies on the
relevant independent-censoring assumptions.

Supply `probs` to add target-event calibration. The calibration tabs use
Aalen-Johansen estimates within prediction quantile groups,
independently of the outcome-distribution controls.

## Examples

``` r
if (FALSE) { # \dontrun{
times <- c(24.1, 9.7, 49.9, 18.6, 34.8, 14.2, 39.2, 46.0, 31.5, 4.3)
reals <- c(1, 1, 1, 1, 0, 2, 1, 2, 0, 1)
fixed_time_horizons <- c(10, 20, 30, 40, 50)

create_summary_report_times(
  reals = reals,
  times = times,
  fixed_time_horizons = fixed_time_horizons,
  output_file = "time_summary_report.html"
)
} # }
```
