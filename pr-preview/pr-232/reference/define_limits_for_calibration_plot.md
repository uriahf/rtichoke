# Define limits for Calibration Curve

Define limits for Calibration Curve

## Usage

``` r
define_limits_for_calibration_plot(calibration_bins_dat)
```

## Arguments

- calibration_bins_dat:

  A data frame containing bin-level calibration data.

## Examples

``` r
if (FALSE) { # \dontrun{
make_calibration_bins_dat(
  probs = example_dat$estimated_probabilities,
  reals = example_dat$outcome
) |>
  define_limits_for_calibration_plot()
} # }
```
