# Define limits for Calibration Curve

Define limits for Calibration Curve

## Usage

``` r
define_limits_for_calibration_plot(deciles_dat)
```

## Arguments

- deciles_dat:

  A data frame containing decile-level calibration data.

## Examples

``` r
if (FALSE) { # \dontrun{
make_deciles_dat(
  probs = example_dat$estimated_probabilities,
  real = example_dat$outcome
) |>
  define_limits_for_calibration_plot()
} # }
```
