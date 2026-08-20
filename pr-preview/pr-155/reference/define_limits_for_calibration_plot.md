# Define limits for Calibration Curve

Define limits for Calibration Curve

## Usage

``` r
define_limits_for_calibration_plot(deciles_dat)
```

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
