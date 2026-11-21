# Plot rtichoke curve

Plot rtichoke curve

## Usage

``` r
plot_rtichoke_curve(
  performance_data,
  curve,
  color_values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#07004D", "#E6AB02",
    "#FE5F55", "#54494B", "#006E90", "#BC96E6", "#52050A", "#1F271B", "#BE7C4D",
    "#63768D", "#08A045", "#320A28", "#82FF9E", "#2176FF", "#D1603D", "#585123"),
  min_p_threshold = 0,
  max_p_threshold = 1,
  size = NULL
)
```

## Arguments

- performance_data:

  an rtichoke Performance Data

- color_values:

  color palette

- min_p_threshold:

  The minimum Probability Threshold value to be displayed

- max_p_threshold:

  The maximum Probability Threshold value to be displayed

- size:

  the size of the curve

## Examples

``` r
if (FALSE) { # \dontrun{

one_pop_one_model |>
  plot_rtichoke_curve("roc")

one_pop_one_model_by_ppcr |>
  plot_rtichoke_curve("roc")

multiple_models |>
  plot_rtichoke_curve("roc")

multiple_models_by_ppcr |>
  plot_rtichoke_curve("roc")

multiple_populations |>
  plot_rtichoke_curve("roc")

multiple_populations_by_ppcr |>
  plot_rtichoke_curve("roc")
} # }
```
