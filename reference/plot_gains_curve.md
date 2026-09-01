# Gains Curve from Performance Data

Plot a Gains Curve

## Usage

``` r
plot_gains_curve(
  performance_data,
  chosen_threshold = NA,
  interactive = TRUE,
  color_values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#07004D", "#E6AB02",
    "#FE5F55", "#54494B", "#006E90", "#BC96E6", "#52050A", "#1F271B", "#BE7C4D",
    "#63768D", "#08A045", "#320A28", "#82FF9E", "#2176FF", "#D1603D", "#585123"),
  size = NULL,
  renderer = "default",
  evaluation_metadata = NULL,
  stratified_by = "probability_threshold"
)
```

## Arguments

- performance_data:

  an rtichoke Performance Data

- chosen_threshold:

  a chosen threshold to display (for non-interactive)

- interactive:

  whether the plot should be interactive plots

- color_values:

  color palette

- size:

  the size of the curve

- renderer:

  rendering backend. `"default"` preserves the existing `interactive`
  behavior; alternatives are `"ggplot2"`, `"plotly"`, and `"browser"`.

- evaluation_metadata:

  explicit semantic evaluation metadata required when
  `renderer = "browser"`. It is supplied automatically by
  [`create_roc_curve()`](https://uriahf.github.io/rtichoke/reference/create_roc_curve.md).

- stratified_by:

  Performance Metrics can be stratified by Probability Threshold or
  alternatively by Predicted Positives Condition Rate

## Examples

``` r
if (FALSE) { # \dontrun{

one_pop_one_model |>
  plot_gains_curve()

one_pop_one_model_by_ppcr |>
  plot_gains_curve()

multiple_models |>
  plot_gains_curve()

multiple_models_by_ppcr |>
  plot_gains_curve()

multiple_populations |>
  plot_gains_curve()

multiple_populations_by_ppcr |>
  plot_gains_curve()
} # }
```
