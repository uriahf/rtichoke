# ROC Curve from Performance Data

Plot a ROC Curve

## Usage

``` r
plot_roc_curve(
  performance_data,
  chosen_threshold = NA,
  interactive = TRUE,
  color_values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#07004D", "#E6AB02",
    "#FE5F55", "#54494B", "#006E90", "#BC96E6", "#52050A", "#1F271B", "#BE7C4D",
    "#63768D", "#08A045", "#320A28", "#82FF9E", "#2176FF", "#D1603D", "#585123"),
  title_included = FALSE,
  size = NULL
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

- title_included:

  add title to the curve

- size:

  the size of the curve

## Examples

``` r
if (FALSE) { # \dontrun{

one_pop_one_model %>%
  plot_roc_curve()

one_pop_one_model_by_ppcr %>%
  plot_roc_curve()

multiple_models %>%
  plot_roc_curve()

multiple_models_by_ppcr %>%
  plot_roc_curve()

multiple_populations %>%
  plot_roc_curve()

multiple_populations_by_ppcr %>%
  plot_roc_curve()
} # }
```
