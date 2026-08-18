# Performance Table

Create a Performance Table

## Usage

``` r
render_performance_table(
  performance_data,
  chosen_threshold = NA,
  output_type = "reactable",
  color_values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#07004D", "#E6AB02",
    "#FE5F55", "#54494B", "#006E90", "#BC96E6", "#52050A", "#1F271B", "#BE7C4D",
    "#63768D", "#08A045", "#320A28", "#82FF9E", "#2176FF", "#D1603D", "#585123")
)
```

## Arguments

- performance_data:

  an rtichoke Performance Data

- chosen_threshold:

  a chosen threshold to display (for non-interactive)

- output_type:

  the type of the output table

- color_values:

  color palette

## Examples

``` r
if (FALSE) { # \dontrun{

one_pop_one_model %>%
  render_performance_table()

one_pop_one_model_by_ppcr %>%
  render_performance_table()

multiple_models %>%
  render_performance_table()

multiple_models_by_ppcr %>%
  render_performance_table()

multiple_populations %>%
  render_performance_table()

multiple_populations_by_ppcr %>%
  render_performance_table()
} # }
```
