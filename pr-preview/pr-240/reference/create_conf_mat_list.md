# Create a list of confusion matrices

Create a list of confusion matrices

## Usage

``` r
create_conf_mat_list(
  performance_table,
  stratified_by = "probability_threshold"
)
```

## Arguments

- stratified_by:

  Performance Metrics can be stratified by Probability Threshold or
  alternatively by Predicted Positives Condition Rate

## Examples

``` r
if (FALSE) { # \dontrun{

one_pop_one_model |>
  create_conf_mat_list()

one_pop_one_model_by_ppcr |>
  create_conf_mat_list()

multiple_models |>
  create_conf_mat_list()

multiple_models_by_ppcr |>
  create_conf_mat_list()

multiple_populations |>
  create_conf_mat_list()

multiple_populations_by_ppcr |>
  create_conf_mat_list()
} # }
```
