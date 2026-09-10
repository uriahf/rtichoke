# Creating rtichoke plotly curve

Creating rtichoke plotly curve

## Usage

``` r
create_plotly_curve(rtichoke_curve_list)
```

## Arguments

- rtichoke_curve_list:

  rtichoke curve list

## Examples

``` r
if (FALSE) { # \dontrun{

one_pop_one_model |>
  create_rtichoke_curve_list("roc") |>
  create_plotly_curve()

one_pop_one_model_by_ppcr |>
  create_rtichoke_curve_list("roc") |>
  create_plotly_curve()

multiple_models |>
  create_rtichoke_curve_list("roc") |>
  create_plotly_curve()

multiple_models_by_ppcr |>
  create_rtichoke_curve_list("roc") |>
  create_plotly_curve()

multiple_populations |>
  create_rtichoke_curve_list("roc") |>
  create_plotly_curve()

multiple_populations_by_ppcr |>
  create_rtichoke_curve_list("roc") |>
  create_plotly_curve()
} # }
```
