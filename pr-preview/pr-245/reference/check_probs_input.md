# Check probs input

Check probs input

## Usage

``` r
check_probs_input(probs)
```

## Arguments

- probs:

  a list of vectors of estimated probabilities (one for each model or
  one for each population)

## Examples

``` r
if (FALSE) { # \dontrun{
check_probs_input(example_dat$estimated_probabilities)

list(
  "train" = example_dat |>
    dplyr::filter(type_of_set == "train") |>
    dplyr::pull(estimated_probabilities),
  "test" = example_dat |> dplyr::filter(type_of_set == "test") |>
    dplyr::pull(estimated_probabilities)
) |>
  check_probs_input()

check_probs_input(c(example_dat$estimated_probabilities, -0.1))
check_probs_input(c(example_dat$estimated_probabilities, 1.1))

list(
  "train" = example_dat |>
    dplyr::filter(type_of_set == "train") |>
    dplyr::pull(estimated_probabilities),
  "test" = c(example_dat |> dplyr::filter(type_of_set == "test") |>
    dplyr::pull(estimated_probabilities), -0.2)
) |>
  check_probs_input()
} # }
```
