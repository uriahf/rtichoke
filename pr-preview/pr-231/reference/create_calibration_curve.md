# Create a Calibration Curve

Create a Calibration Curve

## Usage

``` r
create_calibration_curve(
  probs,
  reals,
  interactive = TRUE,
  color_values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#07004D", "#E6AB02",
    "#FE5F55", "#54494B", "#006E90", "#BC96E6", "#52050A", "#1F271B", "#BE7C4D",
    "#63768D", "#08A045", "#320A28", "#82FF9E", "#2176FF", "#D1603D", "#585123"),
  type = "discrete",
  n_bins = 10,
  size = NULL
)
```

## Arguments

- probs:

  a list of vectors of estimated probabilities (one for each model or
  one for each population)

- reals:

  a list of vectors of binary outcomes (one for each population)

- interactive:

  whether the plot should be interactive plots

- color_values:

  color palette

- type:

  discrete or smooth

- n_bins:

  Number of calibration bins for binned calibration (default 10).

- size:

  the size of the curve

## Examples

``` r
if (FALSE) { # \dontrun{

create_calibration_curve(
  probs = list(example_dat$estimated_probabilities),
  reals = list(example_dat$outcome), type = "discrete"
)


create_calibration_curve(
  probs = list(example_dat$estimated_probabilities),
  reals = list(example_dat$outcome), type = "smooth"
)

# Several Models

create_calibration_curve(
  probs = list(
    "First Model" = example_dat$estimated_probabilities,
    "Second Model" = example_dat$random_guess
  ),
  reals = list(example_dat$outcome),
  type = "discrete"
)


create_calibration_curve(
  probs = list(
    "First Model" = example_dat$estimated_probabilities,
    "Second Model" = example_dat$random_guess
  ),
  reals = list(example_dat$outcome),
  type = "smooth"
)


# Several Populations

create_calibration_curve(
  probs = list(
    "train" = example_dat |>
      dplyr::filter(type_of_set == "train") |>
      dplyr::pull(estimated_probabilities),
    "test" = example_dat |> dplyr::filter(type_of_set == "test") |>
      dplyr::pull(estimated_probabilities)
  ),
  reals = list(
    "train" = example_dat |> dplyr::filter(type_of_set == "train") |>
      dplyr::pull(outcome),
    "test" = example_dat |> dplyr::filter(type_of_set == "test") |>
      dplyr::pull(outcome)
  ),
  type = "discrete"
)


create_calibration_curve(
  probs = list(
    "train" = example_dat |>
      dplyr::filter(type_of_set == "train") |>
      dplyr::pull(estimated_probabilities),
    "test" = example_dat |> dplyr::filter(type_of_set == "test") |>
      dplyr::pull(estimated_probabilities)
  ),
  reals = list(
    "train" = example_dat |> dplyr::filter(type_of_set == "train") |>
      dplyr::pull(outcome),
    "test" = example_dat |> dplyr::filter(type_of_set == "test") |>
      dplyr::pull(outcome)
  ),
  type = "smooth"
)
} # }
```
