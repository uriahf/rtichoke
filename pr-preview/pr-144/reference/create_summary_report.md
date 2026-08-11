# Create a summary report

Create a summary report

## Usage

``` r
create_summary_report(
  probs,
  reals,
  interactive = TRUE,
  output_file = "summary_report.html",
  output_dir = getwd()
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

- output_file:

  The name of the output file

- output_dir:

  The output directory for the rendered `output_file`. This allows for a
  choice of an alternate directory to which the output file should be
  written (the default output directory of that of the input file). If a
  path is provided with a filename in `output_file` the directory
  specified here will take precedence. Please note that any directory
  path provided will create any necessary directories if they do not
  exist.

## Examples

``` r
if (FALSE) { # \dontrun{
create_summary_report(
  probs = list(example_dat$estimated_probabilities),
  reals = list(example_dat$outcome)
)

create_summary_report(
  probs = list(
    "First Model" = example_dat$estimated_probabilities,
    "Second Model" = example_dat$random_guess
  ),
  reals = list(example_dat$outcome)
)

create_summary_report(
  probs = list(
    "train" = example_dat %>%
      dplyr::filter(type_of_set == "train") %>%
      dplyr::pull(estimated_probabilities),
    "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
      dplyr::pull(estimated_probabilities)
  ),
  reals = list(
    "train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
      dplyr::pull(outcome),
    "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
      dplyr::pull(outcome)
  )
)
} # }
```
