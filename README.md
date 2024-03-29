
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rtichoke

<!-- badges: start -->

[![R-CMD-check](https://github.com/uriahf/rtichoke/workflows/R-CMD-check/badge.svg)](https://github.com/uriahf/rtichoke/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/rtichoke)](https://CRAN.R-project.org/package=rtichoke)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/uriahf/rtichoke/branch/main/graph/badge.svg)](https://codecov.io/gh/uriahf/rtichoke?branch=main)
<!-- badges: end -->

For some reproducible examples please visit [rtichoke
blog](https://rtichoke-blog.netlify.app/)!

## Installation

<!-- You can install the released version of rtichoke from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("rtichoke") -->
<!-- ``` -->

You can install rtichoke from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("uriahf/rtichoke")
```

<!-- TODO change to good model, bad model and random guess -->

## Overview:

- `rtichoke` is designed to help analysts with exploration of
  performance metrics with a binary outcome. In order to do so it uses
  interactive visualization.

## Getting started

### Predictions and Outcomes as input

<!-- - ` is designed for interactive visualization for performance metrics of prediction models with a binary outcome. -->
<!-- -- -->
<!-- It is agnostic in a sense that it does not care about the models that were used to develop predictions. It takes only the estimated probabilities of an outcome  -->

In order to use `rtichoke` you need to have

- `probs`: Estimated Probabilities as predictions.
- `reals`: Binary Outcomes.

There are 3 different cases and for each one of them rtichoke requires a
different kind of input:

### Singel Model:

The user is required to provide a list with one vector for the
predictions and a list with one vector for the outcomes.

<!-- ```{r echo=FALSE} -->
<!-- library(rtichoke) -->
<!-- set.seed(123) -->
<!-- predictions_and_outcomes <- tibble::tibble( -->
<!--   probs = example_dat$bad_model, -->
<!--   real = as.numeric( -->
<!--     rtichoke::example_dat$outcome) -->
<!--   )  -->
<!-- predictions_and_outcomes %>%  -->
<!--   dplyr::sample_n(replace = FALSE, size = 6) %>%  -->
<!--   gt::gt() %>%  -->
<!--   gt::cols_align(align = "center") %>%  -->
<!--   gt::fmt_number(columns = probs, -->
<!--                  decimals  = 2)  -->
<!-- ``` -->

``` r
create_roc_curve(
  probs = list(example_dat$bad_model),
  reals = list(example_dat$outcome)
)
```

### Models Comparison:

**Why?** In order to compare performance for several different models
for the same population.

**How?** The user is required to provide a list with one vector of
predictions for each model and a list with one vector for the outcome of
the population.

<!-- ```{r echo=FALSE} -->
<!-- library(rtichoke) -->
<!-- set.seed(42) -->
<!-- predictions_and_outcomes <- tibble::tibble( -->
<!--     "Good Model" = example_dat$estimated_probabilities, -->
<!--     "Bad Model" = example_dat$bad_model, -->
<!--     "Random Guess" = example_dat$random_guess, -->
<!--   real = as.numeric(rtichoke::example_dat$outcome))  -->
<!-- predictions_and_outcomes %>%  -->
<!--   dplyr::sample_n(replace = FALSE, size = 6) %>%  -->
<!--   gt::gt() %>%  -->
<!--   gt::cols_align(align = "center") %>%  -->
<!--   gt::fmt_number(columns = 1:3, -->
<!--                  decimals  = 2) -->
<!-- ``` -->

``` r
create_roc_curve(
  probs = list(
    "Good Model" = example_dat$estimated_probabilities,
    "Bad Model" = example_dat$bad_model,
    "Random Guess" = example_dat$random_guess
  ),
  reals = list(rtichoke::example_dat$outcome)
)
```

### Several Populations

*Why?* In order to compare performance for different populations, like
in Train / Test split or in order to check the fairness of the
algorithms.

*How?* The user is required to provide a list with one vector of
predictions for each population and a list with one vector of outcomes
for each population.

<!-- ```{r echo=FALSE} -->
<!-- library(rtichoke) -->
<!-- set.seed(42) -->
<!-- predictions_and_outcomes_train <- tibble::tibble( -->
<!--     "probs" = example_dat %>% -->
<!--       dplyr::filter(type_of_set == "train") %>% -->
<!--       dplyr::pull(estimated_probabilities), -->
<!--   real = example_dat %>% dplyr::filter(type_of_set == "train") %>% -->
<!--       dplyr::pull(outcome) %>%  -->
<!--     as.numeric())   -->
<!-- predictions_and_outcomes_test <- tibble::tibble( -->
<!--     "probs" = example_dat %>% -->
<!--       dplyr::filter(type_of_set == "test") %>% -->
<!--       dplyr::pull(estimated_probabilities), -->
<!--   real = example_dat %>% dplyr::filter(type_of_set == "test") %>% -->
<!--       dplyr::pull(outcome) %>%  -->
<!--     as.numeric())   -->
<!-- predictions_and_outcomes_train %>%  -->
<!--   dplyr::sample_n(replace = FALSE, size = 6) %>%  -->
<!--   gt::gt() %>% -->
<!--   gt::tab_header( -->
<!--     title = gt::md("**Train Set**") -->
<!--   )  %>%  -->
<!--   gt::fmt_number(columns = probs, -->
<!--                  decimals  = 2) -->
<!-- predictions_and_outcomes_test %>%  -->
<!--   dplyr::sample_n(replace = FALSE, size = 6) %>%  -->
<!--   gt::gt() %>% -->
<!--   gt::tab_header( -->
<!--     title = gt::md("**Test Set**") -->
<!--   )%>%  -->
<!--   gt::fmt_number(columns = probs, -->
<!--                  decimals  = 2) -->
<!-- ``` -->

``` r
create_roc_curve(
  probs = list(
    "Train" = example_dat %>%
      dplyr::filter(type_of_set == "train") %>%
      dplyr::pull(estimated_probabilities),
    "Test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
      dplyr::pull(estimated_probabilities)
  ),
  reals = list(
    "Train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
      dplyr::pull(outcome),
    "Test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
      dplyr::pull(outcome)
  )
)
```

## Performance Data as input

For some outputs in rtichoke you can alternatively prepare a performance
data and use it as an input: instead of `create_*_curve` use
`plot_*_curve` and instead of `create_performance_table` use
`render_performance_table`:

``` r
one_pop_one_model_as_a_vector %>%
  plot_roc_curve()
```

### Summary Report

In order to get all the supported outputs of rtichoke in one html file
the user can call `create_summary_report()`.

### Getting help

If you encounter a bug please fill an issue with a minimal reproducible
example, it will be easier for me to help you and it might help others
in the future. Alternatively you are welcome to contact me personally:
<ufinkel@gmail.com>
