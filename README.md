
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

The goal of rtichoke is to …

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

## Overview:

rtichoke is a package designed for interactive visualization for
performance metrics of prediction models with a binary outcome.

## Getting started

### Predictions and Outcomes

In order to use rtichoke you need to have

1.  `probs`: Estimated Probabilities as predictions (0 ≤ *p̂* ≤ 1).
2.  `real`: Binary Outcomes (*y* ∈ {1, 0}).

There are 3 different cases and for each one of them rtichoke requires a
different kind of input:

1.  One model for One Population: 🔀

The user is required to provide one vector for the model predictions and
one vector for the outcome of the population.

``` r
library(rtichoke)

create_roc_curve(
  probs = example_dat$estimated_probabilities,
  real = example_dat$outcome
)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />
(gif)

Alternatively the vector of the model predictions can be in a list:

``` r
create_roc_curve(
  probs = list("Logistic Regression" = example_dat$estimated_probabilities),
  real = example_dat$outcome
)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

### Make it Interactive

<!-- While you can use the non-interactive version of rtichoke curves, there is nothing special about  -->

(gif roc, lift, precision recall, gains, NB curve for
non-interactive-interactive )

1.  Several models for One Population 🔀 🌲 🙈

The user is required to provide one vector of predictions for each model
in a list and one vector for the outcome of the population.

(code) probs = list(“Model 1” = …, “Model 2” = ..), real = c() (gif)

1.  Several Populations 👩👨

The user is required to provide one vector of predictions for each
population in a list and one vector for each outcome of the population
in another list.

(code) probs = list(“Train” = …, “Test” = …), real = list(“Train” = …,
“Test” = …) (gif)

### Performance Data

For roc, lift, precision recall, gains, decision curves and for
performance table you can alternatively prepare a performance data and
use it as an input, but instead of create\_**curve use plot**\_curve and
instead of create\_performance\_table use render\_performance\_table:
(gif render\_roc\_curve, prepare\_performance\_table, plot\_roc\_curve)
(table of inputs)

### Summary Report

In order to get all the supported outputs of rtichoke in one html file
the user can call create\_summary\_report().

### Getting help

If you encounter a bug please fill an issue with a minimal reproducible
example, it will be easier for me to help you and it might help others
in the future. Alternatively you are welcome to contact me personally:
<ufinkel@gmail.com>
