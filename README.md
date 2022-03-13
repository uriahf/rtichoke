
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

-   `probs`: Estimated Probabilities as predictions (0 ≤ *p̂* ≤ 1).
-   `real`: Binary Outcomes (*y* ∈ {1, 0}).

There are 3 different cases and for each one of them rtichoke requires a
different kind of input:

## One model for One Population:

### (probs = 🔀, real = 😁)

*Why?* In order to examine performance of a single model 🔀 for one
population 😁.

*How?* The user is required to provide one vector for the model’s
predictions and one vector for the outcome of the population.

(table with emojis)

For example a Logistic Regression 🔀:

! (We will use median imputation for the age variable, it is not a good
practice for imputation but it is easy to understand and it allows us to
use the `Age` variable.)

``` r
library(rtichoke)
library(titanic)

data("titanic_train")

titanic_train$Age[is.na(titanic_train$Age)] <- median(titanic_train$Age, na.rm = TRUE)

logistic_regression <- glm(Survived ~ Age + Sex, family = "binomial", data = titanic_train)

logistic_preds_train <- predict(logistic_regression, type = "response")
survived_train <- titanic_train$Survived
```

(gif)

``` r
create_roc_curve(probs = logistic_preds_train,
                 real = survived_train,
                 interactive = TRUE)
```

<!-- TODO: Force cur ve to be squared  -->

Alternatively the vector of the model predictions can be in a list:

## Several Models for One Population

### (probs = list(“logistic regression” = 🔀, “Decision Tree” = 🌲, “Random Guess” = 🙈"),

### real = 😁)

*Why?* In order to compare performance for three different models for
the same population.

*How?* The user is required to provide one vector of predictions for
each model in a list and one vector for the outcome of the population.

This time we will use Decision Tree Model 🌲

And just for fun we will add A random guess 🙈

## Several Populations 🏋📝

### (probs = list(“Train” = 🏋️🌲, “Test” = 📝🌲),

### real = list(“Train” = 🏋🧑, “Test” = 🏋🧔))

*Why?* In order to compare performance for different populations, like
in Train / Test split or in order to check the fairness of the
algorithms.

*How?* The user is required to provide one vector of predictions for
each population in a list and one vector for each outcome of the
population in another list.

(code) probs = list(“Train” = …, “Test” = …), real = list(“Train” = …,
“Test” = …) (gif)

<!-- TODO add another test and return error when NULL -->
<!-- Why not working? :() -->

``` r
data(titanic_test)

tree_preds_test <- predict(tree_model, newdata = titanic_test)

create_roc_curve(probs = list(
  "Train" = tree_preds_train,
  "Test" = tree_preds_test),
  real = list(
  "Train" = titanic_train$Survived,
  "Test" = sample(titanic_train$Survived)), 
  interactive = TRUE)

create_roc_curve(
    probs = list(
        "train" = example_dat %>%
            dplyr::filter(type_of_set == "train") %>%
            dplyr::pull(estimated_probabilities),
        "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
            dplyr::pull(estimated_probabilities)
    ),
    real = list(
        "train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
            dplyr::pull(outcome),
        "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
            dplyr::pull(outcome)
    ),
    interactive = TRUE   
)
```

### Performance Data

For roc, lift, precision recall, gains and decision curves and for
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
