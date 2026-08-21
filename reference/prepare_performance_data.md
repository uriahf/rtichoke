# Prepare Performance Data

The prepare_performance_data function makes a Performance Data that is
made of different cutoffs. Each row represents a cutoff and each column
stands for a performance metric. It is possible to use this function for
more than one model in order to compare different models performance for
the same population. In this case the user should use a list that is
made of vectors of estimated probabilities, one for each model.

## Usage

``` r
prepare_performance_data(
  probs,
  reals,
  by = 0.01,
  stratified_by = "probability_threshold"
)
```

## Arguments

- probs:

  a list of vectors of estimated probabilities (one for each model or
  one for each population)

- reals:

  a list of vectors of binary outcomes (one for each population)

- by:

  number: increment of the sequence.

- stratified_by:

  Performance Metrics can be stratified by Probability Threshold or
  alternatively by Predicted Positives Condition Rate

## Details

Sometime instead of using a cutoff for the estimated probability it is
required to enforce a symmetry between the percentiles of the
probabilities, in medicine it is referred as "Risk Percentile" when the
outcome stands for something negative in essence such as a severe
disease or death: Let's say that we want to see the model performance
for the top 5% patients at risk for some well defined population, in
this case the user should change the parameter stratified_by from the
default "probability_threshold" to "predicted_positives" and the results
will be similar Performance Data, only this time each row will represent
some rounded percentile.

## Examples

``` r
# You can prepare Performance Data for one model

prepare_performance_data(
  probs = list(example_dat$estimated_probabilities),
  reals = list(example_dat$outcome)
)
#> # A tibble: 101 × 14
#>    probability_threshold    TP    TN    FN    FP sensitivity   FPR specificity
#>                    <dbl> <int> <int> <int> <int>       <dbl> <dbl>       <dbl>
#>  1                  0       50     0     0   100           1  1           0   
#>  2                  0.01    50    51     0    49           1  0.49        0.51
#>  3                  0.02    50    55     0    45           1  0.45        0.55
#>  4                  0.03    50    61     0    39           1  0.39        0.61
#>  5                  0.04    50    67     0    33           1  0.33        0.67
#>  6                  0.05    50    67     0    33           1  0.33        0.67
#>  7                  0.06    50    67     0    33           1  0.33        0.67
#>  8                  0.07    50    70     0    30           1  0.3         0.7 
#>  9                  0.08    50    70     0    30           1  0.3         0.7 
#> 10                  0.09    50    70     0    30           1  0.3         0.7 
#> # ℹ 91 more rows
#> # ℹ 6 more variables: PPV <dbl>, NPV <dbl>, lift <dbl>,
#> #   predicted_positives <int>, NB <dbl>, ppcr <dbl>

prepare_performance_data(
  probs = list(example_dat$estimated_probabilities),
  reals = list(example_dat$outcome),
  stratified_by = "ppcr"
)
#> # A tibble: 101 × 13
#>    probability_threshold  ppcr    TP    TN    FN    FP sensitivity   FPR
#>                    <dbl> <dbl> <int> <int> <int> <int>       <dbl> <dbl>
#>  1                 0.996  0        0   100    50     0        0        0
#>  2                 0.994  0.01     1   100    49     0        0.02     0
#>  3                 0.994  0.02     1   100    49     0        0.02     0
#>  4                 0.986  0.03     5   100    45     0        0.1      0
#>  5                 0.982  0.04     5   100    45     0        0.1      0
#>  6                 0.982  0.05     5   100    45     0        0.1      0
#>  7                 0.971  0.06     9   100    41     0        0.18     0
#>  8                 0.963  0.07    11   100    39     0        0.22     0
#>  9                 0.952  0.08    11   100    39     0        0.22     0
#> 10                 0.952  0.09    11   100    39     0        0.22     0
#> # ℹ 91 more rows
#> # ℹ 5 more variables: specificity <dbl>, PPV <dbl>, NPV <dbl>, lift <dbl>,
#> #   predicted_positives <int>

# Several Models

prepare_performance_data(
  probs = list(
    "First Model" = example_dat$estimated_probabilities,
    "Second Model" = example_dat$random_guess
  ),
  reals = list(example_dat$outcome)
)
#> # A tibble: 202 × 15
#>    model       probability_threshold    TP    TN    FN    FP sensitivity   FPR
#>    <chr>                       <dbl> <int> <int> <int> <int>       <dbl> <dbl>
#>  1 First Model                  0       50     0     0   100           1  1   
#>  2 First Model                  0.01    50    51     0    49           1  0.49
#>  3 First Model                  0.02    50    55     0    45           1  0.45
#>  4 First Model                  0.03    50    61     0    39           1  0.39
#>  5 First Model                  0.04    50    67     0    33           1  0.33
#>  6 First Model                  0.05    50    67     0    33           1  0.33
#>  7 First Model                  0.06    50    67     0    33           1  0.33
#>  8 First Model                  0.07    50    70     0    30           1  0.3 
#>  9 First Model                  0.08    50    70     0    30           1  0.3 
#> 10 First Model                  0.09    50    70     0    30           1  0.3 
#> # ℹ 192 more rows
#> # ℹ 7 more variables: specificity <dbl>, PPV <dbl>, NPV <dbl>, lift <dbl>,
#> #   predicted_positives <int>, NB <dbl>, ppcr <dbl>

prepare_performance_data(
  probs = list(
    "First Model" = example_dat$estimated_probabilities,
    "Second Model" = example_dat$random_guess
  ),
  reals = list(example_dat$outcome),
  stratified_by = "ppcr"
)
#> # A tibble: 202 × 14
#>    model   probability_threshold  ppcr    TP    TN    FN    FP sensitivity   FPR
#>    <chr>                   <dbl> <dbl> <int> <int> <int> <int>       <dbl> <dbl>
#>  1 First …                 0.996  0        0   100    50     0        0        0
#>  2 First …                 0.994  0.01     1   100    49     0        0.02     0
#>  3 First …                 0.994  0.02     1   100    49     0        0.02     0
#>  4 First …                 0.986  0.03     5   100    45     0        0.1      0
#>  5 First …                 0.982  0.04     5   100    45     0        0.1      0
#>  6 First …                 0.982  0.05     5   100    45     0        0.1      0
#>  7 First …                 0.971  0.06     9   100    41     0        0.18     0
#>  8 First …                 0.963  0.07    11   100    39     0        0.22     0
#>  9 First …                 0.952  0.08    11   100    39     0        0.22     0
#> 10 First …                 0.952  0.09    11   100    39     0        0.22     0
#> # ℹ 192 more rows
#> # ℹ 5 more variables: specificity <dbl>, PPV <dbl>, NPV <dbl>, lift <dbl>,
#> #   predicted_positives <int>


# Several Populations

prepare_performance_data(
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
  )
)
#> # A tibble: 202 × 15
#>    population probability_threshold    TP    TN    FN    FP sensitivity   FPR
#>    <chr>                      <dbl> <int> <int> <int> <int>       <dbl> <dbl>
#>  1 train                       0       28     0     0    68           1 1    
#>  2 train                       0.01    28    36     0    32           1 0.471
#>  3 train                       0.02    28    39     0    29           1 0.426
#>  4 train                       0.03    28    43     0    25           1 0.368
#>  5 train                       0.04    28    46     0    22           1 0.324
#>  6 train                       0.05    28    46     0    22           1 0.324
#>  7 train                       0.06    28    46     0    22           1 0.324
#>  8 train                       0.07    28    48     0    20           1 0.294
#>  9 train                       0.08    28    48     0    20           1 0.294
#> 10 train                       0.09    28    48     0    20           1 0.294
#> # ℹ 192 more rows
#> # ℹ 7 more variables: specificity <dbl>, PPV <dbl>, NPV <dbl>, lift <dbl>,
#> #   predicted_positives <int>, NB <dbl>, ppcr <dbl>

prepare_performance_data(
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
  stratified_by = "ppcr"
)
#> # A tibble: 202 × 14
#>    population probability_threshold  ppcr    TP    TN    FN    FP sensitivity
#>    <chr>                      <dbl> <dbl> <int> <int> <int> <int>       <dbl>
#>  1 train                      0.996  0        0    68    28     0      0     
#>  2 train                      0.994  0.01     1    68    27     0      0.0357
#>  3 train                      0.994  0.02     1    68    27     0      0.0357
#>  4 train                      0.990  0.03     3    68    25     0      0.107 
#>  5 train                      0.984  0.04     4    68    24     0      0.143 
#>  6 train                      0.974  0.05     5    68    23     0      0.179 
#>  7 train                      0.958  0.06     6    68    22     0      0.214 
#>  8 train                      0.932  0.07     7    68    21     0      0.25  
#>  9 train                      0.922  0.08     7    68    21     0      0.25  
#> 10 train                      0.922  0.09     7    68    21     0      0.25  
#> # ℹ 192 more rows
#> # ℹ 6 more variables: FPR <dbl>, specificity <dbl>, PPV <dbl>, NPV <dbl>,
#> #   lift <dbl>, predicted_positives <int>
```
