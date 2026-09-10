# Prepare Prediction Distribution Data

Internal helper to prepare exact prediction distribution score intervals
(`bins`) and operating points (`operating_points`) for static binary
outcomes. Reuses production
[`prepare_performance_data()`](https://uriahf.github.io/rtichoke/reference/prepare_performance_data.md)
as the authoritative source for cutoff grids and metrics.

## Usage

``` r
prepare_probs_distribution_data(
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

## Value

A named list with two tidy tibbles:

- bins:

  Exact score intervals covering score space 0 to 1. Includes zero-score
  interval `[0, 0]` and right-closed intervals `(lower, upper]` aligned
  to effective cutoffs. Columns: `evaluation`, `model`, `population`,
  `lower`, `upper`, `include_lower`, `include_upper`, `n_positive`,
  `n_negative`.

- operating_points:

  Selectable operating points. Columns: `evaluation`, `model`,
  `population`, `type`, `value` (requested metric value), `cutoff`
  (effective score cutoff), `realized_ppcr` (actual predicted positives
  fraction).

## Details

This internal function is used to prepare static prediction distribution
data before rendering or contract serialization.

## Examples

``` r
# Single model with probability threshold stratification
res_single <- rtichoke:::prepare_probs_distribution_data(
  probs = list(example_dat$estimated_probabilities),
  reals = list(example_dat$outcome),
  by = 0.1
)
res_single$operating_points
#> # A tibble: 11 × 7
#>    evaluation model population type                  value cutoff realized_ppcr
#>    <chr>      <chr> <chr>      <chr>                 <dbl>  <dbl>         <dbl>
#>  1 model      model population probability_threshold   0      0           1    
#>  2 model      model population probability_threshold   0.1    0.1         0.533
#>  3 model      model population probability_threshold   0.2    0.2         0.433
#>  4 model      model population probability_threshold   0.3    0.3         0.393
#>  5 model      model population probability_threshold   0.4    0.4         0.347
#>  6 model      model population probability_threshold   0.5    0.5         0.307
#>  7 model      model population probability_threshold   0.6    0.6         0.3  
#>  8 model      model population probability_threshold   0.7    0.7         0.3  
#>  9 model      model population probability_threshold   0.8    0.8         0.273
#> 10 model      model population probability_threshold   0.9    0.9         0.147
#> 11 model      model population probability_threshold   1      1           0    
res_single$bins
#> # A tibble: 11 × 9
#>    evaluation model population lower upper include_lower include_upper
#>    <chr>      <chr> <chr>      <dbl> <dbl> <lgl>         <lgl>        
#>  1 model      model population   0     0   TRUE          TRUE         
#>  2 model      model population   0     0.1 FALSE         TRUE         
#>  3 model      model population   0.1   0.2 FALSE         TRUE         
#>  4 model      model population   0.2   0.3 FALSE         TRUE         
#>  5 model      model population   0.3   0.4 FALSE         TRUE         
#>  6 model      model population   0.4   0.5 FALSE         TRUE         
#>  7 model      model population   0.5   0.6 FALSE         TRUE         
#>  8 model      model population   0.6   0.7 FALSE         TRUE         
#>  9 model      model population   0.7   0.8 FALSE         TRUE         
#> 10 model      model population   0.8   0.9 FALSE         TRUE         
#> 11 model      model population   0.9   1   FALSE         TRUE         
#> # ℹ 2 more variables: n_positive <int>, n_negative <int>

# Multiple models sharing one outcome vector
res_multi <- rtichoke:::prepare_probs_distribution_data(
  probs = list(
    "Model A" = example_dat$estimated_probabilities,
    "Model B" = example_dat$random_guess
  ),
  reals = list(example_dat$outcome),
  by = 0.2
)
res_multi$operating_points
#> # A tibble: 12 × 7
#>    evaluation model   population type                 value cutoff realized_ppcr
#>    <chr>      <chr>   <chr>      <chr>                <dbl>  <dbl>         <dbl>
#>  1 Model A    Model A population probability_thresho…   0      0           1    
#>  2 Model A    Model A population probability_thresho…   0.2    0.2         0.433
#>  3 Model A    Model A population probability_thresho…   0.4    0.4         0.347
#>  4 Model A    Model A population probability_thresho…   0.6    0.6         0.3  
#>  5 Model A    Model A population probability_thresho…   0.8    0.8         0.273
#>  6 Model A    Model A population probability_thresho…   1      1           0    
#>  7 Model B    Model B population probability_thresho…   0      0           1    
#>  8 Model B    Model B population probability_thresho…   0.2    0.2         0.793
#>  9 Model B    Model B population probability_thresho…   0.4    0.4         0.633
#> 10 Model B    Model B population probability_thresho…   0.6    0.6         0.467
#> 11 Model B    Model B population probability_thresho…   0.8    0.8         0.24 
#> 12 Model B    Model B population probability_thresho…   1      1           0    

# PPCR stratification with tied scores showing requested vs realized PPCR
res_ppcr <- rtichoke:::prepare_probs_distribution_data(
  probs = list(c(0.1, 0.2, 0.5, 0.5, 0.8, 0.9)),
  reals = list(c(0, 0, 1, 0, 1, 1)),
  by = 0.5,
  stratified_by = "ppcr"
)
res_ppcr$operating_points
#> # A tibble: 3 × 7
#>   evaluation model population type  value cutoff realized_ppcr
#>   <chr>      <chr> <chr>      <chr> <dbl>  <dbl>         <dbl>
#> 1 model      model population ppcr    0      0.9         0    
#> 2 model      model population ppcr    0.5    0.5         0.333
#> 3 model      model population ppcr    1      0.1         1    
```
