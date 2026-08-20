# Check real input

Check real input

## Usage

``` r
check_real_input(real)
```

## Examples

``` r
if (FALSE) { # \dontrun{
check_real_input(example_dat$outcome)

list(
  "train" = example_dat %>%
    dplyr::filter(type_of_set == "train") %>%
    dplyr::pull(outcome),
  "test" = example_dat %>%
    dplyr::filter(type_of_set == "test") %>%
    dplyr::pull(outcome)
) %>%
  check_real_input()

check_real_input(c(example_dat$outcome, -0.1))
check_real_input(c(example_dat$outcome, 1.1))

list(
  "train" = example_dat %>%
    dplyr::filter(type_of_set == "train") %>%
    dplyr::pull(outcome),
  "test" = c(example_dat %>% dplyr::filter(type_of_set == "test") %>%
    dplyr::pull(outcome), -0.2)
) %>%
  check_real_input()
} # }
```
