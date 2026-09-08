# Build evaluation semantic metadata

Derive stable internal model, population, and evaluation identities from
the existing `probs`/`reals` input shapes. This formalizes semantics
without changing public APIs or production plotting behavior.

## Usage

``` r
build_evaluation_metadata(probs, reals)
```

## Arguments

- probs:

  A list of model predictions.

- reals:

  A list of observed outcomes.

## Value

A data frame with one row per evaluation and `model`, `population`, and
`evaluation` columns.
