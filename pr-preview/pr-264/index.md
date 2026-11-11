# rtichoke

**`rtichoke`** provides interactive and static visualizations for
evaluating binary prediction models. It allows data scientists and
clinical researchers to seamlessly analyze performance metrics,
including:

- **Discrimination:** Receiver Operating Characteristic (ROC),
  Precision-Recall (PR), Gains, and Lift curves.
- **Calibration:** Calibration curves with binned calibration (10
  rank-based/equal-frequency bins by default) and smooth (lowess)
  representations.
- **Clinical Utility:** Decision Curves and Interventions Avoided.
- **Performance Tables & Reports:** Interactive metric summaries and
  complete HTML reports.

For deep methodological intuition and articles, visit the [rtichoke
blog](https://rtichoke-blog.netlify.app/)! For package guides and API
reference, explore the [pkgdown documentation
site](https://uriahf.github.io/rtichoke/).

------------------------------------------------------------------------

## Installation

You can install `rtichoke` from GitHub:

``` r

# install.packages("devtools")
devtools::install_github("uriahf/rtichoke")
```

------------------------------------------------------------------------

## The `rtichoke` Mental Model

`rtichoke` is model-agnostic: it operates directly on **predicted
probabilities** (`probs`) and **observed binary outcomes** (`reals`).

The core workflow follows a simple conceptual pipeline:

``` math
\text{Predicted Probabilities} + \text{Binary Outcomes} \longrightarrow \text{Prepare Performance Data} \longrightarrow \text{Visualize or Summarize}
```

You can either pass lists of `probs` and `reals` directly to one-step
functions (`create_*_curve`) or pre-compute performance data using
[`prepare_performance_data()`](https://uriahf.github.io/rtichoke/reference/prepare_performance_data.md)
and pass it to plotting/table rendering functions (`plot_*_curve`,
`render_performance_table`).

------------------------------------------------------------------------

## Quickstart Examples

All examples use the built-in benchmark dataset
[`rtichoke::example_dat`](https://uriahf.github.io/rtichoke/reference/example_dat.md).

``` r

library(rtichoke)
```

### 1. Single Model

Pass predicted probabilities and observed binary outcomes as
single-element lists:

``` r

create_roc_curve(
  probs = list(example_dat$estimated_probabilities),
  reals = list(example_dat$outcome)
)
```

### 2. Model Comparison

Compare multiple models evaluated on the same population by passing a
named list of prediction vectors:

``` r

create_roc_curve(
  probs = list(
    "Good Model"   = example_dat$estimated_probabilities,
    "Bad Model"    = example_dat$bad_model,
    "Random Guess" = example_dat$random_guess
  ),
  reals = list(example_dat$outcome)
)
```

### 3. Population Comparison (e.g., Train / Test Split)

Compare performance across distinct cohorts (such as Train vs. Test
sets):

``` r

train_df <- example_dat[example_dat$type_of_set == "train", ]
test_df  <- example_dat[example_dat$type_of_set == "test", ]

create_roc_curve(
  probs = list(
    "Train" = train_df$estimated_probabilities,
    "Test"  = test_df$estimated_probabilities
  ),
  reals = list(
    "Train" = train_df$outcome,
    "Test"  = test_df$outcome
  )
)
```

------------------------------------------------------------------------

## Two-Step Workflow with Prepared Performance Data

For iterative plotting or performance tables, prepare performance data
first:

``` r

perf_data <- prepare_performance_data(
  probs = list(
    "Good Model" = example_dat$estimated_probabilities,
    "Bad Model"  = example_dat$bad_model
  ),
  reals = list(example_dat$outcome)
)

# Plot ROC curve from prepared data
plot_roc_curve(perf_data)

# Render interactive performance table
render_performance_table(perf_data)
```

------------------------------------------------------------------------

## Comprehensive Summary Report

Generate a single self-contained HTML report containing all supported
visualizations and performance tables:

``` r

create_summary_report(
  probs = list("Primary Model" = example_dat$estimated_probabilities),
  reals = list(example_dat$outcome),
  file_path = "model_performance_report.html"
)
```

------------------------------------------------------------------------

## Documentation & Resources

- **[Package Website & Guides](https://uriahf.github.io/rtichoke/):**
  Comprehensive task-oriented tutorials (Discrimination, Calibration,
  Clinical Utility, Performance Tables).
- **[Recipes &
  Workflows](https://uriahf.github.io/rtichoke/articles/recipes-and-workflows.html):**
  Quick copy-paste cheatsheet for common evaluation tasks.
- **[rtichoke Blog](https://rtichoke-blog.netlify.app/):** Deep
  methodological insights, statistical derivations, and background
  theory.

------------------------------------------------------------------------

## Getting Help

If you encounter a bug or have a feature request, please file an issue
on [GitHub Issues](https://github.com/uriahf/rtichoke/issues) with a
reproducible example.
