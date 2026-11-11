# Getting Started with rtichoke

`rtichoke` is an R package for interactive and static evaluation of
binary prediction models. It allows analysts and clinical researchers to
inspect model performance across multiple complementary dimensions:

- **Discrimination:** ROC, Precision-Recall, Gains, and Lift curves.
- **Calibration:** Binned calibration (10 rank-based/equal-frequency
  bins by default) and smooth (lowess) calibration curves.
- **Clinical Utility:** Decision Curves and Interventions Avoided.
- **Performance Tables & Reports:** Interactive tables and automated
  HTML reports.

For methodological intuition, mathematical derivations, and statistical
theory, visit the [rtichoke blog](https://rtichoke-blog.netlify.app/).

------------------------------------------------------------------------

## The `rtichoke` Input Structure

`rtichoke` is model-agnostic. It does not fit models or require specific
model objects (e.g., `glm` or `randomForest`). Instead, it works
directly with vectors of **predicted probabilities** (`probs`) and
**observed binary outcomes** (`reals`).

There are three common input scenarios:

### 1. Single Model Evaluation

Pass a single vector of predicted probabilities and a single vector of
binary outcomes wrapped in lists:

``` r

library(rtichoke)

create_roc_curve(
  probs = list(example_dat$estimated_probabilities),
  reals = list(example_dat$outcome)
)
```

### 2. Comparing Multiple Models

When comparing several candidate models evaluated on the **same
population**, supply a named list of prediction vectors and a single
outcome vector:

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

### 3. Comparing Across Populations (e.g., Train / Test Split)

When evaluating one model across distinct populations (such as training
vs. validation sets or demographic subgroups), supply named lists for
both `probs` and `reals`:

``` r

train_df <- example_dat[example_dat$type_of_set == "train", ]
test_df  <- example_dat[example_dat$type_of_set == "test", ]

create_roc_curve(
  probs = list(
    "Train Set" = train_df$estimated_probabilities,
    "Test Set"  = test_df$estimated_probabilities
  ),
  reals = list(
    "Train Set" = train_df$outcome,
    "Test Set"  = test_df$outcome
  )
)
```

------------------------------------------------------------------------

## One-Step vs. Two-Step Workflow

`rtichoke` provides two equivalent ways to produce plots and tables:

### 1. Direct One-Step Functions (`create_*`)

Functions prefixed with `create_` (e.g.,
[`create_roc_curve()`](https://uriahf.github.io/rtichoke/reference/create_roc_curve.md),
[`create_decision_curve()`](https://uriahf.github.io/rtichoke/reference/create_decision_curve.md),
[`create_performance_table()`](https://uriahf.github.io/rtichoke/reference/create_performance_table.md))
accept raw `probs` and `reals` directly and compute performance metrics
on the fly.

### 2. Two-Step Workflow (`prepare_performance_data` + `plot_*` / `render_*`)

For larger workflows where you want to render multiple curves or tables
without re-computing metrics:

``` r

# Step 1: Prepare performance data object
perf_data <- prepare_performance_data(
  probs = list(
    "Good Model" = example_dat$estimated_probabilities,
    "Bad Model"  = example_dat$bad_model
  ),
  reals = list(example_dat$outcome)
)

# Step 2: Render visualizations or tables from prepared data
plot_roc_curve(perf_data)
plot_precision_recall_curve(perf_data)
plot_decision_curve(perf_data)
render_performance_table(perf_data)
```

------------------------------------------------------------------------

## Automated HTML Summary Reports

To generate a complete, self-contained interactive report combining all
evaluation dimensions into an HTML file:

``` r

create_summary_report(
  probs = list("Primary Model" = example_dat$estimated_probabilities),
  reals = list(example_dat$outcome),
  file_path = "model_evaluation_report.html"
)
```

------------------------------------------------------------------------

## Next Steps & Guides

Explore the detailed guides for each evaluation domain:

- [Discrimination
  Guide](https://uriahf.github.io/rtichoke/articles/discrimination.md) —
  ROC, PR, Gains, and Lift curves.
- [Calibration
  Guide](https://uriahf.github.io/rtichoke/articles/calibration.md) —
  Binned and smooth calibration analysis.
- [Clinical Utility
  Guide](https://uriahf.github.io/rtichoke/articles/clinical-utility.md)
  — Decision Curves and Interventions Avoided.
- [Performance Tables
  Guide](https://uriahf.github.io/rtichoke/articles/performance-tables.md)
  — Interactive tables and reports.
- [Recipes &
  Workflows](https://uriahf.github.io/rtichoke/articles/recipes-and-workflows.md)
  — Copy-paste cheatsheet for common workflows.
