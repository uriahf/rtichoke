# Performance Tables & Summary Reports

In addition to interactive curves, `rtichoke` generates structured,
interactive performance tables and comprehensive HTML summary reports.

------------------------------------------------------------------------

## Interactive Performance Tables

`rtichoke` performance tables summarize threshold-specific confusion
matrix metrics (Sensitivity, Specificity, PPV, NPV, FPR, FNR, Accuracy,
Net Benefit) across probability cutoffs.

### One-Step Table Creation (`create_performance_table`)

``` r

library(rtichoke)

create_performance_table(
  probs = list(
    "Good Model" = example_dat$estimated_probabilities,
    "Bad Model"  = example_dat$bad_model
  ),
  reals = list(example_dat$outcome)
)
```

### Rendering Table from Prepared Data (`render_performance_table`)

When performance data has already been prepared with
[`prepare_performance_data()`](https://uriahf.github.io/rtichoke/reference/prepare_performance_data.md),
use
[`render_performance_table()`](https://uriahf.github.io/rtichoke/reference/render_performance_table.md):

``` r

perf_data <- prepare_performance_data(
  probs = list(
    "Good Model" = example_dat$estimated_probabilities,
    "Bad Model"  = example_dat$bad_model
  ),
  reals = list(example_dat$outcome)
)

render_performance_table(perf_data)
```

------------------------------------------------------------------------

## Comprehensive HTML Summary Reports

[`create_summary_report()`](https://uriahf.github.io/rtichoke/reference/create_summary_report.md)
bundles all `rtichoke` interactive visualizations (ROC,
Precision-Recall, Gains, Lift, Decision Curve, Calibration Curve, and
Performance Tables) into a single, self-contained HTML file.

``` r

create_summary_report(
  probs = list("Primary Model" = example_dat$estimated_probabilities),
  reals = list(example_dat$outcome),
  file_path = "model_performance_report.html"
)
```

This report is ideal for sharing comprehensive model evaluation results
with collaborators, clinical stakeholders, or model validation review
boards.
