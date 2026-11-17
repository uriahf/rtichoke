# Naming Conventions

``` r

library(rtichoke)
```

## Naming conventions

rtichoke uses a consistent naming pattern to distinguish high-level
workflows from lower-level plotting and rendering functions. Functions
named `create_*()` start from predictions and outcomes and create a
curve, table, or report.
[`prepare_performance_data()`](https://uriahf.github.io/rtichoke/reference/prepare_performance_data.md)
exposes the common performance-data layer, while `plot_*()` and
`render_*()` functions operate on already prepared performance data
where those lower-level interfaces are available.

The table below summarizes the main entry points.

|  | Predictions and Outcomes | Performance Data |
|----|----|----|
| Performance Data | [`prepare_performance_data()`](https://uriahf.github.io/rtichoke/reference/prepare_performance_data.md) |  |
| ROC | [`create_roc_curve()`](https://uriahf.github.io/rtichoke/reference/create_roc_curve.md) | [`plot_roc_curve()`](https://uriahf.github.io/rtichoke/reference/plot_roc_curve.md) |
| Lift | [`create_lift_curve()`](https://uriahf.github.io/rtichoke/reference/create_lift_curve.md) | [`plot_lift_curve()`](https://uriahf.github.io/rtichoke/reference/plot_lift_curve.md) |
| Gains | [`create_gains_curve()`](https://uriahf.github.io/rtichoke/reference/create_gains_curve.md) | [`plot_gains_curve()`](https://uriahf.github.io/rtichoke/reference/plot_gains_curve.md) |
| Precision Recall | [`create_precision_recall_curve()`](https://uriahf.github.io/rtichoke/reference/create_precision_recall_curve.md) | [`plot_precision_recall_curve()`](https://uriahf.github.io/rtichoke/reference/plot_precision_recall_curve.md) |
| Decision | [`create_decision_curve()`](https://uriahf.github.io/rtichoke/reference/create_decision_curve.md) | [`plot_decision_curve()`](https://uriahf.github.io/rtichoke/reference/plot_decision_curve.md) |
| Calibration | [`create_calibration_curve()`](https://uriahf.github.io/rtichoke/reference/create_calibration_curve.md) |  |
| Performance Table | [`create_performance_table()`](https://uriahf.github.io/rtichoke/reference/create_performance_table.md) | [`render_performance_table()`](https://uriahf.github.io/rtichoke/reference/render_performance_table.md) |
| Summary Report | [`create_summary_report()`](https://uriahf.github.io/rtichoke/reference/create_summary_report.md) |  |

## Curves and performance metrics

The performance curves are different views of the same underlying
performance data. This table shows which quantities define the axes of
each curve.

|                  | Sens | Spec | PPV | PPCR | Lift | NB  | P. Thr |
|------------------|------|------|-----|------|------|-----|--------|
| ROC              | y    | x    |     |      |      |     |        |
| Lift             |      |      |     | x    | y    |     |        |
| Gains            | y    |      |     | x    |      |     |        |
| Precision Recall | x    |      | y   |      |      |     |        |
| Decision         |      |      |     |      |      | y   | x      |
