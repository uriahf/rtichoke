# Naming Convention

``` r

library(rtichoke)
```

### Naming Convention

|  | Predictions and Outcomes | Performance Data |
|----|----|----|
| Performance Data | [`prepare_performance_data()`](../reference/prepare_performance_data.md) |  |
| ROC | [`create_roc_curve()`](../reference/create_roc_curve.md) | [`plot_roc_curve()`](../reference/plot_roc_curve.md) |
| Lift | [`create_lift_curve()`](../reference/create_lift_curve.md) | [`plot_lift_curve()`](../reference/plot_lift_curve.md) |
| Gains | [`create_gains_curve()`](../reference/create_gains_curve.md) | [`plot_gains_curve()`](../reference/plot_gains_curve.md) |
| Precision Recall | [`create_precision_recall_curve()`](../reference/create_precision_recall_curve.md) | [`plot_precision_recall_curve()`](../reference/plot_precision_recall_curve.md) |
| Decision | [`create_decision_curve()`](../reference/create_decision_curve.md) | [`plot_decision_curve()`](../reference/plot_decision_curve.md) |
| Calibration | [`create_calibration_curve()`](../reference/create_calibration_curve.md) |  |
| Performance Table | [`create_performance_table()`](../reference/create_performance_table.md) | [`render_performance_table()`](../reference/render_performance_table.md) |
| Summary Report | [`create_summary_report()`](../reference/create_summary_report.md) |  |

### Curves based on Performance Metrics

|                  | Sens | Spec | PPV | PPCR | Lift | NB  | P. Thr |
|------------------|------|------|-----|------|------|-----|--------|
| ROC              | y    | x    |     |      |      |     |        |
| Lift             |      |      |     | x    | y    |     |        |
| Gains            | y    |      |     | x    |      |     |        |
| Precision Recall | x    |      | y   |      |      |     |        |
| Decision         |      |      |     |      |      | y   | x      |
