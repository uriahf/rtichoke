# Naming Convention

``` r

library(rtichoke)
```

### Naming Convention

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

### Curves based on Performance Metrics

|                  | Sens | Spec | PPV | PPCR | Lift | NB  | P. Thr |
|------------------|------|------|-----|------|------|-----|--------|
| ROC              | y    | x    |     |      |      |     |        |
| Lift             |      |      |     | x    | y    |     |        |
| Gains            | y    |      |     | x    |      |     |        |
| Precision Recall | x    |      | y   |      |      |     |        |
| Decision         |      |      |     |      |      | y   | x      |
