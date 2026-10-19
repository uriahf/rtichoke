# Add interactive marker based on performance data

Add interactive marker based on performance data

## Usage

``` r
add_interactive_marker_from_performance_data(
  plotly_object,
  performance_data,
  performance_data_type,
  x_perf_metric,
  y_perf_metric,
  stratified_by = "probability_threshold"
)
```

## Arguments

- plotly_object:

  a previous plotly object

- performance_data:

  the performance data for the plot

- performance_data_type:

  the type of the performance data

- x_perf_metric:

  performance metric for the x axis

- y_perf_metric:

  performance metric for the y axis

- stratified_by:

  Performance Metrics can be stratified by Probability Threshold or
  alternatively by Predicted Positives Condition Rate
