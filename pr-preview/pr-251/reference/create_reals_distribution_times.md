# Create an interactive outcome distribution visualization over time horizons

Prepares observed outcome counts (target events, competing events,
censored, and non-events) across time horizons and renders an
interactive Outcome Distribution component in the browser using
`rtichoke_viz`.

## Usage

``` r
create_reals_distribution_times(
  reals,
  times,
  fixed_time_horizons,
  renderer = "browser"
)
```

## Arguments

- reals:

  A numeric vector (or list of numeric vectors) containing binary or
  multi-state outcome labels (0 = censored/no event, 1 = target event, 2
  = competing outcome).

- times:

  A numeric vector (or list of numeric vectors) containing follow-up
  times corresponding to `reals`.

- fixed_time_horizons:

  A numeric vector of fixed evaluation time horizons at which to display
  state distributions.

- renderer:

  Character string specifying the rendering engine. Currently, only
  `"browser"` is supported.

## Value

A browsable htmltools tag list rendering the Outcome Distribution chart.

## Examples

``` r
times <- c(24.1, 9.7, 49.9, 18.6, 34.8, 14.2, 39.2, 46.0, 31.5, 4.3)
reals <- c(1, 1, 1, 1, 0, 2, 1, 2, 0, 1)
fixed_time_horizons <- c(10, 20, 30, 40, 50)

create_reals_distribution_times(
  reals = reals,
  times = times,
  fixed_time_horizons = fixed_time_horizons,
  renderer = "browser"
)

{"schemaVersion":"2.0","type":"outcome_distribution","title":"Outcome Distribution","evaluations":[{"id":"evaluation-1","population":"evaluation"}],"stateDistributions":[{"evaluationId":"evaluation-1","horizon":0,"estimator":"raw","estimateOrigin":"event_table","states":[{"stateId":"real_positive","label":"Target event","count":0},{"stateId":"real_competing","label":"Competing outcome","count":0},{"stateId":"real_negative","label":"No target event","count":10},{"stateId":"real_censored","label":"Unknown / excluded","count":0}]},{"evaluationId":"evaluation-1","horizon":4.3,"estimator":"raw","estimateOrigin":"event_table","states":[{"stateId":"real_positive","label":"Target event","count":1},{"stateId":"real_competing","label":"Competing outcome","count":0},{"stateId":"real_negative","label":"No target event","count":9},{"stateId":"real_censored","label":"Unknown / excluded","count":0}]},{"evaluationId":"evaluation-1","horizon":9.7,"estimator":"raw","estimateOrigin":"event_table","states":[{"stateId":"real_positive","label":"Target event","count":2},{"stateId":"real_competing","label":"Competing outcome","count":0},{"stateId":"real_negative","label":"No target event","count":8},{"stateId":"real_censored","label":"Unknown / excluded","count":0}]},{"evaluationId":"evaluation-1","horizon":14.2,"estimator":"raw","estimateOrigin":"event_table","states":[{"stateId":"real_positive","label":"Target event","count":2},{"stateId":"real_competing","label":"Competing outcome","count":1},{"stateId":"real_negative","label":"No target event","count":7},{"stateId":"real_censored","label":"Unknown / excluded","count":0}]},{"evaluationId":"evaluation-1","horizon":18.6,"estimator":"raw","estimateOrigin":"event_table","states":[{"stateId":"real_positive","label":"Target event","count":3},{"stateId":"real_competing","label":"Competing outcome","count":1},{"stateId":"real_negative","label":"No target event","count":6},{"stateId":"real_censored","label":"Unknown / excluded","count":0}]},{"evaluationId":"evaluation-1","horizon":24.1,"estimator":"raw","estimateOrigin":"event_table","states":[{"stateId":"real_positive","label":"Target event","count":4},{"stateId":"real_competing","label":"Competing outcome","count":1},{"stateId":"real_negative","label":"No target event","count":5},{"stateId":"real_censored","label":"Unknown / excluded","count":0}]},{"evaluationId":"evaluation-1","horizon":31.5,"estimator":"raw","estimateOrigin":"event_table","states":[{"stateId":"real_positive","label":"Target event","count":4},{"stateId":"real_competing","label":"Competing outcome","count":1},{"stateId":"real_negative","label":"No target event","count":5},{"stateId":"real_censored","label":"Unknown / excluded","count":0}]},{"evaluationId":"evaluation-1","horizon":34.8,"estimator":"raw","estimateOrigin":"event_table","states":[{"stateId":"real_positive","label":"Target event","count":4},{"stateId":"real_competing","label":"Competing outcome","count":1},{"stateId":"real_negative","label":"No target event","count":4},{"stateId":"real_censored","label":"Unknown / excluded","count":1}]},{"evaluationId":"evaluation-1","horizon":39.2,"estimator":"raw","estimateOrigin":"event_table","states":[{"stateId":"real_positive","label":"Target event","count":5},{"stateId":"real_competing","label":"Competing outcome","count":1},{"stateId":"real_negative","label":"No target event","count":2},{"stateId":"real_censored","label":"Unknown / excluded","count":2}]},{"evaluationId":"evaluation-1","horizon":46,"estimator":"raw","estimateOrigin":"event_table","states":[{"stateId":"real_positive","label":"Target event","count":5},{"stateId":"real_competing","label":"Competing outcome","count":2},{"stateId":"real_negative","label":"No target event","count":1},{"stateId":"real_censored","label":"Unknown / excluded","count":2}]},{"evaluationId":"evaluation-1","horizon":49.9,"estimator":"raw","estimateOrigin":"event_table","states":[{"stateId":"real_positive","label":"Target event","count":6},{"stateId":"real_competing","label":"Competing outcome","count":2},{"stateId":"real_negative","label":"No target event","count":0},{"stateId":"real_censored","label":"Unknown / excluded","count":2}]},{"evaluationId":"evaluation-1","horizon":0,"estimator":"raw","estimateOrigin":"fixed_time_horizon","states":[{"stateId":"real_positive","label":"Target event","count":0},{"stateId":"real_competing","label":"Competing outcome","count":0},{"stateId":"real_negative","label":"No target event","count":10},{"stateId":"real_censored","label":"Unknown / excluded","count":0}]},{"evaluationId":"evaluation-1","horizon":10,"estimator":"raw","estimateOrigin":"fixed_time_horizon","states":[{"stateId":"real_positive","label":"Target event","count":2},{"stateId":"real_competing","label":"Competing outcome","count":0},{"stateId":"real_negative","label":"No target event","count":8},{"stateId":"real_censored","label":"Unknown / excluded","count":0}]},{"evaluationId":"evaluation-1","horizon":20,"estimator":"raw","estimateOrigin":"fixed_time_horizon","states":[{"stateId":"real_positive","label":"Target event","count":3},{"stateId":"real_competing","label":"Competing outcome","count":1},{"stateId":"real_negative","label":"No target event","count":6},{"stateId":"real_censored","label":"Unknown / excluded","count":0}]},{"evaluationId":"evaluation-1","horizon":30,"estimator":"raw","estimateOrigin":"fixed_time_horizon","states":[{"stateId":"real_positive","label":"Target event","count":4},{"stateId":"real_competing","label":"Competing outcome","count":1},{"stateId":"real_negative","label":"No target event","count":5},{"stateId":"real_censored","label":"Unknown / excluded","count":0}]},{"evaluationId":"evaluation-1","horizon":40,"estimator":"raw","estimateOrigin":"fixed_time_horizon","states":[{"stateId":"real_positive","label":"Target event","count":5},{"stateId":"real_competing","label":"Competing outcome","count":1},{"stateId":"real_negative","label":"No target event","count":2},{"stateId":"real_censored","label":"Unknown / excluded","count":2}]},{"evaluationId":"evaluation-1","horizon":50,"estimator":"raw","estimateOrigin":"fixed_time_horizon","states":[{"stateId":"real_positive","label":"Target event","count":6},{"stateId":"real_competing","label":"Competing outcome","count":2},{"stateId":"real_negative","label":"No target event","count":0},{"stateId":"real_censored","label":"Unknown / excluded","count":2}]}]}import { renderOutcomeDistribution } from './lib/rtichoke-viz-0.23.1/rtichoke-viz.js';
const spec = JSON.parse(document.querySelector('#rtichoke-viz-3-spec').textContent);
document.querySelector('#rtichoke-viz-3').append(renderOutcomeDistribution(spec));
```
