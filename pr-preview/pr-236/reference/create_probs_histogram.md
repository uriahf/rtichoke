# Prediction Probability Histogram

Create an interactive histogram of predicted probabilities, separated by
observed outcome. The browser controls can show the distribution
relative to either probability thresholds or predicted-positive
classification rates (PPCR).

## Usage

``` r
create_probs_histogram(
  probs,
  reals,
  by = 0.01,
  stratified_by = "probability_threshold"
)
```

## Arguments

- probs:

  A list of numeric vectors containing predicted probabilities. A named
  list identifies models or populations in the rendered output.
  Probabilities must be between zero and one.

- reals:

  A list of numeric vectors containing observed binary outcomes, coded
  as zero or one. Supply one vector to evaluate multiple models in the
  same population, or one outcome vector for each probability vector to
  evaluate multiple populations.

- by:

  Numeric increment used to construct selectable probability thresholds
  or PPCR values. The default is `0.01`.

- stratified_by:

  Operating-point dimension. Use `"probability_threshold"` for
  probability cutoffs or `"ppcr"` for predicted-positive classification
  rates.

## Value

A browsable HTML object rendered by the vendored `rtichoke_viz` browser
bundle. It can be displayed in the RStudio Viewer, embedded in R
Markdown or Quarto, or saved as standalone HTML with
[`htmltools::save_html()`](https://rstudio.github.io/htmltools/reference/save_html.html).

## Examples

``` r
create_probs_histogram(
  probs = list(example_dat$estimated_probabilities),
  reals = list(example_dat$outcome),
  by = 0.1
)

{"schemaVersion":"2.0","type":"prediction_distribution","evaluations":[{"id":"evaluation-1","population":"population","model":"model"}],"operatingPoint":{"dimension":"probability_threshold"},"bins":[{"evaluationId":"evaluation-1","lower":0,"upper":0,"includeLower":true,"includeUpper":true,"nPositive":0,"nNegative":0},{"evaluationId":"evaluation-1","lower":0,"upper":0.1,"includeLower":false,"includeUpper":true,"nPositive":0,"nNegative":70},{"evaluationId":"evaluation-1","lower":0.1,"upper":0.2,"includeLower":false,"includeUpper":true,"nPositive":3,"nNegative":12},{"evaluationId":"evaluation-1","lower":0.2,"upper":0.3,"includeLower":false,"includeUpper":true,"nPositive":0,"nNegative":6},{"evaluationId":"evaluation-1","lower":0.3,"upper":0.4,"includeLower":false,"includeUpper":true,"nPositive":2,"nNegative":5},{"evaluationId":"evaluation-1","lower":0.4,"upper":0.5,"includeLower":false,"includeUpper":true,"nPositive":5,"nNegative":1},{"evaluationId":"evaluation-1","lower":0.5,"upper":0.6,"includeLower":false,"includeUpper":true,"nPositive":1,"nNegative":0},{"evaluationId":"evaluation-1","lower":0.6,"upper":0.7,"includeLower":false,"includeUpper":true,"nPositive":0,"nNegative":0},{"evaluationId":"evaluation-1","lower":0.7,"upper":0.8,"includeLower":false,"includeUpper":true,"nPositive":3,"nNegative":1},{"evaluationId":"evaluation-1","lower":0.8,"upper":0.9,"includeLower":false,"includeUpper":true,"nPositive":16,"nNegative":3},{"evaluationId":"evaluation-1","lower":0.9,"upper":1,"includeLower":false,"includeUpper":true,"nPositive":20,"nNegative":2}],"operatingPoints":[{"evaluationId":"evaluation-1","type":"probability_threshold","value":0,"cutoff":0,"realizedPpcr":1},{"evaluationId":"evaluation-1","type":"probability_threshold","value":0.1,"cutoff":0.1,"realizedPpcr":0.533333333333333},{"evaluationId":"evaluation-1","type":"probability_threshold","value":0.2,"cutoff":0.2,"realizedPpcr":0.433333333333333},{"evaluationId":"evaluation-1","type":"probability_threshold","value":0.3,"cutoff":0.3,"realizedPpcr":0.393333333333333},{"evaluationId":"evaluation-1","type":"probability_threshold","value":0.4,"cutoff":0.4,"realizedPpcr":0.346666666666667},{"evaluationId":"evaluation-1","type":"probability_threshold","value":0.5,"cutoff":0.5,"realizedPpcr":0.306666666666667},{"evaluationId":"evaluation-1","type":"probability_threshold","value":0.6,"cutoff":0.6,"realizedPpcr":0.3},{"evaluationId":"evaluation-1","type":"probability_threshold","value":0.7,"cutoff":0.7,"realizedPpcr":0.3},{"evaluationId":"evaluation-1","type":"probability_threshold","value":0.8,"cutoff":0.8,"realizedPpcr":0.273333333333333},{"evaluationId":"evaluation-1","type":"probability_threshold","value":0.9,"cutoff":0.9,"realizedPpcr":0.146666666666667},{"evaluationId":"evaluation-1","type":"probability_threshold","value":1,"cutoff":1,"realizedPpcr":0}]}import { renderPredictionDistribution } from './lib/rtichoke-viz-0.21.0/rtichoke-viz.js';
const spec = JSON.parse(document.querySelector('#rtichoke-viz-1-spec').textContent);
document.querySelector('#rtichoke-viz-1').append(renderPredictionDistribution(spec));
create_probs_histogram(
  probs = list(
    "Prediction Model" = example_dat$estimated_probabilities,
    "Random Guess" = example_dat$random_guess
  ),
  reals = list(example_dat$outcome),
  by = 0.1,
  stratified_by = "ppcr"
)

{"schemaVersion":"2.0","type":"prediction_distribution","evaluations":[{"id":"evaluation-1","population":"population","model":"Prediction Model"},{"id":"evaluation-2","population":"population","model":"Random Guess"}],"operatingPoint":{"dimension":"ppcr"},"bins":[{"evaluationId":"evaluation-1","lower":0,"upper":0,"includeLower":true,"includeUpper":true,"nPositive":0,"nNegative":0},{"evaluationId":"evaluation-1","lower":0,"upper":2.12977135191622e-06,"includeLower":false,"includeUpper":true,"nPositive":0,"nNegative":1},{"evaluationId":"evaluation-1","lower":2.12977135191622e-06,"upper":0.000376658203479926,"includeLower":false,"includeUpper":true,"nPositive":0,"nNegative":16},{"evaluationId":"evaluation-1","lower":0.000376658203479926,"upper":0.00273779783272446,"includeLower":false,"includeUpper":true,"nPositive":0,"nNegative":13},{"evaluationId":"evaluation-1","lower":0.00273779783272446,"upper":0.00833950619639248,"includeLower":false,"includeUpper":true,"nPositive":0,"nNegative":21},{"evaluationId":"evaluation-1","lower":0.00833950619639248,"upper":0.0231296821530497,"includeLower":false,"includeUpper":true,"nPositive":0,"nNegative":10},{"evaluationId":"evaluation-1","lower":0.0231296821530497,"upper":0.100604332312437,"includeLower":false,"includeUpper":true,"nPositive":1,"nNegative":15},{"evaluationId":"evaluation-1","lower":0.100604332312437,"upper":0.239506299197465,"includeLower":false,"includeUpper":true,"nPositive":2,"nNegative":12},{"evaluationId":"evaluation-1","lower":0.239506299197465,"upper":0.632830736533985,"includeLower":false,"includeUpper":true,"nPositive":8,"nNegative":6},{"evaluationId":"evaluation-1","lower":0.632830736533985,"upper":0.875449996374463,"includeLower":false,"includeUpper":true,"nPositive":19,"nNegative":4},{"evaluationId":"evaluation-1","lower":0.875449996374463,"upper":0.951899660263221,"includeLower":false,"includeUpper":true,"nPositive":9,"nNegative":2},{"evaluationId":"evaluation-1","lower":0.951899660263221,"upper":0.996215385502889,"includeLower":false,"includeUpper":true,"nPositive":11,"nNegative":0},{"evaluationId":"evaluation-1","lower":0.996215385502889,"upper":1,"includeLower":false,"includeUpper":true,"nPositive":0,"nNegative":0},{"evaluationId":"evaluation-2","lower":0,"upper":0,"includeLower":true,"includeUpper":true,"nPositive":0,"nNegative":0},{"evaluationId":"evaluation-2","lower":0,"upper":0.00723187602125108,"includeLower":false,"includeUpper":true,"nPositive":0,"nNegative":1},{"evaluationId":"evaluation-2","lower":0.00723187602125108,"upper":0.0996741322334856,"includeLower":false,"includeUpper":true,"nPositive":6,"nNegative":8},{"evaluationId":"evaluation-2","lower":0.0996741322334856,"upper":0.183209521323442,"includeLower":false,"includeUpper":true,"nPositive":7,"nNegative":8},{"evaluationId":"evaluation-2","lower":0.183209521323442,"upper":0.307875424996019,"includeLower":false,"includeUpper":true,"nPositive":5,"nNegative":10},{"evaluationId":"evaluation-2","lower":0.307875424996019,"upper":0.440191815234721,"includeLower":false,"includeUpper":true,"nPositive":6,"nNegative":9},{"evaluationId":"evaluation-2","lower":0.440191815234721,"upper":0.540781599236652,"includeLower":false,"includeUpper":true,"nPositive":4,"nNegative":11},{"evaluationId":"evaluation-2","lower":0.540781599236652,"upper":0.635282384976745,"includeLower":false,"includeUpper":true,"nPositive":4,"nNegative":11},{"evaluationId":"evaluation-2","lower":0.635282384976745,"upper":0.716708688181825,"includeLower":false,"includeUpper":true,"nPositive":2,"nNegative":13},{"evaluationId":"evaluation-2","lower":0.716708688181825,"upper":0.847088379831985,"includeLower":false,"includeUpper":true,"nPositive":4,"nNegative":11},{"evaluationId":"evaluation-2","lower":0.847088379831985,"upper":0.922277670027688,"includeLower":false,"includeUpper":true,"nPositive":5,"nNegative":10},{"evaluationId":"evaluation-2","lower":0.922277670027688,"upper":0.994544052984565,"includeLower":false,"includeUpper":true,"nPositive":7,"nNegative":8},{"evaluationId":"evaluation-2","lower":0.994544052984565,"upper":1,"includeLower":false,"includeUpper":true,"nPositive":0,"nNegative":0}],"operatingPoints":[{"evaluationId":"evaluation-1","type":"ppcr","value":0,"cutoff":0.996215385502889,"realizedPpcr":0},{"evaluationId":"evaluation-1","type":"ppcr","value":0.1,"cutoff":0.951899660263221,"realizedPpcr":0.0733333333333333},{"evaluationId":"evaluation-1","type":"ppcr","value":0.2,"cutoff":0.875449996374463,"realizedPpcr":0.146666666666667},{"evaluationId":"evaluation-1","type":"ppcr","value":0.3,"cutoff":0.632830736533985,"realizedPpcr":0.3},{"evaluationId":"evaluation-1","type":"ppcr","value":0.4,"cutoff":0.239506299197465,"realizedPpcr":0.393333333333333},{"evaluationId":"evaluation-1","type":"ppcr","value":0.5,"cutoff":0.100604332312437,"realizedPpcr":0.486666666666667},{"evaluationId":"evaluation-1","type":"ppcr","value":0.6,"cutoff":0.0231296821530497,"realizedPpcr":0.593333333333333},{"evaluationId":"evaluation-1","type":"ppcr","value":0.7,"cutoff":0.00833950619639248,"realizedPpcr":0.66},{"evaluationId":"evaluation-1","type":"ppcr","value":0.8,"cutoff":0.00273779783272446,"realizedPpcr":0.8},{"evaluationId":"evaluation-1","type":"ppcr","value":0.9,"cutoff":0.000376658203479926,"realizedPpcr":0.886666666666667},{"evaluationId":"evaluation-1","type":"ppcr","value":1,"cutoff":2.12977135191622e-06,"realizedPpcr":1},{"evaluationId":"evaluation-2","type":"ppcr","value":0,"cutoff":0.994544052984565,"realizedPpcr":0},{"evaluationId":"evaluation-2","type":"ppcr","value":0.1,"cutoff":0.922277670027688,"realizedPpcr":0.1},{"evaluationId":"evaluation-2","type":"ppcr","value":0.2,"cutoff":0.847088379831985,"realizedPpcr":0.2},{"evaluationId":"evaluation-2","type":"ppcr","value":0.3,"cutoff":0.716708688181825,"realizedPpcr":0.3},{"evaluationId":"evaluation-2","type":"ppcr","value":0.4,"cutoff":0.635282384976745,"realizedPpcr":0.4},{"evaluationId":"evaluation-2","type":"ppcr","value":0.5,"cutoff":0.540781599236652,"realizedPpcr":0.5},{"evaluationId":"evaluation-2","type":"ppcr","value":0.6,"cutoff":0.440191815234721,"realizedPpcr":0.6},{"evaluationId":"evaluation-2","type":"ppcr","value":0.7,"cutoff":0.307875424996019,"realizedPpcr":0.7},{"evaluationId":"evaluation-2","type":"ppcr","value":0.8,"cutoff":0.183209521323442,"realizedPpcr":0.8},{"evaluationId":"evaluation-2","type":"ppcr","value":0.9,"cutoff":0.0996741322334856,"realizedPpcr":0.9},{"evaluationId":"evaluation-2","type":"ppcr","value":1,"cutoff":0.00723187602125108,"realizedPpcr":1}]}import { renderPredictionDistribution } from './lib/rtichoke-viz-0.21.0/rtichoke-viz.js';
const spec = JSON.parse(document.querySelector('#rtichoke-viz-2-spec').textContent);
document.querySelector('#rtichoke-viz-2').append(renderPredictionDistribution(spec));
```
