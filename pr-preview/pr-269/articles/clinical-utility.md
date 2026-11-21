# Clinical Utility: Decision Curves & Interventions Avoided

Clinical utility evaluation moves beyond statistical accuracy
(discrimination and calibration) to answer a fundamental
decision-analytic question:

> *Does using this prediction model to guide clinical decision-making
> lead to better patient outcomes than standard strategies?*

`rtichoke` supports clinical decision analysis using **Decision Curves
(Net Benefit)** and **Interventions Avoided**.

For decision-analytic theory and threshold weight derivations, visit the
[rtichoke blog](https://rtichoke-blog.netlify.app/).

------------------------------------------------------------------------

## Decision Curves (Net Benefit)

Decision curve analysis weighs true positive benefits against false
positive harms across a range of clinical decision thresholds ($`p_t`$):

``` math
\text{Net Benefit} = \frac{\text{True Positives}}{N} - \frac{\text{False Positives}}{N} \times \left( \frac{p_t}{1 - p_t} \right)
```

``` r

library(rtichoke)

create_decision_curve(
  probs = list(
    "Good Model"   = example_dat$estimated_probabilities,
    "Bad Model"    = example_dat$bad_model,
    "Random Guess" = example_dat$random_guess
  ),
  reals = list(example_dat$outcome)
)
```

### Benchmark Reference Strategies

1.  **Treat All:** Assumes every patient receives the intervention
    regardless of predicted risk.
2.  **Treat None:** Assumes no patient receives the intervention
    ($`\text{Net Benefit} = 0`$).

A model provides clinical value at a decision threshold if its net
benefit curve lies above both default benchmark strategies.

------------------------------------------------------------------------

## Interventions Avoided

The **Interventions Avoided** metric translates Net Benefit into a
practical clinical count: the number of unnecessary interventions
(unneeded tests or treatments) avoided per 100 patients without missing
true positive cases, compared to a “Treat All” strategy:

``` math
\text{Interventions Avoided per 100} = \left( \frac{\text{Net Benefit}_{\text{Model}} - \text{Net Benefit}_{\text{Treat All}}}{p_t / (1 - p_t)} \right) \times 100
```

`rtichoke` integrates Interventions Avoided alongside Decision Curve
metrics.

------------------------------------------------------------------------

## Prepared Data Workflow for Decision Analysis

You can also compute clinical utility metrics via
[`prepare_performance_data()`](https://uriahf.github.io/rtichoke/reference/prepare_performance_data.md):

``` r

perf_data <- prepare_performance_data(
  probs = list(
    "Good Model" = example_dat$estimated_probabilities,
    "Bad Model"  = example_dat$bad_model
  ),
  reals = list(example_dat$outcome)
)

# Plot decision curve from prepared performance data
plot_decision_curve(perf_data)
```
