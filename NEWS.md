# rtichoke 0.0.8

- Introduce the browser-rendered summary report and prediction distribution for binary prediction, including probability-threshold and predicted-positive-rate views.
- Add `create_summary_report_times()` and `create_reals_distribution_times()` for observed outcomes over follow-up. The HTML report supports eight optional combinations for handling censoring and competing events.
- Require positive follow-up times for target and competing events in the time-dependent chart and report. Horizon zero and censoring at zero remain valid.
- Add `create_reals_distribution_steps()` for categorical and continuous outcomes observed at discrete steps.
- Bundle rtichoke_viz v0.24.3 for offline rendering.

# rtichoke 0.0.7

-   Fix Lift Curve ranges [#110](https://github.com/uriahf/rtichoke/issues/110)

# rtichoke 0.0.6

-   Add perfect-model reference lines for lift curve [#106](https://github.com/uriahf/rtichoke/issues/106)

-   Fix gains reference lines hover text [#107](https://github.com/uriahf/rtichoke/issues/107)

# rtichoke 0.0.5

-   Allow zero-variance in `probs` or `reals` arguments [\@filbert42](https://github.com/filbert42){style="font-size: 11pt;"} [#69](https://github.com/uriahf/rtichoke/issues/69){style="font-size: 11pt;"}

-   rtichoke (plotly) curves are now transparent by default [#86](https://github.com/uriahf/rtichoke/issues/86)

-   Treat-All and Treat-None are supported in interventions avoided decision curves ([similar to {dcurves} package update 0.4.0](https://www.danieldsjoberg.com/dcurves/news/index.html#dcurves-040)) [\@ddsjoberg](https://github.com/ddsjoberg) [#89](https://github.com/uriahf/rtichoke/issues/89)

-   Discrete Calibration Curves have additional real numbers hover text to better inform the calculation of the proportion [\@urigott](https://github.com/urigott) [#91](https://github.com/uriahf/rtichoke/issues/91)

# rtichoke 0.0.4

-   Remove main_slider argument from \`plot\_\*\_curve()\` and from \`render_performance_table()\` [#47](https://github.com/uriahf/rtichoke/issues/47)
-   Add Interventions Avoided option to Decision Curve [#61](https://github.com/uriahf/rtichoke/issues/61)
-   Support more than 5 populations [\@filbert42](https://github.com/filbert42) [#70](https://github.com/uriahf/rtichoke/issues/70)
