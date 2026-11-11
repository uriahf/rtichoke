# Create an interactive outcome distribution visualization across discrete steps

Prepares observed state counts or censoring-adjusted (Aalen-Johansen)
state occupancy probabilities across discrete steps and renders an
interactive Outcome Distribution component in the browser using
`rtichoke_viz`.

## Usage

``` r
create_reals_distribution_steps(
  data,
  id_col = "id",
  step_col = "step",
  reals_col = "reals",
  reals_order = NULL,
  reals_scale = "auto",
  censoring_heuristic = "excluded",
  title = "Outcome Distribution",
  renderer = "browser",
  bins = 10
)
```

## Arguments

- data:

  A data.frame, tibble, or data.table containing subject-step panel
  data.

- id_col:

  Character string specifying the subject identifier column.

- step_col:

  Character string specifying the discrete step column.

- reals_col:

  Character string specifying the observed outcome/state column.

- reals_order:

  Vector specifying category ordering. If `NULL`, ordering defaults to
  factor levels or sorted observed categories.

- reals_scale:

  Character string specifying the state scale (`"auto"`, `"nominal"`,
  `"ordinal"`, or `"continuous"`).

- censoring_heuristic:

  Character string specifying the estimation mode: `"excluded"` (raw
  observed counts per step) or `"adjusted"` (Aalen-Johansen state
  occupancy probabilities under right censoring).

- title:

  Character string specifying the chart title.

- renderer:

  Character string specifying the rendering engine. Currently, only
  `"browser"` is supported.

- bins:

  Single positive integer specifying number of equal-width bins or
  numeric vector of bin boundaries (used when
  `reals_scale = "continuous"`). Default is 10.

## Value

A browsable htmltools tag list rendering the Outcome Distribution chart.

## Details

The outcome variable scale can be specified via `reals_scale`:

- `"nominal"`:

  Unordered discrete categories.

- `"ordinal"`:

  Ordered discrete categories.

- `"continuous"`:

  Continuous numeric outcomes discretized into histogram bins.

- `"auto"`:

  Automatically chooses `"ordinal"` if ordered/numeric, otherwise
  `"nominal"`.

Numeric variables are not automatically interpreted as continuous;
continuous mode must be explicitly requested using
`reals_scale = "continuous"`.

For continuous outcomes, `bins` specifies either the number of
equal-width bins or explicit bin boundaries. Left-closed, right-open
intervals \\\[e_k, e\_{k+1})\\ are constructed, with the final bin
right-closed \\\[e_K, e\_{K+1}\]\\. The statistics represent empirical
probability mass (proportions summing to 1 over observed cases) rather
than probability density. Censoring adjustment is not supported for
continuous outcomes, so `censoring_heuristic` must be `"excluded"`.
Unobserved subjects (missing outcome or missing row at a step) are
accounted for in
`unobservedCount = referenceSampleSize - observedCount`.

## Examples

``` r
if (FALSE) { # \dontrun{
# 1. Nominal outcomes
create_reals_distribution_steps(
  clinical_states_steps,
  reals_scale = "nominal"
)

# 2. Ordinal outcomes
create_reals_distribution_steps(
  clinical_states_steps,
  reals_scale = "ordinal"
)

# 3. Continuous outcomes with equal-width bins
create_reals_distribution_steps(
  continuous_outcomes_steps,
  reals_scale = "continuous",
  bins = 5
)

# 4. Continuous outcomes with explicit bin boundaries
create_reals_distribution_steps(
  continuous_outcomes_steps,
  reals_scale = "continuous",
  bins = c(10, 20, 30, 40)
)
} # }
```
