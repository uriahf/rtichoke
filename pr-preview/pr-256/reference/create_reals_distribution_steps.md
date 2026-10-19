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
  renderer = "browser"
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

  Character string specifying the state scale (`"auto"`, `"nominal"`, or
  `"ordinal"`).

- censoring_heuristic:

  Character string specifying the estimation mode: `"excluded"` (raw
  observed counts per step) or `"adjusted"` (Aalen-Johansen state
  occupancy probabilities under right censoring).

- title:

  Character string specifying the chart title.

- renderer:

  Character string specifying the rendering engine. Currently, only
  `"browser"` is supported.

## Value

A browsable htmltools tag list rendering the Outcome Distribution chart.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
  id = rep(1:10, each = 3),
  step = rep(1:3, 10),
  reals = sample(c("A", "B", "C"), 30, replace = TRUE)
)

create_reals_distribution_steps(
  data = df,
  id_col = "id",
  step_col = "step",
  reals_col = "reals",
  censoring_heuristic = "excluded"
)
} # }
```
