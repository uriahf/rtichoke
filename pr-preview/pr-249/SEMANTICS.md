# Cross-language evaluation semantics

This document defines the vocabulary used to characterize and align
`rtichoke` and `rtichoke_python`. It is a descriptive contract for
parity work, not a new public API or a statement that every existing
internal field already maps one-to-one to these concepts.

## Core concepts

### Model

A **model** is a prediction rule, or equivalently a named set of
predictions, whose predictive performance is being evaluated.

A model identifies *what produced the predictions*. It does not by
itself identify the subjects or outcomes used to evaluate those
predictions.

### Population

A **population** is the set of subjects and observed outcomes against
which predictions are evaluated.

Population-level quantities can include outcome prevalence or event
risk, sample size, censoring and competing-event experience, and other
quantities that depend on the evaluation data rather than on the model
alone.

Two populations remain distinct evaluation populations even when a
population-level quantity, such as prevalence, happens to be numerically
the same in both.

### Evaluation

An **evaluation** is one model evaluated in one population.

Conceptually, the identity of an evaluation is therefore the pair

`model × population`.

This distinction matters because the same model can be evaluated in
several populations, several models can be evaluated in the same
population, and inputs can explicitly pair different models with
different populations.

### Evaluation context

The **evaluation context** contains conditions that define how an
evaluation is interpreted but are not the identity of the model itself.

For ordinary binary outcomes, the population is the primary evaluation
context. For time-dependent outcomes, the fixed time horizon is an
additional part of the context.

Other existing implementation choices, such as censoring or
competing-risk heuristics, can further qualify the context where
applicable. Stage 0 does not rename or redesign those choices.

### Fixed time horizon

For a time-dependent evaluation, the **fixed time horizon** is the time
point at which outcome status, event risk, and time-dependent
performance are being evaluated.

The same `model × population` evaluation at two horizons represents two
horizon-specific plotted series where the output is a curve.
Population-level reference quantities may also differ by horizon.

### Plotted series

A **plotted series** is an actual model-derived curve or set of points
drawn for one evaluation in one applicable evaluation context.

For ordinary binary outputs, a plotted series is typically identified by

`model × population`.

For time-dependent outputs, it is typically identified by

`model × population × fixed time horizon`.

A plotted series is distinct from a reference line. Reference lines
provide a benchmark for one or more evaluations but are not themselves
model evaluations.

## Reference-line scope

Reference lines should be characterized by the context that determines
their values, not merely by how many traces happen to be drawn.

Three scopes are useful for the parity audit:

- **Global**: the reference is independent of model, population, and
  horizon for the applicable output.
- **Population-specific**: the reference depends on the evaluation
  population but is shared by models evaluated in that population.
- **Population-and-horizon-specific**: the reference depends on both the
  evaluation population and fixed time horizon and is shared by models
  evaluated in that population at that horizon.

Two contexts can yield numerically identical references without becoming
the same context. For example, two distinct populations with equal
prevalence can have coincident prevalence-dependent reference lines
while remaining separate populations and separate evaluations.

Stage 1 characterization tests will record, for each output, the number,
ownership, labeling, and rendering behavior of reference lines in the
current implementations. This document deliberately does not pre-assign
every output to one of the scopes above before those tests are complete.

## Grouping, labels, legends, colors, and tables

Grouping and presentation are observable consequences of the semantic
model, but they are not definitions of semantic identity.

In particular:

- a legend label can represent a model, population, evaluation, horizon,
  or an existing compatibility grouping depending on the current code
  path;
- equal colors do not imply equal evaluation identity;
- separate colors do not necessarily imply different populations;
- a performance-table row or group should be characterized according to
  which evaluation and context it represents rather than inferred from
  its display label alone.

Stage 1 will characterize these behaviors without changing them.

## `reference_group` compatibility terminology

`reference_group` is an existing implementation and compatibility
concept. It is intentionally **not** defined here as the target domain
abstraction.

Depending on the current function and input shape, a `reference_group`
may act as a generic grouping key that corresponds to a model, a
population, an evaluation, or another compatibility grouping used to
organize calculations, references, colors, legends, or tables.

Therefore parity work must not assume

`reference_group == population`

or

`reference_group == model`.

Where current behavior uses `reference_group`, Stage 1 tests will
characterize what it represents in that specific path. A later parity
stage can decide whether internals or public terminology should change.
Stage 0 and Stage 1 do not make that change.

## Characterization principles

The next stage should preserve current production behavior and make its
semantics observable. Tests should distinguish at least:

1.  one model evaluated in one population;
2.  several models evaluated in the same population;
3.  the same model evaluated in populations with different prevalence or
    event risk;
4.  the same model evaluated in distinct populations with equal
    prevalence or event risk;
5.  explicitly paired model-population inputs; and
6.  time-dependent versions at multiple fixed horizons where supported.

For each applicable output, characterization should record
plotted-series identity, curve grouping, reference-line count and scope,
legend and label behavior, color behavior, and performance-table
semantics.

A surprising or inconsistent result is evidence for a later parity
decision; it is not a reason to change production behavior during
characterization.

## Scope of this contract

This contract does not change statistical calculations, package APIs,
input formats, return values, plotting behavior, labels, colors,
reference-line behavior, or `rtichoke_viz`.

It supplies shared vocabulary so R and Python behavior can be
characterized first and deliberately aligned later.
