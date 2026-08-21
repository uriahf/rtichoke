# Cross-language semantic parity matrix

This document records the semantic parity findings established by the Stage 0
contract and Stage 1 characterization tests in `rtichoke` and
`rtichoke_python`.

It is descriptive. It does not change public APIs, statistical calculations,
input formats, plotting behavior, or `rtichoke_viz`.

## Target semantic model

The cross-language target remains:

- **model**: what produced the predictions;
- **population**: the subjects and observed outcomes used for evaluation;
- **evaluation**: one `model × population` pair;
- **evaluation context**: conditions qualifying that evaluation, including the
  fixed time horizon for time-dependent outputs;
- **plotted series**: one evaluation in one applicable context;
- **reference scope**: the population/context that determines a benchmark,
  independent of whether two benchmarks happen to have equal numerical values.

`reference_group` remains compatibility terminology. It is not the target
domain abstraction.

## Input and grouping semantics

| Scenario | R | Python | Parity assessment |
| --- | --- | --- | --- |
| One model × one population | One plotted evaluation | One plotted evaluation | Semantic parity |
| Multiple models × same population | Explicit `model` grouping | Generic `reference_group` labels act as models | Same semantics, different representation |
| Same model × multiple populations | Explicit `population` grouping | Generic `reference_group` labels act as populations | Same semantics, different representation |
| Distinct populations with equal prevalence | Remain distinct populations | Remain distinct static population contexts | Semantic parity for static outputs |
| Explicit paired model-population inputs | Pair labels are represented through the population-shaped path | Pair labels remain generic `reference_group`s | Same plotted-evaluation cardinality, representation divergence |
| Time-dependent evaluations | No corresponding characterized R time API in Stage 1 | Series are keyed by `reference_group × fixed_time_horizon` | Python-only characterization |

The main static representation difference is therefore structural rather than
statistical: R exposes whether the keyed dimension is a model or population in
its prepared data, while Python currently collapses both into
`reference_group`.

## Reference-line parity by output

The table below describes semantic scope, not implementation-specific names.

| Output | Global reference | Population-dependent reference | Static R/Python parity |
| --- | --- | --- | --- |
| ROC | Random/identity diagonal | None | Aligned |
| Precision-recall | None beyond the prevalence baseline | Random baseline = population prevalence | Aligned |
| Gains | Random baseline | Perfect-model reference depends on population prevalence | Aligned |
| Lift | Random baseline | Perfect-model reference depends on population prevalence | Aligned |
| Decision curve | Treat-none | Treat-all depends on population prevalence | Aligned |
| Interventions avoided | Treat-all | Treat-none counterpart depends on population prevalence | Aligned |
| Calibration | Perfect-calibration identity line | None | Aligned |
| Performance tables | Not applicable | Rows/groups represent evaluation grouping rather than reference lines | Broad semantic parity; representation differs |

For multiple models evaluated in one population, both implementations share the
same population-dependent references across the models. For different
populations, both static implementations retain population ownership of
population-dependent references.

Critically, two distinct static populations with equal prevalence remain
separate contexts in both languages even when their prevalence-dependent
references are numerically identical. Coincident values do not collapse
semantic ownership.

## Plot labels, legends, and colors

Both languages use the currently active grouping key to label and color plotted
series. With multiple models in one population, model labels remain distinct;
with multiple population-shaped inputs, population labels remain distinct.

This is presentation parity at the level needed for the semantic contract, but
not evidence that the underlying representation is identical. In particular,
Python labels and colors are still organized through `reference_group`, whereas
R can retain an explicit `model` or `population` column before plotting.

Plotly renderer bookkeeping traces, cutoff markers, and animation traces are not
additional semantic evaluations. A plotted series is identified by its model
and evaluation context, not by raw trace count.

## Performance-table semantics

Static performance tables follow the same evaluation cardinality as the curve
inputs in both languages:

- multiple models in the same population remain separate model evaluations;
- multiple populations remain separate evaluation contexts;
- paired inputs remain separate evaluations.

The representation differs: R can expose `model` versus `population` in the
prepared data, whereas Python preserves the generic `reference_group` field.

For Python time-dependent performance data and tables, the characterized key is
`reference_group × fixed_time_horizon`. This preserves separate horizon-specific
rows/groups but does not separately encode model and population identity.

## Time-dependent Python findings

The Python time-dependent API is the main semantic divergence identified so far.

A model-derived time series is characterized by a generic group plus horizon,
which corresponds conceptually to

`evaluation × fixed_time_horizon`.

However, the current implementation infers whether multiple groups represent
multiple populations from whether their horizon-specific event risks differ.
This makes reference ownership depend on numerical equality rather than
population identity.

Consequences:

1. Multiple models sharing one outcome population correctly share reference
   lines at each horizon.
2. Distinct populations with different event risks get population-specific
   references at that horizon.
3. Distinct populations with equal event risk remain separate plotted series,
   but their population-dependent references collapse to shared references.
4. The same pair of populations can therefore have shared references at one
   horizon and population-specific references at another if their risks first
   coincide and later diverge.

This behavior differs from the target semantic contract. Population ownership
should not disappear merely because two populations happen to yield the same
risk at one horizon.

## Classification of current differences

### Semantic parity

- evaluation cardinality for the static scenarios;
- static curve grouping across ROC, precision-recall, gains, lift, decision,
  interventions avoided, and calibration;
- static reference-line scope for all characterized outputs;
- preservation of distinct static populations when prevalence is equal;
- calibration identity-line semantics.

### Different representation of the same semantics

- R uses explicit `model` or `population` columns in key static paths;
- Python uses the generic `reference_group` field for model-, population-, and
  paired-evaluation labels;
- paired inputs are not explicitly decomposed into separate model and population
  dimensions in either characterized path.

### True semantic divergence to address later

- Python time-dependent population detection and reference ownership can depend
  on equality of event risk rather than stable population identity;
- Python time-dependent prepared data does not separately encode model and
  population dimensions, so the implementation cannot reliably distinguish
  two models in one population from two populations with equal horizon-specific
  risk using semantic identity alone.

## Recommended parity direction

Future parity work should preserve existing public APIs until a migration plan
is explicit, but internal semantics should move toward stable evaluation
identity:

1. represent or derive **model identity** and **population identity** separately;
2. define an evaluation as their pair;
3. add horizon only as evaluation context, not as a substitute for population
   identity;
4. scope prevalence/event-risk-dependent references to population (and horizon
   for time-dependent outputs), even when values coincide numerically;
5. treat `reference_group` as a compatibility/display grouping field rather than
   the source of semantic truth;
6. derive colors, labels, legends, tables, and eventually visualization specs
   from the explicit evaluation/context model.

This direction does **not** imply that every public function must immediately
accept new `model` and `population` arguments. The next implementation stage
should first identify the smallest internal representation change that can
preserve existing APIs while making population ownership explicit.

## Implication for `rtichoke_viz`

No `rtichoke_viz` schema change should be made from this document alone.

When parity work reaches the visualization layer, the schema should be derived
from explicit evaluation and reference ownership rather than promoting the
current `reference_group` compatibility key into a permanent domain concept.
Until the package-level semantics are aligned, the existing visualization proof
should remain unchanged.
