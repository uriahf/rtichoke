# Internal population preparation and canonical serialization

`rtichoke._terminal_outcome_distribution` provides internal preparation only;
no public entry point or report uses it. Inputs are a mapping of semantic
population labels to nonempty Polars frames (`id`, `reals`, `times`) and supplied
fixed horizons. IDs must be unique/nonnull within each population. Zero is
evaluated normally when supplied; it is never inserted as a synthetic baseline.
Repeated horizons are deduplicated and sorted.

`_prepare_terminal_outcome_distributions()` returns all eight pairs per population
and horizon, including complete probabilities, estimator, status, retained and
excluded IDs, original/retained sizes, exclusion counts, and exclusion fractions
whose denominator is the original population size. It reuses existing outcome
validation and exclusion extraction. Retention is applied to original outcomes
before recoding. Existing cutoff/stratum producers cannot supply population
denominators and are not used. KM/AJ estimation is delegated to public
`polarstate.prepare_event_table()` and `predict_aj_estimates()`; no estimator
formulas are implemented. Original data and existing statistical helpers are
unchanged. The scalar-risk expected failures from #441 remain applicable.

Population labels denote semantic identity independently of numerical equality.
Evaluation IDs follow the existing outcome adapter's deterministic population
enumeration and remain identical across variants/horizons. No model identity is
inferred for these population-only inputs.

## Published v0.24.2 status-aware serialization

The immutable vendored release exports `OutcomeDistributionSpecSchema`,
`StateDistributionSchema`, and `assertOutcomeDistributionReferentialIntegrity`.
Tests execute those exports and validate every #441 reference case, including
all supplied horizons and all eight heuristic pairs. No renderer is rebuilt.

Each distribution maps prepared `status` directly to `estimateStatus`: `defined`,
`undefined_empty_population`, or `beyond_support_plateau`. Empty retained
populations preserve complete state vectors with explicit null estimates.
Supplied plateau values remain unchanged apart from the endpoint-roundoff rule
below. Variants remain available and retain all populations and horizons.

Available variants contain complete `stateDistributions` and `exclusions`.
Probabilities use `estimate`, never integer `count`. `originalSampleSize` and
horizon-specific exclusion counts preserve the original denominator; retained
size is recoverable as original minus both exclusions. No retained-size field
is invented. The base payload is the `adjusted:adjusted_as_negative` variant.
Preparation and serialization remain internal; no public report is wired.

## Numerical serialization boundary

The package returns `event_free = -1.1102230246251565e-16` for Fixture A's
`excluded:adjusted_as_negative` at horizons 14 and 15. It matches independent R
zero within the reference tolerance, but the published integrity check requires
exact `[0, 1]` bounds. Preparation retains the original output. Serialization
clips only endpoint roundoff within eight floating-point epsilons; larger bound
violations, nonfinite values, or missing estimates for defined rows are errors.
There is no renormalization or recomputation of event probabilities. Independent
reference artifacts and expectations are unchanged.
