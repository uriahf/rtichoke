# Independent terminal-event reference contract

PR 1 contains test/reference data only. No production statistics, APIs, report
builders, or renderer code is changed. This fixture is language-neutral and can
be consumed by later R and Python adoption work.

## Regeneration

```sh
Rscript tests/fixtures/terminal_event/generate_reference.R tests/fixtures/terminal_event/reference.json
RSCRIPT=Rscript uv run pytest tests/test_terminal_event_reference.py
```

Required R packages: `survival`, `cmprsk`, `jsonlite`. The generated artifact
records their versions and R's version. Initial generation used R 4.6.1,
survival 3.8-12, cmprsk 2.2-12, jsonlite 2.0.0. CI regenerates using its installed
versions and compares numerical results with absolute tolerance 1e-12, rather
than requiring identical version strings. No Python or rtichoke estimator is
called by the R generator. No new production dependency is introduced.

## Contract resolution

At every horizon, independently retain original subjects except original
censoring strictly before the horizon when censoring is excluded, and competing
events at or before the horizon when competing events are excluded. Only then
recode competing events as censoring or composite events. Retain future events.
Exclusion counts refer to original subjects, not estimated masses. The original
sample size remains the denominator for exclusion percentages.

**Horizon zero is an ordinary right-continuous evaluation time.** Target and
competing events at zero count; original censoring at zero is retained. A
synthetic pre-event baseline is a different context and must not overwrite the
statistical estimate at zero. This follows the agreed `event time <= horizon`
and `original censor time < horizon` predicates, including at zero. The zero-tie
cohort gives AJ `(primary, competing, event_free) = (1/4, 1/4, 1/2)` at zero;
excluding competing events gives target KM risk 1/3, with A/C/D retained.

**An empty retained population has undefined probabilities.** Every state is
explicitly null and status is `undefined_empty_population`; a distribution of
zero event risk and unit event-free probability is not substituted. There is no
estimator call on empty data. This resolves reference semantics only; no public
API or serializer policy is changed by this PR.

**Beyond observed support is distinct from an empty population.** Package
plateau values are recorded unchanged. A horizon beyond the last retained
follow-up with positive remaining event-free probability is flagged
`beyond_support_plateau`. These numbers are not asserted to identify risk after
follow-up ends. Absorbed distributions with zero event-free mass remain defined.
No arbitrary nulling or extrapolation formula is introduced.

KM distributions are `(primary, event_free)` or `(composite, event_free)`.
Competing-as-negative distributions are full AJ `(primary, competing, event_free)`;
competing probability is not folded into event-free. Fits use `survfit()`;
AJ cause probabilities are additionally checked against `cmprsk::cuminc()`.
Before the first fitted observation, use the initial event-free distribution;
at a fitted time, read the right-continuous package estimate. Absent event causes
have structural zero probability. For no transitions, survival 3.8-12 errors on
a multistate response, so the generator fits the degenerate binary survival
process and adds structural zero cause probabilities. `cuminc()` is not called
on empty or entirely censored data, where no cause curve exists.

## Coverage and independent checks

All eight pairs are generated for Fixture A and six boundary cohorts:
zero-time target/competing/censoring ties, all-censored data, absent competing
events, absent target events, all-competing data, and nonzero simultaneous
target/competing/censoring ties. Horizons include before first observation,
exact events and censoring, between observations, last observation, and beyond
follow-up. IDs, both exclusion categories, counts, full probabilities, support
status, and applicable cuminc estimates are committed for every combination.

Tests pin all eight complete Fixture A vectors at 5 and 10 and their retained
IDs. Every reference row is checked for retention, count accounting, state
completeness, and normalization. Nonempty rows are compared with the existing
pooled primary-risk producer and polarstate's complete state vectors; existing
exclusion extraction is also checked. Cutoff-binned performance totals are never
used as a cohort denominator. CI regenerates and verifies the full artifact.

## Discrepancies deliberately left unresolved in production

1. Python `_compute_population_event_risk_times()` returns finite `0.0` for empty
   retained populations. References remain null/undefined. Strict expected-failure
   tests assert the undefined contract; they fail on unexpected success and only
   accept assertion failures, so unrelated estimator exceptions remain failures.
2. The audited R `R/reals_distribution_times.R` explicitly forces an event-free
   baseline at zero, including when events occur at zero. That differs from the
   terminal-event boundary contract here. Python's raw builder applies `<= 0`
   event predicates. R production is not modified or executed by this Python PR;
   the zero-tie reference is the required oracle for later R adoption.
3. The scalar Python risk producer carries no support-status metadata for a
   beyond-support plateau. This PR records that distinction in references only;
   presentation policy remains outside this reference-test change.

Expected results are not adjusted to match these behaviors. No statistical
differences were found in nonempty terminal-event numerical comparisons in the
covered cases. Longitudinal multistate references remain separate and unchanged.
