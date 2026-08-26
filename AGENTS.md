# rtichoke repository rules

## Role and ownership

This repository owns the R implementation of rtichoke: R statistical
calculations and public APIs, performance-data preparation,
model/population/evaluation semantics, canonical visualization adapters,
immutable `rtichoke_viz` vendoring and browser-renderer adoption, Plotly and
ggplot2 compatibility, R tests, package checks, pkgdown, and CRAN-related work.

It does not own shared TypeScript visualization contracts or renderers.

## Start from fresh state

Before modifying anything, inspect actual current `main`, relevant open pull
requests and recent merges, and, when relevant, tags and releases. Check whether
equivalent work already exists. Do not work from stale assumptions. If actual
repository state materially contradicts the task, stop before broadening scope
and report the discrepancy.

## Scope and compatibility

Make the smallest required change. Do not opportunistically redesign unrelated
public APIs, change statistics outside scope, edit `rtichoke_viz` source, expand
summary-report behavior, add unrelated dependencies, or broadly refactor during
focused adoption work. Stop and report any materially broader architectural or
statistical decision.

Preserve public APIs and defaults unless explicitly requested. Do not add
Python-like or time-dependent APIs merely for cross-language symmetry. Keep
Plotly, ggplot2, and default rendering backward compatible; canonical browser
rendering remains opt-in unless explicitly changed. Reuse established renderer
vocabulary and browser infrastructure, and never silently change default return
types or renderer selection.

## Statistical and semantic boundaries

Existing R production statistical calculations are authoritative unless the
task explicitly targets a statistical bug or methodology change. Browser and
canonical-viz adapters consume already-computed R quantities: do not recompute
statistics or change cutoff, decision-curve, calibration, ROC, precision-recall,
Gains, or Lift calculations unless explicitly requested. Preserve established
behavior and regression coverage.

Preserve semantic identity:

- model identity is the prediction source when known;
- population identity is the evaluated subjects and outcomes;
- evaluation identity is semantic model × population;
- rendered series identity is distinct from evaluation identity; and
- horizon, if applicable, is context or geometry metadata, not evaluation
  identity.

Never infer population identity from numerical equality. Distinct populations
remain distinct even when prevalence or reference geometry is equal. Reuse the
current evaluation/model/population helpers rather than creating a parallel
identity system.

Population-dependent references are owned by semantic population. Multiple
models evaluated in one population share population-owned references where
required; distinct populations remain distinct owners even when their numeric
reference geometry matches. Ownership must not be derived from numeric equality.

Canonical adapters map already-computed R output into the shared contract,
preserve evaluation identity and reference ownership, and create deterministic
series geometry identity without statistical recomputation. Extend or reuse the
existing v2 builder/adapter architecture instead of adding one-off parallel
implementations.

## Immutable `rtichoke_viz` consumption

Never consume `rtichoke_viz/main`; use only immutable verified releases and the
established vendoring mechanism. Do not manually edit vendored renderer bytes.

Before vendoring a release, verify its tag, exact source commit, archive name,
SHA-256 and published checksum, `MANIFEST` version and source commit, required
packaged JS/CSS/schema files, and required public renderer exports. Search the
whole repository for stale version, tag, archive, commit, checksum, and path pins.
Classify each match, since some consumers may intentionally remain older, and
keep the archive, extracted payload, `MANIFEST`, `PROVENANCE`, verification
script, integrity tests, runtime dependency version, and workflow guards in sync.

## Validation

Inspect the actual scripts and workflows before choosing commands. Run focused
tests and the complete relevant suite, including R CMD check as appropriate;
run pkgdown/docs checks for documentation changes, real-browser acceptance for
browser-rendering changes, and packaging/provenance guards for vendored assets.
Do not weaken tests to obtain green CI. For vendoring changes, run the full
package test and check suite. Inspect CI logs and artifacts before changing code;
corroborate apparently transient browser timeouts with dedicated acceptance and
rerun when appropriate.

## Pull requests and releases

For mutation tasks, implement and validate one focused change, open one focused
pull request, and inspect all required GitHub Actions checks for its current head.
While the session is active, recheck running jobs. Diagnose failed jobs and fix
in-scope lint, formatting, tests, snapshots, packaging, docs, and similar routine
failures; push and repeat until green or genuinely blocked. Do not ask the user
to monitor CI, and do not merge unless explicitly instructed.

Escalate only when resolution requires a broader statistical, architectural,
compatibility, dependency, product, CRAN, or infrastructure decision; a public
API change; weakening a meaningful quality gate; or unavailable permissions or
credentials. Do not publish an R package release unless explicitly requested.
Consumer-adoption tasks normally stop at an unmerged focused pull request.

## Completion report

For mutation work, report starting `main`, branch and head, package version,
files changed, whether statistical behavior changed, canonical identity and
reference behavior, vendored provenance when applicable, local validation, final
CI status, pull request number/link/state, and anything deliberately deferred.
