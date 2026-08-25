# rtichoke Agent Information

This document provides guidance for AI agents working on the `rtichoke`
R repository.

## PR completion protocol

Do not consider an implementation task complete merely because code has
been pushed or a pull request has been opened.

After creating or updating a pull request:

1.  Inspect all required GitHub Actions checks for the current PR head.
2.  If checks are still running, re-check them while the session is
    active rather than handing the PR back to the user for manual
    monitoring.
3.  If a required check fails, inspect the failing job and logs and
    determine whether the failure is caused by the PR.
4.  If the fix is within the stated task scope, make the fix, push it,
    and inspect CI again.
5.  Repeat the diagnose/fix/re-check loop until all required checks pass
    or a genuine blocker requires user input.

Escalate to the user only when resolving the failure would require one
or more of the following:

- changing frozen statistical semantics, contracts, or architecture;
- broadening the agreed task scope;
- weakening or removing a meaningful test or quality gate;
- changing a public API or backward-compatibility promise beyond the
  task;
- making a product or technical decision with multiple legitimate
  choices;
- resolving an external service, permissions, infrastructure, or
  credential problem that the agent cannot fix safely.

Routine failures such as lint errors, formatting errors, test
regressions caused by the PR, snapshots/fixtures that legitimately need
updating, packaging errors, documentation-build errors, and similar
mechanical issues should be fixed without asking the user to manually
inspect GitHub Actions.

The final handoff should include:

- pull request link;
- final PR head commit;
- tests/checks run locally when applicable;
- final GitHub Actions status;
- any remaining caveats or blockers.

Do not ask the user to manually check whether CI passed.

## Vendored dependency upgrades

When upgrading a vendored dependency:

1.  Search the entire repository for the previous version, tag, source
    commit, archive name, checksum, and dependency path.
2.  Classify each match before changing it; some consumers may
    intentionally remain on an older version for compatibility.
3.  Keep the archive, extracted payload, `MANIFEST`, `PROVENANCE`,
    verification script, integrity tests, runtime dependency version,
    and workflow pins synchronized in one change.
4.  Run the full package test and check suite in addition to focused
    feature tests.
5.  Inspect failing CI logs and artifacts before changing code or
    weakening a test. A browser timeout may be transient; corroborate it
    with dedicated browser acceptance and rerun the failed job when
    appropriate.
6.  Prefer one verified implementation commit over temporary bootstrap
    or “trigger CI” commits.
