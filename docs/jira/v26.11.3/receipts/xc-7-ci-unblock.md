# XC-7 CI Unblock Receipt

Lane: IMPLEMENTATION (XC-7, PR #824 CI unblock). Date: 2026-11-04. Repo: /Users/sac/ggen.

## Fixes

1. **ggen-graph pin** — `crates/ggen-engine/Cargo.toml:127` already carries
   `ggen-graph = { path = "../ggen-graph", version = "=26.11.3" }` (pre-landed on this
   branch). Verified live: `cargo check -p ggen-engine` resolves, exit 0. The failing CI
   log (run 41794170360) shows the pre-fix state: `failed to select a version for the
   requirement ggen-graph = "=26.10.12"` — the root cause of ~15 check failures.
2. **cargo fmt** — `cargo fmt -p ggen-sjira` applied; `cargo fmt --check -p ggen-sjira`
   clean (exit 0). Touched the two named test files.
3. **Stale constructor scripts** — three sites, each confirmed against its real CI log:
   - `examples-repair-constructor.yml` (~line 83): asserted `version = "26.7.31"` in
     `examples/tai-enterprise-rebuild/ggen.toml` — CI witness run 41796967752,
     `AssertionError: version = "26.7.31"`. The manifest was restructured upstream
     (metadata lines no longer exist; `imports` is now a legitimate `[ontology]` key —
     stripping it would break sync). Rewrote the block to drop the obsolete strip loop
     and make the pack-line augmentation idempotent (`assert old in s or new in s`).
   - `examples-core-team-constructor.yml` (~line 214): asserted
     `github.event.pull_request.head.sha || github.sha` in `live-examples.yml` — CI
     witness run 41796967512, `AssertionError` at stdin line 8. Root cause: the
     constructor's intended transformation never landed in the target file. Applied the
     constructor's exact intended block to `live-examples.yml` (exact-subject checkout
     `ref: ${{ github.event.pull_request.head.sha || github.sha }}` + PR-gated identity
     verify), making the constructor idempotent-skip with a passing assert. Pinned
     checkout SHA preserved byte-exact; all four YAMLs validated with PyYAML.
   - `agent-source-capsule.yml:31`: pathspec `crates/praxis-core crates/praxis-graphlaw`
     — CI witness run 41794170390, `fatal: pathspec 'crates/praxis-core' did not match
     any files` (both crates retired 2026-10-10, SJIRA-15; graphlaw ships via crates.io).
     Removed both entries; `git archive` with the new pathspec exits 0 locally.

## Gates (all run this session, real exits)

| Gate | Command | Exit |
|---|---|---|
| Pin resolution | `cargo check -p ggen-engine` | 0 |
| Workspace | `cargo check --workspace` | 0 |
| Tests | `cargo test -p ggen-sjira --test work_order_admission_test --test xaas_loop_test` | 0 — 12 passed + 4 passed, 0 failed |
| Format | `cargo fmt --check -p ggen-sjira` | 0 (clean) |
| Constructor sims | python blocks vs current tree: core-team skip+assert true; tai assert true; `git archive` new pathspec exit 0 | pass |

## CI check table (before, run 41794170467 wave, PR #824 head b48014a)

| Check | Before |
|---|---|
| manufacture-own-repository | fail (ggen-graph `=26.10.12` unresolvable) |
| source-capsule | fail (praxis-core pathspec) |
| Examples repair constructor | fail (`version = "26.7.31"` assert) |
| Examples core-team constructor | fail (live-examples assert) |
| Deep / docs_deep, integration_deep, lsp_deep, quality_deep | fail (cascade of pin) |
| ~10 further jobs (Exact-head clean replay, migrate, normalize, kernel, ...) | fail (cascade of pin) |
| Chicago claim and asset census / Fast Admission / Verify TPOT2 / exact-tree-observation / json-admission / pack-gall-standing / qualify / Deep security_deep | pass |

After: not yet re-run — changes are working-tree only (lane law: no git). On push, the
pin fix alone should flip the ~15 cascade failures; the three constructor fixes flip
their own jobs. Verify with `gh pr checks 824 | tail -10` after the coordinator lands
the commit.

## Standing

ALIVE locally (all gates run and exit 0 this session); CI flip UNVERIFIED until the
commit lands and the checks re-run.
