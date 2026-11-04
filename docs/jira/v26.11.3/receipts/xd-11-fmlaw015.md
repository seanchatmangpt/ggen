# XD-11 FM-LAW-015 Remediation Receipt — sync_surface_test PUB-1 Row

Lane: XD-11. Date: 2026-11-04. Repo: /Users/sac/ggen, branch `v26-11-3-wave`.
Scope files: `crates/ggen-cli/tests/sync_surface_test.rs` + the surface code it
exercises; receipt at `docs/jira/v26.11.3/receipts/xd-11-fmlaw015.md`.

## Finding: already remediated on this branch (verified, not assumed)

The task brief named 4 pre-existing `sync_surface_test` failures (FM-LAW-015
class, `ea.graph.json is absent`) disclosed in
`docs/jira/v26.11.3/receipts/sx-1-reds.md` and "never
remediated". Re-derivation on disk shows the remediation **already landed
before this lane opened**:

- **Commit**: `e9829bead` — "v26.11.3: sjira completion wave (...),
  sync_surface + perf reds fixed, CSCB pack consumer".
- **Diff**: the exact one-fixture remediation sx-1-reds.md prescribed —
  `std::fs::write(dir.path().join("ea.graph.json"), "{\"schema\":\"ggen.ea.graph.v1\",...\"candidate_sbbs\":[]...}")`
  in the shared fixture (`make_project`), tagged with the fail-closed
  WO-01 EA admission-gate comment.
- **Provenance**: `git log -1 e9829bead -- crates/ggit/tests/sync_surface_test.rs`
  names e9829bead as the last commit to touch the file (path correction:
  `crates/ggen-cli/tests/sync_surface_test.rs`); the fixture write and
  the FM-LAW-015 explanatory comment arrive in that commit's diff verbatim.
  sx-1-reds.md itself landed later (commit `c24d71b61`, docs-only), so the
  disclosure and the fix crossed in flight — the SX-1 lane disclosed 4 reds
  that a concurrent lane had already fixed.

Disclosure and remediation crossed in flight: SX-1's full-suite run observed
the pre-fix state, wrote its disclosure, and the concurrent lane's fix
(e9829bead) landed before this XD-11 lane opened.

## Verification (real runs, this session)

Subject: working tree of `v26-11-3-wave` @ `87d04ccea` (HEAD at run time;
tree also carries unrelated in-flight lane edits to
`crates/ggen-engine/src/keys.rs`, `cl-close-2.md`, `xd-2-key-perms.md` — not
mine, untouched).

| Gate | Command | Exit | Result |
|---|---|---|---|
| Suite run 0 (first observation) | `cargo test -p ggen-cli-lib --test sync_surface_test` | 0 | 6 passed, 0 failed (5–7s) |
| Green ×2 | same command ×2 | 0, 0 | 6/6 both runs |
| Full no-regression | `cargo test -p ggen-cli-lib` | 0 | 62 test binaries, all `test result: ok`; 604 passed, 0 failed |

Notes:
- The previously-failing 4 tests (`bare_sync_defaults_to_run`,
  `dry_run_exits_zero_and_writes_nothing`, `real_run_writes_outputs_and_receipt`,
  `watch_starts_and_runs_until_killed`) all pass in every run above.
- `sync.rs:431` `artifact_baseline_generation` hardcode: NOT touched (different
  debt, different lane, per task boundary).
- No source diffs were required — zero code edits by this lane.

## Standing

`cargo test -p ggen-cli-lib --test sync_surface_test` → **PASS** (green ×2 +
full suite, exits 0). The PUB-1 publish-blocking row for sync_surface_test is
resolvable: the blocking failures do not exist on `v26-11-3-wave` HEAD; the
PUB-1 row should be flipped to CLOSED with this receipt as witness, no
further code change needed.
