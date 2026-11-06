# ZD-112 Terminal Gates Receipt

Standing: UNVERIFIED

- Repo: /Users/sac/ggen @ 468291ae336452d387ad4cf7ec14f08153a0f93d (confirmed via `git rev-parse HEAD`)
- Date: 2026-11-04
- Tree at start: clean (`git status --porcelain` = 0 entries)
- Lane: verification (read-only + gates; no source edits, no git transitions)

## Gate results

| # | Gate | Expected | Actual | Exit | Verdict |
|---|------|----------|--------|------|---------|
| 1 | `cargo fmt --all -- --check` | clean | fmt drift in `crates/ggen-abb-sbb/tests/depgraph_hardening_test.rs` and `crates/ggen-abb-sbb/tests/ea_projection_test.rs` (multi-line `.expect()` chains not wrapped; ~12 hunks) | 1 | FAIL |
| 2 | `cargo check --workspace` | 0 | Finished dev profile in 7.42s, no errors | 0 | PASS |
| 3 | `git diff --check` | clean | clean | 0 | PASS |
| 4 | `python3 book/scripts/validate_gap_closure.py` | errors=0 | `obligations=26 declared_gaps=26 evidence_records=18 used_evidence=18 errors=0` | 0 | PASS |
| 5 | `cargo test -p ggen-abb-sbb` | all pass | 11 suites, 0 failures (113 tests total across suites); fmt-regen convergence holds — generated refusals.rs fmt-stable post-commit | 0 | PASS |
| 6 | `cargo test -p ggen-config` | all pass | 17 suites, 0 failures (188 tests); parity + gate-count hold | 0 | PASS |
| 7 | `cd self-host && ggen sync run` | exit 0 | exit 1: `[FM-WRITE-005] /Users/sac/ggen/self-host/generated/AUTHORITY_MAP.md exists with differing content; refusing silent clobber. Remediation: set force: true` | 1 | FAIL |

## Findings

### Gate 1 (fmt) — real failure, same crate as gate 5's convergence claim

`cargo fmt --check` fails on two committed test files in ggen-abb-sbb
(`depgraph_hardening_test.rs`, `ea_projection_test.rs`). These are hand-written test
files, not generated. The generated `refusals.rs` is fmt-stable (gate 5 proves
convergence for generated output); the drift is in committed hand-written tests only.
Note the test files themselves pass their tests (gate 5) while being fmt-dirty.

### Gate 7 (self-host sync) — content clobber refusal, not the predicted C21 revision drift

Predicted failure mode (one-commit `--check` drift from the re-pin commit, gate 010
content check) did NOT materialize as predicted. Actual failure is different:
`FM-WRITE-005` on `self-host/generated/AUTHORITY_MAP.md` — the manifest wants to
write content that differs from the committed file. This is a content divergence,
not a revision divergence. The gate refused loudly (no silent clobber) — the gate
itself behaved correctly; the committed tree is out of sync with its own manifest.
Remediation path (NOT taken — no-edit lane): regenerate with `force: true` and
commit, or reconcile the manifest.

### Side effect of gate 5 (disclosed)

`cargo test -p ggen-abb-sbb` runs the ea_projection example sync, which appended
generation 69 receipt entries, dirtying:
- `crates/ggen-abb-sbb/examples/ea_projection/.ggen-v2/receipt-log.jsonl` (+4 lines)
- `crates/ggen-abb-sbb/examples/ea_projection/.ggen-v2/receipt.json` (generation 65→69, new chain hashes; `ea.graph.json` output hash unchanged = `a5aa32cc…`, decisions `skipped: unchanged: content identical`)

Graph/output content unchanged; only receipt bookkeeping (generation counter, chain
hashes) advanced. This is inherent to running the abb-sbb tests, not a lane error.
Left in place per no-git instruction; coordinator to reconcile at integration.

## Verdict

**UNSAFE** — 2 of 7 gates fail on the committed tree:

1. fmt drift in committed ggen-abb-sbb hand-written test files (gate 1)
2. self-host sync refuses with FM-WRITE-005 on AUTHORITY_MAP.md content divergence (gate 7)

The predicted C21 one-commit `--check` drift did not occur; the actual gate-7 failure
is a distinct, previously-unpredicted content divergence. Both failures are honest,
typed refusals/diffs — no silent corruption.
