# XD-18: CI Residual Fixes (XB-1's Four Mapped Fixes)

Standing: PASS

- Repo: `/Users/sac/ggen`, branch `v26-11-3-wave`, base HEAD `87d04ccea6f931171f71aa74e290fff680ce6f1d`
- Lane: XD-18; owns `book/gap-closure.toml`, three workflow files, this receipt.
- No XB-1 receipt found in `docs/jira/v26.11.3/receipts/` (`ls | grep -i xb` → empty);
  fixes applied from the coordinator brief directly.

## Fix 1 — book/gap-closure.toml:184 stale evidence marker

- **Before**: `[evidence."real-cli-harness"] marker = "CliHarness::cargo_bin"` — no such
  symbol exists in `crates/ggen-engine/tests/composed_packs_e2e.rs` (verified: the file uses
  `CliHarness::from_path(ggen_bin())` at lines 181, 256, 376, 396, 449).
- **After**: `marker = "CliHarness::from_path"`.
- **Gate**: `python3 book/scripts/validate_gap_closure.py` →
  `GAP_CLOSURE obligations=25 declared_gaps=25 evidence_records=17 used_evidence=17 errors=0`,
  exit 0. Re-run after edit; also confirmed no `cargo_bin` remains in the file
  (`grep -n cargo_bin book/gap-closure.toml` → no matches).

## Fix 2 — examples-core-team-constructor.yml:236-260 repair block

- **Before**: `if (cd examples/interview-sandbox && "$GGEN_BIN" receipt history); then
  echo 'expected the known invalid head to refuse before repair' >&2; exit 1; fi` followed by
  an unconditional Python repair that asserts the poisoned head hash
  `ff1aebcb…` is the log tail, then truncates it and rewrites `receipt.json`.
- **After**: `if ! (cd examples/interview-sandbox && "$GGEN_BIN" receipt history); then
  python3 - <<'PY' … PY; (cd … && "$GGEN_BIN" receipt history); fi` — if the head is already
  valid (repair landed in-tree, a3e01321c), the whole repair is skipped; repair runs only
  when history actually refuses. Heredoc delimiter `PY` remains at column 0, so heredoc
  parsing is unaffected by the new `if` indentation.
- **Gate**: `bash -n` on the extracted `run:` script → OK (exit 0); YAML parse → OK.

## Fix 3 — examples-repair-constructor.yml:80-104 heredoc

- **Before**: the live-examples checkout-repair block asserted `assert old in s` against
  `.github/workflows/live-examples.yml`; the "Checkout" → "Checkout exact subject" repair
  already landed in-tree (verified: live-examples.yml contains "Checkout exact subject"),
  so the step would fail.
- **After**: `assert old in s or new in s` + no-op `s.replace(old, new)` — mirrors the
  tai-enterprise-rebuild block pattern in the same heredoc (lines 83-87).
- **Gate**: `bash -n` extracted script → OK; YAML parse → OK.

## Fix 4 — fortune5-bblock-normalize.yml:58-67 fmt scoping

- **Before**: `cargo fmt --manifest-path Cargo.toml --package ggen-cli-lib` formats the whole
  ggen-cli-lib package while the commit step's allowlist regex only guards
  `cmds/(bblock|mod).rs` + the fortune5 pack files — package-wide fmt debt would either fail
  the allowlist check or tempt allowlist widening.
- **After**: direct rustfmt under the pinned toolchain:
  `rustup run nightly-2026-06-22 rustfmt --edition 2021 crates/ggen-cli/src/cmds/bblock.rs
  crates/ggen-cli/src/cmds/mod.rs` (toolchain pinned in `rust-toolchain.toml`; workspace
  edition = 2021, `Cargo.toml:4`). Allowlist NOT widened — XD-13's sweep owns package fmt debt.
- **Gate**: ran the exact command with `--check` locally → exit 0 (both files already
  rustfmt-clean under the pinned toolchain); YAML parse → OK.

## Verification ladder (all run this session)

| Gate | Command | Exit |
|---|---|---|
| Gap-closure validator | `python3 book/scripts/validate_gap_closure.py` | 0 (`errors=0`, 25/25 obligations) |
| YAML parse ×3 | `python3 -c yaml.safe_load` per workflow | 0 ×3 |
| bash -n, embedded scripts | extracted every bash `run:` block from all 3 workflows (23 scripts) | 0 ×23 |
| fmt non-vacuity | `rustup run nightly-2026-06-22 rustfmt --edition 2021 --check` on the two owned files | 0 |

## Concurrency disclosure

Working tree carried many pre-existing uncommitted modifications from other lanes
(`.github/workflows/clap-noun-verb-zero-code.yml` and broad crates/, examples/ edits —
consistent with XD-14/XD-13 in-flight work). Left untouched. My diff touches only the four
owned files plus this receipt.
