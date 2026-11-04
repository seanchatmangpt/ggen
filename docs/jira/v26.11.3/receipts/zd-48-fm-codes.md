# zd-48 — FM-code census triage + 8 Chicago courts (CARRY-09, GAP-3 residue)

Standing: courts green x2 (lane-local build root)

- **Repo/branch**: `/Users/sac/ggen`, `v26-11-3-wave` (no git commands run; working-tree edits only)
- **Files owned**: `docs/FM_REGISTRY.md` (tested_by markers + header counts + one court-directed missing row, disclosed), 5 new test files, this receipt
- **Date**: 2026-11-04

## What ran (ALIVE receipts)

- Courts green ×2, on a lane-local build root (`CARGO_TARGET_DIR=/tmp/zd48-target`) —
  the shared `target/debug` was being swept mid-build by a concurrent lane (SIGKILL'd
  aws-lc-sys/rustix build scripts, sccache watching vanishing rmeta). The first full
  green run happened on the shared root before that sweep; the ×2 rerun used the lane root.

```
cargo test -p ggen-engine --test fm_config_014_gate_glob_court \
  --test fm_tpl_frontmatter_parse_court --test fm_tpl_006_sync_court \
  --test fm_gen_merge_refusal_court --test fm_write_001_root_court
→ run 1: 8 passed / 0 failed across 5 files
→ run 2: 8 passed / 0 failed (grep -c "test result: ok" = 5)
```

- Parity court:

```
cargo test -p ggen-engine --test fm_registry_parity_test
→ before lane edits: FAILED (pre-existing on HEAD: FM-KEY-013 missing from registry)
→ after: 3 passed / 0 failed
```

## The 8 courts (all real pipeline, real files, no mocks)

| Code | Court file (`crates/ggen-engine/tests/`) | Trigger | Extra asserts beyond the code |
|---|---|---|---|
| FM-CONFIG-014 | `fm_config_014_gate_glob_court.rs` | full `sync::sync`; `[law].gates = ["gates/*.norq"]` (the engine frontmatter-schema gate surface — NOT `[validation].gates`, which the config layer pre-refuses as FM-CONFIG-003) | `GLOB_NO_MATCHES` + pattern named; zero writes |
| FM-TPL-001 | `fm_tpl_frontmatter_parse_court.rs` | real temp file → `Template::parse` (the exact parser sync drives; sync wraps the inner code as FM-TPL-006) | remediation text |
| FM-TPL-012 | same | `---to:` glued delimiter | "own line" |
| FM-TPL-013 | same | unterminated frontmatter | "closing `---`" |
| FM-TPL-006 | `fm_tpl_006_sync_court.rs` | full sync; template that starts `---` but fails parse. Note: a frontmatter-less template is deliberately skipped (rules-consumed convention, `sync.rs::load_templates`) — the naive trigger does NOT fire FM-TPL-006 | file named; zero writes |
| FM-GEN-009 | `fm_gen_merge_refusal_court.rs` | full sync, Merge-mode rule, existing target = non-UTF-8 bytes | target named; on-disk bytes re-read unchanged after refusal |
| FM-GEN-010 | same | full sync, Merge-mode rule, `=======` before `<<<<<<< GENERATED` | fail-closed: "stale body" still on disk untouched |
| FM-WRITE-001 | `fm_write_001_root_court.rs` | `write::resolve_target` on a real deleted root (the resolver every sync write routes through) | root named |

The 8 codes: **FM-CONFIG-014, FM-TPL-001, FM-TPL-012, FM-TPL-013, FM-TPL-006,
FM-GEN-009, FM-GEN-010, FM-WRITE-001.**

## Census correction (stale task premise)

Task said "37 codes with tested_by none". Real count at session start: **52** (the
registry's own header already said 52/118; 37 was the narrower pre-SA-16 census scope).
Count at session end: **45 of 119** — 8 closed by this lane's courts, +1 new `none` row
(FM-KEY-013).

## FM-KEY-013 parity fix (outside tested_by scope, disclosed)

Parity court was **red on HEAD before this lane touched anything** — not
session-introduced. `crates/ggen-engine/src/keys.rs:410` carries the static literal
`FM-KEY-013` (a real fail-closed permission refusal, `ensure_owner_only_permissions`)
but the registry had no row: the keys.rs work landed (ed3f1dfb1) after the registry
census (12b460c2e). The registry is hand-maintained — the court's own refusal text
directs "Add one row per code (trigger + remediation from the emission site's
context)" — so the row was added from the emission-site context. Registry rows are
otherwise untouched beyond tested_by markers and the header count prose.

## Typed rulings — remaining 45 `none` codes

Classes: **U** = untestable-by-nature (typed ruling), **P** = crate-private surface
(emitting entry point unreachable from `crates/*/tests/`), **T** = testable, future
court candidate. 12 U + 21 P + 12 T = 45.

| Code | Class | Ruling basis |
|---|---|---|
| FM-CHAIN-001 | P | `ValidationChain::finish` failing-check path driven via crate-internal seam |
| FM-CHAIN-002 | P | receipt-chain hash write fault (`.ggen-v2/receipt-log.jsonl`) — crate-internal seam |
| FM-CHAIN-010 | P | epoch record read fault — crate-internal |
| FM-CHAIN-011 | P | epoch construction fault — crate-internal |
| FM-CHAIN-015 | P | portable-envelope consequence digest binding — crate-internal |
| FM-CHAIN-020 | P | `execute_replay_court` error propagation — crate-internal |
| FM-CLI-001 | P | `DefaultCliValidator` (`error.rs`), asserted only in src `#[cfg(test)]`; no tests/ construction seam |
| FM-CLI-002 | P | same validator seam |
| FM-CLI-042 | U | non-emission member (constructor round-trip witness only) — registry non-emission ledger; the parity court already pins the absence |
| FM-CLI-099 | U | same class |
| FM-GEN-012 | T | future court: template looping a body over `MAX_OUTPUT_BYTES`, full sync |
| FM-GRAPH-001 | U | in-memory store creation fault — storage-engine-internal fault injection |
| FM-GRAPH-004 | U | store iteration fault — storage-engine-internal fault injection |
| FM-GRAPH-005 | U | RDFC-1.0 canonicalization-internal label fault |
| FM-GRAPH-006 | T | future court: apply a delta after the store really changed between two stages |
| FM-GRAPH-008 | T | future court: query with a `GRAPH` clause via the real `GraphEngine` query path |
| FM-GRAPH-009 | T | future court: UPDATE-looking query text via the real query path |
| FM-KEY-002 | P | `resolve_signing_key` is crate-private; self-heal lifecycle unreachable from tests/ |
| FM-KEY-006 | U | keys-directory creation fault (parent permissions) — environment-gated |
| FM-KEY-010 | P | same crate-private key-resolution seam |
| FM-KEY-011 | P | same crate-private key-resolution seam |
| FM-KEY-012 | U | `.gitignore` write fault other than AlreadyExists — environment-gated |
| FM-KEY-013 | P | refusal needs a pre-existing group/other-readable `signing.key` under the crate-private `resolve_signing_key` seam |
| FM-LAW-006 | P | law-engine rule-load refusal — backend inputs not re-exported for tests/ |
| FM-LAW-008 | P | SHACL shapes refusal (`graph.rs` path) — same seam |
| FM-LAW-009 | P | reasoner materialization fault — same seam |
| FM-PACK-006 | U | pack file unreadable in the window between resolve and read — fault injection between two pipeline stages, no seam |
| FM-PACK-011 | T | future court: real git pack checkout with a bad pin marker |
| FM-PACK-019 | T | future court: manifest declaring an empty pack scope, full sync |
| FM-SHELL-002 | U | external `Command::spawn` fault for a named binary — environment-gated |
| FM-TPL-010 | T | future court: SELECT without ORDER BY, strict-mode refusal, real template + query |
| FM-TPL-011 | T | future court: unreadable `rdf:` frontmatter path (real file deletion) + sync |
| FM-TPL-015 | T | future court: unreadable templates dir through the lenient glob loader |
| FM-TPL-023 | T | future court: malformed shapes file during frontmatter-rdf conformance |
| FM-TPL-026 | T | future court: engine fault during shape conformance |
| FM-TPL-027 | U | doc-registered only; no live emission site (registry non-emission ledger) |
| FM-WATCH-001 | P | watch-mode entry is crate-internal; initial-sync failure unreachable from tests/ |
| FM-WATCH-002 | U | watcher creation fault (fsevents/inotify limits, permissions) — environment-gated |
| FM-WRITE-007 | T | future court: frontmatter write path over `MAX_OUTPUT_BYTES` |
| FM-WRITE-020 | U | journal band marker in module doc; not an operator-receivable code |
| FM-WRITE-023 | P | transactional journal recovery — journal seam not re-exported for tests/ |
| FM-WRITE-024 | P | same journal seam |
| FM-WRITE-025 | P | same journal seam |
| FM-WRITE-026 | P | same journal seam |
| FM-WRITE-027 | P | same journal seam |
