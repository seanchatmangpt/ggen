# ggen v26.11.6 Wave-Closing Receipt (Lane R-GGCLOSE)

Standing: ALIVE

Date: 2026-11-06. Repo: /Users/sac/ggen (Rust), branch `v26-11-3-wave`.
Lane: R-GGCLOSE, v26.11.6 milestone closure. Read-only census + two authored files
(this receipt, `open-items.md`). No commits, no reverts, no git state mutations.

## What ggen contributed to v26.11.6

1. **The at-most-once fence (KD-G18)** — idempotent receipt-log append, closing
   arcus's receipt-log duplicate class. Commit `f569fba24` "fix(sync): idempotent
   receipt-log append (KD-G18 at-most-once fence)" on branch `v26-11-3-wave`,
   **confirmed pushed**: `origin/v26-11-3-wave` HEAD == `f569fba24` (re-read this
   session). The fence lives at `crates/ggen-engine/src/sync.rs:4624` (doc-comment
   names KD-G18) with the retry-detection needle at `sync.rs:4640` and the typed
   duplicate fence at `sync.rs:4654`; the court is
   `sync::tests::duplicate_append_is_idempotent_single_line` (`sync.rs:4712`).
2. **The v26.11.4 build** — `target/release/ggen` (37,883,360 bytes, mtime
   2026-11-06 13:31, `ggen --version` → `ggen 26.11.4`), rotated by manual `cp`
   into `~/.local/bin/ggen` with backup `~/.local/bin/ggen.pre-kdg18` (37,967,344
   bytes, same mtime — the pre-fix v26.11.4 binary, superseded by the KD-G18
   fix). Byte-identical rotation witnessed: sha256 of
   `target/release/ggen` == `~/.local/bin/ggen`
   (`97449b677783a475399ecd660c78fa92276d168cf3dcd4796ddb3ae59a4c7f0f`). The
   installed binary is what arcus consumes.

## Gate evidence

`cargo test -p ggen-engine --lib` → **exit 0**, `238 passed; 0 failed; 2
ignored; 0 measured` (1.89s, run 2026-11-06 this session). The KD-G18 appender
court runs inside that suite; filtered re-run witnessed separately:
`cargo test -p ggen-engine --lib duplicate_append` → exit 0,
`1 passed; 0 failed; 239 filtered out` (`sync::tests::duplicate_append_is_idempotent_single_line ... ok`).

## DIRTY tree census and dispositions

361 tracked files modified + 30 untracked (2413 insertions / 582 deletions across
tracked). Read and classified; nothing committed or reverted per lane contract.
Groups:

1. **Standing-line backfill (S-10/SB1) — another session's in-flight work,
   needs-landing.** The bulk (~200 files): `docs/jira/v26.11.3/receipts/*.md`,
   `docs/jira/v26.11.4/**`, sample-checked `ds-1.md` → the diff is exactly an
   inserted `Standing: UNKNOWN` line per receipt (backfill receipt exists:
   `docs/jira/v26.11.4/receipts/sb1-standing-backfill.md`, untracked). Matching
   machinery: `scripts/receipt_lint.py` (+18), new `scripts/ci/receipt_census.py`
   (untracked), `receipts/receipt-standing-map.json` regenerated, `justfile`
   adds a `lint-receipts` alias, and an uncommitted `receipt-lint` CI job in
   `.github/workflows/ci.yml` (lint leg + census leg wired into `ci-status`
   needs). Coherent single initiative — needs-landing as one commit.
2. **KD-RC recount — needs-landing.** `KNOWN_DIVERGENCES.md` gains three
   2026-11-05 "Recount (KD-RC)" blocks (R12 quick-xml OPEN confirmed with moved
   ingress path oxrdfxml/sparesults/wasm4pm-compat; R13 flag confirmed live;
   ZD-135 PARTIAL with a stale rg-absence caveat) + receipt
   `docs/jira/v26.11.4/receipts/kdrc-recount.md` (untracked). Needs-landing.
3. **ML-DSA-65 first increment (ZP-22 / OI-3) — another session's in-flight
   work, needs-landing.** New untracked crate `crates/ggen-dsa/` (Cargo.toml,
   src, tests), workspace member added in `Cargo.toml` (marked NOT
   publish-eligible), `Cargo.lock` +107 lines. Paired receipts:
   `docs/jira/v26.11.4/receipts/zp-22-mldsa65-increment.md` and
   `oi3-mldsa-phase1.md` (untracked). Needs-landing.
4. **ZP-24 tera whitespace dual-engine court — needs-landing.**
   `crates/ggen-engine/Cargo.toml` adds `tera2` (tera 2.4.0 alongside pinned
   1.20) as a dev-dep; `tests/tera_whitespace_census.rs` refactored to return
   `Result` and assert byte-equality across both engines. Receipt
   `zp-24-tera-whitespace-census.md` (untracked). Needs-landing.
5. **ZD-135 tcps evidence-pack restructure — needs-landing.**
   `.gitignore` re-anchors the tcps lock exception (tests/product_evidence
   requires the committed lock at HEAD) and ignores
   `examples/tcps-generated/.ggen-v2/` (reflexive=true appends per-sync
   receipts, never byte-idempotent); `examples/tcps-generated/ggen.toml` moves
   volatile `ver:` facts to `[ontology.imports]` outside packs so pack hashes
   (and ggen.lock) stay stable; lock diff swaps `evidence` → `evidence-pack`
   with new blake3. Receipts `zd-135-fmpack-008-fix.md`,
   `ov6-fmwrite005-sweep.md`, `zd-79-gate0-witness.md` (untracked). Coherent —
   needs-landing.
6. **Wave bookkeeping — needs-landing.** Untracked v26.11.4 closure/receipt
   files (`_LANES.md`, `overnight-wave-ledger.md`, `aud1-wave-audit.md`,
   `gg1-wip-sweep.md`, `s11-lane-registry.md`, `s12-receipt-lint.md`,
   `s6-sed-portable.md`, `s7-repin-gate.md`, `gg1-closure.md` (tracked, M),
   etc.), lane tooling (`scripts/hooks/lane-lease-check`,
   `scripts/lane_slot_map.sh`), self-host observation scripts and the
   `self-host/tests/test_observation_repin_gate.py` repin gate, plus
   examples/docs drift under `examples/` (dspy scripts, gh-enterprise docs,
   terraform, package.json/tsconfig — surfaced by prior waves). Bookkeeping +
   example drift; needs-landing in grouped commits, not abandoned.

**Classification summary: zero abandoned work found.** Every dirty group traces
to a named receipt (S-10/SB1, KD-RC, ZP-22, ZP-24, ZD-135, wave bookkeeping) —
in-flight or complete-but-uncommitted, all needs-landing.

## Open items

See `docs/jira/v26.11.6/open-items.md`. Headline: the dirty-file dispositions
above (six needs-landing groups, one landing order proposal there), and the
**binary-rotation process gap** — v26.11.4 reached `~/.local/bin` via manual
`cp` with a `ggen.pre-kdg18` backup file; no versioned release artifact, no
tag, no sha256 receipt on the installed path. Process gap, unfixed here.

## Replay

```
cd /Users/sac/ggen            # branch v26-11-3-wave, HEAD f569fba24 (== origin)
cargo test -p ggen-engine --lib          # exit 0, 238 passed / 0 failed / 2 ignored
cargo test -p ggen-engine --lib duplicate_append   # exit 0, 1 passed, court witnessed
shasum -a 256 target/release/ggen ~/.local/bin/ggen # both 97449b67...
ggen --version               # ggen 26.11.4
```

Standing: ALIVE — gate ran this session with exit 0 on the exact subject; push
state and binary identity re-read from disk, not memory.
