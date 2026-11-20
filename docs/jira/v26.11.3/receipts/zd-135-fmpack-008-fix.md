# ZD-135 — tcps-generated FM-PACK-008 fix: evidence mini-pack de-volatilized

Standing: PARTIAL (FM-PACK-008 resolved on tcps-generated; full-ladder 53/53 blocked by a separate pre-existing FM-WRITE-005 divergence in 19 sibling examples — out of lane)

Date: 2026-11-06 · Repo: /Users/sac/ggen · Lane: ZD-135 · Tree: 3c2ee02e7 (HEAD at lane start; tree shared with other lanes — nothing committed, coordinator lands)

## Root cause (re-derived, not cited)

The `tcps-evidence` mini-pack (pack root `evidence/`) contained the EMITTED
ver:Check facts (`evidence/ontology.ttl`), which `scripts/verify.sh` rewrites
every run (timestamps, exit summaries, graph-hash binding). Full-tree pack
hashing (`pack.rs content_hash`) therefore changed the pack's blake3 digest on
every emitter run, so the committed `ggen.lock` pin could never match: the
FM-PACK-008 mismatch was structural, not a one-off stale lock. Combined with
`reflexive = true` (every successful sync appends a receipt whose graph hash
differs — the log itself is graph input), the committed tree could never
satisfy FM-PACK-008 (lock == disk) and FM-PACK-013 (evidence bound == log
tail) at the same time: verify-after-sync fixes 013 but re-breaks 008;
sync-after-verify the reverse. This is why the divergence stood at 52/53 since
ZD-127.

## Fix (consumer-local, no engine change)

- `evidence/pack.toml` + `evidence/templates/` moved to `evidence-pack/`
  (stable bytes: pack.toml, new stable stub `evidence-pack/ontology.ttl`,
  templates). Pack hash no longer covers emitted facts.
- `evidence/ontology.ttl` stays the volatile emitter output (path unchanged,
  verify.sh untouched) but now loads via `[ontology.imports]` in `ggen.toml`
  — outside every pack, so `ggen.lock` pinning never races the emitter.
- `ggen.toml`: `tcps-evidence = { path = "evidence-pack" }` + imports line.
- `.gitignore`: `examples/tcps-generated/.ggen-v2/` now ignored and untracked
  (4 files). Reason: reflexive=true appends a new receipt line per sync, so a
  committed log can never be byte-idempotent under the validator's check mode.
  The committed `ggen.lock` itself stays tracked (xd-15/zd-126 contract:
  `tests/product_evidence` requires the committed 8-pack lock at HEAD).

## Convergence procedure (the state the landing must reproduce)

Any change to evidence/packs must end with: sync → verify.sh (binds evidence
to the current log tail) → land, NO sync after the final verify. Running a
sync after the last verify re-opens FM-PACK-013 (gate compares evidence-bound
hash vs log tail at load).

## Verification (real exits)

- `rm ggen.lock; ggen sync run --skip-ea-admission` (from example dir, tree
  binary) — exit 0 (lock re-minted, 8 packs).
- `bash scripts/verify.sh` — exit 0; evidence bound hash == receipt-log tail
  (`9b563997…` both sides, re-read from disk).
- Validator: `GGEN_BIN=target/debug/ggen bash scripts/validate-examples.sh`
  — tcps-generated **ALIVE**; FM-PACK-008 appears **zero times** in the run
  log (`/tmp/zd135-val4.log`).
- zd-126 contract: `CARGO_TARGET_DIR=target cargo test --test
  tcps_chicago_tdd_1000x` — 16 passed; 0 failed. `--test tcps_chicago_tdd_8pow4`
  — 17 passed; 0 failed.
- Lane diff cleanliness: `git status --porcelain examples/` = exactly the
  13-entry tcps change set; all non-tcps sync side effects restored after
  each validator run (`git restore -- examples/ ':(exclude)examples/tcps-generated'`).

## Blocker (typed) — remaining 19/53, separate divergence

At the committed baseline BEFORE any tcps edit (witnessed
`/tmp/zd135-val2.log`, 20/53 → after tcps fix 19/53), 19 examples refuse
`[FM-WRITE-005]` (generated file exists with differing content → silent
clobber refused): each has a tracked generated file byte-stale vs current
template output (e.g. `examples/dspy-basic-qa/src/dspy_optimize.py`,
`examples/nextjs-ai-sdk/package.json`). Same class as zd-126 Root cause 2
(trailing-newline template law). Fixing 19 consumers' stale generated files is
a separate lane; until it lands, the full ladder exits 1 at 34/53 sync-pass.

## Standing

PARTIAL. ZD-135's named defect (tcps-generated FM-PACK-008) is resolved with
real exits above; full-ladder 53/53 is blocked by the FM-WRITE-005 divergence
in 19 sibling examples (register it as its own item; KNOWN_DIVERGENCES.md row
should be updated by the coordinator on landing).
