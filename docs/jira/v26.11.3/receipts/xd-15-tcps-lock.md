# XD-15 — tcps-generated ggen.lock MISSING_ARTIFACT (XA-1 CI root cause 3a)

Date: 2026-11-04 · Branch: v26-11-3-wave · Lane: XD-15 · Status: PARTIAL_ALIVE (artifact produced, uncommitted)

## Symptom

`product evidence + Auto Select 8^4` check fails:
`INVALID_EVIDENCE:ggen.lock:MISSING_ARTIFACT: examples/tcps-generated/ggen.lock: No such file or directory`
— 1 of 43 capability contracts refused (`manufacture.ggen-fixed-point`,
catalog: `examples/tcps-generated/tests/product_evidence/catalog.rs:119`).

## Root cause

- `examples/tcps-generated/ggen.lock` is a `ggen sync` output that the product-evidence
  tests require on disk (`tests/product_evidence/tests.rs:132` reads `ggen.lock` from
  `project_root()`; `manifest.rs:validate_ggen_lock` requires the exact 8-pack set).
- Six sibling examples commit their `ggen.lock` (cargo-cicd-verify, ggen-cli-verify,
  interview-sandbox, receiptctl, rmcp-verify, star-toml-verify) — tcps-generated did not
  have one at HEAD on this branch.
- `just verify-tcps` deletes the lock each run by design (FM-PACK-008 note in justfile
  ~line 855: the tcps-evidence mini-pack's content changes every emitter run), so the lock
  is a per-run re-lock, and it was simply absent from the tree.

## Generation (real commands)

1. `just verify-tcps` — FAILED exit 1: `verify.sh` recorded 6 checks (2 red: test-workspace
   exit 101, clippy-blanket-warnings exit 101; fmt-check exit 1 — pre-existing, out of lane)
   then `ggen sync run` refused with **[FM-LAW-015] EA admission gate: `ea.graph.json` is
   absent**. The justfile recipe is stale relative to the new EA gate (no
   `--skip-ea-admission`, no ea.graph.json in the example).
2. Workaround for this lane (artifact only, no justfile/CI edit — out of lane):
   `cd examples/tcps-generated && ../../target/debug/ggen sync run --skip-ea-admission`
   — exit 0, wrote `ggen.lock` (1288 bytes) naming exactly the 8 expected packs
   (ggen-verify-pack, tcps-{cli,core,evidence,ffi,release,std,wasm}-pack).
   Note: a first attempt at repo root hit FM-LAW-015 against the ROOT manifest
   (`sbb:docs-diataxis-pack` refused) — the command must run from the example dir.
3. Verification: `cd examples/tcps-generated && CARGO_TARGET_DIR=target cargo test
   --test tcps_chicago_tdd_8pow4` — **17 passed; 0 failed** (includes
   `ggen_lock_names_exactly_the_eight_admitted_packs`).
4. Also verified: `cargo test --test tcps_chicago_tdd_1000x` — **16 passed; 0 failed**.

## Findings for coordinator

- `just verify-tcps` (justfile ~849-863) is broken as written under the FM-LAW-015 EA
  gate; it needs `--skip-ea-admission` (or an ea.graph.json). Owner of justfile/CI
  ordering should decide, since the recipe's own FM-PACK-008 comment says the lock is
  intentionally re-made per run — an in-CI sync step may be the durable fix instead of
  committing the lock.
- If committed instead: publish-candidate.yml already excludes
  `examples/tcps-generated/ggen.lock` from its byte-identity diff (lines 63-65),
  anticipating a committed-but-volatile lock.

## Uncommitted artifacts (coordinator sweep)

- `examples/tcps-generated/ggen.lock` (new, the fix)
- Side effects of the real runs: `.ggen-v2/receipt{,-log.jsonl,-portable.json}`,
  `.ggen-v2/lease/`, `VERIFICATION.md`, `evidence/ontology.ttl`,
  `receipts/{EVIDENCE_SNAPSHOT.md,EVIDENCE_NOTE.md,inspection-receipt.json}`
- Nothing committed.
