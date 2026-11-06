# Receipt: x4 — WO-09 ABB/SBB Consumer Guide

Standing: ALIVE

- **Date**: 2026-11-03
- **Lane**: x4 IMPLEMENTATION — `docs/abb-sbb-consumer-guide.md` (new) +
  this receipt
- **Work order**: `docs/jira/v26.11.3/WO-09-working-example.md`
- **Standing**: ALIVE — every command below run this session with its real
  exit; outputs in the guide are real (executed) or quoted from K6's real
  run transcript, cited in place. Nothing invented; zero `[TARGET]`
  markers.

## Subject

- `/Users/sac/ggen/docs/abb-sbb-consumer-guide.md` (NEW, 9 `##` sections:
  header plumbing + 8 walkthrough sections + limitations — H1 =
  filename stem, ≤100-char lines verified, kebab-case)
- `/Users/sac/ggen/docs/jira/v26.11.3/receipts/x4-consumer-guide.md`
  (this receipt)

## What landed

The WO-09 guide, all 8 sections:

1. Author the EA graph: JSON exemplar route (canonical, working) with
   field table; Turtle route cited to WO-04's landed deliverables —
   `ontologies/shapes/ea-graph.shacl.ttl` + parity court
   (receipt `g7-schema.md`) — with the honest note that WO-04 did NOT
   land the Turtle→JSON projection rule (its own receipt says so).
2. Plan per ABB: decision law verbatim ("lowest-id admissible candidate
   wins", `src/lib.rs:847`); Select + Manufacture examples — the Select
   output executed this session (exit 0), the Manufacture-with-refusals
   output quoted from K6's real run (`ea-run-transcript.md` §2, cited).
3. Admit at CONSTRUCT: `expected_graph_digest` pinning, `admit_with_caps`
   envelope caps (min of contract and override, clamp 64), typed refusal
   shapes verbatim from the `Refusal` enum, gate order.
4. Manufacture → receipt: provenance header anatomy (format from
   `src/lib.rs:1004`), full `ManufactureReceipt` field table matching
   `pub struct ManufactureReceipt` (`src/lib.rs:403-425`) field-for-field
   (checked against source this session, not memory); 18 fields.
5. verify_receipt + replay: tamper-evidence mechanism, the 9 compared
   fields enumerated in kernel order (`src/lib.rs:1097-1116`),
   `REPLAY_MISMATCH{field}` with the offending field named; graph_digest
   mapping.
6. envelope_court exit codes: 0/1/2 table, vacuity-never-passes law,
   non-vacuity (courts flip), freshness delegation to verify_receipt.
7. Sync integration: FM-LAW-015 / FM-PACK-015 fail-closed shapes,
   `--skip-ea-admission` CLI flag (K8-landed), refusal run / positive run
   outputs quoted from K6's real `ggen sync run` (exit 1 / exit 0,
   transcript §3).
8. Full loop executable: validate example over
   `crates/ggen-abb-sbb/fixtures/ea-graph.json` with FULL real output
   (exit 0, graph_digest `sha256:76752fac…828f5`), then the two test
   receipts — marketplace_fixture (1 passed; exit 0) and envelope_courts
   (4 passed; exit 0). Repo-root sync leg honestly BLOCKED-marked.

## Commands + exits (real, this session, from /Users/sac/ggen)

- `cargo run -p ggen-abb-sbb --example validate_ea_graph --
  crates/ggen-abb-sbb/fixtures/ea-graph.json` → **exit 0** (full output
  captured in guide §8; graph digest
  `sha256:76752facbad789df2fd7d11bbf2b1914a56bd232a2a63f4438c03a08cec828f5`)
- `cargo test -p ggen-abb-sbb --test marketplace_fixture` → **exit 0**,
  1 passed; 0 failed
- `cargo test -p ggen-abb-sbb --test envelope_courts` → **exit 0**,
  4 passed; 0 failed
- F1/F2 status checks: `grep -c "ea_graph\|abb_sbb\|ea\.graph"
  crates/ggen-engine/src/generation_rules.rs` → **0** (F2 open);
  python read of `.ggen-v2/receipt.json` → `record.chain_rule =
  praxis-chain/v2-fold` (F1 open)

## Blocker status (guide §9, each re-checked on disk this session)

- **F1 (chain rule legacy)** — OPEN. Head `.ggen-v2/receipt.json` still
  `chain_rule = praxis-chain/v2-fold`; engine (graphlaw 26.10.10+)
  recognizes only `graphlaw-chain/v2-fold` (const at graphlaw
  `src/receipt_chain/record.rs:461`).
- **F2 (declarative-path admission)** — OPEN. `generation_rules.rs` has
  zero EA references (grep count 0).
- **CLI skip flag (DEF-K6-3)** — CLOSED by K8
  (`receipts/k8-cli-flag-depgraph.md`): flag on the verb surface,
  threaded to `SyncOptions.skip_ea_admission`, 3 real-binary courts,
  SHIM-COURT flip witnessed.

## Acceptance vs WO-09

1. Reproduction check: Section 8's three commands executed this session
   with exits 0/0/0 recorded above (the repo-root sync leg is
   BLOCKED-marked with the two causes named — per the contract, the
   transcript's scratch-subject run is the witnessed Stage 2c execution).
2. `grep -n TARGET docs/abb-sbb-consumer-guide.md` → 0 matches.
3. Command-citation coverage: every output block is preceded by its
   command, or cites `ea-run-transcript.md` (§2/§3 quotes).
4. Coverage: `grep -c "^## "` → **9** (≥ 9 required).
5. **NOT DONE by this lane**: milestone README listing (lane law: "Do
   NOT touch other docs") — coordinator action: add
   `abb-sbb-consumer-guide` to `docs/jira/v26.11.3/README.md`.
6. Receipt-schema fidelity: table built against
   `crates/ggen-abb-sbb/src/lib.rs:403-425` read this session;
   field-for-field match; 9 replay fields enumerated in §5.

## Defects discovered (filed, not patched — docs WO)

- DEF-X4-1: repo-root `fixtures/` does not exist; the fixture lives at
  `crates/ggen-abb-sbb/fixtures/ea-graph.json`. WO-09's text names
  `fixtures/ea-graph.json` — path drift in the WO, corrected in the
  guide with the real path.
- DEF-X4-2: no CLI/library binary exposes `envelope_court` — exit codes
  are a library-API contract (`ReplayVerdict::exit_code`) witnessed by
  tests, not a runnable command. The guide presents it honestly as the
  API contract + test receipts.
- DEF-X4-3: WO-09 acceptance item 5 (README listing) is outside this
  lane's owned files — flagged for the coordinator.
