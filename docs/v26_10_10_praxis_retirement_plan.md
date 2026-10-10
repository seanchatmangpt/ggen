# v26.10.10 Praxis Retirement Plan

Design for retiring `praxis-core` (receipt-chain family) into `praxis-graphlaw`
(graphlaw modules). Source: Lane Z analysis, 2026-10-09. Companion to
`docs/v26_10_10_phase1_receipt.md` and `docs/v26_10_10_repo_state_and_library_usage_report.md` §7.

## Blocker

The receipt-chain family (`ReceiptRecord`, `ChainVerification`, `receipt_epoch`,
Andon, ≈48 refs) has **no graphlaw equivalent** — this is the dominant praxis
retirement blocker.

## Design: port verbatim into new graphlaw modules

### `graphlaw::receipt_chain` (L, critical path)

- `ReceiptRecord` — all 17 fields, `RECEIPT_RECORD_VERSION = 1`.
- BLAKE3 chain via `build_admission_frame` / `chain_from_frame`.
- The 99-byte `OcelCausalFrame` hash bytes (from crate `bcinr-powl-receipt`)
  must be reproduced **byte-exactly**.

### `graphlaw::receipt_epoch` (L)

- `SCHEMA_V1`/`SCHEMA_V2` — `"ggen-receipt/v1"`, `"ggen-receipt/v2"`.
- `CeilingLevel` ordering: `Red < LegacyObserved < Yellow < Green`.
- `EquivalenceMap` — 8 fields, `deny_unknown_fields`.
- `AdmissionLedger` family.

### Andon (S)

Port `law::Andon` into graphlaw.

### Frozen wire discriminators

`ChainVerification`/`ChainStanding`/`ChainRule` wire discriminators
`"praxis-chain/base"` and `"praxis-chain/v2-fold"` are **FROZEN strings** — they
appear in stored JSON and must not change.

## Compatibility: dual-read, never hard cutover

Golden tests must pass as graphlaw integration tests **before** retirement:

- **FM-CHAIN-009** (`receipt_record.rs:1076`): committed TCPS example receipt;
  head hashes `d04c6d08…` / `bebae299…`.
- **FM-CHAIN-014**.

## Build order

| # | Step | Size |
|---|---|---|
| 1 | Andon port | S |
| 2 | `receipt_chain` | L (critical path) |
| 3 | ChainRule / ChainStanding / ChainMonotonicity | M |
| 4 | `receipt_epoch` | L |
| 5 | Validator + JSONL store port | M |
| 6 | praxis-core retirement + `ggen-engine` `sync.rs` / `handlers.rs` rewiring | M |

## Store note

Keep the JSONL ledger. graphlaw's `receipt_store` is a different (C21)
content-addressed surface — an optional bridge, non-blocking.

## Current praxis state on this tree (2026-10-09)

- praxis-core shim pruned to live surface (`receipt_epoch`, `receipt_record`,
  `law::Andon` + internal deps) — done this session.
- Remaining: 66 refs / 14 files, all in ggen-engine.
- `GraphLawStore` repoint in flight (lane W).
