# industry-financial-ledger

Industry pack (financial services & banking) for the smalt emulator
faces: a FINREC-style double-entry ledger vocabulary authored locally
(FIBO's full structure is not minimally expressible without importing
its finance upper ontology, so the pack authors minimal bookkeeping
terms instead), grounded in the real landed smalt surface:

- Cosmos = CAS ledgers: `lib/smalt/data_plane/cosmos.ex:133` (ETag CAS),
  `lib/smalt/egress/CosmosRoutes.ex:18` (`POST /app/transactions`)
- SB = settlement workflow queues (5672, SQL filter rules)
- EH = audit telemetry (5673, consumer groups
  `lib/smalt/messaging/event_hubs.ex:847-917`)
- KV = keys/sign (PS256, `lib/smalt/data_plane/kv_pss.ex`)

## Vocabulary

| Term | Meaning |
|---|---|
| `fin:Account` | ledger account; the Cosmos document whose `_etag` is the CAS token |
| `fin:JournalEntry` | double-entry transaction; balanced = sum(debits) == sum(credits) |
| `fin:Posting` | one debit or credit leg against one account |
| `fin:casEtag` | required per-entry `_etag` (If-Match concurrency law) |
| `fin:SettlementStage` | workflow stage bound to one SB queue + EH audit topic |

Individuals `fin:Capture/Enrich/Settle/Audit` bind the settlement
workflow queues `settle-capture → settle-enrich → settle-settle →
settle-audit`, all auditing to `eh-audit-ledger`.

## Templates

- `templates/ledger_shapes.ttl.tmpl` — SHACL shapes; the balance law is
  pinned as a SPARQL ASK gate (ASK true = unbalanced = violation)
- `templates/client_walkthrough.md.tmpl` — browser console →
  `smaltFetch` → Cosmos ETag-CAS entry commit → SB settlement queues →
  EH audit telemetry

## Gates (SPARQL refusal gates; any row = refusal)

| Gate | Pillar | Law |
|---|---|---|
| `010_unbalanced_entry` | — | sum(debit) != sum(credit) |
| `020_missing_account_ref` | — | missing/dangling account ref, <2 legs |
| `030_duplicate_entry_id` | — | duplicate `fin:entryId` |
| `040_ledger_isolation` | P1 | every JournalEntry MUST reference an isolated `fin:LedgerStore` with a per-account `fin:journalPath` |
| `050_skr_attestation` | P2 | settlement-stage completions MUST bind `fin:attestationFingerprint` (AZ-2 shape) |
| `060_proposer_judge` | P3 | AI-categorized entries MUST be `fin:JudgeActivity`-adjudicated before commit |
| `070_merkle_seal_missing` | P5 | every settled batch MUST reference a sealed sha256 Merkle root |

Negative-witness fixtures: `witnesses/pass/` (all gates silent) and
`witnesses/fail/` (exactly the named gate fires); the exact-stem court is
`qualification/verify.py`:

```bash
python3 qualification/verify.py
# {"cells": [...14 cells...], "standing": "ALIVE"}
```

Pillar mapping receipt (gate -> pillar -> consumer): `PILLARS.md` in the
consumer (`ggen examples/industry/financial-ledger`).

## Qualification

`qualification/consumer.ttl` is the pack-owned positive fixture for the
marketplace qualification court.

## Routing

Browser → `smaltFetch` (`lib/smalt.ts`) → Cosmos/KV/AppConfig REST on
smalt 8443; SB workflow legs and EH audit ingest via MCP 9545 / AMQP
relay. Raw `fetch` is banned in the workspace; networking goes only
through `smaltFetch` / `assertLocalUrl`.
