# DS-9 Receipt — descriptor→xaas-receipt loop witnessed end-to-end

Standing: PARTIAL_ALIVE

Lane: DS-9 (DMEDI, DEVELOP+IMPLEMENT). Repo: /Users/sac/ggen, crate
`crates/ggen-sjira`. Coordinator owns git; this lane ran no git commands.
Receipt for the sj-1 wave: `docs/jira/v26.11.3/receipts/sj-1.md` (read
first, as chartered).

## Standing

**PARTIAL_ALIVE.** The loop descriptor → contract → [external execution]
→ receipt_from_xaas → reconcile-append is witnessed end-to-end in
`crates/ggen-sjira/tests/xaas_loop_test.rs` (4 tests, all passing) — but
the executor is a SYNTHESIZED one: it reads its obligations from the
contract value itself and fabricates evidence honoring them. A real-fabric
integration is a named follow-on, never claimed here.

## What was done

- MEASURE: `descriptor.rs` carries `build_xaas_contract` +
  `receipt_from_xaas` (full ten-refusal `ReceiptRefusal` matrix, check
  order: bridge shape → digest → bridge equality → final_head → outcome →
  head_verified → alive guards). `execute.rs`'s plans do NOT carry the
  PG-2 bridge-key fields — `receipt_from_xaas`'s inputs come from the
  contract's `bridge` (all 10 BRIDGE_KEYS + requires lists), which the
  contract emits and the executor must echo verbatim on the executed
  receipt. No src changes were needed: the descriptor surface already
  closes the loop.
- DEVELOP+IMPLEMENT: `tests/xaas_loop_test.rs` (Chicago — real frontier,
  admit, digest, ledger collaborators; state assertions; no mocks):
  - `loop_over_two_orders_contract_to_ledger_append`: 2 admitted orders
    (real frontier + admit kernel) → `build_xaas_contract` → simulated
    executor reads `bridge.requires` from the contract itself → seals a
    receipt (`receipt_digest` stamped per the sealing law) →
    `receipt_from_xaas` verifies (carried digest == executor-stamped
    digest — the round-trip) → `ledger::append` stamps seq 1,2 into a real
    dir-ledger → `ledger::fetch` verifies every `event_digest` on replay;
    distinct orders yield distinct carried digests.
  - Refusal matrix now witnessed END-TO-END (real contract → tamper →
    re-seal → refuse), not unit-only: tampered bridge (one flipped byte in
    the receipt's embedded bridge copy) → `BridgeMismatch`; ALIVE outcome
    with verifier fail → `AliveWithoutVerifierPass`; unsupported outcome →
    `UnsupportedOutcome("teleported")`. Re-sealing after tamper isolates
    the target refusal from `ReceiptDigestMismatch` — the tampered receipt
    is internally consistent, refused on the named law.
- Disclosure: reconcile.rs (Slice 3) landed mid-flight from another lane
  but the full reconcile compose is not consumed by this test — the
  append hop closes through `ledger::append` + `ledger::fetch` verify
  directly. Composing the new reconcile.rs is the natural successor slice.
- Mid-flight note (compile-freeze SLA): three transient compile aborts
  from concurrent lanes' in-flight src/test files (pack_health.rs,
  generated.rs, execute_test/projection_test) — none owned by DS-9, none
  fixed by DS-9 (owners fixed them within the SLA window; DS-9 waited and
  re-ran).

## Commands + exits

| Command | Exit | Result |
|---|---|---|
| `cargo test -p ggen-sjira --test xaas_loop_test` | 0 | 4 passed, 0 failed |
| `cargo test -p ggen-sjira` (full gate) | 0 | 211 tests passed, 0 failed across 19 binaries (19th: unit binary 0/0 — lib tests run in-bin) |

## Falsifier boundary

- What this witness proves: the loop's contract law and the ten-refusal
  receipt law hold END-TO-END with real kernel/digest/ledger machinery,
  when the executor is honest or dishonest in the three named ways.
- What it does NOT prove: any real fabric behavior. The executor is a
  simulation; `fabric_verifier`, `final_head`, acceptance/falsifier
  outcomes are synthesized, not observed. Named follow-on: a real
  consumer witness (real fabric integration) — separate slice.
