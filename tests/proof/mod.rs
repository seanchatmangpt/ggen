// Lane A: Proof tests
// Release-gate tests that validate:
// - Smoke tests (system boots and CLI is reachable)
// - Invariants (manifest rules, audit trails, determinism)
// - Refusal states (negative paths, expected errors)
// - Receipts (cryptographic proof of execution)
//
// These run by default (no --features integration required).

mod smoke;
mod invariants;
mod refusal;
mod receipts;
