// Lane A: Proof tests
// Release-gate tests that validate:
// - Smoke tests (system boots and CLI is reachable)
// - Invariants (manifest rules, audit trails, determinism)
// - Refusal states (negative paths, expected errors)
// - Receipts (cryptographic proof of execution)

#[cfg(test)]
mod smoke;
#[cfg(test)]
mod invariants;
#[cfg(test)]
mod refusal;
#[cfg(test)]
mod receipts;
