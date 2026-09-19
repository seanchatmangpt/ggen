# ARD v26.9.18 — GALL-001: Semantic Pack Receipt Replay

**Status:** DRAFT ARCHITECTURE SPEC  
**Release:** v26.9.18  
**Repository:** `seanchatmangpt/ggen`  
**Owner:** ggen / ggen-engine  
**Dependencies:** None  
**Authority ceiling:** CONSTRUCT only

## Architectural objective

A portable receipt whose replay PASS is an observed clean-room manufacture result rather than a declaration.

The architecture follows the Chatman separation laws:

[
Received \neq Admitted,\quad Candidate \neq Authority,\quad SELECT \neq CONSTRUCT \neq DO
]

and every consequence/evidence claim is bounded by exact subject identity and replayable receipts.

## Load-bearing components

- `crates/ggen-engine/src/portable_receipt.rs` — portable envelope owner
- `crates/ggen-engine/src/replay.rs` — proposed replay primitive
- `crates/ggen-engine/src/pack.rs` — PackDigest and dependency closure
- `crates/ggen-engine/src/sync.rs` — manufacture integration
- `crates/ggen-engine/tests/gall_checkpoint_001_replay_e2e.rs` — Chicago court

## Data / control flow

`Pack/graph + declared deps -> admission -> deterministic sync -> portable receipt -> clean replay -> independent identity recomputation -> replay verdict`

## Interfaces

- Input: admitted pack/project root + exact dependency closure
- Output: `.ggen-v2/receipt-portable.json` with replay observation
- Downstream: GALL-002 and GALL-005 consume receipt digest/subject

## Required invariants

1. Extend the existing portable receipt; do not introduce a competing receipt format.
2. Replay an exact admitted pack from clean output state using only canonical pack/graph/dependency/toolchain inputs.
3. Independently recompute PackDigest, dependency identities, canonical graph identity and consequence SHA-256 values.
4. Require exact consequence-set equality by path, operation, digest and cardinality.
5. Typed-refuse stale receipts, mutated packs, mutated dependency closure, admission-refused subjects and ambiguous/ambient dependencies.
6. Record exact repo SHA, Cargo.lock/toolchain identity, replay command, exits and witnesses.
7. Keep ggen limited to manufacture/evidence emission; no process-mining or actuation authority.

## Failure and refusal boundaries

- Dependency exists only ambiently => REFUSED/BLOCKED
- Replay requires first-run output as hidden input => REFUSED
- Graph/toolchain identity unavailable => BLOCKED
- Admission refused => replay cannot PASS

A refusal is a valid architectural result. The implementation MUST NOT add model inference, private state, ambient dependencies, alternate authority paths or hand-written generated projections merely to make a court green.

## Repository-native qualification court

- `cargo fmt --all -- --check`
- `cargo test -p ggen-engine --test portable_receipt_e2e`
- `cargo test -p ggen-engine --test pack_dependency_scope_e2e`
- `cargo test -p ggen-engine --test gall_checkpoint_001_replay_e2e`
- `cargo test -p ggen-engine`

Each command is recorded with exact head SHA, relevant lock/toolchain identities, exit status and artifact digests. A later run against a different subject does not inherit this standing.

## Evidence contract

The checkpoint receipt MUST contain enough identity to let the next boundary validate:

- producer repository and exact SHA;
- semantic/manufacturer/runtime subject as applicable;
- predecessor receipt digests;
- court/falsifier identities;
- exact output artifact digests;
- standing and evidence ceiling.

## Security / authority

Authority is never inferred from capability, model output, successful parsing, observation, conformance, generated source or prior execution. Secrets and bearer credentials are never embedded into cross-repository evidence receipts; only opaque grant/principal identities needed for correlation are allowed.

## Definition of architectural closure

The architecture is closed only when the positive witness executes and every required negative witness is actually attempted against the exact subject. Configuration, source inspection or absence of a violation without an attempted falsifier is insufficient.
