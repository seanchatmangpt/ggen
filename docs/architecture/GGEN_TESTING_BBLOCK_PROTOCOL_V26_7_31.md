# ggen Testing Building Block Protocol v26.7.31

## Preserve

Testing is a canonical Building Block, not a single undifferentiated `cargo test` command.

Stable identity:

```text
GBB-TESTING-PROTOCOL-001
```

The protocol declares 10 distinct executable suites. Each suite has its own acceptance boundary, material falsifier, evidence receipt, and standing.

## Fence

A green unit suite does not imply green integration, E2E, security, chaos, stress, benchmark, replay, or verifier-report standing.

A total test count is not a substitute for suite closure. A weighted average cannot hide one missing conjunctive suite.

The protocol separates:

```text
protocol/unit
property/fuzz
stdio + HTTP integration
black-box CLI E2E
security
chaos
stress
benchmark
replay
machine-readable verifier report
```

## Suite calculus

| Suite | Current standing | Acceptance boundary | Material falsifier |
|---|---|---|---|
| Protocol/unit | `ALIVE` | Exact identity, lifecycle, evidence, standards, composition, and receipt laws execute | Break one law and require deterministic refusal |
| Property/fuzz | `PENDING_CHECKPOINT` | Generated properties and fuzz corpora cover composition, digest, ceiling, and replay invariants | Find an admitted input that violates an invariant |
| stdio + HTTP integration | `PENDING_CHECKPOINT` | Equivalent admitted requests cross real stdio and HTTP boundaries with equivalent receipts | Observe protocol-dependent semantic, authority, output, or receipt divergence |
| Black-box CLI E2E | `ALIVE` | Build real ggen, manufacture TAI twice, execute scenarios, verify receipts | Remove one artifact, scenario, receipt, or replay edge |
| Security | `ALIVE` | BRCE-only authority, direct-actuation exclusion, tamper refusal, passport and resource bounds | Introduce direct authority, stale digest, or capability expansion |
| Chaos | `ALIVE` | Delayed contract, unavailable certification, failed inspection, founder loss, unknown scenario, and tampering execute | Allow injected failure to disappear or self-heal without receipt |
| Stress | `PENDING_CHECKPOINT` | Maximum admitted registry, closure, evidence-ledger, and projection cardinalities execute within ceilings | Exceed a ceiling without typed refusal or degradation receipt |
| Benchmark | `PENDING_CHECKPOINT` | Manufacture, composition, simulation, verification, and replay are measured against declared baselines | Accept regression without exact environment, input digest, baseline, and report |
| Replay | `ALIVE` | Standards, composition, artifact, scenario, and certification receipts recompute from exact inputs | Ambient paths, ordering, timestamps, or mutable state change semantic output |
| Verifier report | `ALIVE` | Machine-readable reports expose exact standing, exclusions, profile digest, and unresolved suites | Omit exact identity, digest, suite standing, broker, or pending count |

## Standing

```text
6 ALIVE
4 PENDING_CHECKPOINT
10 TOTAL
Testing Building Block = PARTIAL_ALIVE
```

`PARTIAL_ALIVE` is the only lawful current standing. The protocol cannot become `ALIVE` until property/fuzz, stdio+HTTP integration, stress, and benchmark suites execute and publish independently verifiable receipts.

## Operational law

Every suite must preserve:

```text
O → O* → action → observed consequence → receipt → replay
```

Primary evidence must cross the real boundary named by the suite. Mocks, stubs, fake telemetry, synthetic receipts, hard-coded success, and internal-state assertions cannot substitute for boundary consequence.

## Evidence contract

Every suite report binds:

- protocol identity and version;
- suite identity and kind;
- exact source tree and toolchain;
- admitted inputs and policy digest;
- commands and boundary addresses;
- positive witness;
- material negative falsifier;
- observed consequence;
- receipt digest;
- replay result;
- resource and time measurements where applicable;
- typed standing and exclusions.

## Integration with TAI certification

`TAI-REBUILD-001` is the black-box enterprise case for the protocol.

The certification workflow executes the currently admitted suites and publishes the Testing Building Block standing inside:

```text
target/certification/seven-day-standards.json
```

Expected bounded report:

```text
testing_protocol_id = GBB-TESTING-PROTOCOL-001
testing_suites = 10
testing_suites_alive = 6
testing_suites_pending = 4
testing_bblock_standing = PARTIAL_ALIVE
```

The report must not collapse the four pending suites into overall success.

## Gall extension plan

### G1 — Property and fuzz

Generate bounded corpora for identifiers, dependency graphs, profiles, receipts, evidence ledgers, ceilings, lifecycle transitions, and standards digests. Preserve minimized counterexamples and replay seeds.

### G2 — stdio and HTTP integration

Run the same admitted protocol vectors through real stdio and HTTP servers. Compare semantic result, authority use, resource envelope, receipt payload, refusal code, and replay digest.

### G3 — Stress

Execute maximum declared graph, closure, evidence, output, and concurrency envelopes. Require typed degradation, circuit breaking, and resource receipts rather than timeouts without diagnosis.

### G4 — Benchmark

Publish exact-environment baselines for resolve, enrich, extract, render, write, receipt verification, scenario execution, and replay. A benchmark regression is evidence, not an automatic truth claim; promotion requires admitted thresholds.

## Exclusions

The protocol does not claim:

- that the four pending suites already execute;
- that unit tests substitute for real protocol or process boundaries;
- that bounded TAI evidence proves arbitrary production safety;
- that benchmark speed proves semantic correctness;
- that fuzz volume proves closure;
- that one suite may self-promote another suite's standing.
