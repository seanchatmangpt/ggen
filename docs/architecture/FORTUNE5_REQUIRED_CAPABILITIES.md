# Fortune-5 Required Capabilities

## Boundary

This is the **repository-defined Fortune-5 contract** carried by ggen. It does not claim that one generic checklist substitutes for the internal risk, regulatory, procurement, or operational requirements of every large enterprise.

The contract combines the capability families already named in ggen's Fortune-5 assessments and benchmark corpus:

| Family | Required capabilities |
|---|---:|
| Release truths | 6 |
| Supporting pack systems | 2 |
| Performance governors | 5 |
| Operational controls | 6 |
| **Total** | **19** |

Every capability requires three independent evidence surfaces:

1. Positive execution over a valid input.
2. Named negative refusal over an invalid input.
3. Cryptographic receipt verification or deterministic replay.

\[
19 \times 3 = 57
\]

No weighted average is used. One open capability prevents crown `ALIVE`.

## Release truths

The executable control plane proves:

- **Install Truth:** deterministic Ed25519 verification, SHA-256 identity, atomic filesystem commit, tamper refusal, and readback replay.
- **Compiler Truth:** ontology, SELECT query, and Tera template produce a deterministic projection; missing compiler inputs refuse.
- **Conflict Truth:** ontology namespace, protocol field, output path, runtime, validator, policy, version, capability identity, receipt schema, and migration claims are all exercised.
- **Rendering Truth:** Tera is deterministic and malformed templates refuse before output.
- **Trust Truth:** minimum tier, registry class, signature requirement, and runtime allowlist are conjunctive.
- **Proof Truth:** consequences form a predecessor-linked SHA-256 chain; tamper and duplicate replay refuse.

## Supporting systems

- The atomic taxonomy requires exactly nine unique categories: surface, contract, projection, runtime, policy, validator, receipt, consequence, and core.
- Bundle expansion is recursive, deterministic, unique, and cycle-refusing.

## Performance governors

The control plane executes deterministic SLO-evaluation machinery for the repository's five Fortune-5 benchmark patterns:

| Governor | Bounded contract |
|---|---|
| CLI startup | p90 ≤ 100 ms |
| Template rendering | typical ≤ 1,000 ms; large ≤ 5,000 ms |
| RDF query | p90 ≤ 100 ms |
| Memory | baseline ≤ 50 MB; peak ≤ 500 MB |
| Concurrency | eight-worker throughput ≥ 90% of linear scaling |

These proofs establish that the **governors, thresholds, refusals, and receipts execute correctly**. They do not represent measurements from an undeclared production workload.

## Operational controls

- Andon signals escalate monotonically and `RED` stops execution.
- Poka-Yoke bounded values and lifecycle states refuse invalid construction or transition.
- W3C `traceparent` context parses, propagates, round-trips, and refuses zero identifiers.
- Retry schedules are bounded and deterministic; repeated failures open a circuit breaker.
- Golden signals yield an actionable health verdict from latency, traffic, errors, and saturation.
- Error-budget consumption gates risky change.

## Ontology-first manufacturing

`packs/fortune5-required-capabilities-pack/ontology.ttl` is the source of truth for:

- all 19 capability identities;
- stable ordering and Rust variants;
- category and required outcome;
- implementation artifact;
- verifier command;
- named falsifier;
- all 57 proof-surface obligations.

`ggen sync run` manufactures an independent Rust consumer, tests, documentation, release declaration, verification script, and CI workflow. The generated release declaration remains `UNKNOWN`; only executed evidence may produce `ALIVE`.

## Verifier ladder

```bash
cargo fmt --all -- --check
cargo test -p ggen-marketplace --test fortune5_required_capabilities
cargo test -p ggen-graph --test rwr_level5_e2e
cargo build -p ggen-cli-lib --bin ggen

cd packs/fortune5-required-capabilities-pack
../../target/debug/ggen sync run
cargo test --manifest-path consumer/fortune5-required-foundation/Cargo.toml
../../target/debug/ggen receipt verify
bash consumer/fortune5-required-foundation/scripts/verify.sh
```

## Promotion law

The bounded reference foundation is `ALIVE` only when:

- the direct implementation passes;
- all negative fixtures fail closed;
- 57 durable witnesses externalize;
- the evidence ledger and assessment receipt replay;
- two `ggen sync run` executions are byte-identical;
- the generated consumer independently reports 19 capabilities, 57 surfaces, zero open obligations, and `ALIVE`.

A compiler failure is `BUILD_BROKEN`. A passing falsifier is `BLOCKED`. Missing execution evidence is `UNKNOWN`.
