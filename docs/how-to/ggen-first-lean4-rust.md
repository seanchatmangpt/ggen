# ggen-first Lean 4 to Rust

`ggen-lean4-rust-pipeline-pack` manufactures a Lean 4 proof library and emitter executable from an admitted RDF graph. Lean checks the proof bundle before that executable may emit a Rust crate. The Rust product contains a Fortune 5 deployment-policy evaluator, cryptographic receipt verifier, and executable SLO probe.

## Source boundary

The consumer authors exactly:

```text
ggen.toml
ontology.ttl
```

There is no handwritten Lean file, Rust file, Cargo manifest, test, receipt schema, policy evaluator, benchmark probe, or pipeline report in the consumer surface. `ggen.lock` is ggen-owned root state.

## Law-state route

```text
parse RDF
→ run fail-closed graph gates
→ ggen emits a complete Lean library-plus-executable project and signed sync receipt
→ Lean kernel checks the bounded-successor and Fortune 5 theorem bundles
→ leanchecker independently checks the proof-library environment
→ the admitted Lean executable emits Cargo.toml and Rust source
→ rustfmt canonicalizes the emitted Rust
→ Clippy and tests admit the crate
→ promotion scenarios execute
→ BLAKE3 binds execution and regional receipts to proof-receipt.json
→ SLO probe emits measured P50/P95/P99 and the resulting promotion decision
→ sabotage, receipt tamper, replay, and second-emission identity are verified
```

## Why this is Lean 4 to Rust rather than parallel projection

The pack contains no Rust template. ggen owns these outputs:

```text
generated/lean/lean-toolchain
generated/lean/lake-manifest.json
generated/lean/lakefile.lean
generated/lean/Fortune5Policy.lean
generated/lean/RustLib.lean
generated/lean/RustMain.lean
generated/lean/RustEvidence.lean
generated/lean/Lean4RustPipeline.lean
generated/lean/Main.lean
generated/PIPELINE.md
```

`Fortune5Policy.lean` owns definitions, theorems, and proof receipts. `RustLib.lean`, `RustMain.lean`, and `RustEvidence.lean` own the Lean-held Rust projections. `Lean4RustPipeline.lean` owns emission only. `Main.lean` is the minimal executable route. The root module produces a package-level `.olean` for independent `leanchecker` replay.

`generated/rust/` does not exist after `ggen sync run`. It appears only after:

```bash
cd generated/lean
lake build
lake exe emitRust
```

The Lean executable emits:

```text
generated/rust/Cargo.toml
generated/rust/src/lib.rs
generated/rust/src/main.rs
generated/rust/src/bin/verify_receipt.rs
generated/rust/src/bin/slo_probe.rs
```

## Bounded-successor cell

```text
step(x) = x + 1  when x < bound
step(x) = bound  otherwise
```

The specimen uses bound `10` and witness `9 → 10`. Lean proves `step_le`, `step_witness`, and `step_fixed_point`.

## Fortune 5 capability cell

The ontology admits the enterprise requirements as executable policy rather than prose:

| Capability | Admitted law |
|---|---|
| SLO tracking | R1 ≤ 2 ns P99 / 8 RDTSC ticks, W1 ≤ 1 ms P99, C1 ≤ 500 ms P99 |
| Promotion | Canary and staging validation plus a promotion receipt are mandatory; SLO violation rolls back; any failed control refuses promotion |
| Multi-region | At least three regions, majority quorum, observed cross-region replication, synchronized receipts, failover readiness, legal-hold readiness |
| Identity | Observed SPIFFE identity must exactly equal the admitted workload identity; authentication and certificate age ≤ 1 hour are required |
| Key management | Runtime readiness receipts for AWS KMS, Azure Key Vault, and HashiCorp Vault are required; key age ≤ 24 hours |
| Network security | mTLS, network policy, and firewall policy required |
| Observability | OTEL correlation plus SLO, guard, receipt-mismatch, and degradation alerts |
| Receipts | Proof digest and regional receipt digest independently recomputed before acceptance |

Lean proves threshold ordering, majority quorum, security time bounds, all required controls, healthy canary/staging/production outcomes, SLO rollback, and refusal for quorum, security, receipt synchronization, failover, identity, KMS, replication, promotion-receipt, or alert-evidence failures.

## Runtime scenarios

The emitted binary supports deterministic scenarios:

```bash
cargo run --bin lean-proof-cell -- canonical 9
cargo run --bin lean-proof-cell -- canary 9
cargo run --bin lean-proof-cell -- production 9
cargo run --bin lean-proof-cell -- slo-violation 9
cargo run --bin lean-proof-cell -- quorum-loss 9
cargo run --bin lean-proof-cell -- security-expired 9
cargo run --bin lean-proof-cell -- receipt-mismatch 9
cargo run --bin lean-proof-cell -- failover-unready 9
cargo run --bin lean-proof-cell -- identity-mismatch 9
cargo run --bin lean-proof-cell -- kms-unready 9
cargo run --bin lean-proof-cell -- replication-unready 9
cargo run --bin lean-proof-cell -- promotion-receipt-missing 9
cargo run --bin lean-proof-cell -- alerts-unready 9
```

The receipt verifier independently recomputes both BLAKE3 bindings:

```bash
cargo run --bin verify_receipt -- \
  ../evidence/execution-canonical.json \
  ../lean/proof-receipt.json
```

The SLO probe performs release-mode batched RDTSC and steady-clock measurements. It emits P50, P95, P99, the R1 nanosecond and tick targets, compliance, and the promotion decision:

```bash
cargo run --release --bin slo_probe
```

The only generated Rust `unsafe` surface is two audited LFENCE/RDTSC blocks in this probe; the policy library, execution binary, and receipt verifier remain safe Rust. A hosted-runner result is a measurement receipt, not a universal performance guarantee. A target violation lawfully produces `ROLLBACK` rather than being masked.

## Verification

Run from the repository root after Lean is installed:

```bash
bash scripts/verify-lean4-rust-pipeline.sh
```

The verifier covers graph admission, signed ggen receipts, Lean build, leanchecker, Rust formatting, Clippy, unit tests, promotion scenarios, actual SLO measurement, isolated-unsafe audit, receipt recomputation, tamper refusal, policy sabotage, and two levels of byte-identity replay.

## Exclusions

This checkpoint does not claim arbitrary Lean extraction, unrestricted recursion translation, FFI correctness, side-effect equivalence, or a deployed Fortune 5 control plane. It implements the complete policy/admission/evidence cell. Real SPIRE, KMS, OTEL collectors, regional replication, quorum transport, firewalls, and deployment systems remain downstream actuators and must return observations and receipts to this law.
