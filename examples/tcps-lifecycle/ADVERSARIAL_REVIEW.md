# Adversarial review of the unified TCPS lifecycle

## Review standard

The implementation was reviewed as an untrusted proof artifact. Agreement between
Rust, Lean, and templates was not treated as correctness because all three could
share the same defect. The review attempted to fabricate capabilities, bypass
receipts, restart without changed standard work, and obtain crown standing from
partial verification.

## Findings and repairs

| Severity | Finding | Repair |
|---|---|---|
| P0 | `Authorization` had public fields and `Deserialize`, allowing arbitrary evidence fabrication. | Made the capability opaque, non-`Clone`, non-`Copy`, non-deserializable, and constructible only through the Praxis authority. |
| P0 | `CargoCicdEvidence::green` let any caller assert green verification. | Evidence is now opaque and manufactured by the cargo-cicd workcell from source digest and normalized affected scope. |
| P0 | The executor manufactured the entire success receipt and the framework checked only two fields. | Executors now return artifact evidence only. The typestate transition atomically constructs the complete receipt and binds observation, plan, authorization, artifact, and executor. |
| P0 | `recover` accepted the existing standard hash despite the no-restart-without-updated-standard claim. | Recovery now refuses empty or unchanged standard work in Rust, the Aeneas kernel, tests, and Lean. |
| P0 | Aeneas extraction returned success while explicitly leaving Rust-to-Lean symbol binding open. | Extraction-only evidence now exits nonzero and records refused standing until an axiom-free binding file links the generated model to `rustStep_sound`. |
| P1 | The nested lifecycle example was not necessarily exercised by the root workspace CI. | Added a dedicated Rust, Lean, and projection-equivalence workflow. |
| P1 | Canonical projection ownership and mandatory invariants were documentary. | Added fail-closed SPARQL gates for required invariants and duplicate projection paths. |
| P1 | Example and pack templates could drift independently. | CI compares every pack template body byte-for-byte with its canonical reference artifact. |

## Preserved claims

- Typestate markers add no state-dependent runtime storage.
- Successful execution has no public intermediate unreceipted state.
- Authorization and verification evidence are affine capabilities.
- Abnormal execution stops the line and raises an andon.
- Restart requires changed, non-empty standard work and fresh admission.
- Selection, authorization, and execution remain separate roles.

## Fences

The following are not closed merely by source review:

- Charon/Aeneas symbol binding to the handwritten Lean refinement theorem.
- Praxis and cargo-cicd integration against their real repositories and CLIs.
- Deleted-consumer regeneration and second-sync byte identity through ggen.
- Signed release, SBOM, provenance, enterprise deployment, rollback, and replay receipts.
- Level 5 promotion standing.

## Falsifiers

The implementation is invalid if any test can construct authorization or green
evidence directly, execute from a pre-authorized state, return success without a
framework-created receipt, restart with the same standard, or report Aeneas pass
without a kernel-checked generated-symbol binding.

## Current standing

The source-level refactor is hardened and mechanically testable. Crown standing
remains refused until all external rails produce real receipts.
