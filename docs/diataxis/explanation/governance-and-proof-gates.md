# Explanation: The 8 Canonical Proof Gates

Proof Gates are the hard-coded validation invariants that govern the `ggen` manufacturing pipeline. They ensure that every artifact released by the factory is lawful, conformant, and verifiable.

## Why Gates?
In traditional CI/CD, "passing tests" is often a shallow assertion. In **Semantic Manufacturing**, we require **Evidence**. Proof Gates interrogate the `BuildReceipt` to confirm that the transformation `A = μ(O)` (Artifact equals the projection of an Ontology) happened exactly as intended.

## The 8 Canonical Gates

| Gate ID | Name | Constraint |
| :--- | :--- | :--- |
| **O-01** | **Schema Valid** | Validates that the input ontology is well-formed RDF and complies with SHACL shapes. |
| **O-02** | **Ontology Lawful** | Ensures the ontology substrate contains no soundness violations (e.g., deadlocks in the state machine). |
| **M-01** | **Projection Complete** | Verifies that all bindings extracted in μ₂ were successfully emitted in μ₃. |
| **M-02** | **Compilation Passes** | Confirms that the μ₄ canonicalization (formatting/syntax check) was successful. |
| **P-01** | **Receipt Valid** | Validates the cryptographic integrity of the BLAKE3 hash chain. |
| **O-03** | **Ethos Conformant** | Confirms that the run aligns with the defined `ManufacturingIntent` objective. |
| **T-01** | **Observability Present** | Verifies that full OTel telemetry spans were emitted for every stage of the pipeline. |
| **C-01** | **Causal Consistent** | Proves the deterministic link between the input ontology hash and the output artifact hash. |

## Implementation in Rust
Unlike Python-based gatekeepers, these gates are implemented as a **Stage 6** interceptor in the `StagedPipeline` struct. Because they are compiled directly into the binary, they cannot be bypassed by external scripts, anchoring the manufacturing constitution in the runtime itself.
