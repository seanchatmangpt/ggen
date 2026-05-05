# Explanation: Vision 2030 Strategic Best Practices

This document outlines the long-term strategic best practices for the `ggen` ecosystem, aligned with the **Vision 2030** roadmap. These practices ensure that the system remains scalable, secure, and verifiable as it transitions toward a planetary-scale autonomic engineering substrate.

## 1. Truth via Real Execution (Chicago TDD)

A core pillar of Vision 2030 is the rejection of "faked" success. The **Chicago TDD** doctrine must be applied to all new components:

- **No Mocks:** Avoid `mockall` or in-memory stubs that simulate boundary behavior. If a component interacts with a database, the test must use a real database instance (e.g., `oxigraph`).
- **Externalizable Evidence:** A test is only valid if it produces evidence outside its own memory space—such as OTel spans, BLAKE3-hashed receipts, or file-system artifacts.
- **Verification over Assertion:** Don't just assert `result == success`. Verify that the side-effects occurred (e.g., the SPARQL store was mutated, the receipt was signed).

## 2. Sub-Microsecond Semantic Operations

To support high-frequency autonomous agents, the core semantic substrate must adhere to strict Service Level Objectives (SLOs) established in the Vision 2030 benchmarks:

- **JSON-RPC Parsing:** Must remain under **10μs**.
- **Task State Transitions:** Must execute in under **1μs**.
- **Verb Dispatch:** The mapping from intent to execution must be branchless and execute in under **1μs**.
- **FMEA Mitigation Overheads:** Security guards (like those in the `RdfControlPlane`) must not introduce more than **100μs** of latency to any single query.

## 3. Modular Specification with Ontology Packs

Vision 2030 moves away from monolithic configuration. All domain knowledge should be encapsulated in **Ontology Packs**:

- **Portable Metadata:** Every pack must include an `OntologyPackMetadata` definition in `ggen.toml`, specifying its namespaces, versions, and supported languages.
- **Independent Evolution:** Changes to one domain (e.g., `FinancialCloseProcess`) should not require re-validation of unrelated domains (e.g., `MaintenanceScheduling`).
- **Template v2 Integration:** Packs should bundle **v2 templates** that use inline SPARQL to extract only the necessary semantic subgraphs, minimizing memory pressure during large-scale generation.

## 4. Immutable Provenance (The Receipt Chain)

In a Vision 2030 world, "who did what" is not a log entry; it is a cryptographic proof:

- **Ed25519-Signed Receipts:** Every successful code generation or state transition must produce a receipt signed by the responsible agent or system component.
- **BLAKE3 Content Addressing:** Every artifact (code file, ontology, snapshot) is identified by its BLAKE3 hash. If the hash doesn't match the receipt, the artifact is considered unverified.
- **Causality via Vector Clocks:** In distributed scenarios, use vector clocks to track the causal order of events, ensuring that time-travel reconstruction remains deterministic.

## 5. Security as a Formal Guard (FMEA & Poka-Yoke)

Security is not a layer on top; it is a formal guard within the reconciler pipeline:

- **Poka-Yoke Builders:** Use typed builders for RDF and SPARQL construction to prevent malformed URIs or injection attacks at compile-time.
- **Active FMEA Mitigations:** The `RdfControlPlane` must actively monitor for failure modes (e.g., recursive deletions, unauthorized state jumps) and abort the operation before any mutation occurs.
- **SHACL-Validated Transitions:** All state changes in the marketplace must pass dynamic SHACL validation to ensure the graph never enters a semantically invalid state.

## Summary

By adhering to these Vision 2030 best practices, the `ggen` ecosystem guarantees that its high-performance semantic operations remain secure, modular, and—above all—empirically verifiable through real-world execution.
