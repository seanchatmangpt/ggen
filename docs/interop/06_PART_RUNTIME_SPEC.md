# Genesis-Bearing Interchangeable Part Spec

## 1. Definition of Genesis-Bearing Interchangeable Part
[IMPLEMENTED: DOC_ONLY]

A Genesis-bearing interchangeable part is the fundamental unit of architecture in the distributed system. It is not merely a downstream executable artifact or passive data structure. It is a self-contained, physically disciplined unit of logic consisting of a runtime body, a ggen-manufactured membrane, the Genesis kernel, and a strict surface for receipt, replay, and refusal.

## 2. Why Genesis is Inside the Part, Not Downstream
[IMPLEMENTED: DOC_ONLY]

Genesis is the pure Chatman Equation foundation. It must be inside the part to guarantee lawful construction locally. It is not an ingestion layer, a parser, an adapter layer, or a downstream SQL/graph database. By embedding Genesis, the part is granted the authority to evaluate consequence immutably before any state is shared. If Genesis were downstream, the part could emit unlawful state, violating core architecture doctrine.

## 3. AtomVM Custody Role
[IMPLEMENTED: DOC_ONLY]

AtomVM provides the custody role. It offers an Erlang-style actor shell that isolates faults, manages the state lifecycle, and ensures that the Genesis-bearing part operates within a protected, preemptively scheduled envelope.

## 4. WASM Portability Role
[IMPLEMENTED: DOC_ONLY]

WASM gives portability. It allows the interchangeable part to run consistently across any compute substrate—from browser edge workers to local CLI hosts, factory cells, and device gateways—without altering the physical discipline enforced by Rust.

## 5. Rust Physical Discipline Role
[IMPLEMENTED: DOC_ONLY]

Rust provides physical discipline. It guarantees memory safety, strict boundary definition, and predictable performance, ensuring that the part cannot violate its own constraints or the physical laws of Genesis.

## 6. ggen Membrane Around the Part
[IMPLEMENTED: DOC_ONLY]

ggen is the foundry and membrane that manufactures, packages, adapts, validates, and projects Genesis-bearing parts. It encapsulates the part, acting as the boundary that protects the pure Chatman Equation foundation inside from outside-world dependencies. ggen does not decide consequence; it simply provides the membrane.

## 7. Genesis Kernel Inside the Part
[IMPLEMENTED: DOC_ONLY]

Genesis gives the part lawful construction and consequence. The kernel maintains the true state using predicate-fixed RelationPages and Pair2 tuples. A Pair2 is a tuple, not a compressed RDF triple, and the predicate is not stored per tuple. JSON is strictly forbidden as an internal representation for Genesis and may only appear as an external boundary projection.

## 8. Local Receipt Production
[IMPLEMENTED: DOC_ONLY]

Every Genesis-bearing part must produce unforgeable receipts locally for every state transition. Receipts make parts composable. A receipt must bind actual observed boundary evidence, not placeholder strings like `"hash_placeholder"` or `"TODO"`.

## 9. Local Replay Cursor
[IMPLEMENTED: DOC_ONLY]

Each part maintains a local replay cursor. This allows the part to reconstruct its state deterministically from its receipt chain and provides the necessary mechanism for Truex execution trust.

## 10. Local Refusal Evidence
[IMPLEMENTED: DOC_ONLY]

If a state transition is rejected, the part must generate robust refusal evidence. Even failure has a path. Missing values must emit `null` or `Option::None` and explicitly classify the resulting refusal state (e.g., `BoundaryEvidenceMissing`). A refusal receipt must contain the real observed boundary evidence up to the point of failure.

## 11. Rollup into Segment/Shard/Corpus
[IMPLEMENTED: DOC_ONLY]

Parts roll up their local receipts and validated state into segments, shards, and ultimately a corpus. Indexes are receipted derived views/access paths, not the authority. Rollups do not count mock or random matter toward real corpus claims.

## 12. Part Manifest
[IMPLEMENTED: DOC_ONLY]

The part manifest must be formatted in TOML or as a plain text manifest. It explicitly declares the part's capabilities, boundaries, membrane configuration, and cryptographic identity.

## 13. Part Lifecycle
[IMPLEMENTED: DOC_ONLY]

The Truex framework governs the lifecycle for receipted execution trust. It spans from ggen manufacturing and packaging to deployment via AtomVM/WASM custody, execution, local receipt generation, and eventual decommissioning or interchange.

## 14. Replacement/Interchangeability Rules
[IMPLEMENTED: DOC_ONLY]

Parts are strictly interchangeable. A part can be replaced by any other Genesis-bearing part that satisfies the same membrane input, output, and proof boundaries.

## 15. Compatibility Rules
[IMPLEMENTED: DOC_ONLY]

Compatibility is guaranteed by the receipt and proof boundaries. A part is compatible if its receipts can be cryptographically verified and its event log satisfies the temporal and spatial laws defined by external process models (validated via wasm4pm/pictl).

## 16. Definition of Done
[IMPLEMENTED: DOC_ONLY]

A Genesis-bearing interchangeable part is considered "done" when:
- The membrane is successfully generated by ggen.
- The Rust core compiles to WASM under physical discipline.
- The AtomVM host successfully provides custody.
- It produces cryptographically sound receipts for all state transitions.
- It produces explicit refusal evidence for invalid states, proving why an operation was refused.
- It uses strictly no JSON for internal representation.
- Implementation evidence exists in files and is not a mock claim.

## Interface Table

| Interface | Owner | Input | Output | Proof | Replay | Refusal | Validator |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **Membrane Ingress** | ggen | Raw external signal | Formatted internal command | Cryptographic signature of input | Replay log cursor | `BoundaryEvidenceMissing` | ggen sync gate |
| **Genesis Evaluation** | Genesis Kernel | Internal command | State transition & Pair2 tuple | BLAKE3 state hash | Event sourcing append | `LawfulRefusal` | Genesis internal checks |
| **Receipt Egress** | Truex Lifecycle | State transition | BLAKE3 Receipt | Signed receipt chain link | Truex cursor sync | `ReceiptGenerationFailed` | wasm4pm / pictl |
| **Custody Envelope** | AtomVM | Receipted Part | Managed Actor State | Process isolation trace | Actor restart log | `CustodyViolation` | Erlang supervisor / host |

**AtomVM gives custody. WASM gives portability. Genesis gives consequence. ggen gives the part a membrane.**
