<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Genesis Kernel Specification](#genesis-kernel-specification)
  - [1. Purpose](#1-purpose)
  - [2. Non-goals](#2-non-goals)
  - [3. Core Equation](#3-core-equation)
  - [4. Pair2 Model](#4-pair2-model)
  - [5. RelationPage Model](#5-relationpage-model)
  - [6. Construct8 Packet Model](#6-construct8-packet-model)
  - [7. Need9 Split Law](#7-need9-split-law)
  - [8. Need257 Split Law](#8-need257-split-law)
  - [9. Set/Bag/Stream/Event-Addressed Multiplicity Law](#9-setbagstreamevent-addressed-multiplicity-law)
  - [10. Source-Addressing Law](#10-source-addressing-law)
  - [11. Receipt Law](#11-receipt-law)
  - [12. Replay Law](#12-replay-law)
  - [13. Refusal Law](#13-refusal-law)
  - [14. Shard/Corpus Composition Law](#14-shardcorpus-composition-law)
  - [15. Interop Contracts Exposed to ggen](#15-interop-contracts-exposed-to-ggen)
  - [16. Rust Implementation Expectations](#16-rust-implementation-expectations)
  - [17. Tests Required](#17-tests-required)
  - [18. Definition of Done](#18-definition-of-done)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Genesis Kernel Specification

**Status:** `DOC_ONLY`
**Component:** Genesis Core

## 1. Purpose
Genesis is the pure Chatman Equation foundation ($A = \mu(O)$). It provides the lawful construction for interchangeable, Genesis-bearing parts. It enforces physical discipline, custody, and composability via rigorous cryptographic receipts and state transitions, independent of any external context, formatting, or environment.

## 2. Non-goals
Genesis **DOES NOT** own:
- Adapters, parsers, filesystem IO, or network IO.
- External APIs or SaaS ingestion layers.
- Graph database behavior, SPARQL engine behavior, or SQL engine behavior.
- Process mining or event bus behavior.
- CLI UX or external format projection (including JSON, RDF, or SHACL).

## 3. Core Equation
**Status:** `DOC_ONLY` (Implementation verified via `A = \mu(O)`)
- **$A = \mu(O)$**: The Chatman Equation.
- $O$: The starting ontology/state.
- $\mu$: The transition function (membrane).
- $A$: The resulting artifact/capability.
- Genesis enforces that any state transition is mathematically and cryptographically accountable to this equation.

## 4. Pair2 Model
**Status:** `DOC_ONLY`
- **Definition:** `Pair2` is a two-byte tuple representing a subject-object pair.
- **Key Correction:** `Pair2` is **NOT** a compressed RDF triple. It consists of a left byte and a right byte under a predicate-fixed `RelationPage` context.
- **Predicate as Context:** The middle (predicate) is supplied by the relation context. The predicate is law/context, not payload.

## 5. RelationPage Model
**Status:** `DOC_ONLY`
- A `RelationPage` groups `Pair2` entries that share the exact same predicate.
- The predicate is fixed at the page level, meaning tuples within the page do not redundantly store the predicate.
- Enforces strict memory and semantic boundaries for a single type of relation.

## 6. Construct8 Packet Model
**Status:** `DOC_ONLY`
- An 8-byte packet structure providing custody and routing for fundamental Genesis units.
- Facilitates safe transport and cryptographic hashing within the kernel.

## 7. Need9 Split Law
**Status:** `DOC_ONLY`
- Governs the expansion and splitting of structures when a capacity threshold (9 elements) is reached.
- Ensures predictable memory usage and limits page/fragment fragmentation.

## 8. Need257 Split Law
**Status:** `DOC_ONLY`
- Governs the splitting of `RelationPage` or other block structures when the 257th element is introduced.
- Ensures no 8-bit index overflow occurs, maintaining the strict byte-addressable boundaries.

## 9. Set/Bag/Stream/Event-Addressed Multiplicity Law
**Status:** `DOC_ONLY`
- Dictates how multiple entries of the same type are addressed and resolved.
- Distinguishes between unique sets, counted bags, ordered streams, and temporally bound events.
- Prevents index collision and enforces deterministic replayability.

## 10. Source-Addressing Law
**Status:** `DOC_ONLY`
- Every Construct8 or Pair2 MUST trace back to a verifiable source address.
- Ensures all matter within Genesis is cryptographically bound to its origin, preventing hallucinated or "mock" data from corrupting the real corpus.

## 11. Receipt Law
**Status:** `DOC_ONLY`
- Every lawful state transition ($\mu$) MUST emit a BLAKE3 receipt.
- Receipts make parts composable by cryptographically proving that an output $A$ was derived from input $O$ using $\mu$.
- Placeholders, mock hashes, and null boundaries without explicit refusal states are structurally impossible.

## 12. Replay Law
**Status:** `DOC_ONLY`
- Given the initial state $O$ and the sequence of receipts, the exact transition $\mu$ MUST be deterministically replayable to achieve $A$.
- Replay requires strict source addressing and invariant evaluation.

## 13. Refusal Law
**Status:** `DOC_ONLY`
- Any operation that violates a semantic law, split law, or memory boundary MUST emit a definitive refusal state instead of panicking or ignoring the input.
- Missing values emit `Option::None` / `null` mapped to a categorized refusal receipt (e.g., `BoundaryEvidenceMissing`), explicitly capturing the refusal in the causal chain.

## 14. Shard/Corpus Composition Law
**Status:** `DOC_ONLY`
- A `Shard` is a bounded collection of `RelationPage` structures.
- A `Corpus` is a composed collection of `Shard` instances.
- Composition relies entirely on receipts; a `Corpus` cannot merge `Shard`s that lack an unbroken causal receipt chain.
- Indexes are receipted derived views, not the authority.

## 15. Interop Contracts Exposed to ggen
**Status:** `DOC_ONLY`
- **Owner:** Genesis Kernel
- **Input:** $O$ (Strictly parsed binary pages or structured bytes).
- **Output:** $A$ (Genesis-bearing parts, Construct8 packets, generated Receipts).
- **Proof:** BLAKE3 cryptographic receipts binding inputs to outputs.
- **Replay:** Deterministic state recreation via logged `Construct8` sequences.
- **Refusal:** Emitted refusal receipts (not exceptions or silent ignores).
- **Validation Path:** `ggen` consumes the receipts to validate that the kernel adhered strictly to the Chatman Equation and split laws.

## 16. Rust Implementation Expectations
**Status:** `MISSING` (Code to be implemented in e.g., `crates/genesis/` or similar)
- **Physical Discipline:** Where possible, structures must be zero-copy and `#[repr(C)]`.
- **Typestate Pattern:** Prevent illegal states at compile time (e.g., a `RelationPage` cannot be built without a fixed predicate).
- **Error Handling:** Return explicit `Result<T, RefusalState>`, avoiding `panic!`.
- **Serialization limits:** JSON must not be used as an internal representation.

## 17. Tests Required
**Status:** `MISSING`
- **Boundary Crossing:** Real WASM instantiation and memory sharing with adapters.
- **Causality Chain:** Verify that adding a `Pair2` yields a receipt linking to the previous shard state without placeholder hashes.
- **Anti-Cheating:** Tests must assert physical memory layout (e.g., verifying `Pair2` strictly does not contain the predicate and occupies exactly 2 bytes).
- **Refusal Triggering:** Feed invalid constructs or force Need257 overflows and verify precise refusal receipt generation.
- **No Mocking:** Real block allocation, real cryptographic hashing, real boundary evidence.

## 18. Definition of Done
- [ ] `Pair2`, `RelationPage`, `Construct8`, `Shard`, and `Corpus` models are physically implemented in Rust (`#[repr(C)]` where applicable).
- [ ] Need9 and Need257 split laws are fully implemented and proven with failing boundary tests that emit refusal receipts.
- [ ] Multiplicity laws distinguish between set/bag/stream/event forms reliably.
- [ ] Source addressing and receipt laws enforce unbroken causality for all structural changes.
- [ ] Indexes exist strictly as receipted derived views, never as authoritative stores.
- [ ] Zero mock data or placeholder values are present in testing.
- [ ] Interop boundaries (Genesis <-> ggen) produce externalizable evidence (BLAKE3 receipts).