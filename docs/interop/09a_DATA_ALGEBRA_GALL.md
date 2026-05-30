<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Data Algebra GALL (Gate for Algebraic Logic Limits)](#data-algebra-gall-gate-for-algebraic-logic-limits)
  - [1. Purpose](#1-purpose)
  - [2. Relational and Binary Relation Theory](#2-relational-and-binary-relation-theory)
  - [3. Set vs Bag vs Stream](#3-set-vs-bag-vs-stream)
  - [4. Local vs Global Identity](#4-local-vs-global-identity)
  - [5. Indexes Are Not Authority](#5-indexes-are-not-authority)
  - [6. Algebra Safety Checklist](#6-algebra-safety-checklist)
  - [7. Final Judgment](#7-final-judgment)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Data Algebra GALL (Gate for Algebraic Logic Limits)

## 1. Purpose
Protect the Genesis core from violating known formal data and database algebra. Genesis hot matter is NOT compressed triples. It is receipted binary-relation construction.

## 2. Relational and Binary Relation Theory
Genesis implements `A = μ(O)`. The payload `Pair2` is strictly a tuple `(left, right)` evaluated within a predicate-fixed `RelationPage`. This enforces strict binary relation theory.

## 3. Set vs Bag vs Stream
Genesis must support multiple algebraic multiplicities:
- **Stream:** The raw `Construct8` packet flow (append-only, duplicates possible).
- **Bag (Multiset):** Aggregated packets before deduplication.
- **Set:** The final deduplicated canonical corpus.

## 4. Local vs Global Identity
`Pair2` bytes are strictly LOCAL to the `RelationPage` symbol table. Global URIs/IRIs are resolved by ggen. Genesis operates purely on local algebraic identities.

## 5. Indexes Are Not Authority
Indexes (B-Trees, Hash Maps) are receipted derived views/access paths. They CANNOT be treated as the source of truth. The `Construct8` packet stream is the absolute authority.

## 6. Algebra Safety Checklist

| Check | Question | Pass criteria | Failure mode | Required doc/test |
| :--- | :--- | :--- | :--- | :--- |
| **RelationPage Validity** | Is the predicate fixed outside the tuple? | Yes | Tuples store predicate (bloat/RDF confusion) | `test_page_layout` |
| **Pair2 Domain Bound** | Are left/right strictly 8-bit or 16-bit scoped? | Yes | Global URIs inside hot path | `test_pair2_bounds` |
| **Need257 Split** | Does a page split when symbols exceed limits? | Yes | Overflow exception or widening | `test_need257_split` |
| **Need9 Split** | Does a packet split at 8 tuples? | Yes | Unbounded packet allocation | `test_need9_split` |
| **Context Authority** | Is the assumed middle bound to O*? | Yes | Blind insertion | `test_context_auth` |
| **Multiplicity Law** | Does the system distinguish set/bag/stream? | Yes | Duplicate inflation | `test_multiplicity` |
| **Join Law** | Can two pages be joined algebraically? | Yes | Missing cross-page identity map | `test_page_join` |
| **Index Non-Authority**| Can the index be rebuilt from receipts? | Yes | Data loss on index wipe | `test_index_rebuild` |
| **Refusal Distinction**| Is an invalid act explicitly refused? | Yes | Silent drop | `test_refusal_gen` |

## 7. Final Judgment
**Judgment: PARTIALLY SAFE.**
The theoretical foundation (Pair2, Construct8, RelationPage) is deeply algebraically sound. However, the formal implementation of the `Need257` symbol split law and cross-page Join laws require stricter formalization in the Rust kernel to ensure mathematical completeness.

| Status | Component | File/Artifact Evidence |
| :--- | :--- | :--- |
| **IMPLEMENTED** | Pair2/Construct8 Base | `crates/genesis-wasm-shell/src/lib.rs` |
| **MISSING** | Need257 Split Logic | Needs implementation |
| **MISSING** | Formal Join Logic | Needs implementation |
