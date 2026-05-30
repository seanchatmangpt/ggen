<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Interop Contracts](#interop-contracts)
  - [1. Scope](#1-scope)
  - [2. Contract Matrix](#2-contract-matrix)
    - [2.1. The Construction Contract (ggen → Genesis)](#21-the-construction-contract-ggen-%E2%86%92-genesis)
    - [2.2. The Custody Contract (AtomVM/WASM → ggen)](#22-the-custody-contract-atomvmwasm-%E2%86%92-ggen)
    - [2.3. The Projection Contract (ggen → External Validators)](#23-the-projection-contract-ggen-%E2%86%92-external-validators)
    - [2.4. The Promotion Contract (Genesis → Truex)](#24-the-promotion-contract-genesis-%E2%86%92-truex)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Interop Contracts

## 1. Scope
Defines the strict contracts between the layers of the Genesis-bearing interchangeable part architecture.

## 2. Contract Matrix

### 2.1. The Construction Contract (ggen → Genesis)
- **Owner:** Genesis
- **Input:** `RelationPage` (Symbol dictionaries + Predicate + raw `Pair2` streams).
- **Output:** Admitted `Construct8` packets.
- **Proof:** Packet Receipt (BLAKE3 hash).
- **Replay:** Deterministic replay cursor included in receipt.
- **Refusal:** Emits `Need9`, `Need257`, or `UnauthorizedContext` blocks.
- **Validator:** Truex (validates the signature and hash).

### 2.2. The Custody Contract (AtomVM/WASM → ggen)
- **Owner:** ggen
- **Input:** Raw host byte streams, socket connections, Erlang mailbox messages.
- **Output:** Membrane-adapted `RelationPage` contexts.
- **Proof:** Host PID / Actor ID binding to the source.
- **Replay:** N/A (Custody is a side-effect, only the output into Genesis is replayed).
- **Refusal:** Adapter parsing errors (e.g., malformed JSON).
- **Validator:** Truex (verifies the source address matches the manifest).

### 2.3. The Projection Contract (ggen → External Validators)
- **Owner:** ggen
- **Input:** Cryptographically signed `Construct8` packets and local symbol tables.
- **Output:** OCEL 2.0, N-Quads, DCAT, PROV.
- **Proof:** Projected artifact mathematically traces back to the Packet Receipt.
- **Replay:** The external validator can re-run queries, but true replay requires the Genesis cursor.
- **Refusal:** Shape violation (e.g., fails SHACL validation).
- **Validator:** `wasm4pm`, QLever, DuckDB.

### 2.4. The Promotion Contract (Genesis → Truex)
- **Owner:** Truex
- **Input:** Chain of Segment Receipts from a Genesis part.
- **Output:** Shard Receipt (Promotion into enterprise corpus).
- **Proof:** Truex Authorization Signature.
- **Replay:** Truex forces a random sample replay of the Segment before promotion.
- **Refusal:** Policy Block (e.g., unauthorized part manifest).
- **Validator:** Enterprise Audit Regulators.
