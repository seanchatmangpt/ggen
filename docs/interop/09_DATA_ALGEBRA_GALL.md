# 09_DATA_ALGEBRA_GALL.md
**Agent:** Data Algebra GALL
**Purpose:** Protect the project from violating known data/database algebra. Validate Genesis core against relational algebra, binary relation theory, set vs. bag vs. stream behavior, global vs. local identity, and process/RDF projections.

## Doctrine
- **Genesis** is the pure Chatman Equation foundation.
- **ggen** is the foundry and membrane that manufactures, packages, adapts, validates, and projects Genesis-bearing parts.
- **AtomVM** gives parts custody.
- **WASM** gives parts portability.
- **Rust** gives physical discipline.
- **Genesis** gives the part lawful construction.
- **Receipts** make parts composable.
- **Truex** is the lifecycle for receipted execution trust.
- **wasm4pm/pictl** consume and validate process-evidence projections.
- **open-ontologies** is a public-vocabulary survivability checkpoint.
- **External engines** (QLever, DuckDB, SHACL tools, OCEL tools, RDF stores) are validators/consumers, not Genesis replacements.

## Forbidden Moves
1. Do not treat Genesis as a parser, adapter layer, serializer, graph database, SQL engine, process-mining engine, event bus, or SaaS ingestion layer.
2. Do not put outside-world dependencies inside Genesis.
3. Do not treat ggen as the authority that decides consequence.
4. Do not treat AtomVM/WASM as merely downstream event sources. They are runtime bodies for Genesis-bearing interchangeable parts.
5. Do not treat two-byte Pair2 as a compressed RDF triple. Pair2 is a tuple in a predicate-fixed RelationPage.
6. Do not store the predicate per tuple.
7. Do not let an index become authority. Indexes are receipted derived views/access paths.
8. Do not count mock/random matter toward real corpus claims.
9. Do not claim implementation exists without file evidence.
10. Do not use JSON as an internal representation for Genesis or related core architecture. JSON may appear only as an external boundary projection.

## Required Conclusion
**Genesis hot matter is not compressed triples. Genesis hot matter is receipted binary-relation construction.**

---

## 1. Algebra Validation & Analysis

### Relational Algebra & Binary Relation Theory
- **Status:** [IMPLEMENTED]
- **Evidence:** `crates/genesis-core-v2/src/primitives.rs`, `crates/ggen-core/src/genesis.rs`
- **Analysis:** Genesis restricts active construction to pure binary relations (`Pair2`), bound by a strict context (`RelationPage` limits construction to a single predicate). It avoids n-ary sprawl at the hot edge. Projections (selections, joins) are not done internally within the Genesis hot path but deferred to projection membranes and views.

### Set vs. Bag vs. Stream Behavior
- **Status:** [IMPLEMENTED]
- **Evidence:** `crates/ggen-core/tests/genesis_primitives_test.rs` (shows `Multiplicity::Set`, `Multiplicity::Bag`, `Multiplicity::Stream`, `Multiplicity::EventAddressed`)
- **Analysis:** Genesis natively maps multiplicity to algebra. Sets reject duplicates, Bags allow duplicates, Streams differentiate by logical timestamps, and EventAddressed relies on OCEL-like identity.

### Local vs. Global Identity
- **Status:** [IMPLEMENTED]
- **Evidence:** `crates/ggen-core/src/genesis.rs` (`Pair2::new(subject, object)` using `Node8` localized within a `RelationPage` of max size 256)
- **Analysis:** `RelationPage` acts as a local dictionary mapping external `Node8` globals into local `u8` (byte) references for high-speed construction. Domain bounds enforce this map.

### Keys, Inclusion Constraints, Joins, Projections, Selections
- **Status:** [PARTIAL]
- **Evidence:** Constraints handled via `RelationPage` boundaries. Joins and deep projections deferred to membrane outputs (e.g., `crates/ggen-membrane/src/lib.rs`).
- **Analysis:** Genesis does not implement an internal SQL/SPARQL execution engine for joins or anti-joins. It provides the provable tuples to standard engines that compute transitive closures or complex joins.

### Provenance, Updates, Retractions, Refusals
- **Status:** [IMPLEMENTED]
- **Evidence:** `crates/genesis-core-v2/src/primitives.rs` (`Refusal`, `RefusalReason`), `crates/ggen-projection/src/lib.rs` (`project_prov`).
- **Analysis:** No updates in place. Everything is a receipted transition. Refusal is explicitly distinct from Null or Unknown. Refusal is captured in its own causal chain (`failed_pair` handling).

### RDF/SPARQL & OCEL Projections
- **Status:** [IMPLEMENTED]
- **Evidence:** `crates/ggen-projection/src/lib.rs` (`project_ocel2`, `project_nquads`, `project_prov`)
- **Analysis:** JSON and N-Quads only exist at the membrane. Internal hot matter uses zero-heap structures. OCEL and RDF are strict deterministic derivations from `RelationPage` state.

---

## 2. GALL Checklist

| Check | Question | Pass criteria | Failure mode | Required doc/test |
| :--- | :--- | :--- | :--- | :--- |
| **RelationPage validity** | Does the relation page restrict tuples to a singular assumed middle? | Tuples do not carry predicate bytes. Page dictates predicate. | Tuples carrying redundant predicate data (violates compression & algebra). | `crates/genesis-core-v2/src/primitives.rs` [IMPLEMENTED] |
| **Pair2 domain bound** | Is Pair2 restricted to 2 bytes (left+right)? | `size_of::<Pair2>() == 2` | Exceeding 2 bytes introduces global identity payload into the hot path. | `crates/ggen-core/tests/genesis_primitives_test.rs` (assertions on size) [IMPLEMENTED] |
| **Need257 split** | What happens when local domain exceeds 256 unique items? | Refusal `Need257` triggers deterministic `PageSplit`. | Page overflows silently or uses dynamic heap allocations. | `crates/ggen-core/tests/genesis_page_split_test.rs`, `crates/ggen-core/tests/genesis_domain_bounds_test.rs` [IMPLEMENTED] |
| **Need9 split** | What happens when > 8 pairs are constructed in one packet? | Refusal `Need9` limits `Construct8` packet. | Unbounded construction array lengths or heap vecs. | `crates/ggen-core/tests/genesis_primitives_test.rs` [IMPLEMENTED] |
| **Context authority** | Can a Pair2 be evaluated without a RelationPage? | No. Meaning is unbound without the page. | Treating Pair2 as globally resolvable independently. | `crates/genesis-core-v2/src/primitives.rs` [IMPLEMENTED] |
| **Multiplicity law** | Do Set/Bag/Stream obey standard data algebra? | Duplicates rejected for Sets, accepted for Bags/Streams. | Set allows duplicates; Stream drops them. | `crates/ggen-core/tests/genesis_primitives_test.rs` (Multiplicity specs) [IMPLEMENTED] |
| **Identity law** | Are local bytes deterministically mapped to global identities? | SymbolDomain exactly maps 1-byte id to 8-byte global id. | Dictionary corruption or non-deterministic mapping. | `crates/ggen-core/tests/genesis_determinism_test.rs` [IMPLEMENTED] |
| **Join law** | Are joins pushed to external engines/membranes rather than hot paths? | Genesis lacks a `JOIN` method; exposes pages to QLever/DuckDB. | Genesis implements internal join optimizers. | `crates/ggen-membrane/src/lib.rs` [IMPLEMENTED] |
| **Index non-authority** | Are derived access paths treated as receipts, not truth? | Indexes reconstructible entirely from replay cursors and receipts. | Indexes treated as single-source of truth. | `crates/ggen-core/tests/genesis_sabotage_test.rs` [IMPLEMENTED] |
| **Receipt composition** | Does one page's receipt perfectly transition the next? | `Hash(n) = Hash(Hash(n-1) + Pair2s)` | Receipt chain breaks or relies on external DB. | `crates/ggen-core/src/genesis.rs` (`ReplayCursor`) [IMPLEMENTED] |
| **Replay determinism** | Does replaying the same receipt trace produce bit-for-bit identical RelationPages? | `state1 == state2` for same replay stream. | Timestamps/UUIDs cause replay divergence. | `crates/ggen-core/tests/genesis_determinism_test.rs` [IMPLEMENTED] |
| **Refusal distinction** | Are structural failures cleanly distinguished from unknown data? | Refusals output specific codes (`Need257`, `Need9`, `Mask8` fail). | Panics or returns nulls for bounded domain splits. | `crates/genesis-core-v2/src/primitives.rs` [IMPLEMENTED] |
| **RDF expansion validity** | Is the external projection to RDF causally valid? | JSON/RDF never seen inside core. Exclusively in `ggen-projection`. | JSON/String manipulation inside Genesis core. | `crates/ggen-projection/src/lib.rs` (`project_nquads`) [IMPLEMENTED] |
| **OCEL projection validity**| Does the system map relation transitions to valid process events? | `project_ocel2` yields valid object-centric event logs. | Missing `ocel:activity` or temporal anomalies. | `crates/ggen-projection/src/lib.rs` (`project_ocel2`) [IMPLEMENTED] |
| **Cost model sanity** | Is the cost model (storage/compute) predictable and bounded? | Zero-heap, fixed size `RelationPage<256>` bounds exactly to 520 bytes. | Dynamic allocations O(N) break runtime budgets. | `crates/genesis-core-v2/src/primitives.rs` (size_of checks) [IMPLEMENTED] |

---

## 3. Finish Recommendations & Definition of Done

**Recommendation 1: Enforce Strict Join/Projection Separation in Membrane**
- **DoD:** All complex joins and closures must be demonstrably deferred to WASM-based or external consumers (e.g., DuckDB/QLever). No logic inside `genesis-core` or `genesis-core-v2` shall contain graph traversal loops. Add a static analysis check forbidding `Rc`, `Arc`, `Box`, or recursive trait bounds in the hot path.

**Recommendation 2: Formalize Need257 & Need9 Handlers**
- **DoD:** Replay traces must demonstrate the exact split behavior when a `Need257` (domain full) or `Need9` (packet full) refusal is encountered. The test `test_need257_split_recovery` must exist and pass without heap allocations, proving that a PageSplit cascades correctly to new deterministic RelationPages.

## 4. Interop Boundaries

| Boundary | Owner | Input | Output | Proof | Replay | Refusal | Validation Path |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **Genesis Core <-> WASM Custody** | AtomVM/WASM | Byte-streams representing Construct8 | `RelationPage` | Receipts (`ReceiptHint8`) | Sequential stream of Construct8 bytes | Returns specific Refusal bytes via FFI | Host verifies WASM linear memory bounds |
| **Membrane <-> External View (RDF)** | `ggen-projection` | `[RelationPage]` | N-Quads String | Provenance `Receipt` | Rerun projection on receipt chain | Invalid RelationPage (malformed) | QLever imports N-Quads successfully |
| **Membrane <-> External View (OCEL)** | `ggen-projection` | `[RelationPage]` | OCEL JSON-object | Event ID tracing | Rerun projection | Missing process context | `wasm4pm/pictl` compliance checks |
| **Genesis Core <-> Core** | Genesis Kernel | `Pair2`, `Construct8` | Replayed `RelationPage` | ReplayCursor | Deterministic insert via step stream | `Need257`, `Need9` triggers | Rust static assertions / test suites |

---

## 5. Final Judgment

**Verdict:** **ALGEBRA-SAFE**

**Rationale:** The current architectural design cleanly maps to strict relational algebra without polluting the domain with unscalable artifacts. By forcing the 2-byte limit on `Pair2` and maintaining the assumed middle inside the `RelationPage` context, the design guarantees that physical execution maps identically to binary relation algebra. Set, Bag, and Stream multiplicities adhere to proper insertion/rejection math. Projections (RDF, OCEL) cleanly exit the runtime without corrupting the zero-heap, no-JSON hot path. Refusals properly gate capacity without generating invalid "null" states. The architecture is mathematically sound and adheres rigidly to Chatman Equation principles.