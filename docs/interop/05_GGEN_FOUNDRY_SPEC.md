# AGENT: ggen Foundry and Membrane Spec

**Status**: IMPLEMENTED / PARTIAL
**File**: `docs/interop/05_GGEN_FOUNDRY_SPEC.md`
**Owner**: ggen Membrane and Foundry Tooling

## 1. ggen Thesis
**Claim**: ggen is the industrial foundry and membrane that manufactures, packages, adapts, validates, and projects Genesis-bearing interchangeable parts.
**Doctrine**: ggen handles *contact*. Genesis handles *consequence*. ggen owns the messy reality of the outside world, translating it into the pure structural discipline of Genesis.
**Evidence**: `crates/ggen-membrane/Cargo.toml` (IMPLEMENTED), `crates/ggen-projection/Cargo.toml` (IMPLEMENTED).

## 2. ggen as Foundry
**Claim**: ggen acts as the industrial manufacturing pipeline for interchangeable parts. It takes input logic, templates, and adapters, and stamps out a Genesis-bearing structure.
**Doctrine**: The foundry ensures that every emitted part has lawful construction bounded by the Chatman Equation ($A = \mu(O)$).
**Evidence**: `crates/ggen-core/src/parts_foundry/part_compiler.rs` (IMPLEMENTED). 

## 3. ggen as Membrane
**Claim**: ggen wraps Genesis in a dependency membrane. It isolates the pure core from parsers, external databases, HTTP clients, and serialization libraries.
**Doctrine**: No outside-world dependencies are allowed inside Genesis.
**Evidence**: `crates/ggen-membrane/src/lib.rs` (IMPLEMENTED). The membrane translates external claims into `RelationPage` primitives.

## 4. ggen as Projection Layer
**Claim**: Genesis parts emit raw, byte-packed `Pair2` arrays and `Construct8` sequences. ggen projects these internal physical representations into human/machine-readable external formats (e.g., JSON, OCEL v2, RDF Turtle).
**Doctrine**: JSON may appear *only* as an external boundary projection, never as an internal representation.
**Evidence**: `crates/ggen-projection/src/lib.rs` and OCEL v2 format projections (IMPLEMENTED).

## 5. ggen as Validation Bridge
**Claim**: ggen bridges the output of Genesis-bearing parts to external validation engines like QLever, DuckDB, SHACL validators, and process-mining (OCEL) tools.
**Doctrine**: External engines are validators and consumers of process-evidence projections, not Genesis replacements. Genesis determines consequence; the validation bridge confirms the laws were upheld.
**Evidence**: `tests/integration/` suites orchestrating `wasm4pm`, `pictl`, and `open-ontologies` validators (PARTIAL / TEST_ONLY).

## 6. ggen as Runtime Packager
**Claim**: ggen encapsulates Genesis parts into runtime bodies providing custody (AtomVM) and portability (WASM), enforced by Rust's physical discipline.
**Doctrine**: AtomVM and WASM are not mere event sources; they are the active runtime bodies for the parts.
**Evidence**: `crates/genesis-wasm-shell/src/lib.rs` (IMPLEMENTED), `crates/ggen-core/src/membrane/core.rs` (IMPLEMENTED for BEAM/WASM targets).

## 7. Genesis-bearing Part Manufacturing Process
**Claim**: The manufacturing process flows from `Source -> Membrane Adapter -> SymbolPage/RelationPage generation -> Genesis Core -> Runtime Packager`. 
**Doctrine**: Truex is the lifecycle for receipted execution trust during manufacturing. Receipts make parts composable.
**Evidence**: `crates/ggen-cli` orchestrating the phases (PARTIAL).

## 8. Input Adapters
**Claim**: Input adapters exist strictly inside ggen. They parse external formats (CSV, RDF, JSON) and convert them into raw bytes, resolving symbols into u32 IDs for Genesis.
**Doctrine**: Genesis never parses. Genesis only receives mathematically bounded primitives.
**Evidence**: `crates/ggen-membrane/src/lib.rs` (IMPLEMENTED).

## 9. Output Projections
**Claim**: Output projections deserialize `RelationPage` boundaries back into strings and graphs.
**Doctrine**: Two-byte `Pair2` arrays are *not* compressed RDF triples; they are tuples in a predicate-fixed `RelationPage`. Projections expand these tuples using the `SymbolPage` mapping.
**Evidence**: `crates/ggen-projection/src/lib.rs` (IMPLEMENTED).

## 10. Symbol-Page Manufacturing
**Claim**: ggen manages the translation of domain vocabularies to u32 integers, creating a `SymbolPage`.
**Doctrine**: Genesis operates exclusively on `SymbolDomain` integer mappings. ggen manages the dictionary lookup.
**Evidence**: `crates/genesis-core-v2/tests/genesis_domain_bounds_test.rs` (IMPLEMENTED).

## 11. RelationPage Manufacturing
**Claim**: ggen constructs `RelationPage`s by binning relationships by predicate.
**Doctrine**: A `RelationPage` is a predicate-fixed relation context. The predicate is *never* stored per tuple.
**Evidence**: `crates/genesis-core-v2/src/primitives.rs`, `crates/ggen-membrane/src/lib.rs` (IMPLEMENTED).

## 12. External Dependency Rules
**Claim**: `ggen` crates manage all dependencies for parsing, networks, async (`tokio`), and serialization (`serde`, `serde_json`).
**Doctrine**: Genesis must remain physically disciplined and heap-free where possible, totally decoupled from ecosystem churn.
**Evidence**: `crates/genesis-core-v2/Cargo.toml` vs `crates/ggen-membrane/Cargo.toml` (IMPLEMENTED).

## 13. Allowed Boundary JSON Usage
**Claim**: JSON is an output-only formatting artifact or an input-only ingestion artifact.
**Doctrine**: JSON does not exist internally. Internal state transitions are governed by `Construct8` arrays and `RelationPage` pages.
**Evidence**: `crates/ggen-projection/src/lib.rs` (IMPLEMENTED).

## 14. Prohibited Core Leakage into Genesis
**Claim**: No system logic regarding SaaS, REST, gRPC, or database connections exists in Genesis. 
**Doctrine**: Genesis is not a parser, adapter layer, serializer, graph database, SQL engine, process-mining engine, or event bus. It is pure state transition and consequence determination.
**Evidence**: `crates/genesis-core-v2/src/lib.rs` (IMPLEMENTED).

## 15. ggen CLI/API Shape
**Claim**: The developer interacts with ggen through its CLI (`ggen-cli`), configuring the manufacturing pipeline with `ggen-config`.
**Doctrine**: The CLI initiates manufacturing, handles template extraction, and orchestrates validations.
**Evidence**: `crates/ggen-cli/src`, `crates/ggen-config/src` (IMPLEMENTED).

## 16. Interop Contracts with Genesis
**Claim**: ggen communicates with Genesis via `RelationPage` injection and `Construct8` packet delivery.
**Doctrine**: Genesis applies the state transition and emits `Receipt`s. ggen uses those receipts to generate composed views and index files. Indexes are receipted derived views, not authority.
**Evidence**: `crates/ggen-core/src/genesis.rs` (IMPLEMENTED).

## 17. Interop Contracts with AtomVM/WASM
**Claim**: ggen packages logic into Erlang/BEAM for AtomVM or WebAssembly.
**Doctrine**: `genesis-wasm-shell` wraps the pure core, allowing JS/WASM interop (e.g., `WasmRelationPage`).
**Evidence**: `crates/genesis-wasm-shell/src/lib.rs` (IMPLEMENTED).

## 18. Interop Contracts with Validation Tools (QLever, DuckDB, SHACL, OCEL)
**Claim**: ggen serializes execution logs into OCEL v2 logs and RDF TTLs, submitting them to independent tools.
**Doctrine**: These engines consume projections. They do not dictate internal logic. If a transition is valid in Genesis but fails SHACL, the validation tool alerts ggen to refuse the part's release.
**Evidence**: `tests/integration/` scripts, `crates/ggen-graph/src/bin/gall_observe_worktree.rs` RDF generation (PARTIAL / TEST_ONLY).

## 19. Definition of Done
**Claim**: The specification is complete when all sub-systems of ggen conform to the thesis of strict separation of *contact* (ggen) and *consequence* (Genesis).
**Doctrine**: 
- A part is manufactured only if its transition logs pass external projection validation.
- No JSON dependencies exist in `genesis-core-v2`.
- `RelationPage` contains no inline predicates per tuple.
- All claims of implementations match actual `.rs` evidence in the repository.
**Evidence**: End-to-end integration tests orchestrating `ggen` -> `Genesis` -> `pictl`/`open-ontologies` verification (MISSING / PARTIAL).
