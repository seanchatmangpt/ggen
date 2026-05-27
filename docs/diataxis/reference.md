# Reference: ggen-graph API & Vocabulary Specifications

This reference document provides a detailed specification of the `ggen-graph` crate, including its API interfaces, core data structures, error types, and the complete set of supported RDF vocabulary mappings.

This document is organized into the following sections:
- [Technical Architecture & Mapping Abstractions](#technical-architecture--mapping-abstractions)
- [Core Structs & API Specifications](#core-structs--api-specifications)
- [Error Variants](#error-variants)
- [Vocabulary Mappings](#vocabulary-mappings)

---

## Technical Architecture & Mapping Abstractions

`ggen-graph` wraps the `oxigraph` store to provide deterministic, thread-safe RDF graph operations, transactional validation, and state-transition receipts.

The API exposes several concrete Rust structures that map to conceptual components of the semantic pipeline:

| Conceptual Component | Rust Implementation | Purpose |
| :--- | :--- | :--- |
| **Dataset** | [`DeterministicGraph`](file:///Users/sac/ggen/crates/ggen-graph/src/lib.rs#L44-L46) | Represents the active RDF store, facilitating query execution and state hashing. |
| **RdfDelta** | [`RdfDelta`](file:///Users/sac/ggen/crates/ggen-graph/src/lib.rs#L263-L269) | Encapsulates set-based additions and deletions applied atomically. |
| **HookPack** | `&[KnowledgeHook]` | A slice of validation queries checked before committing a transaction. |
| **HookRuntime** | [`KnowledgeHook::execute`](file:///Users/sac/ggen/crates/ggen-graph/src/lib.rs#L356-L370) & [`DeterministicGraph::apply_delta`](file:///Users/sac/ggen/crates/ggen-graph/src/lib.rs#L212-L260) | The execution system for validation hooks, providing rollbacks on failure. |
| **GraphReceipt** | [`TransitionReceipt`](file:///Users/sac/ggen/crates/ggen-graph/src/lib.rs#L373-L385) | Cryptographically binds pre-state, post-state, delta, and timestamps. |

---

## Core Structs & API Specifications

### `DeterministicGraph`

A thread-safe RDF graph wrapper around `oxigraph::store::Store` ensuring deterministic state serialization and cryptographic hashing.

* **Definition:**
  ```rust
  #[derive(Clone)]
  pub struct DeterministicGraph {
      store: Arc<oxigraph::store::Store>,
  }
  ```

* **Associated Functions & Methods:**
  * [`new() -> Result<Self, GraphError>`](file:///Users/sac/ggen/crates/ggen-graph/src/lib.rs#L85-L90): Creates a new empty graph.
  * [`default() -> Self`](file:///Users/sac/ggen/crates/ggen-graph/src/lib.rs#L48-L62): Fallback constructor initializing an empty store.
  * [`insert_quad(&self, quad: &oxigraph::model::Quad) -> Result<(), GraphError>`](file:///Users/sac/ggen/crates/ggen-graph/src/lib.rs#L97-L100): Inserts a quad directly into the store.
  * [`remove_quad(&self, quad: &oxigraph::model::Quad) -> Result<(), GraphError>`](file:///Users/sac/ggen/crates/ggen-graph/src/lib.rs#L107-L110): Removes a quad from the store.
  * [`contains_quad(&self, quad: &oxigraph::model::Quad) -> Result<bool, GraphError>`](file:///Users/sac/ggen/crates/ggen-graph/src/lib.rs#L117-L120): Checks for existence of a quad.
  * [`query(&self, query_str: &str) -> Result<oxigraph::sparql::QueryResults<'_>, GraphError>`](file:///Users/sac/ggen/crates/ggen-graph/src/lib.rs#L127-L133): Evaluates a SPARQL query against the graph state.
  * [`all_quads(&self) -> Result<Vec<oxigraph::model::Quad>, GraphError>`](file:///Users/sac/ggen/crates/ggen-graph/src/lib.rs#L140-L146): Reads and returns all quads in the graph.
  * [`parse_nquad(nquad: &str) -> Result<oxigraph::model::Quad, GraphError>`](file:///Users/sac/ggen/crates/ggen-graph/src/lib.rs#L153-L186): Parses a serialized N-Quad string into an `oxigraph` Quad structure.
  * [`state_hash(&self) -> Result<[u8; 32], GraphError>`](file:///Users/sac/ggen/crates/ggen-graph/src/lib.rs#L193-L203): Computes a deterministic `blake3` hash over all quads sorted alphabetically.
  * [`apply_delta(&self, delta: &RdfDelta, hooks: &[KnowledgeHook]) -> Result<TransitionReceipt, GraphError>`](file:///Users/sac/ggen/crates/ggen-graph/src/lib.rs#L212-L260): Applies deletions and additions. Runs validation hooks; rolls back changes on any validation failure.

---

### `RdfDelta`

Represents a discrete set of modifications to be applied to an RDF graph.

* **Definition:**
  ```rust
  #[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Eq)]
  pub struct RdfDelta {
      pub additions: Vec<String>,
      pub deletions: Vec<String>,
  }
  ```

* **Associated Functions & Methods:**
  * [`new(additions: Vec<String>, deletions: Vec<String>) -> Self`](file:///Users/sac/ggen/crates/ggen-graph/src/lib.rs#L273-L278): Constructs a new delta.
  * [`compute(baseline: &DeterministicGraph, target: &DeterministicGraph) -> Result<Self, GraphError>`](file:///Users/sac/ggen/crates/ggen-graph/src/lib.rs#L285-L319): Computes the difference between a baseline and target graph.
  * [`hash(&self) -> [u8; 32]`](file:///Users/sac/ggen/crates/ggen-graph/src/lib.rs#L322-L333): Computes the `blake3` hash of the delta using sorted string additions and deletions prefixed by `+` and `-`.

---

### `KnowledgeHook`

A validation constraint defined by a SPARQL query to enforce rules over the graph state.

* **Definition:**
  ```rust
  #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
  pub struct KnowledgeHook {
      pub name: String,
      pub sparql_query: String,
  }
  ```

* **Associated Functions & Methods:**
  * [`new(name: String, sparql_query: String) -> Self`](file:///Users/sac/ggen/crates/ggen-graph/src/lib.rs#L347-L349): Constructor for validation hooks.
  * [`execute(&self, graph: &DeterministicGraph) -> Result<bool, GraphError>`](file:///Users/sac/ggen/crates/ggen-graph/src/lib.rs#L356-L370): Executes the SPARQL query. Evaluates `ASK` boolean results or `SELECT` solutions (where an empty solution set indicates compliance).

---

### `TransitionReceipt`

A cryptographically bound receipt generated upon successful application of an `RdfDelta`.

* **Definition:**
  ```rust
  #[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Eq)]
  pub struct TransitionReceipt {
      pub timestamp: chrono::DateTime<chrono::Utc>,
      pub pre_state_hash: [u8; 32],
      pub post_state_hash: [u8; 32],
      pub delta_hash: [u8; 32],
      pub signature_or_hash: [u8; 32],
  }
  ```

* **Associated Functions & Methods:**
  * [`new(pre_state_hash: [u8; 32], post_state_hash: [u8; 32], delta_hash: [u8; 32]) -> Self`](file:///Users/sac/ggen/crates/ggen-graph/src/lib.rs#L389-L409): Constructs a receipt binding state hashes and the current system timestamp.
  * [`verify(&self) -> Result<(), GraphError>`](file:///Users/sac/ggen/crates/ggen-graph/src/lib.rs#L416-L431): Verifies that the `signature_or_hash` matches a local `blake3` hash computed from the receipt fields.

---

## Error Variants

The [`GraphError`](file:///Users/sac/ggen/crates/ggen-graph/src/lib.rs#L18-L39) enumeration defines all error states returned during graph operations:

| Variant | Internal Structure / Cause | Description |
| :--- | :--- | :--- |
| `Oxigraph(StorageError)` | `#[from] oxigraph::store::StorageError` | Failures inside the underlying Oxigraph database engine (e.g. disk or transaction errors). |
| `Sparql(QueryEvaluationError)` | `#[from] oxigraph::sparql::QueryEvaluationError` | Errors parsing or executing SPARQL queries. |
| `Serialization(String)` | `String` error context | Failures during N-Quad parsing or general RDF serialization. |
| `VerificationFailed(String)` | `String` signature mismatch details | Cryptographic hash validation failures on `TransitionReceipt`. |
| `HookFailed(String)` | `String` validation failure message | Errors triggered when a `KnowledgeHook` fails and changes are rolled back. |

---

## Vocabulary Mappings

All metadata and structural declarations in `ggen` adhere to the **Authorized Baseline** namespaces. Standard vocabulary prefix associations are defined in [`standard-vocabularies.ttl`](file:///Users/sac/ggen/.specify/ontologies/standard-vocabularies.ttl):

| Prefix | Namespace IRI | Description |
| :--- | :--- | :--- |
| `rdf` | `http://www.w3.org/1999/02/22-rdf-syntax-ns#` | Core RDF syntax vocabulary (e.g., classes, types, properties). |
| `rdfs` | `http://www.w3.org/2000/01/rdf-schema#` | RDF Schema vocabulary providing basic taxonomic properties (e.g., labels, comments). |
| `owl` | `http://www.w3.org/2002/07/owl#` | Web Ontology Language terms. |
| `xsd` | `http://www.w3.org/2001/XMLSchema#` | XML Schema Datatypes used for data type casting (e.g., `xsd:string`, `xsd:integer`). |
| `prov` | `http://www.w3.org/ns/prov#` | PROV-O provenance ontology mapping execution boundaries and generation ancestry. |
| `sh` | `http://www.w3.org/ns/shacl#` | Shapes Constraint Language namespace used for structural graph validation. |
| `ggen` | `https://ggen.io/marketplace/` | Primary vocabulary for `ggen-marketplace` components (e.g. `Package`, `Dependency`). |
| `gv26` | `http://ggen.dev/v26.5.19#` | Semantic schema for `ggen` release admissions and package metadata. |
| `meta` | `http://ggen.ai/ontology/meta#` | Internal meta-ontology rules and specifications. |
| `dcterms` | `http://purl.org/dc/terms/` | Dublin Core Metadata Terms for annotations and authorship mapping. |
| `ocel` | `CanonicalOCEL.v1` | Schema definition mapping trace events, objects, and object references for process mining. |

### Prolog8 Vocabularies

The `Prolog8` compiler and admission verification stack maps capabilities, errors, and implementations under the following namespaces:

* `p8`: `https://prolog8.dev/ontology#` (Core ontology)
* `type`: `https://prolog8.dev/type#` (Type definitions)
* `cap`: `https://prolog8.dev/cap#` (Implementation capabilities)
* `error`: `https://prolog8.dev/error#` (Admission errors)
* `feat`: `https://prolog8.dev/feat#` (Feature bits)
* `module`: `https://prolog8.dev/module#` (Module definitions)
* `fn`: `https://prolog8.dev/function#` (Function definitions)
* `artifact`: `https://prolog8.dev/artifact#` (Artifact definitions)
* `crate`: `https://prolog8.dev/crate#` (Crate definitions)
* `demo`: `https://prolog8.dev/demo#` (Demo definitions)
* `feature`: `https://prolog8.dev/feature#` (Feature definitions)
* `impl`: `https://prolog8.dev/implementation#` (Implementation definitions)
* `test`: `https://prolog8.dev/test#` (Test definitions)
