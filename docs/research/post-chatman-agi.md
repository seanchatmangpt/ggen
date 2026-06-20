# Post-Chatman ggen AGI — Why, What, How

> Grounded in actual codebase: `crates/ggen-lsp/`, `crates/ggen-graph/`, `crates/genesis-types-v2/`,
> `crates/ggen-core/src/reverse_sync/`, `crates/ggen-core/src/ocel/`. All claims reference real files.

---

## WHY — The Equation Has Outgrown Its Terms

The Chatman equation **A = μ(O)** established a discipline: ontology is source of truth, the μ₁–μ₅
pipeline is the one-way passage, generated artifacts are derived and inert. This was the correct
restorative discipline for a world where code and specification had drifted apart.

It worked. That is the crisis.

### Three symptoms already present in this codebase

**1. `reverse_sync/ast_extractor.rs` — μ⁻¹ exists but is unnamed.**
`crates/ggen-core/src/reverse_sync/mod.rs` exports `extract_rust_service`, `extract_elixir_genserver`,
`extract_go_service`, `convert_to_rdf`. These recover ontological structure from source code.
The inverse pipeline is real. It is just not formally acknowledged as equal in authority to μ.

**2. `ggen-graph/src/ocel/` — the event log is a third ontological pole.**
`check_guard`, `check_lifecycle_order`, `discover_dfg` operate on OCEL event logs.
The event log describes process execution. It is not a side channel. It is a formal object
with the same claim to truth as the RDF triplestore — but currently treated as subordinate.

**3. `ggen-lsp/src/state.rs` — `FileType` stops at Rust/RDF/SPARQL/Tera/TOML.**
USD (`*.usda`), MaterialX (`*.mtlx`), and POWL process law files are produced by the pipeline
but receive no LSP law-surface treatment. The law surfaces stop before the 3D asset layer.
This is an epistemic bypass: generated geometry files live outside the diagnostic authority of ggen.

### The post-Chatman claim

**A ≅ O ≅ L** — artifact, ontology, and event log are isomorphic representations of one canonical
formal object. The pipeline μ is a change-of-notation, not a one-way production function.

This requires:
- μ⁻¹ formally equal to μ in authority (`reverse_sync` promoted)
- L (event log via OCEL) as a first-class pole alongside O and A
- USD/MTLX/POWL law surfaces with diagnostic authority (`ggen-lsp` extended)
- Coherence maintenance across all three poles (drift = diagnostic, not silent)
- Residual-vector repair as the canonical repair workflow (not opaque "fix it")
- POWL process types enabling wasm4pm admission reporting

---

## WHAT — Five Post-Chatman Features

### Feature 1: USD/MTLX LSP Law Surfaces

**Location:** `crates/ggen-lsp/src/analyzers/`

Extend `FileType` in `state.rs` to include `Usd` and `Mtlx`. Add `usd_analyzer.rs` and
`mtlx_analyzer.rs` implementing the `Analyzer` trait. Wire into `build_analyzer()` and
`DocumentAnalyzer`.

Diagnostic codes introduced:
- `GGEN-USD-001` — foreign geometry detected in part file (owner_part_id violation)
- `GGEN-USD-002` — missing `owner_part_id` attribute on prim
- `GGEN-USD-003` — socket prim contains mesh payload (boundary violation)
- `GGEN-MTLX-001` — unbound material input (no upstream connection or default value)

**Why this matters:** Generated USD/MTLX files are currently invisible to ggen's diagnostic
authority. A torso file containing arm geometry is a modular identity violation — but ggen
cannot see it. These analyzers close that gap.

### Feature 2: Three-Pole Coherence Checker

**Location:** `crates/ggen-graph/src/coherence.rs`

`CoherenceChecker` computes BLAKE3 hashes of each pole (O = RDF graph, A = artifact paths,
L = OCEL log) and reports drift as typed `CoherenceDrift` values. The checker is registered
in the `TransitionReceipt` flow so every sync emits a three-pole coherence fingerprint.

Types:
- `PoleState { pole: Pole, hash: Blake3Hash, timestamp: DateTime<Utc> }`
- `CoherenceDrift { kind: DriftKind, source_pole: Pole, target_pole: Pole, delta: String }`
- `CoherenceReport { poles: [PoleState; 3], drifts: Vec<CoherenceDrift>, admitted: bool }`

**Why this matters:** Today a receipt proves O → A was faithful. It says nothing about
whether A is consistent with L, or whether L can reconstruct O. The coherence checker
makes all three poles auditable in one report.

### Feature 3: POWL Process Types

**Location:** `crates/genesis-types-v2/src/lib.rs`

Add POWL-native types to the KNHK V2 type system, enabling `wasm4pm` to ingest and
admit process law graphs as first-class objects.

Types:
- `PowlNode { id: String, activity: String, object_refs: Vec<String> }`
- `PowlEdge { from: String, to: String, condition: Option<String> }`
- `PowlGraph { nodes: Vec<PowlNode>, edges: Vec<PowlEdge>, root: String }`
- `AdmissionStatus` enum: `Alive | PartialAlive | Refused | Unknown`
- `ProcessAdmissionReport { graph_id: String, status: AdmissionStatus, gates: Vec<GateResult>, receipt_hash: Option<String> }`

**Why this matters:** `VisionSnapLoop.powl` and similar process law files produce process
graphs. Without native types, these graphs have no standing in the type system — they are
opaque files. These types give wasm4pm the vocabulary to emit typed admission reports.

### Feature 4: Residual-Vector Repair Types

**Location:** `crates/genesis-types-v2/src/lib.rs`

Formal types for residual measurement, bounded repair selection, and repair admission.
Replaces opaque "make it better" LLM edits with structured, evidenced repair loops.

Types:
- `EvidenceTier` enum: `Known | Inferred | Estimated | Forbidden | ExceptionClass`
- `RepairBand { default_band: (f64, f64), preferred_band: (f64, f64), forbidden_band: (f64, f64), tier: EvidenceTier }`
- `ResidualDimension { name: String, measured: f64, target: (f64, f64), residual: f64 }`
- `ResidualVector { dimensions: Vec<ResidualDimension>, dominant: Option<String> }`
- `BoundedRepairOperator { id: String, targets_dimension: String, band: RepairBand, description: String }`
- `VisualGapReport { render_hash: String, timestamp: DateTime<Utc>, residuals: ResidualVector }`
- `RepairAdmissionReport { operator_id: String, before: ResidualVector, after: ResidualVector, admitted: bool }`

**Why this matters:** The PRD/ARD mandates residual-vector repair before any repair operator
runs. Without these types, the repair loop has no formal contract. With them, every repair
is bounded, typed, and receipted.

### Feature 5: Formal Inverse Pipeline (μ⁻¹)

**Location:** `crates/ggen-core/src/reverse_sync/inverse_pipeline.rs`

Promote `reverse_sync` from an informal utility to a formally named inverse pipeline with
typed stages, matching the five forward stages (μ₁–μ₅).

Stages:
- `μ⁻¹₁` — Scan: enumerate artifact paths (Rust, USD, MTLX)
- `μ⁻¹₂` — Extract: `ServiceDef` / geometry prim structure from AST/text
- `μ⁻¹₃` — Convert: recovered structure → RDF triples via `convert_to_rdf`
- `μ⁻¹₄` — Validate: recovered ontology against known SHACL shapes
- `μ⁻¹₅` — Emit: `InverseReceipt` with input hashes, recovered triple count, validation result

Types:
- `InverseStage` enum: `Scan | Extract | Convert | Validate | Emit`
- `InverseReceipt { operation_id: Uuid, timestamp: DateTime<Utc>, input_hashes: HashMap<String, String>, recovered_triple_count: usize, shacl_valid: bool, signature: String }`
- `InversePipeline` struct with `run(paths: &[PathBuf]) -> Result<InverseReceipt>`

**Why this matters:** `reverse_sync` currently has no receipt, no validation stage, and
no formal claim to authority. Promoting it to μ⁻¹ with typed stages and a signed receipt
makes A → O a first-class provenance path alongside O → A.

---

## HOW — Implementation Discipline

### Branch
`claude/intelligent-gauss-0eq78j` — all changes committed here.

### Testing (Chicago TDD only)
Each feature requires a real state-based test:
- USD analyzer: create a temp `.usda` file with foreign geometry, assert `GGEN-USD-001` diagnostic
- Coherence checker: create divergent pole states, assert `CoherenceDrift` emitted
- POWL types: serialize/deserialize a `PowlGraph`, assert round-trip integrity
- Residual types: construct a `ResidualVector` with known residual, assert dominant dimension
- InversePipeline: run against a real temp Rust file, assert `recovered_triple_count > 0`

### Definition of Done (per feature)
```
just check       — no compiler errors
just test-unit   — all new tests pass
just lint        — no clippy warnings
```

### File budget
| Feature | New files | Modified files |
|---------|-----------|---------------|
| USD/MTLX LSP | 2 new analyzers | `mod.rs`, `state.rs`, `lib.rs` |
| Coherence checker | `coherence.rs` | `lib.rs` (pub mod + pub use) |
| POWL types | 0 (extend `lib.rs`) | `genesis-types-v2/src/lib.rs` |
| Residual types | 0 (extend `lib.rs`) | `genesis-types-v2/src/lib.rs` |
| Inverse pipeline | `inverse_pipeline.rs` | `reverse_sync/mod.rs` |

### No fabrication rule
Every diagnostic code, every type, every test references real codebase paths.
No example output is invented. No API surface is documented before it compiles.
