# ggen Projection Intelligence Analysis Report

This report analyzes the existing pack structures, resolver architecture, LSP diagnostics pipeline, and `tower-lsp-max` composition layer, providing concrete architectural recommendations for implementing the **ggen Projection Intelligence** requirement.

---

## 1. Existing Pack Structure and Resolver

### 1.1 Gpack Structure
* **File Representation**: `crates/ggen-core/src/gpack.rs` defines the manifest structure for `gpack.toml` through `GpackManifest` (lines 123-140).
* **Metadata & Fields**:
  - `gpack`: `GpackMetadata` containing `id`, `name`, `version`, `description`, `license`, and `ggen_compat` (lines 144-151).
  - `dependencies`: `BTreeMap<String, String>` mapping pack IDs to semver constraints (line 127).
  - `templates`: `TemplatesConfig` with glob patterns and Tera includes (line 129).
  - `macros`: `MacrosConfig` specifying paths (line 131).
  - `rdf`: `RdfConfig` specifying base IRI, prefixes, glob patterns, and inline RDF content (line 133).
  - `queries`: `QueriesConfig` specifying query glob patterns and query aliases (line 135).
  - `shapes`: `ShapesConfig` with glob patterns for SHACL shape file discovery (line 137).
  - `preset`: `PresetConfig` for default configuration paths and variable bindings (line 139).
* **Conventions**: By default, `PackConventions` (lines 95-101) discover files in structured paths:
  - Templates: `templates/**/*.tmpl`, `templates/**/*.tera`
  - RDF: `templates/**/graphs/*.ttl`, `templates/**/graphs/*.rdf`, etc.
  - Queries: `templates/**/queries/*.rq`, `templates/**/queries/*.sparql`
  - Shapes: `templates/**/graphs/shapes/*.shacl.ttl`, `templates/**/shapes/*.ttl`

### 1.2 Domain Pack Representation
* **File Representation**: `crates/ggen-core/src/domain/packs/types.rs` defines a domain-level `Pack` struct (lines 9-53) and `PackTemplate` (lines 57-63) representing an installed and fully resolved pack in memory. It tracks variables, SPARQL query contents (`sparql_queries`), dependency lists, readiness flag, and metadata.
* **Storage Location**: Determined by `domain/packs/metadata.rs:get_packs_dir()` (lines 14-47), resolved from:
  1. `GGEN_PACKS_DIR` environment variable.
  2. Relative development paths: `marketplace/packs`, `../marketplace/packs`, `../../marketplace/packs`.
  3. User home directory: `~/.ggen/packs`.

### 1.3 Pack Resolver
* **File Representation**: `crates/ggen-core/src/pack_resolver.rs` contains the `PackResolver` struct (lines 171-178), executing **μ₀ Stage (Pack Resolution)** of the generation pipeline.
* **Resolution Workflow** (`PackResolver::resolve`, lines 232-307):
  1. **Read Lockfile**: Invokes `read_lockfile` to load `.ggen/packs.lock` (deserialized as `PackLockfile` defined in `crates/ggen-core/src/packs/lockfile.rs`).
  2. **Expand Bundles**: Converts bundle aliases (like `"mcp-rust"`) to atomic pack IDs (like `"surface-mcp"`, `"projection-rust"`) using `expand_bundles`.
  3. **Resolve Dependencies**: Recursively resolves transitive dependencies of atomic packs using registry metadata (`get_pack_dependencies` via `package.toml` or `dependencies.json`).
  4. **Check Compatibility**: Validates pack namespace, protocol field, and output path ownership constraints using `OwnershipMap` from `ownership.json` or `package.toml` declarations.
  5. **Enforce Policy Profiles**: Invokes `PolicyEnforcer::enforce` against policy profiles (e.g., `"development"`, `"production"`, `"ciso"`) using `pack_contexts`.
  6. **Merge Ontologies**: Loads RDF ontology files (`ontology/pack.ttl`) from all resolved packs and parses them into a single Oxigraph-compatible `Graph`. Foundation packs are merged first.
  7. **Load Templates & Queries**: Scans cache directories for Tera templates (`.tera`) and SPARQL queries (`.rq`) to produce a final `ResolvedPacks` struct.

---

## 2. LSP Diagnostics: Detection, Processing, and Publication

The `ggen-lsp` server (`crates/ggen-lsp/src/`) behaves as a type-system gate that evaluates "refusal before execution" constraints on source-law surfaces.

### 2.1 Diagnostics Detection
Diagnostics are divided into single-file syntax validations and cross-surface semantic validations:
* **Single-File Validation**:
  - `analyzers::mod.rs:build_analyzer` (lines 286-303) parses the path extension to produce a `DocumentAnalyzer` enum (variant of `Rdf`, `Sparql`, `Tera`, or `Toml`).
  - Each analyzer runs custom parsing logic. E.g., `TomlAnalyzer` parses TOML manifests and extracts structural validation errors; `TeraAnalyzer` evaluates Tera syntax.
* **Cross-Surface (Cross-File) Validation**:
  - `detect_rule_001` (`GGEN-RULE-001`): Detects missing template/query files declared in rules (lines 124-135).
  - `detect_tpl_001` (`GGEN-TPL-001`): Detects unbound variables in Tera templates compared against the SPARQL SELECT projection variables (lines 59-73).
  - `detect_out_001` (`GGEN-OUT-001`): Detects unbound variables inside dynamic output paths (`output_file` patterns) (lines 93-107).
  - `detect_harness_001` (`GGEN-HARNESS-001`): Detects mismatched test/bench harnesses declaring paths to non-existent proof files (lines 37-46).
  - `detect_src_001` (`GGEN-SRC-001`): Detects output files targeting disallowed directories like `generated/` (lines 144-170).
  - `detect_src_002_003_in_dir`: Scans output directories to find missing `DO NOT EDIT` banners or improper source-caste comments in generated Rust code.

### 2.2 Diagnostics Processing
* **Server State Loop** (`state.rs:ServerState::analyze_and_observe`):
  - Splices the current document contents into a live in-memory buffer overlay (`BufferOverlay`) to support live-editing cross-surface updates.
  - Sequentially runs single-file parsing followed by cross-surface detectors (`TPL`, `HARNESS`, `OUT`, `RULE` in Phase A).
  - Merges single-file diagnostics with cross-surface diagnostics (merge-once behavior ensures error squiggles align correctly).
  - Logs lifecycle events (`DiagnosticRaised`, `RouteSelected`, `RepairSuggested`, `RepairApplied`, `GatePassed`, `ReceiptEmitted`) to the on-disk OCEL log (`.agent-admissibility/self_audit.ocel.json`).
* **Headless Gate Loop** (`check.rs:check_files_in_root`):
  - Performs the same single-file and cross-surface detection on disk.
  - Aggregates errors and warnings, and compiles a `CheckReport`.
  - Terminates the process with exit code `1` if any `ERROR`-severity diagnostics are present, functioning as a commit/CI check gate.

### 2.3 Diagnostics Publication
* **Interactive LSP** (`server.rs`): Runs `refresh_analyzer` inside `did_open`/`did_change`, which maps output arrays of `(Url, Vec<Diagnostic>)` and publishes them to the client editor via `self.client.publish_diagnostics(target_uri, diagnostics, None)`.
* **Headless CLI**: Outputs diagnostic details to `stdout` (plain text or structured JSON).

---

## 3. tower-lsp-max Crate Layout & Integration

### 3.1 tower-lsp-max Crate Layout
The `tower-lsp-max` repository is composed of several modular crates:
* `tower-lsp-max-base`: Contains pipeline trait abstractions: `SourceObservation`, `ParseIngress` (parses raw files), `RelationAdmitter` (determines relational semantic validity), and `StaticIndexEmitter` (emits LSIF 0.6.0).
* `tower-lsp-max-protocol`: Declares LSP structures, custom RPC methods (`max/*`), capability vectors, receipts, and the extended `MaxDiagnostic` containing metadata (violated axes, repair actions, verification gates, receipt obligations).
* `tower-lsp-max-runtime`: Contains the typestate lifecycle engine (`AutonomicMesh` with instances mapping `Uninitialized -> Initializing -> Initialized -> ShutDown -> Exited`), process ledger, and mesh hooks. It also implements the `MaterializedViewStore` which stores materialized hovers, definitions, and diagnostics in `DashMap` indices.
* `crates/playground`: A testing server harness acting as a mock provider of LSP operations.

### 3.2 Routing, Composition, and Composition Strategy
* **Oxigraph view population**: `tower-lsp-max-runtime/src/control_plane/views/update.rs` clears and rebuilds views from the Oxigraph triple store. Diagnostics are populated via `populate_diagnostics` (in `populate_hover_diag.rs`) using SPARQL queries `QUERY_LSIF_DIAGNOSTICS` (loading serialized LSIF diagnostic data) and `QUERY_LIVE_DIAGNOSTICS` (loading live control-plane errors).
* **Materialized Storage**: Diagnostics are stored inside `MaterializedViewStore.diagnostics` which maps `Url -> Vec<Diagnostic>` (defined in `types.rs`).

### 3.3 Composing ggen-lsp Diagnostics
`tower-lsp-max` can compose and load `ggen-lsp` diagnostics using two clean pathways:
1. **Direct Composition In Client Wrapper**: `tower-lsp-max` acts as a composite LSP proxy. When fanning out document syncs and hover/diagnostic requests, it calls the `ggen-lsp` analyzer engine (either running as a child process or integrated library) and merges the resulting diagnostics into the `MaterializedViewStore` under the respective document `Url`.
2. **Attribution tagging via `source_id`**: Diagnostics returned from `ggen-lsp` should be converted into `MaxDiagnostic` instances, assigning `source_id = "ggen-lsp"`. These are placed in the `MaterializedViewStore` alongside dynamic compiler or downstream diagnostics (e.g. `rust-analyzer`), fanning out a consolidated list to the editor.

---

## 4. Recommended R1 Structures

To support ggen Projection Intelligence, R1 structures must manage metadata, plans, mappings, overrides, and verification chains. These should be defined inside a new library module in `crates/ggen-projection/src/` (as planned in the `PROJECT.md` layout) or under `crates/ggen-core/src/domain/packs/projection.rs` and re-exported.

The proposed Rust structures look as follows:

```rust
use std::collections::{BTreeMap, HashMap};
use std::path::PathBuf;
use serde::{Serialize, Deserialize};

/// 1. PackDescriptor: Represents the versioned metadata and file bindings of a template pack.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct PackDescriptor {
    pub id: String,
    pub name: String,
    pub version: String,
    pub description: String,
    pub license: String,
    pub dependencies: BTreeMap<String, String>, // Pack ID -> Semver constraint
    pub templates: Vec<PackTemplateDescriptor>,
    pub query_aliases: BTreeMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct PackTemplateDescriptor {
    pub path: PathBuf,
    pub description: String,
    pub variables: Vec<String>,
}

/// 2. PackPlan: Represents the execution plan resolved for a target sync.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct PackPlan {
    pub target_profile: String,
    pub packs: Vec<PackDescriptor>,
    pub resolution_order: Vec<String>, // Topologically sorted pack IDs
    pub checksums: HashMap<String, String>, // Pack ID -> Blake3 hash
}

/// 3. ProjectionMap: Maps generated output files back to their source templates and queries.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ProjectionMap {
    pub mappings: HashMap<PathBuf, ProjectionMapping>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ProjectionMapping {
    pub pack_id: String,
    pub template_path: PathBuf,
    pub query_path: Option<PathBuf>,
    pub bound_variables: Vec<String>,
    pub merge_strategy: String, // E.g., "Exclusive", "Mergeable", "Overlay"
}

/// 4. CustomizationMap: Tracks overrides, manual sections, and variable overrides.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct CustomizationMap {
    pub vars: HashMap<String, String>, // Variable name -> Override value
    pub file_overrides: HashMap<PathBuf, String>, // File path -> Specific merge rules
}

/// 5. ReceiptIndex: Tracks and chains cryptographic verification receipts.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReceiptIndex {
    pub receipts: HashMap<String, CryptographicReceipt>,
    pub last_updated: chrono::DateTime<chrono::Utc>,
    pub index_hash: String, // Blake3 hash of the combined index
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CryptographicReceipt {
    pub target_id: String,      // Output file path or Pack ID
    pub receipt_id: String,     // Unique UUID
    pub blake3_hash: String,    // Hash of the generated code / ontology state
    pub signature: Option<String>,
    pub verified_at: chrono::DateTime<chrono::Utc>,
}
```

---

## 5. Files to Modify for "ggen sync" CLI / Pipeline Step

Implementing the Projection Intelligence step in `ggen sync` requires updating the CLI parsing layer, the step execution sequence, and the verification/receipt pipeline:

1. **`crates/ggen-cli/src/cmds/sync.rs`**:
   - Expose new CLI options, such as `--pack-plan <PATH>`, `--profile <PROFILE>`, and `--locked` mode validation.
   - Refactor `emit_sync_receipt` to support the new `ReceiptIndex` formatting and write it to `.ggen/receipts/index.json`.
2. **`crates/ggen-core/src/codegen/executor.rs`**:
   - Extend `SyncExecutor::execute` to include a **μ₀ stage** check before proceeding to manifest parsing.
   - Invoke `PackResolver` to load resolved packs, build the `PackPlan`, and verify the safety of pack dependencies.
3. **`crates/ggen-core/src/codegen/pipeline.rs`**:
   - Integrate the `ResolvedPacks` ontology graph and custom Tera templates directly into the generator pipeline.
   - Export the compilation audit trail to serialize the `ProjectionMap` and `CustomizationMap` to the generated output directory.
4. **`crates/ggen-core/src/pack_resolver.rs`**:
   - Update resolver interfaces to output structural `PackPlan` objects containing fully populated `PackDescriptor`s.
