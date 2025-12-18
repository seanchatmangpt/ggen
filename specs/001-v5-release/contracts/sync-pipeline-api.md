# Sync Pipeline API Contract

**Version**: 1.0.0
**Feature**: ggen v5.0.0 Release
**Date**: 2025-12-17

## Overview

This document defines the internal API contracts for the ggen v5 sync pipeline. These contracts specify how components interact across the three architectural layers: CLI → Executor → Pipeline.

---

## Layer 3: CLI → Executor Interface

### SyncOptions (Input Contract)

**Purpose**: Convert CLI arguments to structured options for executor

```rust
pub struct SyncOptions {
    /// Path to ggen.toml manifest
    pub manifest_path: PathBuf,

    /// Override output directory from manifest
    pub output_dir: Option<PathBuf>,

    /// Sync execution mode
    pub mode: SyncMode,

    /// Preview changes without writing files
    pub dry_run: bool,

    /// Overwrite existing files
    pub force: bool,

    /// Generate audit trail (audit.json)
    pub audit: bool,

    /// Sync only specific rule(s) by name
    pub rule_filter: Option<Vec<String>>,

    /// Enable verbose output
    pub verbose: bool,

    /// Override SPARQL query timeout (milliseconds)
    pub timeout_ms: Option<u64>,

    /// Output format (json | text)
    pub format: OutputFormat,
}

pub enum SyncMode {
    /// Full regeneration (ignore staleness)
    Full,
    /// Incremental (regenerate only stale artifacts) - v5.1+
    Incremental,
    /// Validate consistency without modifying files
    Verify,
}

pub enum OutputFormat {
    Json,
    Text,
}
```

**Validation Rules**:
- manifest_path MUST exist and be readable
- If mode=Incremental AND .ggen/sync-state.json missing → error
- timeout_ms MUST be > 0 if provided

---

### SyncOutput (Output Contract)

**Purpose**: Return execution results to CLI layer for display

```rust
pub struct SyncOutput {
    /// Operation succeeded
    pub success: bool,

    /// Files generated or modified
    pub modified_files: Vec<PathBuf>,

    /// Files skipped (incremental mode)
    pub skipped_files: Vec<PathBuf>,

    /// Error messages (if success=false)
    pub errors: Vec<String>,

    /// Warnings (non-fatal issues)
    pub warnings: Vec<String>,

    /// Execution metrics
    pub metrics: SyncMetrics,
}

pub struct SyncMetrics {
    /// Total execution time (milliseconds)
    pub duration_ms: u64,

    /// Number of triples loaded
    pub triples_loaded: usize,

    /// Number of CONSTRUCT queries executed
    pub construct_count: usize,

    /// Number of SELECT queries executed
    pub select_count: usize,

    /// Number of templates rendered
    pub templates_rendered: usize,

    /// Number of files written
    pub files_written: usize,
}
```

**Exit Code Mapping**:
```rust
impl From<&SyncOutput> for i32 {
    fn from(output: &SyncOutput) -> i32 {
        if output.success {
            0  // Success
        } else {
            // Determine exit code from first error
            match output.errors.first().map(|e| e.as_str()) {
                Some(e) if e.contains("manifest") => 1,  // Manifest error
                Some(e) if e.contains("ontology") => 2,  // Ontology load error
                Some(e) if e.contains("SPARQL") => 3,    // Query error
                Some(e) if e.contains("template") => 4,  // Template error
                Some(e) if e.contains("I/O") || e.contains("permission") => 5,  // File I/O error
                Some(e) if e.contains("timeout") => 6,   // Timeout
                _ => 1,  // Generic error
            }
        }
    }
}
```

---

## Layer 2: Executor → Pipeline Interface

### GgenManifest (Configuration Contract)

**Purpose**: Parsed and validated manifest passed to pipeline

```rust
pub struct GgenManifest {
    /// Manifest schema version
    pub version: String,  // "1.0"

    /// Ontology configuration
    pub ontology: OntologyConfig,

    /// Inference rules (CONSTRUCT queries)
    pub inference: InferenceConfig,

    /// Generation rules (SELECT + templates)
    pub generation: GenerationConfig,

    /// Base directory (resolved from manifest location)
    pub base_dir: PathBuf,
}

pub struct OntologyConfig {
    /// Primary ontology source file
    pub source: PathBuf,

    /// Imported ontologies
    pub imports: Vec<PathBuf>,
}

pub struct InferenceConfig {
    /// CONSTRUCT rules with execution order
    pub rules: Vec<InferenceRule>,
}

pub struct InferenceRule {
    /// Execution order (ascending)
    pub order: u32,

    /// SPARQL CONSTRUCT query
    pub query: String,

    /// Optional rule name
    pub name: Option<String>,
}

pub struct GenerationConfig {
    /// Output directory for generated files
    pub output_dir: PathBuf,

    /// Default generation mode
    pub mode: GenerationMode,

    /// Generation rules
    pub rules: Vec<GenerationRule>,
}

pub struct GenerationRule {
    /// Rule identifier
    pub name: String,

    /// SPARQL SELECT query (inline or file path)
    pub query: QuerySource,

    /// Tera template (inline or file path)
    pub template: TemplateSource,

    /// Output file path pattern
    pub output_file: String,

    /// File generation mode
    pub mode: GenerationMode,

    /// Unix file permissions (optional)
    pub permissions: Option<u32>,
}

pub enum QuerySource {
    Inline(String),
    File(PathBuf),
}

pub enum TemplateSource {
    Inline(String),
    File(PathBuf),
}

pub enum GenerationMode {
    /// Create new file (error if exists)
    Create,

    /// Overwrite existing file
    Overwrite,

    /// Merge with manual regions - v5.1+
    Merge,
}
```

---

### PipelineResult (Execution Contract)

**Purpose**: Return pipeline execution results to executor

```rust
pub struct PipelineResult {
    /// Execution succeeded
    pub success: bool,

    /// Generated artifacts
    pub artifacts: Vec<GeneratedArtifact>,

    /// Errors encountered
    pub errors: Vec<PipelineError>,

    /// Warnings (non-fatal)
    pub warnings: Vec<String>,

    /// Pipeline metrics
    pub metrics: PipelineMetrics,
}

pub struct GeneratedArtifact {
    /// Output file path
    pub path: PathBuf,

    /// Generation timestamp
    pub generated_at: SystemTime,

    /// Content hash (SHA-256)
    pub content_hash: String,

    /// File size (bytes)
    pub size_bytes: u64,
}

pub enum PipelineError {
    ManifestError { message: String, line: Option<usize> },
    OntologyLoadError { path: PathBuf, message: String },
    SparqlError { query: String, message: String, line: Option<usize> },
    TemplateError { template: String, message: String, line: Option<usize> },
    IoError { path: PathBuf, message: String },
    TimeoutError { operation: String, timeout_ms: u64 },
}

pub struct PipelineMetrics {
    /// Stage timings
    pub stage_durations: HashMap<String, Duration>,

    /// Triple counts
    pub triples_loaded: usize,
    pub triples_inferred: usize,

    /// Query execution counts
    pub construct_executed: usize,
    pub select_executed: usize,

    /// Cache statistics
    pub cache_hits: usize,
    pub cache_misses: usize,
}
```

---

## Layer 1: Pipeline Stages Interface

### Stage 1: Manifest Loading

**Input**: `PathBuf` (manifest path)
**Output**: `Result<GgenManifest, ManifestError>`

```rust
pub fn load_manifest(path: &Path) -> Result<GgenManifest, ManifestError> {
    // 1. Read TOML file
    // 2. Parse with serde
    // 3. Validate schema version
    // 4. Resolve relative paths to absolute
    // 5. Return manifest
}
```

**Error Cases**:
- File not found
- Invalid TOML syntax
- Unsupported schema version
- Invalid file paths

---

### Stage 2: Ontology Loading

**Input**: `OntologyConfig`
**Output**: `Result<Store, OntologyLoadError>`

```rust
pub fn load_ontology(config: &OntologyConfig) -> Result<Store, OntologyLoadError> {
    // 1. Create Oxigraph Store
    // 2. Load primary ontology
    // 3. Load imports
    // 4. Return populated store
}
```

**Error Cases**:
- Ontology file not found
- Invalid RDF syntax
- Import resolution failure

---

### Stage 3: Inference Execution

**Input**: `Store`, `Vec<InferenceRule>`
**Output**: `Result<InferenceMetrics, SparqlError>`

```rust
pub fn execute_inference(
    store: &Store,
    rules: &[InferenceRule],
) -> Result<InferenceMetrics, SparqlError> {
    // 1. Sort rules by order
    // 2. For each rule:
    //    a. Execute CONSTRUCT query
    //    b. Materialize inferred triples into store
    //    c. Track triple count added
    // 3. Return metrics
}

pub struct InferenceMetrics {
    pub rules_executed: usize,
    pub triples_inferred: usize,
    pub duration: Duration,
}
```

**Error Cases**:
- Invalid SPARQL CONSTRUCT syntax
- Query timeout
- Circular dependency detected

---

### Stage 4: Data Extraction

**Input**: `Store`, `Vec<GenerationRule>`
**Output**: `Result<HashMap<String, Value>, SparqlError>`

```rust
pub fn extract_data(
    store: &Store,
    rules: &[GenerationRule],
) -> Result<HashMap<String, Value>, SparqlError> {
    // 1. For each rule:
    //    a. Load SPARQL SELECT query
    //    b. Execute query
    //    c. Convert QuerySolution to JSON Value
    //    d. Store in context map with rule name as key
    // 2. Return merged context
}
```

**Error Cases**:
- Invalid SPARQL SELECT syntax
- Query timeout
- Query returns no results (warning)

---

### Stage 5: Template Rendering

**Input**: `GenerationRule`, `HashMap<String, Value>` (context)
**Output**: `Result<String, TemplateError>`

```rust
pub fn render_template(
    rule: &GenerationRule,
    context: &HashMap<String, Value>,
) -> Result<String, TemplateError> {
    // 1. Load Tera template
    // 2. Render with context
    // 3. Return rendered string
}
```

**Error Cases**:
- Template file not found
- Invalid Tera syntax
- Missing required variable
- Rendering logic error

---

### Stage 6: File Writing

**Input**: `PathBuf`, `String` (content), `GenerationMode`
**Output**: `Result<GeneratedArtifact, IoError>`

```rust
pub fn write_file(
    path: &Path,
    content: &str,
    mode: GenerationMode,
) -> Result<GeneratedArtifact, IoError> {
    // 1. Check file existence based on mode
    // 2. Create parent directories if needed
    // 3. Write content to file
    // 4. Set permissions (if specified)
    // 5. Compute content hash
    // 6. Return artifact metadata
}
```

**Error Cases**:
- File already exists (Create mode)
- Permission denied
- Disk space exhausted
- Invalid file path

---

## Error Handling Contract

### Error Propagation

All errors MUST be propagated using `Result<T, E>` (no panics in production code).

**Error Type Hierarchy**:
```rust
pub enum SyncError {
    Manifest(ManifestError),
    Ontology(OntologyLoadError),
    Sparql(SparqlError),
    Template(TemplateError),
    Io(IoError),
    Timeout(TimeoutError),
}

impl From<ManifestError> for SyncError { ... }
impl From<OntologyLoadError> for SyncError { ... }
// ... etc
```

### Error Context

Errors MUST include actionable context:
- File path (if applicable)
- Line/column number (for syntax errors)
- Suggested fix (if known)

**Example**:
```rust
ManifestError {
    message: "Invalid ontology source path",
    path: Some("ontology/missing.ttl".into()),
    suggestion: Some("Check that the file exists and is readable"),
}
```

---

## Performance Contract

### Timeouts

All operations MUST respect timeout limits:
- SPARQL queries: configurable (default: 5000ms)
- Template rendering: 10000ms hard limit
- File I/O: 30000ms hard limit

### Caching

Query cache MUST be used for all SPARQL executions:
- Cache key: `(query_hash, ontology_version)`
- Cache size: 1000 entries (LRU eviction)
- Invalidation: On ontology modification (version increment)

---

## Testing Contract

### Unit Test Requirements

Each pipeline stage MUST have unit tests covering:
- ✅ Happy path (valid inputs → success)
- ✅ Error cases (invalid inputs → specific errors)
- ✅ Edge cases (empty inputs, boundary values)

### Integration Test Requirements

Full pipeline MUST have tests covering:
- ✅ End-to-end sync (manifest → generated files)
- ✅ Multi-rule orchestration
- ✅ Error propagation across stages
- ✅ Performance (within SLO thresholds)

---

## Versioning Contract

### API Stability

- **Layer 3 (CLI)**: External API - semver compatibility required
- **Layer 2 (Executor)**: Internal API - can change across minor versions
- **Layer 1 (Pipeline)**: Internal API - can change freely

### Manifest Version Support

ggen v5 MUST support:
- ✅ ggen.toml v1.0 (current)
- ⚠️ Future versions MUST be backward compatible or provide auto-migration

---

## Conclusion

This contract specification defines the boundaries between ggen v5's architectural layers. All implementations MUST adhere to these contracts to ensure modularity, testability, and maintainability.

**Next Steps**: Implement pipeline stages following Chicago TDD (tests first, then implementation).
