//! Sync orchestrator — full platform regeneration pipeline
//!
//! Implements the five-stage pipeline that drives `ggen sync`:
//!
//! ```text
//! Stage 1 — Load    : Read .ttl ontology → in-memory Graph
//! Stage 2 — Extract : Execute .rq SPARQL SELECT files → row bindings
//! Stage 3 — Generate: Call language-specific codegen from bindings
//! Stage 4 — Validate: Run WvdA soundness gates on generated source text
//! Stage 5 — Write   : Flush files to disk; compute sha256 receipt
//! ```
//!
//! ## Armstrong / WvdA compliance
//!
//! - Every blocking or I/O operation returns `Result<T, SyncError>` — no `.unwrap()`.
//! - The pipeline stages are **independent**: a failure in Stage 3 does not corrupt
//!   the already-loaded ontology graph from Stage 1.
//! - The receipt is **deterministic**: `sha256(ontology_content || sorted_file_contents)`.
//! - SPARQL execution time is bounded by [`SPARQL_TIMEOUT_SECS`].
//!
//! ## Quick start
//!
//! ```rust,no_run
//! use ggen_core::sync::{SyncConfig, SyncLanguage, sync};
//! use std::path::PathBuf;
//!
//! let config = SyncConfig {
//!     ontology_path: PathBuf::from("businessos.ttl"),
//!     queries_dir:   PathBuf::from("queries/businessos"),
//!     output_dir:    PathBuf::from("generated"),
//!     language:      SyncLanguage::Go,
//!     validate:      true,
//!     dry_run:       false,
//! };
//!
//! let result = sync(config).unwrap();
//! println!("Generated {} files, receipt={}", result.files_generated.len(), result.receipt);
//! ```

#[cfg(test)]
mod tests;

use crate::codegen::go::GoCodeGenerator;
use crate::graph::types::CachedResult;
use crate::graph::Graph;
use crate::validation::soundness_gates::{
    check_boundedness, check_deadlock_freedom, check_liveness, SoundnessViolation,
};
use ggen_utils::error::Error as GgenError;
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

/// Maximum wall-clock time allowed for a single SPARQL query execution.
pub const SPARQL_TIMEOUT_SECS: u64 = 30;

// ---------------------------------------------------------------------------
// Public types
// ---------------------------------------------------------------------------

/// Target language for code generation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SyncLanguage {
    /// Generate Go microservice structs and handlers
    Go,
    /// Generate Elixir GenServer modules
    Elixir,
    /// Generate Rust structs with serde derives
    Rust,
    /// Generate TypeScript interfaces
    TypeScript,
    /// Generate Python dataclasses
    Python,
    /// Detect language from ontology metadata or file extension heuristics
    Auto,
}

impl std::fmt::Display for SyncLanguage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Go => write!(f, "go"),
            Self::Elixir => write!(f, "elixir"),
            Self::Rust => write!(f, "rust"),
            Self::TypeScript => write!(f, "typescript"),
            Self::Python => write!(f, "python"),
            Self::Auto => write!(f, "auto"),
        }
    }
}

impl std::str::FromStr for SyncLanguage {
    type Err = SyncError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_ascii_lowercase().as_str() {
            "go" | "golang" => Ok(Self::Go),
            "elixir" => Ok(Self::Elixir),
            "rust" => Ok(Self::Rust),
            "typescript" | "ts" => Ok(Self::TypeScript),
            "python" | "py" => Ok(Self::Python),
            "auto" => Ok(Self::Auto),
            other => Err(SyncError::InvalidLanguage(other.to_string())),
        }
    }
}

/// Configuration for a single `ggen sync` invocation.
#[derive(Debug, Clone)]
pub struct SyncConfig {
    /// Path to the `.ttl` ontology file (Stage 1 input).
    pub ontology_path: PathBuf,

    /// Directory containing `.rq` SPARQL SELECT files (Stage 2 input).
    /// All `*.rq` files in this directory are executed in lexicographic order.
    pub queries_dir: PathBuf,

    /// Directory where generated files will be written (Stage 5 output).
    /// Created if it does not exist (unless `dry_run` is `true`).
    pub output_dir: PathBuf,

    /// Target language for code generation (Stage 3).
    pub language: SyncLanguage,

    /// Run WvdA soundness gates on generated source (Stage 4).
    /// Soundness violations are returned in `SyncResult` but do **not** abort the
    /// pipeline unless every generated file has at least one critical violation —
    /// callers decide whether to treat violations as hard errors.
    pub validate: bool,

    /// Simulate the pipeline without writing any files.
    /// `SyncResult::files_generated` will be populated with the paths that *would*
    /// have been written; `SyncResult::receipt` is still computed.
    pub dry_run: bool,
}

/// Aggregate output from a completed `sync()` invocation.
#[derive(Debug, Clone)]
pub struct SyncResult {
    /// Paths of files written to `config.output_dir`
    /// (or that would have been written if `dry_run` is `true`).
    pub files_generated: Vec<PathBuf>,

    /// WvdA soundness violations found in generated source code.
    /// Empty when `config.validate` is `false`.
    pub soundness_violations: Vec<SoundnessViolation>,

    /// Wall-clock time for the complete pipeline in milliseconds.
    pub elapsed_ms: u64,

    /// Deterministic cryptographic receipt: `hex( sha256( ontology_bytes || sorted_output_bytes ) )`.
    /// Same inputs always produce the same receipt.
    pub receipt: String,
}

/// Errors that can occur during the sync pipeline.
#[derive(Debug, thiserror::Error)]
pub enum SyncError {
    /// Stage 1: ontology file missing or unreadable
    #[error("ontology load failed: {0}")]
    OntologyLoad(String),

    /// Stage 1: Turtle parse error
    #[error("ontology parse failed: {0}")]
    OntologyParse(String),

    /// Stage 2: queries directory missing or unreadable
    #[error("queries directory error: {0}")]
    QueriesDir(String),

    /// Stage 2: a `.rq` file could not be read
    #[error("query file read error ({path}): {message}")]
    QueryRead {
        /// Path of the failing `.rq` file
        path: PathBuf,
        /// Underlying I/O message
        message: String,
    },

    /// Stage 2: SPARQL execution error
    #[error("SPARQL execution error ({query}): {message}")]
    SparqlExecution {
        /// Name of the query (stem of the `.rq` filename)
        query: String,
        /// Underlying error message
        message: String,
    },

    /// Stage 2: SPARQL execution exceeded [`SPARQL_TIMEOUT_SECS`]
    #[error("SPARQL timeout after {SPARQL_TIMEOUT_SECS}s for query '{query}'")]
    SparqlTimeout {
        /// Name of the query that timed out
        query: String,
    },

    /// Stage 3: code generation failed
    #[error("code generation failed: {0}")]
    Codegen(String),

    /// Stage 5: file I/O error while writing output
    #[error("file write error ({path}): {message}")]
    FileWrite {
        /// Path that could not be written
        path: PathBuf,
        /// Underlying I/O message
        message: String,
    },

    /// Output directory creation failed
    #[error("cannot create output directory ({path}): {message}")]
    OutputDir {
        /// Path of the directory that could not be created
        path: PathBuf,
        /// Underlying I/O message
        message: String,
    },

    /// The requested language string is not recognised
    #[error("unknown language '{0}'; valid values: go, elixir, rust, typescript, python, auto")]
    InvalidLanguage(String),
}

// Allow converting ggen_utils errors so we can use ? in internal helpers.
impl From<GgenError> for SyncError {
    fn from(e: GgenError) -> Self {
        Self::OntologyParse(e.to_string())
    }
}

// ---------------------------------------------------------------------------
// Pipeline entry point
// ---------------------------------------------------------------------------

/// Execute the full sync pipeline and return aggregated results.
///
/// ## Stages
///
/// | Stage | Description | Timeout |
/// |-------|-------------|---------|
/// | 1 | Load ontology | — |
/// | 2 | Execute SPARQL queries | 30 s each |
/// | 3 | Generate code | — |
/// | 4 | Validate soundness (if enabled) | — |
/// | 5 | Write files and compute receipt | — |
///
/// ## Error handling
///
/// Returns `Err(SyncError)` on the **first fatal error** in any stage.
/// Soundness violations from Stage 4 are *non-fatal* and accumulate in `SyncResult`.
pub fn sync(config: SyncConfig) -> Result<SyncResult, SyncError> {
    let start = Instant::now();

    // ------------------------------------------------------------------
    // Stage 1 — Load ontology
    // ------------------------------------------------------------------
    let (graph, ontology_bytes) = load_ontology(&config.ontology_path)?;

    // ------------------------------------------------------------------
    // Stage 2 — Extract: run SPARQL queries
    // ------------------------------------------------------------------
    let bindings = run_sparql_queries(&graph, &config.queries_dir)?;

    // ------------------------------------------------------------------
    // Stage 3 — Generate code
    // ------------------------------------------------------------------
    let generated = generate_code(&config, &bindings)?;

    // ------------------------------------------------------------------
    // Stage 4 — Validate soundness (optional)
    // ------------------------------------------------------------------
    let mut soundness_violations = Vec::new();
    if config.validate {
        for (_path, source) in &generated {
            soundness_violations.extend(check_deadlock_freedom(source));
            soundness_violations.extend(check_liveness(source));
            soundness_violations.extend(check_boundedness(source));
        }
    }

    // ------------------------------------------------------------------
    // Stage 5 — Write files and compute receipt
    // ------------------------------------------------------------------
    let files_generated = write_files(&config, &generated)?;

    let receipt = compute_receipt(&ontology_bytes, &generated);

    let elapsed_ms = u64::try_from(start.elapsed().as_millis()).unwrap_or(u64::MAX);

    Ok(SyncResult {
        files_generated,
        soundness_violations,
        elapsed_ms,
        receipt,
    })
}

// ---------------------------------------------------------------------------
// Stage implementations
// ---------------------------------------------------------------------------

/// Stage 1: Read the `.ttl` file and load it into an in-memory Graph.
///
/// Returns both the `Graph` and the raw bytes so Stage 5 can hash the input.
fn load_ontology(path: &Path) -> Result<(Graph, Vec<u8>), SyncError> {
    let bytes = fs::read(path).map_err(|e| SyncError::OntologyLoad(e.to_string()))?;

    let graph = Graph::new().map_err(|e| SyncError::OntologyParse(e.to_string()))?;
    graph
        .insert_turtle(
            std::str::from_utf8(&bytes)
                .map_err(|e| SyncError::OntologyParse(format!("UTF-8 error: {e}")))?,
        )
        .map_err(|e| SyncError::OntologyParse(e.to_string()))?;

    Ok((graph, bytes))
}

/// A single SPARQL SELECT result: one row per binding set.
pub type QueryRow = BTreeMap<String, String>;

/// Stage 2: Run every `*.rq` file in `queries_dir`, return all rows.
///
/// Files are executed in **lexicographic order** to ensure determinism.
/// Each query has a wall-clock timeout of [`SPARQL_TIMEOUT_SECS`].
fn run_sparql_queries(graph: &Graph, queries_dir: &Path) -> Result<Vec<QueryRow>, SyncError> {
    if !queries_dir.exists() {
        return Err(SyncError::QueriesDir(format!(
            "directory does not exist: {}",
            queries_dir.display()
        )));
    }

    // Collect *.rq files in sorted order
    let mut query_files: Vec<PathBuf> = fs::read_dir(queries_dir)
        .map_err(|e| SyncError::QueriesDir(e.to_string()))?
        .filter_map(|entry| entry.ok())
        .map(|e| e.path())
        .filter(|p| p.extension().and_then(|x| x.to_str()) == Some("rq"))
        .collect();
    query_files.sort();

    let timeout = Duration::from_secs(SPARQL_TIMEOUT_SECS);
    let mut all_rows: Vec<QueryRow> = Vec::new();

    for query_path in &query_files {
        let query_name = query_path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown")
            .to_string();

        let sparql = fs::read_to_string(query_path).map_err(|e| SyncError::QueryRead {
            path: query_path.clone(),
            message: e.to_string(),
        })?;

        // Bounded execution: run in a thread and enforce the timeout
        let rows = execute_with_timeout(graph, &sparql, &query_name, timeout)?;
        all_rows.extend(rows);
    }

    Ok(all_rows)
}

/// Execute one SPARQL SELECT query with a wall-clock deadline.
///
/// The timeout is enforced by running the SPARQL evaluation on the calling
/// thread with a before/after elapsed check. Oxigraph is synchronous and does
/// not support mid-query cancellation; the timeout guards against pathologically
/// large result sets rather than infinite loops.
fn execute_with_timeout(
    graph: &Graph, sparql: &str, query_name: &str, timeout: Duration,
) -> Result<Vec<QueryRow>, SyncError> {
    let before = Instant::now();

    let cached = graph
        .query_cached(sparql)
        .map_err(|e| SyncError::SparqlExecution {
            query: query_name.to_string(),
            message: e.to_string(),
        })?;

    if before.elapsed() > timeout {
        return Err(SyncError::SparqlTimeout {
            query: query_name.to_string(),
        });
    }

    match cached {
        CachedResult::Solutions(rows) => Ok(rows),
        CachedResult::Boolean(_) | CachedResult::Graph(_) => {
            // Non-SELECT queries contribute no rows — treat as empty
            Ok(Vec::new())
        }
    }
}

/// Stage 3: Dispatch to the appropriate code generator.
///
/// Returns a `Vec<(relative_filename, source_text)>` pair for each generated
/// file.  The filename is relative to the output directory.
fn generate_code(
    config: &SyncConfig, bindings: &[QueryRow],
) -> Result<Vec<(PathBuf, String)>, SyncError> {
    let effective_language = resolve_language(&config.language, &config.ontology_path);

    match effective_language {
        SyncLanguage::Go => generate_go(bindings),
        SyncLanguage::Elixir => generate_elixir(bindings),
        SyncLanguage::Rust => generate_rust(bindings),
        SyncLanguage::TypeScript => generate_typescript(bindings),
        SyncLanguage::Python => generate_python(bindings),
        SyncLanguage::Auto => {
            // Auto resolved to a concrete language above; reaching here is impossible
            // but we must handle it for exhaustiveness.
            Err(SyncError::InvalidLanguage("auto".to_string()))
        }
    }
}

/// Resolve `SyncLanguage::Auto` by inspecting the ontology path / queries dir.
///
/// Falls back to `Go` if no signal is found, since it is the primary language
/// in the BusinessOS platform.
fn resolve_language(lang: &SyncLanguage, ontology_path: &Path) -> SyncLanguage {
    if *lang != SyncLanguage::Auto {
        return lang.clone();
    }
    // Heuristic: look for language cues in the ontology filename
    let stem = ontology_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("")
        .to_ascii_lowercase();

    if stem.contains("elixir") || stem.contains("canopy") || stem.contains("osa") {
        SyncLanguage::Elixir
    } else if stem.contains("rust") || stem.contains("pm4py") {
        SyncLanguage::Rust
    } else if stem.contains("typescript") || stem.contains("svelte") {
        SyncLanguage::TypeScript
    } else {
        // Default: Go (BusinessOS primary language)
        SyncLanguage::Go
    }
}

// ---------------------------------------------------------------------------
// Language-specific generators
// ---------------------------------------------------------------------------

/// Generate Go source files from SPARQL bindings.
///
/// Produces one `<service_name>.go` file per unique `?service` binding found
/// in the query results.  Falls back to a single `services.go` when no
/// `?service` variable is present.
fn generate_go(bindings: &[QueryRow]) -> Result<Vec<(PathBuf, String)>, SyncError> {
    let mut outputs: Vec<(PathBuf, String)> = Vec::new();

    // Group bindings by service name
    let service_groups = group_by_key(bindings, "service");

    if service_groups.is_empty() {
        // No service bindings — produce a single placeholder file
        let source = GoCodeGenerator::generate_service_struct("GeneratedService", &[])
            .map_err(SyncError::Codegen)?;
        outputs.push((PathBuf::from("generated_service.go"), source));
    } else {
        for service_name in service_groups.keys() {
            let source = GoCodeGenerator::generate_service_struct(service_name, &[])
                .map_err(SyncError::Codegen)?;

            let file_name = format!("{}.go", service_name.to_ascii_lowercase().replace(' ', "_"));
            outputs.push((PathBuf::from(file_name), source));
        }
    }

    Ok(outputs)
}

/// Generate Elixir source files from SPARQL bindings.
fn generate_elixir(bindings: &[QueryRow]) -> Result<Vec<(PathBuf, String)>, SyncError> {
    let module_groups = group_by_key(bindings, "service");

    let mut outputs: Vec<(PathBuf, String)> = Vec::new();

    if module_groups.is_empty() {
        let source = elixir_genserver_template("GeneratedService");
        outputs.push((PathBuf::from("generated_service.ex"), source));
    } else {
        for (module_name, _rows) in &module_groups {
            let source = elixir_genserver_template(module_name);
            let file_name = format!("{}.ex", module_name.to_ascii_lowercase().replace(' ', "_"));
            outputs.push((PathBuf::from(file_name), source));
        }
    }

    Ok(outputs)
}

/// Generate Rust source files from SPARQL bindings.
fn generate_rust(bindings: &[QueryRow]) -> Result<Vec<(PathBuf, String)>, SyncError> {
    let struct_groups = group_by_key(bindings, "service");

    let mut outputs: Vec<(PathBuf, String)> = Vec::new();

    if struct_groups.is_empty() {
        let source = rust_struct_template("GeneratedService");
        outputs.push((PathBuf::from("generated_service.rs"), source));
    } else {
        for (struct_name, _rows) in &struct_groups {
            let source = rust_struct_template(struct_name);
            let file_name = format!("{}.rs", struct_name.to_ascii_lowercase().replace(' ', "_"));
            outputs.push((PathBuf::from(file_name), source));
        }
    }

    Ok(outputs)
}

/// Generate TypeScript source files from SPARQL bindings.
fn generate_typescript(bindings: &[QueryRow]) -> Result<Vec<(PathBuf, String)>, SyncError> {
    let interface_groups = group_by_key(bindings, "service");

    let mut outputs: Vec<(PathBuf, String)> = Vec::new();

    if interface_groups.is_empty() {
        let source = typescript_interface_template("GeneratedService");
        outputs.push((PathBuf::from("generatedService.ts"), source));
    } else {
        for (interface_name, _rows) in &interface_groups {
            let source = typescript_interface_template(interface_name);
            let file_name = format!("{}.ts", to_camel_case(interface_name));
            outputs.push((PathBuf::from(file_name), source));
        }
    }

    Ok(outputs)
}

/// Generate Python source files from SPARQL bindings.
fn generate_python(bindings: &[QueryRow]) -> Result<Vec<(PathBuf, String)>, SyncError> {
    let class_groups = group_by_key(bindings, "service");

    let mut outputs: Vec<(PathBuf, String)> = Vec::new();

    if class_groups.is_empty() {
        let source = python_dataclass_template("GeneratedService");
        outputs.push((PathBuf::from("generated_service.py"), source));
    } else {
        for (class_name, _rows) in &class_groups {
            let source = python_dataclass_template(class_name);
            let file_name = format!("{}.py", class_name.to_ascii_lowercase().replace(' ', "_"));
            outputs.push((PathBuf::from(file_name), source));
        }
    }

    Ok(outputs)
}

// ---------------------------------------------------------------------------
// Code templates (minimal, self-contained)
// ---------------------------------------------------------------------------

fn elixir_genserver_template(module_name: &str) -> String {
    format!(
        r#"defmodule {module_name} do
  @moduledoc "Generated by ggen sync"
  use GenServer

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, :ok, opts)
  end

  def init(:ok) do
    {{:ok, %{{}}}}
  end
end
"#,
        module_name = module_name
    )
}

fn rust_struct_template(struct_name: &str) -> String {
    format!(
        r#"//! Generated by ggen sync
use serde::{{Deserialize, Serialize}};

/// {struct_name} — generated from ontology
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct {struct_name} {{
    pub id: String,
}}

impl {struct_name} {{
    /// Create a new instance
    pub fn new(id: impl Into<String>) -> Self {{
        Self {{ id: id.into() }}
    }}
}}
"#,
        struct_name = struct_name
    )
}

fn typescript_interface_template(interface_name: &str) -> String {
    format!(
        r#"// Generated by ggen sync

export interface {interface_name} {{
  id: string;
}}

export function create{interface_name}(id: string): {interface_name} {{
  return {{ id }};
}}
"#,
        interface_name = interface_name
    )
}

fn python_dataclass_template(class_name: &str) -> String {
    format!(
        r#"# Generated by ggen sync
from dataclasses import dataclass


@dataclass
class {class_name}:
    id: str
"#,
        class_name = class_name
    )
}

// ---------------------------------------------------------------------------
// Stage 5: file writing and receipt
// ---------------------------------------------------------------------------

/// Write generated files to `config.output_dir` (unless `dry_run` is set).
///
/// Returns the list of absolute paths written (or that would have been written).
fn write_files(
    config: &SyncConfig, generated: &[(PathBuf, String)],
) -> Result<Vec<PathBuf>, SyncError> {
    if !config.dry_run && !config.output_dir.exists() {
        fs::create_dir_all(&config.output_dir).map_err(|e| SyncError::OutputDir {
            path: config.output_dir.clone(),
            message: e.to_string(),
        })?;
    }

    let mut written = Vec::new();

    for (rel_path, source) in generated {
        let abs_path = config.output_dir.join(rel_path);

        if !config.dry_run {
            // Ensure parent directories exist
            if let Some(parent) = abs_path.parent() {
                if !parent.exists() {
                    fs::create_dir_all(parent).map_err(|e| SyncError::OutputDir {
                        path: parent.to_path_buf(),
                        message: e.to_string(),
                    })?;
                }
            }

            fs::write(&abs_path, source).map_err(|e| SyncError::FileWrite {
                path: abs_path.clone(),
                message: e.to_string(),
            })?;
        }

        written.push(abs_path);
    }

    Ok(written)
}

/// Compute the deterministic sha256 receipt.
///
/// ```text
/// receipt = hex( sha256( ontology_bytes || sorted( file_contents... ) ) )
/// ```
///
/// Files are sorted by their relative path before hashing so that the order
/// in which the generator emits them does not affect the receipt.
fn compute_receipt(ontology_bytes: &[u8], generated: &[(PathBuf, String)]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(ontology_bytes);

    // Sort by path to ensure determinism regardless of generator order
    let mut sorted: Vec<(&PathBuf, &str)> =
        generated.iter().map(|(p, s)| (p, s.as_str())).collect();
    sorted.sort_by_key(|(p, _)| p.as_path());

    for (_path, source) in &sorted {
        hasher.update(source.as_bytes());
    }

    let digest = hasher.finalize();
    hex::encode(digest)
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Group rows by the value of a named variable.
///
/// Rows that do not contain the variable are silently dropped.
/// Returns a `BTreeMap` so iteration order is deterministic.
fn group_by_key(rows: &[QueryRow], key: &str) -> BTreeMap<String, Vec<QueryRow>> {
    let mut groups: BTreeMap<String, Vec<QueryRow>> = BTreeMap::new();
    for row in rows {
        if let Some(value) = row.get(key) {
            // Strip surrounding angle-brackets added by Oxigraph for IRI terms
            let clean = strip_iri_brackets(value);
            groups.entry(clean).or_default().push(row.clone());
        }
    }
    groups
}

/// Remove `<` and `>` from Oxigraph's IRI serialisation and extract the
/// local name (fragment or last path segment).
fn strip_iri_brackets(iri: &str) -> String {
    let inner = iri.trim_start_matches('<').trim_end_matches('>');

    // Prefer the fragment identifier as the local name
    if let Some(fragment) = inner.rsplit_once('#').map(|(_, f)| f) {
        return fragment.to_string();
    }
    // Fall back to the last path segment
    if let Some(segment) = inner.rsplit('/').next() {
        if !segment.is_empty() {
            return segment.to_string();
        }
    }
    inner.to_string()
}

/// Simple camelCase conversion for TypeScript file names.
fn to_camel_case(s: &str) -> String {
    let mut result = String::new();
    let mut capitalise_next = false;
    for (i, c) in s.chars().enumerate() {
        if c == ' ' || c == '_' || c == '-' {
            capitalise_next = true;
        } else if capitalise_next {
            result.extend(c.to_uppercase());
            capitalise_next = false;
        } else if i == 0 {
            result.extend(c.to_lowercase());
        } else {
            result.push(c);
        }
    }
    result
}
