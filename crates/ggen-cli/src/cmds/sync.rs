//! Sync Command - The ONLY command in ggen v5
//!
//! `ggen sync` is the unified code synchronization pipeline that replaces ALL
//! previous ggen commands. It transforms domain ontologies through inference
//! rules into typed code via Tera templates.
//!
//! ## Architecture: Three-Layer Pattern
//!
//! - **Layer 3 (CLI)**: Input validation, output formatting, thin routing
//! - **Layer 2 (Integration)**: Async execution, error handling
//! - **Layer 1 (Domain)**: Pure generation logic from ggen_core::codegen
//!
//! ## Exit Codes
//!
//! | Code | Meaning |
//! |------|---------|
//! | 0 | Success |
//! | 1 | Manifest validation error |
//! | 2 | Ontology load error |
//! | 3 | SPARQL query error |
//! | 4 | Template rendering error |
//! | 5 | File I/O error |
//! | 6 | Timeout exceeded |

use clap_noun_verb::Result as VerbResult;
use clap_noun_verb_macros::verb;
use ggen_core::codegen::{OutputFormat, SyncExecutor, SyncOptions, SyncResult};
use serde::Serialize;
use std::path::PathBuf;

// ============================================================================
// Output Types (re-exported for CLI compatibility)
// ============================================================================

/// Output for the `ggen sync` command
#[derive(Debug, Clone, Serialize)]
pub struct SyncOutput {
    /// Overall status: "success" or "error"
    pub status: String,

    /// Number of files synced
    pub files_synced: usize,

    /// Total duration in milliseconds
    pub duration_ms: u64,

    /// Generated files with details
    pub files: Vec<SyncedFile>,

    /// Number of inference rules executed
    pub inference_rules_executed: usize,

    /// Number of generation rules executed
    pub generation_rules_executed: usize,

    /// Audit trail path (if enabled)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub audit_trail: Option<String>,

    /// Error message (if failed)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,
}

/// Individual file sync result
#[derive(Debug, Clone, Serialize)]
pub struct SyncedFile {
    /// File path relative to output directory
    pub path: String,

    /// File size in bytes
    pub size_bytes: usize,

    /// Action taken: "created", "updated", "unchanged"
    pub action: String,
}

impl From<SyncResult> for SyncOutput {
    fn from(result: SyncResult) -> Self {
        Self {
            status: result.status,
            files_synced: result.files_synced,
            duration_ms: result.duration_ms,
            files: result
                .files
                .into_iter()
                .map(|f| SyncedFile {
                    path: f.path,
                    size_bytes: f.size_bytes,
                    action: f.action,
                })
                .collect(),
            inference_rules_executed: result.inference_rules_executed,
            generation_rules_executed: result.generation_rules_executed,
            audit_trail: result.audit_trail,
            error: result.error,
        }
    }
}

// ============================================================================
// The ONLY Command: ggen sync
// ============================================================================

/// Execute the complete code synchronization pipeline from a ggen.toml manifest.
///
/// This is THE ONLY command in ggen v5. It replaces all previous commands
/// (`ggen generate`, `ggen validate`, `ggen template`, etc.) with a single
/// unified pipeline.
///
/// ## Pipeline Flow
///
/// ```text
/// ggen.toml → ontology → CONSTRUCT inference → SELECT → Template → Code
/// ```
///
/// ## Examples
///
/// ```bash
/// # Basic sync (the primary workflow)
/// ggen sync
///
/// # Sync from specific manifest
/// ggen sync --manifest project/ggen.toml
///
/// # Dry-run to preview changes
/// ggen sync --dry-run
///
/// # Sync specific rule only
/// ggen sync --rule structs
///
/// # Force overwrite with audit trail
/// ggen sync --force --audit
///
/// # Watch mode for development
/// ggen sync --watch --verbose
///
/// # Validate without generating
/// ggen sync --validate-only
///
/// # JSON output for CI/CD
/// ggen sync --format json
/// ```
#[allow(clippy::unused_unit, clippy::too_many_arguments)]
#[verb("sync", "root")]
fn sync(
    manifest: Option<String>, output_dir: Option<String>, dry_run: Option<bool>,
    force: Option<bool>, audit: Option<bool>, rule: Option<String>, verbose: Option<bool>,
    watch: Option<bool>, validate_only: Option<bool>, format: Option<String>, timeout: Option<u64>,
) -> VerbResult<SyncOutput> {
    // Build options from CLI args
    let options = build_sync_options(
        manifest,
        output_dir,
        dry_run,
        force,
        audit,
        rule,
        verbose,
        watch,
        validate_only,
        format,
        timeout,
    );

    // Execute via domain executor
    let result = SyncExecutor::new(options)
        .execute()
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

    Ok(SyncOutput::from(result))
}

/// Build SyncOptions from CLI arguments
///
/// This helper keeps the verb function thin by extracting option building.
#[allow(clippy::too_many_arguments)]
fn build_sync_options(
    manifest: Option<String>, output_dir: Option<String>, dry_run: Option<bool>,
    force: Option<bool>, audit: Option<bool>, rule: Option<String>, verbose: Option<bool>,
    watch: Option<bool>, validate_only: Option<bool>, format: Option<String>, timeout: Option<u64>,
) -> SyncOptions {
    let mut options = SyncOptions::new();

    // Set manifest path
    options.manifest_path = PathBuf::from(manifest.unwrap_or_else(|| "ggen.toml".to_string()));

    // Set optional output directory
    if let Some(dir) = output_dir {
        options.output_dir = Some(PathBuf::from(dir));
    }

    // Set boolean flags
    options.dry_run = dry_run.unwrap_or(false);
    options.force = force.unwrap_or(false);
    options.audit = audit.unwrap_or(false);
    options.verbose = verbose.unwrap_or(false);
    options.watch = watch.unwrap_or(false);
    options.validate_only = validate_only.unwrap_or(false);

    // Set selected rules
    if let Some(r) = rule {
        options.selected_rules = Some(vec![r]);
    }

    // Set output format
    if let Some(fmt) = format {
        options.output_format = fmt.parse().unwrap_or(OutputFormat::Text);
    }

    // Set timeout
    if let Some(t) = timeout {
        options.timeout_ms = t;
    }

    options
}
