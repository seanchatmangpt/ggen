//! Sync Command - The ONLY command in ggen v5
//!
//! `ggen sync` is the unified code synchronization pipeline that replaces ALL
//! previous ggen commands. It transforms domain ontologies through inference
//! rules into typed code via Tera templates.

#![allow(clippy::unused_unit)] // clap-noun-verb macro generates this
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
/// ## Flags
///
/// --manifest PATH         Path to ggen.toml (default: ./ggen.toml)
/// --output-dir PATH       Override output directory from manifest
/// --dry-run               Preview changes without writing files
/// --force                 Overwrite existing files (DESTRUCTIVE - use with --audit)
/// --audit                 Create detailed audit trail in .ggen/audit/
/// --rule NAME             Execute only specific generation rule
/// --verbose               Show detailed execution logs
/// --watch                 Continuous file monitoring and auto-regeneration
/// --validate-only         Run SHACL/SPARQL validation without generation
/// --format FORMAT         Output format: text, json, yaml (default: text)
/// --timeout MS            Maximum execution time in milliseconds (default: 30000)
///
/// ## Flag Combinations
///
/// Safe workflows:
///   ggen sync --dry-run --audit         Preview with audit
///   ggen sync --force --audit           Destructive overwrite with tracking
///   ggen sync --watch --validate-only   Continuous validation
///
/// CI/CD workflows:
///   ggen sync --format json             Machine-readable output
///   ggen sync --validate-only           Pre-flight checks
///
/// Development workflows:
///   ggen sync --watch --verbose         Live feedback
///   ggen sync --rule structs            Focused iteration
///
/// ## Flag Precedence
///
/// --validate-only overrides --force
/// --dry-run prevents file writes (--force has no effect)
/// --watch triggers continuous execution
///
/// ## Safety Notes
///
/// ⚠️  ALWAYS use --audit with --force to enable rollback
/// ⚠️  ALWAYS use --dry-run before --force to preview changes
/// ⚠️  Review docs/features/force-flag.md before using --force
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
/// # Force overwrite with audit trail (RECOMMENDED)
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
///
/// # Complex: Watch, audit, verbose
/// ggen sync --watch --audit --verbose --rule api_endpoints
/// ```
///
/// ## Documentation
///
/// Full feature documentation:
///   - docs/features/audit-trail.md          Audit trail format and usage
///   - docs/features/force-flag.md           Safe destructive workflows
///   - docs/features/merge-mode.md           Hybrid manual/generated code
///   - docs/features/watch-mode.md           Continuous regeneration
///   - docs/features/conditional-execution.md SPARQL ASK conditions
///   - docs/features/validation.md           SHACL/SPARQL constraints
#[allow(clippy::unused_unit, clippy::too_many_arguments)]
#[verb("sync", "root")]
pub fn sync(
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
    let mut options = SyncOptions::default();

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
        options.output_format = match fmt.to_lowercase().as_str() {
            "text" => OutputFormat::Text,
            "json" => OutputFormat::Json,
            _ => {
                // Note: Invalid format will use default (Text)
                // Could enhance with error handling in future
                eprintln!("Warning: Invalid output format '{}', using 'text'. Valid formats: text, json", fmt);
                OutputFormat::Text
            }
        };
    }

    // Note: timeout parameter is accepted but not yet used by SyncOptions
    // Will be implemented in future version
    if timeout.is_some() {
        eprintln!("Warning: --timeout flag is not yet implemented");
    }

    options
}
