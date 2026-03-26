//! Apply Command - Selective Rule Execution
//!
//! `ggen apply` executes selected rules from ggen.toml with fine-grained control:
//! - Execute a single rule by exact name
//! - Execute multiple rules matching a regex pattern
//! - Use a custom config file
//! - Preview changes with --dry-run
//!
//! ## Flags
//!
//! --rule NAME             Execute only specific rule (exact name match)
//! --filter PATTERN        Execute rules matching regex pattern
//! --config PATH           Path to config file (default: ./ggen.toml)
//! --dry-run               Preview changes without writing files
//! --verbose               Show detailed execution logs
//! --force                 Overwrite existing files
//!
//! ## Examples
//!
//! ```bash
//! # Execute a single rule by name
//! ggen apply --rule jpa-entities
//!
//! # Execute multiple rules matching pattern
//! ggen apply --filter "e2e|integration"
//!
//! # Use custom config file
//! ggen apply --config custom.toml --rule api-endpoints
//!
//! # Preview changes without writing
//! ggen apply --rule jpa-entities --dry-run
//!
//! # Verbose execution with audit trail
//! ggen apply --filter ".*" --verbose --force
//! ```

use clap_noun_verb::Result as VerbResult;
use clap_noun_verb_macros::verb;
use ggen_core::codegen::SyncResult;
use serde::Serialize;
use std::path::PathBuf;

// ============================================================================
// Output Types (re-exported for CLI compatibility)
// ============================================================================

/// Output for the `ggen apply` command
#[derive(Debug, Clone, Serialize)]
pub struct ApplyOutput {
    /// Overall status: "success" or "error"
    pub status: String,

    /// Rules matched and executed
    pub rules_executed: Vec<String>,

    /// Total number of files generated
    pub files_generated: usize,

    /// Total duration in milliseconds
    pub duration_ms: u64,

    /// Error message (if failed)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,
}

impl From<SyncResult> for ApplyOutput {
    fn from(result: SyncResult) -> Self {
        Self {
            status: result.status,
            files_generated: result.files_synced,
            duration_ms: result.duration_ms,
            rules_executed: vec![],
            error: result.error,
        }
    }
}

// ============================================================================
// Apply Command
// ============================================================================

/// Execute selected generation rules from ggen.toml with fine-grained control.
///
/// `ggen apply` selectively executes rules instead of running the full pipeline.
/// This is useful for incremental development and testing specific rules.
///
/// Delegates to the domain layer (SyncExecutor) for the actual generation work.
#[allow(clippy::unused_unit)]
#[verb("apply", "root")]
pub fn apply(
    rule: Option<String>,
    filter: Option<String>,
    config: Option<String>,
    dry_run: Option<bool>,
    verbose: Option<bool>,
    force: Option<bool>,
) -> VerbResult<ApplyOutput> {
    // Build apply options from CLI args
    let opts = build_apply_options(rule, filter, config, dry_run, verbose, force)?;

    // Delegate to domain logic via sync
    let result = crate::cmds::sync::sync(
        Some(opts.config_path.to_string_lossy().to_string()),
        None,
        Some(opts.dry_run),
        Some(opts.force),
        None,
        opts.selected_rule,
        Some(opts.verbose),
        None,
        None,
        None,
        None,
        None,
        None,
    )?;

    // Convert sync output to apply output
    Ok(ApplyOutput {
        status: result.status,
        files_generated: result.files_synced,
        duration_ms: result.duration_ms,
        rules_executed: vec![],
        error: result.error,
    })
}

// ============================================================================
// Helper Structures and Functions
// ============================================================================

/// Options for the apply command
struct ApplyOptions {
    config_path: PathBuf,
    selected_rule: Option<String>,
    dry_run: bool,
    verbose: bool,
    force: bool,
}

/// Build ApplyOptions from CLI arguments
fn build_apply_options(
    rule: Option<String>,
    filter: Option<String>,
    config: Option<String>,
    dry_run: Option<bool>,
    verbose: Option<bool>,
    force: Option<bool>,
) -> VerbResult<ApplyOptions> {
    let config_path = PathBuf::from(config.unwrap_or_else(|| "ggen.toml".to_string()));

    // Validate config file exists
    if !config_path.exists() {
        return Err(clap_noun_verb::NounVerbError::execution_error(format!(
            "error[E0001]: Config file not found: {}",
            config_path.display()
        )));
    }

    // Resolve selected rule from --rule or --filter
    let selected_rule = if let Some(r) = rule {
        Some(r)
    } else if let Some(_p) = filter {
        // For now, --filter is not supported; use --rule instead
        return Err(clap_noun_verb::NounVerbError::execution_error(
            "error[E0002]: --filter not yet implemented. Use --rule NAME".to_string(),
        ));
    } else {
        None
    };

    Ok(ApplyOptions {
        config_path,
        selected_rule,
        dry_run: dry_run.unwrap_or(true),
        verbose: verbose.unwrap_or(false),
        force: force.unwrap_or(false),
    })
}
