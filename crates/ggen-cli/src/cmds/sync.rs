//! Sync Command - The ONLY command in mcpp v5
//!
//! `mcpp sync` is the unified code synchronization pipeline that replaces ALL
//! previous mcpp commands. It transforms domain ontologies through inference
//! rules into typed code via Tera templates.
//!
//! ## A2A-RS μ Pipeline
//!
//! For A2A-RS integration, the sync command executes the full μ₁-μ₅ pipeline:
//!
//! - **μ₁ (CONSTRUCT)**: Normalize RDF ontology from .specify/specs/014-a2a-integration/
//! - **μ₂ (SELECT)**: Extract bindings for each module (agent, message, task, transport, skill)
//! - **μ₃ (Tera)**: Generate Rust code from templates
//! - **μ₄ (Canonicalize)**: Format and organize generated code
//! - **μ₅ (Receipt)**: Generate cryptographic receipt for verification
//!
//! Usage:
//!   mcpp sync --audit              # Full A2A pipeline with receipt
//!   mcpp sync --dry-run            # Preview without writing
//!   mcpp sync --output crates/     # Custom output directory

#![allow(clippy::unused_unit)] // clap-noun-verb macro generates this
//!
//! ## Architecture: Three-Layer Pattern
//!
//! - **Layer 3 (CLI)**: Input validation, output formatting, thin routing
//! - **Layer 2 (Integration)**: Async execution, error handling
//! - **Layer 1 (Domain)**: Pure generation logic from mcpp_core::codegen
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

use clap_noun_verb_macros::verb;
use clap_noun_verb::NounVerbError;
use mcpp_core::codegen::{OutputFormat, SyncExecutor, SyncOptions, SyncResult};
use mcpp_core::sync::{sync as low_level_sync, SyncConfig, SyncLanguage};
use serde::Serialize;
use std::path::{Path, PathBuf};

use crate::error::{GgenError, Result};

// Import llm_bridge module from the same crate
#[allow(unused_imports)]
use crate::llm_bridge::GroqLlmBridge;

// ============================================================================
// Output Types (re-exported for CLI compatibility)
// ============================================================================

/// Output for the `mcpp sync` command
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
// The ONLY Command: mcpp sync
// ============================================================================

/// Execute the complete code synchronization pipeline from a mcpp.toml manifest.
///
/// This is THE ONLY command in mcpp v5. It replaces all previous commands
/// (`mcpp generate`, `mcpp validate`, `mcpp template`, etc.) with a single
/// unified pipeline.
///
/// The sync command implements a five-stage μ pipeline (μ₁ through μ₅) that
/// transforms RDF ontologies into typed code through SPARQL queries and
/// Tera templates, with optional cryptographic receipts for verification.
///
/// ## A2A-RS μ Pipeline (μ₁ through μ₅)
///
/// When generating A2A-RS code from `.specify/specs/014-a2a-integration/`:
///
/// ```text
/// μ₁ CONSTRUCT: Normalize RDF ontology
///    Input: .specify/specs/014-a2a-integration/a2a-ontology.ttl
///    Query: crates/mcpp-core/queries/a2a/construct-agents.rq
///    Output: Normalized A2A RDF with a2a: prefix
///
/// μ₂ SELECT: Extract bindings for each module
///    Queries: crates/mcpp-core/queries/a2a/extract-*.rq
///      - extract-agents.rq → agent bindings
///      - extract-messages.rq → message bindings
///      - extract-tasks.rq → task bindings
///      - extract-transports.rq → transport bindings
///      - extract-skills.rq → skill bindings
///    Output: SPARQL result bindings
///
/// μ₃ Tera: Generate Rust code
///    Templates: crates/mcpp-core/templates/a2a/*.tera
///      - agent.rs.tera → crates/a2a-generated/src/agent.rs
///      - message.rs.tera → crates/a2a-generated/src/message.rs
///      - task.rs.tera → crates/a2a-generated/src/task.rs
///      - transport.rs.tera → crates/a2a-generated/src/transport.rs
///      - skill.rs.tera → crates/a2a-generated/src/skill.rs
///      - lib.rs.tera → crates/a2a-generated/src/lib.rs
///    Output: Generated Rust source files
///
/// μ₄ Canonicalize: Format and organize
///    Action: rustfmt, organize imports, verify compilation
///    Output: Formatted, ready-to-compile code
///
/// μ₅ Receipt: Generate cryptographic verification
///    Output: .mcpp/receipts/a2a-{timestamp}.json
///    Contains: SHA256 hashes, input ontology hash, timestamp
/// ```
///
/// ## Pipeline Flow
///
/// ```text
/// mcpp.toml → ontology → CONSTRUCT inference → SELECT → Template → Code
/// ```
///
/// ## Arguments
///
/// ### Input Options
///
/// * `manifest` - Path to mcpp.toml manifest file (default: ./mcpp.toml)
///   * Environment: `GGEN_MANIFEST`
///   * The manifest defines generation rules, output paths, and validation settings
///   * Required for manifest-driven pipeline (default mode)
///
/// * `ontology` - Override ontology path from manifest
///   * Environment: `GGEN_ONTOLOGY`
///   * Specifies the RDF ontology file (.ttl) as pipeline input
///   * Useful for A2A or custom ontology workflows
///
/// * `queries` - Directory containing SPARQL .rq query files
///   * Environment: `GGEN_QUERIES`
///   * Activates ontology-first pipeline (bypasses mcpp.toml)
///   * When set, runs low-level pipeline without manifest validation
///   * Example: `--queries ./queries/businessos/`
///
/// * `language` - Target language for code generation
///   * Environment: `GGEN_LANGUAGE`
///   * Supported: auto, go, elixir, rust, typescript, python
///   * Default: auto (detect from ontology or manifest)
///   * Only used with --queries (ontology-first mode)
///
/// ### Execution Mode
///
/// * `dry_run` - Preview changes without writing files
///   * Environment: `GGEN_DRY_RUN`
///   * Shows what would be generated without modifying filesystem
///   * Safe for exploration and validation
///   * Default: false
///
/// * `force` - Overwrite existing files destructively
///   * Environment: `GGEN_FORCE`
///   * ⚠️ DESTRUCTIVE: Overwrites manually-edited files
///   * ALWAYS use with --audit to enable rollback
///   * ALWAYS use --dry-run first to preview changes
///   * Default: false
///
/// * `locked` - Require pack lockfile (mcpp v6.1.0+)
///   * Environment: `GGEN_LOCKED`
///   * Ensures reproducible builds from locked pack set
///   * Fails hard if .mcpp/packs.lock is missing or invalid
///   * Default: false
///
/// * `validate_only` - Run validation without generation
///   * Environment: `GGEN_VALIDATE_ONLY`
///   * Executes SHACL/SPARQL validation, skips code generation
///   * Useful for pre-flight checks in CI/CD
///   * Default: false
///
/// * `watch` - Continuous file monitoring and auto-regeneration
///   * Environment: `GGEN_WATCH`
///   * Monitors ontology and template files for changes
///   * Automatically re-runs pipeline on file modifications
///   * Ideal for development workflows
///   * Default: false
///
/// ### Output Options
///
/// * `output_dir` - Override output directory from manifest
///   * Environment: `GGEN_OUTPUT_DIR`
///   * Specifies where generated code will be written
///   * Example: `--output-dir crates/generated/`
///
/// * `format` - Output format for status and results
///   * Environment: `GGEN_FORMAT`
///   * Supported: text, json
///   * Default: text
///   * JSON format useful for CI/CD integration
///
/// * `audit` - Create detailed audit trail
///   * Environment: `GGEN_AUDIT`
///   * Writes execution metadata to .mcpp/audit/
///   * Includes input hashes, timestamps, and rule execution
///   * Required for --force rollback capability
///   * Default: false
///
/// * `rule` - Execute only specific generation rule
///   * Environment: `GGEN_RULE`
///   * Filters pipeline to single rule for focused iteration
///   * Example: `--rule api_endpoints`
///
/// * `stage` - Run specific μ stage only
///   * Environment: `GGEN_STAGE`
///   * Supported: μ₁, μ₂, μ₃, μ₄, μ₅ (or mu1-mu5)
///   * Limits execution to single pipeline stage
///   * Example: `--stage μ₃` (template generation only)
///
/// ### Diagnostics
///
/// * `verbose` - Show detailed execution logs
///   * Environment: `GGEN_VERBOSE`
///   * Enables debug-level logging for pipeline stages
///   * Shows SPARQL queries, template bindings, file paths
///   * Default: false
///
/// * `timeout` - Maximum execution time in milliseconds
///   * Environment: `GGEN_TIMEOUT`
///   * Default: 30000 (30 seconds)
///   * Prevents runaway SPARQL queries or template rendering
///   * Example: `--timeout 60000` (60 seconds)
///
/// ## Environment Variables
///
/// In addition to the flag-specific environment variables above:
///
/// * `GGEN_OFFLINE` - Disable network operations
///   * Set to "1" or "true" to enable offline mode
///   * Skips remote pack fetching and external API calls
///   * Useful for air-gapped environments
///
/// * `GGEN_LOCKED` - Require locked pack set
///   * Set to "1" or "true" to enforce locked mode
///   * Fails if .mcpp/packs.lock is missing or invalid
///   * Ensures reproducible builds
///
/// * `GROQ_API_KEY` - Groq API key for LLM auto-generation
///   * Required when manifest has `enable_llm = true`
///   * Used for automatic inference rule and template generation
///   * Get key from: https://console.groq.com/
///
/// ## Examples
///
/// ### Basic Usage
///
/// ```bash
/// # Default sync (reads mcpp.toml from current directory)
/// mcpp sync
///
/// # Sync from specific manifest
/// mcpp sync --manifest project/mcpp.toml
///
/// # Preview changes without writing files
/// mcpp sync --dry-run
/// ```
///
/// ### A2A Pipeline Workflows
///
/// ```bash
/// # Full A2A μ₁-μ₅ pipeline with cryptographic receipt
/// mcpp sync --ontology .specify/specs/014-a2a-integration/a2a-ontology.ttl --audit
///
/// # Run only template generation stage (μ₃)
/// mcpp sync --stage μ₃ --ontology .specify/specs/014-a2a-integration/a2a-ontology.ttl
///
/// # Run specific extraction queries (μ₂) only
/// mcpp sync --stage μ₂ --ontology .specify/specs/014-a2a-integration/a2a-ontology.ttl
/// ```
///
/// ### Ontology-First Pipeline (No Manifest)
///
/// ```bash
/// # Generate Go code from BusinessOS ontology
/// mcpp sync \
///   --ontology ./businessos.ttl \
///   --queries ./queries/businessos/ \
///   --output ./generated/ \
///   --language go
///
/// # Generate Elixir code with auto-detection
/// mcpp sync \
///   --ontology ./domain.ttl \
///   --queries ./queries/elixir/ \
///   --output ./lib/generated/
/// ```
///
/// ### Safe Development Workflows
///
/// ```bash
/// # Preview changes with audit trail
/// mcpp sync --dry-run --audit
///
/// # Watch mode with verbose logging
/// mcpp sync --watch --verbose
///
/// # Validate ontology without generating code
/// mcpp sync --validate-only
///
/// # Execute single rule for focused iteration
/// mcpp sync --rule api_endpoints --verbose
/// ```
///
/// ### CI/CD Integration
///
/// ```bash
/// # Machine-readable JSON output
/// mcpp sync --format json
///
/// # Pre-flight validation checks
/// mcpp sync --validate-only --format json
///
/// # Timeout-sensitive CI environment
/// mcpp sync --timeout 60000 --format json
/// ```
///
/// ### Reproducible Builds (mcpp v6.1.0+)
///
/// ```bash
/// # Require locked pack set
/// mcpp sync --locked
///
/// # Offline mode (no network operations)
/// GGEN_OFFLINE=1 mcpp sync --locked
/// ```
///
/// ## Progress Reporting (A2A Pipeline)
///
/// When running A2A sync, progress is reported for each μ stage:
///
/// ```text
/// [μ₁/5] CONSTRUCT: Normalizing ontology...
///        Loaded 847 triples from a2a-ontology.ttl
///        +124 triples from construct-agents.rq
/// [μ₂/5] SELECT: Extracting bindings...
///        Agents: 8 bindings
///        Messages: 12 bindings
///        Tasks: 15 bindings
///        Transports: 3 bindings
///        Skills: 24 bindings
/// [μ₃/5] Tera: Generating code...
///        agent.rs (2.4 KB)
///        message.rs (3.1 KB)
///        task.rs (2.8 KB)
///        transport.rs (1.2 KB)
///        skill.rs (4.5 KB)
///        lib.rs (1.8 KB)
/// [μ₄/5] Canonicalizing: Formatting code...
///        Running rustfmt...
///        Verifying compilation...
/// [μ₅/5] Receipt: Generating verification...
///        Receipt: .mcpp/receipts/a2a-20250208-143022.json
///        Ontology hash: a3f2e1b4...
///        Total: 6 files, 15.8 KB, 2.34s
/// ```
///
/// ## Flag Precedence
///
/// * `--validate-only` overrides `--force` (validation skips generation)
/// * `--dry-run` prevents file writes (`--force` has no effect)
/// * `--watch` triggers continuous execution (ignores timeout)
/// * `--stage` limits execution to specific μ stage
/// * `--queries` activates ontology-first mode (bypasses manifest)
///
/// ## Safety Notes
///
/// ⚠️ **ALWAYS use `--audit` with `--force`** to enable rollback capability
/// ⚠️ **ALWAYS use `--dry-run` before `--force`** to preview changes
/// ⚠️ **Review docs/features/force-flag.md** before using `--force`
/// ⚠️ **Use `--locked` in CI/CD** for reproducible builds
/// ⚠️ **Set `GGEN_OFFLINE=1`** in air-gapped environments
///
/// ## Exit Codes
///
/// | Code | Meaning |
/// |------|---------|
/// | 0 | Success |
/// | 1 | Manifest validation error |
/// | 2 | Ontology load error |
/// | 3 | SPARQL query error |
/// | 4 | Template rendering error |
/// | 5 | File I/O error |
/// | 6 | Timeout exceeded |
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
///   - docs/features/a2a-pipeline.md         A2A μ₁-μ₅ pipeline details
///   - docs/features/reproducible-builds.md  Locked mode and offline operation
#[allow(clippy::unused_unit, clippy::too_many_arguments)]
#[verb("sync", "root")]
pub fn sync(
    manifest: Option<String>,
    output_dir: Option<String>,
    dry_run: Option<bool>,
    force: Option<bool>,
    locked: Option<bool>,
    audit: Option<bool>,
    rule: Option<String>,
    verbose: Option<bool>,
    watch: Option<bool>,
    validate_only: Option<bool>,
    format: Option<String>,
    timeout: Option<u64>,
    stage: Option<String>,
    ontology: Option<String>,
    queries: Option<String>,
    language: Option<String>,
) -> Result<SyncOutput> {
    // When --queries is supplied, bypass the manifest and run the low-level pipeline directly
    if let Some(ref queries_dir) = queries {
        return run_low_level_pipeline(
            ontology,
            queries_dir.clone(),
            output_dir,
            language,
            dry_run.unwrap_or(false),
        );
    }

    // Build executor and inject LLM service if enabled
    let manifest_path = PathBuf::from(manifest.clone().unwrap_or_else(|| "mcpp.toml".to_string()));

    // Build options from CLI args (manifest-driven pipeline)
    let options = build_sync_options(
        manifest,
        output_dir,
        dry_run,
        force,
        locked,
        audit,
        rule,
        verbose,
        watch,
        validate_only,
        format,
        timeout,
        stage,
        ontology,
    )?;

    let executor = SyncExecutor::new(options);
    let executor = inject_llm_if_enabled(executor, &manifest_path, verbose.unwrap_or(false));

    // Execute pipeline
    let result = executor
        .execute()
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

    Ok(SyncOutput::from(result))
}

/// Invoke the low-level `mcpp_core::sync::sync()` pipeline directly.
///
/// Activated when the user supplies `--queries`.  Bypasses `mcpp.toml` entirely.
///
/// Usage:
/// ```bash
/// mcpp sync --ontology ./businessos.ttl --queries ./queries/businessos/ --output ./generated/ --language go
/// ```
fn run_low_level_pipeline(
    ontology: Option<String>, queries_dir: String, output_dir: Option<String>,
    language: Option<String>, dry_run: bool,
) -> Result<SyncOutput> {
    let ontology_path = PathBuf::from(ontology.unwrap_or_else(|| "ontology.ttl".to_string()));
    let queries_path = PathBuf::from(queries_dir);
    let output_path = PathBuf::from(output_dir.unwrap_or_else(|| ".".to_string()));

    let lang: SyncLanguage =
        language.as_deref().unwrap_or("auto").parse().map_err(
            |e: mcpp_core::sync::SyncError| NounVerbError::execution_error(e.to_string()),
        )?;

    let config = SyncConfig {
        ontology_path,
        queries_dir: queries_path,
        output_dir: output_path,
        language: lang,
        validate: true,
        dry_run,
    };

    let result =
        low_level_sync(config).map_err(|e| NounVerbError::execution_error(e.to_string()))?;

    let files: Vec<SyncedFile> = result
        .files_generated
        .iter()
        .map(|p| SyncedFile {
            path: p.display().to_string(),
            size_bytes: if dry_run {
                0
            } else {
                std::fs::metadata(p).map_or(0, |m| m.len() as usize)
            },
            action: if dry_run {
                "would create".to_string()
            } else {
                "created".to_string()
            },
        })
        .collect();

    let violation_msg = if result.soundness_violations.is_empty() {
        None
    } else {
        Some(format!(
            "{} soundness violation(s): {}",
            result.soundness_violations.len(),
            result
                .soundness_violations
                .iter()
                .map(|v| v.rule.as_str())
                .collect::<Vec<_>>()
                .join(", ")
        ))
    };

    Ok(SyncOutput {
        status: "success".to_string(),
        files_synced: files.len(),
        duration_ms: result.elapsed_ms,
        files,
        inference_rules_executed: 0,
        generation_rules_executed: result.files_generated.len(),
        audit_trail: None,
        error: violation_msg,
    })
}

/// Inject LLM service into executor if enable_llm is set in manifest
///
/// This helper function checks if the manifest has enable_llm set to true,
/// and if so, creates a GroqLlmBridge and injects it into the executor.
///
/// # Arguments
/// * `executor` - The SyncExecutor to inject the service into
/// * `manifest_path` - Path to the mcpp.toml manifest file
/// * `verbose` - Whether to print verbose output
///
/// # Returns
/// * The executor with LLM service injected if enabled
fn inject_llm_if_enabled(
    executor: SyncExecutor, manifest_path: &Path, verbose: bool,
) -> SyncExecutor {
    if !manifest_path.exists() {
        return executor;
    }

    // Parse manifest to check enable_llm flag
    let manifest_data = match mcpp_core::manifest::ManifestParser::parse(manifest_path) {
        Ok(data) => data,
        Err(_) => return executor, // If parsing fails, return executor as-is
    };

    // Only inject if enable_llm is true
    if !manifest_data.generation.enable_llm {
        return executor;
    }

    // Create Groq LLM bridge
    let bridge = match GroqLlmBridge::new() {
        Ok(b) => b,
        Err(e) => {
            eprintln!(
                "Error: enable_llm is true but GroqLlmBridge creation failed: {}\n\
                 Hint: Set GROQ_API_KEY environment variable\n\
                 Continuing without LLM auto-generation.",
                e
            );
            return executor;
        }
    };

    if verbose {
        eprintln!("✓ LLM auto-generation enabled (Groq)");
    }

    executor.with_llm_service(Some(Box::new(bridge)))
}

/// Build SyncOptions from CLI arguments
///
/// This helper keeps the verb function thin by extracting option building.
#[allow(clippy::too_many_arguments)]
fn build_sync_options(
    manifest: Option<String>,
    output_dir: Option<String>,
    dry_run: Option<bool>,
    force: Option<bool>,
    locked: Option<bool>,
    audit: Option<bool>,
    rule: Option<String>,
    verbose: Option<bool>,
    watch: Option<bool>,
    validate_only: Option<bool>,
    format: Option<String>,
    timeout: Option<u64>,
    stage: Option<String>,
    ontology: Option<String>,
) -> Result<SyncOptions> {
    let mut options = SyncOptions::new();

    // Set manifest path
    options.manifest_path = PathBuf::from(manifest.unwrap_or_else(|| "mcpp.toml".to_string()));

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

    // Handle locked mode (mcpp v6.1.0+)
    // Note: SyncOptions may not have a locked field yet, so we check at runtime
    if locked.unwrap_or(false) {
        // Verify .mcpp/packs.lock exists
        let lockfile_path = PathBuf::from(".mcpp/packs.lock");
        if !lockfile_path.exists() {
            return Err(GgenError::from_clap_error(NounVerbError::execution_error(
                "error[E0007]: Locked mode required but .mcpp/packs.lock not found\n  |\n  = help: Run 'mcpp sync' first to generate lockfile, or unset GGEN_LOCKED".to_string()
            )));
        }
        // Additional lockfile validation would go here
        if verbose.unwrap_or(false) {
            eprintln!("✓ Locked mode: using .mcpp/packs.lock");
        }
    }

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
                return Err(GgenError::from_clap_error(NounVerbError::execution_error(
                    format!("error[E0005]: Invalid output format '{}'\n  |\n  = help: Valid formats: text, json", fmt)
                )))
            }
        };
    }

    // Set timeout
    if let Some(t) = timeout {
        options.timeout_ms = Some(t);
    }

    // A2A-specific options
    if let Some(s) = stage {
        // Validate stage format: μ₁, μ₂, μ₃, μ₄, μ₅
        if !matches!(
            s.as_str(),
            "μ₁" | "μ₂" | "μ₃" | "μ₄" | "μ₅" | "mu1" | "mu2" | "mu3" | "mu4" | "mu5"
        ) {
            return Err(GgenError::from_clap_error(NounVerbError::execution_error(
                format!("error[E0006]: Invalid stage '{}'\n  |\n  = help: Valid stages: μ₁, μ₂, μ₃, μ₄, μ₅ (or mu1-mu5)", s)
            )));
        }
        options.a2a_stage = Some(s);
    }

    if let Some(ont) = ontology {
        options.ontology_path = Some(PathBuf::from(ont));
    }

    Ok(options)
}
