//! Sync Command - The ONLY command in ggen v5
//!
//! `ggen sync` is the unified code synchronization pipeline that replaces ALL
//! previous ggen commands. It transforms domain ontologies through inference
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
//!   ggen sync --audit              # Full A2A pipeline with receipt
//!   ggen sync --dry-run            # Preview without writing
//!   ggen sync --output crates/     # Custom output directory

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

use chrono::Utc;
use clap_noun_verb::{NounVerbError, Result as VerbResult};
use clap_noun_verb_macros::verb;
use ggen_core::codegen::{OutputFormat, SyncExecutor, SyncOptions, SyncResult};
use ggen_core::sync::{sync as low_level_sync, SyncConfig, SyncLanguage};
use ggen_receipt::{generate_keypair, hash_data, Receipt};
use serde::Serialize;
use std::path::{Path, PathBuf};

// Import llm_bridge module from the same crate
#[allow(unused_imports)]
use crate::llm_bridge::GroqLlmBridge;

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

    /// Path to the cryptographic receipt emitted after sync (if generated)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub receipt_path: Option<String>,
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
            receipt_path: None, // populated separately by emit_sync_receipt
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
/// ## A2A-RS μ Pipeline (μ₁ through μ₅)
///
/// When generating A2A-RS code from `.specify/specs/014-a2a-integration/`:
///
/// ```text
/// μ₁ CONSTRUCT: Normalize RDF ontology
///    Input: .specify/specs/014-a2a-integration/a2a-ontology.ttl
///    Query: crates/ggen-core/queries/a2a/construct-agents.rq
///    Output: Normalized A2A RDF with a2a: prefix
///
/// μ₂ SELECT: Extract bindings for each module
///    Queries: crates/ggen-core/queries/a2a/extract-*.rq
///      - extract-agents.rq → agent bindings
///      - extract-messages.rq → message bindings
///      - extract-tasks.rq → task bindings
///      - extract-transports.rq → transport bindings
///      - extract-skills.rq → skill bindings
///    Output: SPARQL result bindings
///
/// μ₃ Tera: Generate Rust code
///    Templates: crates/ggen-core/templates/a2a/*.tera
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
///    Output: .ggen/receipts/a2a-{timestamp}.json
///    Contains: SHA256 hashes, input ontology hash, timestamp
/// ```
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
/// --stage STAGE           Run specific μ stage only (μ₁, μ₂, μ₃, μ₄, μ₅)
/// --ontology PATH         Override ontology path (default: from manifest)
///
/// ## Flag Combinations
///
/// Safe workflows:
///   ggen sync --dry-run --audit         Preview with audit
///   ggen sync --force --audit           Destructive overwrite with tracking
///   ggen sync --watch --validate-only   Continuous validation
///
/// A2A-specific workflows:
///   ggen sync --audit                   Full A2A μ₁-μ₅ pipeline with receipt
///   ggen sync --stage μ₃                Only run template generation
///   ggen sync --ontology .specify/specs/014-a2a-integration/a2a-ontology.ttl
///
/// CI/CD workflows:
///   ggen sync --format json             Machine-readable output
///   ggen sync --validate-only           Pre-flight checks
///
/// Development workflows:
///   ggen sync --watch --verbose         Live feedback
///   ggen sync --rule structs            Focused iteration
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
///        Receipt: .ggen/receipts/a2a-20250208-143022.json
///        Ontology hash: a3f2e1b4...
///        Total: 6 files, 15.8 KB, 2.34s
/// ```
///
/// ## Flag Precedence
///
/// --validate-only overrides --force
/// --dry-run prevents file writes (--force has no effect)
/// --watch triggers continuous execution
/// --stage limits execution to specific μ stage
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
/// # A2A generation with custom ontology
/// ggen sync --ontology .specify/specs/014-a2a-integration/a2a-ontology.ttl --audit
///
/// # Run specific μ stage only
/// ggen sync --stage μ₃
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
///   - docs/features/a2a-pipeline.md         A2A μ₁-μ₅ pipeline details
#[allow(clippy::unused_unit, clippy::too_many_arguments)]
#[verb("sync", "root")]
pub fn sync(
    manifest: Option<String>,
    output_dir: Option<String>,
    dry_run: Option<bool>,
    force: Option<bool>,
    audit: Option<bool>,
    rule: Option<String>,
    verbose: Option<bool>,
    watch: Option<bool>,
    validate_only: Option<bool>,
    format: Option<String>,
    timeout: Option<u64>,
    stage: Option<String>,
    ontology: Option<String>,
    queries: Option<String>, // dir of .rq files — activates ontology-first pipeline (no ggen.toml needed)
    language: Option<String>, // go, elixir, rust, typescript, python, auto
    profile: Option<String>, // enforcement profile (e.g. enterprise-strict, permissive)
    locked: bool,            // require exact lock-file match (no implicit upgrades)
) -> VerbResult<SyncOutput> {
    check_profile_preconditions(profile.as_deref(), locked)?;

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

    run_manifest_pipeline(
        manifest, output_dir, dry_run, force, audit, rule, verbose, watch, validate_only, format,
        timeout, stage, ontology,
    )
}

/// Check profile and locked preconditions before any pipeline work.
fn check_profile_preconditions(
    profile: Option<&str>, locked: bool,
) -> VerbResult<()> {
    if profile.is_some() || locked {
        let workspace =
            std::env::current_dir().map_err(|e| NounVerbError::execution_error(e.to_string()))?;
        ggen_domain::sync_profile::validate_sync_preconditions(profile, locked, &workspace)
            .map_err(NounVerbError::execution_error)?;
    }
    Ok(())
}

/// Execute the manifest-driven (ggen.toml) sync pipeline and emit a signed receipt.
#[allow(clippy::too_many_arguments)]
fn run_manifest_pipeline(
    manifest: Option<String>, output_dir: Option<String>, dry_run: Option<bool>,
    force: Option<bool>, audit: Option<bool>, rule: Option<String>, verbose: Option<bool>,
    watch: Option<bool>, validate_only: Option<bool>, format: Option<String>, timeout: Option<u64>,
    stage: Option<String>, ontology: Option<String>,
) -> VerbResult<SyncOutput> {
    let installed_packs = read_installed_packs(".ggen/packs.lock");

    let manifest_path = PathBuf::from(manifest.clone().unwrap_or_else(|| "ggen.toml".to_string()));
    let options = build_sync_options(
        manifest, output_dir, dry_run, force, audit, rule, verbose, watch, validate_only, format,
        timeout, stage, ontology,
    )?;

    let executor = SyncExecutor::new(options);
    let executor = inject_llm_if_enabled(executor, &manifest_path, verbose.unwrap_or(false));

    let result = executor
        .execute()
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

    // Emit a signed sync receipt (best-effort — never fails the sync).
    // This makes `ggen receipt verify --receipt-file .ggen/receipts/latest.json` work.
    let receipt_path = emit_sync_receipt_best_effort(&result, &installed_packs);

    let mut output = SyncOutput::from(result);
    output.receipt_path = receipt_path;
    Ok(output)
}

/// Attempt to write a sync receipt; log a warning on failure but never abort the sync.
fn emit_sync_receipt_best_effort(result: &SyncResult, installed_packs: &[String]) -> Option<String> {
    match emit_sync_receipt(result, installed_packs) {
        Ok(path) => Some(path),
        Err(e) => {
            eprintln!("Warning: could not write sync receipt: {}", e);
            None
        }
    }
}

/// Read the installed packs from a packs.lock JSON file.
///
/// Returns a list of `"<id>@<version>"` strings, one per installed pack.
/// Returns an empty vec if the file is absent or cannot be parsed.
fn read_installed_packs(lock_path: &str) -> Vec<String> {
    let path = std::path::Path::new(lock_path);
    if !path.exists() {
        return vec![];
    }
    let content = std::fs::read_to_string(path).unwrap_or_default();
    let Ok(val) = serde_json::from_str::<serde_json::Value>(&content) else {
        return vec![];
    };
    val.get("packs")
        .and_then(|p| p.as_object())
        .map(|obj| {
            obj.iter()
                .map(|(id, entry)| {
                    let version = entry
                        .get("version")
                        .and_then(|v| v.as_str())
                        .unwrap_or("unknown");
                    format!("{}@{}", id, version)
                })
                .collect()
        })
        .unwrap_or_default()
}

/// Emit a cryptographically signed sync receipt to `.ggen/receipts/`.
///
/// Writes two files on success:
///   `.ggen/receipts/sync-<YYYYMMDD-HHMMSS>.json` — timestamped archive copy
///   `.ggen/receipts/latest.json`                  — always points at the latest receipt
///
/// The receipt captures:
///   - SHA-256 hash of `ggen.toml` as an input hash
///   - One entry per installed pack (`pack:<id>@<version>`) as input hashes
///   - SHA-256 hash of each generated file as output hashes
///   - Ed25519 signature over the entire payload
///
/// Signing key is persisted at `.ggen/keys/signing.key` (hex).
/// A new keypair is generated only when no key file exists; existing keys are
/// never overwritten.
///
/// Returns the path written to `latest.json` on success, or a string error on
/// failure.  The caller logs the error as a warning but does NOT abort the sync.
fn emit_sync_receipt(result: &SyncResult, installed_packs: &[String]) -> std::result::Result<String, String> {
    use std::fs;

    // 1. Ensure .ggen/keys/ directory exists.
    let keys_dir = std::path::Path::new(".ggen/keys");
    fs::create_dir_all(keys_dir).map_err(|e| e.to_string())?;

    // 2. Load or generate signing keypair — never overwrite an existing key.
    let signing_key_path = keys_dir.join("signing.key");
    let verifying_key_path = keys_dir.join("verifying.key");

    let signing_key = if signing_key_path.exists() {
        let hex_str = fs::read_to_string(&signing_key_path).map_err(|e| e.to_string())?;
        let bytes = hex::decode(hex_str.trim()).map_err(|e| e.to_string())?;
        let sk_bytes: [u8; 32] = bytes
            .try_into()
            .map_err(|_| "Invalid signing key length (expected 32 bytes)".to_string())?;
        ed25519_dalek::SigningKey::from_bytes(&sk_bytes)
    } else {
        let (sk, vk) = generate_keypair();
        fs::write(&signing_key_path, hex::encode(sk.to_bytes()))
            .map_err(|e| e.to_string())?;
        fs::write(&verifying_key_path, hex::encode(vk.to_bytes()))
            .map_err(|e| e.to_string())?;
        sk
    };

    // 3. Build input hashes: ggen.toml + installed packs.
    let mut input_hashes: Vec<String> = Vec::new();
    if let Ok(manifest_content) = std::fs::read_to_string("ggen.toml") {
        input_hashes.push(format!("ggen.toml:{}", hash_data(manifest_content.as_bytes())));
    }
    for pack in installed_packs {
        input_hashes.push(format!("pack:{}", pack));
    }

    // 4. Build output hashes from generated file paths stored in SyncResult.
    //    SyncedFileInfo.path is a String (relative or absolute path).
    let output_hashes: Vec<String> = result
        .files
        .iter()
        .filter_map(|f| {
            fs::read(&f.path)
                .ok()
                .map(|content| format!("{}:{}", f.path, hash_data(&content)))
        })
        .collect();

    // 5. Create and sign the receipt.
    let receipt = Receipt::new(
        "ggen-sync".to_string(),
        input_hashes,
        output_hashes,
        None,
    )
    .sign(&signing_key)
    .map_err(|e| e.to_string())?;

    // 6. Write timestamped archive copy.
    let receipts_dir = std::path::Path::new(".ggen/receipts");
    fs::create_dir_all(receipts_dir).map_err(|e| e.to_string())?;
    let timestamp = Utc::now().format("%Y%m%d-%H%M%S");
    let receipt_json = serde_json::to_string_pretty(&receipt).map_err(|e| e.to_string())?;
    let timestamped_path = receipts_dir.join(format!("sync-{}.json", timestamp));
    fs::write(&timestamped_path, &receipt_json).map_err(|e| e.to_string())?;

    // 7. Overwrite latest.json so the golden-path verify command always works.
    let latest_path = receipts_dir.join("latest.json");
    fs::write(&latest_path, &receipt_json).map_err(|e| e.to_string())?;

    Ok(latest_path.to_string_lossy().into_owned())
}

/// Invoke the low-level `ggen_core::sync::sync()` pipeline directly.
///
/// Activated when the user supplies `--queries`.  Bypasses `ggen.toml` entirely.
///
/// Usage:
/// ```bash
/// ggen sync --ontology ./businessos.ttl --queries ./queries/businessos/ --output ./generated/ --language go
/// ```
fn run_low_level_pipeline(
    ontology: Option<String>, queries_dir: String, output_dir: Option<String>,
    language: Option<String>, dry_run: bool,
) -> VerbResult<SyncOutput> {
    let ontology_path = PathBuf::from(ontology.unwrap_or_else(|| "ontology.ttl".to_string()));
    let queries_path = PathBuf::from(queries_dir);
    let output_path = PathBuf::from(output_dir.unwrap_or_else(|| ".".to_string()));

    let lang: SyncLanguage =
        language.as_deref().unwrap_or("auto").parse().map_err(
            |e: ggen_core::sync::SyncError| NounVerbError::execution_error(e.to_string()),
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
        receipt_path: None,
    })
}

/// Inject LLM service into executor if enable_llm is set in manifest
///
/// This helper function checks if the manifest has enable_llm set to true,
/// and if so, creates a GroqLlmBridge and injects it into the executor.
///
/// # Arguments
/// * `executor` - The SyncExecutor to inject the service into
/// * `manifest_path` - Path to the ggen.toml manifest file
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
    let manifest_data = match ggen_core::manifest::ManifestParser::parse(manifest_path) {
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
    manifest: Option<String>, output_dir: Option<String>, dry_run: Option<bool>,
    force: Option<bool>, audit: Option<bool>, rule: Option<String>, verbose: Option<bool>,
    watch: Option<bool>, validate_only: Option<bool>, format: Option<String>, timeout: Option<u64>,
    stage: Option<String>, ontology: Option<String>,
) -> Result<SyncOptions, NounVerbError> {
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
        options.output_format = match fmt.to_lowercase().as_str() {
            "text" => OutputFormat::Text,
            "json" => OutputFormat::Json,
            _ => {
                return Err(NounVerbError::execution_error(format!(
                    "error[E0005]: Invalid output format '{}'\n  |\n  = help: Valid formats: text, json",
                    fmt
                )))
            }
        };
    }

    // Set timeout (TODO: Add timeout_ms field to SyncOptions if needed)
    // if let Some(t) = timeout {
    //     options.timeout_ms = t;
    // }
    let _ = timeout; // Suppress unused variable warning

    // A2A-specific options
    if let Some(s) = stage {
        // Validate stage format: μ₁, μ₂, μ₃, μ₄, μ₅
        if !matches!(
            s.as_str(),
            "μ₁" | "μ₂" | "μ₃" | "μ₄" | "μ₅" | "mu1" | "mu2" | "mu3" | "mu4" | "mu5"
        ) {
            return Err(NounVerbError::execution_error(format!(
                "error[E0006]: Invalid stage '{}'\n  |\n  = help: Valid stages: μ₁, μ₂, μ₃, μ₄, μ₅ (or mu1-mu5)",
                s
            )));
        }
        options.a2a_stage = Some(s);
    }

    if let Some(ont) = ontology {
        options.ontology_path = Some(PathBuf::from(ont));
    }

    Ok(options)
}
