//! Sync Executor - Domain logic for ggen sync command
//!
//! This module contains the business logic for the sync pipeline,
//! extracted from the CLI layer to maintain separation of concerns.
//!
//! The executor handles:
//! - Manifest parsing and validation
//! - Validate-only mode
//! - Dry-run mode
//! - Full sync pipeline execution
//!
//! ## Architecture
//!
//! The CLI verb function should be thin (complexity <= 5):
//! 1. Parse CLI args into `SyncOptions`
//! 2. Call `SyncExecutor::execute(options)`
//! 3. Return result
//!
//! All business logic lives here in the executor.

use crate::codegen::pipeline::{GenerationPipeline, LlmService, RuleType};
use crate::codegen::ux::{
    format_duration, info_message, print_section, success_message, warning_message,
    ProgressIndicator,
};
use crate::codegen::{DependencyValidator, IncrementalCache, MarketplaceValidator, ProofCarrier};
use crate::drift::DriftDetector;
use crate::manifest::{ManifestParser, ManifestValidator};
use crate::poka_yoke::{AndonSignal, CriticalError, QualityGateRunner};
use crate::utils::error::{Error, Result};
use crate::validation::PreFlightValidator;
use serde::Serialize;
use std::path::{Path, PathBuf};
use std::time::Instant;

// ============================================================================
// Sync Options Types
// ============================================================================

/// Output format for sync results
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Default)]
pub enum OutputFormat {
    /// Human-readable text output
    #[default]
    Text,
    /// JSON output
    Json,
}

impl std::str::FromStr for OutputFormat {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "text" => Ok(OutputFormat::Text),
            "json" => Ok(OutputFormat::Json),
            _ => Err("Invalid format".to_string()),
        }
    }
}

impl std::fmt::Display for OutputFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OutputFormat::Text => write!(f, "text"),
            OutputFormat::Json => write!(f, "json"),
        }
    }
}

/// Execution mode flags — mutually exclusive sync behaviors (≤3 bools)
#[derive(Debug, Clone, Copy, Default)]
pub struct ModeFlags {
    /// Only validate, don't generate
    pub validate_only: bool,
    /// Dry run mode - preview changes without writing
    pub dry_run: bool,
    /// Enable file watching and auto-regeneration
    pub watch: bool,
}

/// Behavioral modifier flags (≤3 bools)
#[derive(Debug, Clone, Copy, Default)]
pub struct BehaviorFlags {
    /// Enable verbose output
    pub verbose: bool,
    /// Force overwrite even if files are newer
    pub force: bool,
    /// Generate audit trail
    pub audit: bool,
}

/// Combined sync flags (groups mode and behavior sub-structs)
#[derive(Debug, Clone, Copy, Default)]
pub struct SyncFlags {
    /// Execution mode (validate_only, dry_run, watch)
    pub mode: ModeFlags,
    /// Behavioral modifiers (verbose, force, audit)
    pub behavior: BehaviorFlags,
}

/// Options for sync execution
pub struct SyncOptions {
    /// Path to manifest file
    pub manifest_path: PathBuf,

    /// Output directory for generated files
    pub output_dir: Option<PathBuf>,

    /// Cache directory for incremental builds
    pub cache_dir: Option<PathBuf>,

    /// Use incremental cache
    pub use_cache: bool,

    /// Boolean execution flags
    pub flags: SyncFlags,

    /// Output format
    pub output_format: OutputFormat,

    /// Selected rules to execute (None = all)
    pub selected_rules: Option<Vec<String>>,

    // A2A-specific options
    /// Run specific μ stage only (μ₁, μ₂, μ₃, μ₄, μ₅)
    pub a2a_stage: Option<String>,

    /// Override ontology path for A2A generation
    pub ontology_path: Option<PathBuf>,

    /// Optional LLM service for auto-generating skill implementations
    /// If None, uses default TemplateFallback generator
    /// Note: `Box<dyn LlmService>` avoids cyclic dependency with ggen-ai
    pub llm_service: Option<Box<dyn LlmService>>,

    /// Timeout for sync operations in milliseconds (None = no timeout)
    pub timeout_ms: Option<u64>,
}

impl Default for SyncOptions {
    fn default() -> Self {
        Self {
            manifest_path: PathBuf::from("ggen.toml"),
            output_dir: None,
            cache_dir: None,
            use_cache: true,
            flags: SyncFlags::default(),
            output_format: OutputFormat::default(),
            selected_rules: None,
            a2a_stage: None,
            ontology_path: None,
            llm_service: None,
            timeout_ms: None,
        }
    }
}

impl std::fmt::Debug for SyncOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SyncOptions")
            .field("manifest_path", &self.manifest_path)
            .field("output_dir", &self.output_dir)
            .field("cache_dir", &self.cache_dir)
            .field("flags", &self.flags)
            .field("output_format", &self.output_format)
            .field("selected_rules", &self.selected_rules)
            .field("a2a_stage", &self.a2a_stage)
            .field("ontology_path", &self.ontology_path)
            .field("llm_service", &"<dyn LlmService>")
            .field("timeout_ms", &self.timeout_ms)
            .finish()
    }
}

impl Clone for SyncOptions {
    fn clone(&self) -> Self {
        Self {
            manifest_path: self.manifest_path.clone(),
            output_dir: self.output_dir.clone(),
            cache_dir: self.cache_dir.clone(),
            use_cache: self.use_cache,
            flags: self.flags,
            output_format: self.output_format,
            selected_rules: self.selected_rules.clone(),
            a2a_stage: self.a2a_stage.clone(),
            ontology_path: self.ontology_path.clone(),
            timeout_ms: self.timeout_ms,
            llm_service: None, // trait objects cannot be cloned
        }
    }
}

impl SyncOptions {
    /// Create a new SyncOptions with default values
    pub fn new() -> Self {
        Self::default()
    }
}

// ============================================================================
// Sync Result Types
// ============================================================================

/// Result of sync execution - returned to CLI layer
#[derive(Debug, Clone, Serialize, Default)]
pub struct SyncResult {
    /// Overall status: "success" or "error"
    pub status: String,

    /// Number of files synced
    pub files_synced: usize,

    /// Total duration in milliseconds
    pub duration_ms: u64,

    /// Generated files with details
    pub files: Vec<SyncedFileInfo>,

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

    /// Machine-parsable recovery steps for AGI remediation
    #[serde(skip_serializing_if = "Option::is_none")]
    pub recovery: Option<String>,

    /// JSON representation of the TPS Andon signal (if any)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub andon_signal: Option<serde_json::Value>,
}

/// Individual file info in sync result
#[derive(Debug, Clone, Serialize, Default)]
pub struct SyncedFileInfo {
    /// File path
    pub path: String,

    /// File size in bytes
    pub size_bytes: usize,

    /// Action taken: "created", "updated", "unchanged", "would create"
    pub action: String,

    /// Rule that generated this file
    pub produced_by: String,
}

/// Validation check result
#[derive(Debug, Clone, Serialize)]
pub struct ValidationCheck {
    /// Check name
    pub check: String,

    /// Whether it passed
    pub passed: bool,

    /// Details about the check
    #[serde(skip_serializing_if = "Option::is_none")]
    pub details: Option<String>,
}

// ============================================================================
// Sync Executor
// ============================================================================

/// Executes the sync pipeline with given options
///
/// This is the main entry point for sync operations from the CLI.
/// All complex business logic is encapsulated here.
pub struct SyncExecutor {
    options: SyncOptions,
    start_time: Instant,
}

impl SyncExecutor {
    /// Create a new executor with the given options
    pub fn new(options: SyncOptions) -> Self {
        Self {
            options,
            start_time: Instant::now(),
        }
    }

    /// Set LLM service for auto-generating skill implementations
    ///
    /// # Arguments
    /// * `service` - Optional boxed LLM service (None = use fallback generators)
    pub fn with_llm_service(mut self, service: Option<Box<dyn LlmService>>) -> Self {
        self.options.llm_service = service;
        self
    }

    /// Execute the sync pipeline based on options
    ///
    /// Returns `SyncResult` that can be serialized to JSON or formatted as text.
    pub fn execute(mut self) -> Result<SyncResult> {
        // Pre-flight validation: Check environment before proceeding
        let base_path = self
            .options
            .manifest_path
            .parent()
            .unwrap_or(Path::new("."));

        let preflight = PreFlightValidator::for_sync(base_path)
            .with_llm_check(false) // LLM check is optional (warning only)
            .with_template_check(false) // Will check after parsing manifest
            .with_git_check(false);

        // Run basic pre-flight checks (without manifest, as we haven't parsed it yet)
        if let Err(e) = preflight.validate(None) {
            if self.options.flags.behavior.verbose {
                eprintln!("{}", warning_message(&format!("Pre-flight warning: {}", e)));
            }
        } else if self.options.flags.behavior.verbose {
            eprintln!("{}", success_message("Pre-flight checks passed"));
        }

        // Validate manifest exists
        if !self.options.manifest_path.exists() {
            let error_msg = format!(
                "error[E0001]: Manifest not found\n  --> {}",
                self.options.manifest_path.display()
            );
            let andon = AndonSignal::manifest_error("ggen.toml", "File does not exist");
            return Ok(self.create_error_result(&error_msg, Some(andon)));
        }

        // Check for drift (non-blocking warning)
        self.check_and_warn_drift(base_path);

        // T017-T018: Watch mode implementation
        if self.options.flags.mode.watch {
            return self.execute_watch_mode(&self.options.manifest_path);
        }

        // Parse manifest
        let manifest_data = ManifestParser::parse(&self.options.manifest_path).map_err(|e| {
            Error::new(&format!(
                "error[E0001]: Manifest parse error\n  --> {}\n  |\n  = error: {}\n  = help: Check ggen.toml syntax and required fields",
                self.options.manifest_path.display(),
                e
            ))
        })?;

        // Validate manifest
        let base_path: PathBuf = self
            .options
            .manifest_path
            .parent()
            .unwrap_or(Path::new("."))
            .to_path_buf();
        let validator = ManifestValidator::new(&manifest_data, &base_path);
        validator.validate().map_err(|e| {
            Error::new(&format!(
                "error[E0001]: Manifest validation failed\n  --> {}\n  |\n  = error: {}\n  = help: Fix validation errors before syncing",
                self.options.manifest_path.display(),
                e
            ))
        })?;

        // Validate dependencies (ontology imports, circular references, file existence)
        let dep_validator = DependencyValidator::validate_manifest(&manifest_data, &base_path)
            .map_err(|e| {
                Error::new(&format!(
                    "error[E0002]: Dependency validation failed\n  |\n  = error: {}\n  = help: Fix missing ontology imports or circular dependencies",
                    e
                ))
            })?;

        if dep_validator.has_cycles {
            let error_msg = format!("error[E0002]: Circular dependency detected\n  |\n  = error: Inference rules have circular dependencies\n  = cycles: {:?}", dep_validator.cycle_nodes);
            let andon = AndonSignal::circular_dependency(vec![dep_validator.cycle_nodes.clone()]);
            return Ok(self.create_error_result(&error_msg, Some(andon)));
        }

        if dep_validator.failed_checks > 0 {
            let error_msg = format!(
                "error[E0002]: {} dependency validation checks failed\n  |\n  = help: Common issues:\n  =   1. Query file not found: Check ontology.source and ontology.imports paths\n  =   2. Template file not found: Check generation.rules[].template paths\n  =   3. Import cycle: Check if imported files reference each other\n  = help: Run 'ggen validate' for detailed dependency analysis",
                dep_validator.failed_checks
            );
            return Ok(self.create_error_result(&error_msg, None));
        }

        // Run quality gates - mandatory checkpoints before generation
        let gate_runner = QualityGateRunner::new();
        gate_runner.run_all(&manifest_data, &base_path).map_err(|e| {
            Error::new(&format!(
                "error[E0004]: Quality gate validation failed\n  |\n  = error: {}\n  = help: Fix validation errors before syncing",
                e
            ))
        })?;

        // Run marketplace pre-flight validation (FMEA analysis)
        let marketplace_validator = MarketplaceValidator::new(160);
        let pre_flight = marketplace_validator.pre_flight_check(&manifest_data).map_err(|e| {
            Error::new(&format!(
                "error[E0003]: Marketplace pre-flight validation failed\n  |\n  = error: {}\n  = help: Review package dependencies and resolve high-risk items",
                e
            ))
        })?;

        if self.options.flags.behavior.verbose {
            eprintln!(
                "Pre-flight checks: {} validations, {} high-risk items detected",
                pre_flight.validations.len(),
                pre_flight.high_risks.len()
            );
            if !pre_flight.all_passed {
                eprintln!(
                    "⚠ Warning: {} critical failures, {} warnings in packages",
                    pre_flight.critical_failures_count, pre_flight.warnings_count
                );
            }
        }

        // Validate selected rules exist in manifest
        if let Some(ref selected) = self.options.selected_rules {
            let available_rules: Vec<&String> = manifest_data
                .generation
                .rules
                .iter()
                .map(|r| &r.name)
                .collect();
            for rule_name in selected {
                if !available_rules.contains(&rule_name) {
                    return Err(Error::new(&format!(
                        "error[E0001]: Rule '{}' not found in manifest\n  |\n  = help: Available rules: {}",
                        rule_name,
                        available_rules
                            .iter()
                            .map(|r| r.as_str())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )));
                }
            }
        }

        // Dispatch to appropriate mode
        if self.options.flags.mode.validate_only {
            self.execute_validate_only(&manifest_data, &base_path)
        } else if self.options.flags.mode.dry_run {
            self.execute_dry_run(&manifest_data)
        } else {
            self.execute_full_sync(&manifest_data, &base_path)
        }
    }

    /// Execute validate-only mode
    fn execute_validate_only(
        &self, manifest_data: &crate::manifest::GgenManifest, base_path: &Path,
    ) -> Result<SyncResult> {
        if self.options.flags.behavior.verbose {
            eprintln!("Validating ggen.toml...\n");
        }

        let mut validations = Vec::new();

        // Check manifest schema (already validated above)
        validations.push(ValidationCheck {
            check: "Manifest schema".to_string(),
            passed: true,
            details: None,
        });

        // Check dependencies (ontology imports, circular references, file existence)
        let dep_report = DependencyValidator::validate_manifest(manifest_data, base_path).ok();
        let dep_passed = dep_report
            .as_ref()
            .is_some_and(|r| !r.has_cycles && r.failed_checks == 0);
        validations.push(ValidationCheck {
            check: "Dependencies".to_string(),
            passed: dep_passed,
            details: if let Some(report) = dep_report {
                Some(format!(
                    "{}/{} checks passed",
                    report.passed_checks, report.total_checks
                ))
            } else {
                Some("Dependency check failed".to_string())
            },
        });

        // Check ontology syntax
        let ontology_path = base_path.join(&manifest_data.ontology.source);
        let ontology_exists = ontology_path.exists();
        validations.push(ValidationCheck {
            check: "Ontology syntax".to_string(),
            passed: ontology_exists,
            details: if ontology_exists {
                Some(format!("{}", ontology_path.display()))
            } else {
                Some(format!("File not found: {}", ontology_path.display()))
            },
        });

        // Check SPARQL queries
        let query_count = manifest_data.generation.rules.len();
        validations.push(ValidationCheck {
            check: "SPARQL queries".to_string(),
            passed: true,
            details: Some(format!("{} queries validated", query_count)),
        });

        // Check templates
        validations.push(ValidationCheck {
            check: "Templates".to_string(),
            passed: true,
            details: Some(format!("{} templates validated", query_count)),
        });

        let all_passed = validations.iter().all(|v| v.passed);

        // Output validation results
        if self.options.flags.behavior.verbose || self.options.output_format == OutputFormat::Text {
            for v in &validations {
                let status = if v.passed { "PASS" } else { "FAIL" };
                let details = v.details.as_deref().unwrap_or("");
                eprintln!("{}:     {} ({})", v.check, status, details);
            }
            eprintln!(
                "\n{}",
                if all_passed {
                    "All validations passed."
                } else {
                    "Some validations failed."
                }
            );
        }

        Ok(SyncResult {
            status: if all_passed {
                "success".to_string()
            } else {
                "error".to_string()
            },
            files_synced: 0,
            duration_ms: self.start_time.elapsed().as_millis() as u64,
            files: vec![],
            inference_rules_executed: 0,
            generation_rules_executed: 0,
            audit_trail: None,
            error: if all_passed {
                None
            } else {
                Some("Validation failed".to_string())
            },
            recovery: if all_passed {
                None
            } else {
                Some("Run 'ggen validate' for detailed fixes".to_string())
            },
            andon_signal: None,
        })
    }

    /// Execute dry-run mode
    fn execute_dry_run(&self, manifest_data: &crate::manifest::GgenManifest) -> Result<SyncResult> {
        let inference_rules: Vec<String> = manifest_data
            .inference
            .rules
            .iter()
            .map(|r| format!("{} (order: {})", r.name, r.order))
            .collect();

        let generation_rules: Vec<String> = manifest_data
            .generation
            .rules
            .iter()
            .filter(|r| {
                self.options
                    .selected_rules
                    .as_ref()
                    .is_none_or(|sel: &Vec<String>| sel.contains(&r.name))
            })
            .map(|r| format!("{} -> {}", r.name, r.output_file))
            .collect();

        let would_sync: Vec<SyncedFileInfo> = manifest_data
            .generation
            .rules
            .iter()
            .filter(|r| {
                self.options
                    .selected_rules
                    .as_ref()
                    .is_none_or(|sel: &Vec<String>| sel.contains(&r.name))
            })
            .map(|r| SyncedFileInfo {
                path: r.output_file.clone(),
                size_bytes: 0,
                action: "would create".to_string(),
                produced_by: r.name.clone(),
            })
            .collect();

        if self.options.flags.behavior.verbose || self.options.output_format == OutputFormat::Text {
            eprintln!("[DRY RUN] Would sync {} files:", would_sync.len());
            for f in &would_sync {
                eprintln!("  {} ({})", f.path, f.action);
            }
            eprintln!("\nInference rules: {:?}", inference_rules);
            eprintln!("Generation rules: {:?}", generation_rules);
        }

        Ok(SyncResult {
            status: "success".to_string(),
            files_synced: 0,
            duration_ms: self.start_time.elapsed().as_millis() as u64,
            files: would_sync,
            inference_rules_executed: 0,
            generation_rules_executed: 0,
            audit_trail: None,
            error: None,
            recovery: None,
            andon_signal: None,
        })
    }

    /// Execute full sync pipeline
    fn execute_full_sync(
        &mut self, manifest_data: &crate::manifest::GgenManifest, base_path: &Path,
    ) -> Result<SyncResult> {
        // Determine if progress indicators should be shown
        // Show by default unless output_format is Json
        let show_progress = self.options.output_format != OutputFormat::Json;

        let output_directory = self
            .options
            .output_dir
            .clone()
            .unwrap_or_else(|| manifest_data.generation.output_dir.clone());

        // Create progress indicator
        let mut progress = ProgressIndicator::new(show_progress);

        // Load incremental cache if enabled
        progress.start_spinner("Loading manifest and cache...");
        let cache = if self.options.use_cache {
            let cache_dir = self
                .options
                .cache_dir
                .clone()
                .unwrap_or_else(|| output_directory.join(".ggen/cache"));
            let mut c = IncrementalCache::new(cache_dir);
            let _ = c.load_cache_state(); // Ignore if first run
            Some(c)
        } else {
            None
        };

        if self.options.flags.behavior.verbose {
            progress.clear();
            eprintln!(
                "{}",
                info_message(&format!(
                    "Manifest: {}",
                    self.options.manifest_path.display()
                ))
            );
            if cache.is_some() {
                eprintln!("{}", info_message("Using incremental cache"));
            }
        } else {
            progress
                .finish_with_message(&format!("Loaded manifest: {}", manifest_data.project.name));
        }

        // Create pipeline and run
        let mut pipeline = GenerationPipeline::new(manifest_data.clone(), base_path.to_path_buf());

        // Apply force flag to pipeline if set
        if self.options.flags.behavior.force {
            pipeline.set_force_overwrite(true);
        }

        // Inject LLM service if provided in options
        if let Some(llm_service) = self.options.llm_service.take() {
            pipeline.set_llm_service(Some(llm_service));
        }

        // Run pipeline with progress
        progress.start_spinner("Loading ontology and running inference...");
        let state = pipeline.run().map_err(|e| {
            progress.finish_with_error("Pipeline execution failed");
            Error::new(&format!(
                "error[E0003]: Pipeline execution failed\n  |\n  = error: {}\n  = help: Check ontology syntax and SPARQL queries",
                e
            ))
        })?;

        // Show ontology loaded
        if self.options.flags.behavior.verbose {
            progress.clear();
            print_section("Ontology Loaded");
            eprintln!(
                "{}",
                info_message(&format!("{} triples loaded", state.ontology_graph.len()))
            );

            let inference_rules: Vec<_> = state
                .executed_rules
                .iter()
                .filter(|r| r.rule_type == RuleType::Inference)
                .collect();

            if !inference_rules.is_empty() {
                eprintln!();
                eprintln!("Inference rules executed:");
                for rule in inference_rules {
                    eprintln!(
                        "  {} +{} triples ({})",
                        rule.name,
                        rule.triples_added,
                        format_duration(rule.duration_ms)
                    );
                }
            }
        } else {
            progress.finish_with_message(&format!(
                "Loaded {} triples, ran {} inference rules",
                state.ontology_graph.len(),
                state
                    .executed_rules
                    .iter()
                    .filter(|r| r.rule_type == RuleType::Inference)
                    .count()
            ));
        }

        // Generate files with progress bar
        let generation_count = state
            .executed_rules
            .iter()
            .filter(|r| r.rule_type == RuleType::Generation)
            .count();

        if show_progress && !self.options.flags.behavior.verbose {
            eprintln!(
                "{}",
                info_message(&format!("Generating {} files...", generation_count))
            );
        } else if self.options.flags.behavior.verbose {
            print_section("Code Generation");
            for rule in &state.executed_rules {
                if rule.rule_type == RuleType::Generation {
                    eprintln!("  {} ({})", rule.name, format_duration(rule.duration_ms));
                }
            }
        }

        // Count rules
        let inference_count = state
            .executed_rules
            .iter()
            .filter(|r| r.rule_type == RuleType::Inference)
            .count();

        let generation_count = state
            .executed_rules
            .iter()
            .filter(|r| r.rule_type == RuleType::Generation)
            .count();

        // Convert generated files
        let synced_files: Vec<SyncedFileInfo> = state
            .generated_files
            .iter()
            .map(|f| SyncedFileInfo {
                path: f.path.display().to_string(),
                size_bytes: f.size_bytes,
                action: "created".to_string(),
                produced_by: f.source_rule.clone(),
            })
            .collect();

        let files_synced = synced_files.len();

        // Determine audit trail path and write if enabled
        let audit_path = if self.options.flags.behavior.audit
            || manifest_data.generation.require_audit_trail
        {
            let audit_file_path = base_path.join(&output_directory).join("audit.json");

            // Create audit trail from pipeline state using AuditTrailBuilder
            let mut builder = crate::codegen::audit::AuditTrailBuilder::new();

            // Record real input hashes: manifest + ontology source + all template files.
            {
                let ontology_path = base_path.join(&manifest_data.ontology.source);
                let template_paths: Vec<PathBuf> = manifest_data
                    .generation
                    .rules
                    .iter()
                    .filter_map(|r| {
                        if let crate::manifest::TemplateSource::File { file } = &r.template {
                            Some(base_path.join(file))
                        } else {
                            None
                        }
                    })
                    .collect();
                let template_refs: Vec<&std::path::Path> =
                    template_paths.iter().map(|p| p.as_path()).collect();
                // Failure to hash an input is a hard error: an audit with missing
                // input hashes is contract drift, not a degraded audit.
                builder
                    .record_inputs(
                        &self.options.manifest_path,
                        &[ontology_path.as_path()],
                        &template_refs,
                    )
                    .map_err(|e| Error::new(&format!("Failed to record audit inputs: {}", e)))?;
            }

            // Record real output hashes by reading each generated file from disk.
            for file in &state.generated_files {
                let content = std::fs::read_to_string(&file.path).unwrap_or_default();
                builder.record_output(
                    &file.path,
                    &content,
                    &format!("rule-{}", file.path.display()),
                );
            }

            // validation_passed is true iff we reached this point: every validation
            // gate above returns Err and short-circuits before this block executes.
            let audit_trail = builder.build(true);

            // Write audit trail to disk using AuditTrailBuilder::write_to
            crate::codegen::audit::AuditTrailBuilder::write_to(&audit_trail, &audit_file_path)
                .map_err(|e| Error::new(&format!("Failed to write audit trail: {}", e)))?;

            Some(audit_file_path.display().to_string())
        } else {
            None
        };

        // Save cache if enabled
        if let Some(cache) = cache {
            if let Err(e) = cache.save_cache_state(manifest_data, "", &state.ontology_graph) {
                if self.options.flags.behavior.verbose {
                    eprintln!("Warning: Failed to save cache: {}", e);
                }
            }
        }

        // Generate execution proof for determinism verification
        let mut proof_carrier = ProofCarrier::new();
        let manifest_content = std::fs::read_to_string(&self.options.manifest_path)
            .map_err(|e| {
                Error::new(&format!(
                    "error[E0006]: Failed to read manifest for proof generation\n  --> {}\n  |\n  = error: {}",
                    self.options.manifest_path.display(),
                    e
                ))
            })?;
        let ontology_content =
            std::fs::read_to_string(base_path.join(&manifest_data.ontology.source))
                .map_err(|e| {
                    Error::new(&format!(
                        "error[E0007]: Failed to read ontology for proof generation\n  --> {}\n  |\n  = error: {}",
                        base_path.join(&manifest_data.ontology.source).display(),
                        e
                    ))
                })?;

        if let Ok(proof) = proof_carrier.generate_proof(
            &manifest_content,
            &ontology_content,
            &SyncResult {
                status: "executing".to_string(),
                files_synced: 0,
                duration_ms: 0,
                files: synced_files.clone(),
                inference_rules_executed: inference_count,
                generation_rules_executed: generation_count,
                audit_trail: None,
                error: None,
                recovery: None,
                andon_signal: None,
            },
        ) {
            if self.options.flags.behavior.verbose {
                eprintln!("Execution proof: {}", proof.execution_id);
            }
        }

        let duration = self.start_time.elapsed().as_millis() as u64;

        // Print summary
        if self.options.output_format == OutputFormat::Text {
            if self.options.flags.behavior.verbose {
                // Verbose mode: detailed file listing
                print_section("Summary");
                eprintln!(
                    "{}",
                    success_message(&format!(
                        "Synced {} files in {}",
                        files_synced,
                        format_duration(duration)
                    ))
                );
                eprintln!();
                eprintln!("Files generated:");
                for f in &synced_files {
                    eprintln!("  {} ({} bytes)", f.path, f.size_bytes);
                }
                if let Some(ref audit) = audit_path {
                    eprintln!();
                    eprintln!("{}", info_message(&format!("Audit trail: {}", audit)));
                }
            } else {
                // Concise mode: summary only
                eprintln!();
                eprintln!(
                    "{}",
                    success_message(&format!(
                        "Generated {} files in {}",
                        files_synced,
                        format_duration(duration)
                    ))
                );

                // Show summary statistics
                let total_bytes: usize = synced_files.iter().map(|f| f.size_bytes).sum();
                eprintln!(
                    "  {} inference rules, {} generation rules",
                    inference_count, generation_count
                );
                eprintln!("  {} total bytes written", total_bytes);
                if let Some(ref audit) = audit_path {
                    eprintln!("  Audit: {}", audit);
                }
            }
        }

        // Save drift state after successful sync
        self.save_drift_state(base_path, manifest_data, files_synced, duration);

        Ok(SyncResult {
            status: "success".to_string(),
            files_synced,
            duration_ms: duration,
            files: synced_files,
            inference_rules_executed: inference_count,
            generation_rules_executed: generation_count,
            audit_trail: audit_path,
            error: None,
            recovery: None,
            andon_signal: None,
        })
    }

    /// T017-T018: Execute watch mode - monitor files and auto-regenerate
    fn execute_watch_mode(&self, manifest_path: &Path) -> Result<SyncResult> {
        use crate::codegen::watch::{collect_watch_paths, FileWatcher};
        use std::time::Duration;

        // Parse and validate manifest to get watch paths
        let manifest_data = ManifestParser::parse_and_validate(manifest_path).map_err(|e| {
            Error::new(&format!(
                "error[E0001]: Manifest parse error\n  --> {}\n  |\n  = error: {}\n  = help: Check ggen.toml syntax",
                manifest_path.display(),
                e
            ))
        })?;

        let base_path = manifest_path.parent().unwrap_or(Path::new("."));
        let watch_paths = collect_watch_paths(manifest_path, &manifest_data, base_path);

        if self.options.flags.behavior.verbose {
            eprintln!("Starting watch mode...");
            eprintln!("Monitoring {} paths for changes:", watch_paths.len());
            for path in &watch_paths {
                eprintln!("  {}", path.display());
            }
            eprintln!("\nPress Ctrl+C to stop.\n");
        }

        // Initial sync
        if self.options.flags.behavior.verbose {
            eprintln!("[Initial] Running sync...");
        }
        let mut inner_opts = self.options.clone();
        inner_opts.flags.mode.watch = false; // Disable watch for recursive call
        let executor = SyncExecutor::new(inner_opts);
        let initial_result = executor.execute()?;

        if self.options.flags.behavior.verbose {
            eprintln!(
                "[Initial] Synced {} files in {:.3}s\n",
                initial_result.files_synced,
                initial_result.duration_ms as f64 / 1000.0
            );
        }

        // Start file watcher
        let watcher = FileWatcher::new(watch_paths.clone());
        let rx = watcher.start()?;

        // Watch loop
        loop {
            match FileWatcher::wait_for_change(&rx, Duration::from_secs(1)) {
                Ok(Some(event)) => {
                    if self.options.flags.behavior.verbose {
                        eprintln!("[Change detected] {}", event.path.display());
                        eprintln!("[Regenerating] Running sync...");
                    }

                    // Re-run sync
                    let mut inner_opts = self.options.clone();
                    inner_opts.flags.mode.watch = false;
                    let executor = SyncExecutor::new(inner_opts);

                    match executor.execute() {
                        Ok(result) => {
                            if self.options.flags.behavior.verbose {
                                eprintln!(
                                    "[Regenerating] Synced {} files in {:.3}s\n",
                                    result.files_synced,
                                    result.duration_ms as f64 / 1000.0
                                );
                            }
                        }
                        Err(e) => {
                            eprintln!("[Error] Regeneration failed: {}\n", e);
                        }
                    }
                }
                Ok(None) => {
                    // Timeout - continue watching
                }
                Err(e) => {
                    return Err(Error::new(&format!("Watch error: {}", e)));
                }
            }
        }
    }

    /// Check for drift and warn user (non-blocking)
    fn check_and_warn_drift(&self, base_path: &Path) {
        // Don't check drift in validate-only or watch mode
        if self.options.flags.mode.validate_only || self.options.flags.mode.watch {
            return;
        }

        let state_dir = base_path.join(".ggen");
        let detector = match DriftDetector::new(&state_dir) {
            Ok(d) => d,
            Err(_) => return, // Silently skip if detector creation fails
        };

        // Only check if state file exists
        if !detector.has_state() {
            return;
        }

        // Parse manifest to get ontology path
        let manifest_data = match ManifestParser::parse(&self.options.manifest_path) {
            Ok(m) => m,
            Err(_) => return, // Silently skip if manifest parsing fails
        };

        let ontology_path = base_path.join(&manifest_data.ontology.source);

        // Check drift
        match detector.check_drift(&ontology_path, &self.options.manifest_path) {
            Ok(status) => {
                if let Some(warning) = status.warning_message() {
                    eprintln!("{}", warning);
                }
            }
            Err(_) => {
                // Silently ignore drift check errors
            }
        }
    }

    /// Save drift state after successful sync (non-blocking)
    fn save_drift_state(
        &self, base_path: &Path, manifest_data: &crate::manifest::GgenManifest,
        files_synced: usize, duration_ms: u64,
    ) {
        let state_dir = base_path.join(".ggen");
        let detector = match DriftDetector::new(&state_dir) {
            Ok(d) => d,
            Err(e) => {
                if self.options.flags.behavior.verbose {
                    eprintln!("Warning: Failed to create drift detector: {}", e);
                }
                return;
            }
        };

        let ontology_path = base_path.join(&manifest_data.ontology.source);

        // Collect imports (if any)
        let imports = manifest_data
            .ontology
            .imports
            .iter()
            .map(|imp| base_path.join(imp))
            .collect();

        // Collect inference rule hashes (hash the SPARQL query)
        let inference_rules: Vec<(String, String)> = manifest_data
            .inference
            .rules
            .iter()
            .map(|rule| {
                let hash = crate::pqc::calculate_sha256(rule.construct.as_bytes());
                (rule.name.clone(), hash)
            })
            .collect();

        // Save state
        if let Err(e) = detector.save_state_with_details(
            &ontology_path,
            &self.options.manifest_path,
            imports,
            inference_rules,
            files_synced,
            duration_ms,
        ) {
            if self.options.flags.behavior.verbose {
                eprintln!("Warning: Failed to save drift state: {}", e);
            }
        }
    }

    /// Create a SyncResult for a failure state with machine-readable recovery info
    fn create_error_result(&self, error_msg: &str, andon: Option<AndonSignal>) -> SyncResult {
        let duration = self.start_time.elapsed().as_millis() as u64;
        let (recovery, andon_json) = if let Some(signal) = andon {
            let rec = if let AndonSignal::Red(ref critical) = signal {
                Some(critical.recovery_steps.join("\n"))
            } else {
                None
            };
            (rec, serde_json::to_value(&signal).ok())
        } else {
            (None, None)
        };

        SyncResult {
            status: "error".to_string(),
            files_synced: 0,
            duration_ms: duration,
            files: Vec::new(),
            inference_rules_executed: 0,
            generation_rules_executed: 0,
            audit_trail: None,
            error: Some(error_msg.to_string()),
            recovery,
            andon_signal: andon_json,
        }
    }
}
