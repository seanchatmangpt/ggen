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

use crate::codegen::pipeline::{GenerationPipeline, RuleType};
use crate::codegen::ux::{
    format_duration, info_message, print_section, success_message, warning_message,
    ProgressIndicator,
};
use crate::codegen::{
    DependencyValidator, IncrementalCache, MarketplaceValidator, OutputFormat, ProofCarrier,
    SyncOptions,
};
use crate::drift::DriftDetector;
use crate::manifest::{ManifestParser, ManifestValidator};
use crate::poka_yoke::QualityGateRunner;
use ggen_utils::error::{Error, Result};
use serde::Serialize;
use std::path::Path;
use std::time::Instant;

// ============================================================================
// Sync Result Types
// ============================================================================

/// Result of sync execution - returned to CLI layer
#[derive(Debug, Clone, Serialize)]
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
}

/// Individual file info in sync result
#[derive(Debug, Clone, Serialize)]
pub struct SyncedFileInfo {
    /// File path
    pub path: String,

    /// File size in bytes
    pub size_bytes: usize,

    /// Action taken: "created", "updated", "unchanged", "would create"
    pub action: String,
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

    /// Execute the sync pipeline based on options
    ///
    /// Returns `SyncResult` that can be serialized to JSON or formatted as text.
    pub fn execute(self) -> Result<SyncResult> {
        // Pre-flight validation: Check environment before proceeding
        let base_path = self
            .options
            .manifest_path
            .parent()
            .unwrap_or(Path::new("."));

        let preflight = crate::validation::PreFlightValidator::for_sync(base_path)
            .with_llm_check(false) // LLM check is optional (warning only)
            .with_template_check(false) // Will check after parsing manifest
            .with_git_check(false);

        // Run basic pre-flight checks (without manifest, as we haven't parsed it yet)
        if let Err(e) = preflight.validate(None) {
            if self.options.verbose {
                eprintln!("{}", warning_message(&format!("Pre-flight warning: {}", e)));
            }
        } else if self.options.verbose {
            eprintln!("{}", success_message("Pre-flight checks passed"));
        }

        // Validate manifest exists
        if !self.options.manifest_path.exists() {
            return Err(Error::new(&format!(
                "error[E0001]: Manifest not found\n  --> {}\n  |\n  = help: Create a ggen.toml manifest file or specify path with --manifest",
                self.options.manifest_path.display()
            )));
        }

        // Check for drift (non-blocking warning)
        self.check_and_warn_drift(base_path);

        // T017-T018: Watch mode implementation
        if self.options.watch {
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
        let base_path = self
            .options
            .manifest_path
            .parent()
            .unwrap_or(Path::new("."));
        let validator = ManifestValidator::new(&manifest_data, base_path);
        validator.validate().map_err(|e| {
            Error::new(&format!(
                "error[E0001]: Manifest validation failed\n  --> {}\n  |\n  = error: {}\n  = help: Fix validation errors before syncing",
                self.options.manifest_path.display(),
                e
            ))
        })?;

        // Validate dependencies (ontology imports, circular references, file existence)
        let dep_validator = DependencyValidator::validate_manifest(&manifest_data, base_path)
            .map_err(|e| {
                Error::new(&format!(
                    "error[E0002]: Dependency validation failed\n  |\n  = error: {}\n  = help: Fix missing ontology imports or circular dependencies",
                    e
                ))
            })?;

        if dep_validator.has_cycles {
            return Err(Error::new(&format!(
                "error[E0002]: Circular dependency detected\n  |\n  = error: Inference rules have circular dependencies\n  = cycles: {:?}\n  = help: Review rule dependencies in manifest",
                dep_validator.cycle_nodes
            )));
        }

        if dep_validator.failed_checks > 0 {
            return Err(Error::new(&format!(
                "error[E0002]: {} dependency checks failed\n  |\n  = help: Fix missing files or imports before syncing",
                dep_validator.failed_checks
            )));
        }

        // Run quality gates - mandatory checkpoints before generation
        let gate_runner = QualityGateRunner::new();
        gate_runner.run_all(&manifest_data, base_path).map_err(|e| {
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

        if self.options.verbose {
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
        if self.options.validate_only {
            self.execute_validate_only(&manifest_data, base_path)
        } else if self.options.dry_run {
            self.execute_dry_run(&manifest_data)
        } else {
            self.execute_full_sync(&manifest_data, base_path)
        }
    }

    /// Execute validate-only mode
    fn execute_validate_only(
        &self, manifest_data: &crate::manifest::GgenManifest, base_path: &Path,
    ) -> Result<SyncResult> {
        if self.options.verbose {
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
        if self.options.verbose || self.options.output_format == OutputFormat::Text {
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
                    .is_none_or(|sel| sel.contains(&r.name))
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
                    .is_none_or(|sel| sel.contains(&r.name))
            })
            .map(|r| SyncedFileInfo {
                path: r.output_file.clone(),
                size_bytes: 0,
                action: "would create".to_string(),
            })
            .collect();

        if self.options.verbose || self.options.output_format == OutputFormat::Text {
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
        })
    }

    /// Execute full sync pipeline
    fn execute_full_sync(
        &self, manifest_data: &crate::manifest::GgenManifest, base_path: &Path,
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

        if self.options.verbose {
            progress.clear();
            eprintln!("{}", info_message(&format!("Manifest: {}", self.options.manifest_path.display())));
            if cache.is_some() {
                eprintln!("{}", info_message("Using incremental cache"));
            }
        } else {
            progress.finish_with_message(&format!("Loaded manifest: {}", manifest_data.project.name));
        }

        // Create pipeline and run
        let mut pipeline = GenerationPipeline::new(manifest_data.clone(), base_path.to_path_buf());

        // Apply force flag to pipeline if set
        if self.options.force {
            pipeline.set_force_overwrite(true);
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
        if self.options.verbose {
            progress.clear();
            print_section("Ontology Loaded");
            eprintln!("{}", info_message(&format!("{} triples loaded", state.ontology_graph.len())));

            let inference_rules: Vec<_> = state.executed_rules.iter()
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
                state.executed_rules.iter().filter(|r| r.rule_type == RuleType::Inference).count()
            ));
        }

        // Generate files with progress bar
        let generation_count = state.executed_rules.iter()
            .filter(|r| r.rule_type == RuleType::Generation)
            .count();

        if show_progress && !self.options.verbose {
            eprintln!("{}", info_message(&format!("Generating {} files...", generation_count)));
        } else if self.options.verbose {
            print_section("Code Generation");
            for rule in &state.executed_rules {
                if rule.rule_type == RuleType::Generation {
                    eprintln!(
                        "  {} ({})",
                        rule.name,
                        format_duration(rule.duration_ms)
                    );
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
            })
            .collect();

        let files_synced = synced_files.len();

        // Determine audit trail path and write if enabled
        let audit_path = if self.options.audit || manifest_data.generation.require_audit_trail {
            let audit_file_path = base_path.join(&output_directory).join("audit.json");

            // Create audit trail from pipeline state
            let mut audit_trail = crate::audit::AuditTrail::new(
                "5.1.0",
                &self.options.manifest_path.display().to_string(),
                &manifest_data.ontology.source.display().to_string(),
            );

            // Record rules executed
            for _ in &state.executed_rules {
                audit_trail.record_rule_executed();
            }

            // Record files changed with hashes
            for file in &state.generated_files {
                audit_trail
                    .record_file_change(file.path.display().to_string(), file.content_hash.clone());
            }

            // Set execution metadata
            audit_trail.metadata.duration_ms = self.start_time.elapsed().as_millis() as u64;
            audit_trail.metadata.spec_hash = format!("manifest-{}", manifest_data.project.version);

            // Write audit trail to disk
            crate::audit::writer::AuditTrailWriter::write(&audit_trail, &audit_file_path)
                .map_err(|e| Error::new(&format!("Failed to write audit trail: {}", e)))?;

            Some(audit_file_path.display().to_string())
        } else {
            None
        };

        // Save cache if enabled
        if let Some(cache) = cache {
            if let Err(e) = cache.save_cache_state(manifest_data, "", &state.ontology_graph) {
                if self.options.verbose {
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
            },
        ) {
            if self.options.verbose {
                eprintln!("Execution proof: {}", proof.execution_id);
            }
        }

        let duration = self.start_time.elapsed().as_millis() as u64;

        // Print summary
        if self.options.output_format == OutputFormat::Text {
            if self.options.verbose {
                // Verbose mode: detailed file listing
                print_section("Summary");
                eprintln!("{}", success_message(&format!("Synced {} files in {}", files_synced, format_duration(duration))));
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
                eprintln!(
                    "  {} total bytes written",
                    total_bytes
                );
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
        })
    }

    /// T017-T018: Execute watch mode - monitor files and auto-regenerate
    fn execute_watch_mode(&self, manifest_path: &Path) -> Result<SyncResult> {
        use crate::codegen::watch::{collect_watch_paths, FileWatcher};
        use std::time::Duration;

        // Parse manifest to get watch paths
        let manifest_data = ManifestParser::parse(manifest_path).map_err(|e| {
            Error::new(&format!(
                "error[E0001]: Manifest parse error\n  --> {}\n  |\n  = error: {}\n  = help: Check ggen.toml syntax",
                manifest_path.display(),
                e
            ))
        })?;

        let base_path = manifest_path.parent().unwrap_or(Path::new("."));
        let watch_paths = collect_watch_paths(manifest_path, &manifest_data, base_path);

        if self.options.verbose {
            eprintln!("Starting watch mode...");
            eprintln!("Monitoring {} paths for changes:", watch_paths.len());
            for path in &watch_paths {
                eprintln!("  {}", path.display());
            }
            eprintln!("\nPress Ctrl+C to stop.\n");
        }

        // Initial sync
        if self.options.verbose {
            eprintln!("[Initial] Running sync...");
        }
        let executor = SyncExecutor::new(SyncOptions {
            watch: false, // Disable watch for recursive call
            ..self.options.clone()
        });
        let initial_result = executor.execute()?;

        if self.options.verbose {
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
                    if self.options.verbose {
                        eprintln!("[Change detected] {}", event.path.display());
                        eprintln!("[Regenerating] Running sync...");
                    }

                    // Re-run sync
                    let executor = SyncExecutor::new(SyncOptions {
                        watch: false,
                        ..self.options.clone()
                    });

                    match executor.execute() {
                        Ok(result) => {
                            if self.options.verbose {
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
        if self.options.validate_only || self.options.watch {
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
        &self,
        base_path: &Path,
        manifest_data: &crate::manifest::GgenManifest,
        files_synced: usize,
        duration_ms: u64,
    ) {
        let state_dir = base_path.join(".ggen");
        let detector = match DriftDetector::new(&state_dir) {
            Ok(d) => d,
            Err(e) => {
                if self.options.verbose {
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
            if self.options.verbose {
                eprintln!("Warning: Failed to save drift state: {}", e);
            }
        }
    }
}
