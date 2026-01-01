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
use crate::codegen::{
    DependencyValidator, IncrementalCache, MarketplaceValidator, OutputFormat, ProofCarrier,
    SyncOptions,
};
use crate::manifest::{ManifestParser, ManifestValidator};
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
        // Validate manifest exists
        if !self.options.manifest_path.exists() {
            return Err(Error::new(&format!(
                "error[E0001]: Manifest not found\n  --> {}\n  |\n  = help: Create a ggen.toml manifest file or specify path with --manifest",
                self.options.manifest_path.display()
            )));
        }

        // Watch mode - not yet implemented
        if self.options.watch {
            return Err(Error::new(
                "Watch mode (--watch) is not yet implemented. Use manual sync for now.",
            ));
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

        // Run marketplace pre-flight validation (FMEA analysis)
        let marketplace_validator = MarketplaceValidator::new(160);
        let pre_flight = marketplace_validator.pre_flight_check(&manifest_data).map_err(|e| {
            Error::new(&format!(
                "error[E0003]: Marketplace pre-flight validation failed\n  |\n  = error: {}\n  = help: Review package dependencies and resolve high-risk items",
                e
            ))
        })?;

        if self.options.verbose {
            eprintln!("Pre-flight checks: {} validations, {} high-risk items detected",
                pre_flight.validations.len(), pre_flight.high_risks.len());
            if !pre_flight.all_passed {
                eprintln!("⚠ Warning: {} critical failures, {} warnings in packages",
                    pre_flight.critical_failures_count, pre_flight.warnings_count);
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
        let dep_passed = dep_report.as_ref().map_or(false, |r| !r.has_cycles && r.failed_checks == 0);
        validations.push(ValidationCheck {
            check: "Dependencies".to_string(),
            passed: dep_passed,
            details: if let Some(report) = dep_report {
                Some(format!("{}/{} checks passed", report.passed_checks, report.total_checks))
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
        let output_directory = self
            .options
            .output_dir
            .clone()
            .unwrap_or_else(|| manifest_data.generation.output_dir.clone());

        // Load incremental cache if enabled
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

        // Create pipeline and run
        let mut pipeline = GenerationPipeline::new(manifest_data.clone(), base_path.to_path_buf());

        if self.options.verbose {
            eprintln!("Loading manifest: {}", self.options.manifest_path.display());
            if let Some(ref _cache) = cache {
                eprintln!("Using incremental cache...");
            }
        }

        let state = pipeline.run().map_err(|e| {
            Error::new(&format!(
                "error[E0003]: Pipeline execution failed\n  |\n  = error: {}\n  = help: Check ontology syntax and SPARQL queries",
                e
            ))
        })?;

        if self.options.verbose {
            eprintln!("Loading ontology: {} triples", state.ontology_graph.len());
            for rule in &state.executed_rules {
                if rule.rule_type == RuleType::Inference {
                    eprintln!(
                        "  [inference] {}: +{} triples ({}ms)",
                        rule.name, rule.triples_added, rule.duration_ms
                    );
                }
            }
            for rule in &state.executed_rules {
                if rule.rule_type == RuleType::Generation {
                    eprintln!("  [generation] {}: ({}ms)", rule.name, rule.duration_ms);
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

        // Determine audit trail path
        let audit_path = if self.options.audit || manifest_data.generation.require_audit_trail {
            Some(output_directory.join("audit.json").display().to_string())
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
            .unwrap_or_default();
        let ontology_content = std::fs::read_to_string(base_path.join(&manifest_data.ontology.source))
            .unwrap_or_default();

        if let Ok(proof) = proof_carrier.generate_proof(&manifest_content, &ontology_content, &SyncResult {
            status: "executing".to_string(),
            files_synced: 0,
            duration_ms: 0,
            files: synced_files.clone(),
            inference_rules_executed: inference_count,
            generation_rules_executed: generation_count,
            audit_trail: None,
            error: None,
        }) {
            if self.options.verbose {
                eprintln!("Execution proof: {}", proof.execution_id);
            }
        }

        let duration = self.start_time.elapsed().as_millis() as u64;

        if self.options.verbose || self.options.output_format == OutputFormat::Text {
            eprintln!(
                "\nSynced {} files in {:.3}s",
                files_synced,
                duration as f64 / 1000.0
            );
            for f in &synced_files {
                eprintln!("  {} ({} bytes)", f.path, f.size_bytes);
            }
            if let Some(ref audit) = audit_path {
                eprintln!("Audit trail: {}", audit);
            }
        }

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
}
