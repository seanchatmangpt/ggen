//! Mutation testing integration with cargo-mutants
//!
//! This module provides automated mutation testing to verify that tests actually catch bugs.
//! Key responsibilities:
//! - Execute cargo-mutants on specified crate paths
//! - Parse mutation results from JSON output
//! - Calculate mutation kill rate (target: ≥80%)
//! - Identify tests that fail to catch mutants (false positives)

use crate::types::{AuditResult, MutationResult, MutationType, TestId};
use chrono::Utc;
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use std::process::Command;
use walkdir::WalkDir;

/// Mutation testing orchestrator using cargo-mutants
#[derive(Debug)]
pub struct MutationAnalyzer {
    /// Workspace root directory
    workspace_root: PathBuf,
    /// Crate paths to analyze
    crate_paths: Vec<PathBuf>,
    /// Output directory for mutation reports
    output_dir: PathBuf,
}

impl MutationAnalyzer {
    /// Create a new mutation analyzer
    ///
    /// # Arguments
    /// * `workspace_root` - Workspace root directory containing crates
    /// * `output_dir` - Directory to store mutation reports (typically .ggen/mutation-reports/)
    ///
    /// # Errors
    /// Returns `AuditError::IoError` if workspace_root does not exist
    pub fn new(
        workspace_root: impl Into<PathBuf>, output_dir: impl Into<PathBuf>,
    ) -> AuditResult<Self> {
        let workspace_root = workspace_root.into();
        let output_dir = output_dir.into();

        if !workspace_root.exists() {
            return Err(crate::types::AuditError::IoError(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("Workspace root not found: {}", workspace_root.display()),
            )));
        }

        std::fs::create_dir_all(&output_dir)?;

        Ok(Self {
            workspace_root,
            crate_paths: Vec::new(),
            output_dir,
        })
    }

    /// Add a crate path for mutation testing
    ///
    /// # Arguments
    /// * `crate_path` - Relative path from workspace root (e.g., "crates/ggen-core")
    pub fn add_crate(&mut self, crate_path: impl Into<PathBuf>) {
        self.crate_paths.push(crate_path.into());
    }

    /// Add multiple critical paths for mutation testing
    ///
    /// Critical paths for ggen (Feature 004 requirement):
    /// - RDF parsing (ggen-rdf)
    /// - Ontology projection (ggen-ontology)
    /// - Code generation (ggen-core)
    /// - Configuration (ggen.toml handling)
    pub fn add_critical_paths(&mut self) {
        let critical_crates = vec![
            "crates/ggen-core",
            "crates/ggen-rdf",
            "crates/ggen-ontology",
            "crates/ggen-config",
        ];

        for crate_path in critical_crates {
            self.add_crate(crate_path);
        }
    }

    /// Run mutation testing on all configured crates
    ///
    /// Executes cargo-mutants and collects results for analysis.
    ///
    /// # Errors
    /// Returns `AuditError::MutationFailed` if:
    /// - cargo-mutants is not installed
    /// - Mutation testing execution fails
    /// - Output parsing fails
    pub fn run_mutations(&self) -> AuditResult<Vec<MutationResult>> {
        let mut all_results = Vec::new();

        for crate_path in &self.crate_paths {
            let full_path = self.workspace_root.join(crate_path);

            if !full_path.exists() {
                return Err(crate::types::AuditError::MutationFailed(format!(
                    "Crate path not found: {}",
                    full_path.display()
                )));
            }

            let results = self.run_mutations_for_crate(&full_path)?;
            all_results.extend(results);
        }

        Ok(all_results)
    }

    /// Run mutations for a single crate
    fn run_mutations_for_crate(&self, crate_path: &Path) -> AuditResult<Vec<MutationResult>> {
        // Execute cargo-mutants with JSON output
        let output = Command::new("cargo")
            .args([
                "mutants",
                "--json",
                "--output",
                self.output_dir.to_str().unwrap_or("."),
            ])
            .current_dir(crate_path)
            .output()
            .map_err(|e| {
                crate::types::AuditError::MutationFailed(format!(
                    "Failed to execute cargo-mutants: {}. Is cargo-mutants installed?",
                    e
                ))
            })?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(crate::types::AuditError::MutationFailed(format!(
                "cargo-mutants failed: {}",
                stderr
            )));
        }

        // Parse JSON output
        self.parse_mutation_results(crate_path)
    }

    /// Parse mutation results from cargo-mutants JSON output
    ///
    /// Converts cargo-mutants output format to ggen's MutationResult type.
    ///
    /// # Errors
    /// Returns `AuditError::JsonError` if JSON parsing fails
    pub fn parse_mutation_results(&self, crate_path: &Path) -> AuditResult<Vec<MutationResult>> {
        let mut results = Vec::new();

        // Find mutation result files in output directory
        for entry in WalkDir::new(&self.output_dir)
            .max_depth(2)
            .into_iter()
            .filter_map(Result::ok)
        {
            if entry.path().extension().and_then(|s| s.to_str()) == Some("json") {
                let content = std::fs::read_to_string(entry.path())?;
                let mutants: Vec<CargoMutantsResult> = serde_json::from_str(&content)?;

                for mutant in mutants {
                    results.push(self.convert_mutant_to_result(mutant, crate_path)?);
                }
            }
        }

        Ok(results)
    }

    /// Convert cargo-mutants result to ggen MutationResult
    fn convert_mutant_to_result(
        &self, mutant: CargoMutantsResult, _crate_path: &Path,
    ) -> AuditResult<MutationResult> {
        let test_id = TestId::new(mutant.test_id.unwrap_or_else(|| "unknown".to_string()))?;

        Ok(MutationResult {
            mutation_id: mutant.mutation_id,
            test_id,
            mutant_survived: !mutant.killed,
            mutation_type: self.classify_mutation_type(&mutant.mutation_op),
            kill_timestamp: Utc::now(),
        })
    }

    /// Classify mutation operation into MutationType enum
    fn classify_mutation_type(&self, op: &str) -> MutationType {
        if op.contains("binary") || op.contains("+") || op.contains("==") {
            MutationType::BinaryOp
        } else if op.contains("unary") || op.contains("!") {
            MutationType::UnaryOp
        } else if op.contains("const") || op.contains("literal") {
            MutationType::ConstantReplacement
        } else if op.contains("return") {
            MutationType::ReturnValueChange
        } else {
            MutationType::StatementDeletion
        }
    }

    /// Calculate mutation kill rate
    ///
    /// Returns the percentage of mutants killed (0.0 - 1.0).
    /// Target: ≥0.80 (80% kill rate)
    ///
    /// # Arguments
    /// * `results` - Mutation test results to analyze
    #[must_use]
    pub fn calculate_kill_rate(&self, results: &[MutationResult]) -> f64 {
        if results.is_empty() {
            return 0.0;
        }

        let killed = results.iter().filter(|r| !r.mutant_survived).count();
        killed as f64 / results.len() as f64
    }

    /// Generate baseline mutation report
    ///
    /// Creates initial mutation report for evidence collection.
    ///
    /// # Errors
    /// Returns `AuditError::IoError` if file write fails
    pub fn generate_baseline_report(&self, results: &[MutationResult]) -> AuditResult<PathBuf> {
        let report = BaselineMutationReport {
            timestamp: Utc::now(),
            total_mutants: results.len(),
            killed_mutants: results.iter().filter(|r| !r.mutant_survived).count(),
            survived_mutants: results.iter().filter(|r| r.mutant_survived).count(),
            kill_rate: self.calculate_kill_rate(results),
            results: results.to_vec(),
        };

        let report_path = self.output_dir.join("baseline-mutation-kill-rate.json");
        let json = serde_json::to_string_pretty(&report)?;
        std::fs::write(&report_path, json)?;

        Ok(report_path)
    }
}

/// Cargo-mutants JSON output format (subset)
#[derive(Debug, Deserialize)]
struct CargoMutantsResult {
    mutation_id: String,
    mutation_op: String,
    killed: bool,
    test_id: Option<String>,
}

/// Baseline mutation report for evidence collection
#[derive(Debug, Serialize)]
struct BaselineMutationReport {
    timestamp: chrono::DateTime<Utc>,
    total_mutants: usize,
    killed_mutants: usize,
    survived_mutants: usize,
    kill_rate: f64,
    results: Vec<MutationResult>,
}
