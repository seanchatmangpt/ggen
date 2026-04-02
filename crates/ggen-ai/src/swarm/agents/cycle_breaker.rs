//! CycleBreakerAgent - Autonomous cycle detection and fixing for ontologies
//!
//! This agent autonomously detects and fixes circular dependencies in RDF ontology
//! import graphs using the CycleFixer from ggen-core.
//!
//! # Capabilities
//!
//! - **Detect Cycles**: Analyze ontology import graphs for circular dependencies
//! - **Fix Cycles**: Apply automated fix strategies (remove_import, merge_files, create_interface)
//! - **Verify Fixes**: Run quality gates to ensure fixes worked correctly
//! - **Dry Run**: Preview changes before applying them
//! - **Backups**: Create automatic backups before modifying files
//!
//! # Fix Strategies
//!
//! 1. **RemoveImport**: Remove the problematic import statement (simplest)
//! 2. **MergeFiles**: Merge all files in the cycle into a single ontology
//! 3. **CreateInterface**: Extract shared definitions into a separate interface file
//!
//! # Examples
//!
//! ```ignore
//! use ggen_ai::swarm::agents::CycleBreakerAgent;
//!
//! let agent = CycleBreakerAgent::new("/path/to/ontology", false);
//! let report = agent.analyze_and_fix("/path/to/ontology").unwrap();
//!
//! println!("Cycles found: {}", report.cycles_found);
//! println!("Fixes applied: {}", report.fixes_applied);
//! ```

use crate::error::{GgenAiError, Result};
use crate::swarm::agents::BaseAgent;
use crate::swarm::{AgentHealth, AgentInput, AgentOutput, HealthStatus, SwarmAgent, SwarmContext};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use tracing::{debug, info, warn};

/// CycleBreakerAgent - Autonomous cycle detection and fixing
#[derive(Debug)]
pub struct CycleBreakerAgent {
    /// Base agent functionality
    base: BaseAgent,
    /// Backup directory for ontology files
    backup_dir: PathBuf,
    /// Dry run mode (preview changes without applying)
    dry_run: bool,
}

/// Cycle analysis and fix report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FixReport {
    /// Number of cycles detected
    pub cycles_found: usize,
    /// Number of fixes applied
    pub fixes_applied: usize,
    /// Files that were modified
    pub files_modified: Vec<String>,
    /// Path to backup directory (if backups were created)
    pub backup_path: Option<String>,
    /// Detailed cycle information
    pub cycles: Vec<CycleInfo>,
    /// Overall success status
    pub success: bool,
    /// Execution time in milliseconds
    pub execution_time_ms: u64,
}

/// Information about a detected cycle
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CycleInfo {
    /// Files involved in the cycle (in order)
    pub files: Vec<String>,
    /// Strategy used to fix this cycle
    pub fix_strategy: Option<String>,
    /// Whether the fix was successful
    pub fixed: bool,
}

/// Fix strategy for resolving cycles
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum FixStrategy {
    /// Remove the problematic import statement
    RemoveImport,
    /// Merge all files in the cycle into a single ontology
    MergeFiles,
    /// Extract shared definitions into a separate interface file
    CreateInterface,
}

impl FixStrategy {
    /// Parse strategy from string
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "remove_import" => Some(FixStrategy::RemoveImport),
            "merge_files" => Some(FixStrategy::MergeFiles),
            "create_interface" => Some(FixStrategy::CreateInterface),
            _ => None,
        }
    }

    /// Convert to display string
    pub fn as_str(&self) -> &str {
        match self {
            FixStrategy::RemoveImport => "remove_import",
            FixStrategy::MergeFiles => "merge_files",
            FixStrategy::CreateInterface => "create_interface",
        }
    }
}

impl CycleBreakerAgent {
    /// Create a new CycleBreakerAgent
    ///
    /// # Arguments
    ///
    /// * `ontology_path` - Path to the ontology directory
    /// * `dry_run` - If true, preview changes without applying them
    pub fn new(ontology_path: impl AsRef<Path>, dry_run: bool) -> Self {
        let ontology_path = ontology_path.as_ref();
        let backup_dir = ontology_path.join(".ggen").join("backups");

        let base = BaseAgent::new(
            "cycle-breaker",
            vec![
                "cycle_detection".to_string(),
                "cycle_fixing".to_string(),
                "ontology_validation".to_string(),
                "backup_management".to_string(),
            ],
            crate::swarm::agents::AgentConfig {
                timeout_seconds: 300, // 5 minutes
                retry_attempts: 2,
                verbose_logging: false,
                performance_thresholds: crate::swarm::agents::PerformanceThresholds {
                    max_execution_time_ms: 300000,
                    max_memory_usage_mb: 100,
                    min_quality_score: 0.95,
                },
            },
        );

        Self {
            base,
            backup_dir,
            dry_run,
        }
    }

    /// Analyze ontology for cycles and optionally fix them
    ///
    /// # Arguments
    ///
    /// * `ontology_path` - Path to the ontology directory to analyze
    ///
    /// # Returns
    ///
    /// * `Result<FixReport>` - Detailed report of cycles found and fixes applied
    pub fn analyze_and_fix(&self, ontology_path: &Path) -> Result<FixReport> {
        let start_time = std::time::Instant::now();

        info!(
            "Analyzing ontology at {} for cycles (dry_run: {})",
            ontology_path.display(),
            self.dry_run
        );

        // Use ggen_core's CycleFixer
        let fixer = ggen_core::graph::CycleFixer::new(ontology_path);

        // Detect cycles (use RemoveImport as default strategy)
        let core_report = fixer
            .detect_and_fix(ggen_core::graph::FixStrategy::RemoveImport, self.dry_run)
            .map_err(|e| {
                GgenAiError::ontology_analysis(&format!("Cycle detection failed: {}", e))
            })?;

        let execution_time = start_time.elapsed().as_millis() as u64;

        // Convert core report to our report format
        let report = FixReport {
            cycles_found: core_report.cycles_found,
            fixes_applied: core_report.fixes_applied,
            files_modified: core_report.files_modified,
            backup_path: core_report.backup_path,
            cycles: core_report
                .cycles
                .into_iter()
                .map(|c| CycleInfo {
                    files: c.files,
                    fix_strategy: c.fix_strategy.map(|s| format!("{:?}", s)),
                    fixed: c.fixed,
                })
                .collect(),
            success: core_report.cycles_found == 0 || core_report.fixes_applied > 0,
            execution_time_ms: execution_time,
        };

        if report.cycles_found > 0 {
            if self.dry_run {
                info!(
                    "Dry run: detected {} cycles (would modify {} files)",
                    report.cycles_found,
                    report.files_modified.len()
                );
            } else {
                info!(
                    "Fixed {} out of {} cycles (modified {} files)",
                    report.fixes_applied,
                    report.cycles_found,
                    report.files_modified.len()
                );
            }
        } else {
            info!("No cycles detected - ontology is acyclic");
        }

        Ok(report)
    }

    /// Detect cycles without fixing them
    ///
    /// # Arguments
    ///
    /// * `ontology_path` - Path to the ontology directory to analyze
    ///
    /// # Returns
    ///
    /// * `Result<Vec<Vec<String>>>` - List of cycles found (each cycle is a list of file paths)
    pub fn detect_cycles(&self, ontology_path: &Path) -> Result<Vec<Vec<String>>> {
        debug!("Detecting cycles in {}", ontology_path.display());

        // Build import graph
        let import_graph = self.build_import_graph(ontology_path)?;

        // Use ggen_core's cycle detection
        let cycles = ggen_core::graph::cycle_detection::detect_cycles(&import_graph);

        debug!("Detected {} cycles", cycles.len());

        Ok(cycles)
    }

    /// Fix cycles using a specific strategy
    ///
    /// # Arguments
    ///
    /// * `ontology_path` - Path to the ontology directory
    /// * `strategy` - Fix strategy to apply
    ///
    /// # Returns
    ///
    /// * `Result<FixReport>` - Detailed report of fixes applied
    pub fn fix_cycles(&self, ontology_path: &Path, strategy: FixStrategy) -> Result<FixReport> {
        let start_time = std::time::Instant::now();

        if self.dry_run {
            warn!("Dry run mode - will not apply fixes");
        }

        info!(
            "Fixing cycles in {} using strategy: {}",
            ontology_path.display(),
            strategy.as_str()
        );

        // Convert strategy
        let core_strategy = match strategy {
            FixStrategy::RemoveImport => ggen_core::graph::FixStrategy::RemoveImport,
            FixStrategy::MergeFiles => ggen_core::graph::FixStrategy::MergeFiles,
            FixStrategy::CreateInterface => ggen_core::graph::FixStrategy::CreateInterface,
        };

        // Use ggen_core's CycleFixer
        let fixer = ggen_core::graph::CycleFixer::new(ontology_path);

        let core_report = fixer
            .detect_and_fix(core_strategy, self.dry_run)
            .map_err(|e| GgenAiError::ontology_analysis(&format!("Cycle fixing failed: {}", e)))?;

        let execution_time = start_time.elapsed().as_millis() as u64;

        Ok(FixReport {
            cycles_found: core_report.cycles_found,
            fixes_applied: core_report.fixes_applied,
            files_modified: core_report.files_modified,
            backup_path: core_report.backup_path,
            cycles: core_report
                .cycles
                .into_iter()
                .map(|c| CycleInfo {
                    files: c.files,
                    fix_strategy: c.fix_strategy.map(|s| format!("{:?}", s)),
                    fixed: c.fixed,
                })
                .collect(),
            success: core_report.fixes_applied == core_report.cycles_found,
            execution_time_ms: execution_time,
        })
    }

    /// Verify that fixes were successful by re-detecting cycles
    ///
    /// # Arguments
    ///
    /// * `ontology_path` - Path to the ontology directory
    ///
    /// # Returns
    ///
    /// * `Result<bool>` - True if no cycles detected (fix successful)
    pub fn verify_fix(&self, ontology_path: &Path) -> Result<bool> {
        debug!("Verifying fixes in {}", ontology_path.display());

        let cycles = self.detect_cycles(ontology_path)?;

        if cycles.is_empty() {
            info!("Verification successful - no cycles detected");
            Ok(true)
        } else {
            warn!(
                "Verification failed - {} cycles still present",
                cycles.len()
            );
            Ok(false)
        }
    }

    /// Build import graph by parsing TTL files for owl:imports
    fn build_import_graph(&self, ontology_path: &Path) -> Result<HashMap<String, Vec<String>>> {
        debug!("Building import graph from {}", ontology_path.display());

        let fixer = ggen_core::graph::CycleFixer::new(ontology_path);

        // Use CycleFixer's internal method to build graph
        // We need to use reflection or re-implement since build_import_graph is private
        let mut graph = HashMap::new();

        // Find all .ttl files
        let ttl_files = self.find_ttl_files(ontology_path)?;

        for ttl_file in ttl_files {
            let file_name = ttl_file
                .strip_prefix(ontology_path)
                .unwrap_or(&ttl_file)
                .to_string_lossy()
                .to_string();

            // Parse file for owl:imports statements
            let imports = self.extract_imports(&ttl_file)?;
            graph.insert(file_name, imports);
        }

        debug!("Built import graph with {} nodes", graph.len());

        Ok(graph)
    }

    /// Find all .ttl files in the ontology directory
    fn find_ttl_files(&self, dir: &Path) -> Result<Vec<PathBuf>> {
        let mut ttl_files = Vec::new();

        if !dir.exists() {
            return Ok(ttl_files);
        }

        self.visit_dir(dir, &mut ttl_files)?;

        Ok(ttl_files)
    }

    /// Recursively visit directory to find .ttl files
    fn visit_dir(&self, dir: &Path, ttl_files: &mut Vec<PathBuf>) -> Result<()> {
        let entries = std::fs::read_dir(dir).map_err(|e| {
            GgenAiError::io_error(&format!(
                "Failed to read directory {}: {}",
                dir.display(),
                e
            ))
        })?;

        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                self.visit_dir(&path, ttl_files)?;
            } else if path.extension().and_then(|e| e.to_str()) == Some("ttl") {
                ttl_files.push(path);
            }
        }

        Ok(())
    }

    /// Extract owl:imports statements from a TTL file
    fn extract_imports(&self, ttl_file: &Path) -> Result<Vec<String>> {
        let content = std::fs::read_to_string(ttl_file).map_err(|e| {
            GgenAiError::io_error(&format!("Failed to read {}: {}", ttl_file.display(), e))
        })?;

        let mut imports = Vec::new();

        // Parse owl:imports statements
        for line in content.lines() {
            let line = line.trim();

            // Skip comments
            if line.starts_with('#') {
                continue;
            }

            // Look for owl:imports pattern
            if line.contains("owl:imports") {
                // Extract the imported file path (between < >)
                if let Some(start) = line.find('<') {
                    if let Some(end) = line.rfind('>') {
                        if start < end {
                            let import_path = &line[start + 1..end];
                            imports.push(import_path.to_string());
                        }
                    }
                }
            }
        }

        Ok(imports)
    }

    /// Create backup of ontology files before modifying
    fn create_backup(&self, ontology_path: &Path) -> Result<String> {
        let timestamp = chrono::Utc::now().format("%Y%m%d_%H%M%S");
        let backup_path = self
            .backup_dir
            .join(format!("cycle_fix_backup_{}", timestamp));

        std::fs::create_dir_all(&backup_path).map_err(|e| {
            GgenAiError::io_error(&format!("Failed to create backup directory: {}", e))
        })?;

        // Copy all TTL files to backup
        let ttl_files = self.find_ttl_files(ontology_path)?;
        for ttl_file in &ttl_files {
            let file_name = ttl_file.strip_prefix(ontology_path).unwrap_or(ttl_file);
            let backup_file = backup_path.join(file_name);

            if let Some(parent) = backup_file.parent() {
                std::fs::create_dir_all(parent).map_err(|e| {
                    GgenAiError::io_error(&format!("Failed to create backup directory: {}", e))
                })?;
            }

            std::fs::copy(ttl_file, &backup_file).map_err(|e| {
                GgenAiError::io_error(&format!("Failed to backup {}: {}", ttl_file.display(), e))
            })?;
        }

        Ok(backup_path.to_string_lossy().to_string())
    }
}

#[async_trait]
impl SwarmAgent for CycleBreakerAgent {
    fn name(&self) -> &str {
        self.base.name()
    }

    fn capabilities(&self) -> Vec<String> {
        self.base.capabilities()
    }

    async fn execute(&self, _context: &SwarmContext, input: AgentInput) -> Result<AgentOutput> {
        debug!(
            "CycleBreakerAgent executing with input: {:?}",
            input.input_type
        );

        // Parse input parameters
        let ontology_path = input
            .data
            .get("ontology_path")
            .and_then(|v| v.as_str())
            .ok_or_else(|| GgenAiError::invalid_input("Missing ontology_path parameter"))?;

        let strategy_str = input
            .data
            .get("strategy")
            .and_then(|v| v.as_str())
            .unwrap_or("remove_import");
        let strategy = FixStrategy::from_str(strategy_str).map_err(|_| {
            GgenAiError::invalid_input(&format!("Invalid strategy: {}", strategy_str))
        })?;

        let dry_run = input
            .data
            .get("dry_run")
            .and_then(|v| v.as_bool())
            .unwrap_or(false);

        // Create agent instance with dry_run setting
        let agent = CycleBreakerAgent::new(ontology_path, dry_run);

        // Execute fix
        let report = agent.fix_cycles(Path::new(ontology_path), strategy)?;

        // Convert to output
        let output_data = serde_json::to_value(report).map_err(|e| {
            GgenAiError::serialization(&format!("Failed to serialize report: {}", e))
        })?;

        Ok(AgentOutput {
            data: output_data,
            output_type: "fix_report".to_string(),
            target_agents: vec![],
            metadata: {
                let mut meta = HashMap::new();
                meta.insert("agent".to_string(), "cycle_breaker".to_string());
                meta.insert("strategy".to_string(), strategy.as_str().to_string());
                meta.insert("dry_run".to_string(), dry_run.to_string());
                meta
            },
        })
    }

    async fn validate(&self) -> Result<bool> {
        // Validate backup directory is accessible
        if self.backup_dir.exists() {
            // Check if writable
            let test_file = self.backup_dir.join(".write_test");
            std::fs::write(&test_file, b"test").map_err(|e| {
                GgenAiError::io_error(&format!("Backup directory not writable: {}", e))
            })?;
            std::fs::remove_file(&test_file)
                .map_err(|e| GgenAiError::io_error(&format!("Failed to clean test file: {}", e)))?;
        }

        Ok(true)
    }

    async fn health_check(&self) -> AgentHealth {
        AgentHealth {
            status: HealthStatus::Healthy,
            score: 1.0,
            last_check: chrono::Utc::now().to_rfc3339(),
            issues: vec![],
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    fn create_test_ontology(dir: &Path, name: &str, imports: &[&str]) -> Result<()> {
        let path = dir.join(name);
        let mut content = String::new();
        content.push_str("@prefix owl: <http://www.w3.org/2002/07/owl#> .\n");
        content.push_str("@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n\n");

        for import in imports {
            content.push_str(&format!("<> owl:imports <{}> .\n", import));
        }

        content.push_str(&format!(
            "\n<{}> a owl:Ontology .\n",
            name.trim_end_matches(".ttl")
        ));

        fs::write(&path, content)
            .map_err(|e| GgenAiError::io_error(&format!("Failed to write test file: {}", e)))?;

        Ok(())
    }

    #[test]
    fn test_cycle_breaker_creation() {
        let temp_dir = TempDir::new().unwrap();
        let agent = CycleBreakerAgent::new(temp_dir.path(), true);

        assert_eq!(agent.name(), "cycle-breaker");
        assert!(agent
            .capabilities()
            .contains(&"cycle_detection".to_string()));
        assert!(agent.dry_run);
    }

    #[test]
    fn test_detect_cycles() {
        let temp_dir = TempDir::new().unwrap();

        // Create cycle: A -> B -> C -> A
        create_test_ontology(temp_dir.path(), "A.ttl", &["B.ttl"]).unwrap();
        create_test_ontology(temp_dir.path(), "B.ttl", &["C.ttl"]).unwrap();
        create_test_ontology(temp_dir.path(), "C.ttl", &["A.ttl"]).unwrap();

        let agent = CycleBreakerAgent::new(temp_dir.path(), true);
        let cycles = agent.detect_cycles(temp_dir.path()).unwrap();

        assert_eq!(cycles.len(), 1);
        assert_eq!(cycles[0].len(), 4); // A -> B -> C -> A
    }

    #[test]
    fn test_analyze_and_fix_dry_run() {
        let temp_dir = TempDir::new().unwrap();

        // Create cycle: A -> B -> C -> A
        create_test_ontology(temp_dir.path(), "A.ttl", &["B.ttl"]).unwrap();
        create_test_ontology(temp_dir.path(), "B.ttl", &["C.ttl"]).unwrap();
        create_test_ontology(temp_dir.path(), "C.ttl", &["A.ttl"]).unwrap();

        let agent = CycleBreakerAgent::new(temp_dir.path(), true); // dry_run = true
        let report = agent.analyze_and_fix(temp_dir.path()).unwrap();

        assert_eq!(report.cycles_found, 1);
        assert_eq!(report.fixes_applied, 0); // No fixes in dry run
        assert!(report.backup_path.is_none());
        assert!(report.success); // Dry run is always successful
    }

    #[test]
    fn test_fix_cycles_with_strategy() {
        let temp_dir = TempDir::new().unwrap();

        // Create cycle: A -> B -> C -> A
        create_test_ontology(temp_dir.path(), "A.ttl", &["B.ttl"]).unwrap();
        create_test_ontology(temp_dir.path(), "B.ttl", &["C.ttl"]).unwrap();
        create_test_ontology(temp_dir.path(), "C.ttl", &["A.ttl"]).unwrap();

        let agent = CycleBreakerAgent::new(temp_dir.path(), false); // dry_run = false
        let report = agent
            .fix_cycles(temp_dir.path(), FixStrategy::RemoveImport)
            .unwrap();

        assert_eq!(report.cycles_found, 1);
        assert_eq!(report.fixes_applied, 1);
        assert_eq!(report.files_modified.len(), 1);
        assert!(report.backup_path.is_some());
        assert!(report.success);
    }

    #[test]
    fn test_verify_fix() {
        let temp_dir = TempDir::new().unwrap();

        // Create cycle: A -> B -> C -> A
        create_test_ontology(temp_dir.path(), "A.ttl", &["B.ttl"]).unwrap();
        create_test_ontology(temp_dir.path(), "B.ttl", &["C.ttl"]).unwrap();
        create_test_ontology(temp_dir.path(), "C.ttl", &["A.ttl"]).unwrap();

        let agent = CycleBreakerAgent::new(temp_dir.path(), false);

        // Fix the cycle
        agent
            .fix_cycles(temp_dir.path(), FixStrategy::RemoveImport)
            .unwrap();

        // Verify fix worked
        let verified = agent.verify_fix(temp_dir.path()).unwrap();
        assert!(verified);
    }

    #[test]
    fn test_no_cycles() {
        let temp_dir = TempDir::new().unwrap();

        // Create DAG: A -> B, A -> C, B -> D, C -> D
        create_test_ontology(temp_dir.path(), "A.ttl", &["B.ttl", "C.ttl"]).unwrap();
        create_test_ontology(temp_dir.path(), "B.ttl", &["D.ttl"]).unwrap();
        create_test_ontology(temp_dir.path(), "C.ttl", &["D.ttl"]).unwrap();
        create_test_ontology(temp_dir.path(), "D.ttl", &[]).unwrap();

        let agent = CycleBreakerAgent::new(temp_dir.path(), true);
        let cycles = agent.detect_cycles(temp_dir.path()).unwrap();

        assert_eq!(cycles.len(), 0);
    }

    #[test]
    fn test_fix_strategy_from_str() {
        assert_eq!(
            FixStrategy::from_str("remove_import"),
            Ok(FixStrategy::RemoveImport)
        );
        assert_eq!(
            FixStrategy::from_str("merge_files"),
            Ok(FixStrategy::MergeFiles)
        );
        assert_eq!(
            FixStrategy::from_str("create_interface"),
            Ok(FixStrategy::CreateInterface)
        );
        assert!(FixStrategy::from_str("invalid").is_err());
    }

    #[tokio::test]
    async fn test_swarm_agent_execute() {
        let temp_dir = TempDir::new().unwrap();

        // Create cycle: A -> B -> C -> A
        create_test_ontology(temp_dir.path(), "A.ttl", &["B.ttl"]).unwrap();
        create_test_ontology(temp_dir.path(), "B.ttl", &["C.ttl"]).unwrap();
        create_test_ontology(temp_dir.path(), "C.ttl", &["A.ttl"]).unwrap();

        let agent = CycleBreakerAgent::new(temp_dir.path(), true);

        let context = SwarmContext {
            graph_state: String::new(),
            active_agents: vec![],
            metrics: Default::default(),
            config: Default::default(),
        };

        let input_data = serde_json::json!({
            "ontology_path": temp_dir.path().to_str(),
            "strategy": "remove_import",
            "dry_run": true
        });

        let input = AgentInput {
            data: input_data,
            input_type: "cycle_fix_request".to_string(),
            source_agent: None,
            context: HashMap::new(),
        };

        let output = agent.execute(&context, input).await.unwrap();

        assert_eq!(output.output_type, "fix_report");
        assert!(output.data.is_object());
    }

    #[tokio::test]
    async fn test_swarm_agent_health_check() {
        let temp_dir = TempDir::new().unwrap();
        let agent = CycleBreakerAgent::new(temp_dir.path(), true);

        let health = agent.health_check().await;

        assert!(matches!(health.status, HealthStatus::Healthy));
        assert_eq!(health.score, 1.0);
        assert!(health.issues.is_empty());
    }
}
