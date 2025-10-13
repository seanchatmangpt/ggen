# Ggen Test Harness Specification

**Version**: 1.0
**Date**: 2025-10-13
**Strategic Architect**: Claude (Hive Mind)

## 1. Overview

This document specifies the implementation details for the ggen test harness using the cleanroom framework. The test harness provides a comprehensive, reusable testing infrastructure for the ggen CLI.

## 2. Architecture

### 2.1 Component Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                    GgenTestHarness                         │
│  ┌─────────────────┐  ┌─────────────────┐                 │
│  │  Cleanroom Env  │  │  Ggen Binary    │                 │
│  │  Management     │  │  Executor       │                 │
│  └─────────────────┘  └─────────────────┘                 │
│  ┌─────────────────┐  ┌─────────────────┐                 │
│  │  Workspace      │  │  Mock Services  │                 │
│  │  Manager        │  │  (Marketplace)  │                 │
│  └─────────────────┘  └─────────────────┘                 │
│  ┌─────────────────┐  ┌─────────────────┐                 │
│  │  Assertion      │  │  Metrics        │                 │
│  │  Engine         │  │  Collector      │                 │
│  └─────────────────┘  └─────────────────┘                 │
└─────────────────────────────────────────────────────────────┘
```

### 2.2 Module Structure

```rust
// cleanroom/tests/ggen/mod.rs
pub mod harness;          // Core test harness
pub mod workspace;        // Workspace management
pub mod assertions;       // Custom assertions
pub mod mocks;            // Mock services
pub mod fixtures;         // Test fixtures
pub mod helpers;          // Helper utilities
```

## 3. Core Test Harness Implementation

### 3.1 Main Harness Structure

```rust
// cleanroom/tests/ggen/harness.rs

use cleanroom::{
    CleanroomEnvironment, CleanroomConfig, scenario, Scenario,
    Policy, SecurityPolicy, ResourceLimits, Assert, Result,
};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::fs;
use anyhow::Context;

/// Main test harness for ggen CLI testing
pub struct GgenTestHarness {
    /// Cleanroom environment for isolated execution
    cleanroom: Arc<CleanroomEnvironment>,

    /// Path to ggen binary
    ggen_binary: PathBuf,

    /// Configuration
    config: HarnessConfig,

    /// Mock marketplace registry
    mock_marketplace: Option<MockMarketplaceRegistry>,

    /// Mock LLM service
    mock_llm: Option<MockLLMService>,

    /// Test metrics collector
    metrics: Arc<TestMetricsCollector>,

    /// Workspace manager
    workspace_manager: WorkspaceManager,
}

/// Harness configuration
#[derive(Debug, Clone)]
pub struct HarnessConfig {
    /// Enable network access
    pub enable_network: bool,

    /// Enable filesystem isolation
    pub enable_filesystem_isolation: bool,

    /// Maximum memory for tests (MB)
    pub max_memory_mb: u64,

    /// Maximum CPU percentage
    pub max_cpu_percent: f64,

    /// Test timeout
    pub timeout: Duration,

    /// Enable performance tracking
    pub track_performance: bool,

    /// Enable snapshot testing
    pub enable_snapshots: bool,
}

impl Default for HarnessConfig {
    fn default() -> Self {
        Self {
            enable_network: false,
            enable_filesystem_isolation: true,
            max_memory_mb: 2048,
            max_cpu_percent: 80.0,
            timeout: Duration::from_secs(300),
            track_performance: true,
            enable_snapshots: true,
        }
    }
}

impl GgenTestHarness {
    /// Create new test harness with default configuration
    pub async fn new() -> Result<Self> {
        Self::with_config(HarnessConfig::default()).await
    }

    /// Create test harness with custom configuration
    pub async fn with_config(config: HarnessConfig) -> Result<Self> {
        // Create cleanroom environment
        let cleanroom_config = CleanroomConfig {
            security: SecurityPolicy {
                enable_network_isolation: !config.enable_network,
                enable_filesystem_isolation: config.enable_filesystem_isolation,
                ..Default::default()
            },
            resources: ResourceLimits {
                max_memory_mb: config.max_memory_mb,
                max_cpu_percent: config.max_cpu_percent,
                timeout_seconds: config.timeout.as_secs() as u32,
                ..Default::default()
            },
            ..Default::default()
        };

        let cleanroom = Arc::new(CleanroomEnvironment::new(cleanroom_config).await?);

        // Find ggen binary
        let ggen_binary = Self::find_ggen_binary()
            .context("Failed to locate ggen binary")?;

        // Initialize components
        let metrics = Arc::new(TestMetricsCollector::new());
        let workspace_manager = WorkspaceManager::new()?;

        Ok(Self {
            cleanroom,
            ggen_binary,
            config,
            mock_marketplace: None,
            mock_llm: None,
            metrics,
            workspace_manager,
        })
    }

    /// Enable mock marketplace
    pub fn with_mock_marketplace(mut self) -> Self {
        self.mock_marketplace = Some(MockMarketplaceRegistry::new());
        self
    }

    /// Enable mock LLM service
    pub fn with_mock_llm(mut self) -> Self {
        self.mock_llm = Some(MockLLMService::new());
        self
    }

    /// Create isolated workspace for testing
    pub async fn create_workspace(&mut self, name: &str) -> Result<TestWorkspace> {
        self.workspace_manager.create_workspace(name).await
    }

    /// Execute ggen command
    pub async fn execute_command(
        &self,
        workspace: &TestWorkspace,
        args: &[&str],
    ) -> Result<GgenTestResult> {
        let start = Instant::now();

        // Build command
        let mut cmd_args = vec![self.ggen_binary.to_str().unwrap()];
        cmd_args.extend_from_slice(args);

        // Execute in cleanroom
        let result = self.cleanroom.execute_command_in_dir(
            workspace.path(),
            &cmd_args
        ).await?;

        let duration = start.elapsed();

        // Collect metrics
        if self.config.track_performance {
            self.metrics.record_execution(
                args.join(" "),
                duration,
                result.exit_code == 0,
            ).await;
        }

        Ok(GgenTestResult {
            exit_code: result.exit_code,
            stdout: result.stdout,
            stderr: result.stderr,
            duration,
            workspace: workspace.clone(),
            metrics: self.collect_execution_metrics().await?,
        })
    }

    /// Execute scenario
    pub async fn execute_scenario(
        &self,
        workspace: &TestWorkspace,
        scenario: &Scenario,
    ) -> Result<ScenarioTestResult> {
        let start = Instant::now();

        let result = self.cleanroom.execute_scenario_in_dir(
            workspace.path(),
            scenario
        ).await?;

        let duration = start.elapsed();

        Ok(ScenarioTestResult {
            scenario_name: scenario.name().to_string(),
            steps: result.steps,
            duration,
            workspace: workspace.clone(),
            success: result.steps.iter().all(|s| s.success),
        })
    }

    /// Take snapshot of workspace
    pub async fn snapshot_workspace(
        &self,
        workspace: &TestWorkspace,
        name: &str,
    ) -> Result<WorkspaceSnapshot> {
        if !self.config.enable_snapshots {
            return Err(anyhow::anyhow!("Snapshots are disabled"));
        }

        WorkspaceSnapshot::create(workspace, name).await
    }

    /// Compare workspace with snapshot
    pub async fn compare_snapshot(
        &self,
        workspace: &TestWorkspace,
        snapshot: &WorkspaceSnapshot,
    ) -> Result<SnapshotDiff> {
        snapshot.compare(workspace).await
    }

    /// Get test metrics
    pub async fn get_metrics(&self) -> TestMetrics {
        self.metrics.get_metrics().await
    }

    /// Cleanup resources
    pub async fn cleanup(&mut self) -> Result<()> {
        self.workspace_manager.cleanup_all().await?;
        Arc::get_mut(&mut self.cleanroom)
            .unwrap()
            .cleanup()
            .await?;
        Ok(())
    }

    /// Find ggen binary
    fn find_ggen_binary() -> Result<PathBuf> {
        // Try cargo build output first
        let cargo_target = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .join("target/debug/ggen");

        if cargo_target.exists() {
            return Ok(cargo_target);
        }

        // Try release build
        let release_target = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .join("target/release/ggen");

        if release_target.exists() {
            return Ok(release_target);
        }

        // Try installed binary
        which::which("ggen")
            .context("ggen binary not found. Run 'cargo build' first")
    }

    /// Collect execution metrics
    async fn collect_execution_metrics(&self) -> Result<ExecutionMetrics> {
        let cleanroom_metrics = self.cleanroom.get_metrics().await;

        Ok(ExecutionMetrics {
            cpu_usage_percent: cleanroom_metrics.resource_usage.cpu_usage_percent,
            memory_usage_bytes: cleanroom_metrics.resource_usage.memory_usage_bytes,
            duration_ms: cleanroom_metrics.total_duration_ms,
        })
    }
}
```

### 3.2 Test Result Types

```rust
// cleanroom/tests/ggen/harness.rs (continued)

/// Ggen test result with rich assertions
#[derive(Debug, Clone)]
pub struct GgenTestResult {
    pub exit_code: i32,
    pub stdout: String,
    pub stderr: String,
    pub duration: Duration,
    pub workspace: TestWorkspace,
    pub metrics: ExecutionMetrics,
}

impl GgenTestResult {
    /// Assert command succeeded
    pub fn assert_success(&self) -> &Self {
        assert_eq!(
            self.exit_code, 0,
            "Command failed with exit code {}\nstderr: {}\nstdout: {}",
            self.exit_code, self.stderr, self.stdout
        );
        self
    }

    /// Assert command failed
    pub fn assert_failure(&self) -> &Self {
        assert_ne!(
            self.exit_code, 0,
            "Command unexpectedly succeeded\nstdout: {}",
            self.stdout
        );
        self
    }

    /// Assert stdout contains text
    pub fn assert_stdout_contains(&self, text: &str) -> &Self {
        assert!(
            self.stdout.contains(text),
            "stdout does not contain '{}'\nstdout: {}",
            text, self.stdout
        );
        self
    }

    /// Assert stderr contains text
    pub fn assert_stderr_contains(&self, text: &str) -> &Self {
        assert!(
            self.stderr.contains(text),
            "stderr does not contain '{}'\nstderr: {}",
            text, self.stderr
        );
        self
    }

    /// Assert file exists
    pub fn assert_file_exists(&self, path: impl AsRef<Path>) -> &Self {
        let full_path = self.workspace.path().join(path);
        assert!(
            full_path.exists(),
            "File does not exist: {}",
            full_path.display()
        );
        self
    }

    /// Assert file contains text
    pub fn assert_file_contains(&self, path: impl AsRef<Path>, text: &str) -> &Self {
        let full_path = self.workspace.path().join(path);
        let content = std::fs::read_to_string(&full_path)
            .unwrap_or_else(|_| panic!("Failed to read file: {}", full_path.display()));

        assert!(
            content.contains(text),
            "File {} does not contain '{}'\nContent: {}",
            full_path.display(), text, content
        );
        self
    }

    /// Assert execution time under threshold
    pub fn assert_execution_time_under(&self, max: Duration) -> &Self {
        assert!(
            self.duration <= max,
            "Execution took {:?}, expected under {:?}",
            self.duration, max
        );
        self
    }

    /// Assert memory usage under threshold
    pub fn assert_memory_under(&self, max_mb: u64) -> &Self {
        let max_bytes = max_mb * 1024 * 1024;
        assert!(
            self.metrics.memory_usage_bytes <= max_bytes,
            "Memory usage {} bytes exceeded limit {} bytes ({} MB)",
            self.metrics.memory_usage_bytes, max_bytes, max_mb
        );
        self
    }

    /// Assert CPU usage under threshold
    pub fn assert_cpu_under(&self, max_percent: f64) -> &Self {
        assert!(
            self.metrics.cpu_usage_percent <= max_percent,
            "CPU usage {:.2}% exceeded limit {:.2}%",
            self.metrics.cpu_usage_percent, max_percent
        );
        self
    }
}

/// Scenario test result
#[derive(Debug, Clone)]
pub struct ScenarioTestResult {
    pub scenario_name: String,
    pub steps: Vec<StepResult>,
    pub duration: Duration,
    pub workspace: TestWorkspace,
    pub success: bool,
}

impl ScenarioTestResult {
    /// Assert all steps succeeded
    pub fn assert_all_steps_succeeded(&self) -> &Self {
        for step in &self.steps {
            assert!(
                step.success,
                "Step '{}' failed: {}",
                step.name, step.stderr
            );
        }
        self
    }

    /// Assert specific step succeeded
    pub fn assert_step_succeeded(&self, step_name: &str) -> &Self {
        let step = self.steps.iter()
            .find(|s| s.name == step_name)
            .unwrap_or_else(|| panic!("Step '{}' not found", step_name));

        assert!(
            step.success,
            "Step '{}' failed: {}",
            step_name, step.stderr
        );
        self
    }
}

/// Step result
#[derive(Debug, Clone)]
pub struct StepResult {
    pub name: String,
    pub success: bool,
    pub exit_code: i32,
    pub stdout: String,
    pub stderr: String,
    pub duration: Duration,
}

/// Execution metrics
#[derive(Debug, Clone)]
pub struct ExecutionMetrics {
    pub cpu_usage_percent: f64,
    pub memory_usage_bytes: u64,
    pub duration_ms: u64,
}
```

## 4. Workspace Management

### 4.1 Workspace Manager

```rust
// cleanroom/tests/ggen/workspace.rs

use anyhow::{Context, Result};
use std::path::{Path, PathBuf};
use tokio::fs;
use uuid::Uuid;

/// Workspace manager for test environments
pub struct WorkspaceManager {
    base_dir: PathBuf,
    workspaces: Vec<TestWorkspace>,
}

impl WorkspaceManager {
    /// Create new workspace manager
    pub fn new() -> Result<Self> {
        let base_dir = tempfile::tempdir()?.into_path();

        Ok(Self {
            base_dir,
            workspaces: Vec::new(),
        })
    }

    /// Create new workspace
    pub async fn create_workspace(&mut self, name: &str) -> Result<TestWorkspace> {
        let id = Uuid::new_v4();
        let path = self.base_dir.join(format!("{}-{}", name, id));

        fs::create_dir_all(&path).await
            .context("Failed to create workspace directory")?;

        let workspace = TestWorkspace {
            id,
            name: name.to_string(),
            path,
        };

        self.workspaces.push(workspace.clone());

        Ok(workspace)
    }

    /// Get workspace by name
    pub fn get_workspace(&self, name: &str) -> Option<&TestWorkspace> {
        self.workspaces.iter().find(|w| w.name == name)
    }

    /// Cleanup all workspaces
    pub async fn cleanup_all(&mut self) -> Result<()> {
        for workspace in &self.workspaces {
            if workspace.path.exists() {
                fs::remove_dir_all(&workspace.path).await
                    .context(format!("Failed to cleanup workspace: {}", workspace.name))?;
            }
        }
        self.workspaces.clear();
        Ok(())
    }
}

/// Test workspace
#[derive(Debug, Clone)]
pub struct TestWorkspace {
    pub id: Uuid,
    pub name: String,
    pub path: PathBuf,
}

impl TestWorkspace {
    /// Get workspace path
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// Create subdirectory
    pub async fn create_dir(&self, path: impl AsRef<Path>) -> Result<PathBuf> {
        let dir = self.path.join(path);
        fs::create_dir_all(&dir).await?;
        Ok(dir)
    }

    /// Write file
    pub async fn write_file(
        &self,
        path: impl AsRef<Path>,
        content: impl AsRef<[u8]>,
    ) -> Result<()> {
        let file_path = self.path.join(path);

        // Create parent directory if needed
        if let Some(parent) = file_path.parent() {
            fs::create_dir_all(parent).await?;
        }

        fs::write(&file_path, content).await?;
        Ok(())
    }

    /// Read file
    pub async fn read_file(&self, path: impl AsRef<Path>) -> Result<String> {
        let file_path = self.path.join(path);
        fs::read_to_string(&file_path).await
            .context(format!("Failed to read file: {}", file_path.display()))
    }

    /// File exists
    pub async fn file_exists(&self, path: impl AsRef<Path>) -> bool {
        self.path.join(path).exists()
    }
}
```

## 5. Mock Services

### 5.1 Mock Marketplace

```rust
// cleanroom/tests/ggen/mocks.rs

use std::collections::HashMap;
use serde::{Serialize, Deserialize};

/// Mock marketplace registry
pub struct MockMarketplaceRegistry {
    packages: HashMap<String, MockPackage>,
}

impl MockMarketplaceRegistry {
    /// Create new mock registry with default packages
    pub fn new() -> Self {
        let mut registry = Self {
            packages: HashMap::new(),
        };

        // Add default packages
        registry.add_package(MockPackage {
            name: "rust-axum-service".to_string(),
            version: "1.0.0".to_string(),
            description: "Axum web service template".to_string(),
            category: "web".to_string(),
            templates: vec![
                "user-service.tmpl".to_string(),
                "api-module.tmpl".to_string(),
            ],
        });

        registry.add_package(MockPackage {
            name: "postgresql-database".to_string(),
            version: "1.0.0".to_string(),
            description: "PostgreSQL database setup".to_string(),
            category: "database".to_string(),
            templates: vec![
                "schema.sql".to_string(),
                "migrations.tmpl".to_string(),
            ],
        });

        registry.add_package(MockPackage {
            name: "docker-compose".to_string(),
            version: "1.0.0".to_string(),
            description: "Docker Compose configuration".to_string(),
            category: "infrastructure".to_string(),
            templates: vec![
                "docker-compose.yml".to_string(),
            ],
        });

        registry
    }

    /// Add package to registry
    pub fn add_package(&mut self, package: MockPackage) {
        self.packages.insert(package.name.clone(), package);
    }

    /// Search packages
    pub fn search(&self, query: &str) -> Vec<&MockPackage> {
        let query_lower = query.to_lowercase();

        self.packages.values()
            .filter(|pkg| {
                pkg.name.to_lowercase().contains(&query_lower) ||
                pkg.description.to_lowercase().contains(&query_lower) ||
                pkg.category.to_lowercase().contains(&query_lower)
            })
            .collect()
    }

    /// Get package by name
    pub fn get_package(&self, name: &str) -> Option<&MockPackage> {
        self.packages.get(name)
    }

    /// List all packages
    pub fn list_packages(&self) -> Vec<&MockPackage> {
        self.packages.values().collect()
    }
}

/// Mock package
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MockPackage {
    pub name: String,
    pub version: String,
    pub description: String,
    pub category: String,
    pub templates: Vec<String>,
}
```

### 5.2 Mock LLM Service

```rust
// cleanroom/tests/ggen/mocks.rs (continued)

use std::collections::HashMap;

/// Mock LLM service for AI command testing
pub struct MockLLMService {
    responses: HashMap<String, String>,
    default_response: String,
}

impl MockLLMService {
    /// Create new mock LLM service
    pub fn new() -> Self {
        Self {
            responses: HashMap::new(),
            default_response: "// Generated code\npub struct GeneratedCode {}".to_string(),
        }
    }

    /// Set response for specific prompt
    pub fn set_response(&mut self, prompt: &str, response: impl Into<String>) {
        self.responses.insert(prompt.to_string(), response.into());
    }

    /// Get response for prompt
    pub fn get_response(&self, prompt: &str) -> String {
        self.responses.get(prompt)
            .cloned()
            .unwrap_or_else(|| self.default_response.clone())
    }

    /// Set default response
    pub fn set_default_response(&mut self, response: impl Into<String>) {
        self.default_response = response.into();
    }
}
```

## 6. Test Metrics Collection

```rust
// cleanroom/tests/ggen/metrics.rs

use std::sync::Arc;
use tokio::sync::RwLock;
use std::time::Duration;
use std::collections::HashMap;

/// Test metrics collector
pub struct TestMetricsCollector {
    metrics: Arc<RwLock<TestMetrics>>,
}

impl TestMetricsCollector {
    /// Create new metrics collector
    pub fn new() -> Self {
        Self {
            metrics: Arc::new(RwLock::new(TestMetrics::default())),
        }
    }

    /// Record command execution
    pub async fn record_execution(
        &self,
        command: String,
        duration: Duration,
        success: bool,
    ) {
        let mut metrics = self.metrics.write().await;
        metrics.total_executions += 1;

        if success {
            metrics.successful_executions += 1;
        } else {
            metrics.failed_executions += 1;
        }

        // Update command-specific metrics
        let cmd_metrics = metrics.command_metrics
            .entry(command)
            .or_insert_with(CommandMetrics::default);

        cmd_metrics.executions += 1;
        cmd_metrics.total_duration += duration;
        cmd_metrics.min_duration = cmd_metrics.min_duration
            .map(|d| d.min(duration))
            .or(Some(duration));
        cmd_metrics.max_duration = cmd_metrics.max_duration
            .map(|d| d.max(duration))
            .or(Some(duration));
    }

    /// Get current metrics
    pub async fn get_metrics(&self) -> TestMetrics {
        self.metrics.read().await.clone()
    }
}

/// Test metrics
#[derive(Debug, Clone, Default)]
pub struct TestMetrics {
    pub total_executions: u64,
    pub successful_executions: u64,
    pub failed_executions: u64,
    pub command_metrics: HashMap<String, CommandMetrics>,
}

impl TestMetrics {
    /// Get success rate
    pub fn success_rate(&self) -> f64 {
        if self.total_executions == 0 {
            return 0.0;
        }
        (self.successful_executions as f64 / self.total_executions as f64) * 100.0
    }
}

/// Command-specific metrics
#[derive(Debug, Clone, Default)]
pub struct CommandMetrics {
    pub executions: u64,
    pub total_duration: Duration,
    pub min_duration: Option<Duration>,
    pub max_duration: Option<Duration>,
}

impl CommandMetrics {
    /// Get average duration
    pub fn avg_duration(&self) -> Duration {
        if self.executions == 0 {
            return Duration::from_secs(0);
        }
        self.total_duration / self.executions as u32
    }
}
```

## 7. Snapshot Testing

```rust
// cleanroom/tests/ggen/snapshots.rs

use anyhow::{Context, Result};
use std::path::{Path, PathBuf};
use tokio::fs;
use serde::{Serialize, Deserialize};

/// Workspace snapshot
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkspaceSnapshot {
    pub name: String,
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub files: HashMap<PathBuf, FileSnapshot>,
}

impl WorkspaceSnapshot {
    /// Create snapshot of workspace
    pub async fn create(
        workspace: &TestWorkspace,
        name: &str,
    ) -> Result<Self> {
        let mut files = HashMap::new();

        // Walk workspace directory
        let mut entries = fs::read_dir(workspace.path()).await?;
        while let Some(entry) = entries.next_entry().await? {
            let path = entry.path();
            if path.is_file() {
                let content = fs::read_to_string(&path).await?;
                let rel_path = path.strip_prefix(workspace.path())?;

                files.insert(
                    rel_path.to_path_buf(),
                    FileSnapshot {
                        content,
                        hash: Self::compute_hash(&content),
                    }
                );
            }
        }

        Ok(Self {
            name: name.to_string(),
            timestamp: chrono::Utc::now(),
            files,
        })
    }

    /// Compare with workspace
    pub async fn compare(&self, workspace: &TestWorkspace) -> Result<SnapshotDiff> {
        let current = Self::create(workspace, "current").await?;

        let mut added = Vec::new();
        let mut removed = Vec::new();
        let mut modified = Vec::new();
        let mut unchanged = Vec::new();

        // Find added and modified files
        for (path, file) in &current.files {
            match self.files.get(path) {
                Some(old_file) if old_file.hash == file.hash => {
                    unchanged.push(path.clone());
                }
                Some(_) => {
                    modified.push(path.clone());
                }
                None => {
                    added.push(path.clone());
                }
            }
        }

        // Find removed files
        for path in self.files.keys() {
            if !current.files.contains_key(path) {
                removed.push(path.clone());
            }
        }

        Ok(SnapshotDiff {
            added,
            removed,
            modified,
            unchanged,
        })
    }

    /// Compute hash of content
    fn compute_hash(content: &str) -> String {
        use sha2::{Sha256, Digest};
        let mut hasher = Sha256::new();
        hasher.update(content.as_bytes());
        format!("{:x}", hasher.finalize())
    }
}

/// File snapshot
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileSnapshot {
    pub content: String,
    pub hash: String,
}

/// Snapshot diff
#[derive(Debug, Clone)]
pub struct SnapshotDiff {
    pub added: Vec<PathBuf>,
    pub removed: Vec<PathBuf>,
    pub modified: Vec<PathBuf>,
    pub unchanged: Vec<PathBuf>,
}

impl SnapshotDiff {
    /// Check if snapshots are identical
    pub fn is_identical(&self) -> bool {
        self.added.is_empty() &&
        self.removed.is_empty() &&
        self.modified.is_empty()
    }
}
```

## 8. Usage Examples

### 8.1 Basic Test

```rust
#[tokio::test]
async fn test_market_search() -> Result<()> {
    let mut harness = GgenTestHarness::new().await?;
    let workspace = harness.create_workspace("market-search-test").await?;

    let result = harness.execute_command(
        &workspace,
        &["market", "search", "rust"]
    ).await?;

    result
        .assert_success()
        .assert_stdout_contains("rust-axum-service")
        .assert_execution_time_under(Duration::from_secs(3));

    harness.cleanup().await?;
    Ok(())
}
```

### 8.2 Scenario Test

```rust
#[tokio::test]
async fn test_marketplace_workflow() -> Result<()> {
    let mut harness = GgenTestHarness::new().await?;
    let workspace = harness.create_workspace("workflow-test").await?;

    let scenario = scenario("marketplace_workflow")
        .step("init", ["ggen", "lifecycle", "run", "init"])
        .step("search", ["ggen", "market", "search", "rust"])
        .step("add", ["ggen", "market", "add", "rust-axum-service"]);

    let result = harness.execute_scenario(&workspace, &scenario).await?;

    result
        .assert_all_steps_succeeded()
        .assert_step_succeeded("add");

    harness.cleanup().await?;
    Ok(())
}
```

### 8.3 Snapshot Test

```rust
#[tokio::test]
async fn test_template_generation_snapshot() -> Result<()> {
    let mut harness = GgenTestHarness::new().await?;
    let workspace = harness.create_workspace("snapshot-test").await?;

    // Generate template
    harness.execute_command(
        &workspace,
        &["template", "generate", "basic.tmpl"]
    ).await?;

    // Create snapshot
    let snapshot = harness.snapshot_workspace(&workspace, "after_generation").await?;

    // Regenerate
    harness.execute_command(
        &workspace,
        &["template", "generate", "basic.tmpl"]
    ).await?;

    // Compare
    let diff = harness.compare_snapshot(&workspace, &snapshot).await?;
    assert!(diff.is_identical(), "Template generation not deterministic");

    harness.cleanup().await?;
    Ok(())
}
```

---

**Document Status**: Ready for Implementation
**Next Steps**: Begin implementing core harness module
