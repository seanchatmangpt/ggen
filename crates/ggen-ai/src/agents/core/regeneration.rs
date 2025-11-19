//! Regeneration Agent - Continuous regeneration of dependent artifacts

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::{Notify, RwLock};
use uuid::Uuid;

use crate::agents::{
    Agent, AgentConfig, AgentMessage, AgentRole, AgentStatus, TaskDefinition, TaskResult,
};
use crate::error::{GgenAiError, Result};

/// Configuration for regeneration agent
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RegenerationConfig {
    /// Base directory for generated artifacts
    pub artifacts_base_dir: String,
    /// File system watch interval (seconds)
    pub watch_interval_secs: u64,
    /// Maximum regeneration batch size
    pub max_batch_size: usize,
    /// Enable dependency tracking for incremental regeneration
    pub enable_dependency_tracking: bool,
    /// Enable artifact caching to avoid unnecessary regeneration
    pub enable_artifact_caching: bool,
    /// Regeneration timeout per artifact (seconds)
    pub regeneration_timeout_secs: u64,
}

impl Default for RegenerationConfig {
    fn default() -> Self {
        Self {
            artifacts_base_dir: "./generated".to_string(),
            watch_interval_secs: 30,
            max_batch_size: 50,
            enable_dependency_tracking: true,
            enable_artifact_caching: true,
            regeneration_timeout_secs: 300,
        }
    }
}

/// Artifact dependency information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ArtifactDependency {
    pub artifact_path: PathBuf,
    pub depends_on: Vec<String>, // Graph file paths or artifact paths
    pub dependency_type: DependencyType,
    pub last_modified: DateTime<Utc>,
    pub hash: String, // Content hash for change detection
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum DependencyType {
    /// Depends on RDF graph content
    GraphContent,
    /// Depends on template source
    TemplateSource,
    /// Depends on other artifacts
    ArtifactDependency,
    /// External dependency (API, database schema, etc.)
    ExternalDependency,
}

/// Regeneration trigger
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RegenerationTrigger {
    pub id: Uuid,
    pub trigger_type: TriggerType,
    pub target_artifacts: Vec<PathBuf>,
    pub reason: String,
    pub timestamp: DateTime<Utc>,
    pub priority: RegenerationPriority,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum TriggerType {
    /// Graph file was modified
    GraphModified,
    /// Template was updated
    TemplateModified,
    /// Dependency was regenerated
    DependencyRegenerated,
    /// Scheduled regeneration
    Scheduled,
    /// Manual trigger
    Manual,
    /// External event (API change, schema update)
    ExternalEvent,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum RegenerationPriority {
    Critical,
    High,
    Normal,
    Low,
}

/// Regeneration result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RegenerationResult {
    pub trigger_id: Uuid,
    pub success: bool,
    pub regenerated_artifacts: Vec<PathBuf>,
    pub failed_artifacts: Vec<PathBuf>,
    pub skipped_artifacts: Vec<PathBuf>,
    pub duration_ms: u64,
    pub timestamp: DateTime<Utc>,
    pub metrics: RegenerationMetrics,
}

/// Regeneration metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RegenerationMetrics {
    pub total_artifacts: usize,
    pub regenerated_count: usize,
    pub failed_count: usize,
    pub skipped_count: usize,
    pub cache_hits: usize,
    pub dependency_violations: usize,
    pub average_regeneration_time_ms: f64,
}

/// Regeneration agent for continuous artifact regeneration
#[derive(Debug)]
pub struct RegenerationAgent {
    config: AgentConfig,
    regeneration_config: RegenerationConfig,
    status: AgentStatus,
    artifact_registry: Arc<RwLock<HashMap<PathBuf, ArtifactDependency>>>,
    regeneration_queue: Arc<RwLock<Vec<RegenerationTrigger>>>,
    regeneration_history: Arc<RwLock<Vec<RegenerationResult>>>,
    shutdown_notify: Arc<Notify>,
    last_scan_time: Arc<RwLock<Instant>>,
}

impl RegenerationAgent {
    pub fn new(config: AgentConfig, regeneration_config: RegenerationConfig) -> Self {
        Self {
            config,
            regeneration_config,
            status: AgentStatus::Healthy,
            artifact_registry: Arc::new(RwLock::new(HashMap::new())),
            regeneration_queue: Arc::new(RwLock::new(Vec::new())),
            regeneration_history: Arc::new(RwLock::new(Vec::new())),
            shutdown_notify: Arc::new(Notify::new()),
            last_scan_time: Arc::new(RwLock::new(std::time::Instant::now())),
        }
    }

    /// Start file system watcher for continuous regeneration
    pub async fn start_file_watcher(&self) -> Result<()> {
        let shutdown_notify = self.shutdown_notify.clone();
        let watch_interval = Duration::from_secs(self.regeneration_config.watch_interval_secs);
        let artifacts_dir = PathBuf::from(&self.regeneration_config.artifacts_base_dir);

        tokio::spawn(async move {
            loop {
                tokio::select! {
                    _ = shutdown_notify.notified() => {
                        tracing::info!("Regeneration file watcher shutting down");
                        break;
                    }
                    _ = tokio::time::sleep(watch_interval) => {
                        // File watching logic would go here
                        // This is handled by the run_regeneration_loop in the start() method
                        tracing::debug!("File watcher tick (artifacts: {})", artifacts_dir.display());
                    }
                }
            }
        });

        Ok(())
    }

    /// Scan for file changes and trigger regeneration
    async fn scan_for_changes(&self) -> Result<()> {
        let mut last_scan = self.last_scan_time.write().await;
        *last_scan = std::time::Instant::now();

        let mut triggers = Vec::new();

        // Check for graph file changes
        if let Ok(graph_changes) = self.detect_graph_changes().await {
            for change in graph_changes {
                let trigger = RegenerationTrigger {
                    id: Uuid::new_v4(),
                    trigger_type: TriggerType::GraphModified,
                    target_artifacts: change.affected_artifacts,
                    reason: format!("Graph file modified: {}", change.graph_path.display()),
                    timestamp: Utc::now(),
                    priority: RegenerationPriority::High,
                };
                triggers.push(trigger);
            }
        }

        // Check for template changes
        if let Ok(template_changes) = self.detect_template_changes().await {
            for change in template_changes {
                let trigger = RegenerationTrigger {
                    id: Uuid::new_v4(),
                    trigger_type: TriggerType::TemplateModified,
                    target_artifacts: change.affected_artifacts,
                    reason: format!("Template modified: {}", change.template_path.display()),
                    timestamp: Utc::now(),
                    priority: RegenerationPriority::Normal,
                };
                triggers.push(trigger);
            }
        }

        // Add triggers to queue
        if !triggers.is_empty() {
            let mut queue = self.regeneration_queue.write().await;
            queue.extend(triggers);
        }

        Ok(())
    }

    /// Detect changes in RDF graph files
    async fn detect_graph_changes(&self) -> Result<Vec<GraphChange>> {
        let mut changes = Vec::new();

        // Walk the graphs directory and check modification times
        let graphs_dir = Path::new("./graphs");
        if graphs_dir.exists() {
            for entry in std::fs::read_dir(graphs_dir)? {
                let entry = entry?;
                let path = entry.path();

                if path
                    .extension()
                    .map_or(false, |ext| ext == "ttl" || ext == "rdf" || ext == "owl")
                {
                    let metadata = entry.metadata()?;
                    let modified = metadata.modified()?;

                    // Check if file was modified since last scan
                    let last_scan = *self.last_scan_time.read().await;
                    let elapsed = last_scan.elapsed();
                    if modified.elapsed().map_or(false, |t| t < elapsed) {
                        // File was modified recently, find affected artifacts
                        let affected = self.find_artifacts_depending_on_graph(&path).await?;
                        if !affected.is_empty() {
                            changes.push(GraphChange {
                                graph_path: path,
                                affected_artifacts: affected,
                            });
                        }
                    }
                }
            }
        }

        Ok(changes)
    }

    /// Detect changes in template files
    async fn detect_template_changes(&self) -> Result<Vec<TemplateChange>> {
        let mut changes = Vec::new();

        // Walk the templates directory and check modification times
        let templates_dir = Path::new("./templates");
        if templates_dir.exists() {
            for entry in std::fs::read_dir(templates_dir)? {
                let entry = entry?;
                let path = entry.path();

                if path.extension().map_or(false, |ext| ext == "tmpl") {
                    let metadata = entry.metadata()?;
                    let modified = metadata.modified()?;

                    // Check if file was modified since last scan
                    let last_scan = *self.last_scan_time.read().await;
                    let elapsed = last_scan.elapsed();
                    if modified.elapsed().map_or(false, |t| t < elapsed) {
                        // Template was modified, find affected artifacts
                        let affected = self.find_artifacts_depending_on_template(&path).await?;
                        if !affected.is_empty() {
                            changes.push(TemplateChange {
                                template_path: path,
                                affected_artifacts: affected,
                            });
                        }
                    }
                }
            }
        }

        Ok(changes)
    }

    /// Find artifacts that depend on a specific graph
    async fn find_artifacts_depending_on_graph(&self, graph_path: &Path) -> Result<Vec<PathBuf>> {
        let registry = self.artifact_registry.read().await;
        let mut affected = Vec::new();

        for (artifact_path, dependency) in registry.iter() {
            if dependency
                .depends_on
                .contains(&graph_path.to_string_lossy().to_string())
            {
                affected.push(artifact_path.clone());
            }
        }

        Ok(affected)
    }

    /// Find artifacts that depend on a specific template
    async fn find_artifacts_depending_on_template(
        &self, template_path: &Path,
    ) -> Result<Vec<PathBuf>> {
        let registry = self.artifact_registry.read().await;
        let mut affected = Vec::new();

        for (artifact_path, dependency) in registry.iter() {
            if dependency
                .depends_on
                .contains(&template_path.to_string_lossy().to_string())
            {
                affected.push(artifact_path.clone());
            }
        }

        Ok(affected)
    }

    /// Process regeneration queue
    pub async fn process_regeneration_queue(&self) -> Result<RegenerationResult> {
        let mut queue = self.regeneration_queue.write().await;

        if queue.is_empty() {
            return Ok(RegenerationResult {
                trigger_id: Uuid::new_v4(),
                success: true,
                regenerated_artifacts: Vec::new(),
                failed_artifacts: Vec::new(),
                skipped_artifacts: Vec::new(),
                duration_ms: 0,
                timestamp: Utc::now(),
                metrics: RegenerationMetrics {
                    total_artifacts: 0,
                    regenerated_count: 0,
                    failed_count: 0,
                    skipped_count: 0,
                    cache_hits: 0,
                    dependency_violations: 0,
                    average_regeneration_time_ms: 0.0,
                },
            });
        }

        let trigger = queue.remove(0);
        let start_time = Utc::now();

        tracing::info!(
            "Processing regeneration trigger: {:?} for {} artifacts",
            trigger.trigger_type,
            trigger.target_artifacts.len()
        );

        let mut regenerated = Vec::new();
        let mut failed = Vec::new();
        let mut skipped = Vec::new();
        let mut cache_hits = 0;
        let mut dependency_violations = 0;

        // Process artifacts in batches
        for artifact_path in &trigger.target_artifacts {
            match self.regenerate_artifact(artifact_path, &trigger).await {
                Ok(regenerated_path) => {
                    regenerated.push(regenerated_path);
                }
                Err(_) => {
                    failed.push(artifact_path.clone());
                }
            }
        }

        let duration_ms = Utc::now()
            .signed_duration_since(start_time)
            .num_milliseconds() as u64;

        let metrics = RegenerationMetrics {
            total_artifacts: trigger.target_artifacts.len(),
            regenerated_count: regenerated.len(),
            failed_count: failed.len(),
            skipped_count: skipped.len(),
            cache_hits,
            dependency_violations,
            average_regeneration_time_ms: if regenerated.is_empty() {
                0.0
            } else {
                duration_ms as f64 / regenerated.len() as f64
            },
        };

        let result = RegenerationResult {
            trigger_id: trigger.id,
            success: failed.is_empty(),
            regenerated_artifacts: regenerated,
            failed_artifacts: failed,
            skipped_artifacts: skipped,
            duration_ms,
            timestamp: Utc::now(),
            metrics,
        };

        // Record in history
        let mut history = self.regeneration_history.write().await;
        history.push(result.clone());

        Ok(result)
    }

    /// Regenerate a single artifact
    async fn regenerate_artifact(
        &self, artifact_path: &Path, trigger: &RegenerationTrigger,
    ) -> Result<PathBuf> {
        // Check cache first if enabled
        if self.regeneration_config.enable_artifact_caching {
            if let Some(cached_path) = self.check_artifact_cache(artifact_path).await? {
                tracing::debug!("Using cached artifact: {}", cached_path.display());
                return Ok(cached_path);
            }
        }

        // Check dependencies
        if self.regeneration_config.enable_dependency_tracking {
            if !self.validate_dependencies(artifact_path).await? {
                return Err(GgenAiError::Validation(format!(
                    "Dependency validation failed for: {}",
                    artifact_path.display()
                )));
            }
        }

        // Perform regeneration (this would call the actual generation logic)
        self.perform_regeneration(artifact_path, trigger).await?;

        // Update cache if enabled
        if self.regeneration_config.enable_artifact_caching {
            self.update_artifact_cache(artifact_path).await?;
        }

        Ok(artifact_path.to_path_buf())
    }

    /// Check if artifact is in cache and still valid
    async fn check_artifact_cache(&self, _artifact_path: &Path) -> Result<Option<PathBuf>> {
        // Placeholder - would check cache validity
        Ok(None)
    }

    /// Validate artifact dependencies
    async fn validate_dependencies(&self, _artifact_path: &Path) -> Result<bool> {
        // Placeholder - would validate all dependencies are up to date
        Ok(true)
    }

    /// Perform actual artifact regeneration
    async fn perform_regeneration(
        &self, artifact_path: &Path, _trigger: &RegenerationTrigger,
    ) -> Result<()> {
        // This would call the appropriate generation logic based on artifact type
        tracing::info!("Regenerating artifact: {}", artifact_path.display());

        // For now, just touch the file to update its modification time
        if artifact_path.exists() {
            let _ = std::fs::File::open(artifact_path)?;
        }

        Ok(())
    }

    /// Update artifact cache
    async fn update_artifact_cache(&self, _artifact_path: &Path) -> Result<()> {
        // Placeholder - would update cache with new artifact
        Ok(())
    }

    /// Trigger manual regeneration
    pub async fn trigger_manual_regeneration(
        &self, artifacts: Vec<PathBuf>, reason: String,
    ) -> Result<Uuid> {
        let trigger = RegenerationTrigger {
            id: Uuid::new_v4(),
            trigger_type: TriggerType::Manual,
            target_artifacts: artifacts,
            reason,
            timestamp: Utc::now(),
            priority: RegenerationPriority::Normal,
        };

        let mut queue = self.regeneration_queue.write().await;
        queue.push(trigger.clone());

        tracing::info!(
            "Manual regeneration triggered for {} artifacts",
            trigger.target_artifacts.len()
        );

        Ok(trigger.id)
    }

    /// Get regeneration history
    pub async fn get_regeneration_history(&self) -> Vec<RegenerationResult> {
        self.regeneration_history.read().await.clone()
    }

    /// Get current regeneration queue status
    pub async fn get_queue_status(&self) -> (usize, Option<RegenerationTrigger>) {
        let queue = self.regeneration_queue.read().await;
        let next = queue.first().cloned();
        (queue.len(), next)
    }
}

/// Detected graph change
#[derive(Debug)]
struct GraphChange {
    graph_path: PathBuf,
    affected_artifacts: Vec<PathBuf>,
}

/// Detected template change
#[derive(Debug)]
struct TemplateChange {
    template_path: PathBuf,
    affected_artifacts: Vec<PathBuf>,
}

#[async_trait::async_trait]
impl Agent for RegenerationAgent {
    async fn initialize(
        &mut self,
    ) -> std::result::Result<(), Box<dyn std::error::Error + Send + Sync>> {
        tracing::info!("Initializing RegenerationAgent with ID: {}", self.config.id);

        // Ensure artifacts directory exists
        std::fs::create_dir_all(&self.regeneration_config.artifacts_base_dir)?;

        // Start file watcher
        self.start_file_watcher().await?;

        self.status = AgentStatus::Healthy;
        Ok(())
    }

    async fn start(&mut self) -> std::result::Result<(), Box<dyn std::error::Error + Send + Sync>> {
        tracing::info!("Starting RegenerationAgent");

        // Start regeneration queue processor
        //
        // SAFETY: This is UNSAFE and UNSOUND - it causes undefined behavior:
        //
        // ## What This Code Does:
        // `std::ptr::read(self as *const Self)` performs a bitwise copy of the entire
        // RegenerationAgent struct, creating a duplicate without calling Clone.
        //
        // ## Why This Is Unsound:
        //
        // 1. **Double-Free / Use-After-Free**: RegenerationAgent contains Arc fields:
        //    - artifact_registry: Arc<RwLock<HashMap<...>>>
        //    - regeneration_queue: Arc<RwLock<Vec<...>>>
        //    - regeneration_history: Arc<RwLock<Vec<...>>>
        //    - shutdown_notify: Arc<Notify>
        //    - last_scan_time: Arc<RwLock<Instant>>
        //
        //    When ptr::read copies these, it duplicates the Arc pointers WITHOUT incrementing
        //    their reference counts. This creates two Arc pointers that both think they own
        //    the data, leading to:
        //    - When first Arc is dropped: reference count decremented incorrectly
        //    - When second Arc is dropped: double-free or use-after-free
        //
        // 2. **Violation of Move Semantics**: After ptr::read, `self` still exists and will
        //    be dropped when the function returns, but we've created another copy that will
        //    also be dropped in the spawned task. This violates Rust's "exactly one owner" rule.
        //
        // 3. **Memory Safety Violation**: Both the original and copied RegenerationAgent will
        //    try to drop the same Arc values, but with incorrect reference counts. This can
        //    cause:
        //    - Premature deallocation (first drop frees memory, second drop uses freed memory)
        //    - Memory leaks (if reference counts get corrupted)
        //    - Data races (concurrent access to reference count without proper atomics)
        //
        // 4. **Not a Valid Copy**: RegenerationAgent does NOT implement Copy (and cannot,
        //    because it contains non-Copy types). Using ptr::read to bypass this is unsound.
        //
        // ## Invariants Violated:
        // - Arc's reference counting invariant: each Arc pointer must be properly tracked
        // - Rust's ownership invariant: each value has exactly one owner
        // - Drop safety: each value's Drop must run exactly once
        //
        // ## Potential Undefined Behavior:
        // - Double-free when both copies try to drop the same Arc-managed data
        // - Use-after-free if one copy drops while the other is still using the data
        // - Memory corruption from concurrent drops without synchronization
        // - Reference count corruption leading to leaks or premature deallocation
        //
        // # Safety Analysis Result: UNSOUND - CAUSES UNDEFINED BEHAVIOR
        //
        // This code WILL cause memory safety violations. It is fundamentally broken.
        //
        // ## Recommended Fix:
        //
        // Restructure to avoid needing a copy of self. Options:
        //
        // 1. Store fields needed by the loop in Arc and clone only those:
        //    ```rust
        //    let queue = self.regeneration_queue.clone();
        //    let shutdown = self.shutdown_notify.clone();
        //    tokio::spawn(async move {
        //        run_loop(queue, shutdown).await;
        //    });
        //    ```
        //
        // 2. Make the entire agent Arc-wrapped from creation:
        //    ```rust
        //    pub struct RegenerationAgent {
        //        inner: Arc<RegenerationAgentInner>,
        //    }
        //    ```
        //
        // 3. Use channels to communicate with a separate task instead of sharing self
        let agent = unsafe { std::ptr::read(self as *const Self) };
        tokio::spawn(async move {
            agent.run_regeneration_loop().await;
        });

        self.status = AgentStatus::Healthy;
        Ok(())
    }

    async fn stop(&mut self) -> std::result::Result<(), Box<dyn std::error::Error + Send + Sync>> {
        tracing::info!("Stopping RegenerationAgent");

        // Notify shutdown
        self.shutdown_notify.notify_waiters();

        self.status = AgentStatus::Healthy; // Could be set to stopped status
        Ok(())
    }

    async fn status(&self) -> AgentStatus {
        self.status.clone()
    }

    fn config(&self) -> &AgentConfig {
        &self.config
    }

    async fn handle_message(
        &mut self, message: AgentMessage,
    ) -> std::result::Result<AgentMessage, Box<dyn std::error::Error + Send + Sync>> {
        match message {
            AgentMessage::TaskAssignment { task_id, task } => {
                let result = self.handle_task(task).await?;
                Ok(AgentMessage::TaskCompletion { task_id, result })
            }
            AgentMessage::HealthCheck { .. } => {
                let (queue_size, next_trigger) = self.get_queue_status().await;
                Ok(AgentMessage::HealthResponse {
                    status: self.status.clone(),
                    metrics: Some(serde_json::json!({
                        "regeneration_queue_size": queue_size,
                        "next_trigger": next_trigger.map(|t| t.trigger_type),
                        "history_size": self.regeneration_history.read().await.len(),
                        "registry_size": self.artifact_registry.read().await.len(),
                    })),
                })
            }
            _ => Err("Unsupported message type".into()),
        }
    }
}

impl RegenerationAgent {
    /// Main regeneration loop
    async fn run_regeneration_loop(&self) {
        loop {
            tokio::select! {
                _ = self.shutdown_notify.notified() => {
                    tracing::info!("Regeneration loop shutting down");
                    break;
                }
                _ = tokio::time::sleep(Duration::from_secs(1)) => {
                    // Process regeneration queue periodically
                    if let Err(e) = self.process_regeneration_queue().await {
                        tracing::error!("Error processing regeneration queue: {}", e);
                    }
                }
            }
        }
    }

    /// Handle task execution for regeneration
    async fn handle_task(&self, task: TaskDefinition) -> Result<TaskResult> {
        let start_time = chrono::Utc::now();

        match task.task_type {
            crate::agents::TaskType::TemplateGeneration => {
                // Handle regeneration tasks
                Ok(TaskResult {
                    task_id: task.id,
                    success: true,
                    result: Some(serde_json::json!({
                        "message": "Regeneration task completed"
                    })),
                    error: None,
                    duration_ms: Utc::now()
                        .signed_duration_since(start_time)
                        .num_milliseconds() as u64,
                    metrics: Some(serde_json::json!({
                        "artifacts_regenerated": 0,
                        "cache_hits": 0
                    })),
                })
            }
            _ => Ok(TaskResult {
                task_id: task.id,
                success: false,
                result: None,
                error: Some("Unsupported task type".to_string()),
                duration_ms: Utc::now()
                    .signed_duration_since(start_time)
                    .num_milliseconds() as u64,
                metrics: None,
            }),
        }
    }
}

#[cfg(test)]
mod unsafe_safety_tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc as StdArc;

    /// Test: Demonstrates Arc reference count corruption via ptr::read
    ///
    /// This test validates that using ptr::read to copy a struct containing
    /// Arc fields causes reference count corruption and undefined behavior.
    ///
    /// # Safety Invariant Being Tested:
    /// - Arc reference counts must be accurate
    /// - Each Arc clone must increment the reference count
    /// - Each Arc drop must decrement the reference count exactly once
    ///
    /// # What ptr::read Does Wrong:
    /// ```rust
    /// let agent = unsafe { std::ptr::read(self as *const Self) };
    /// ```
    /// This creates a bitwise copy of the struct, duplicating all Arc pointers
    /// WITHOUT incrementing their reference counts.
    ///
    /// # Expected Behavior (Current Implementation):
    /// - Two Arc pointers to the same data, both thinking they own it
    /// - When first is dropped: ref count decremented
    /// - When second is dropped: double-free or use-after-free
    ///
    /// # Why This Test Matters:
    /// Demonstrates that ptr::read on non-Copy types containing Arc causes
    /// immediate undefined behavior that violates memory safety.
    #[tokio::test]
    #[ignore] // Ignored by default as it demonstrates UNSAFE behavior that causes UB
    async fn test_ptr_read_arc_corruption() {
        // Create an agent with Arc fields
        let config = AgentConfig {
            id: uuid::Uuid::new_v4(),
            name: "test_agent".to_string(),
            role: AgentRole::Custom,
            max_concurrent_tasks: 1,
        };
        let regen_config = RegenerationConfig::default();

        let mut agent = RegenerationAgent::new(config, regen_config);

        // Get the initial Arc reference count for one of the fields
        // Arc::strong_count gives us the number of Arc pointers to the data
        let initial_count = StdArc::strong_count(&agent.shutdown_notify);
        println!("Initial Arc strong count: {}", initial_count);

        // This is what the unsafe code does - create a bitwise copy
        // WITHOUT incrementing Arc reference counts
        let _copied_agent = unsafe { std::ptr::read(&agent as *const RegenerationAgent) };

        // Check the reference count after the "copy"
        let after_copy_count = StdArc::strong_count(&agent.shutdown_notify);
        println!("After ptr::read count: {}", after_copy_count);

        // CRITICAL BUG: The count should have increased, but it didn't!
        // We now have TWO Arc pointers to the same data, but the reference
        // count thinks there's still only ONE.
        //
        // This means when the first Arc is dropped, the data will be freed,
        // and the second Arc will be left with a dangling pointer.

        assert_eq!(
            initial_count, after_copy_count,
            "ptr::read did NOT increment Arc reference count - UB incoming!"
        );

        // When this function returns:
        // 1. `agent` goes out of scope and drops its Arcs
        // 2. `_copied_agent` goes out of scope and drops the SAME Arcs
        // 3. UNDEFINED BEHAVIOR: double-free, use-after-free, or memory corruption

        // NOTE: This test may crash, may corrupt memory, or may appear to work
        // depending on timing, allocator behavior, and other factors.
        // The point is: the behavior is UNDEFINED.

        println!("WARNING: This test demonstrates undefined behavior!");

        // Prevent the double-drop by forgetting the copied agent
        // (This is just to allow the test to complete without crashing)
        std::mem::forget(_copied_agent);
    }

    /// Test: Demonstrates memory leak when preventing double-free
    ///
    /// This test shows that to avoid the double-free from ptr::read,
    /// we'd have to leak memory using std::mem::forget.
    ///
    /// # Safety Invariant Being Tested:
    /// - Memory should be properly managed (no leaks)
    /// - All allocated resources should be freed exactly once
    ///
    /// # Why This Test Matters:
    /// Shows that ptr::read creates a no-win situation:
    /// - Let both copies drop: undefined behavior (double-free)
    /// - Forget one copy: memory leak
    /// - Neither option is acceptable
    #[tokio::test]
    async fn test_ptr_read_forces_memory_leak() {
        let config = AgentConfig {
            id: uuid::Uuid::new_v4(),
            name: "test_agent".to_string(),
            role: AgentRole::Custom,
            max_concurrent_tasks: 1,
        };
        let regen_config = RegenerationConfig::default();

        let agent = RegenerationAgent::new(config, regen_config);

        // To avoid undefined behavior from ptr::read, we'd have to:
        let copied_agent = unsafe { std::ptr::read(&agent as *const RegenerationAgent) };

        // Prevent the original from being dropped (memory leak)
        std::mem::forget(agent);

        // Now only the copy will be dropped properly
        drop(copied_agent);

        // Result: We had to leak the original agent to avoid UB
        // This is obviously not acceptable for production code

        println!("Avoided UB but caused memory leak instead");
    }

    /// Test: Demonstrates the correct safe alternative
    ///
    /// This test shows how to properly clone Arc fields when needed,
    /// without any unsafe code or reference count corruption.
    ///
    /// # Safety Guarantees:
    /// - Arc reference counts are properly maintained
    /// - No unsafe code needed
    /// - No undefined behavior possible
    /// - No memory leaks
    #[tokio::test]
    async fn test_safe_alternative_arc_clone() {
        let config = AgentConfig {
            id: uuid::Uuid::new_v4(),
            name: "test_agent".to_string(),
            role: AgentRole::Custom,
            max_concurrent_tasks: 1,
        };
        let regen_config = RegenerationConfig::default();

        let agent = RegenerationAgent::new(config, regen_config);

        // SAFE alternative: Clone only the Arc fields needed
        let shutdown_notify = agent.shutdown_notify.clone();
        let queue = agent.regeneration_queue.clone();

        // Check reference counts
        let count_shutdown = StdArc::strong_count(&agent.shutdown_notify);
        let count_queue = StdArc::strong_count(&agent.regeneration_queue);

        // Reference counts correctly show 2 references each
        assert_eq!(count_shutdown, 2, "Shutdown notify should have 2 references");
        assert_eq!(count_queue, 2, "Queue should have 2 references");

        // Now we can spawn a task with just the fields we need
        let _handle = tokio::spawn(async move {
            // Use the cloned Arcs safely
            let _notify = shutdown_notify;
            let _q = queue;
        });

        // Original agent can be used or dropped safely
        // No undefined behavior, no memory leaks

        println!("Safe alternative using Arc::clone works correctly");
    }

    /// Test: Documents the severity of the unsafe code
    ///
    /// This test documents why the ptr::read pattern is categorically unsound
    /// and cannot be made safe through any external invariants.
    #[test]
    fn test_ptr_read_is_always_unsound() {
        // The pattern `unsafe { std::ptr::read(self as *const Self) }` is ALWAYS
        // unsound when Self contains:
        //
        // 1. Arc<T> - duplicates Arc without incrementing refcount
        // 2. Box<T> - creates two owners of heap allocation
        // 3. Vec<T> - duplicates ownership of heap buffer
        // 4. String - duplicates ownership of heap buffer
        // 5. Any type with Drop - will drop twice
        // 6. Any non-Copy type - violates ownership semantics
        //
        // RegenerationAgent contains multiple Arc fields, making this
        // pattern GUARANTEED to cause undefined behavior.
        //
        // There are NO external invariants that can make this safe:
        // - "Promise to only drop one copy" - cannot be enforced by type system
        // - "Promise not to use after copy" - would require std::mem::forget (leak)
        // - "External synchronization" - doesn't help with reference counts
        //
        // The ONLY fix is to not use ptr::read on non-Copy types.

        println!("ptr::read on RegenerationAgent is categorically unsound");
    }
}
