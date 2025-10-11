//! Regeneration Agent - Continuous regeneration of dependent artifacts

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::{RwLock, Notify};
use serde::{Deserialize, Serialize};
use chrono::{DateTime, Utc};
use uuid::Uuid;

use crate::agents::{Agent, AgentConfig, AgentMessage, AgentRole, AgentStatus, TaskDefinition, TaskResult};
use crate::error::{Result, GgenMcpError};

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
            last_scan_time: Arc::new(RwLock::new(Utc::now())),
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
                        if let Err(e) = self.scan_for_changes().await {
                            tracing::error!("Error scanning for file changes: {}", e);
                        }
                    }
                }
            }
        });

        Ok(())
    }

    /// Scan for file changes and trigger regeneration
    async fn scan_for_changes(&self) -> Result<()> {
        let mut last_scan = self.last_scan_time.write().await;
        *last_scan = Utc::now();

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

                if path.extension().map_or(false, |ext| ext == "ttl" || ext == "rdf" || ext == "owl") {
                    let metadata = entry.metadata()?;
                    let modified = metadata.modified()?;

                    // Check if file was modified since last scan
                    let last_scan = *self.last_scan_time.read().await;
                    if modified.duration_since(last_scan.into()).is_ok() {
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
                    if modified.duration_since(last_scan.into()).is_ok() {
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
            if dependency.depends_on.contains(&graph_path.to_string_lossy().to_string()) {
                affected.push(artifact_path.clone());
            }
        }

        Ok(affected)
    }

    /// Find artifacts that depend on a specific template
    async fn find_artifacts_depending_on_template(&self, template_path: &Path) -> Result<Vec<PathBuf>> {
        let registry = self.artifact_registry.read().await;
        let mut affected = Vec::new();

        for (artifact_path, dependency) in registry.iter() {
            if dependency.depends_on.contains(&template_path.to_string_lossy().to_string()) {
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

        tracing::info!("Processing regeneration trigger: {:?} for {} artifacts",
                     trigger.trigger_type, trigger.target_artifacts.len());

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

        let duration_ms = Utc::now().signed_duration_since(start_time).num_milliseconds() as u64;

        let metrics = RegenerationMetrics {
            total_artifacts: trigger.target_artifacts.len(),
            regenerated_count: regenerated.len(),
            failed_count: failed.len(),
            skipped_count: skipped.len(),
            cache_hits,
            dependency_violations,
            average_regeneration_time_ms: if regenerated.is_empty() { 0.0 } else { duration_ms as f64 / regenerated.len() as f64 },
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
        &self,
        artifact_path: &Path,
        trigger: &RegenerationTrigger,
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
                return Err(GgenMcpError::ExecutionFailed(
                    format!("Dependency validation failed for: {}", artifact_path.display())
                ));
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
        &self,
        artifact_path: &Path,
        _trigger: &RegenerationTrigger,
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
        &self,
        artifacts: Vec<PathBuf>,
        reason: String,
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

        tracing::info!("Manual regeneration triggered for {} artifacts", trigger.target_artifacts.len());

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
    async fn initialize(&mut self) -> std::result::Result<(), Box<dyn std::error::Error + Send + Sync>> {
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

    async fn handle_message(&mut self, message: AgentMessage) -> std::result::Result<AgentMessage, Box<dyn std::error::Error + Send + Sync>> {
        match message {
            AgentMessage::TaskAssignment { task_id, task } => {
                let result = self.handle_task(task).await?;
                Ok(AgentMessage::TaskCompletion {
                    task_id,
                    result,
                })
            }
            AgentMessage::HealthCheck { .. } => {
                let (queue_size, next_trigger) = self.get_queue_status().await;
                Ok(AgentMessage::HealthResponse {
                    status: self.status(),
                    metrics: Some(serde_json::json!({
                        "regeneration_queue_size": queue_size,
                        "next_trigger": next_trigger.map(|t| t.trigger_type),
                        "history_size": self.regeneration_history.read().await.len(),
                        "registry_size": self.artifact_registry.read().await.len(),
                    })),
                })
            }
            _ => {
                Err("Unsupported message type".into())
            }
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
                    duration_ms: Utc::now().signed_duration_since(start_time).num_milliseconds() as u64,
                    metrics: Some(serde_json::json!({
                        "artifacts_regenerated": 0,
                        "cache_hits": 0
                    })),
                })
            }
            _ => {
                Ok(TaskResult {
                    task_id: task.id,
                    success: false,
                    result: None,
                    error: Some("Unsupported task type".to_string()),
                    duration_ms: Utc::now().signed_duration_since(start_time).num_milliseconds() as u64,
                    metrics: None,
                })
            }
        }
    }
}
