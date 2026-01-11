//! Event system for the ultrathink swarm
//!
//! Handles event detection, routing, and processing for autonomous operations.

use crate::error::{GgenAiError, Result};
use crate::swarm::{SystemEvent, EventSource, EventStream};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::fs;
use tokio::sync::broadcast;

/// Event router for distributing events to appropriate agents
#[derive(Debug)]
pub struct EventRouter {
    /// Event sources
    event_sources: HashMap<String, Box<dyn EventSource>>,
    /// Event broadcaster for swarm communication
    event_broadcaster: broadcast::Sender<SystemEvent>,
    /// Event filters for different agent types
    event_filters: HashMap<String, Vec<EventFilter>>,
}

/// Event filter for routing events to specific agents
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EventFilter {
    /// Filter name
    pub name: String,
    /// Event types to match
    pub event_types: Vec<String>,
    /// Path patterns to match (for file events)
    pub path_patterns: Vec<String>,
    /// Target agent for this filter
    pub target_agent: String,
}

/// File system event source
#[derive(Debug)]
pub struct FileSystemEventSource {
    /// Paths to monitor
    watch_paths: Vec<PathBuf>,
    /// File extensions to monitor
    extensions: Vec<String>,
    /// Event buffer for batching
    event_buffer: Arc<std::sync::Mutex<Vec<SystemEvent>>>,
}

/// Git event source
#[derive(Debug)]
pub struct GitEventSource {
    /// Repository paths to monitor
    repo_paths: Vec<PathBuf>,
    /// Git event types to monitor
    event_types: Vec<GitEventType>,
}

/// API webhook event source
#[derive(Debug)]
pub struct ApiWebhookEventSource {
    /// Webhook endpoints
    endpoints: Vec<String>,
    /// Authentication tokens
    auth_tokens: HashMap<String, String>,
}

/// Runtime telemetry event source
#[derive(Debug)]
pub struct RuntimeTelemetryEventSource {
    /// Services to monitor
    services: Vec<String>,
    /// Metrics to collect
    metrics: Vec<String>,
}

/// Git event types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GitEventType {
    /// Commit events
    Commit,
    /// Branch creation/deletion
    Branch,
    /// Tag creation
    Tag,
    /// Pull request events
    PullRequest,
    /// Merge events
    Merge,
}

/// Event processing configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EventConfig {
    /// Enable event buffering
    pub enable_buffering: bool,
    /// Buffer size for events
    pub buffer_size: usize,
    /// Event processing interval (ms)
    pub processing_interval_ms: u64,
    /// Maximum events per batch
    pub max_events_per_batch: usize,
}

impl EventRouter {
    /// Create a new event router
    pub fn new() -> Self {
        let (event_broadcaster, _) = broadcast::channel(1000);

        Self {
            event_sources: HashMap::new(),
            event_broadcaster,
            event_filters: HashMap::new(),
        }
    }

    /// Add an event source
    pub fn add_event_source(&mut self, name: String, source: Box<dyn EventSource>) {
        self.event_sources.insert(name, source);
    }

    /// Add an event filter
    pub fn add_event_filter(&mut self, agent_name: String, filter: EventFilter) {
        self.event_filters.entry(agent_name).or_insert_with(Vec::new).push(filter);
    }

    /// Start event monitoring
    pub async fn start_monitoring(&self) -> Result<()> {
        // Start all event sources
        for (name, source) in &self.event_sources {
            // Clone the Arc<Box<dyn EventSource>> for safe sharing across the task boundary.
            // This ensures the EventSource lives for the entire duration of the monitoring task.
            let source_clone = source.clone();
            let event_tx = self.event_broadcaster.clone();
            let name_clone = name.clone();

            tokio::spawn(async move {
                if let Err(e) = Self::monitor_event_source(source_clone.as_ref(), event_tx).await {
                    log::error!("Error monitoring event source {}: {}", name_clone, e);
                }
            });
        }

        Ok(())
    }

    /// Monitor a single event source
    async fn monitor_event_source(
        source: &dyn EventSource,
        event_tx: broadcast::Sender<SystemEvent>,
    ) -> Result<()> {
        loop {
            match source.poll_events().await {
                Ok(events) => {
                    for event in events {
                        if let Err(e) = event_tx.send(event) {
                            log::error!("Failed to broadcast event: {}", e);
                        }
                    }
                }
                Err(e) => {
                    log::error!("Error polling events from {}: {}", source.name(), e);
                }
            }

            // Poll every 5 seconds
            tokio::time::sleep(std::time::Duration::from_secs(5)).await;
        }
    }

    /// Subscribe to events for a specific agent
    pub fn subscribe_for_agent(&self, agent_name: &str) -> broadcast::Receiver<SystemEvent> {
        self.event_broadcaster.subscribe()
    }

    /// Filter events for a specific agent
    pub fn filter_events_for_agent(&self, agent_name: &str, events: &[SystemEvent]) -> Vec<SystemEvent> {
        if let Some(filters) = self.event_filters.get(agent_name) {
            events.iter().filter(|event| self.event_matches_filters(event, filters)).cloned().collect()
        } else {
            events.to_vec()
        }
    }

    /// Check if an event matches any of the given filters
    fn event_matches_filters(&self, event: &SystemEvent, filters: &[EventFilter]) -> bool {
        filters.iter().any(|filter| self.event_matches_filter(event, filter))
    }

    /// Check if an event matches a specific filter
    fn event_matches_filter(&self, event: &SystemEvent, filter: &EventFilter) -> bool {
        // Check event type match
        let event_type_match = filter.event_types.is_empty() ||
            filter.event_types.iter().any(|event_type| {
                match (event_type.as_str(), event) {
                    ("filesystem", SystemEvent::FileSystem { .. }) => true,
                    ("git", SystemEvent::Git { .. }) => true,
                    ("api", SystemEvent::ApiWebhook { .. }) => true,
                    ("database", SystemEvent::Database { .. }) => true,
                    ("telemetry", SystemEvent::RuntimeTelemetry { .. }) => true,
                    ("requirement", SystemEvent::BusinessRequirement { .. }) => true,
                    _ => false,
                }
            });

        if !event_type_match {
            return false;
        }

        // Check path pattern match for file events
        if let SystemEvent::FileSystem { path, .. } = event {
            if !filter.path_patterns.is_empty() {
                return filter.path_patterns.iter().any(|pattern| {
                    path.to_string_lossy().contains(pattern)
                });
            }
        }

        true
    }
}

impl Default for EventRouter {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl EventSource for FileSystemEventSource {
    fn name(&self) -> &str {
        "filesystem"
    }

    async fn poll_events(&self) -> Result<Vec<SystemEvent>> {
        let mut events = Vec::new();

        for path in &self.watch_paths {
            if let Ok(mut dir_entries) = fs::read_dir(path).await {
                while let Ok(Some(entry)) = dir_entries.next_entry().await {
                    if let Ok(metadata) = entry.metadata().await {
                        let path_str = entry.path().to_string_lossy().to_string();

                        // Check if file extension matches
                        if let Some(extension) = entry.path().extension() {
                            if self.extensions.iter().any(|ext| ext == extension) {
                                let change_type = if metadata.is_file() {
                                    crate::swarm::FileChangeType::Modified
                                } else {
                                    crate::swarm::FileChangeType::Created
                                };

                                events.push(SystemEvent::FileSystem {
                                    path: path_str,
                                    change_type,
                                    content: None, // Would read file content if needed
                                });
                            }
                        }
                    }
                }
            }
        }

        Ok(events)
    }

    async fn subscribe(&self) -> Result<Box<dyn EventStream>> {
        // For now, return a simple polling-based stream
        Ok(Box::new(FileSystemEventStream {
            source: self.watch_paths.clone(),
            current_index: 0,
        }))
    }
}

/// File system event stream for continuous monitoring
pub struct FileSystemEventStream {
    source: Vec<PathBuf>,
    current_index: usize,
}

#[async_trait]
impl EventStream for FileSystemEventStream {
    async fn next_event(&mut self) -> Option<SystemEvent> {
        // Simple round-robin polling for demonstration
        if self.current_index >= self.source.len() {
            self.current_index = 0;
        }

        let path = &self.source[self.current_index];
        self.current_index += 1;

        if let Ok(mut dir_entries) = fs::read_dir(path).await {
            while let Ok(Some(entry)) = dir_entries.next_entry().await {
                if let Ok(metadata) = entry.metadata().await {
                    let path_str = entry.path().to_string_lossy().to_string();

                    if let Some(extension) = entry.path().extension() {
                        if extension == "rs" || extension == "toml" || extension == "md" {
                            let change_type = if metadata.is_file() {
                                crate::swarm::FileChangeType::Modified
                            } else {
                                crate::swarm::FileChangeType::Created
                            };

                            return Some(SystemEvent::FileSystem {
                                path: path_str,
                                change_type,
                                content: None,
                            });
                        }
                    }
                }
            }
        }

        None
    }
}

impl FileSystemEventSource {
    /// Create a new file system event source
    pub fn new(watch_paths: Vec<PathBuf>, extensions: Vec<String>) -> Self {
        Self {
            watch_paths,
            extensions,
            event_buffer: Arc::new(std::sync::Mutex::new(Vec::new())),
        }
    }
}

impl GitEventSource {
    /// Create a new git event source
    pub fn new(repo_paths: Vec<PathBuf>, event_types: Vec<GitEventType>) -> Self {
        Self {
            repo_paths,
            event_types,
        }
    }
}

#[async_trait]
impl EventSource for GitEventSource {
    fn name(&self) -> &str {
        "git"
    }

    async fn poll_events(&self) -> Result<Vec<SystemEvent>> {
        let mut events = Vec::new();

        for repo_path in &self.repo_paths {
            // Check for recent commits
            if let Ok(output) = tokio::process::Command::new("git")
                .args(&["log", "--oneline", "-1", "--format=%H %s"])
                .current_dir(repo_path)
                .output()
                .await
            {
                if output.status.success() {
                    let commit_info = String::from_utf8_lossy(&output.stdout);
                    if let Some((hash, message)) = commit_info.split_once(' ') {
                        events.push(SystemEvent::Git {
                            repository: repo_path.to_string_lossy().to_string(),
                            commit_hash: hash.trim().to_string(),
                            changed_files: vec![], // Would need git diff for this
                            commit_message: message.trim().to_string(),
                        });
                    }
                }
            }
        }

        Ok(events)
    }

    async fn subscribe(&self) -> Result<Box<dyn EventStream>> {
        // Would implement git hooks or polling
        Ok(Box::new(GitEventStream {
            repos: self.repo_paths.clone(),
            current_repo: 0,
        }))
    }
}

/// Git event stream
pub struct GitEventStream {
    repos: Vec<PathBuf>,
    current_repo: usize,
}

#[async_trait]
impl EventStream for GitEventStream {
    async fn next_event(&mut self) -> Option<SystemEvent> {
        // Simple polling for git events
        if self.current_repo >= self.repos.len() {
            self.current_repo = 0;
        }

        let repo = &self.repos[self.current_repo];
        self.current_repo += 1;

        // Check for new commits
        if let Ok(output) = tokio::process::Command::new("git")
            .args(&["log", "--oneline", "-1", "--format=%H %s"])
            .current_dir(repo)
            .output()
            .await
        {
            if output.status.success() {
                let commit_info = String::from_utf8_lossy(&output.stdout);
                if let Some((hash, message)) = commit_info.split_once(' ') {
                    return Some(SystemEvent::Git {
                        repository: repo.to_string_lossy().to_string(),
                        commit_hash: hash.trim().to_string(),
                        changed_files: vec![],
                        commit_message: message.trim().to_string(),
                    });
                }
            }
        }

        None
    }
}

impl ApiWebhookEventSource {
    /// Create a new API webhook event source
    pub fn new(endpoints: Vec<String>, auth_tokens: HashMap<String, String>) -> Self {
        Self {
            endpoints,
            auth_tokens,
        }
    }
}

#[async_trait]
impl EventSource for ApiWebhookEventSource {
    fn name(&self) -> &str {
        "api_webhook"
    }

    async fn poll_events(&self) -> Result<Vec<SystemEvent>> {
        // For polling-based webhooks, we'd make HTTP requests to check for new events
        // For now, return empty - real implementation would depend on specific APIs
        Ok(vec![])
    }

    async fn subscribe(&self) -> Result<Box<dyn EventStream>> {
        // Would implement webhook server or polling
        Ok(Box::new(ApiWebhookEventStream {
            endpoints: self.endpoints.clone(),
            current_endpoint: 0,
        }))
    }
}

/// API webhook event stream
pub struct ApiWebhookEventStream {
    endpoints: Vec<String>,
    current_endpoint: usize,
}

#[async_trait]
impl EventStream for ApiWebhookEventStream {
    async fn next_event(&mut self) -> Option<SystemEvent> {
        // Simple polling for webhook events
        if self.current_endpoint >= self.endpoints.len() {
            self.current_endpoint = 0;
        }

        let endpoint = &self.endpoints[self.current_endpoint];
        self.current_endpoint += 1;

        // Would make HTTP request to check for new events
        // For now, return None
        None
    }
}

impl RuntimeTelemetryEventSource {
    /// Create a new runtime telemetry event source
    pub fn new(services: Vec<String>, metrics: Vec<String>) -> Self {
        Self {
            services,
            metrics,
        }
    }
}

#[async_trait]
impl EventSource for RuntimeTelemetryEventSource {
    fn name(&self) -> &str {
        "runtime_telemetry"
    }

    async fn poll_events(&self) -> Result<Vec<SystemEvent>> {
        // Would collect runtime metrics from services
        // For now, return empty
        Ok(vec![])
    }

    async fn subscribe(&self) -> Result<Box<dyn EventStream>> {
        Ok(Box::new(RuntimeTelemetryEventStream {
            services: self.services.clone(),
            current_service: 0,
        }))
    }
}

/// Runtime telemetry event stream
pub struct RuntimeTelemetryEventStream {
    services: Vec<String>,
    current_service: usize,
}

#[async_trait]
impl EventStream for RuntimeTelemetryEventStream {
    async fn next_event(&mut self) -> Option<SystemEvent> {
        // Would collect telemetry data from services
        if self.current_service >= self.services.len() {
            self.current_service = 0;
        }

        let service = &self.services[self.current_service];
        self.current_service += 1;

        // Would query service for metrics
        // For now, return None
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn test_event_router_creation() {
        let router = EventRouter::new();
        assert_eq!(router.event_sources.len(), 0);
    }

    #[tokio::test]
    async fn test_event_filter_matching() {
        let router = EventRouter::new();

        let filter = EventFilter {
            name: "test_filter".to_string(),
            event_types: vec!["filesystem".to_string()],
            path_patterns: vec!["src/".to_string()],
            target_agent: "graph_extender".to_string(),
        };

        router.add_event_filter("graph_extender".to_string(), filter);

        let file_event = SystemEvent::FileSystem {
            path: "src/main.rs".to_string(),
            change_type: crate::swarm::FileChangeType::Modified,
            content: None,
        };

        let filtered = router.filter_events_for_agent("graph_extender", &[file_event.clone()]);
        assert_eq!(filtered.len(), 1);

        let filtered_empty = router.filter_events_for_agent("other_agent", &[file_event]);
        assert_eq!(filtered_empty.len(), 0);
    }
}
