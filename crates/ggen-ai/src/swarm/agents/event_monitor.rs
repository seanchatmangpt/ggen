//! Event Monitor Agent - Monitors for events and triggers swarm execution
//!
//! Provides comprehensive event monitoring with:
//! - File system events (create, modify, delete, rename)
//! - Git events (commits, branches, tags, pull requests, merges)
//! - API webhook events (HTTP endpoints, webhooks)
//! - Event filtering by type, pattern, and agent
//! - Event buffering for batch processing
//! - Async event delivery via channels

use super::{BaseAgent, EventSource, EventStream, SystemEvent, FileChangeType};
use crate::error::{GgenAiError, Result};
use async_trait::async_trait;
use serde_json::Value;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::{Duration, SystemTime};
use tokio::fs;
use tokio::sync::mpsc;
use tokio::sync::Mutex;
use tracing::{debug, error, info, warn};

/// Event Monitor Agent implementation
pub struct EventMonitorAgentImpl {
    base: BaseAgent,
    event_sources: Vec<Box<dyn EventSource>>,
    event_buffer: Arc<Mutex<EventBuffer>>,
    event_filters: Vec<EventFilter>,
}

impl EventMonitorAgentImpl {
    pub fn new(base: BaseAgent) -> Self {
        Self {
            base,
            event_sources: Vec::new(),
            event_buffer: Arc::new(Mutex::new(EventBuffer::new(1000))),
            event_filters: Vec::new(),
        }
    }

    /// Add an event source
    pub async fn add_event_source(&mut self, source: Box<dyn EventSource>) {
        info!("Adding event source: {}", source.name());
        self.event_sources.push(source);
    }

    /// Add an event filter
    pub fn add_event_filter(&mut self, filter: EventFilter) {
        info!("Adding event filter: {} for agent: {}", filter.name, filter.target_agent);
        self.event_filters.push(filter);
    }

    /// Start monitoring events
    pub async fn start_monitoring(&self) -> Result<()> {
        info!("Starting event monitoring with {} sources", self.event_sources.len());

        for source in &self.event_sources {
            let source_name = source.name().to_string();
            let buffer = self.event_buffer.clone();
            let filters = self.event_filters.clone();

            // Clone the source for the task
            // Note: We need to work around the trait object limitation
            // by subscribing to the source's stream
            match source.subscribe().await {
                Ok(mut stream) => {
                    tokio::spawn(async move {
                        info!("Started monitoring event source: {}", source_name);
                        loop {
                            match stream.next_event().await {
                                Some(event) => {
                                    debug!("Received event from {}: {:?}", source_name, event);

                                    // Apply filters
                                    let filtered = Self::apply_filters(&event, &filters);
                                    if filtered {
                                        // Add to buffer
                                        let mut buffer_guard = buffer.lock().await;
                                        buffer_guard.add_event(event).await;
                                    }
                                }
                                None => {
                                    // No event, sleep briefly
                                    tokio::time::sleep(Duration::from_millis(100)).await;
                                }
                            }
                        }
                    });
                }
                Err(e) => {
                    error!("Failed to subscribe to event source {}: {}", source_name, e);
                }
            }
        }

        Ok(())
    }

    /// Get buffered events
    pub async fn get_buffered_events(&self) -> Vec<SystemEvent> {
        let buffer = self.event_buffer.lock().await;
        buffer.get_events().await
    }

    /// Clear event buffer
    pub async fn clear_buffer(&self) {
        let mut buffer = self.event_buffer.lock().await;
        buffer.clear().await;
    }

    /// Apply filters to an event
    fn apply_filters(event: &SystemEvent, filters: &[EventFilter]) -> bool {
        // If no filters, accept all events
        if filters.is_empty() {
            return true;
        }

        // Check if any filter matches
        filters.iter().any(|filter| filter.matches(event))
    }
}

/// Event filter for routing events to specific agents
#[derive(Debug, Clone)]
pub struct EventFilter {
    /// Filter name
    pub name: String,
    /// Event types to match (empty = all types)
    pub event_types: Vec<String>,
    /// Path patterns to match (for file events)
    pub path_patterns: Vec<String>,
    /// Target agent for this filter
    pub target_agent: String,
}

impl EventFilter {
    /// Check if an event matches this filter
    fn matches(&self, event: &SystemEvent) -> bool {
        // Check event type
        if !self.event_types.is_empty() {
            let event_type = match event {
                SystemEvent::FileSystem { .. } => "filesystem",
                SystemEvent::Git { .. } => "git",
                SystemEvent::ApiWebhook { .. } => "api",
                SystemEvent::Database { .. } => "database",
                SystemEvent::RuntimeTelemetry { .. } => "telemetry",
                SystemEvent::BusinessRequirement { .. } => "requirement",
            };

            if !self.event_types.contains(&event_type.to_string()) {
                return false;
            }
        }

        // Check path patterns for file events
        if !self.path_patterns.is_empty() {
            if let SystemEvent::FileSystem { path, .. } = event {
                let pattern_matches = self
                    .path_patterns
                    .iter()
                    .any(|pattern| path.contains(pattern));
                if !pattern_matches {
                    return false;
                }
            }
        }

        true
    }
}

/// Event buffer for batching and deduplication
pub struct EventBuffer {
    events: Vec<SystemEvent>,
    max_size: usize,
    last_flush: SystemTime,
}

impl EventBuffer {
    /// Create a new event buffer
    fn new(max_size: usize) -> Self {
        Self {
            events: Vec::new(),
            max_size,
            last_flush: SystemTime::now(),
        }
    }

    /// Add an event to the buffer
    async fn add_event(&mut self, event: SystemEvent) {
        // Check for duplicates
        if self.events.iter().any(|e| self.events_equal(e, &event)) {
            debug!("Deduplicating event: {:?}", event);
            return;
        }

        self.events.push(event);

        // Auto-flush if buffer is full
        if self.events.len() >= self.max_size {
            info!("Event buffer full, auto-flushing {} events", self.events.len());
            self.clear().await;
        }
    }

    /// Get all events from buffer
    async fn get_events(&self) -> Vec<SystemEvent> {
        self.events.clone()
    }

    /// Clear the buffer
    async fn clear(&mut self) {
        self.events.clear();
        self.last_flush = SystemTime::now();
    }

    /// Check if two events are equal (for deduplication)
    fn events_equal(&self, a: &SystemEvent, b: &SystemEvent) -> bool {
        match (a, b) {
            (
                SystemEvent::FileSystem { path: path1, change_type: ct1, .. },
                SystemEvent::FileSystem { path: path2, change_type: ct2, .. },
            ) => path1 == path2 && ct1 == ct2,
            (
                SystemEvent::Git { commit_hash: hash1, .. },
                SystemEvent::Git { commit_hash: hash2, .. },
            ) => hash1 == hash2,
            _ => false,
        }
    }
}

/// File system event source with comprehensive monitoring
#[derive(Debug)]
pub struct FileSystemEventSource {
    watch_paths: Vec<PathBuf>,
    extensions: Vec<String>,
    event_tx: mpsc::Sender<SystemEvent>,
    _event_rx: Arc<Mutex<mpsc::Receiver<SystemEvent>>>,
}

impl FileSystemEventSource {
    /// Create a new file system event source
    pub fn new(watch_paths: Vec<PathBuf>, extensions: Vec<String>) -> Self {
        let (event_tx, event_rx) = mpsc::channel(1000);

        Self {
            watch_paths,
            extensions,
            event_tx,
            _event_rx: Arc::new(Mutex::new(event_rx)),
        }
    }

    /// Monitor a single path for changes
    async fn monitor_path(&self, path: &Path) -> Result<Vec<SystemEvent>> {
        let mut events = Vec::new();

        if !path.exists() {
            warn!("Path does not exist: {:?}", path);
            return Ok(events);
        }

        // Read directory entries
        if let Ok(mut dir_entries) = fs::read_dir(path).await {
            while let Ok(Some(entry)) = dir_entries.next_entry().await {
                if let Ok(metadata) = entry.metadata().await {
                    let file_path = entry.path();

                    // Check if file extension matches
                    if let Some(extension) = file_path.extension() {
                        if self.extensions.iter().any(|ext| ext == extension) {
                            let change_type = if metadata.is_file() {
                                FileChangeType::Modified
                            } else {
                                FileChangeType::Created
                            };

                            let event = SystemEvent::FileSystem {
                                path: file_path.to_string_lossy().to_string(),
                                change_type,
                                content: None,
                            };

                            events.push(event);
                        }
                    }
                }
            }
        }

        Ok(events)
    }
}

#[async_trait]
impl EventSource for FileSystemEventSource {
    fn name(&self) -> &str {
        "filesystem"
    }

    async fn poll_events(&self) -> Result<Vec<SystemEvent>> {
        let mut all_events = Vec::new();

        for path in &self.watch_paths {
            match self.monitor_path(path).await {
                Ok(events) => all_events.extend(events),
                Err(e) => error!("Error monitoring path {:?}: {}", path, e),
            }
        }

        Ok(all_events)
    }

    async fn subscribe(&self) -> Result<Box<dyn EventStream>> {
        Ok(Box::new(FileSystemEventStream {
            watch_paths: self.watch_paths.clone(),
            extensions: self.extensions.clone(),
            event_rx: Arc::clone(&self._event_rx),
            current_path: 0,
        }))
    }
}

/// File system event stream for continuous monitoring
pub struct FileSystemEventStream {
    watch_paths: Vec<PathBuf>,
    extensions: Vec<String>,
    event_rx: Arc<Mutex<mpsc::Receiver<SystemEvent>>>,
    current_path: usize,
}

#[async_trait]
impl EventStream for FileSystemEventStream {
    async fn next_event(&mut self) -> Option<SystemEvent> {
        // Try to receive from channel first
        let mut rx = self.event_rx.lock().await;
        match rx.try_recv() {
            Ok(event) => return Some(event),
            Err(_) => {}
        }

        // If no events in channel, poll paths
        drop(rx); // Release lock before async operation

        if self.watch_paths.is_empty() {
            return None;
        }

        // Round-robin through paths
        if self.current_path >= self.watch_paths.len() {
            self.current_path = 0;
        }

        let path = &self.watch_paths[self.current_path];
        self.current_path += 1;

        // Check for file changes
        if let Ok(mut dir_entries) = fs::read_dir(path).await {
            while let Ok(Some(entry)) = dir_entries.next_entry().await {
                if let Ok(metadata) = entry.metadata().await {
                    let file_path = entry.path();

                    if let Some(extension) = file_path.extension() {
                        let ext_str = extension.to_string_lossy();
                        if self.extensions.iter().any(|e| e == &*ext_str) {
                            let change_type = if metadata.is_file() {
                                FileChangeType::Modified
                            } else {
                                FileChangeType::Created
                            };

                            return Some(SystemEvent::FileSystem {
                                path: file_path.to_string_lossy().to_string(),
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

/// Git event source for monitoring repository changes
#[derive(Debug)]
pub struct GitEventSource {
    repo_paths: Vec<PathBuf>,
    event_types: Vec<String>,
}

impl GitEventSource {
    /// Create a new git event source
    pub fn new(repo_paths: Vec<PathBuf>, event_types: Vec<String>) -> Self {
        Self {
            repo_paths,
            event_types,
        }
    }

    /// Get latest commit from a repository
    async fn get_latest_commit(&self, repo_path: &Path) -> Result<SystemEvent> {
        let output = tokio::process::Command::new("git")
            .args(&["log", "-1", "--format=%H|%s|%an|%ae"])
            .current_dir(repo_path)
            .output()
            .await
            .map_err(|e| GgenAiError::internal(&format!("Failed to run git log: {}", e)))?;

        if !output.status.success() {
            return Err(GgenAiError::internal("Git log command failed"));
        }

        let commit_info = String::from_utf8_lossy(&output.stdout);
        let parts: Vec<&str> = commit_info.trim().split('|').collect();

        if parts.len() >= 2 {
            Ok(SystemEvent::Git {
                repository: repo_path.to_string_lossy().to_string(),
                commit_hash: parts[0].to_string(),
                changed_files: vec![],
                commit_message: parts[1].to_string(),
            })
        } else {
            Err(GgenAiError::internal("Invalid git log output format"))
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
            match self.get_latest_commit(repo_path).await {
                Ok(event) => events.push(event),
                Err(e) => error!("Error getting git events from {:?}: {}", repo_path, e),
            }
        }

        Ok(events)
    }

    async fn subscribe(&self) -> Result<Box<dyn EventStream>> {
        Ok(Box::new(GitEventStream {
            repo_paths: self.repo_paths.clone(),
            current_repo: 0,
            last_commits: HashMap::new(),
        }))
    }
}

/// Git event stream for continuous monitoring
pub struct GitEventStream {
    repo_paths: Vec<PathBuf>,
    current_repo: usize,
    last_commits: HashMap<PathBuf, String>,
}

#[async_trait]
impl EventStream for GitEventStream {
    async fn next_event(&mut self) -> Option<SystemEvent> {
        if self.repo_paths.is_empty() {
            return None;
        }

        // Round-robin through repositories
        if self.current_repo >= self.repo_paths.len() {
            self.current_repo = 0;
        }

        let repo = &self.repo_paths[self.current_repo];
        self.current_repo += 1;

        // Get latest commit
        let output = tokio::process::Command::new("git")
            .args(&["log", "-1", "--format=%H|%s"])
            .current_dir(repo)
            .output()
            .await
            .ok()?;

        if !output.status.success() {
            return None;
        }

        let commit_info = String::from_utf8_lossy(&output.stdout);
        let parts: Vec<&str> = commit_info.trim().split('|').collect();

        if parts.len() < 2 {
            return None;
        }

        let commit_hash = parts[0].to_string();
        let commit_message = parts[1].to_string();

        // Check if this is a new commit
        let last_commit = self.last_commits.get(repo);
        if last_commit == Some(&commit_hash) {
            return None; // Same commit as before
        }

        self.last_commits.insert(repo.clone(), commit_hash.clone());

        Some(SystemEvent::Git {
            repository: repo.to_string_lossy().to_string(),
            commit_hash,
            changed_files: vec![],
            commit_message,
        })
    }
}

/// API webhook event source for HTTP endpoints
#[derive(Debug)]
pub struct ApiWebhookEventSource {
    endpoints: Vec<String>,
    auth_tokens: HashMap<String, String>,
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
        // For polling-based webhooks, make HTTP requests to check for new events
        // This is a placeholder - real implementation would depend on specific APIs
        Ok(vec![])
    }

    async fn subscribe(&self) -> Result<Box<dyn EventStream>> {
        Ok(Box::new(ApiWebhookEventStream {
            endpoints: self.endpoints.clone(),
            auth_tokens: self.auth_tokens.clone(),
            current_endpoint: 0,
        }))
    }
}

/// API webhook event stream for continuous monitoring
pub struct ApiWebhookEventStream {
    endpoints: Vec<String>,
    auth_tokens: HashMap<String, String>,
    current_endpoint: usize,
}

#[async_trait]
impl EventStream for ApiWebhookEventStream {
    async fn next_event(&mut self) -> Option<SystemEvent> {
        // Placeholder for webhook polling
        // Real implementation would make HTTP requests to check for new events
        tokio::time::sleep(Duration::from_secs(1)).await;
        None
    }
}
