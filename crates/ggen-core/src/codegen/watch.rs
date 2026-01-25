//! Watch mode - File system monitoring for auto-regeneration
//!
//! This module implements the `--watch` flag functionality, monitoring ontology
//! and manifest files for changes and triggering automatic regeneration.
//!
//! ## Features
//!
//! - Cross-platform file watching using notify crate
//! - 500ms debounce to avoid duplicate events
//! - Graceful shutdown on SIGINT (Ctrl+C)
//! - Monitors: ggen.toml, *.ttl ontology files, *.sparql queries, *.tera templates
//! - Real-time regeneration on file changes
//!
//! ## Architecture
//!
//! ```text
//! File Change → notify → Debouncer (500ms) → Channel → Regeneration
//!     ↓
//!   SIGINT → Graceful Shutdown
//! ```

use ggen_utils::error::{Error, Result};
use notify::event::{ModifyKind, RenameMode};
use notify::{Event, EventKind, RecommendedWatcher, RecursiveMode};
use notify_debouncer_full::{new_debouncer, DebounceEventResult, Debouncer, NoCache};
use std::path::{Path, PathBuf};
use std::sync::mpsc::{channel, Receiver, Sender};
use std::sync::Arc;
use std::time::Duration;

/// File system watcher for auto-regeneration
pub struct FileWatcher {
    /// Paths to watch
    watch_paths: Vec<PathBuf>,
    /// Debounce duration (milliseconds)
    pub debounce_ms: u64,
    /// Queue capacity
    pub queue_capacity: usize,
}

/// Watch event indicating file change
#[derive(Debug, Clone)]
pub struct WatchEvent {
    /// Changed file path
    pub path: PathBuf,
    /// Event timestamp
    pub timestamp: std::time::Instant,
    /// Event kind (created, modified, removed)
    pub kind: WatchEventKind,
}

/// Type of file system change
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WatchEventKind {
    /// File was created
    Created,
    /// File was modified
    Modified,
    /// File was removed
    Removed,
    /// File was renamed
    Renamed,
}

impl FileWatcher {
    /// Create a new FileWatcher with default settings
    ///
    /// - Debounce: 500ms (per requirements)
    /// - Queue capacity: 10 items
    pub fn new<P: AsRef<Path>>(watch_paths: Vec<P>) -> Self {
        Self {
            watch_paths: watch_paths
                .iter()
                .map(|p| p.as_ref().to_path_buf())
                .collect(),
            debounce_ms: 500,
            queue_capacity: 10,
        }
    }

    /// Set debounce duration in milliseconds
    pub fn with_debounce_ms(mut self, debounce_ms: u64) -> Self {
        self.debounce_ms = debounce_ms;
        self
    }

    /// Set queue capacity
    pub fn with_queue_capacity(mut self, capacity: usize) -> Self {
        self.queue_capacity = capacity;
        self
    }

    /// Start watching and return event receiver
    ///
    /// Uses notify-debouncer-full for cross-platform file watching with debouncing.
    ///
    /// ## Returns
    ///
    /// A `Receiver<WatchEvent>` that yields file change events after debouncing.
    pub fn start(self) -> Result<Receiver<WatchEvent>> {
        let (tx, rx) = channel();

        // Validate watch paths exist
        for path in &self.watch_paths {
            if !path.exists() {
                return Err(Error::new(&format!(
                    "Watch path does not exist: {}",
                    path.display()
                )));
            }
        }

        let debounce_duration = Duration::from_millis(self.debounce_ms);
        let watch_paths = self.watch_paths.clone();

        // Spawn watcher in background thread
        std::thread::spawn(move || {
            if let Err(e) = Self::watch_loop(watch_paths, debounce_duration, tx) {
                eprintln!("Watch error: {}", e);
            }
        });

        Ok(rx)
    }

    /// Internal watch loop that runs in background thread
    fn watch_loop(
        watch_paths: Vec<PathBuf>, debounce_duration: Duration, tx: Sender<WatchEvent>,
    ) -> Result<()> {
        let tx_clone = tx.clone();

        // Create debounced watcher
        let mut debouncer: Debouncer<RecommendedWatcher, NoCache> = new_debouncer(
            debounce_duration,
            None,
            move |result: DebounceEventResult| {
                match result {
                    Ok(events) => {
                        for event in events {
                            if let Some(watch_event) = Self::convert_event(event.event) {
                                // Send event, ignore errors if receiver is dropped
                                let _ = tx_clone.send(watch_event);
                            }
                        }
                    }
                    Err(errors) => {
                        for error in errors {
                            eprintln!("Watch error: {:?}", error);
                        }
                    }
                }
            },
        )
        .map_err(|e| Error::new(&format!("Failed to create file watcher: {}", e)))?;

        // Add all watch paths - debouncer now implements Watcher directly
        for path in &watch_paths {
            let watch_mode = if path.is_dir() {
                RecursiveMode::Recursive
            } else {
                RecursiveMode::NonRecursive
            };

            debouncer
                .watch(path, watch_mode)
                .map_err(|e| Error::new(&format!("Failed to watch {}: {}", path.display(), e)))?;
        }

        // Keep debouncer alive - it will run until this thread ends
        loop {
            std::thread::sleep(Duration::from_secs(1));
        }
    }

    /// Convert notify Event to WatchEvent
    fn convert_event(event: Event) -> Option<WatchEvent> {
        let kind = match event.kind {
            EventKind::Create(_) => WatchEventKind::Created,
            EventKind::Modify(ModifyKind::Data(_) | ModifyKind::Any) => WatchEventKind::Modified,
            EventKind::Remove(_) => WatchEventKind::Removed,
            EventKind::Modify(ModifyKind::Name(RenameMode::Both | RenameMode::To)) => {
                WatchEventKind::Renamed
            }
            _ => return None, // Ignore other event types
        };

        // Take first path from event
        event.paths.into_iter().next().map(|path| WatchEvent {
            path,
            timestamp: std::time::Instant::now(),
            kind,
        })
    }

    /// Wait for next change event with timeout
    ///
    /// ## Arguments
    ///
    /// * `rx` - Event receiver from `start()`
    /// * `timeout` - Maximum wait duration
    ///
    /// ## Returns
    ///
    /// - `Ok(Some(event))` if change detected
    /// - `Ok(None)` if timeout reached
    /// - `Err` if channel closed
    pub fn wait_for_change(
        rx: &Receiver<WatchEvent>, timeout: Duration,
    ) -> Result<Option<WatchEvent>> {
        match rx.recv_timeout(timeout) {
            Ok(event) => Ok(Some(event)),
            Err(std::sync::mpsc::RecvTimeoutError::Timeout) => Ok(None),
            Err(std::sync::mpsc::RecvTimeoutError::Disconnected) => {
                Err(Error::new("Watch channel disconnected"))
            }
        }
    }
}

/// Collect watch paths from manifest
///
/// Returns all paths that should be monitored for changes:
/// - ggen.toml (manifest)
/// - ontology.source (main ontology file)
/// - ontology.imports (imported ontology files)
/// - generation.rules[].query files
/// - generation.rules[].template files
pub fn collect_watch_paths(
    manifest_path: &Path, manifest: &crate::manifest::GgenManifest, base_path: &Path,
) -> Vec<PathBuf> {
    use crate::manifest::{QuerySource, TemplateSource};

    let mut paths = Vec::new();

    // Watch manifest itself
    paths.push(manifest_path.to_path_buf());

    // Watch ontology source
    paths.push(base_path.join(&manifest.ontology.source));

    // Watch ontology imports
    for import in &manifest.ontology.imports {
        paths.push(base_path.join(import));
    }

    // Watch query files
    for rule in &manifest.generation.rules {
        if let QuerySource::File { file } = &rule.query {
            paths.push(base_path.join(file));
        }
    }

    // Watch template files
    for rule in &manifest.generation.rules {
        if let TemplateSource::File { file } = &rule.template {
            paths.push(base_path.join(file));
        }
    }

    paths
}

/// Install signal handler for graceful shutdown on SIGINT (Ctrl+C)
///
/// Returns an Arc-wrapped atomic bool that will be set to true on SIGINT.
/// The watch loop should check this flag and exit cleanly.
pub fn install_shutdown_handler() -> Result<Arc<std::sync::atomic::AtomicBool>> {
    use signal_hook::consts::SIGINT;
    use signal_hook::flag;
    use std::sync::atomic::AtomicBool;

    let shutdown = Arc::new(AtomicBool::new(false));
    flag::register(SIGINT, Arc::clone(&shutdown))
        .map_err(|e| Error::new(&format!("Failed to register SIGINT handler: {}", e)))?;

    Ok(shutdown)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_file_watcher_creation() {
        let paths = vec![PathBuf::from(".")];
        let watcher = FileWatcher::new(paths);
        assert_eq!(watcher.debounce_ms, 500); // Updated to 500ms per requirements
        assert_eq!(watcher.queue_capacity, 10);
    }

    #[test]
    fn test_file_watcher_configuration() {
        let paths = vec![PathBuf::from(".")];
        let watcher = FileWatcher::new(paths)
            .with_debounce_ms(1000)
            .with_queue_capacity(20);

        assert_eq!(watcher.debounce_ms, 1000);
        assert_eq!(watcher.queue_capacity, 20);
    }

    #[test]
    fn test_watch_event_kind() {
        let event = WatchEvent {
            path: PathBuf::from("test.txt"),
            timestamp: std::time::Instant::now(),
            kind: WatchEventKind::Modified,
        };
        assert_eq!(event.kind, WatchEventKind::Modified);
    }

    #[test]
    fn test_collect_watch_paths_empty() {
        use crate::manifest::{
            GenerationConfig, GgenManifest, InferenceConfig, OntologyConfig, ProjectConfig,
            ValidationConfig,
        };
        use std::collections::BTreeMap;
        use std::path::PathBuf;

        let manifest = GgenManifest {
            project: ProjectConfig {
                name: "test".to_string(),
                version: "1.0.0".to_string(),
                description: None,
            },
            ontology: OntologyConfig {
                source: PathBuf::from("ontology.ttl"),
                imports: vec![],
                base_iri: None,
                prefixes: BTreeMap::new(),
            },
            inference: InferenceConfig {
                rules: vec![],
                max_reasoning_timeout_ms: 5000,
            },
            generation: GenerationConfig {
                rules: vec![],
                max_sparql_timeout_ms: 5000,
                require_audit_trail: false,
                determinism_salt: None,
                output_dir: PathBuf::from("generated"),
            },
            validation: ValidationConfig::default(),
        };

        let manifest_path = Path::new("ggen.toml");
        let base_path = Path::new(".");
        let paths = collect_watch_paths(manifest_path, &manifest, base_path);

        // Should have at least manifest and ontology source
        assert!(paths.len() >= 2);
        assert!(paths.contains(&PathBuf::from("ggen.toml")));
        // Ontology path might be joined with base_path, check if any path ends with ontology.ttl
        assert!(
            paths
                .iter()
                .any(|p| p.to_string_lossy().ends_with("ontology.ttl")),
            "Should contain ontology.ttl path (possibly joined with base_path)"
        );
    }
}
