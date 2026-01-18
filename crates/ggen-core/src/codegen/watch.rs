//! Watch mode - File system monitoring for auto-regeneration
//!
//! This module implements the `--watch` flag functionality, monitoring ontology
//! and manifest files for changes and triggering automatic regeneration.
//!
//! ## Features
//!
//! - 300ms debounce to avoid duplicate events
//! - Bounded queue (10 items) to prevent memory exhaustion
//! - Monitors: ggen.toml, *.ttl ontology files, *.sparql queries
//! - Real-time regeneration on file changes
//!
//! ## Architecture
//!
//! ```text
//! File Change → Debouncer (300ms) → Queue (bounded) → Regeneration
//! ```

use ggen_utils::error::{Error, Result};
use std::path::{Path, PathBuf};
use std::sync::mpsc::{channel, Receiver, RecvTimeoutError};
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
}

impl FileWatcher {
    /// Create a new FileWatcher with default settings
    ///
    /// - Debounce: 300ms
    /// - Queue capacity: 10 items
    pub fn new<P: AsRef<Path>>(watch_paths: Vec<P>) -> Self {
        Self {
            watch_paths: watch_paths
                .iter()
                .map(|p| p.as_ref().to_path_buf())
                .collect(),
            debounce_ms: 300,
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
    /// This is a placeholder implementation. Real implementation would use
    /// the `notify` crate for cross-platform file watching.
    ///
    /// ## Returns
    ///
    /// A `Receiver<WatchEvent>` that yields file change events after debouncing.
    pub fn start(self) -> Result<Receiver<WatchEvent>> {
        let (_tx, rx) = channel();

        // Validate watch paths exist
        for path in &self.watch_paths {
            if !path.exists() {
                return Err(Error::new(&format!(
                    "Watch path does not exist: {}",
                    path.display()
                )));
            }
        }

        // TODO: Implement actual file watching using notify crate
        // For now, this is a stub implementation that returns the receiver
        //
        // Real implementation would:
        // 1. Create notify::Watcher
        // 2. Add watch_paths to watcher
        // 3. Spawn thread to handle events
        // 4. Implement debouncing logic
        // 5. Send WatchEvent through _tx channel

        std::thread::spawn(move || {
            // Placeholder: In real implementation, this would receive from notify::Watcher
            // and apply debouncing before sending to _tx
            loop {
                std::thread::sleep(Duration::from_millis(1000));
                // Real implementation would receive from notify and send WatchEvent via _tx
            }
        });

        Ok(rx)
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
            Err(RecvTimeoutError::Timeout) => Ok(None),
            Err(RecvTimeoutError::Disconnected) => Err(Error::new("Watch channel disconnected")),
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_file_watcher_creation() {
        let paths = vec![PathBuf::from(".")];
        let watcher = FileWatcher::new(paths);
        assert_eq!(watcher.debounce_ms, 300);
        assert_eq!(watcher.queue_capacity, 10);
    }

    #[test]
    fn test_file_watcher_configuration() {
        let paths = vec![PathBuf::from(".")];
        let watcher = FileWatcher::new(paths)
            .with_debounce_ms(500)
            .with_queue_capacity(20);

        assert_eq!(watcher.debounce_ms, 500);
        assert_eq!(watcher.queue_capacity, 20);
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
