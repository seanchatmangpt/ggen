//! File watcher for automatic regeneration on RDF changes

use ggen_utils::error::Result;
use notify::{Event, RecursiveMode, Watcher};
use notify_debouncer_full::{new_debouncer, DebounceEventResult, Debouncer, FileIdMap};
use std::path::{Path, PathBuf};
use std::sync::mpsc::{channel, Receiver};
use std::time::Duration;

use super::planner::{GenerationPlan, GenerationPlanner};
use super::resolver::ConventionResolver;

/// Watches project files and triggers regeneration on changes
pub struct ProjectWatcher {
    debouncer: Debouncer<notify::RecommendedWatcher, FileIdMap>,
    receiver: Receiver<DebounceEventResult>,
    resolver: ConventionResolver,
    planner: GenerationPlanner,
    /// Debounce delay in milliseconds
    #[allow(dead_code)]
    debounce_ms: u64,
}

impl ProjectWatcher {
    /// Create a new project watcher with default 300ms debounce
    pub fn new(project_root: PathBuf) -> Result<Self> {
        Self::with_debounce(project_root, 300)
    }

    /// Create a new project watcher with custom debounce duration
    fn with_debounce(project_root: PathBuf, debounce_ms: u64) -> Result<Self> {
        let (tx, rx) = channel();

        let debouncer = new_debouncer(
            Duration::from_millis(debounce_ms),
            None,
            move |result: DebounceEventResult| {
                let _ = tx.send(result);
            },
        )
        .map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to create file watcher: {}", e))
        })?;

        let resolver = ConventionResolver::new(project_root.clone());
        let conventions = resolver.discover()?;
        let planner = GenerationPlanner::new(conventions.clone());

        Ok(Self {
            debouncer,
            receiver: rx,
            resolver,
            planner,
            debounce_ms,
        })
    }

    /// Start watching the project directories
    pub fn watch(&mut self) -> Result<()> {
        // Get conventions to determine watch directories
        let conventions = self.resolver.discover()?;

        let watched_dirs = vec![&conventions.rdf_dir, &conventions.templates_dir];

        for dir in watched_dirs {
            if dir.exists() {
                self.debouncer
                    .watcher()
                    .watch(dir, RecursiveMode::Recursive)
                    .map_err(|e| {
                        ggen_utils::error::Error::new(&format!(
                            "Failed to watch directory {:?}: {}",
                            dir, e
                        ))
                    })?;
            }
        }

        Ok(())
    }

    /// Stop watching (drops the watcher)
    pub fn stop(self) -> Result<()> {
        drop(self.debouncer);
        Ok(())
    }

    /// Process pending file system events
    pub fn process_events(&mut self) -> Result<Vec<GenerationPlan>> {
        let mut plans = Vec::new();

        while let Ok(result) = self.receiver.try_recv() {
            match result {
                Ok(events) => {
                    for event in events {
                        if let Some(plan) = self.handle_change(event.event)? {
                            plans.push(plan);
                        }
                    }
                }
                Err(errors) => {
                    for error in errors {
                        log::error!("Watch error: {:?}", error);
                    }
                }
            }
        }

        Ok(plans)
    }

    /// Handle a file system change event
    fn handle_change(&mut self, event: Event) -> Result<Option<GenerationPlan>> {
        use notify::EventKind;

        match event.kind {
            EventKind::Create(_) | EventKind::Modify(_) | EventKind::Remove(_) => {
                let changed_files: Vec<PathBuf> = event
                    .paths
                    .into_iter()
                    .filter(|p| self.should_process_file(p))
                    .collect();

                if changed_files.is_empty() {
                    return Ok(None);
                }

                // For now, regenerate all templates when any file changes
                // A more sophisticated implementation could track dependencies
                let plan = self.planner.plan()?;
                Ok(Some(plan))
            }
            _ => Ok(None),
        }
    }

    /// Check if a file should be processed
    fn should_process_file(&self, path: &Path) -> bool {
        // Ignore generated files
        if path.to_string_lossy().contains("generated/") {
            return false;
        }

        // Ignore hidden files and directories
        if path
            .file_name()
            .and_then(|n| n.to_str())
            .is_some_and(|n| n.starts_with('.'))
        {
            return false;
        }

        // Ignore temporary files
        if path.extension().and_then(|e| e.to_str()) == Some("tmp") {
            return false;
        }

        true
    }

    /// Find templates affected by file changes
    #[allow(dead_code)]
    fn find_affected_templates(&self, _changed: &[PathBuf]) -> Vec<String> {
        // For now, return all templates
        // A more sophisticated implementation would analyze dependencies
        match self.resolver.discover() {
            Ok(conventions) => conventions.templates.keys().cloned().collect(),
            Err(e) => {
                log::warn!("Failed to discover conventions for affected templates: {}", e);
                // Return empty list on error - safer than continuing with stale data
                Vec::new()
            }
        }
    }

    /// Regenerate a specific template
    pub fn regenerate_template(&self, template: &str) -> Result<()> {
        // In a real implementation, this would call the template engine
        // For now, we just log it
        log::info!("Regenerating template: {}", template);
        Ok(())
    }

    /// Get the resolver
    pub fn resolver(&self) -> &ConventionResolver {
        &self.resolver
    }

    /// Get the planner
    pub fn planner(&self) -> &GenerationPlanner {
        &self.planner
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_watcher_creation() {
        let temp_dir = TempDir::new().unwrap();
        let watcher = ProjectWatcher::new(temp_dir.path().to_path_buf());
        assert!(watcher.is_ok());
    }

    #[test]
    fn test_watcher_with_debounce() {
        let temp_dir = TempDir::new().unwrap();
        let watcher = ProjectWatcher::with_debounce(temp_dir.path().to_path_buf(), 500);
        assert!(watcher.is_ok());
    }

    #[test]
    fn test_should_process_file() {
        let temp_dir = TempDir::new().unwrap();
        let watcher = ProjectWatcher::new(temp_dir.path().to_path_buf()).unwrap();

        // Should process
        assert!(watcher.should_process_file(&PathBuf::from("domain/test.yaml")));
        assert!(watcher.should_process_file(&PathBuf::from("templates/test.tera")));

        // Should not process
        assert!(!watcher.should_process_file(&PathBuf::from("generated/test.rs")));
        assert!(!watcher.should_process_file(&PathBuf::from(".hidden")));
        assert!(!watcher.should_process_file(&PathBuf::from("test.tmp")));
    }

    #[test]
    fn test_watch_and_stop() {
        let temp_dir = TempDir::new().unwrap();

        // Create watched directories
        fs::create_dir_all(temp_dir.path().join("domain")).unwrap();
        fs::create_dir_all(temp_dir.path().join("templates")).unwrap();

        let mut watcher = ProjectWatcher::new(temp_dir.path().to_path_buf()).unwrap();
        assert!(watcher.watch().is_ok());
        assert!(watcher.stop().is_ok());
    }

    #[test]
    fn test_process_events() {
        let temp_dir = TempDir::new().unwrap();

        // Create watched directories
        fs::create_dir_all(temp_dir.path().join("domain")).unwrap();

        let mut watcher = ProjectWatcher::new(temp_dir.path().to_path_buf()).unwrap();
        watcher.watch().unwrap();

        // Process events (should be empty initially)
        let plans = watcher.process_events().unwrap();
        assert_eq!(plans.len(), 0);
    }

    #[test]
    fn test_regenerate_template() {
        let temp_dir = TempDir::new().unwrap();
        let watcher = ProjectWatcher::new(temp_dir.path().to_path_buf()).unwrap();

        // Should not fail
        assert!(watcher.regenerate_template("test_template").is_ok());
    }

    #[test]
    fn test_find_affected_templates() {
        let temp_dir = TempDir::new().unwrap();
        let watcher = ProjectWatcher::new(temp_dir.path().to_path_buf()).unwrap();

        let changed = vec![temp_dir.path().join("domain/test.yaml")];
        let affected = watcher.find_affected_templates(&changed);

        // Should return empty since no conventions are registered
        assert_eq!(affected.len(), 0);
    }
}
