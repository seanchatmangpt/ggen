//! Fixtures for Real Collaborators
//!
//! Chicago TDD uses real collaborators, not mocks. This module provides
//! fixtures for creating real test instances with observable state.
//!
//! # Principles
//!
//! - Real implementations, not mocks
//! - Observable state changes
//! - Isolated test environments
//! - Deterministic behavior

use std::path::{Path, PathBuf};
use anyhow::Result;
use tempfile::TempDir;
use std::collections::BTreeMap;
use serde::{Deserialize, Serialize};

/// Temporary file system fixture
///
/// Creates isolated temporary directories for file system tests.
/// Automatically cleaned up when dropped.
#[derive(Debug)]
pub struct TempFsFixture {
    dir: TempDir,
}

impl TempFsFixture {
    /// Create a new temporary file system fixture
    ///
    /// # Errors
    ///
    /// Returns error if temporary directory creation fails
    pub fn new() -> Result<Self> {
        Ok(Self {
            dir: TempDir::new()?,
        })
    }

    /// Get the root path of the temporary directory
    #[must_use]
    pub fn path(&self) -> &Path {
        self.dir.path()
    }

    /// Create a file with content
    ///
    /// # Errors
    ///
    /// Returns error if file creation or writing fails
    pub fn create_file(&self, path: impl AsRef<Path>, content: &str) -> Result<PathBuf> {
        let full_path = self.dir.path().join(path);
        if let Some(parent) = full_path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(&full_path, content)?;
        Ok(full_path)
    }

    /// Create a directory
    ///
    /// # Errors
    ///
    /// Returns error if directory creation fails
    pub fn create_dir(&self, path: impl AsRef<Path>) -> Result<PathBuf> {
        let full_path = self.dir.path().join(path);
        std::fs::create_dir_all(&full_path)?;
        Ok(full_path)
    }

    /// Read file content
    ///
    /// # Errors
    ///
    /// Returns error if file reading fails
    pub fn read_file(&self, path: impl AsRef<Path>) -> Result<String> {
        let full_path = self.dir.path().join(path);
        Ok(std::fs::read_to_string(full_path)?)
    }

    /// Check if file exists
    #[must_use]
    pub fn exists(&self, path: impl AsRef<Path>) -> bool {
        self.dir.path().join(path).exists()
    }
}

/// In-memory key-value store fixture
///
/// Provides a real key-value store for testing without external dependencies.
/// Uses BTreeMap for deterministic ordering in snapshots.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct InMemoryStoreFixture {
    data: BTreeMap<String, String>,
}

impl InMemoryStoreFixture {
    /// Create a new in-memory store
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Insert a key-value pair
    pub fn insert(&mut self, key: String, value: String) -> Option<String> {
        self.data.insert(key, value)
    }

    /// Get a value by key
    #[must_use]
    pub fn get(&self, key: &str) -> Option<&String> {
        self.data.get(key)
    }

    /// Remove a key-value pair
    pub fn remove(&mut self, key: &str) -> Option<String> {
        self.data.remove(key)
    }

    /// Check if key exists
    #[must_use]
    pub fn contains_key(&self, key: &str) -> bool {
        self.data.contains_key(key)
    }

    /// Get all keys
    #[must_use]
    pub fn keys(&self) -> Vec<&String> {
        self.data.keys().collect()
    }

    /// Get number of entries
    #[must_use]
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Check if store is empty
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Clear all entries
    pub fn clear(&mut self) {
        self.data.clear();
    }
}

/// Event log fixture for capturing system events
///
/// Real event logging for testing event-driven systems.
#[derive(Debug, Clone, Default)]
pub struct EventLogFixture {
    events: Vec<Event>,
}

/// Event captured in the log
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Event {
    pub name: String,
    pub timestamp: i64,
    pub data: BTreeMap<String, String>,
}

impl EventLogFixture {
    /// Create a new event log
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Log an event
    pub fn log(&mut self, name: String, data: BTreeMap<String, String>) {
        self.events.push(Event {
            name,
            timestamp: chrono::Utc::now().timestamp(),
            data,
        });
    }

    /// Get all events
    #[must_use]
    pub fn events(&self) -> &[Event] {
        &self.events
    }

    /// Get events by name
    #[must_use]
    pub fn events_by_name(&self, name: &str) -> Vec<&Event> {
        self.events.iter().filter(|e| e.name == name).collect()
    }

    /// Count events
    #[must_use]
    pub fn count(&self) -> usize {
        self.events.len()
    }

    /// Count events by name
    #[must_use]
    pub fn count_by_name(&self, name: &str) -> usize {
        self.events.iter().filter(|e| e.name == name).count()
    }

    /// Clear all events
    pub fn clear(&mut self) {
        self.events.clear();
    }

    /// Check if event exists
    #[must_use]
    pub fn contains(&self, name: &str) -> bool {
        self.events.iter().any(|e| e.name == name)
    }
}

/// Test data builder for creating consistent test data
#[derive(Debug, Clone, Default)]
pub struct TestDataBuilder<T> {
    data: Option<T>,
}

impl<T> TestDataBuilder<T> {
    /// Create a new test data builder
    #[must_use]
    pub fn new() -> Self {
        Self { data: None }
    }

    /// Set the data
    #[must_use]
    pub fn with_data(mut self, data: T) -> Self {
        self.data = Some(data);
        self
    }

    /// Build the test data
    ///
    /// # Panics
    ///
    /// Panics if data is not set
    #[must_use]
    pub fn build(self) -> T {
        self.data.expect("Test data must be set")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_temp_fs_fixture() -> Result<()> {
        let fixture = TempFsFixture::new()?;
        fixture.create_file("test.txt", "content")?;
        assert!(fixture.exists("test.txt"));
        assert_eq!(fixture.read_file("test.txt")?, "content");
        Ok(())
    }

    #[test]
    fn test_in_memory_store() {
        let mut store = InMemoryStoreFixture::new();
        assert!(store.is_empty());
        store.insert("key".to_string(), "value".to_string());
        assert_eq!(store.get("key"), Some(&"value".to_string()));
        assert_eq!(store.len(), 1);
    }

    #[test]
    fn test_event_log() {
        let mut log = EventLogFixture::new();
        assert_eq!(log.count(), 0);
        log.log("test_event".to_string(), BTreeMap::new());
        assert_eq!(log.count(), 1);
        assert!(log.contains("test_event"));
    }
}
