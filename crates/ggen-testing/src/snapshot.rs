//! Snapshot Testing Integration
//!
//! Integrates insta for snapshot testing with Chicago TDD principles.
//! Snapshots verify observable state at specific points in time.
//!
//! # Examples
//!
//! ```
//! use ggen_testing::snapshot::*;
//!
//! #[test]
//! fn test_output() {
//!     let output = generate_output();
//!     assert_snapshot!(output);
//! }
//! ```

pub use insta::{assert_debug_snapshot, assert_json_snapshot, assert_snapshot, assert_yaml_snapshot};
use anyhow::Result;
use serde::Serialize;
use std::path::Path;

/// Snapshot test helper for state verification
pub struct SnapshotTest {
    name: String,
}

impl SnapshotTest {
    /// Create a new snapshot test
    #[must_use]
    pub fn new(name: impl Into<String>) -> Self {
        Self { name: name.into() }
    }

    /// Assert snapshot matches for serializable state
    pub fn assert_state<T: Serialize>(&self, state: &T) {
        insta::assert_json_snapshot!(self.name.clone(), state);
    }

    /// Assert debug snapshot matches
    pub fn assert_debug<T: std::fmt::Debug>(&self, value: &T) {
        insta::assert_debug_snapshot!(self.name.clone(), value);
    }

    /// Assert string snapshot matches
    pub fn assert_str(&self, content: &str) {
        insta::assert_snapshot!(self.name.clone(), content);
    }
}

/// Snapshot manager for batch snapshot operations
#[derive(Debug)]
pub struct SnapshotManager {
    base_name: String,
    counter: usize,
}

impl SnapshotManager {
    /// Create a new snapshot manager
    #[must_use]
    pub fn new(base_name: impl Into<String>) -> Self {
        Self {
            base_name: base_name.into(),
            counter: 0,
        }
    }

    /// Take a snapshot with auto-incremented name
    pub fn snapshot<T: Serialize>(&mut self, state: &T) {
        let name = format!("{}_{}", self.base_name, self.counter);
        insta::assert_json_snapshot!(name, state);
        self.counter += 1;
    }

    /// Take a debug snapshot with auto-incremented name
    pub fn snapshot_debug<T: std::fmt::Debug>(&mut self, value: &T) {
        let name = format!("{}_{}", self.base_name, self.counter);
        insta::assert_debug_snapshot!(name, value);
        self.counter += 1;
    }

    /// Take a string snapshot with auto-incremented name
    pub fn snapshot_str(&mut self, content: &str) {
        let name = format!("{}_{}", self.base_name, self.counter);
        insta::assert_snapshot!(name, content);
        self.counter += 1;
    }

    /// Reset counter
    pub fn reset(&mut self) {
        self.counter = 0;
    }

    /// Get current counter value
    #[must_use]
    pub fn counter(&self) -> usize {
        self.counter
    }
}

/// Helper to create inline snapshots
#[macro_export]
macro_rules! assert_inline_snapshot {
    ($value:expr) => {
        insta::assert_snapshot!($value)
    };
}

/// Helper for file-based snapshots with custom path
pub fn assert_file_snapshot(path: impl AsRef<Path>, content: &str) -> Result<()> {
    let path = path.as_ref();
    let name = path
        .file_stem()
        .and_then(|s| s.to_str())
        .ok_or_else(|| anyhow::anyhow!("Invalid file path: {path:?}"))?;
    insta::assert_snapshot!(name, content);
    Ok(())
}

/// Settings builder for snapshot customization
#[derive(Debug, Default)]
pub struct SnapshotSettings {
    snapshot_path: Option<String>,
    prepend_module_to_snapshot: bool,
}

impl SnapshotSettings {
    /// Create new settings
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Set snapshot path
    #[must_use]
    pub fn snapshot_path(mut self, path: impl Into<String>) -> Self {
        self.snapshot_path = Some(path.into());
        self
    }

    /// Set prepend module to snapshot name
    #[must_use]
    pub fn prepend_module(mut self, prepend: bool) -> Self {
        self.prepend_module_to_snapshot = prepend;
        self
    }

    /// Apply settings to insta
    pub fn bind<F, R>(&self, f: F) -> R
    where
        F: FnOnce() -> R,
    {
        let mut settings = insta::Settings::clone_current();

        if let Some(ref path) = self.snapshot_path {
            settings.set_snapshot_path(path);
        }

        settings.set_prepend_module_to_snapshot(self.prepend_module_to_snapshot);

        settings.bind(f)
    }
}

/// Redaction helper for sensitive data in snapshots
#[macro_export]
macro_rules! with_redaction {
    ($selector:expr, $value:expr) => {{
        let mut settings = insta::Settings::clone_current();
        settings.add_redaction($selector, insta::dynamic_redaction(|value, _| {
            $value
        }));
        settings
    }};
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Serialize, Deserialize, PartialEq)]
    struct TestState {
        value: i32,
        name: String,
    }

    #[test]
    fn test_snapshot_state() {
        let state = TestState {
            value: 42,
            name: "test".to_string(),
        };
        let test = SnapshotTest::new("test_state");
        test.assert_state(&state);
    }

    #[test]
    fn test_snapshot_manager() {
        let mut manager = SnapshotManager::new("test_manager");
        assert_eq!(manager.counter(), 0);

        let state = TestState {
            value: 1,
            name: "first".to_string(),
        };
        manager.snapshot(&state);
        assert_eq!(manager.counter(), 1);
    }

    #[test]
    fn test_snapshot_settings() {
        let settings = SnapshotSettings::new()
            .snapshot_path("snapshots")
            .prepend_module(true);

        settings.bind(|| {
            assert_snapshot!("test", "content");
        });
    }
}
