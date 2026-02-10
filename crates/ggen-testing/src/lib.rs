//! Chicago TDD Test Infrastructure
//!
//! This crate provides state-based verification utilities for Chicago-style TDD.
//! Key principles:
//! - State-based verification (not behavior mocking)
//! - Real collaborators (not mocks)
//! - Observable outcomes verification
//! - AAA pattern (Arrange-Act-Assert)
//!
//! # Examples
//!
//! ```
//! use ggen_testing::{TestHarness, TestResult};
//!
//! #[tokio::test]
//! async fn test_example() -> TestResult<()> {
//!     // Arrange
//!     let harness = TestHarness::new("example_test");
//!
//!     // Act
//!     let result = harness.execute(|| async {
//!         Ok(42)
//!     }).await?;
//!
//!     // Assert
//!     assert_eq!(result, 42);
//!     Ok(())
//! }
//! ```

pub mod assertions;
pub mod fixtures;
pub mod property;
pub mod snapshot;

use std::future::Future;
use std::sync::Arc;
use std::time::{Duration, Instant};
use anyhow::Result;
use serde::{Deserialize, Serialize};

/// Type alias for test results
pub type TestResult<T> = Result<T>;

/// Test execution metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestMetadata {
    pub test_name: String,
    pub duration: Duration,
    pub timestamp: i64,
    pub success: bool,
}

/// Core test harness for Chicago-style TDD
///
/// Provides state-based verification with real collaborators.
/// No mocking - only real implementations and observable state changes.
#[derive(Debug, Clone)]
pub struct TestHarness {
    name: String,
    metadata: Arc<std::sync::Mutex<Vec<TestMetadata>>>,
}

impl TestHarness {
    /// Create a new test harness
    #[must_use]
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            metadata: Arc::new(std::sync::Mutex::new(Vec::new())),
        }
    }

    /// Execute a test with timing and metadata capture
    ///
    /// # Errors
    ///
    /// Returns error if the test closure fails
    pub async fn execute<F, Fut, T>(&self, f: F) -> TestResult<T>
    where
        F: FnOnce() -> Fut,
        Fut: Future<Output = TestResult<T>>,
    {
        let start = Instant::now();
        let result = f().await;
        let duration = start.elapsed();

        let metadata = TestMetadata {
            test_name: self.name.clone(),
            duration,
            timestamp: chrono::Utc::now().timestamp(),
            success: result.is_ok(),
        };

        if let Ok(mut meta) = self.metadata.lock() {
            meta.push(metadata);
        }

        result
    }

    /// Execute a synchronous test
    ///
    /// # Errors
    ///
    /// Returns error if the test closure fails
    pub fn execute_sync<F, T>(&self, f: F) -> TestResult<T>
    where
        F: FnOnce() -> TestResult<T>,
    {
        let start = Instant::now();
        let result = f();
        let duration = start.elapsed();

        let metadata = TestMetadata {
            test_name: self.name.clone(),
            duration,
            timestamp: chrono::Utc::now().timestamp(),
            success: result.is_ok(),
        };

        if let Ok(mut meta) = self.metadata.lock() {
            meta.push(metadata);
        }

        result
    }

    /// Get test metadata
    ///
    /// # Panics
    ///
    /// Panics if the metadata lock is poisoned
    #[must_use]
    pub fn metadata(&self) -> Vec<TestMetadata> {
        self.metadata.lock()
            .map(|guard| guard.clone())
            .unwrap_or_default()
    }

    /// Get test name
    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }
}

/// Builder for test harness configuration
#[derive(Debug, Default)]
pub struct TestHarnessBuilder {
    name: Option<String>,
}

impl TestHarnessBuilder {
    /// Create a new builder
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Set test name
    #[must_use]
    pub fn name(mut self, name: impl Into<String>) -> Self {
        self.name = Some(name.into());
        self
    }

    /// Build the test harness
    ///
    /// # Panics
    ///
    /// Panics if name is not set
    #[must_use]
    pub fn build(self) -> TestHarness {
        TestHarness::new(self.name.expect("Test name must be set"))
    }
}

/// State verification helper for Chicago TDD
///
/// Captures state before and after test execution for comparison
#[derive(Debug)]
pub struct StateVerifier<T> {
    before: T,
    after: Option<T>,
}

impl<T> StateVerifier<T> {
    /// Create a new state verifier with initial state
    #[must_use]
    pub fn new(initial: T) -> Self {
        Self {
            before: initial,
            after: None,
        }
    }

    /// Capture the after state
    pub fn capture(&mut self, state: T) {
        self.after = Some(state);
    }

    /// Get the before state
    #[must_use]
    pub fn before(&self) -> &T {
        &self.before
    }

    /// Get the after state
    #[must_use]
    pub fn after(&self) -> Option<&T> {
        self.after.as_ref()
    }
}

impl<T: PartialEq> StateVerifier<T> {
    /// Verify state changed
    #[must_use]
    pub fn changed(&self) -> bool {
        if let Some(after) = &self.after {
            &self.before != after
        } else {
            false
        }
    }
}

impl<T: Clone> StateVerifier<T> {
    /// Get state diff (returns tuple of before and after)
    #[must_use]
    pub fn diff(&self) -> Option<(T, T)> {
        self.after.as_ref().map(|a| (self.before.clone(), a.clone()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_harness_executes_async() {
        let harness = TestHarness::new("test");
        let result = harness
            .execute(|| async { Ok::<_, anyhow::Error>(42) })
            .await;
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 42);
    }

    #[test]
    fn test_harness_executes_sync() {
        let harness = TestHarness::new("test");
        let result = harness.execute_sync(|| Ok::<_, anyhow::Error>(42));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 42);
    }

    #[test]
    fn test_state_verifier_detects_change() {
        let mut verifier = StateVerifier::new(10);
        assert!(!verifier.changed());
        verifier.capture(20);
        assert!(verifier.changed());
    }

    #[test]
    fn test_state_verifier_detects_no_change() {
        let mut verifier = StateVerifier::new(10);
        verifier.capture(10);
        assert!(!verifier.changed());
    }
}
