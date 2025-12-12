//! Core data types for test quality audit
//!
//! This module defines the foundational entities used throughout the test audit tooling,
//! including test case metadata, mutation results, and assertion strength classification.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::fmt;
use std::str::FromStr;
use thiserror::Error;

// =============================================================================
// TestId Newtype (T019) - Prevents primitive obsession
// =============================================================================

/// Unique test identifier newtype to prevent primitive obsession.
///
/// Wraps a String with type safety and validation.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TestId(String);

impl TestId {
    /// Create a new TestId with validation
    ///
    /// # Errors
    /// Returns `AuditError::InvalidTestId` if the id is empty or contains invalid characters
    pub fn new(id: impl Into<String>) -> Result<Self, AuditError> {
        let id = id.into();
        if id.is_empty() {
            return Err(AuditError::InvalidTestId("Test ID cannot be empty".into()));
        }
        if id.contains('\0') {
            return Err(AuditError::InvalidTestId(
                "Test ID cannot contain null bytes".into(),
            ));
        }
        Ok(Self(id))
    }

    /// Get the inner string reference
    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

// T020: Implement Display and FromStr for TestId
impl fmt::Display for TestId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for TestId {
    type Err = AuditError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::new(s)
    }
}

// =============================================================================
// TestType Enum - Unit vs Integration classification
// =============================================================================

/// Test type classification
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TestType {
    /// Unit test (in-memory, no I/O, <1s budget)
    Unit,
    /// Integration test (file system, network, <10s budget)
    Integration,
}

// =============================================================================
// TestCase (T015) - Test metadata and failure history
// =============================================================================

/// Test case with metadata, execution history, and failure tracking
///
/// Tracks all information needed for quality audit and value scoring.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestCase {
    /// Unique test identifier
    pub id: TestId,
    /// Human-readable test name
    pub name: String,
    /// File path where test is defined
    pub file_path: String,
    /// Test type (Unit or Integration)
    pub test_type: TestType,
    /// Average execution time in milliseconds
    pub execution_time_ms: u64,
    /// Historical failure data
    pub failure_history: Vec<TestFailure>,
}

/// Individual test failure record
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestFailure {
    /// When the failure occurred
    pub timestamp: DateTime<Utc>,
    /// Error message
    pub message: String,
    /// Whether this was a flaky failure (non-deterministic)
    pub is_flaky: bool,
}

// =============================================================================
// MutationResult (T016) - Mutation testing outcomes
// =============================================================================

/// Result from a single mutation test
///
/// Tracks whether a mutant survived (test didn't catch it) or was killed (test failed).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MutationResult {
    /// Unique mutation identifier
    pub mutation_id: String,
    /// Test that should catch this mutation
    pub test_id: TestId,
    /// True if mutant survived (BAD - test didn't catch bug)
    pub mutant_survived: bool,
    /// Type of mutation applied
    pub mutation_type: MutationType,
    /// When the test killed the mutant (or didn't)
    pub kill_timestamp: DateTime<Utc>,
}

/// Types of mutations that can be applied
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum MutationType {
    /// Replace binary operator (+ → -, == → !=, etc.)
    BinaryOp,
    /// Replace unary operator (! → identity, - → identity)
    UnaryOp,
    /// Replace constant value (0 → 1, true → false)
    ConstantReplacement,
    /// Remove statement (delete line)
    StatementDeletion,
    /// Replace return value
    ReturnValueChange,
}

// =============================================================================
// AssertionStrength (T017) - Assertion quality classification
// =============================================================================

/// Classification of assertion strength
///
/// Weak assertions only check execution (is_ok), while strong assertions
/// validate exact values (assert_eq with expected output).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum AssertionStrength {
    /// Weak: Only checks code runs (is_ok, is_some, is_none)
    Weak,
    /// Medium: Checks truthiness (assert!, is_err)
    Medium,
    /// Strong: Validates exact values (assert_eq!, assert_ne! with exact expected)
    Strong,
}

impl AssertionStrength {
    /// Convert to numeric score (0-10 scale)
    ///
    /// - Weak: 0-3
    /// - Medium: 4-7
    /// - Strong: 8-10
    #[must_use]
    pub fn to_score(self) -> f64 {
        match self {
            Self::Weak => 2.0,
            Self::Medium => 5.5,
            Self::Strong => 9.0,
        }
    }
}

// =============================================================================
// Error Types (T026, T028)
// =============================================================================

/// Errors that can occur during test audit operations
#[derive(Debug, thiserror::Error)]
pub enum AuditError {
    /// Mutation testing failed
    #[error("Mutation testing failed: {0}")]
    MutationFailed(String),

    /// Failed to parse assertion from test code
    #[error("Failed to parse assertion: {0}")]
    AssertionParseError(String),

    /// I/O error during audit
    #[error("I/O error: {0}")]
    IoError(#[from] std::io::Error),

    /// Invalid test ID
    #[error("Invalid test ID: {0}")]
    InvalidTestId(String),

    /// JSON serialization/deserialization error
    #[error("JSON error: {0}")]
    JsonError(#[from] serde_json::Error),
}

/// Type alias for audit results (T028)
pub type AuditResult<T> = Result<T, AuditError>;
