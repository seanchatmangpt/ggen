//! SHACL validation violation types
//!
//! This module defines the core types for representing SHACL validation violations,
//! following Constitution Principle V (Type-First Thinking).

use std::path::PathBuf;

/// Constraint types from W3C SHACL specification
///
/// Represents the 5 constraint types implemented for spec-kit validation:
/// - Cardinality (sh:minCount, sh:maxCount)
/// - Datatype (sh:datatype)
/// - Enumeration (sh:in)
/// - Pattern (sh:pattern)
/// - StringLength (sh:minLength, sh:maxLength)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ConstraintType {
    /// Cardinality constraint violation (sh:minCount, sh:maxCount)
    ///
    /// Examples:
    /// - Missing required property (minCount 1 violated)
    /// - Too many values for single-valued property (maxCount 1 violated)
    Cardinality,

    /// Datatype constraint violation (sh:datatype)
    ///
    /// Examples:
    /// - Expected xsd:string but got xsd:integer
    /// - Expected xsd:date but got plain literal
    Datatype,

    /// Enumeration constraint violation (sh:in)
    ///
    /// Examples:
    /// - Priority value "HIGH" when expecting ["P1", "P2", "P3"]
    /// - Status value "DONE" when expecting ["Draft", "In Progress", "Complete"]
    Enumeration,

    /// Pattern constraint violation (sh:pattern)
    ///
    /// Examples:
    /// - Branch name "feature" when expecting "^[0-9]{3}-[a-z0-9-]+$"
    /// - Invalid email format
    Pattern,

    /// String length constraint violation (sh:minLength, sh:maxLength)
    ///
    /// Examples:
    /// - Title too short (<5 characters)
    /// - Description too long (>500 characters)
    StringLength,
}

impl std::fmt::Display for ConstraintType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConstraintType::Cardinality => write!(f, "Cardinality"),
            ConstraintType::Datatype => write!(f, "Datatype"),
            ConstraintType::Enumeration => write!(f, "Enumeration"),
            ConstraintType::Pattern => write!(f, "Pattern"),
            ConstraintType::StringLength => write!(f, "StringLength"),
        }
    }
}

/// Severity level from SHACL (sh:severity)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Severity {
    /// sh:Violation - Validation failure (blocking)
    Violation,

    /// sh:Warning - Potential issue (non-blocking)
    Warning,

    /// sh:Info - Informational message (non-blocking)
    Info,
}

impl std::fmt::Display for Severity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Severity::Violation => write!(f, "VIOLATION"),
            Severity::Warning => write!(f, "WARNING"),
            Severity::Info => write!(f, "INFO"),
        }
    }
}

/// Location in source TTL file
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceLocation {
    pub file_path: PathBuf,
    pub line: usize,
    pub column: usize,
}

/// Single SHACL validation violation
///
/// Represents a specific constraint violation detected during validation.
/// Designed to provide actionable error messages with context.
///
/// ## Constitution Compliance
///
/// - ✓ Principle V: Type-First Thinking (strong typing, no stringly-typed data)
/// - ✓ Principle II: Deterministic (violations are comparable and orderable)
#[derive(Debug, Clone)]
pub struct Violation {
    /// Node (subject) that violates the constraint
    ///
    /// Example: ":us-001" (user story IRI)
    pub focus_node: String,

    /// Property path (predicate) that failed validation
    ///
    /// Example: Some("sk:priority") or None for node-level constraints
    pub result_path: Option<String>,

    /// Type of constraint that was violated
    pub constraint_type: ConstraintType,

    /// SHACL shape that defined the constraint
    ///
    /// Example: "sk:UserStoryShape"
    pub source_shape: String,

    /// Human-readable error message
    ///
    /// Example: "Missing required property sk:title on node :us-001"
    pub message: String,

    /// Location in source TTL file (if available)
    ///
    /// Note: MVP skips line number tracking per DEC-004. Future enhancement.
    pub location: Option<SourceLocation>,

    /// Severity level (Violation, Warning, Info)
    pub severity: Severity,

    /// Expected value for the constraint
    ///
    /// Example: Some("xsd:string") for datatype constraint
    pub expected_value: Option<String>,

    /// Actual value that violated the constraint
    ///
    /// Example: Some("HIGH") for enumeration violation
    pub actual_value: Option<String>,
}

impl Violation {
    /// Create a new violation with minimum required fields
    pub fn new(
        focus_node: impl Into<String>,
        constraint_type: ConstraintType,
        message: impl Into<String>,
    ) -> Self {
        Self {
            focus_node: focus_node.into(),
            result_path: None,
            constraint_type,
            source_shape: String::new(),
            message: message.into(),
            location: None,
            severity: Severity::Violation,
            expected_value: None,
            actual_value: None,
        }
    }

    /// Set the result path (property that failed)
    pub fn with_result_path(mut self, path: impl Into<String>) -> Self {
        self.result_path = Some(path.into());
        self
    }

    /// Set the source shape
    pub fn with_source_shape(mut self, shape: impl Into<String>) -> Self {
        self.source_shape = shape.into();
        self
    }

    /// Set the severity
    pub fn with_severity(mut self, severity: Severity) -> Self {
        self.severity = severity;
        self
    }

    /// Set expected and actual values for better error messages
    pub fn with_values(
        mut self,
        expected: impl Into<String>,
        actual: impl Into<String>,
    ) -> Self {
        self.expected_value = Some(expected.into());
        self.actual_value = Some(actual.into());
        self
    }
}

/// Validation result for a single TTL file or graph
///
/// ## Constitution Compliance
///
/// - ✓ Principle II: Deterministic RDF Projections (violations use BTreeMap internally if needed)
/// - ✓ Principle V: Type-First Thinking (explicit passed/failed state)
#[derive(Debug, Clone)]
pub struct ValidationResult {
    /// File path that was validated (if applicable)
    pub file_path: Option<PathBuf>,

    /// Whether validation passed (true) or failed (false)
    pub passed: bool,

    /// Number of violations found (convenience field)
    pub violation_count: usize,

    /// List of violations (empty if passed)
    pub violations: Vec<Violation>,

    /// Validation duration in milliseconds
    pub duration_ms: u64,
}

impl ValidationResult {
    /// Create a passing result (no violations)
    pub fn pass(duration_ms: u64) -> Self {
        Self {
            file_path: None,
            passed: true,
            violation_count: 0,
            violations: Vec::new(),
            duration_ms,
        }
    }

    /// Create a failing result with violations
    pub fn fail(violations: Vec<Violation>, duration_ms: u64) -> Self {
        let violation_count = violations.len();
        Self {
            file_path: None,
            passed: false,
            violation_count,
            violations,
            duration_ms,
        }
    }

    /// Set the file path for this result
    pub fn with_file_path(mut self, path: impl Into<PathBuf>) -> Self {
        self.file_path = Some(path.into());
        self
    }

    /// Get only violations of a specific severity
    pub fn violations_by_severity(&self, severity: Severity) -> Vec<&Violation> {
        self.violations
            .iter()
            .filter(|v| v.severity == severity)
            .collect()
    }

    /// Get only blocking violations (severity = Violation)
    pub fn blocking_violations(&self) -> Vec<&Violation> {
        self.violations_by_severity(Severity::Violation)
    }

    /// Check if there are any blocking violations
    pub fn has_blocking_violations(&self) -> bool {
        self.violations
            .iter()
            .any(|v| v.severity == Severity::Violation)
    }
}
