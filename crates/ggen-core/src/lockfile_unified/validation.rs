//! Validation strategies for unified lockfile system
//!
//! Provides integrity verification, circular dependency detection,
//! and schema validation for lockfiles.

use ggen_utils::error::{Error, Result};
use std::collections::{BTreeMap, BTreeSet};

use super::traits::{LockEntry, Lockfile};

/// Result of a validation operation
#[derive(Debug, Clone)]
pub struct ValidationResult {
    /// Whether validation passed
    pub valid: bool,
    /// List of errors found
    pub errors: Vec<ValidationError>,
    /// List of warnings (non-fatal)
    pub warnings: Vec<ValidationWarning>,
    /// Validation metadata
    pub metadata: ValidationMetadata,
}

impl ValidationResult {
    /// Create a successful validation result
    pub fn ok() -> Self {
        Self {
            valid: true,
            errors: Vec::new(),
            warnings: Vec::new(),
            metadata: ValidationMetadata::default(),
        }
    }

    /// Create a failed validation result
    pub fn fail(errors: Vec<ValidationError>) -> Self {
        Self {
            valid: false,
            errors,
            warnings: Vec::new(),
            metadata: ValidationMetadata::default(),
        }
    }

    /// Add an error
    pub fn with_error(mut self, error: ValidationError) -> Self {
        self.errors.push(error);
        self.valid = false;
        self
    }

    /// Add a warning
    pub fn with_warning(mut self, warning: ValidationWarning) -> Self {
        self.warnings.push(warning);
        self
    }

    /// Merge with another result
    pub fn merge(mut self, other: ValidationResult) -> Self {
        self.errors.extend(other.errors);
        self.warnings.extend(other.warnings);
        self.valid = self.valid && other.valid;
        self
    }

    /// Convert to Result type
    pub fn to_result(self) -> Result<()> {
        if self.valid {
            Ok(())
        } else {
            let msg = self
                .errors
                .iter()
                .map(|e| e.message.clone())
                .collect::<Vec<_>>()
                .join("; ");
            Err(Error::new(&format!("Validation failed: {}", msg)))
        }
    }
}

/// Validation error details
#[derive(Debug, Clone)]
pub struct ValidationError {
    /// Error code for programmatic handling
    pub code: ValidationErrorCode,
    /// Human-readable message
    pub message: String,
    /// Entry ID if applicable
    pub entry_id: Option<String>,
    /// Severity level
    pub severity: ErrorSeverity,
}

impl ValidationError {
    /// Create new validation error
    pub fn new(code: ValidationErrorCode, message: impl Into<String>) -> Self {
        Self {
            code,
            message: message.into(),
            entry_id: None,
            severity: ErrorSeverity::Error,
        }
    }

    /// Set entry ID
    pub fn with_entry(mut self, id: impl Into<String>) -> Self {
        self.entry_id = Some(id.into());
        self
    }
}

/// Validation error codes
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValidationErrorCode {
    /// Schema version mismatch
    SchemaVersionMismatch,
    /// Missing required field
    MissingField,
    /// Invalid integrity hash
    InvalidIntegrity,
    /// Circular dependency detected
    CircularDependency,
    /// Missing dependency
    MissingDependency,
    /// Duplicate entry
    DuplicateEntry,
    /// Invalid format
    InvalidFormat,
    /// PQC signature verification failed
    PqcVerificationFailed,
}

/// Error severity levels
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorSeverity {
    /// Warning - does not block
    Warning,
    /// Error - blocks validation
    Error,
    /// Critical - requires immediate attention
    Critical,
}

/// Validation warning (non-fatal)
#[derive(Debug, Clone)]
pub struct ValidationWarning {
    /// Warning code
    pub code: WarningCode,
    /// Human-readable message
    pub message: String,
    /// Entry ID if applicable
    pub entry_id: Option<String>,
}

impl ValidationWarning {
    /// Create new warning
    pub fn new(code: WarningCode, message: impl Into<String>) -> Self {
        Self {
            code,
            message: message.into(),
            entry_id: None,
        }
    }
}

/// Warning codes
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WarningCode {
    /// Deprecated schema version
    DeprecatedSchemaVersion,
    /// Missing optional field
    MissingOptionalField,
    /// Weak integrity algorithm
    WeakIntegrity,
    /// Unused entry
    UnusedEntry,
}

/// Validation metadata
#[derive(Debug, Clone, Default)]
pub struct ValidationMetadata {
    /// Number of entries validated
    pub entries_validated: usize,
    /// Validation duration in milliseconds
    pub duration_ms: u64,
    /// Schema version found
    pub schema_version: Option<u32>,
}

/// Integrity check result for a single entry
#[derive(Debug, Clone)]
pub struct IntegrityCheck {
    /// Entry ID
    pub id: String,
    /// Expected hash
    pub expected: String,
    /// Actual computed hash
    pub actual: String,
    /// Whether check passed
    pub passed: bool,
}

impl IntegrityCheck {
    /// Create new integrity check
    pub fn new(
        id: impl Into<String>, expected: impl Into<String>, actual: impl Into<String>,
    ) -> Self {
        let expected = expected.into();
        let actual = actual.into();
        let passed = expected == actual;
        Self {
            id: id.into(),
            expected,
            actual,
            passed,
        }
    }

    /// Create a passed check
    pub fn passed(id: impl Into<String>, hash: impl Into<String>) -> Self {
        let hash = hash.into();
        Self {
            id: id.into(),
            expected: hash.clone(),
            actual: hash,
            passed: true,
        }
    }
}

/// Validate a lockfile for internal consistency
pub fn validate_lockfile<L: Lockfile>(lockfile: &L) -> ValidationResult {
    let mut result = ValidationResult::ok();
    let start = std::time::Instant::now();

    // Validate schema version
    let schema_version = lockfile.schema_version();
    if schema_version == 0 {
        result = result.with_error(ValidationError::new(
            ValidationErrorCode::SchemaVersionMismatch,
            "Schema version must be > 0",
        ));
    }

    // Validate generated_at timestamp
    let generated_at = lockfile.generated_at();
    if generated_at.is_empty() {
        result = result.with_error(ValidationError::new(
            ValidationErrorCode::MissingField,
            "Missing generated_at timestamp",
        ));
    }

    // Validate entries
    // **FMEA Fix**: Use BTreeSet for deterministic iteration order
    let mut seen_ids = BTreeSet::new();
    let mut entries_validated = 0;

    for (id, entry) in lockfile.entries() {
        entries_validated += 1;

        // Check for duplicates
        if !seen_ids.insert(id.to_string()) {
            result = result.with_error(
                ValidationError::new(
                    ValidationErrorCode::DuplicateEntry,
                    format!("Duplicate entry: {}", id),
                )
                .with_entry(id),
            );
        }

        // Validate entry fields
        if entry.version().is_empty() {
            result = result.with_error(
                ValidationError::new(
                    ValidationErrorCode::MissingField,
                    format!("Entry '{}' missing version", id),
                )
                .with_entry(id),
            );
        }

        // Validate integrity hash format (should be 64 hex chars for SHA256)
        if let Some(integrity) = entry.integrity() {
            if !integrity.is_empty() && !is_valid_sha256(integrity) {
                result = result.with_warning(ValidationWarning {
                    code: WarningCode::WeakIntegrity,
                    message: format!(
                        "Entry '{}' has non-standard integrity format (expected 64 hex chars)",
                        id
                    ),
                    entry_id: Some(id.to_string()),
                });
            }
        }

        // Validate dependencies exist
        for dep_id in entry.dependencies() {
            if !lockfile.get(dep_id).is_some() && !seen_ids.contains(dep_id) {
                result = result.with_warning(ValidationWarning {
                    code: WarningCode::MissingOptionalField,
                    message: format!(
                        "Entry '{}' depends on '{}' which is not in lockfile",
                        id, dep_id
                    ),
                    entry_id: Some(id.to_string()),
                });
            }
        }
    }

    result.metadata = ValidationMetadata {
        entries_validated,
        duration_ms: start.elapsed().as_millis() as u64,
        schema_version: Some(schema_version),
    };

    result
}

/// Check for circular dependencies in a lockfile
pub fn check_circular_dependencies<L: Lockfile>(lockfile: &L) -> Result<()> {
    // Build dependency graph
    // **FMEA Fix**: Use BTreeMap for deterministic iteration order
    let mut graph: BTreeMap<String, Vec<String>> = BTreeMap::new();

    for (id, entry) in lockfile.entries() {
        graph.insert(id.to_string(), entry.dependencies().to_vec());
    }

    // DFS to detect cycles
    // **FMEA Fix**: Use BTreeSet for deterministic iteration order
    let mut visited = BTreeSet::new();
    let mut rec_stack = BTreeSet::new();

    for id in graph.keys() {
        if !visited.contains(id) {
            if let Some(cycle) =
                detect_cycle(&graph, id, &mut visited, &mut rec_stack, &mut Vec::new())
            {
                return Err(Error::new(&format!(
                    "Circular dependency detected: {}",
                    cycle.join(" -> ")
                )));
            }
        }
    }

    Ok(())
}

// DFS helper for cycle detection
fn detect_cycle(
    graph: &BTreeMap<String, Vec<String>>, node: &str, visited: &mut BTreeSet<String>,
    rec_stack: &mut BTreeSet<String>, path: &mut Vec<String>,
) -> Option<Vec<String>> {
    visited.insert(node.to_string());
    rec_stack.insert(node.to_string());
    path.push(node.to_string());

    if let Some(deps) = graph.get(node) {
        for dep in deps {
            if !visited.contains(dep) {
                if let Some(cycle) = detect_cycle(graph, dep, visited, rec_stack, path) {
                    return Some(cycle);
                }
            } else if rec_stack.contains(dep) {
                // Found cycle - return path from dep to current
                let cycle_start = path.iter().position(|x| x == dep).unwrap_or(0);
                let mut cycle: Vec<String> = path[cycle_start..].to_vec();
                cycle.push(dep.clone());
                return Some(cycle);
            }
        }
    }

    path.pop();
    rec_stack.remove(node);
    None
}

/// Verify integrity hashes for all entries
pub fn verify_integrity<L, F>(lockfile: &L, compute_hash: F) -> Vec<IntegrityCheck>
where
    L: Lockfile,
    F: Fn(&str, &L::Entry) -> String,
{
    let mut checks = Vec::new();

    for (id, entry) in lockfile.entries() {
        let expected = entry.integrity().unwrap_or("").to_string();
        if !expected.is_empty() {
            let actual = compute_hash(id, entry);
            checks.push(IntegrityCheck::new(id, expected, actual));
        }
    }

    checks
}

/// Check if string is valid SHA256 hex format
fn is_valid_sha256(s: &str) -> bool {
    s.len() == 64 && s.chars().all(|c| c.is_ascii_hexdigit())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validation_result_operations() {
        let result = ValidationResult::ok();
        assert!(result.valid);
        assert!(result.errors.is_empty());

        let result = result.with_error(ValidationError::new(
            ValidationErrorCode::MissingField,
            "Test error",
        ));
        assert!(!result.valid);
        assert_eq!(result.errors.len(), 1);
    }

    #[test]
    fn test_validation_result_merge() {
        let result1 = ValidationResult::ok().with_warning(ValidationWarning::new(
            WarningCode::UnusedEntry,
            "Warning 1",
        ));

        let result2 = ValidationResult::fail(vec![ValidationError::new(
            ValidationErrorCode::InvalidIntegrity,
            "Error 1",
        )]);

        let merged = result1.merge(result2);
        assert!(!merged.valid);
        assert_eq!(merged.errors.len(), 1);
        assert_eq!(merged.warnings.len(), 1);
    }

    #[test]
    fn test_integrity_check() {
        let check = IntegrityCheck::new("test", "abc123", "abc123");
        assert!(check.passed);

        let check = IntegrityCheck::new("test", "abc123", "def456");
        assert!(!check.passed);
    }

    #[test]
    fn test_is_valid_sha256() {
        assert!(is_valid_sha256(
            "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
        ));
        assert!(!is_valid_sha256("short"));
        assert!(!is_valid_sha256(
            "g3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
        )); // 'g' not hex
    }

    #[test]
    fn test_circular_dependency_detection() {
        // Simple graph with no cycles
        // **FMEA Fix**: Use BTreeMap for deterministic iteration order
        let mut graph: BTreeMap<String, Vec<String>> = BTreeMap::new();
        graph.insert("a".into(), vec!["b".into()]);
        graph.insert("b".into(), vec!["c".into()]);
        graph.insert("c".into(), vec![]);

        let mut visited = BTreeSet::new();
        let mut rec_stack = BTreeSet::new();

        assert!(detect_cycle(&graph, "a", &mut visited, &mut rec_stack, &mut Vec::new()).is_none());

        // Graph with cycle: a -> b -> c -> a
        let mut graph_cycle: BTreeMap<String, Vec<String>> = BTreeMap::new();
        graph_cycle.insert("a".into(), vec!["b".into()]);
        graph_cycle.insert("b".into(), vec!["c".into()]);
        graph_cycle.insert("c".into(), vec!["a".into()]);

        visited.clear();
        rec_stack.clear();

        assert!(detect_cycle(
            &graph_cycle,
            "a",
            &mut visited,
            &mut rec_stack,
            &mut Vec::new()
        )
        .is_some());
    }
}
