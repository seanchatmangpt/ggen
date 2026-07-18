//! Property definitions and checking logic
//!
//! Each property represents an invariant that must hold for valid ggen output.

use serde::{Deserialize, Serialize};
use std::fmt::{self, Display, Formatter};

/// Types of properties that agents validate
///
/// Unlike consensus (where agents vote on the same thing),
/// each agent validates a DIFFERENT property (complementary).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum PropertyType {
    /// Property 1: Determinism - same input always produces same output
    ///
    /// Invariant: f(x) = f(x) for all x (no randomness in generation)
    ///
    /// Checked by: Running generation twice with same input and comparing BLAKE3 hashes
    Determinism,

    /// Property 2: Completeness - all required fields/data present
    ///
    /// Invariant: ∀ required fields r ∈ output, r is present and non-null
    ///
    /// Checked by: Parsing output and verifying required fields exist
    Completeness,

    /// Property 3: Consistency - no contradictions in output
    ///
    /// Invariant: No two statements in output contradict each other
    ///
    /// Checked by: SPARQL queries to detect contradictory triples
    Consistency,

    /// Property 4: Soundness - no deadlocks, bounded resources
    ///
    /// Invariant: System is deadlock-free, has liveness, bounded memory
    ///
    /// Checked by: Formal verification or runtime analysis (WvdA soundness)
    Soundness,

    /// Property 5: Performance - meets latency/throughput SLOs
    ///
    /// Invariant: Generation time < threshold, memory usage < limit
    ///
    /// Checked by: Measuring generation metrics against SLO thresholds
    Performance,

    /// Property 6: Security - no vulnerabilities, proper encoding
    ///
    /// Invariant: No XSS, SQL injection, path traversal, etc.
    ///
    /// Checked by: Security scanning and input validation checks
    Security,

    /// Property 7: Correctness - output matches specification
    ///
    /// Invariant: Output conforms to schema and semantic specification
    ///
    /// Checked by: Schema validation and semantic checks
    Correctness,
}

impl Display for PropertyType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            PropertyType::Determinism => write!(f, "Determinism"),
            PropertyType::Completeness => write!(f, "Completeness"),
            PropertyType::Consistency => write!(f, "Consistency"),
            PropertyType::Soundness => write!(f, "Soundness"),
            PropertyType::Performance => write!(f, "Performance"),
            PropertyType::Security => write!(f, "Security"),
            PropertyType::Correctness => write!(f, "Correctness"),
        }
    }
}

/// Result of checking a single property
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PropertyResult {
    /// Which property was checked
    pub property_type: PropertyType,

    /// Whether the property passed (true) or was violated (false)
    pub passed: bool,

    /// List of violations found (empty if passed)
    pub violations: Vec<PropertyViolation>,

    /// Time taken to check this property
    pub check_duration_ms: u64,

    /// Agent ID that performed this check
    pub agent_id: String,

    /// Timestamp when check was performed
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

/// A single violation of a property invariant
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PropertyViolation {
    /// Description of what was violated
    pub description: String,

    /// Location where violation was found (file, line, etc.)
    pub location: Option<String>,

    /// Severity of violation (critical, high, medium, low)
    pub severity: ViolationSeverity,

    /// Suggested fix for this violation
    pub suggestion: Option<String>,
}

/// Severity levels for property violations
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ViolationSeverity {
    /// Violation makes system unusable (must fix)
    Critical,

    /// Violation is a serious issue (should fix)
    High,

    /// Violation is a minor issue (nice to fix)
    Medium,

    /// Violation is cosmetic (optional fix)
    Low,
}

/// Trait for property checking logic
pub trait PropertyCheck: Send + Sync {
    /// Check if this property holds for the given package
    ///
    /// # Arguments
    /// * `package_path` - Path to ggen-generated package
    /// * `generation_receipt` - BLAKE3 receipt from ggen μ₅
    ///
    /// # Returns
    /// Property check result with violations if any
    fn check(
        &self,
        package_path: &str,
        generation_receipt: &str,
    ) -> impl Future<Output = Result<PropertyResult, Box<dyn std::error::Error>>> + Send;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_property_type_display() {
        assert_eq!(format!("{}", PropertyType::Determinism), "Determinism");
        assert_eq!(format!("{}", PropertyType::Completeness), "Completeness");
        assert_eq!(format!("{}", PropertyType::Consistency), "Consistency");
        assert_eq!(format!("{}", PropertyType::Soundness), "Soundness");
        assert_eq!(format!("{}", PropertyType::Performance), "Performance");
        assert_eq!(format!("{}", PropertyType::Security), "Security");
        assert_eq!(format!("{}", PropertyType::Correctness), "Correctness");
    }

    #[test]
    fn test_violation_severity_ordering() {
        // Critical > High > Medium > Low
        assert!(ViolationSeverity::Critical > ViolationSeverity::High);
        assert!(ViolationSeverity::High > ViolationSeverity::Medium);
        assert!(ViolationSeverity::Medium > ViolationSeverity::Low);
    }
}
