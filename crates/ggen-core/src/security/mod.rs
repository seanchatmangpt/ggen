//! Security module for input validation, command execution safety, and error sanitization
//!
//! This module provides comprehensive security mechanisms to prevent:
//! - Command injection attacks
//! - Path traversal vulnerabilities
//! - Information disclosure via error messages
//! - Malicious input exploitation
//! - Template injection attacks
//!
//! ## Week 3 Security Hardening: Template Security
//!
//! - Sandboxed Tera environments with function whitelisting
//! - Template variable validation (alphanumeric + underscore)
//! - Context-aware output escaping (HTML, SQL, Shell)
//! - Template size limits (1MB maximum)
//! - Path traversal prevention in includes
//!
//! ## Week 4 Security Hardening
//!
//! Target: 82% → 85% security health improvement
//!
//! Fixed issues:
//! 1. Panic in library code → Result-based error handling
//! 2. Unwrap() usage → Proper error propagation
//! 3. Command injection → Safe command execution
//! 4. Input validation → Comprehensive validation functions
//! 5. Error message leakage → Sanitized error messages
//!
//! ## Week 10 Security Logging & Intrusion Detection (v6.0.0)
//!
//! New capabilities:
//! 1. Comprehensive security event logging with structured data
//! 2. Immutable audit trail with Merkle tree tamper-proofing
//! 3. Intrusion detection with pattern matching for common attacks
//! 4. Security metrics collection and aggregation
//! 5. Real-time alerting for critical security events
//!
//! ### Usage Example
//!
//! ```rust,no_run
//! use ggen_core::security::logging::SecurityLogger;
//! use ggen_core::security::events::{SecurityEvent, SecuritySeverity, EventCategory};
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let mut logger = SecurityLogger::new()?;
//!
//! // Log security events
//! let event = SecurityEvent::new(
//!     SecuritySeverity::High,
//!     EventCategory::Authentication,
//!     "Failed login attempt"
//! );
//! logger.log(event)?;
//!
//! // Analyze input for attacks
//! if let Some(attack_event) = logger.analyze_input("SELECT * FROM users")? {
//!     println!("Attack detected: {:?}", attack_event.attack_pattern);
//! }
//!
//! // Get security metrics
//! if let Some(metrics) = logger.get_metrics_for_last_hour() {
//!     println!("Total attacks: {}", metrics.total_attacks);
//! }
//! # Ok(())
//! # }
//! ```
//!
//! ## Security Scanning Infrastructure
//!
//! New capabilities for vulnerability scanning and compliance checking:
//! 1. Vulnerability scanning with cargo audit integration
//! 2. Docker image scanning with Trivy
//! 3. SARIF output generation for GitHub Security tab
//! 4. Compliance checking for code quality standards
//! 5. Detection of unwrap/expect in production code
//! 6. SPARQL injection prevention validation
//!
//! ### Vulnerability Scanning Example
//!
//! ```rust,no_run
//! use ggen_core::security::{VulnerabilityScanner, ScanConfig};
//! use std::path::PathBuf;
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let config = ScanConfig {
//!     project_root: PathBuf::from("."),
//!     fail_on_high_severity: true,
//!     scan_docker: false,
//!     docker_image: None,
//! };
//!
//! let scanner = VulnerabilityScanner::new(config);
//! let results = scanner.scan()?;
//!
//! println!("Found {} vulnerabilities", results.total_count());
//! println!("Critical: {}", results.critical_count);
//! println!("High: {}", results.high_count);
//!
//! // Generate SARIF for GitHub Security
//! scanner.write_sarif(&results, &PathBuf::from("results.sarif"))?;
//! # Ok(())
//! # }
//! ```
//!
//! ### Compliance Checking Example
//!
//! ```rust,no_run
//! use ggen_core::security::{ComplianceChecker, ComplianceConfig};
//! use std::path::PathBuf;
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let config = ComplianceConfig {
//!     root_dir: PathBuf::from("."),
//!     include_patterns: vec!["**/*.rs".to_string()],
//!     exclude_patterns: vec!["**/target/**".to_string(), "**/tests/**".to_string()],
//!     check_unwrap: true,
//!     check_sparql_injection: true,
//!     check_atom_exhaustion: false,
//! };
//!
//! let checker = ComplianceChecker::new(config)?;
//! let results = checker.check()?;
//!
//! println!("Found {} violations", results.total_count());
//! println!("Critical: {}", results.critical_count);
//! println!("High: {}", results.high_count);
//!
//! // Generate compliance report
//! let report = checker.generate_report(&results);
//! println!("{}", report);
//! # Ok(())
//! # }
//! ```

pub mod command;
pub mod error;
pub mod template_secure;
pub mod validation;

// Week 10: Security logging and intrusion detection
pub mod alerting;
pub mod audit_trail;
pub mod events;
pub mod intrusion_detection;
pub mod logging;
pub mod metrics;

// Security scanning and compliance
pub mod compliance_checker;
pub mod vulnerability_scanner;

pub use command::{CommandError, CommandExecutor, SafeCommand};
pub use error::{ErrorSanitizer, SanitizedError};
pub use template_secure::{
    ContextEscaper, SecureTeraEnvironment, TemplateSandbox, TemplateSecurityError,
    TemplateValidator, MAX_TEMPLATE_SIZE, MAX_VARIABLE_NAME_LENGTH,
};
pub use validation::{EnvVarValidator, InputValidator, PathValidator, ValidationError};

// Week 10 exports
pub use alerting::{Alert, AlertHandler, AlertManager, AlertSeverity};
pub use audit_trail::{AuditEntry, AuditError, AuditTrail, MerkleProof};
pub use events::{AttackPattern, EventCategory, SecurityEvent, SecuritySeverity};
pub use intrusion_detection::{DetectionError, IntrusionDetector, PatternMatcher, RateLimiter};
pub use logging::{LoggingError, SecurityLogger, SecurityLoggerConfig};
pub use metrics::{MetricsCollector, MetricsError, SecurityMetrics, TimeWindow};

// Security scanning and compliance exports
pub use compliance_checker::{
    ComplianceChecker, ComplianceConfig, ComplianceResults, Violation, ViolationSeverity,
    ViolationType,
};
pub use vulnerability_scanner::{
    ScanConfig, ScanResults, Severity, Vulnerability, VulnerabilityScanner,
};

#[cfg(test)]
mod tests;
