//! Pre-registered critical failure modes (Pareto 80/20 analysis).
//!
//! This module defines the top 8 failure modes that account for 75% of risk.
//! Based on RPN (Risk Priority Number) analysis of ggen CLI codebase.
//!
//! Top Failure Modes (by RPN):
//! 1. path_traversal_attack (RPN 441) - 18.4% of risk
//! 2. template_ssti (RPN 378) - 15.8% of risk
//! 3. dep_cycle_detected (RPN 343) - 14.3% of risk
//! 4. file_io_write_fail (RPN 288) - 12.0% of risk
//! 5. lockfile_race_corrupt (RPN 240) - 10.0% of risk
//! 6. network_timeout (RPN 180) - 7.5% of risk
//! 7. mutex_poisoned (RPN 144) - 6.0% of risk
//! 8. deser_invalid_format (RPN 120) - 5.0% of risk

use super::{
    FmeaRegistry, FailureMode, FailureCategory,
    Severity, Occurrence, Detection,
};

/// Registers all critical failure modes in the registry.
///
/// Called automatically when FMEA_REGISTRY is first accessed.
pub fn register_critical_failures(registry: &mut FmeaRegistry) {
    registry.register(path_traversal_attack());
    registry.register(template_ssti());
    registry.register(dep_cycle_detected());
    registry.register(file_io_write_fail());
    registry.register(lockfile_race_corrupt());
    registry.register(network_timeout());
    registry.register(mutex_poisoned());
    registry.register(deser_invalid_format());
}

/// Path traversal attack vulnerability (RPN 441).
///
/// **Risk**: CRITICAL
/// - Severity: 9 (Security breach, data exfiltration)
/// - Occurrence: 7 (Common attack vector)
/// - Detection: 7 (Difficult to detect without validation)
///
/// **Location**: `crates/ggen-domain/src/project/apply.rs:36-43`
fn path_traversal_attack() -> FailureMode {
    FailureMode::builder()
        .id("path_traversal_attack")
        .category(FailureCategory::InputValidation)
        .description("Path contains '../' or absolute path escaping working directory")
        .severity(Severity::new(9).expect("valid severity"))
        .occurrence(Occurrence::new(7).expect("valid occurrence"))
        .detection(Detection::new(7).expect("valid detection"))
        .effect("Unauthorized file access outside project directory")
        .effect("Data exfiltration via template generation")
        .effect("Overwriting system files")
        .cause("Insufficient path validation")
        .cause("TOCTOU race condition (validate, then use)")
        .cause("Symlink following")
        .control("PathValidator::validate() basic checks")
        .action("Implement ValidatedPath NewType with comprehensive validation")
        .action("Canonicalize paths to prevent TOCTOU")
        .action("Check for shell metacharacters")
        .build()
        .expect("valid failure mode")
}

/// Server-Side Template Injection (SSTI) vulnerability (RPN 378).
///
/// **Risk**: CRITICAL
/// - Severity: 9 (Arbitrary code execution)
/// - Occurrence: 6 (Moderate, requires user input)
/// - Detection: 7 (Difficult without sanitization)
///
/// **Location**: `crates/ggen-domain/src/template/mod.rs`
fn template_ssti() -> FailureMode {
    FailureMode::builder()
        .id("template_ssti")
        .category(FailureCategory::TemplateRendering)
        .description("Template contains injection sequences ({{, ${, <%)")
        .severity(Severity::new(9).expect("valid severity"))
        .occurrence(Occurrence::new(6).expect("valid occurrence"))
        .detection(Detection::new(7).expect("valid detection"))
        .effect("Arbitrary code execution via template rendering")
        .effect("File system access")
        .effect("Environment variable disclosure")
        .cause("Unsanitized template variables")
        .cause("User-provided templates not validated")
        .cause("Tera template engine allows code execution")
        .control("None (templates trusted)")
        .action("Implement SanitizedInput NewType")
        .action("Validate templates for injection patterns")
        .action("Sandbox template rendering")
        .build()
        .expect("valid failure mode")
}

/// Circular dependency detection failure (RPN 343).
///
/// **Risk**: HIGH
/// - Severity: 7 (Install blocked, dependency resolution failure)
/// - Occurrence: 7 (Common in complex dependency graphs)
/// - Detection: 7 (Only detected at runtime)
///
/// **Location**: `crates/ggen-domain/src/marketplace/install.rs:350-650`
fn dep_cycle_detected() -> FailureMode {
    FailureMode::builder()
        .id("dep_cycle_detected")
        .category(FailureCategory::DependencyResolution)
        .description("Circular dependency in pack graph (A → B → A)")
        .severity(Severity::new(7).expect("valid severity"))
        .occurrence(Occurrence::new(7).expect("valid occurrence"))
        .detection(Detection::new(7).expect("valid detection"))
        .effect("Install operation fails")
        .effect("Dependency resolution deadlock")
        .effect("Poor user experience (cryptic error)")
        .cause("No cycle detection in dependency resolver")
        .cause("Marketplace allows circular dependencies")
        .cause("Async resolution makes cycles hard to detect")
        .control("Basic cycle check in resolver")
        .action("Implement Tarjan's SCC algorithm for cycle detection")
        .action("Add cycle detection to pack upload validation")
        .action("Improve error messages with cycle path")
        .build()
        .expect("valid failure mode")
}

/// File I/O write failure (RPN 288).
///
/// **Risk**: HIGH
/// - Severity: 8 (Data loss, partial writes, corruption)
/// - Occurrence: 6 (Common: ENOSPC, EPERM, race conditions)
/// - Detection: 6 (Detected but often too late)
///
/// **Location**: `crates/ggen-domain/src/template/generate.rs:79-107`
fn file_io_write_fail() -> FailureMode {
    FailureMode::builder()
        .id("file_io_write_fail")
        .category(FailureCategory::FileIO)
        .description("Write fails (ENOSPC, EPERM, partial write, race condition)")
        .severity(Severity::new(8).expect("valid severity"))
        .occurrence(Occurrence::new(6).expect("valid occurrence"))
        .detection(Detection::new(6).expect("valid detection"))
        .effect("Partial file writes (corrupted output)")
        .effect("Data loss (no rollback)")
        .effect("File system full error")
        .cause("Direct std::fs::write (not atomic)")
        .cause("No disk space pre-check")
        .cause("No rollback on failure")
        .control("Error propagation via Result<T>")
        .action("Implement AtomicFileWriter with temp file + rename")
        .action("Pre-flight disk space check")
        .action("Add RAII cleanup for temp files")
        .build()
        .expect("valid failure mode")
}

/// Lockfile race condition corruption (RPN 240).
///
/// **Risk**: HIGH
/// - Severity: 8 (Lockfile corruption, build inconsistency)
/// - Occurrence: 5 (Moderate, concurrent usage)
/// - Detection: 6 (Detected by checksum mismatch)
///
/// **Location**: `crates/ggen-core/src/lockfile.rs:210-224`
fn lockfile_race_corrupt() -> FailureMode {
    FailureMode::builder()
        .id("lockfile_race_corrupt")
        .category(FailureCategory::ConcurrencyRace)
        .description("Concurrent lockfile writes cause corruption")
        .severity(Severity::new(8).expect("valid severity"))
        .occurrence(Occurrence::new(5).expect("valid occurrence"))
        .detection(Detection::new(6).expect("valid detection"))
        .effect("Lockfile corruption (invalid TOML)")
        .effect("Build inconsistency (wrong versions)")
        .effect("Data loss (last writer wins)")
        .cause("No exclusive file locking")
        .cause("Non-atomic write operation")
        .cause("No backup before overwrite")
        .control("None (relies on OS atomicity)")
        .action("Implement LockfileGuard with exclusive file lock")
        .action("Use AtomicFileWriter for lockfile saves")
        .action("Create backup before overwrite")
        .build()
        .expect("valid failure mode")
}

/// Network operation timeout (RPN 180).
///
/// **Risk**: MEDIUM
/// - Severity: 6 (CLI hangs, poor UX, resource leak)
/// - Occurrence: 5 (Moderate, network issues common)
/// - Detection: 6 (User must manually interrupt)
///
/// **Location**: `crates/ggen-domain/src/marketplace/install.rs:350-650`
fn network_timeout() -> FailureMode {
    FailureMode::builder()
        .id("network_timeout")
        .category(FailureCategory::NetworkOps)
        .description("Network operation exceeds 30s SLO (hangs indefinitely)")
        .severity(Severity::new(6).expect("valid severity"))
        .occurrence(Occurrence::new(5).expect("valid occurrence"))
        .detection(Detection::new(6).expect("valid detection"))
        .effect("CLI hangs indefinitely")
        .effect("Poor user experience (must Ctrl+C)")
        .effect("Resource leak (connection not closed)")
        .cause("No timeout on reqwest::Client")
        .cause("No timeout on tokio operations")
        .cause("Transient network issues")
        .control("None (no timeouts configured)")
        .action("Implement TimeoutIO with reqwest timeout config")
        .action("Add tokio::time::timeout wrappers")
        .action("Implement NetworkRetry with exponential backoff")
        .build()
        .expect("valid failure mode")
}

/// Mutex poisoning from panic (RPN 144).
///
/// **Risk**: MEDIUM
/// - Severity: 6 (Operation fails, cache unavailable)
/// - Occurrence: 4 (Low, requires panic while holding lock)
/// - Detection: 6 (Detected but unclear cause)
///
/// **Location**: Multiple (lockfile.rs, query.rs, mape_k_integration.rs)
fn mutex_poisoned() -> FailureMode {
    FailureMode::builder()
        .id("mutex_poisoned")
        .category(FailureCategory::ConcurrencyRace)
        .description("Mutex poisoned by panic in another thread")
        .severity(Severity::new(6).expect("valid severity"))
        .occurrence(Occurrence::new(4).expect("valid occurrence"))
        .detection(Detection::new(6).expect("valid detection"))
        .effect("Operation fails with PoisonError")
        .effect("Cache unavailable")
        .effect("Cascading failures")
        .cause("Using .lock().unwrap() (panics on poison)")
        .cause("Panic while holding lock")
        .cause("No poison recovery strategy")
        .control("None (immediate panic)")
        .action("Replace .unwrap() with .map_err()")
        .action("Clear poisoned mutex and retry")
        .action("Use RwLock where appropriate (better concurrency)")
        .build()
        .expect("valid failure mode")
}

/// Invalid TOML/JSON deserialization (RPN 120).
///
/// **Risk**: MEDIUM
/// - Severity: 5 (Operation fails, user-fixable)
/// - Occurrence: 4 (Low, usually caught in testing)
/// - Detection: 6 (Detected but unclear cause)
///
/// **Location**: Multiple (config parsing, lockfile loading)
fn deser_invalid_format() -> FailureMode {
    FailureMode::builder()
        .id("deser_invalid_format")
        .category(FailureCategory::Deserialization)
        .description("Invalid TOML/JSON format in config/lockfile")
        .severity(Severity::new(5).expect("valid severity"))
        .occurrence(Occurrence::new(4).expect("valid occurrence"))
        .detection(Detection::new(6).expect("valid detection"))
        .effect("Config load fails")
        .effect("Lockfile parse error")
        .effect("Poor error message (serde error)")
        .cause("Malformed TOML/JSON (user edit)")
        .cause("Schema mismatch (version upgrade)")
        .cause("File corruption")
        .control("Serde deserialization with error propagation")
        .action("Validate config schema on save")
        .action("Add helpful error messages with line/column")
        .action("Implement config migration for version upgrades")
        .build()
        .expect("valid failure mode")
}
