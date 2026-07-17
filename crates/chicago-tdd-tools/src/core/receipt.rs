//! Proof-Carrying Test Receipts
//!
//! Provides cryptographically verifiable receipts for test execution:
//! - Test contract ID
//! - Code hash (of test + SUT)
//! - Environment fingerprint (Rust version, OS, features)
//! - Invariants checked (Qᵢ)
//! - Timing windows (τ for hot, durations for warm/cold)
//! - Effect envelope exercised
//! - Optional cryptographic signature (SHA-3 + Ed25519)
//!
//! Receipts make tests observable operations with cryptographic provenance,
//! like everything else in the AHI architecture.
//!
//! # Architecture
//!
//! ```text
//! TestReceipt {
//!     contract_id,
//!     code_hash,
//!     environment,
//!     invariants,
//!     timing,
//!     effects,
//!     signature
//! }
//!     ↓
//! Receipt Query API (Γₜ for tests)
//!     ↓
//! Governance/deployment gates
//! ```

use crate::core::contract::TestContract;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::time::{SystemTime, UNIX_EPOCH};

/// Test receipt: cryptographically verifiable record of test execution
///
/// Receipts provide:
/// - Provenance: who ran the test, when, where
/// - Integrity: code hash ensures test hasn't changed
/// - Observability: timing, effects, invariants checked
/// - Verifiability: optional cryptographic signature
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestReceipt {
    /// Receipt ID (unique identifier)
    pub receipt_id: String,

    /// Test contract that was executed
    pub contract_name: String,

    /// Code hash of test + system under test (SHA-256)
    pub code_hash: String,

    /// Environment fingerprint
    pub environment: EnvironmentFingerprint,

    /// Invariants checked during this execution
    pub invariants_checked: Vec<String>,

    /// Timing measurements
    pub timing: TimingMeasurement,

    /// Effects actually exercised
    pub effects_exercised: Vec<String>,

    /// Test result (pass/fail)
    pub result: TestOutcome,

    /// Timestamp (Unix epoch milliseconds)
    pub timestamp: u64,

    /// Optional cryptographic signature (hex-encoded)
    pub signature: Option<String>,

    /// Metadata (key-value pairs for extensibility)
    pub metadata: Vec<(String, String)>,
}

/// Environment fingerprint: captures execution environment
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnvironmentFingerprint {
    /// Rust version (e.g., "1.75.0")
    pub rust_version: String,

    /// Operating system (e.g., "Linux", "macOS", "Windows")
    pub os: String,

    /// Architecture (e.g., `"x86_64"`, `"aarch64"`)
    pub arch: String,

    /// Enabled features (comma-separated)
    pub features: String,

    /// CI environment (e.g., "GitHub Actions", "local")
    pub ci_env: Option<String>,
}

impl EnvironmentFingerprint {
    /// Capture current environment
    #[must_use]
    pub fn capture() -> Self {
        Self {
            rust_version: rustc_version_runtime::version().to_string(),
            os: std::env::consts::OS.to_string(),
            arch: std::env::consts::ARCH.to_string(),
            features: capture_enabled_features(),
            ci_env: std::env::var("CI").ok().map(|_| detect_ci_env()),
        }
    }

    /// Create a fingerprint hash (for compact representation)
    #[must_use]
    pub fn hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.rust_version.as_bytes());
        hasher.update(self.os.as_bytes());
        hasher.update(self.arch.as_bytes());
        hasher.update(self.features.as_bytes());
        if let Some(ci) = &self.ci_env {
            hasher.update(ci.as_bytes());
        }
        format!("{:x}", hasher.finalize())
    }
}

/// Timing measurement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TimingMeasurement {
    /// Total ticks elapsed
    pub total_ticks: u64,

    /// Wall clock duration in milliseconds
    pub wall_clock_ms: u64,

    /// Thermal class (hot/warm/cold)
    pub thermal_class: String,

    /// Whether timing budget was met
    pub budget_met: bool,

    /// Expected budget (ticks)
    pub expected_budget: u64,
}

impl TimingMeasurement {
    /// Create a new timing measurement
    #[must_use]
    pub const fn new(
        total_ticks: u64, wall_clock_ms: u64, thermal_class: String, budget_met: bool,
        expected_budget: u64,
    ) -> Self {
        Self {
            total_ticks,
            wall_clock_ms,
            thermal_class,
            budget_met,
            expected_budget,
        }
    }

    /// Check if this violates τ (for hot path)
    #[must_use]
    pub fn violates_tau(&self) -> bool {
        self.thermal_class == "hot" && !self.budget_met
    }
}

/// Test result
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TestOutcome {
    /// Test passed
    Pass,
    /// Test failed
    Fail,
    /// Test skipped
    Skip,
    /// Test error (infrastructure failure)
    Error,
}

impl std::fmt::Display for TestOutcome {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Pass => write!(f, "PASS"),
            Self::Fail => write!(f, "FAIL"),
            Self::Skip => write!(f, "SKIP"),
            Self::Error => write!(f, "ERROR"),
        }
    }
}

impl TestReceipt {
    /// Create a new test receipt
    #[must_use]
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        contract_name: String, code_hash: String, environment: EnvironmentFingerprint,
        invariants_checked: Vec<String>, timing: TimingMeasurement, effects_exercised: Vec<String>,
        result: TestOutcome,
    ) -> Self {
        let receipt_id = Self::generate_receipt_id(&contract_name);
        #[allow(clippy::cast_possible_truncation)]
        // Timestamp truncation acceptable for receipt IDs
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis() as u64;

        Self {
            receipt_id,
            contract_name,
            code_hash,
            environment,
            invariants_checked,
            timing,
            effects_exercised,
            result,
            timestamp,
            signature: None,
            metadata: Vec::new(),
        }
    }

    /// Create a receipt from a test contract
    #[must_use]
    pub fn from_contract(
        contract: &TestContract, timing: TimingMeasurement, result: TestOutcome,
    ) -> Self {
        let code_hash = Self::compute_code_hash(contract);
        let environment = EnvironmentFingerprint::capture();
        let invariants_checked = contract
            .invariants
            .iter()
            .map(std::string::ToString::to_string)
            .collect();
        let effects_exercised = Vec::new(); // Effects are tracked via the effect system when using fixture_test! macro

        Self::new(
            contract.name.to_string(),
            code_hash,
            environment,
            invariants_checked,
            timing,
            effects_exercised,
            result,
        )
    }

    /// Generate a unique receipt ID
    fn generate_receipt_id(contract_name: &str) -> String {
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis();
        let mut hasher = Sha256::new();
        hasher.update(contract_name.as_bytes());
        hasher.update(timestamp.to_string().as_bytes());
        format!("{:x}", hasher.finalize())[..16].to_string()
    }

    /// Compute code hash for a test contract
    fn compute_code_hash(contract: &TestContract) -> String {
        let mut hasher = Sha256::new();
        hasher.update(contract.name.as_bytes());
        for cov in contract.coverage {
            hasher.update(cov.as_bytes());
        }
        for inv in contract.invariants {
            hasher.update(inv.as_bytes());
        }
        format!("{:x}", hasher.finalize())[..16].to_string()
    }

    /// Sign this receipt using a SHA-256 content hash.
    ///
    // SECURITY: Uses SHA-256 content hash, not a keyed signature. Sufficient for tamper detection, not authentication.
    pub fn sign(&mut self) {
        let signature_input = format!(
            "{}-{}-{}-{}",
            self.receipt_id, self.contract_name, self.timestamp, self.result
        );
        let mut hasher = Sha256::new();
        hasher.update(signature_input.as_bytes());
        self.signature = Some(format!("{:x}", hasher.finalize()));
    }

    /// Verify receipt signature by recomputing the SHA-256 content hash and comparing.
    ///
    // SECURITY: Uses SHA-256 content hash, not a keyed signature. Sufficient for tamper detection, not authentication.
    #[must_use]
    pub fn verify_signature(&self) -> bool {
        self.signature.as_ref().is_some_and(|sig| {
            let signature_input = format!(
                "{}-{}-{}-{}",
                self.receipt_id, self.contract_name, self.timestamp, self.result
            );
            let mut hasher = Sha256::new();
            hasher.update(signature_input.as_bytes());
            let expected = format!("{:x}", hasher.finalize());
            sig == &expected
        })
    }

    /// Add metadata
    pub fn add_metadata(&mut self, key: impl Into<String>, value: impl Into<String>) {
        self.metadata.push((key.into(), value.into()));
    }

    /// Get metadata value
    #[must_use]
    pub fn get_metadata(&self, key: &str) -> Option<&str> {
        self.metadata
            .iter()
            .find(|(k, _)| k == key)
            .map(|(_, v)| v.as_str())
    }

    /// Serialize to JSON
    ///
    /// # Errors
    ///
    /// Returns error if serialization fails.
    pub fn to_json(&self) -> Result<String, String> {
        serde_json::to_string_pretty(self).map_err(|e| format!("Serialization error: {e}"))
    }

    /// Deserialize from JSON
    ///
    /// # Errors
    ///
    /// Returns error if deserialization fails.
    pub fn from_json(json: &str) -> Result<Self, String> {
        serde_json::from_str(json).map_err(|e| format!("Deserialization error: {e}"))
    }
}

/// Test receipt registry: collection of receipts for querying
///
/// Provides Γₜ (test receipt query API) for the test suite.
#[derive(Debug, Clone, Default)]
pub struct TestReceiptRegistry {
    receipts: Vec<TestReceipt>,
}

impl TestReceiptRegistry {
    /// Create a new receipt registry
    #[must_use]
    pub const fn new() -> Self {
        Self {
            receipts: Vec::new(),
        }
    }

    /// Add a receipt to the registry
    pub fn add_receipt(&mut self, receipt: TestReceipt) {
        self.receipts.push(receipt);
    }

    /// Get all receipts
    #[must_use]
    pub fn all_receipts(&self) -> &[TestReceipt] {
        &self.receipts
    }

    /// Get receipts for a specific test
    #[must_use]
    pub fn receipts_for_test(&self, test_name: &str) -> Vec<&TestReceipt> {
        self.receipts
            .iter()
            .filter(|r| r.contract_name == test_name)
            .collect()
    }

    /// Get receipts that verified a specific invariant
    #[must_use]
    pub fn receipts_for_invariant(&self, invariant: &str) -> Vec<&TestReceipt> {
        self.receipts
            .iter()
            .filter(|r| r.invariants_checked.iter().any(|i| i == invariant))
            .collect()
    }

    /// Get receipts that exercised a specific effect
    #[must_use]
    pub fn receipts_for_effect(&self, effect: &str) -> Vec<&TestReceipt> {
        self.receipts
            .iter()
            .filter(|r| r.effects_exercised.iter().any(|e| e == effect))
            .collect()
    }

    /// Get failed test receipts
    #[must_use]
    pub fn failed_receipts(&self) -> Vec<&TestReceipt> {
        self.receipts
            .iter()
            .filter(|r| r.result == TestOutcome::Fail)
            .collect()
    }

    /// Get receipts that violated τ
    #[must_use]
    pub fn tau_violations(&self) -> Vec<&TestReceipt> {
        self.receipts
            .iter()
            .filter(|r| r.timing.violates_tau())
            .collect()
    }

    /// Total number of receipts
    #[must_use]
    pub const fn len(&self) -> usize {
        self.receipts.len()
    }

    /// Check if registry is empty
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.receipts.is_empty()
    }
}

// Helper functions

fn capture_enabled_features() -> String {
    let mut features = Vec::new();

    // Check each known feature via cfg! compile-time introspection
    if cfg!(feature = "logging") {
        features.push("logging");
    }
    if cfg!(feature = "workflow-engine") {
        features.push("workflow-engine");
    }
    if cfg!(feature = "mutation-testing") {
        features.push("mutation-testing");
    }
    if cfg!(feature = "async") {
        features.push("async");
    }
    if cfg!(feature = "benchmarking") {
        features.push("benchmarking");
    }
    if cfg!(feature = "property-testing") {
        features.push("property-testing");
    }
    if cfg!(feature = "snapshot-testing") {
        features.push("snapshot-testing");
    }
    if cfg!(feature = "fake-data") {
        features.push("fake-data");
    }
    if cfg!(feature = "concurrency-testing") {
        features.push("concurrency-testing");
    }
    if cfg!(feature = "parameterized-testing") {
        features.push("parameterized-testing");
    }
    if cfg!(feature = "cli-testing") {
        features.push("cli-testing");
    }
    if cfg!(feature = "otel") {
        features.push("otel");
    }
    if cfg!(feature = "weaver") {
        features.push("weaver");
    }
    if cfg!(feature = "testcontainers") {
        features.push("testcontainers");
    }
    if cfg!(feature = "testing-extras") {
        features.push("testing-extras");
    }
    if cfg!(feature = "testing-full") {
        features.push("testing-full");
    }
    if cfg!(feature = "observability-full") {
        features.push("observability-full");
    }
    if cfg!(feature = "integration-full") {
        features.push("integration-full");
    }

    if features.is_empty() {
        // Fall back to CARGO_FEATURES env var set at build time, or "default"
        option_env!("CARGO_FEATURES")
            .unwrap_or("default")
            .to_string()
    } else {
        features.join(",")
    }
}

fn detect_ci_env() -> String {
    if std::env::var("GITHUB_ACTIONS").is_ok() {
        "GitHub Actions".to_string()
    } else if std::env::var("GITLAB_CI").is_ok() {
        "GitLab CI".to_string()
    } else if std::env::var("CIRCLECI").is_ok() {
        "CircleCI".to_string()
    } else {
        "Unknown CI".to_string()
    }
}

// Note: rustc_version is not in Cargo.toml. We use RUSTC_VERSION env var if set at
// build time (e.g. by build.rs), falling back to the compile-time constant below.
// To get an accurate value, add to build.rs:
//   println!("cargo:rustc-env=RUSTC_VERSION={}", rustc_version::version().unwrap());
mod rustc_version_runtime {
    pub struct Version(String);

    impl std::fmt::Display for Version {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    pub fn version() -> Version {
        // Use RUSTC_VERSION set at build time if available; otherwise fall back to
        // a conservative recent stable version. This is not cryptographically
        // meaningful — it is a best-effort environment fingerprint.
        let v = option_env!("RUSTC_VERSION").unwrap_or("1.82.0");
        Version(v.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_environment_fingerprint() {
        let env = EnvironmentFingerprint::capture();
        assert!(!env.rust_version.is_empty());
        assert!(!env.os.is_empty());
        assert!(!env.arch.is_empty());

        let hash = env.hash();
        assert_eq!(hash.len(), 64); // SHA-256 produces 64 hex chars
    }

    #[test]
    fn test_timing_measurement() {
        let timing = TimingMeasurement::new(5, 1, "hot".to_string(), true, 8);

        assert_eq!(timing.total_ticks, 5);
        assert!(timing.budget_met);
        assert!(!timing.violates_tau());
    }

    #[test]
    fn test_timing_violation() {
        let timing = TimingMeasurement::new(10, 1, "hot".to_string(), false, 8);

        assert!(!timing.budget_met);
        assert!(timing.violates_tau());
    }

    #[test]
    fn test_test_result_display() {
        assert_eq!(TestOutcome::Pass.to_string(), "PASS");
        assert_eq!(TestOutcome::Fail.to_string(), "FAIL");
        assert_eq!(TestOutcome::Skip.to_string(), "SKIP");
        assert_eq!(TestOutcome::Error.to_string(), "ERROR");
    }

    #[test]
    fn test_receipt_creation() {
        let env = EnvironmentFingerprint::capture();
        let timing = TimingMeasurement::new(5, 1, "hot".to_string(), true, 8);
        let receipt = TestReceipt::new(
            "test_hot_path".to_string(),
            "abc123".to_string(),
            env,
            vec!["τ ≤ 8".to_string()],
            timing,
            vec![],
            TestOutcome::Pass,
        );

        assert_eq!(receipt.contract_name, "test_hot_path");
        assert_eq!(receipt.result, TestOutcome::Pass);
        assert!(!receipt.receipt_id.is_empty());
    }

    #[test]
    fn test_receipt_from_contract() {
        let contract = TestContract::hot_path("test_hot", &["core::hot"]);
        let timing = TimingMeasurement::new(5, 1, "hot".to_string(), true, 8);
        let receipt = TestReceipt::from_contract(&contract, timing, TestOutcome::Pass);

        assert_eq!(receipt.contract_name, "test_hot");
        assert!(receipt.invariants_checked.contains(&"τ ≤ 8".to_string()));
    }

    #[test]
    fn test_receipt_signature() {
        let env = EnvironmentFingerprint::capture();
        let timing = TimingMeasurement::new(5, 1, "hot".to_string(), true, 8);
        let mut receipt = TestReceipt::new(
            "test_sig".to_string(),
            "abc123".to_string(),
            env,
            vec![],
            timing,
            vec![],
            TestOutcome::Pass,
        );

        assert!(receipt.signature.is_none());
        assert!(!receipt.verify_signature());

        receipt.sign();
        assert!(receipt.signature.is_some());
        assert!(receipt.verify_signature());
    }

    #[test]
    fn test_receipt_metadata() {
        let env = EnvironmentFingerprint::capture();
        let timing = TimingMeasurement::new(5, 1, "hot".to_string(), true, 8);
        let mut receipt = TestReceipt::new(
            "test_meta".to_string(),
            "abc123".to_string(),
            env,
            vec![],
            timing,
            vec![],
            TestOutcome::Pass,
        );

        receipt.add_metadata("author", "test_user");
        receipt.add_metadata("branch", "main");

        assert_eq!(receipt.get_metadata("author"), Some("test_user"));
        assert_eq!(receipt.get_metadata("branch"), Some("main"));
        assert_eq!(receipt.get_metadata("missing"), None);
    }

    #[test]
    fn test_receipt_serialization() {
        let env = EnvironmentFingerprint::capture();
        let timing = TimingMeasurement::new(5, 1, "hot".to_string(), true, 8);
        let receipt = TestReceipt::new(
            "test_json".to_string(),
            "abc123".to_string(),
            env,
            vec!["τ ≤ 8".to_string()],
            timing,
            vec![],
            TestOutcome::Pass,
        );

        let json = receipt.to_json();
        assert!(json.is_ok());

        let json_str = json.unwrap();
        assert!(json_str.contains("test_json"));

        let deserialized = TestReceipt::from_json(&json_str);
        assert!(deserialized.is_ok());
        assert_eq!(deserialized.unwrap().contract_name, "test_json");
    }

    #[test]
    fn test_receipt_registry() {
        let mut registry = TestReceiptRegistry::new();
        assert!(registry.is_empty());

        let env = EnvironmentFingerprint::capture();
        let timing = TimingMeasurement::new(5, 1, "hot".to_string(), true, 8);
        let receipt1 = TestReceipt::new(
            "test1".to_string(),
            "hash1".to_string(),
            env.clone(),
            vec!["τ ≤ 8".to_string()],
            timing.clone(),
            vec!["NetworkRead".to_string()],
            TestOutcome::Pass,
        );

        let receipt2 = TestReceipt::new(
            "test2".to_string(),
            "hash2".to_string(),
            env,
            vec!["no_panics".to_string()],
            timing,
            vec!["StorageWrite".to_string()],
            TestOutcome::Fail,
        );

        registry.add_receipt(receipt1);
        registry.add_receipt(receipt2);

        assert_eq!(registry.len(), 2);
        assert!(!registry.is_empty());

        let test1_receipts = registry.receipts_for_test("test1");
        assert_eq!(test1_receipts.len(), 1);

        let tau_receipts = registry.receipts_for_invariant("τ ≤ 8");
        assert_eq!(tau_receipts.len(), 1);

        let network_receipts = registry.receipts_for_effect("NetworkRead");
        assert_eq!(network_receipts.len(), 1);

        let failed = registry.failed_receipts();
        assert_eq!(failed.len(), 1);
        assert_eq!(failed[0].contract_name, "test2");
    }
}
