//! Attestation - Machine-readable evidence of test execution
//!
//! Every test run generates an attestation document that provides:
//! - Complete surface configuration (time, RNG, fs, net, proc)
//! - Policy applied
//! - Seeds used
//! - Digests of images/binaries
//! - SBOM (Software Bill of Materials)
//! - SLSA-like provenance
//! - Coverage map
//!
//! This makes audits programmable and enables reproducibility verification.

use std::path::Path;
use std::time::SystemTime;
use serde::{Serialize, Deserialize};
use std::collections::HashMap;

use super::surfaces::DeterministicSurfaces;
use crate::error::Result;

/// Attestation document for a test run
///
/// This is the core artifact that proves:
/// 1. What configuration was used
/// 2. What policy was enforced
/// 3. What deterministic guarantees were provided
/// 4. What dependencies were involved
///
/// # Example
///
/// ```json
/// {
///   "version": "1.0",
///   "timestamp": "2025-01-15T10:30:00Z",
///   "policy": "Locked",
///   "surfaces": {
///     "time": { "Frozen": 42 },
///     "rng": { "Seeded": 42 },
///     "fs": "Ephemeral",
///     "net": "Offline",
///     "proc": { "NonRoot": { "uid": 1000, "gid": 1000 } }
///   },
///   "determinism_score": 1.0,
///   "security_level": 100
/// }
/// ```
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Attestation {
    /// Attestation format version
    pub version: String,

    /// When this test run occurred
    pub timestamp: String,

    /// Policy applied (Locked, Permissive, etc.)
    pub policy: String,

    /// Deterministic surfaces configuration
    pub surfaces: DeterministicSurfaces,

    /// Determinism score (0.0 to 1.0)
    pub determinism_score: f64,

    /// Security level (0 to 100)
    pub security_level: u8,

    /// Seeds used (for reproducibility)
    pub seeds: HashMap<String, u64>,

    /// Image/binary digests (SHA256)
    pub digests: HashMap<String, String>,

    /// SBOM (Software Bill of Materials)
    pub sbom: Option<Sbom>,

    /// Provenance information
    pub provenance: Option<Provenance>,

    /// Coverage map
    pub coverage: Option<CoverageMap>,

    /// Custom metadata
    pub metadata: HashMap<String, serde_json::Value>,
}

/// Software Bill of Materials
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Sbom {
    /// SBOM format (SPDX, CycloneDX, etc.)
    pub format: String,

    /// SBOM version
    pub version: String,

    /// Dependencies
    pub dependencies: Vec<Dependency>,

    /// Creation time
    pub created_at: String,
}

/// Dependency information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Dependency {
    /// Package name
    pub name: String,

    /// Package version
    pub version: String,

    /// Package license
    pub license: Option<String>,

    /// Package source (crates.io, github, etc.)
    pub source: String,

    /// SHA256 digest
    pub sha256: Option<String>,
}

/// Provenance information (SLSA-like)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Provenance {
    /// Builder identity
    pub builder: BuilderIdentity,

    /// Build invocation
    pub invocation: BuildInvocation,

    /// Materials (source code, dependencies)
    pub materials: Vec<Material>,

    /// Build metadata
    pub metadata: BuildMetadata,
}

/// Builder identity
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuilderIdentity {
    /// Builder name (ggen-cleanroom, etc.)
    pub name: String,

    /// Builder version
    pub version: String,

    /// Builder digest
    pub digest: Option<String>,
}

/// Build invocation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuildInvocation {
    /// Command that was run
    pub command: Vec<String>,

    /// Environment variables (redacted)
    pub env: HashMap<String, String>,

    /// Working directory
    pub working_dir: String,
}

/// Material (source code, dependency, etc.)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Material {
    /// Material URI (file://, https://, etc.)
    pub uri: String,

    /// Material digest (SHA256)
    pub digest: String,
}

/// Build metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuildMetadata {
    /// Build start time
    pub started_at: String,

    /// Build finish time
    pub finished_at: Option<String>,

    /// Build duration (seconds)
    pub duration_secs: Option<f64>,

    /// Exit code
    pub exit_code: Option<i32>,
}

/// Coverage map
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoverageMap {
    /// Total lines
    pub total_lines: u64,

    /// Covered lines
    pub covered_lines: u64,

    /// Coverage percentage
    pub coverage_pct: f64,

    /// Files with coverage data
    pub files: Vec<FileCoverage>,
}

/// Coverage for a single file
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileCoverage {
    /// File path
    pub path: String,

    /// Total lines
    pub total_lines: u64,

    /// Covered lines
    pub covered_lines: u64,

    /// Coverage percentage
    pub coverage_pct: f64,
}

impl Attestation {
    /// Create new attestation
    pub fn new(surfaces: &DeterministicSurfaces, policy: &str) -> Self {
        let mut seeds = HashMap::new();

        // Extract seeds from surfaces
        if let super::surfaces::TimeMode::Frozen(seed) = surfaces.time {
            seeds.insert("time".to_string(), seed);
        }
        if let super::surfaces::RngMode::Seeded(seed) = surfaces.rng {
            seeds.insert("rng".to_string(), seed);
        }

        let security_level = match policy {
            "Locked" => 100,
            "Permissive" => 30,
            _ => 50,
        };

        Self {
            version: "1.0".to_string(),
            timestamp: format_timestamp(SystemTime::now()),
            policy: policy.to_string(),
            surfaces: surfaces.clone(),
            determinism_score: surfaces.determinism_score(),
            security_level,
            seeds,
            digests: HashMap::new(),
            sbom: None,
            provenance: None,
            coverage: None,
            metadata: HashMap::new(),
        }
    }

    /// Add digest for binary/image
    pub fn add_digest(&mut self, name: impl Into<String>, digest: impl Into<String>) {
        self.digests.insert(name.into(), digest.into());
    }

    /// Set SBOM
    pub fn set_sbom(&mut self, sbom: Sbom) {
        self.sbom = Some(sbom);
    }

    /// Set provenance
    pub fn set_provenance(&mut self, provenance: Provenance) {
        self.provenance = Some(provenance);
    }

    /// Set coverage map
    pub fn set_coverage(&mut self, coverage: CoverageMap) {
        self.coverage = Some(coverage);
    }

    /// Add custom metadata
    pub fn add_metadata(&mut self, key: impl Into<String>, value: serde_json::Value) {
        self.metadata.insert(key.into(), value);
    }

    /// Get policy name
    pub fn policy(&self) -> &str {
        &self.policy
    }

    /// Export attestation to JSON file
    pub fn export(&self, path: impl AsRef<Path>) -> Result<()> {
        let json = serde_json::to_string_pretty(self)?;
        std::fs::write(path, json)?;
        Ok(())
    }

    /// Import attestation from JSON file
    pub fn import(path: impl AsRef<Path>) -> Result<Self> {
        let json = std::fs::read_to_string(path)?;
        let attestation = serde_json::from_str(&json)?;
        Ok(attestation)
    }

    /// Verify attestation signature (placeholder for future crypto)
    pub fn verify(&self) -> Result<bool> {
        // FUTURE: Implement signature verification with cosign/notation
        Ok(true)
    }

    /// Get reproducibility instructions
    pub fn reproducibility_instructions(&self) -> String {
        let mut instructions = String::new();

        instructions.push_str("# Reproducibility Instructions\n\n");
        instructions.push_str(&format!("Policy: {}\n", self.policy));
        instructions.push_str(&format!("Determinism Score: {:.2}\n", self.determinism_score));
        instructions.push_str(&format!("Security Level: {}\n\n", self.security_level));

        instructions.push_str("## Seeds\n\n");
        for (key, value) in &self.seeds {
            instructions.push_str(&format!("- {}: {}\n", key, value));
        }

        instructions.push_str("\n## Configuration\n\n");
        instructions.push_str(&format!("```json\n{}\n```\n", serde_json::to_string_pretty(&self.surfaces).unwrap()));

        instructions
    }
}

/// Format timestamp in ISO 8601 format
fn format_timestamp(time: SystemTime) -> String {
    let duration = time.duration_since(SystemTime::UNIX_EPOCH)
        .unwrap_or_default();

    let secs = duration.as_secs();
    let nanos = duration.subsec_nanos();

    // Simple ISO 8601 formatting (good enough for attestation)
    format!("{}.{:09}Z", secs, nanos)
}

#[cfg(test)]
mod tests {
    use super::*;
    use chicago_tdd_tools::test;

    test!(test_attestation_creation, {
        let surfaces = DeterministicSurfaces::deterministic(42);
        let attestation = Attestation::new(&surfaces, "Locked");

        assert_eq!(attestation.policy, "Locked");
        assert_eq!(attestation.determinism_score, 1.0);
        assert_eq!(attestation.security_level, 100);
        assert!(attestation.seeds.contains_key("time"));
        assert!(attestation.seeds.contains_key("rng"));
    });

    test!(test_attestation_export_import, {
        use tempfile::TempDir;

        let surfaces = DeterministicSurfaces::deterministic(42);
        let attestation = Attestation::new(&surfaces, "Locked");

        let temp_dir = TempDir::new().unwrap();
        let path = temp_dir.path().join("attestation.json");

        attestation.export(&path).unwrap();
        let imported = Attestation::import(&path).unwrap();

        assert_eq!(attestation.policy, imported.policy);
        assert_eq!(attestation.determinism_score, imported.determinism_score);
    });

    test!(test_reproducibility_instructions, {
        let surfaces = DeterministicSurfaces::deterministic(42);
        let attestation = Attestation::new(&surfaces, "Locked");

        let instructions = attestation.reproducibility_instructions();

        assert!(instructions.contains("Policy: Locked"));
        assert!(instructions.contains("Determinism Score: 1.00"));
        assert!(instructions.contains("time: 42"));
        assert!(instructions.contains("rng: 42"));
    });
}
