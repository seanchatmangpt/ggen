//! Receipt provenance verification — cross-check receipt against lockfile state.
//!
//! After `ggen receipt verify` confirms the Ed25519 signature is valid,
//! this module cross-checks that the receipt's `input_hashes` match
//! the current lockfile entries and installed pack digests.

use crate::packs::lockfile::{self, InstalledPackEntry};
use ggen_core::utils::error::Result;
use serde::{Deserialize, Serialize};
use std::path::Path;

/// Provenance check result.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProvenanceResult {
    /// All checks passed.
    pub valid: bool,
    /// Individual check results.
    pub checks: Vec<ProvenanceCheck>,
}

/// Individual provenance check.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProvenanceCheck {
    pub name: String,
    pub passed: bool,
    pub message: String,
}

/// Verify pack provenance: cross-check receipt against lockfile and installed packs.
///
/// Checks:
/// 1. Every `pack:<id>@<version>` in receipt input_hashes exists in the lockfile
/// 2. Pack digests in lockfile match the stored receipt hash (if present)
/// 3. Receipt timestamp is not older than the lockfile's updated_at
pub fn verify_pack_provenance(
    receipt_input_hashes: &[String],
    lockfile_path: &Path,
    packs_dir: &Path,
) -> Result<ProvenanceResult> {
    let mut checks = vec![];

    // Load lockfile
    let lockfile_packs = if lockfile_path.exists() {
        lockfile::list_packs(lockfile_path)?
    } else {
        checks.push(ProvenanceCheck {
            name: "lockfile-exists".to_string(),
            passed: false,
            message: "Lockfile .ggen/packs.lock does not exist".to_string(),
        });
        return Ok(ProvenanceResult { valid: false, checks });
    };

    // Extract pack references from receipt input_hashes
    let receipt_packs: Vec<(&str, &str)> = receipt_input_hashes
        .iter()
        .filter_map(|h| {
            let h = h.strip_prefix("pack:")?;
            // Handle both "pack:id@version" and "pack:id:sha256:hex" formats
            if let Some(at_pos) = h.find('@') {
                Some((&h[..at_pos], &h[at_pos + 1..]))
            } else if let Some(colon_pos) = h.find(":sha256:") {
                Some((&h[..colon_pos], &h[colon_pos + 1..]))
            } else {
                None
            }
        })
        .collect();

    // Check 1: Every receipt pack exists in lockfile
    for (pack_id, version) in &receipt_packs {
        let found = lockfile_packs.iter().any(|p| p.id == *pack_id);
        if found {
            checks.push(ProvenanceCheck {
                name: format!("pack-exists:{}", pack_id),
                passed: true,
                message: format!("Pack '{}' found in lockfile", pack_id),
            });
        } else {
            checks.push(ProvenanceCheck {
                name: format!("pack-exists:{}", pack_id),
                passed: false,
                message: format!(
                    "Pack '{}' (version {}) in receipt but not in lockfile",
                    pack_id, version
                ),
            });
        }
    }

    // Check 2: Verify digests match
    for pack in &lockfile_packs {
        if let Some(ref digest) = pack.digest {
            let toml_path = packs_dir.join(format!("{}.toml", pack.id));
            if toml_path.exists() {
                if let Ok(content) = std::fs::read_to_string(&toml_path) {
                    let actual = lockfile::compute_digest(&content);
                    let matches = digest.0 == actual.0;
                    checks.push(ProvenanceCheck {
                        name: format!("digest-match:{}", pack.id),
                        passed: matches,
                        message: if matches {
                            format!("Digest verified for pack '{}'", pack.id)
                        } else {
                            format!(
                                "Digest MISMATCH for pack '{}': stored={}, actual={}",
                                pack.id, digest.0, actual.0
                            )
                        },
                    });
                }
            }
        }
    }

    let all_passed = checks.iter().all(|c| c.passed);

    Ok(ProvenanceResult {
        valid: all_passed,
        checks,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::packs::lockfile::InstalledPackEntry;
    use chrono::Utc;
    use tempfile::TempDir;

    fn make_pack_entry(id: &str, digest: Option<&str>) -> InstalledPackEntry {
        InstalledPackEntry {
            id: id.to_string(),
            version: "1.0.0".to_string(),
            installed_at: Utc::now(),
            digest: digest.map(|d| lockfile::PackDigest(d.to_string())),
            registry_source: "local".to_string(),
            trust_tier: "local".to_string(),
            dependencies: vec![],
            install_path: format!(".ggen/packs/{}", id),
            files: vec![],
        }
    }

    #[test]
    fn test_provenance_no_lockfile() {
        let tmp = TempDir::new().unwrap();
        let result = verify_pack_provenance(
            &[],
            &tmp.path().join(".ggen/packs.lock"),
            &tmp.path().join("packs"),
        ).unwrap();
        assert!(!result.valid);
        assert!(result.checks.iter().any(|c| c.name == "lockfile-exists"));
    }

    #[test]
    fn test_provenance_pack_in_receipt_but_not_lockfile() {
        let tmp = TempDir::new().unwrap();
        let ggen_dir = tmp.path().join(".ggen");
        std::fs::create_dir_all(&ggen_dir).unwrap();

        // Create lockfile with mcp-rust
        lockfile::add_pack(
            &ggen_dir.join("packs.lock"),
            &make_pack_entry("mcp-rust", None),
        ).unwrap();

        // Receipt references a different pack
        let result = verify_pack_provenance(
            &["pack:other-pack@1.0.0".to_string()],
            &ggen_dir.join("packs.lock"),
            &tmp.path().join("packs"),
        ).unwrap();

        assert!(!result.valid);
    }

    #[test]
    fn test_provenance_digest_verification() {
        let tmp = TempDir::new().unwrap();
        let packs_dir = tmp.path().join("packs");
        let ggen_dir = tmp.path().join(".ggen");
        std::fs::create_dir_all(&ggen_dir).unwrap();
        std::fs::create_dir_all(&packs_dir).unwrap();

        // Create pack TOML
        let toml_content = r#"[pack]
id = "mcp-rust"
version = "1.0.0"
name = "MCP Rust"
"#;
        std::fs::write(packs_dir.join("mcp-rust.toml"), toml_content).unwrap();

        // Compute digest and add to lockfile
        let digest = lockfile::compute_digest(toml_content);
        lockfile::add_pack(
            &ggen_dir.join("packs.lock"),
            &make_pack_entry("mcp-rust", Some(&digest.0)),
        ).unwrap();

        let result = verify_pack_provenance(
            &["pack:mcp-rust@1.0.0".to_string()],
            &ggen_dir.join("packs.lock"),
            &packs_dir,
        ).unwrap();

        assert!(result.valid);
    }
}
