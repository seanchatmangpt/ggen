//! Pack loader — load and validate installed packs from lockfile.
//!
//! Used by `sync --locked` to consume pack data into the pipeline.

use crate::packs::lockfile::{self, InstalledPackEntry};
use ggen_core::utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::path::Path;

/// A validated installed pack with its materialized content paths.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstalledPack {
    /// Pack ID from lockfile
    pub id: String,
    /// Pack version
    pub version: String,
    /// Digest from lockfile (if stored)
    pub digest: Option<String>,
    /// Path to the pack's install directory (.ggen/packs/<id>/)
    pub install_path: std::path::PathBuf,
    /// Templates available in this pack
    pub templates: Vec<String>,
    /// SPARQL queries available in this pack
    pub sparql_queries: Vec<String>,
    /// Regular queries (.rq files) available
    pub queries: Vec<String>,
    /// Packages installed by this pack
    pub packages: Vec<String>,
    /// Whether digest verification passed
    pub integrity_verified: bool,
}

/// Load all installed packs from the lockfile, with optional digest verification.
///
/// If `verify_digests` is true and a pack's digest doesn't match its TOML content,
/// returns Err in enterprise-strict mode (caller decides policy).
pub fn load_installed_packs(
    lockfile_path: &Path,
    packs_dir: &Path,
    verify_digests: bool,
) -> Result<Vec<InstalledPack>> {
    let entries = lockfile::list_packs(lockfile_path)?;

    let mut packs = vec![];

    for entry in &entries {
        let install_path = std::path::PathBuf::from(&entry.install_path);

        // If no install_path in entry, derive it
        let effective_install_path = if install_path.as_os_str().is_empty() {
            std::path::PathBuf::from(".ggen/packs").join(&entry.id)
        } else {
            install_path
        };

        // Verify digest if requested
        let integrity_verified = if verify_digests {
            lockfile::verify_pack_integrity(lockfile_path, &entry.id, packs_dir)?
        } else {
            true
        };

        // Scan for materialized content
        let templates = scan_directory_files(&effective_install_path.join("templates"), ".tera");
        let sparql_queries = scan_directory_files(&effective_install_path.join("sparql"), ".rq");
        let queries = scan_directory_files(&effective_install_path.join("queries"), ".rq");

        // Get packages from pack metadata
        let packages = crate::packs::metadata::load_pack_metadata(&entry.id)
            .map(|p| p.packages)
            .unwrap_or_default();

        packs.push(InstalledPack {
            id: entry.id.clone(),
            version: entry.version.clone(),
            digest: entry.digest.as_ref().map(|d| d.prefixed()),
            install_path: effective_install_path,
            templates,
            sparql_queries,
            queries,
            packages,
            integrity_verified,
        });
    }

    Ok(packs)
}

/// Load a single installed pack by ID.
pub fn load_installed_pack(
    lockfile_path: &Path,
    pack_id: &str,
    packs_dir: &Path,
    verify_digest: bool,
) -> Result<InstalledPack> {
    let all = load_installed_packs(lockfile_path, packs_dir, verify_digest)?;
    all.into_iter()
        .find(|p| p.id == pack_id)
        .ok_or_else(|| Error::new(&format!("Pack '{}' not found in lockfile", pack_id)))
}

/// Scan a directory for files with a given extension.
fn scan_directory_files(dir: &std::path::Path, extension: &str) -> Vec<String> {
    if !dir.exists() {
        return vec![];
    }

    let mut files = vec![];
    if let Ok(entries) = std::fs::read_dir(dir) {
        for entry in entries.flatten() {
            if let Some(name) = entry.file_name().to_str() {
                if name.ends_with(extension) {
                    files.push(name.to_string());
                }
            }
        }
    }

    files.sort();
    files
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::packs::lockfile::{InstalledPackEntry, PackDigest};
    use chrono::Utc;
    use tempfile::TempDir;

    fn make_entry(id: &str) -> InstalledPackEntry {
        InstalledPackEntry {
            id: id.to_string(),
            version: "1.0.0".to_string(),
            installed_at: Utc::now(),
            digest: None,
            registry_source: "local".to_string(),
            trust_tier: "local".to_string(),
            dependencies: vec![],
            install_path: format!(".ggen/packs/{}", id),
            files: vec![],
        }
    }

    #[test]
    fn test_load_empty_lockfile() {
        let tmp = TempDir::new().unwrap();
        let lock_path = tmp.path().join("packs.lock");
        let packs = load_installed_packs(&lock_path, tmp.path(), false).unwrap();
        assert!(packs.is_empty());
    }

    #[test]
    fn test_load_packs_no_verify() {
        let tmp = TempDir::new().unwrap();
        let ggen_dir = tmp.path().join(".ggen");
        std::fs::create_dir_all(&ggen_dir).unwrap();

        lockfile::add_pack(&ggen_dir.join("packs.lock"), &make_entry("mcp-rust")).unwrap();

        let packs = load_installed_packs(
            &ggen_dir.join("packs.lock"),
            tmp.path().join("packs"),
            false,
        ).unwrap();

        assert_eq!(packs.len(), 1);
        assert_eq!(packs[0].id, "mcp-rust");
        assert!(packs[0].integrity_verified);
    }

    #[test]
    fn test_scan_directory_files() {
        let tmp = TempDir::new().unwrap();
        let dir = tmp.path().join("templates");
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("a.rs.tera"), "a").unwrap();
        std::fs::write(dir.join("b.rs.tera"), "b").unwrap();
        std::fs::write(dir.join("c.txt"), "c").unwrap();

        let files = scan_directory_files(&dir, ".tera");
        assert_eq!(files, vec!["a.rs.tera", "b.rs.tera"]);
    }

    #[test]
    fn test_scan_missing_directory() {
        let files = scan_directory_files(std::path::Path::new("/nonexistent"), ".tera");
        assert!(files.is_empty());
    }
}
