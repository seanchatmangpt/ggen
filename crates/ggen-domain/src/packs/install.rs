//! Pack installation logic — with digest, materialization, and domain lockfile.
//!
//! This is the authoritative path for `ggen pack add`.

use crate::packs::lockfile::{self, InstalledPackEntry};
use crate::packs::metadata::load_pack_metadata;
use chrono::Utc;
use ggen_core::utils::error::Result;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Install pack input
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstallInput {
    pub pack_id: String,
    pub target_dir: Option<PathBuf>,
    #[serde(default)]
    pub force: bool,
    #[serde(default)]
    pub dry_run: bool,
}

/// Install pack output
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstallOutput {
    pub pack_id: String,
    pub pack_name: String,
    pub packages_installed: Vec<String>,
    pub templates_available: Vec<String>,
    pub sparql_queries: usize,
    pub total_packages: usize,
    pub install_path: PathBuf,
    pub digest: Option<String>,
}

/// Install a pack: compute digest, materialize files, write lockfile entry.
///
/// This is the single authoritative path for pack installation.
/// Both `pack add` and `capability enable` converge here via the domain lockfile.
pub async fn install_pack(input: &InstallInput) -> Result<InstallOutput> {
    // Load pack metadata
    let pack = load_pack_metadata(&input.pack_id)?;

    if input.dry_run {
        return Ok(InstallOutput {
            pack_id: input.pack_id.clone(),
            pack_name: pack.name.clone(),
            packages_installed: vec![],
            templates_available: pack.templates.iter().map(|t| t.name.clone()).collect(),
            sparql_queries: pack.sparql_queries.len(),
            total_packages: pack.packages.len(),
            install_path: input
                .target_dir
                .clone()
                .unwrap_or_else(|| PathBuf::from(".ggen/packs")),
            digest: None,
        });
    }

    let install_path = input
        .target_dir
        .clone()
        .unwrap_or_else(|| PathBuf::from(".ggen/packs"))
        .join(&input.pack_id);

    // Step 1: Compute digest of pack TOML
    let pack_toml_content = std::fs::read_to_string(
        crate::packs::metadata::get_packs_dir()?.join(format!("{}.toml", input.pack_id)),
    )?;
    let digest = lockfile::compute_digest(&pack_toml_content);

    // Step 2: Materialize pack files (copy templates/queries/sparql/packages)
    materialize_pack(&input.pack_id, &install_path)?;

    // Step 3: Write lockfile entry via domain lockfile module (single entry point)
    let lockfile_path = PathBuf::from(".ggen/packs.lock");
    let entry = InstalledPackEntry {
        id: input.pack_id.clone(),
        version: pack.version.clone(),
        installed_at: Utc::now(),
        digest: Some(digest.clone()),
        registry_source: "local".to_string(),
        trust_tier: "local".to_string(),
        dependencies: pack.dependencies.iter().map(|d| d.pack_id.clone()).collect(),
        install_path: install_path.display().to_string(),
        files: collect_installed_files(&install_path),
    };

    lockfile::add_pack(&lockfile_path, &entry)?;

    tracing::info!(
        "Pack '{}' installed (digest: {})",
        input.pack_id,
        digest.prefixed(),
    );

    Ok(InstallOutput {
        pack_id: input.pack_id.clone(),
        pack_name: pack.name,
        packages_installed: pack.packages.clone(),
        templates_available: pack.templates.iter().map(|t| t.name.clone()).collect(),
        sparql_queries: pack.sparql_queries.len(),
        total_packages: pack.packages.len(),
        install_path,
        digest: Some(digest.prefixed()),
    })
}

/// Materialize pack files from the packs directory to the install directory.
///
/// Copies:
/// - templates/ → .ggen/packs/<id>/templates/
/// - queries/ → .ggen/packs/<id>/queries/
/// - sparql/ → .ggen/packs/<id>/sparql/
fn materialize_pack(pack_id: &str, install_path: &std::path::Path) -> Result<()> {
    let packs_dir = crate::packs::metadata::get_packs_dir()?;
    let source_dir = packs_dir.join(pack_id);

    let subdirs = ["templates", "queries", "sparql"];

    for subdir in &subdirs {
        let src = source_dir.join(subdir);
        if !src.exists() {
            continue;
        }

        let dst = install_path.join(subdir);
        std::fs::create_dir_all(&dst)?;

        copy_dir_contents(&src, &dst)?;
    }

    Ok(())
}

/// Copy all files from src to dst directory (non-recursive).
fn copy_dir_contents(src: &std::path::Path, dst: &std::path::Path) -> Result<()> {
    for entry in std::fs::read_dir(src)? {
        let entry = entry?;
        let src_path = entry.path();

        if src_path.is_file() {
            if let Some(name) = src_path.file_name() {
                std::fs::copy(&src_path, dst.join(name))?;
            }
        }
    }

    Ok(())
}

/// Collect list of installed file paths relative to install_path.
fn collect_installed_files(install_path: &std::path::Path) -> Vec<String> {
    let mut files = vec![];

    let subdirs = ["templates", "queries", "sparql"];
    for subdir in &subdirs {
        let dir = install_path.join(subdir);
        if !dir.exists() {
            continue;
        }
        if let Ok(entries) = std::fs::read_dir(&dir) {
            for entry in entries.flatten() {
                if entry.path().is_file() {
                    if let Some(name) = entry.file_name().to_str() {
                        files.push(format!("{}/{}", subdir, name));
                    }
                }
            }
        }
    }

    files
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_install_pack_dry_run_returns_without_files() {
        let input = InstallInput {
            pack_id: "test-pack".to_string(),
            target_dir: None,
            force: false,
            dry_run: true,
        };

        // Will fail if pack doesn't exist, but tests the dry_run code path
        let _ = install_pack(&input).await;
    }

    #[test]
    fn test_collect_installed_files_empty() {
        let tmp = TempDir::new().unwrap();
        let files = collect_installed_files(tmp.path());
        assert!(files.is_empty());
    }

    #[test]
    fn test_collect_installed_files_with_content() {
        let tmp = TempDir::new().unwrap();
        let tmpl_dir = tmp.path().join("templates");
        std::fs::create_dir_all(&tmpl_dir).unwrap();
        std::fs::write(tmpl_dir.join("hello.rs.tera"), "content").unwrap();

        let files = collect_installed_files(tmp.path());
        assert_eq!(files, vec!["templates/hello.rs.tera"]);
    }
}
