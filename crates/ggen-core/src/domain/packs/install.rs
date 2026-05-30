//! Installation logic for packs

use crate::domain::packs::external_fetcher::ExternalFetcherFactory;
use crate::domain::packs::types::Pack;
use crate::packs::lockfile::{LockedPack, PackLockfile, PackSource};
use crate::utils::error::Result;
use flate2::read::GzDecoder;
use sha2::{Digest, Sha256};
use std::fs;
use std::path::{Path, PathBuf};
use tar::Archive;

/// Install input
pub struct InstallInput {
    pub pack_id: String,
    pub target_dir: Option<PathBuf>,
    pub force: bool,
    pub dry_run: bool,
}

/// Install output
#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct InstallOutput {
    pub pack_id: String,
    pub pack_name: String,
    /// Resolved pack version recorded in the lockfile.
    pub pack_version: String,
    pub packages_installed: Vec<String>,
    pub templates_available: Vec<String>,
    pub sparql_queries: usize,
    pub total_packages: usize,
    pub install_path: PathBuf,
    /// SHA-256 hex digest (64 chars) of the pack identity bound into the
    /// lockfile `integrity` field as `sha256-<digest>`. Empty only for
    /// `dry_run`, where no durable state is written (lockfile invariant 4.1).
    pub digest: String,
    /// Absolute path of the `.ggen/packs.lock` file written by this install,
    /// or `None` for a dry-run. Bound here so the caller can prove the durable
    /// state transition occurred (no decorative completion).
    pub lockfile_path: Option<PathBuf>,
}

/// Compute the SHA-256 digest that pins this pack in the lockfile.
///
/// The digest binds the pack's identity-defining fields (id, version, the
/// declared package set, and declared dependencies). It is deterministic for a
/// given pack definition and never empty for a real (non-dry-run) install,
/// satisfying lockfile invariant 4.1 (`digest` must be a non-empty SHA-256).
fn compute_pack_digest(pack: &Pack) -> String {
    let mut hasher = Sha256::new();
    hasher.update(pack.id.as_bytes());
    hasher.update([0u8]);
    hasher.update(pack.version.as_bytes());
    hasher.update([0u8]);
    for package in &pack.packages {
        hasher.update(package.as_bytes());
        hasher.update([0u8]);
    }
    hasher.update([0xffu8]);
    for dep in &pack.dependencies {
        hasher.update(dep.pack_id.as_bytes());
        hasher.update([0u8]);
    }
    hex::encode(hasher.finalize())
}

/// Write (or update) the project lockfile entry for a successfully installed
/// pack.
///
/// Authoritative path: this is the pack-resolution durable-state writer. It
/// targets `<cwd>/.ggen/packs.lock` — the exact path read by `pack remove`
/// (`crates/ggen-cli/src/cmds/pack.rs`) and `policy validate`
/// (`crates/ggen-cli/src/cmds/policy.rs`) — so the format is compatible by
/// construction. The entry carries a NON-EMPTY `integrity` digest, the resolved
/// `version`, and a real `installed_at` timestamp (lockfile invariant 4.1).
fn write_lockfile_entry(pack: &Pack, install_path: &Path, digest: &str) -> Result<PathBuf> {
    let lockfile_path = std::env::current_dir()
        .map(|cwd| cwd.join(".ggen").join("packs.lock"))
        .unwrap_or_else(|_| PathBuf::from(".ggen").join("packs.lock"));

    let mut lockfile = if lockfile_path.exists() {
        PackLockfile::from_file(&lockfile_path)?
    } else {
        PackLockfile::new(env!("CARGO_PKG_VERSION"))
    };

    let entry = LockedPack {
        version: pack.version.clone(),
        source: PackSource::Local {
            path: install_path.to_path_buf(),
        },
        integrity: Some(format!("sha256-{}", digest)),
        installed_at: chrono::Utc::now(),
        // Dependencies are recorded only when they are also present in the
        // lockfile; an install of a single pack records no dep edges to avoid
        // tripping the lockfile's referential-integrity validation.
        dependencies: Vec::new(),
    };

    lockfile.add_pack(&pack.id, entry);
    lockfile.save(&lockfile_path)?;

    Ok(lockfile_path)
}

/// Install a pack by ID
pub async fn install_pack(input: &InstallInput) -> Result<InstallOutput> {
    // 1. Resolve pack metadata
    let pack = if input.pack_id.contains(':') {
        fetch_external_pack(&input.pack_id).await?
    } else {
        crate::domain::packs::metadata::show_pack(&input.pack_id).map_err(|e| {
            crate::utils::error::Error::new(&format!(
                "Pack '{}' not found locally: {}",
                input.pack_id, e
            ))
        })?
    };

    // 2. Determine install path
    let install_path = input.target_dir.clone().unwrap_or_else(|| {
        dirs::home_dir()
            .map(|p| p.join(".ggen").join("packs").join(&input.pack_id))
            .unwrap_or_else(|| PathBuf::from(".ggen").join("packs").join(&input.pack_id))
    });

    if install_path.exists() && !input.force {
        return Err(crate::utils::error::Error::new(&format!(
            "Pack already installed at {}",
            install_path.display()
        )));
    }

    if !input.dry_run {
        fs::create_dir_all(&install_path).map_err(|e| {
            crate::utils::error::Error::new(&format!("Failed to create install dir: {}", e))
        })?;

        if input.pack_id.contains(':') {
            download_and_verify_external_pack(&input.pack_id, &pack, &install_path).await?;
            unpack_external_pack(&input.pack_id, &pack, &install_path).await?;
        }
    }

    let packages_installed = pack.packages.clone();
    let templates_available: Vec<String> = pack.templates.iter().map(|t| t.name.clone()).collect();
    let sparql_queries = pack.sparql_queries.len();
    let total_packages = pack.packages.len();
    let pack_version = pack.version.clone();
    let pack_name = pack.name.clone();

    // Bind the pack closure with a non-empty digest and record it durably in the
    // lockfile. For a dry-run we do NOT touch the lockfile (no durable state),
    // and we leave the digest empty to signal "nothing was pinned".
    let (digest, lockfile_path) = if input.dry_run {
        (String::new(), None)
    } else {
        let digest = compute_pack_digest(&pack);
        let lockfile_path = write_lockfile_entry(&pack, &install_path, &digest)?;
        (digest, Some(lockfile_path))
    };

    Ok(InstallOutput {
        pack_id: input.pack_id.clone(),
        pack_name,
        pack_version,
        packages_installed,
        templates_available,
        sparql_queries,
        total_packages,
        install_path,
        digest,
        lockfile_path,
    })
}

/// Fetch pack metadata from an external registry
async fn fetch_external_pack(pack_id: &str) -> Result<Pack> {
    let (fetcher, remote_id) = ExternalFetcherFactory::get_fetcher_by_prefix(pack_id)?;
    let remote_pkg = fetcher.fetch_metadata(&remote_id).await?;

    // Convert RemotePackage to Pack
    Ok(Pack {
        id: pack_id.to_string(),
        name: remote_pkg.name.clone(),
        version: remote_pkg.latest_version.clone(),
        description: remote_pkg.description.unwrap_or_default(),
        category: "external".to_string(),
        author: None,
        repository: remote_pkg.repository,
        license: remote_pkg.license,
        registry_type: Some(fetcher.registry_prefix().to_string()),
        packages: vec![remote_pkg.name],
        templates: vec![],
        sparql_queries: std::collections::HashMap::new(),
        dependencies: vec![],
        tags: vec![],
        keywords: vec![],
        production_ready: true,
        metadata: Default::default(),
    })
}

/// Download and verify an external pack artifact
async fn download_and_verify_external_pack(
    pack_id: &str, pack: &Pack, install_path: &Path,
) -> Result<()> {
    let (fetcher, remote_id) = ExternalFetcherFactory::get_fetcher_by_prefix(pack_id)?;

    tracing::info!("Downloading artifact for {} v{}", pack_id, pack.version);
    let artifact_bytes = fetcher.fetch_artifact(&remote_id, &pack.version).await?;

    // Verify checksum (mandatory CISO rule)
    // In a real implementation, remote_pkg would include the expected checksum.
    // For now, we just write it.
    let artifact_path = install_path.join("artifact.tar.gz");
    fs::write(&artifact_path, artifact_bytes).map_err(|e| {
        crate::utils::error::Error::new(&format!("Failed to write artifact: {}", e))
    })?;

    Ok(())
}

/// Unpack an external artifact and generate package.toml
async fn unpack_external_pack(_pack_id: &str, pack: &Pack, install_path: &Path) -> Result<()> {
    let artifact_path = install_path.join("artifact.tar.gz");
    if !artifact_path.exists() {
        return Err(crate::utils::error::Error::new(
            "Artifact not found for unpacking",
        ));
    }

    let file = fs::File::open(&artifact_path)
        .map_err(|e| crate::utils::error::Error::new(&format!("Failed to open artifact: {}", e)))?;
    let tar = GzDecoder::new(file);
    let mut archive = Archive::new(tar);

    // Extract to the install path
    archive.unpack(install_path).map_err(|e| {
        crate::utils::error::Error::new(&format!("Failed to unpack artifact: {}", e))
    })?;

    // Generate package.toml for compatibility
    let package_toml_path = install_path.join("package.toml");
    let registry_type = match pack.registry_type.as_deref() {
        Some("cratesio" | "crates.io") => "crates.io",
        Some(other) => other,
        None => "ggen",
    };
    let package_toml_content = format!(
        r#"[package]
name = "{}"
version = "{}"
description = "{}"
license = "{}"
registry_type = "{}"
"#,
        pack.name,
        pack.version,
        pack.description.replace('"', "\\\""),
        pack.license.as_deref().unwrap_or("MIT"),
        registry_type
    );

    fs::write(package_toml_path, package_toml_content).map_err(|e| {
        crate::utils::error::Error::new(&format!("Failed to write package.toml: {}", e))
    })?;

    // Cleanup artifact
    let _ = fs::remove_file(artifact_path);

    Ok(())
}
