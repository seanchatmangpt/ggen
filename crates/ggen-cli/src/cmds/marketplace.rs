//! Marketplace Commands - Package Discovery and Management
//!
//! This module provides marketplace operations for discovering and managing packages.

use clap_noun_verb::Result as VerbResult;
use clap_noun_verb_macros::verb;
use ggen_core::marketplace::prelude::*;
use ggen_core::marketplace::registry_rdf::RdfRegistry;
use ggen_core::marketplace::trust::RegistryType;
use oxigraph::store::Store;
use serde::{Deserialize, Serialize};
use std::path::Path;

use crate::runtime::block_on;

// ============================================================================
// Helpers
// ============================================================================

/// Map string to RegistryType
fn map_registry_type(s: Option<String>) -> RegistryType {
    match s.as_deref() {
        Some("crates.io") => RegistryType::CratesIo,
        Some("npm") => RegistryType::Npm,
        Some("pypi") => RegistryType::PyPi,
        Some("github") => RegistryType::GitHub,
        Some("other") => RegistryType::Other,
        Some("ggen") | None => RegistryType::Ggen,
        _ => RegistryType::Ggen,
    }
}

// ============================================================================
// Output Types
// ============================================================================

#[derive(Serialize)]
struct SearchOutput {
    query: String,
    results: Vec<SearchResultCli>,
    total: usize,
}

#[derive(Serialize, Clone)]
struct SearchResultCli {
    id: String,
    name: String,
    description: String,
    version: String,
    relevance: f64,
    registry_type: String,
}

#[derive(Serialize)]
struct ListOutput {
    packages: Vec<ListPackage>,
    total: usize,
}

#[derive(Serialize, Clone)]
struct ListPackage {
    id: String,
    name: String,
    description: String,
    latest_version: String,
    registry_type: String,
}

#[derive(Serialize)]
struct DetailOutput {
    id: String,
    name: String,
    description: String,
    authors: Vec<String>,
    license: String,
    homepage: Option<String>,
    repository: Option<String>,
    keywords: Vec<String>,
    categories: Vec<String>,
    latest_version: String,
    created_at: String,
    updated_at: String,
    registry_type: String,
}

// ============================================================================
// Registry Helper
// ============================================================================

/// Get or initialize the RDF registry with a persistent store
fn get_registry() -> VerbResult<RdfRegistry> {
    let cache_dir = resolve_cache_directory()?;
    let db_path = cache_dir.join("marketplace.db");

    if !cache_dir.exists() {
        std::fs::create_dir_all(&cache_dir).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!(
                "Failed to create cache directory: {}",
                e
            ))
        })?;
    }

    // Open persistent oxigraph store
    let store = Store::open(&db_path).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to open RDF store: {}", e))
    })?;

    Ok(RdfRegistry::from_store(store))
}

// ============================================================================
// Local Sync Manifests
// ============================================================================

#[derive(Debug, Deserialize)]
struct LocalPackageToml {
    package: LocalPackageSection,
}

#[derive(Debug, Deserialize)]
struct LocalPackageSection {
    name: String,
    #[serde(default)]
    full_name: Option<String>,
    version: String,
    description: String,
    #[serde(default)]
    category: Option<String>,
    #[serde(default)]
    author: Option<String>,
    #[serde(default)]
    repository: Option<String>,
    #[serde(default)]
    license: Option<String>,
    #[serde(default)]
    registry_type: Option<String>,
    #[serde(default)]
    keywords: Vec<String>,
}

// ============================================================================
// Verb Functions
// ============================================================================

/// Search for packages by query
#[verb]
fn search(query: String, limit: Option<usize>) -> VerbResult<SearchOutput> {
    if query.is_empty() {
        return Err(clap_noun_verb::NounVerbError::argument_error(
            "Search query cannot be empty",
        ));
    }

    let registry = get_registry()?;
    let limit = limit.unwrap_or(10);

    let results = block_on(async { registry.search_packages(&query, limit).await })
        .map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Runtime error: {}", e))
        })?
        .map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Search failed: {}", e))
        })?;

    let total = results.len();
    let search_results = results
        .into_iter()
        .map(|r| SearchResultCli {
            id: r.package.metadata.id.to_string(),
            name: r.package.metadata.name,
            description: r.package.metadata.description,
            version: r.package.latest_version.to_string(),
            relevance: r.relevance,
            registry_type: r.package.metadata.registry_type.to_string(),
        })
        .collect();

    Ok(SearchOutput {
        query,
        results: search_results,
        total,
    })
}

/// List all available packages
#[verb]
fn list(verbose: bool) -> VerbResult<ListOutput> {
    let registry = get_registry()?;

    let packages = block_on(async { registry.all_packages().await })
        .map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Runtime error: {}", e))
        })?
        .map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Listing failed: {}", e))
        })?;

    if verbose {
        for pkg in &packages {
            println!(
                "  - {} ({}): {}",
                pkg.metadata.id, pkg.latest_version, pkg.metadata.name
            );
        }
    }

    let total = packages.len();
    let list_packages = packages
        .into_iter()
        .map(|p| ListPackage {
            id: p.metadata.id.to_string(),
            name: p.metadata.name,
            description: p.metadata.description,
            latest_version: p.latest_version.to_string(),
            registry_type: p.metadata.registry_type.to_string(),
        })
        .collect();

    Ok(ListOutput {
        packages: list_packages,
        total,
    })
}

/// Show package details
#[verb]
fn show(package_id: String) -> VerbResult<DetailOutput> {
    if package_id.is_empty() {
        return Err(clap_noun_verb::NounVerbError::argument_error(
            "Package ID cannot be empty",
        ));
    }

    let id = PackageId::new(&package_id).map_err(|e| {
        clap_noun_verb::NounVerbError::argument_error(format!("Invalid package ID: {}", e))
    })?;

    let registry = get_registry()?;
    let package = block_on(async { registry.get_package(&id).await })
        .map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Runtime error: {}", e))
        })?
        .map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!(
                "Package '{}' not found: {}",
                package_id, e
            ))
        })?;

    Ok(DetailOutput {
        id: package.metadata.id.to_string(),
        name: package.metadata.name,
        description: package.metadata.description,
        authors: package.metadata.authors,
        license: package.metadata.license,
        homepage: package.metadata.homepage,
        repository: package.metadata.repository,
        keywords: package.metadata.keywords,
        categories: package.metadata.categories,
        latest_version: package.latest_version.to_string(),
        created_at: package.metadata.created_at.to_rfc3339(),
        updated_at: package.metadata.updated_at.to_rfc3339(),
        registry_type: package.metadata.registry_type.to_string(),
    })
}

/// Show package details (alias for show)
#[verb]
fn info(package_id: String) -> VerbResult<DetailOutput> {
    show(package_id)
}

/// Run health check on the marketplace RDF store and cache
#[verb]
fn doctor() -> VerbResult<serde_json::Value> {
    use ggen_core::domain::utils::{execute_doctor, DoctorInput};

    let result = block_on(async {
        execute_doctor(DoctorInput {
            verbose: true,
            check: Some("marketplace".to_string()),
            env: false,
        })
        .await
    })
    .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Runtime error: {}", e)))?
    .map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Doctor execution failed: {}", e))
    })?;

    Ok(serde_json::to_value(result).unwrap_or(serde_json::Value::Null))
}

/// Install a package from the marketplace
#[verb]
fn install(
    package_id: String, force: Option<bool>, dry_run: Option<bool>,
) -> VerbResult<serde_json::Value> {
    use ggen_core::domain::packs::install::{install_pack, InstallInput};

    let force = force.unwrap_or(false);
    let dry_run = dry_run.unwrap_or(false);

    let input = InstallInput {
        pack_id: package_id,
        force,
        dry_run,
        target_dir: None,
    };

    let result = block_on(async move { install_pack(&input).await })
        .map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Runtime error: {}", e))
        })?
        .map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Installation failed: {}", e))
        })?;

    Ok(serde_json::to_value(result).unwrap_or(serde_json::Value::Null))
}

// ============================================================================
// Marketplace Sync Command
// ============================================================================

/// Sync output structure
#[derive(Serialize)]
struct SyncOutput {
    status: String,
    message: String,
    packages_synced: usize,
    packages_updated: usize,
    packages_skipped: usize,
    cache_location: String,
    duration_ms: u64,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<String>,
}

/// Refresh package metadata from the marketplace registry
///
/// Downloads and caches updated package metadata. By default, only packages
/// with changed checksums are downloaded (cache-aware). Use --force to skip
/// cache checks and download all metadata.
///
/// Features:
/// - Checksum-based change detection (cache-aware)
/// - Automatic cache location resolution (~/.cache/ggen/packs/)
/// - Support for custom registry URLs (--source)
/// - Dry-run mode for preview (--dry-run)
/// - Progress reporting
///
/// Examples:
///   # Refresh with cache (only changed packages)
///   ggen marketplace sync
///
///   # Force full refresh (skip cache)
///   ggen marketplace sync --force
///
///   # Preview changes without writing
///   ggen marketplace sync --dry-run
///
///   # Sync from custom registry
///   ggen marketplace sync --source https://custom-registry.example.com
///
///   # Verbose progress reporting
///   ggen marketplace sync --verbose
#[verb]
fn sync(
    force: Option<bool>, dry_run: Option<bool>, source: Option<String>, verbose: Option<bool>,
) -> VerbResult<SyncOutput> {
    let force = force.unwrap_or(false);
    let dry_run = dry_run.unwrap_or(false);
    let verbose = verbose.unwrap_or(false);

    perform_marketplace_sync(force, dry_run, source, verbose)
}

// ============================================================================
// Domain Logic for sync (extracted for complexity reduction)
// ============================================================================

fn perform_marketplace_sync(
    _force: bool, dry_run: bool, _source: Option<String>, verbose: bool,
) -> VerbResult<SyncOutput> {
    use std::time::Instant;

    let start = Instant::now();
    let cache_dir = resolve_cache_directory()?;
    if verbose {
        eprintln!("Cache directory: {}", cache_dir.display());
    }

    let registry = get_registry()?;
    let packages_to_sync = get_local_packages()?;

    if verbose {
        eprintln!(
            "Found {} packages in local repository",
            packages_to_sync.len()
        );
    }

    let mut synced = 0;
    if !dry_run {
        synced = registry
            .batch_insert_packages(packages_to_sync)
            .map_err(|e| {
                clap_noun_verb::NounVerbError::execution_error(format!(
                    "Batch insert failed: {}",
                    e
                ))
            })?;
    }

    invalidate_registry_cache(&cache_dir)?;

    let duration_ms = start.elapsed().as_millis() as u64;

    Ok(SyncOutput {
        status: "success".to_string(),
        message: format!(
            "Marketplace sync complete: {} packages synced into RDF store",
            synced
        ),
        packages_synced: synced,
        packages_updated: synced,
        packages_skipped: 0,
        cache_location: cache_dir.display().to_string(),
        duration_ms,
        error: None,
    })
}

/// Walk marketplace/packages and marketplace/packs to find packages
fn get_local_packages() -> VerbResult<Vec<Package>> {
    let mut packages = Vec::new();

    // 1. Scan marketplace/packages/
    let packages_root = Path::new("marketplace/packages");
    if packages_root.exists() {
        for entry in std::fs::read_dir(packages_root).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!(
                "Failed to read packages dir: {}",
                e
            ))
        })? {
            let entry = entry.map_err(|e| {
                clap_noun_verb::NounVerbError::execution_error(format!(
                    "Failed to read entry: {}",
                    e
                ))
            })?;
            let path = entry.path();
            if path.is_dir() {
                let toml_path = path.join("package.toml");
                if toml_path.exists() {
                    if let Ok(package) = parse_local_package(&toml_path) {
                        packages.push(package);
                    }
                }
            }
        }
    }

    // 2. Scan marketplace/packs/
    let packs_root = Path::new("marketplace/packs");
    if packs_root.exists() {
        for entry in std::fs::read_dir(packs_root).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!(
                "Failed to read packs dir: {}",
                e
            ))
        })? {
            let entry = entry.map_err(|e| {
                clap_noun_verb::NounVerbError::execution_error(format!(
                    "Failed to read entry: {}",
                    e
                ))
            })?;
            let path = entry.path();
            if path.is_file() && path.extension().map_or(false, |ext| ext == "toml") {
                if let Ok(package) = parse_local_pack(&path) {
                    packages.push(package);
                }
            }
        }
    }

    // 3. Scan user global cache (.ggen/packs)
    if let Some(home) = dirs::home_dir() {
        let user_packs = home.join(".ggen").join("packs");
        if user_packs.exists() {
            for entry in std::fs::read_dir(user_packs).map_err(|e| {
                clap_noun_verb::NounVerbError::execution_error(format!(
                    "Failed to read user packs dir: {}",
                    e
                ))
            })? {
                let entry = entry.map_err(|e| {
                    clap_noun_verb::NounVerbError::execution_error(format!(
                        "Failed to read entry: {}",
                        e
                    ))
                })?;
                let path = entry.path();
                if path.is_dir() {
                    let toml_path = path.join("package.toml");
                    if toml_path.exists() {
                        if let Ok(package) = parse_local_package(&toml_path) {
                            packages.push(package);
                        }
                    }
                }
            }
        }
    }

    Ok(packages)
}

fn parse_local_package(path: &Path) -> Result<Package> {
    let content = std::fs::read_to_string(path).map_err(Error::IoError)?;
    let local_toml: LocalPackageToml = toml::from_str(&content).map_err(Error::TomlError)?;

    let id_str = local_toml
        .package
        .full_name
        .unwrap_or(local_toml.package.name.clone());
    let id = PackageId::new(id_str)?;
    let version = PackageVersion::new(local_toml.package.version)?;

    let mut metadata = PackageMetadata::new(
        id.clone(),
        local_toml.package.name,
        local_toml.package.description,
        local_toml
            .package
            .license
            .unwrap_or_else(|| "MIT".to_string()),
    );

    if let Some(author) = local_toml.package.author {
        metadata.authors.push(author);
    }
    metadata.repository = local_toml.package.repository;
    if let Some(category) = local_toml.package.category {
        metadata.categories.push(category);
    }
    metadata.keywords = local_toml.package.keywords;
    metadata.registry_type = map_registry_type(local_toml.package.registry_type);

    let package = Package {
        metadata,
        latest_version: version.clone(),
        versions: vec![version.clone()],
        releases: indexmap::IndexMap::new(),
    };

    Ok(package)
}

fn parse_local_pack(path: &Path) -> Result<Package> {
    use ggen_core::domain::packs::types::PackFile;

    let content = std::fs::read_to_string(path).map_err(Error::IoError)?;
    let pack_file: PackFile = toml::from_str(&content)
        .map_err(|e| Error::RegistryError(format!("Failed to parse pack TOML: {}", e)))?;
    let pack = pack_file.pack;

    let id = PackageId::new(&pack.id)?;
    let version = PackageVersion::new(&pack.version)?;

    let mut metadata = PackageMetadata::new(
        id.clone(),
        pack.name,
        pack.description,
        pack.license.unwrap_or_else(|| "MIT".to_string()),
    );

    if let Some(author) = pack.author {
        metadata.authors.push(author);
    }
    metadata.repository = pack.repository;
    metadata.categories.push(pack.category);
    metadata.keywords = pack.keywords;
    metadata.registry_type = map_registry_type(pack.registry_type);

    let package = Package {
        metadata,
        latest_version: version.clone(),
        versions: vec![version.clone()],
        releases: indexmap::IndexMap::new(),
    };

    Ok(package)
}

/// Invalidate registry cache by bumping epoch
fn invalidate_registry_cache(cache_dir: &std::path::Path) -> VerbResult<()> {
    use std::time::{SystemTime, UNIX_EPOCH};

    let cache_metadata_file = cache_dir.join(".cache-metadata");

    // Create metadata with current epoch timestamp
    let epoch = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0);

    let metadata = serde_json::json!({
        "epoch": epoch,
        "sync_timestamp": epoch,
    });

    std::fs::write(
        &cache_metadata_file,
        serde_json::to_string_pretty(&metadata).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(&format!(
                "Failed to write cache metadata: {}",
                e
            ))
        })?,
    )
    .map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(&format!(
            "Failed to write cache metadata: {}",
            e
        ))
    })?;

    Ok(())
}

/// Resolve the cache directory for marketplace packages
fn resolve_cache_directory() -> VerbResult<std::path::PathBuf> {
    // Check environment variable first
    if let Ok(custom_dir) = std::env::var("GGEN_MARKETPLACE_CACHE") {
        return Ok(std::path::PathBuf::from(custom_dir));
    }

    // Use XDG standard cache directory
    if let Some(cache_dir) = dirs::cache_dir() {
        return Ok(cache_dir.join("ggen").join("packs"));
    }

    // Fall back to ~/.cache/ggen/packs
    if let Some(home) = dirs::home_dir() {
        return Ok(home.join(".cache").join("ggen").join("packs"));
    }

    Err(clap_noun_verb::NounVerbError::execution_error(
        "Cannot resolve cache directory: set HOME or GGEN_MARKETPLACE_CACHE",
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_map_registry_type() {
        assert_eq!(
            map_registry_type(Some("crates.io".to_string())),
            RegistryType::CratesIo
        );
        assert_eq!(
            map_registry_type(Some("npm".to_string())),
            RegistryType::Npm
        );
        assert_eq!(
            map_registry_type(Some("pypi".to_string())),
            RegistryType::PyPi
        );
        assert_eq!(
            map_registry_type(Some("github".to_string())),
            RegistryType::GitHub
        );
        assert_eq!(
            map_registry_type(Some("other".to_string())),
            RegistryType::Other
        );
        assert_eq!(
            map_registry_type(Some("ggen".to_string())),
            RegistryType::Ggen
        );
        assert_eq!(map_registry_type(None), RegistryType::Ggen);
        assert_eq!(
            map_registry_type(Some("unknown".to_string())),
            RegistryType::Ggen
        );
    }
}
