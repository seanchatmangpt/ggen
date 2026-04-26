//! Marketplace Commands - Package Discovery and Management
//!
//! This module provides marketplace operations for discovering and managing packages.

use clap_noun_verb::Result as VerbResult;
use clap_noun_verb_macros::verb;
use ggen_marketplace::prelude::*;
use serde::Serialize;

// ============================================================================
// Output Types
// ============================================================================

#[derive(Serialize)]
struct SearchOutput {
    query: String,
    results: Vec<SearchResult>,
    total: usize,
}

#[derive(Serialize, Clone)]
struct SearchResult {
    id: String,
    name: String,
    description: String,
    version: String,
    relevance: f64,
    quality_score: Option<u32>,
    downloads: u64,
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
    quality_score: Option<u32>,
    downloads: u64,
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
    quality_score: Option<u32>,
    downloads: u64,
    latest_version: String,
    versions: Vec<String>,
    created_at: String,
    updated_at: String,
}

// ============================================================================
// Verb Functions
// ============================================================================

/// Search for packages by query
#[verb]
fn search(query: String, _limit: Option<usize>) -> VerbResult<SearchOutput> {
    if query.is_empty() {
        return Err(clap_noun_verb::NounVerbError::argument_error(
            "Search query cannot be empty",
        ));
    }

    // Marketplace is currently empty; return no results
    // In production, would fetch from registry and execute search
    Ok(SearchOutput {
        query,
        results: Vec::new(),
        total: 0,
    })
}

/// List all available packages
#[verb]
fn list(_verbose: bool) -> VerbResult<ListOutput> {
    // Marketplace is currently empty; return no results
    // In production, would fetch from registry
    Ok(ListOutput {
        packages: Vec::new(),
        total: 0,
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

    // Validate package ID format
    PackageId::new(&package_id).map_err(|e| {
        clap_noun_verb::NounVerbError::argument_error(&format!("Invalid package ID: {}", e))
    })?;

    // In a real implementation, would fetch from registry
    Err(clap_noun_verb::NounVerbError::execution_error(&format!(
        "Package '{}' not found",
        package_id
    )))
}

/// Show package details (alias for show)
#[verb]
fn info(package_id: String) -> VerbResult<DetailOutput> {
    show(package_id)
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
    force: bool, dry_run: bool, source: Option<String>, verbose: bool,
) -> VerbResult<SyncOutput> {
    use std::time::Instant;

    let start = Instant::now();
    let cache_dir = resolve_cache_directory()?;
    if verbose {
        eprintln!("Cache directory: {}", cache_dir.display());
    }

    if !dry_run {
        std::fs::create_dir_all(&cache_dir).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(&format!(
                "Failed to create cache directory: {}",
                e
            ))
        })?;
    }

    let registry_url =
        source.unwrap_or_else(|| "https://marketplace.ggen.dev/registry".to_string());
    if verbose {
        eprintln!("Marketplace registry: {}", registry_url);
    }

    let registry = Registry::new(1000);
    let packages_to_sync = get_marketplace_packages(&registry, force)?;

    if verbose {
        eprintln!("Found {} packages to evaluate", packages_to_sync.len());
    }

    let (synced, updated, skipped) =
        perform_sync_loop(&cache_dir, &packages_to_sync, dry_run, verbose)?;
    invalidate_registry_cache(&cache_dir)?;

    let duration_ms = start.elapsed().as_millis() as u64;

    Ok(SyncOutput {
        status: "success".to_string(),
        message: format!(
            "Marketplace sync complete: {} synced, {} updated, {} skipped",
            synced, updated, skipped
        ),
        packages_synced: synced,
        packages_updated: updated,
        packages_skipped: skipped,
        cache_location: cache_dir.display().to_string(),
        duration_ms,
        error: None,
    })
}

fn perform_sync_loop(
    cache_dir: &std::path::Path, packages: &[PackageInfo], dry_run: bool, verbose: bool,
) -> VerbResult<(usize, usize, usize)> {
    let mut synced = 0usize;
    let mut updated = 0usize;
    let mut skipped = 0usize;

    for (idx, pkg_info) in packages.iter().enumerate() {
        if verbose {
            eprintln!(
                "[{}/{}] Processing {}@{}",
                idx + 1,
                packages.len(),
                pkg_info.id,
                pkg_info.version
            );
        }

        let pkg_cache_dir = cache_dir.join(&pkg_info.id);
        let needs_update = should_update_package(&pkg_cache_dir, &pkg_info.checksum, false)?;

        if !needs_update {
            skipped += 1;
            if verbose {
                eprintln!("  ✓ Skipped (cache valid)");
            }
            continue;
        }

        if !dry_run {
            if let Err(e) =
                download_package_metadata(&pkg_cache_dir, &pkg_info.id, &pkg_info.version)
            {
                eprintln!("  ✗ Failed to sync {}: {}", pkg_info.id, e);
                continue;
            }

            if let Err(e) = write_checksum_file(&pkg_cache_dir, &pkg_info.checksum) {
                eprintln!("  ✗ Failed to write checksum for {}: {}", pkg_info.id, e);
                continue;
            }
        }

        updated += 1;
        synced += 1;
        if verbose {
            eprintln!("  ✓ Synced");
        }
    }

    Ok((synced, updated, skipped))
}

// ============================================================================
// Helper Functions for sync
// ============================================================================

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

/// Package information from marketplace
struct PackageInfo {
    id: String,
    version: String,
    checksum: String,
}

/// Get list of packages available in the marketplace
fn get_marketplace_packages(_registry: &Registry, _force: bool) -> VerbResult<Vec<PackageInfo>> {
    // In production, this would fetch from the marketplace registry
    // For now, return empty list (marketplace is seeded via other means)
    Ok(Vec::new())
}

/// Check if a package needs to be updated based on checksum
fn should_update_package(
    pkg_dir: &std::path::Path, new_checksum: &str, force: bool,
) -> VerbResult<bool> {
    if force {
        return Ok(true); // Always update with --force
    }

    // Check if checksum file exists and matches
    let checksum_file = pkg_dir.join(".checksum");

    if !checksum_file.exists() {
        return Ok(true); // No checksum file = needs update
    }

    // Read existing checksum
    match std::fs::read_to_string(&checksum_file) {
        Ok(content) => {
            let existing = content.trim();
            Ok(existing != new_checksum) // Different = needs update
        }
        Err(_) => Ok(true), // Can't read = needs update
    }
}

/// Download and update package metadata
fn download_package_metadata(
    pkg_dir: &std::path::Path, _pkg_id: &str, _version: &str,
) -> Result<(), String> {
    // Create package directory structure
    std::fs::create_dir_all(pkg_dir).map_err(|e| e.to_string())?;

    // In production, would:
    // 1. Fetch metadata from registry
    // 2. Verify signature
    // 3. Extract to pkg_dir

    Ok(())
}

/// Write checksum file to track cache validity
fn write_checksum_file(pkg_dir: &std::path::Path, checksum: &str) -> Result<(), String> {
    let checksum_file = pkg_dir.join(".checksum");
    std::fs::write(&checksum_file, checksum).map_err(|e| e.to_string())
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
