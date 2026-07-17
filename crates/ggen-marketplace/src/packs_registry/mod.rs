//! Pack domain logic
//!
//! This module provides domain operations for working with packs,
//! separating business logic from CLI concerns.
//!
//! Ported from `ggen-core/src/domain/packs/` (specs/014-ggen-core-replacement,
//! docs/jira/v26.7.16/06-MARKETPLACE-PACK-REGISTRY-MERGE.md). `install.rs` is
//! deliberately absent here -- its logic was merged into the already-more-mature
//! `crate::marketplace::install::Installer` instead of being relocated verbatim
//! (see that module's own merge note).

pub mod capability_registry;
pub mod compose;
pub mod dependency_graph;
pub mod external_fetcher;
pub mod generator;
pub mod metadata;
pub mod registry;
pub mod repository;
pub mod types;
pub mod validate;

use crate::marketplace::error::Error;
use serde::Serialize;

/// Check compatibility between packs
pub async fn check_packs_compatibility(
    pack_ids: &[String],
) -> Result<CheckCompatibilityResult, Error> {
    let mut packs = Vec::new();
    let mut load_errors = Vec::new();

    // Load all packs
    for pack_id in pack_ids {
        match load_pack(pack_id).await {
            Ok(pack) => packs.push(pack),
            Err(e) => load_errors.push(format!("Failed to load pack '{}': {}", pack_id, e)),
        }
    }

    if !load_errors.is_empty() {
        return Ok(CheckCompatibilityResult {
            compatible: false,
            pack_ids: pack_ids.to_vec(),
            conflicts: load_errors,
            warnings: vec![],
            message: "Failed to load one or more packs".to_string(),
        });
    }

    // Check for conflicts
    let mut all_packages = std::collections::HashSet::new();
    let mut conflicts = Vec::new();
    let warnings = Vec::new();

    for pack in &packs {
        for package in &pack.packages {
            if !all_packages.insert(package.clone()) {
                conflicts.push(format!(
                    "Package '{}' is included in multiple packs",
                    package
                ));
            }
        }
    }

    let compatible = conflicts.is_empty();
    let message = if compatible {
        "All packs are compatible".to_string()
    } else {
        "Found conflicts between packs".to_string()
    };

    Ok(CheckCompatibilityResult {
        compatible,
        pack_ids: pack_ids.to_vec(),
        conflicts,
        warnings,
        message,
    })
}

/// Load a pack's real metadata from the registry — the same source the CLI
/// (`ggen pack show`) and the agent facade (`PackAgent::show`, in `ggen-cli`)
/// read.
///
/// Fail-closed: a pack that is not present in the registry is a hard error, so
/// [`check_packs_compatibility`] reports it as a load failure instead of
/// fabricating a phantom pack whose package set is invented from the ID. (The
/// previous implementation returned a synthetic `package-<id>` pack, which made
/// compatibility checks pass against data that never existed.)
async fn load_pack(pack_id: &str) -> Result<LoadedPack, Error> {
    let pack = metadata::show_pack(pack_id)
        .map_err(|e| Error::Other(format!("Failed to load pack '{}': {}", pack_id, e)))?;
    Ok(LoadedPack {
        id: pack.id,
        name: pack.name,
        packages: pack.packages,
        dependencies: pack.dependencies.into_iter().map(|d| d.pack_id).collect(),
    })
}

#[derive(Debug, Serialize)]
pub struct CheckCompatibilityResult {
    pub compatible: bool,
    pub pack_ids: Vec<String>,
    pub conflicts: Vec<String>,
    pub warnings: Vec<String>,
    pub message: String,
}

#[derive(Debug)]
pub struct LoadedPack {
    pub id: String,
    pub name: String,
    pub packages: Vec<String>,
    pub dependencies: Vec<String>,
}
