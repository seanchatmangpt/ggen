//! Pack domain logic
//!
//! This module provides domain operations for working with packs,
//! separating business logic from CLI concerns.

use ggen_utils::error::Error;
use serde::Serialize;

/// Check compatibility between packs
pub async fn check_packs_compatibility(pack_ids: &[String]) -> Result<CheckCompatibilityResult, Error> {
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

// Mock implementation for loading packs - replace with actual implementation
async fn load_pack(pack_id: &str) -> Result<LoadedPack, Error> {
    // This should load actual packs from storage
    Ok(LoadedPack {
        id: pack_id.to_string(),
        name: format!("Pack {}", pack_id),
        packages: vec![format!("package-{}", pack_id)],
        dependencies: vec![],
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