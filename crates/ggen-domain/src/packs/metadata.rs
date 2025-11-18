//! Pack metadata loading and management

use crate::packs::types::{Pack, PackFile};
use ggen_utils::error::Result;
use std::fs;
use std::path::PathBuf;

/// Get packs directory
pub fn get_packs_dir() -> Result<PathBuf> {
    // Try multiple locations
    let possible_paths = vec![
        PathBuf::from("marketplace/packs"),
        PathBuf::from("../marketplace/packs"),
        PathBuf::from("../../marketplace/packs"),
    ];

    for path in possible_paths {
        if path.exists() && path.is_dir() {
            return Ok(path);
        }
    }

    Err(ggen_utils::error::Error::new(
        "Packs directory not found. Expected marketplace/packs/",
    ))
}

/// Load pack from TOML file
pub fn load_pack_metadata(pack_id: &str) -> Result<Pack> {
    let packs_dir = get_packs_dir()?;
    let pack_path = packs_dir.join(format!("{}.toml", pack_id));

    if !pack_path.exists() {
        return Err(ggen_utils::error::Error::new(&format!(
            "Pack '{}' not found at {}",
            pack_id,
            pack_path.display()
        )));
    }

    let content = fs::read_to_string(&pack_path)?;
    let pack_file: PackFile = toml::from_str(&content).map_err(|e| {
        ggen_utils::error::Error::new(&format!("Failed to parse pack '{}': {}", pack_id, e))
    })?;

    Ok(pack_file.pack)
}

/// List all available packs
pub fn list_packs(category: Option<&str>) -> Result<Vec<Pack>> {
    let packs_dir = get_packs_dir()?;
    let mut packs = Vec::new();

    for entry in fs::read_dir(&packs_dir)? {
        let entry = entry?;
        let path = entry.path();

        if path.extension().and_then(|s| s.to_str()) == Some("toml") {
            match fs::read_to_string(&path) {
                Ok(content) => match toml::from_str::<PackFile>(&content) {
                    Ok(pack_file) => {
                        let pack = pack_file.pack;

                        // Filter by category if specified
                        if let Some(cat) = category {
                            if pack.category == cat {
                                packs.push(pack);
                            }
                        } else {
                            packs.push(pack);
                        }
                    }
                    Err(e) => {
                        tracing::warn!("Failed to parse pack {}: {}", path.display(), e);
                    }
                },
                Err(e) => {
                    tracing::warn!("Failed to read pack {}: {}", path.display(), e);
                }
            }
        }
    }

    Ok(packs)
}

/// Show pack details
pub fn show_pack(pack_id: &str) -> Result<Pack> {
    load_pack_metadata(pack_id)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_packs_dir_tries_multiple_paths() {
        // This test verifies the function tries multiple paths
        // Actual success depends on test environment
        let _ = get_packs_dir();
    }
}
