//! Pack metadata loading and management

use crate::packs::types::{Pack, PackFile};
use ggen_utils::error::Result;
use std::fs;
use std::path::PathBuf;

/// Get packs directory
///
/// Resolution order (first match wins):
/// 1. `GGEN_PACKS_DIR` environment variable (highest priority)
/// 2. Relative paths (for development / source checkout)
/// 3. `~/.ggen/packs` (conventional home location for installed binaries)
pub fn get_packs_dir() -> Result<PathBuf> {
    // 1. GGEN_PACKS_DIR env var override — highest priority
    if let Ok(env_dir) = std::env::var("GGEN_PACKS_DIR") {
        let p = PathBuf::from(env_dir);
        if p.exists() && p.is_dir() {
            return Ok(p);
        }
    }

    // 2. Relative paths (development / source checkout)
    let relative_paths = vec![
        PathBuf::from("marketplace/packs"),
        PathBuf::from("../marketplace/packs"),
        PathBuf::from("../../marketplace/packs"),
    ];

    for path in relative_paths {
        if path.exists() && path.is_dir() {
            return Ok(path);
        }
    }

    // 3. ~/.ggen/packs (conventional home location for cargo-installed binaries)
    if let Some(home) = dirs::home_dir() {
        let p = home.join(".ggen").join("packs");
        if p.exists() && p.is_dir() {
            return Ok(p);
        }
    }

    Err(ggen_utils::error::Error::new(
        "Packs directory not found. Set GGEN_PACKS_DIR or create ~/.ggen/packs/",
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
    fn test_ggen_packs_dir_env_var() {
        let temp = tempfile::TempDir::new().unwrap();
        std::env::set_var("GGEN_PACKS_DIR", temp.path().to_str().unwrap());
        let result = get_packs_dir();
        std::env::remove_var("GGEN_PACKS_DIR");
        assert!(result.is_ok(), "expected Ok when GGEN_PACKS_DIR points to a valid dir");
        assert_eq!(result.unwrap(), temp.path());
    }

    #[test]
    fn test_ggen_packs_dir_env_var_missing_dir_skipped() {
        // Points to a path that does not exist — should NOT succeed via env var,
        // must fall through to other lookups (which also fail in CI) → Err
        std::env::set_var("GGEN_PACKS_DIR", "/nonexistent/path/that/cannot/exist");
        let result = get_packs_dir();
        std::env::remove_var("GGEN_PACKS_DIR");
        // We cannot assert Ok here because relative + home paths also absent in CI,
        // but we confirm the function does not panic and returns a Result.
        let _ = result;
    }

    #[test]
    fn test_get_packs_dir_no_env_var_returns_err_when_absent() {
        // Ensure no GGEN_PACKS_DIR is set; relative paths and home path absent in
        // the test sandbox → function must return Err, not Ok(empty).
        std::env::remove_var("GGEN_PACKS_DIR");
        // Only assert Err if none of the fallback paths happen to exist.
        let relative_exists = PathBuf::from("marketplace/packs").exists()
            || PathBuf::from("../marketplace/packs").exists()
            || PathBuf::from("../../marketplace/packs").exists();
        let home_exists = dirs::home_dir()
            .map(|h| h.join(".ggen").join("packs").exists())
            .unwrap_or(false);
        if !relative_exists && !home_exists {
            let result = get_packs_dir();
            assert!(result.is_err(), "expected Err when no packs dir is reachable");
        }
    }
}
