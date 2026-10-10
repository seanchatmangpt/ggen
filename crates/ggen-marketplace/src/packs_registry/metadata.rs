//! Pack metadata loading and management

use crate::marketplace::error::Result;
use crate::packs_registry::types::{Pack, PackFile};
use std::fs;
use std::path::PathBuf;

/// Resolve the packs directory without erroring when none can be found.
///
/// Resolution order (first match wins):
/// 1. `GGEN_PACKS_DIR` environment variable (highest priority)
/// 2. Relative paths (for development / source checkout)
/// 3. `~/.ggen/packs` (conventional home location for installed binaries)
///
/// Returns `None` when no candidate exists on disk yet — a fresh install with
/// nothing registered is a legitimate, non-error state, not a misconfiguration.
fn try_get_packs_dir() -> Option<PathBuf> {
    // 1. GGEN_PACKS_DIR env var override — highest priority
    if let Ok(env_dir) = std::env::var("GGEN_PACKS_DIR") {
        let p = PathBuf::from(env_dir);
        if p.exists() && p.is_dir() {
            return Some(p);
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
            return Some(path);
        }
    }

    // 3. ~/.ggen/packs (conventional home location for cargo-installed binaries)
    if let Some(home) = dirs::home_dir() {
        let p = home.join(".ggen").join("packs");
        if p.exists() && p.is_dir() {
            return Some(p);
        }
    }

    None
}

/// Get packs directory, erroring when none can be resolved.
///
/// Use this for operations that target a specific pack (load/show) where an
/// unresolvable directory is a real failure. Use [`try_get_packs_dir`] for
/// listing, where "nothing registered yet" is a valid empty result, not an
/// error.
pub fn get_packs_dir() -> Result<PathBuf> {
    try_get_packs_dir().ok_or_else(|| {
        crate::marketplace::error::Error::Other(
            "Packs directory not found. Set GGEN_PACKS_DIR or create ~/.ggen/packs/".to_string(),
        )
    })
}

/// Load pack from TOML file
pub fn load_pack_metadata(pack_id: &str) -> Result<Pack> {
    if pack_id.is_empty() {
        return Err(crate::marketplace::error::Error::Other(
            "Pack id must not be empty".to_string(),
        ));
    }

    let packs_dir = get_packs_dir()?;
    let pack_path = packs_dir.join(format!("{}.toml", pack_id));

    if !pack_path.exists() {
        // Report the resolved absolute path (matching what `pack doctor`/`pack
        // add` report) rather than a possibly-relative dev-checkout path, so
        // the error points somewhere the user can actually find on disk.
        let resolved_display = pack_path.canonicalize().unwrap_or_else(|_| {
            std::env::current_dir()
                .map(|cwd| cwd.join(&pack_path))
                .unwrap_or_else(|_| pack_path.clone())
        });
        return Err(crate::marketplace::error::Error::Other(format!(
            "Pack '{}' not found at {}",
            pack_id,
            resolved_display.display()
        )));
    }

    let content = fs::read_to_string(&pack_path)?;
    let pack_file: PackFile = star_toml::from_str(&content).map_err(|e| {
        crate::marketplace::error::Error::Other(format!(
            "Failed to parse pack '{}': {}",
            pack_id, e
        ))
    })?;

    Ok(pack_file.pack)
}

/// List all available packs
///
/// A packs directory that cannot be resolved at all (no `GGEN_PACKS_DIR`, no
/// dev-relative checkout, no `~/.ggen/packs`) means zero packs are
/// registered yet, not a failure — a fresh install must list cleanly.
pub fn list_packs(category: Option<&str>) -> Result<Vec<Pack>> {
    let Some(packs_dir) = try_get_packs_dir() else {
        return Ok(Vec::new());
    };
    let mut packs = Vec::new();

    for entry in fs::read_dir(&packs_dir)? {
        let entry = entry?;
        let path = entry.path();

        if path.extension().and_then(|s| s.to_str()) == Some("toml") {
            let content = fs::read_to_string(&path).map_err(|e| {
                crate::marketplace::error::Error::Other(format!(
                    "Failed to read pack {}: {}",
                    path.display(),
                    e
                ))
            })?;
            let pack_file = star_toml::from_str::<PackFile>(&content).map_err(|e| {
                crate::marketplace::error::Error::Other(format!(
                    "Failed to parse pack {}: {}",
                    path.display(),
                    e
                ))
            })?;
            let pack = pack_file.pack;
            if let Some(cat) = category {
                if pack.category == cat {
                    packs.push(pack);
                }
            } else {
                packs.push(pack);
            }
        }
    }

    Ok(packs)
}

/// Show pack details
pub fn show_pack(pack_id: &str) -> Result<Pack> {
    load_pack_metadata(pack_id)
}

/// Load a `pack.toml` from a pack directory, bridging the on-disk corpus shape
/// to the full [`PackFile`] model.
///
/// Real corpus pack.tomls (~400 across `~/ggen/packs` and
/// `~/ggen-marketplace/packs`) carry `[pack] name/version/description` and an
/// optional `[capabilities]` table, but predate the marketplace `Pack` model's
/// required fields. This function injects the canonical defaults for absent
/// required fields, matching how the registry resolves pack identity:
///
/// - `id`: the pack **directory name** (only if `[pack]` omits it)
/// - `name`: empty string if absent
/// - `version`: `"0.0.0"` if absent
/// - `description`: empty string if absent
/// - `category`: `"uncategorized"` if absent
/// - `packages`: empty if absent (the corpus does not carry a packages list;
///   the capability surface lives in `[capabilities]`, preserved verbatim as
///   `PackFile::capabilities`)
///
/// Present fields are never overwritten. Deterministic: the same directory
/// always yields an identical `PackFile`. IO and parse failures are typed
/// errors (`crate::marketplace::error::Error`).
pub fn pack_file_from_dir(dir: &std::path::Path) -> Result<PackFile> {
    let pack_path = dir.join("pack.toml");
    let raw = fs::read_to_string(&pack_path).map_err(|e| {
        crate::marketplace::error::Error::Other(format!(
            "Failed to read {}: {}",
            pack_path.display(),
            e
        ))
    })?;
    let mut value: toml::Value = star_toml::from_str(&raw).map_err(|e| {
        crate::marketplace::error::Error::Other(format!(
            "Failed to parse {}: {}",
            pack_path.display(),
            e
        ))
    })?;
    let dir_name = dir
        .file_name()
        .map(|n| n.to_string_lossy().to_string())
        .ok_or_else(|| {
            crate::marketplace::error::Error::Other(format!(
                "{}: path has no file-name component",
                dir.display()
            ))
        })?;
    let pack_table = value
        .get_mut("pack")
        .and_then(|p| p.as_table_mut())
        .ok_or_else(|| {
            crate::marketplace::error::Error::Other(format!(
                "{}: missing [pack] table",
                pack_path.display()
            ))
        })?;
    let defaults: &[(&str, toml::Value)] = &[
        ("id", toml::Value::String(dir_name)),
        ("name", toml::Value::String(String::new())),
        ("version", toml::Value::String("0.0.0".into())),
        ("description", toml::Value::String(String::new())),
        ("category", toml::Value::String("uncategorized".into())),
        ("packages", toml::Value::Array(vec![])),
    ];
    for (key, default) in defaults {
        pack_table
            .entry(key.to_string())
            .or_insert_with(|| default.clone());
    }
    value.try_into().map_err(|e| {
        crate::marketplace::error::Error::Other(format!(
            "Failed to deserialize {}: {}",
            pack_path.display(),
            e
        ))
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use serial_test::serial;

    /// Saves the prior value of an env var on construction and restores it
    /// (or removes it if previously unset) on Drop, so env mutation in one
    /// test cannot leak into another.
    struct EnvVarGuard {
        key: &'static str,
        previous: Option<std::ffi::OsString>,
    }

    impl EnvVarGuard {
        fn set(key: &'static str, value: &str) -> Self {
            let previous = std::env::var_os(key);
            std::env::set_var(key, value);
            Self { key, previous }
        }

        fn unset(key: &'static str) -> Self {
            let previous = std::env::var_os(key);
            std::env::remove_var(key);
            Self { key, previous }
        }
    }

    impl Drop for EnvVarGuard {
        fn drop(&mut self) {
            match &self.previous {
                None => std::env::remove_var(self.key),
                Some(v) => std::env::set_var(self.key, v),
            }
        }
    }

    #[test]
    #[serial(GGEN_PACKS_DIR)]
    fn test_ggen_packs_dir_env_var() {
        let temp = tempfile::TempDir::new().unwrap();
        let _guard = EnvVarGuard::set("GGEN_PACKS_DIR", temp.path().to_str().unwrap());
        let result = get_packs_dir();
        assert!(
            result.is_ok(),
            "expected Ok when GGEN_PACKS_DIR points to a valid dir"
        );
        assert_eq!(result.unwrap(), temp.path());
    }

    #[test]
    #[serial(GGEN_PACKS_DIR)]
    fn test_ggen_packs_dir_env_var_missing_dir_skipped() {
        // Points to a path that does not exist — should NOT succeed via env var,
        // must fall through to other lookups (which also fail in CI) → Err
        let _guard = EnvVarGuard::set("GGEN_PACKS_DIR", "/nonexistent/path/that/cannot/exist");
        let result = get_packs_dir();
        // The contract under test: a missing env-var dir is SKIPPED. Whatever
        // the fallbacks yield (Ok in a source checkout, Err in a bare CI env),
        // the resolved dir must never be the nonexistent env-var path, and an
        // Ok result must point at a real directory.
        if let Ok(dir) = result {
            assert_ne!(dir, PathBuf::from("/nonexistent/path/that/cannot/exist"));
            assert!(
                dir.is_dir(),
                "resolved packs dir must exist: {}",
                dir.display()
            );
        }
    }

    #[test]
    #[serial(GGEN_PACKS_DIR)]
    fn test_get_packs_dir_no_env_var_returns_err_when_absent() {
        // Ensure no GGEN_PACKS_DIR is set; relative paths and home path absent in
        // the test sandbox → function must return Err, not Ok(empty).
        let _guard = EnvVarGuard::unset("GGEN_PACKS_DIR");
        // Only assert Err if none of the fallback paths happen to exist.
        let relative_exists = PathBuf::from("marketplace/packs").exists()
            || PathBuf::from("../marketplace/packs").exists()
            || PathBuf::from("../../marketplace/packs").exists();
        let home_exists = dirs::home_dir()
            .map(|h| h.join(".ggen").join("packs").exists())
            .unwrap_or(false);
        if !relative_exists && !home_exists {
            let result = get_packs_dir();
            assert!(
                result.is_err(),
                "expected Err when no packs dir is reachable"
            );
        }
    }

    #[test]
    #[serial(GGEN_PACKS_DIR)]
    fn test_list_packs_returns_empty_not_err_when_no_dir_resolves() {
        // Reproduces the real CI failure this test was written to close:
        // `ggen pack list` in a fresh checkout with no GGEN_PACKS_DIR, no
        // dev-relative marketplace/packs/, and (in CI) no ~/.ggen/packs/ must
        // list zero packs, not refuse. Only meaningful when no fallback path
        // happens to exist in this sandbox — mirrors the sibling test above.
        let _guard = EnvVarGuard::unset("GGEN_PACKS_DIR");
        let relative_exists = PathBuf::from("marketplace/packs").exists()
            || PathBuf::from("../marketplace/packs").exists()
            || PathBuf::from("../../marketplace/packs").exists();
        let home_exists = dirs::home_dir()
            .map(|h| h.join(".ggen").join("packs").exists())
            .unwrap_or(false);
        if !relative_exists && !home_exists {
            let result = list_packs(None);
            assert!(
                result.is_ok(),
                "expected Ok(empty) when no packs dir is reachable, got Err: {:?}",
                result.err()
            );
            assert!(
                result.unwrap().is_empty(),
                "fresh install must list zero packs"
            );
        }
    }

    // ── Sabotage tests (coding-agent-mistakes.md §5) ─────────────────────────

    /// Sabotage §5 row 5: with GGEN_PACKS_DIR pointing at an EMPTY directory,
    /// `load_pack_metadata("acme/base")` must return Err referencing "not found".
    ///
    /// This proves Fail-Open (Mistake Class 1.3) is absent: a missing pack does
    /// not silently succeed or warn — it hard-fails with a clear error message.
    #[test]
    #[serial(GGEN_PACKS_DIR)]
    fn sabotage_empty_packs_dir_load_metadata_returns_not_found_err() {
        // Arrange — empty temp dir as the packs directory
        let empty_dir = tempfile::TempDir::new().unwrap();
        let _guard = EnvVarGuard::set("GGEN_PACKS_DIR", empty_dir.path().to_str().unwrap());

        // Act
        let result = load_pack_metadata("acme/base");

        // Assert — must be Err, and the message must say "not found"
        match result {
            Ok(_) => panic!(
                "load_pack_metadata must return Err when pack does not exist in the packs dir, \
                 got Ok instead — this is Fail-Open (coding-agent-mistakes.md §1.3)"
            ),
            Err(e) => {
                let msg = e.to_string();
                assert!(
                    msg.contains("not found") || msg.contains("acme/base"),
                    "error message must reference 'not found' or the pack id; got: {msg}"
                );
            }
        }
    }
}
