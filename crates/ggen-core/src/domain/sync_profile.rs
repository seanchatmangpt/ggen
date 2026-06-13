//! Sync profile enforcement for `ggen sync --profile` and `--locked`.
//!
//! Known enforcement profiles control pre-flight checks before the sync
//! pipeline runs.  The lockfile check is a real `Path::exists()` call —
//! no test doubles required.

use crate::packs::lockfile::PackLockfile;
use std::str::FromStr;

/// Known enforcement profiles for `ggen sync --profile <name>`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SyncProfile {
    /// Strict enterprise governance: lockfile required, no unsigned packs.
    EnterpriseStrict,
    /// Permissive: most checks relaxed; suitable for exploration.
    Permissive,
    /// Development / dev alias: same as permissive with debug-friendly messages.
    Development,
}

impl std::str::FromStr for SyncProfile {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "enterprise-strict" => Ok(Self::EnterpriseStrict),
            "permissive" => Ok(Self::Permissive),
            "development" | "dev" => Ok(Self::Development),
            other => Err(format!(
                "Unknown profile '{}'. Known: enterprise-strict, permissive, development",
                other
            )),
        }
    }
}

impl SyncProfile {
    /// Canonical string representation of the profile.
    pub fn as_str(&self) -> &str {
        match self {
            Self::EnterpriseStrict => "enterprise-strict",
            Self::Permissive => "permissive",
            Self::Development => "development",
        }
    }

    /// Returns `true` when this profile requires a `.ggen/packs.lock` file to
    /// exist before sync may proceed.
    pub fn requires_lockfile(&self) -> bool {
        matches!(self, Self::EnterpriseStrict)
    }

    /// Returns `true` when this profile allows unsigned packs.
    pub fn allows_unsigned_packs(&self) -> bool {
        !matches!(self, Self::EnterpriseStrict)
    }
}

/// Verify that every entry in a loaded lockfile has a non-empty `integrity`
/// field.  Returns `Err` listing all pack IDs that are missing integrity when
/// `--locked` is in effect.
fn check_integrity_fields(lockfile: &PackLockfile) -> Result<(), String> {
    let missing: Vec<&str> = lockfile
        .packs
        .iter()
        .filter(|(_, pack)| {
            pack.integrity
                .as_deref()
                .map(|v| v.is_empty())
                .unwrap_or(true)
        })
        .map(|(id, _)| id.as_str())
        .collect();

    if missing.is_empty() {
        return Ok(());
    }

    Err(format!(
        "Lockfile integrity check failed (--locked): the following pack(s) are missing a \
         non-empty integrity digest: [{}]. Run `ggen packs add <pack>` to reinstall them \
         with integrity hashes.",
        missing.join(", ")
    ))
}

/// Pre-flight check executed before `ggen sync` runs.
///
/// Returns `Ok(())` when all profile requirements are satisfied, or an
/// `Err(String)` with a human-readable explanation when they are not.
///
/// # Arguments
/// * `profile`        – Optional profile name from `--profile <name>`.
/// * `locked`         – Whether `--locked` was passed on the CLI.
/// * `workspace_root` – The working directory used to resolve `.ggen/packs.lock`.
pub fn validate_sync_preconditions(
    profile: Option<&str>, locked: bool, workspace_root: &std::path::Path,
) -> Result<(), String> {
    let profile = match profile {
        Some(p) => SyncProfile::from_str(p)?,
        None => {
            // No profile — only enforce --locked if it was explicitly requested.
            if locked {
                let lockfile_path = workspace_root.join(".ggen").join("packs.lock");
                if !lockfile_path.exists() {
                    return Err(
                        "Lockfile required (--locked) but .ggen/packs.lock not found. \
                         Run `ggen packs add <pack>` first."
                            .to_string(),
                    );
                }
                // Load and verify integrity of every entry.
                let lockfile = PackLockfile::from_file(&lockfile_path)
                    .map_err(|e| format!("Failed to load lockfile (--locked): {e}"))?;
                check_integrity_fields(&lockfile)?;
            }
            return Ok(());
        }
    };

    // --locked or profile.requires_lockfile() both mandate the lockfile.
    if locked || profile.requires_lockfile() {
        let lockfile_path = workspace_root.join(".ggen").join("packs.lock");
        if !lockfile_path.exists() {
            return Err(format!(
                "Lockfile required (profile='{}', --locked={}) but .ggen/packs.lock not found. \
                 Run `ggen packs add <pack>` first.",
                profile.as_str(),
                locked
            ));
        }
        if locked {
            // Load and verify integrity of every entry.
            let lockfile = PackLockfile::from_file(&lockfile_path)
                .map_err(|e| format!("Failed to load lockfile (--locked): {e}"))?;
            check_integrity_fields(&lockfile)?;
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::packs::lockfile::{LockedPack, PackLockfile, PackSource};
    use chrono::Utc;
    use std::fs;
    use tempfile::TempDir;

    // ── helpers ──────────────────────────────────────────────────────────────

    /// Write a minimal valid lockfile (no packs) to `.ggen/packs.lock`.
    fn write_empty_lockfile(dir: &TempDir) {
        let lockfile = PackLockfile::new("4.0.0");
        let ggen_dir = dir.path().join(".ggen");
        fs::create_dir_all(&ggen_dir).unwrap();
        lockfile.save(&ggen_dir.join("packs.lock")).unwrap();
    }

    /// Write a lockfile that contains one pack with a valid integrity field.
    fn write_lockfile_with_integrity(dir: &TempDir) {
        let mut lockfile = PackLockfile::new("4.0.0");
        lockfile.add_pack(
            "io.ggen.test.pack",
            LockedPack {
                version: "1.0.0".to_string(),
                source: PackSource::Registry {
                    url: "https://registry.ggen.io".to_string(),
                },
                integrity: Some(
                    "sha256-e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
                        .to_string(),
                ),
                installed_at: Utc::now(),
                dependencies: vec![],
            },
        );
        let ggen_dir = dir.path().join(".ggen");
        fs::create_dir_all(&ggen_dir).unwrap();
        lockfile.save(&ggen_dir.join("packs.lock")).unwrap();
    }

    /// Write a lockfile that contains one pack with integrity = None.
    fn write_lockfile_missing_integrity(dir: &TempDir, pack_id: &str) {
        let mut lockfile = PackLockfile::new("4.0.0");
        lockfile.add_pack(
            pack_id,
            LockedPack {
                version: "1.0.0".to_string(),
                source: PackSource::Registry {
                    url: "https://registry.ggen.io".to_string(),
                },
                integrity: None,
                installed_at: Utc::now(),
                dependencies: vec![],
            },
        );
        let ggen_dir = dir.path().join(".ggen");
        fs::create_dir_all(&ggen_dir).unwrap();
        lockfile.save(&ggen_dir.join("packs.lock")).unwrap();
    }

    // ── SyncProfile::from_str ────────────────────────────────────────────────

    #[test]
    fn parse_known_profiles() {
        assert_eq!(
            SyncProfile::from_str("enterprise-strict").unwrap(),
            SyncProfile::EnterpriseStrict
        );
        assert_eq!(
            SyncProfile::from_str("permissive").unwrap(),
            SyncProfile::Permissive
        );
        assert_eq!(
            SyncProfile::from_str("development").unwrap(),
            SyncProfile::Development
        );
        assert_eq!(
            SyncProfile::from_str("dev").unwrap(),
            SyncProfile::Development
        );
    }

    #[test]
    fn parse_unknown_profile_returns_error() {
        let err = SyncProfile::from_str("bogus").unwrap_err();
        assert!(err.contains("Unknown profile 'bogus'"), "error was: {err}");
        assert!(err.contains("enterprise-strict"), "error was: {err}");
    }

    // ── SyncProfile methods ──────────────────────────────────────────────────

    #[test]
    fn enterprise_strict_requires_lockfile() {
        assert!(SyncProfile::EnterpriseStrict.requires_lockfile());
        assert!(!SyncProfile::Permissive.requires_lockfile());
        assert!(!SyncProfile::Development.requires_lockfile());
    }

    #[test]
    fn enterprise_strict_forbids_unsigned_packs() {
        assert!(!SyncProfile::EnterpriseStrict.allows_unsigned_packs());
        assert!(SyncProfile::Permissive.allows_unsigned_packs());
    }

    #[test]
    fn as_str_round_trips() {
        for (input, expected) in [
            ("enterprise-strict", "enterprise-strict"),
            ("permissive", "permissive"),
            ("development", "development"),
            ("dev", "development"),
        ] {
            let p = SyncProfile::from_str(input).unwrap();
            assert_eq!(p.as_str(), expected);
        }
    }

    // ── validate_sync_preconditions ──────────────────────────────────────────

    #[test]
    fn no_profile_no_locked_always_passes() {
        let dir = TempDir::new().unwrap();
        assert!(validate_sync_preconditions(None, false, dir.path()).is_ok());
    }

    #[test]
    fn locked_flag_without_lockfile_fails() {
        let dir = TempDir::new().unwrap();
        let err = validate_sync_preconditions(None, true, dir.path()).unwrap_err();
        assert!(err.contains("--locked"), "error was: {err}");
        assert!(err.contains("packs.lock"), "error was: {err}");
    }

    #[test]
    fn locked_flag_with_valid_lockfile_passes() {
        let dir = TempDir::new().unwrap();
        write_lockfile_with_integrity(&dir);
        assert!(validate_sync_preconditions(None, true, dir.path()).is_ok());
    }

    #[test]
    fn locked_flag_with_empty_lockfile_passes() {
        // Empty lockfile (no packs) — nothing to fail integrity check on.
        let dir = TempDir::new().unwrap();
        write_empty_lockfile(&dir);
        assert!(validate_sync_preconditions(None, true, dir.path()).is_ok());
    }

    #[test]
    fn locked_flag_rejects_entry_with_missing_integrity() {
        // Arrange: lockfile with one pack whose integrity field is None.
        let dir = TempDir::new().unwrap();
        write_lockfile_missing_integrity(&dir, "io.ggen.bad.pack");

        // Act
        let err = validate_sync_preconditions(None, true, dir.path()).unwrap_err();

        // Assert: error mentions --locked and the offending pack ID.
        assert!(
            err.contains("--locked"),
            "error should mention --locked, was: {err}"
        );
        assert!(
            err.contains("io.ggen.bad.pack"),
            "error should name the pack missing integrity, was: {err}"
        );
    }

    #[test]
    fn enterprise_strict_without_lockfile_fails() {
        let dir = TempDir::new().unwrap();
        let err =
            validate_sync_preconditions(Some("enterprise-strict"), false, dir.path()).unwrap_err();
        assert!(err.contains("enterprise-strict"), "error was: {err}");
        assert!(err.contains("packs.lock"), "error was: {err}");
    }

    #[test]
    fn enterprise_strict_with_lockfile_passes() {
        let dir = TempDir::new().unwrap();
        write_empty_lockfile(&dir);
        assert!(validate_sync_preconditions(Some("enterprise-strict"), false, dir.path()).is_ok());
    }

    #[test]
    fn permissive_profile_passes_without_lockfile() {
        let dir = TempDir::new().unwrap();
        assert!(validate_sync_preconditions(Some("permissive"), false, dir.path()).is_ok());
    }

    #[test]
    fn development_profile_passes_without_lockfile() {
        let dir = TempDir::new().unwrap();
        assert!(validate_sync_preconditions(Some("dev"), false, dir.path()).is_ok());
    }

    #[test]
    fn unknown_profile_name_returns_error() {
        let dir = TempDir::new().unwrap();
        let err = validate_sync_preconditions(Some("nonexistent"), false, dir.path()).unwrap_err();
        assert!(err.contains("Unknown profile"), "error was: {err}");
    }
}
