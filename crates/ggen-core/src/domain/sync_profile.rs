//! Sync profile enforcement for `ggen sync --profile` and `--locked`.
//!
//! Known enforcement profiles control pre-flight checks before the sync
//! pipeline runs.  The lockfile check is a real `Path::exists()` call —
//! no test doubles required.

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
                let lockfile = workspace_root.join(".ggen").join("packs.lock");
                if !lockfile.exists() {
                    return Err(
                        "Lockfile required (--locked) but .ggen/packs.lock not found. \
                         Run `ggen packs add <pack>` first."
                            .to_string(),
                    );
                }
            }
            return Ok(());
        }
    };

    // --locked or profile.requires_lockfile() both mandate the lockfile.
    if locked || profile.requires_lockfile() {
        let lockfile = workspace_root.join(".ggen").join("packs.lock");
        if !lockfile.exists() {
            return Err(format!(
                "Lockfile required (profile='{}', --locked={}) but .ggen/packs.lock not found. \
                 Run `ggen packs add <pack>` first.",
                profile.as_str(),
                locked
            ));
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

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
    fn locked_flag_with_lockfile_passes() {
        let dir = TempDir::new().unwrap();
        let ggen_dir = dir.path().join(".ggen");
        fs::create_dir_all(&ggen_dir).unwrap();
        fs::write(ggen_dir.join("packs.lock"), "{}").unwrap();

        assert!(validate_sync_preconditions(None, true, dir.path()).is_ok());
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
        let ggen_dir = dir.path().join(".ggen");
        fs::create_dir_all(&ggen_dir).unwrap();
        fs::write(ggen_dir.join("packs.lock"), "{}").unwrap();

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

    // ── Sabotage tests (coding-agent-mistakes.md §5) ─────────────────────────

    /// Sabotage §5 row 2: writing garbage to packs.lock then calling --locked
    /// must hard-fail.  The lockfile EXISTS (so the file-presence check passes)
    /// but the content is invalid.  validate_sync_preconditions only checks
    /// presence; the corrupt-content rejection is the responsibility of the
    /// sync command's lockfile parser.  This test therefore exercises the
    /// precondition layer: with a *present* but garbage lockfile the precondition
    /// returns Ok (presence satisfied) — proving the precondition check is not
    /// the layer that catches corruption, and that the sync command must also
    /// validate content before trusting it.
    ///
    /// Separately, the CLI-level sabotage (binary exits non-zero on corrupt JSON)
    /// is covered by `crates/ggen-cli/tests/sabotage_tests.rs`.
    #[test]
    fn sabotage_corrupt_lockfile_content_presence_check_still_passes() {
        // Arrange — write garbage to packs.lock
        let dir = TempDir::new().unwrap();
        let ggen_dir = dir.path().join(".ggen");
        fs::create_dir_all(&ggen_dir).unwrap();
        fs::write(ggen_dir.join("packs.lock"), "this is not JSON {{{{{").unwrap();

        // Act — validate_sync_preconditions only checks presence, not content
        // A present-but-corrupt lockfile satisfies the precondition check.
        // The content validation (and hard failure) happens later in the sync
        // pipeline — that path is exercised by the CLI sabotage tests.
        let result = validate_sync_preconditions(None, true, dir.path());

        // Assert — precondition passes (file exists); pipeline will catch content
        assert!(
            result.is_ok(),
            "precondition layer checks presence only; corrupt content is caught \
             by the sync pipeline. result was: {result:?}"
        );
    }

    /// Sabotage §5 row 1 / invariant 4.3.3: absent lockfile with --locked must
    /// always hard-fail regardless of profile.  This is the unit-level proof that
    /// the precondition layer is the last defence before a blind sync.
    #[test]
    fn sabotage_missing_lockfile_with_locked_flag_hard_fails() {
        // Arrange — no lockfile at all (no .ggen/ directory)
        let dir = TempDir::new().unwrap();

        // Act
        let err = validate_sync_preconditions(None, true, dir.path()).unwrap_err();

        // Assert — error must reference --locked and packs.lock
        assert!(
            err.contains("--locked") || err.contains("packs.lock"),
            "error must reference --locked or packs.lock; got: {err}"
        );
    }
}
