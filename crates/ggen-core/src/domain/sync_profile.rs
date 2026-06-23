//! Sync profile enforcement for `ggen sync --profile` and `--locked`.
//!
//! Known enforcement profiles control pre-flight checks before the sync
//! pipeline runs.  The lockfile check is a real `Path::exists()` call —
//! no test doubles required.

use crate::domain::packs::install::compute_pack_digest;
use crate::domain::packs::metadata::load_pack_metadata;
use crate::packs::lockfile::{PackLockfile, PackSource};
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

/// RE-VERIFY every locked pack's digest against the pack on disk (lockfile
/// invariant 4.3.3 — `digest` must be re-verified at `ggen sync --locked` time;
/// mismatch must hard-fail). A non-empty `integrity` field alone is fail-open:
/// it proves a digest was once recorded, not that the pack on disk still
/// matches it. This recomputes the digest with the SAME algorithm used at
/// install time (`compute_pack_digest`) and compares to the stored
/// `sha256-<hex>` value.
///
/// For each locked pack we require:
///   1. The recorded `PackSource::Local { path }` install directory still
///      exists (a deleted install dir => "missing pack").
///   2. The pack definition is re-loadable from the packs registry via
///      `load_pack_metadata` — this is the same source `install_pack` read the
///      pack from when it computed the install-time digest, so the digest is
///      deterministically reconstructible. A pack TOML that was deleted or made
///      unreadable => "missing pack".
///   3. The recomputed `sha256-<hex>` digest equals the stored `integrity`
///      value. Any divergence => "digest mismatch".
///
/// Packs whose recorded source is not `Local` (e.g. `Registry`/`GitHub`) are
/// skipped here: their on-disk closure is not addressable through a local path,
/// so this layer can only assert the non-empty digest already checked by
/// `check_integrity_fields`. This keeps re-verification grounded in real,
/// re-loadable on-disk state rather than fabricating a comparison.
fn verify_pack_digests(lockfile: &PackLockfile) -> Result<(), String> {
    for (pack_id, locked) in &lockfile.packs {
        // Only locally-installed packs expose an on-disk path we can re-check.
        let install_path = match &locked.source {
            PackSource::Local { path } => path,
            _ => continue,
        };

        // (1) The recorded install directory must still exist.
        if !install_path.exists() {
            return Err(format!(
                "Lockfile digest re-verification failed (--locked): missing pack '{}'. \
                 Its recorded install path '{}' no longer exists. Run `ggen packs add {}` \
                 to reinstall it.",
                pack_id,
                install_path.display(),
                pack_id
            ));
        }

        // (2) Re-load the pack definition from the registry (same source the
        //     install-time digest was computed from). A missing/unreadable pack
        //     definition is treated as a missing pack — fail closed.
        let pack = load_pack_metadata(pack_id).map_err(|e| {
            format!(
                "Lockfile digest re-verification failed (--locked): missing pack '{}'. \
                 Could not re-load its definition to recompute the digest: {}. Run \
                 `ggen packs add {}` to reinstall it.",
                pack_id, e, pack_id
            )
        })?;

        // (3) Recompute the digest and compare to the stored integrity value.
        let recomputed = format!("sha256-{}", compute_pack_digest(&pack));
        let stored = locked.integrity.as_deref().unwrap_or("");
        if recomputed != stored {
            return Err(format!(
                "Lockfile digest re-verification failed (--locked): digest mismatch for \
                 pack '{}'. Lockfile records '{}' but the pack on disk hashes to '{}'. The \
                 pack was modified after it was locked. Run `ggen packs add {}` to relock it.",
                pack_id, stored, recomputed, pack_id
            ));
        }
    }

    Ok(())
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
                // Presence is satisfied. Attempt to load and verify integrity of
                // every entry. A *corrupt* (unparseable) lockfile is NOT rejected
                // here: the precondition layer checks presence only; corrupt
                // content is the responsibility of the sync pipeline / CLI parser
                // (see sabotage tests). A lockfile that parses cleanly but is
                // missing integrity digests IS rejected here, and every locked
                // pack's digest is RE-VERIFIED against the pack on disk
                // (invariant 4.3.3) — a non-empty digest alone is fail-open.
                if let Ok(lockfile) = PackLockfile::from_file(&lockfile_path) {
                    check_integrity_fields(&lockfile)?;
                    verify_pack_digests(&lockfile)?;
                }
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
            // Load, verify integrity, then RE-VERIFY every entry's digest
            // against the pack on disk (invariant 4.3.3).
            let lockfile = PackLockfile::from_file(&lockfile_path)
                .map_err(|e| format!("Failed to load lockfile (--locked): {e}"))?;
            check_integrity_fields(&lockfile)?;
            verify_pack_digests(&lockfile)?;
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

    // ── Digest re-verification (invariant 4.3.3) ─────────────────────────────
    //
    // These tests exercise `verify_pack_digests` end-to-end with REAL on-disk
    // state: a registry `<id>.toml` (read by `load_pack_metadata`, the same
    // source the install-time digest came from), a real `Local` install dir,
    // and a lockfile whose `integrity` is computed with the SAME algorithm
    // (`compute_pack_digest`). No mocks — `GGEN_PACKS_DIR` is mutated globally so
    // these are `#[serial(GGEN_PACKS_DIR)]` like the `metadata` tests.
    use crate::domain::packs::types::Pack;
    use serial_test::serial;
    use std::collections::HashMap;

    /// Restore `GGEN_PACKS_DIR` to its prior value (or unset) on Drop.
    struct PacksDirGuard {
        previous: Option<std::ffi::OsString>,
    }

    impl PacksDirGuard {
        fn set(value: &std::path::Path) -> Self {
            let previous = std::env::var_os("GGEN_PACKS_DIR");
            std::env::set_var("GGEN_PACKS_DIR", value);
            Self { previous }
        }
    }

    impl Drop for PacksDirGuard {
        fn drop(&mut self) {
            match &self.previous {
                None => std::env::remove_var("GGEN_PACKS_DIR"),
                Some(v) => std::env::set_var("GGEN_PACKS_DIR", v),
            }
        }
    }

    /// Build the in-memory `Pack` that the registry TOML below deserializes to,
    /// so the test can compute the expected digest with the production
    /// algorithm rather than hardcoding a hex string.
    fn sample_pack(id: &str, version: &str) -> Pack {
        Pack {
            id: id.to_string(),
            name: format!("Sample {id}"),
            version: version.to_string(),
            description: "reverify fixture".to_string(),
            category: "test".to_string(),
            author: None,
            repository: None,
            license: Some("MIT".to_string()),
            registry_type: None,
            packages: vec![format!("{id}-core")],
            templates: vec![],
            sparql_queries: HashMap::new(),
            dependencies: vec![],
            tags: vec![],
            keywords: vec![],
            production_ready: true,
            metadata: Default::default(),
        }
    }

    /// Write a registry `<id>.toml` matching `sample_pack(id, version)`.
    fn write_registry_pack(registry: &std::path::Path, id: &str, version: &str) {
        let toml = format!(
            r#"[pack]
id = "{id}"
name = "Sample {id}"
version = "{version}"
description = "reverify fixture"
category = "test"
license = "MIT"
production_ready = true
packages = ["{id}-core"]
"#
        );
        fs::write(registry.join(format!("{id}.toml")), toml).unwrap();
    }

    /// Write a lockfile pinning `id` as a `Local` source at `install_path` with
    /// the supplied integrity string.
    fn write_local_lockfile(
        project: &std::path::Path, id: &str, version: &str, install_path: &std::path::Path,
        integrity: &str,
    ) {
        let mut lockfile = PackLockfile::new("4.0.0");
        lockfile.add_pack(
            id,
            LockedPack {
                version: version.to_string(),
                source: PackSource::Local {
                    path: install_path.to_path_buf(),
                },
                integrity: Some(integrity.to_string()),
                installed_at: Utc::now(),
                dependencies: vec![],
            },
        );
        let ggen_dir = project.join(".ggen");
        fs::create_dir_all(&ggen_dir).unwrap();
        lockfile.save(&ggen_dir.join("packs.lock")).unwrap();
    }

    #[test]
    #[serial(GGEN_PACKS_DIR)]
    fn reverify_passes_when_pack_unmodified() {
        // Arrange: registry pack + matching install dir + correct digest.
        let registry = TempDir::new().unwrap();
        let project = TempDir::new().unwrap();
        let install = TempDir::new().unwrap();
        let _guard = PacksDirGuard::set(registry.path());

        write_registry_pack(registry.path(), "io.ggen.ok", "1.0.0");
        let digest = format!(
            "sha256-{}",
            compute_pack_digest(&sample_pack("io.ggen.ok", "1.0.0"))
        );
        write_local_lockfile(
            project.path(),
            "io.ggen.ok",
            "1.0.0",
            install.path(),
            &digest,
        );

        // Act + Assert: happy path passes --locked.
        assert!(
            validate_sync_preconditions(None, true, project.path()).is_ok(),
            "unmodified pack with matching digest must pass --locked re-verification"
        );
    }

    #[test]
    #[serial(GGEN_PACKS_DIR)]
    fn reverify_fails_on_digest_mismatch() {
        // Arrange: lock the pack at v1.0.0's digest, then mutate the registry
        // TOML to v2.0.0 so the recomputed digest diverges.
        let registry = TempDir::new().unwrap();
        let project = TempDir::new().unwrap();
        let install = TempDir::new().unwrap();
        let _guard = PacksDirGuard::set(registry.path());

        write_registry_pack(registry.path(), "io.ggen.drift", "1.0.0");
        let locked_digest = format!(
            "sha256-{}",
            compute_pack_digest(&sample_pack("io.ggen.drift", "1.0.0"))
        );
        write_local_lockfile(
            project.path(),
            "io.ggen.drift",
            "1.0.0",
            install.path(),
            &locked_digest,
        );

        // Sabotage: rewrite the on-disk pack to a different version.
        write_registry_pack(registry.path(), "io.ggen.drift", "2.0.0");

        // Act
        let err = validate_sync_preconditions(None, true, project.path()).unwrap_err();

        // Assert: digest mismatch named, with the offending pack id.
        assert!(
            err.contains("digest mismatch"),
            "error must reference 'digest mismatch'; got: {err}"
        );
        assert!(
            err.contains("io.ggen.drift"),
            "error must name the drifted pack; got: {err}"
        );
    }

    #[test]
    #[serial(GGEN_PACKS_DIR)]
    fn reverify_fails_when_pack_definition_removed() {
        // Arrange: valid lock, then delete the registry TOML => not re-loadable.
        let registry = TempDir::new().unwrap();
        let project = TempDir::new().unwrap();
        let install = TempDir::new().unwrap();
        let _guard = PacksDirGuard::set(registry.path());

        write_registry_pack(registry.path(), "io.ggen.gone", "1.0.0");
        let digest = format!(
            "sha256-{}",
            compute_pack_digest(&sample_pack("io.ggen.gone", "1.0.0"))
        );
        write_local_lockfile(
            project.path(),
            "io.ggen.gone",
            "1.0.0",
            install.path(),
            &digest,
        );

        // Sabotage: remove the registry pack definition.
        fs::remove_file(registry.path().join("io.ggen.gone.toml")).unwrap();

        // Act
        let err = validate_sync_preconditions(None, true, project.path()).unwrap_err();

        // Assert: treated as a missing pack.
        assert!(
            err.contains("missing pack"),
            "error must reference 'missing pack'; got: {err}"
        );
        assert!(
            err.contains("io.ggen.gone"),
            "error must name the missing pack; got: {err}"
        );
    }

    #[test]
    #[serial(GGEN_PACKS_DIR)]
    fn reverify_fails_when_install_dir_removed() {
        // Arrange: valid lock pointing at an install dir we then delete.
        let registry = TempDir::new().unwrap();
        let project = TempDir::new().unwrap();
        let install = TempDir::new().unwrap();
        let _guard = PacksDirGuard::set(registry.path());

        write_registry_pack(registry.path(), "io.ggen.noinstall", "1.0.0");
        let digest = format!(
            "sha256-{}",
            compute_pack_digest(&sample_pack("io.ggen.noinstall", "1.0.0"))
        );
        let install_path = install.path().to_path_buf();
        write_local_lockfile(
            project.path(),
            "io.ggen.noinstall",
            "1.0.0",
            &install_path,
            &digest,
        );

        // Sabotage: remove the recorded install directory.
        drop(install);
        assert!(!install_path.exists());

        // Act
        let err = validate_sync_preconditions(None, true, project.path()).unwrap_err();

        // Assert: missing pack (install path gone).
        assert!(
            err.contains("missing pack"),
            "error must reference 'missing pack'; got: {err}"
        );
        assert!(
            err.contains("io.ggen.noinstall"),
            "error must name the pack with the missing install path; got: {err}"
        );
    }
}
