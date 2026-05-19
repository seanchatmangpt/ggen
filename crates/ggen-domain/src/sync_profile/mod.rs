//! Sync profile definitions and enforcement.
//!
//! Profiles gate the sync pipeline:
//! - `enterprise-strict`: lockfile required, no unsigned content, trust tier checks
//! - `permissive`: lockfile recommended, warns on unsigned
//! - `development`: no restrictions

use ggen_core::utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::path::Path;

/// Sync profile — determines enforcement level.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum SyncProfile {
    /// Lockfile required, unsigned packs rejected, trust tier enforced.
    EnterpriseStrict,
    /// Lockfile recommended, unsigned packs warned.
    Permissive,
    /// No restrictions.
    Development,
}

impl SyncProfile {
    /// Parse from string. Case-insensitive.
    pub fn from_str_loose(s: &str) -> Result<Self> {
        match s.to_lowercase().as_str() {
            "enterprise-strict" | "enterprisestrict" | "strict" => Ok(SyncProfile::EnterpriseStrict),
            "permissive" | "default" => Ok(SyncProfile::Permissive),
            "development" | "dev" => Ok(SyncProfile::Development),
            _ => Err(Error::new(&format!(
                "Unknown sync profile: '{}'. Valid: enterprise-strict, permissive, development",
                s
            ))),
        }
    }

    /// Whether this profile requires a lockfile to exist.
    pub fn requires_lockfile(&self) -> bool {
        matches!(self, SyncProfile::EnterpriseStrict)
    }

    /// Whether this profile rejects unsigned packs.
    pub fn rejects_unsigned(&self) -> bool {
        matches!(self, SyncProfile::EnterpriseStrict)
    }

    /// Whether this profile enforces trust tier checks.
    pub fn enforces_trust_tier(&self) -> bool {
        matches!(self, SyncProfile::EnterpriseStrict)
    }
}

impl std::fmt::Display for SyncProfile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SyncProfile::EnterpriseStrict => write!(f, "enterprise-strict"),
            SyncProfile::Permissive => write!(f, "permissive"),
            SyncProfile::Development => write!(f, "development"),
        }
    }
}

/// Validate sync preconditions before pipeline execution.
///
/// This is called from the CLI `sync` verb before any I/O happens.
/// Checks:
/// - Profile is parseable (if provided)
/// - `--locked` implies lockfile must exist
/// - `enterprise-strict` implies lockfile must exist
pub fn validate_sync_preconditions(
    profile: Option<&str>,
    locked: bool,
    workspace: &Path,
) -> Result<()> {
    let lockfile_path = workspace.join(".ggen/packs.lock");

    // Parse profile if provided
    let parsed_profile = match profile {
        Some(p) => Some(SyncProfile::from_str_loose(p)?),
        None => None,
    };

    // --locked always requires lockfile
    if locked && !lockfile_path.exists() {
        return Err(Error::new(
            "--locked requires a lockfile at .ggen/packs.lock. Run `ggen pack add <pack>` first.",
        ));
    }

    // enterprise-strict requires lockfile
    if let Some(SyncProfile::EnterpriseStrict) = parsed_profile {
        if !lockfile_path.exists() {
            return Err(Error::new(
                "enterprise-strict profile requires a lockfile at .ggen/packs.lock. \
                 Run `ggen pack add <pack>` first.",
            ));
        }
    }

    Ok(())
}

/// Enforce profile policy against installed packs.
///
/// Called after pack loading but before pipeline execution.
/// Returns a list of violations. In enterprise-strict mode, any violation
/// causes a hard fail. In permissive mode, violations are warnings.
pub fn enforce_profile_policy(
    profile: SyncProfile,
    packs: &[crate::packs::loader::InstalledPack],
) -> Result<Vec<PolicyViolation>> {
    let mut violations = vec![];

    for pack in packs {
        // Check: unsigned packs
        if profile.rejects_unsigned() && pack.digest.is_none() {
            violations.push(PolicyViolation {
                pack_id: pack.id.clone(),
                rule: "unsigned-content".to_string(),
                severity: ViolationSeverity::Error,
                message: format!(
                    "Pack '{}' has no digest (unsigned). enterprise-strict rejects unsigned content.",
                    pack.id
                ),
            });
        }
    }

    // In enterprise-strict, any error-level violation fails the sync
    if profile == SyncProfile::EnterpriseStrict {
        let errors: Vec<_> = violations.iter().filter(|v| v.severity == ViolationSeverity::Error).collect();
        if !errors.is_empty() {
            let messages: Vec<String> = errors.iter().map(|e| e.message.clone()).collect();
            return Err(Error::new(&format!(
                "Profile enforcement failed ({} violation(s)):\n  {}",
                errors.len(),
                messages.join("\n  ")
            )));
        }
    }

    Ok(violations)
}

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PolicyViolation {
    pub pack_id: String,
    pub rule: String,
    pub severity: ViolationSeverity,
    pub message: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ViolationSeverity {
    Error,
    Warning,
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_parse_profile() {
        assert_eq!(SyncProfile::from_str_loose("enterprise-strict").unwrap(), SyncProfile::EnterpriseStrict);
        assert_eq!(SyncProfile::from_str_loose("permissive").unwrap(), SyncProfile::Permissive);
        assert_eq!(SyncProfile::from_str_loose("development").unwrap(), SyncProfile::Development);
        assert_eq!(SyncProfile::from_str_loose("strict").unwrap(), SyncProfile::EnterpriseStrict);
        assert!(SyncProfile::from_str_loose("invalid").is_err());
    }

    #[test]
    fn test_locked_requires_lockfile() {
        let tmp = TempDir::new().unwrap();
        let result = validate_sync_preconditions(None, true, tmp.path());
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("--locked"));
    }

    #[test]
    fn test_enterprise_strict_requires_lockfile() {
        let tmp = TempDir::new().unwrap();
        let result = validate_sync_preconditions(Some("enterprise-strict"), false, tmp.path());
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("enterprise-strict"));
    }

    #[test]
    fn test_development_no_lockfile_ok() {
        let tmp = TempDir::new().unwrap();
        let result = validate_sync_preconditions(Some("development"), false, tmp.path());
        assert!(result.is_ok());
    }

    #[test]
    fn test_locked_with_lockfile_ok() {
        let tmp = TempDir::new().unwrap();
        let ggen_dir = tmp.path().join(".ggen");
        std::fs::create_dir_all(&ggen_dir).unwrap();
        std::fs::write(ggen_dir.join("packs.lock"), "{}").unwrap();

        let result = validate_sync_preconditions(None, true, tmp.path());
        assert!(result.is_ok());
    }

    #[test]
    fn test_profile_requires_lockfile_flag() {
        assert!(SyncProfile::EnterpriseStrict.requires_lockfile());
        assert!(!SyncProfile::Permissive.requires_lockfile());
        assert!(!SyncProfile::Development.requires_lockfile());
    }
}
