//! Pack metadata loading from package.toml or metadata.json
//!
//! This module provides functionality to load pack metadata (signatures, trust tiers, checksums)
//! from cached pack directories. Packs can store metadata in either:
//! - `package.toml` - TOML format with \[package\] and \[security\] sections
//! - `metadata.json` - JSON format with signature, `trust_tier`, checksum fields
//!
//! Both formats are supported with fallback logic for backwards compatibility.

use crate::marketplace::error::{Error, Result};
use crate::marketplace::models::PackageId;
use crate::marketplace::trust::{RegistryType, TrustTier};
use serde::Deserialize;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use tracing::{debug, warn};

/// Pack security metadata loaded from package.toml or metadata.json
#[derive(Clone, Debug)]
pub struct PackMetadata {
    /// Ed25519 signature (hex)
    pub signature: Option<String>,
    /// Trust tier classification
    pub trust_tier: TrustTier,
    /// SHA-256 checksum
    pub checksum: Option<String>,
    /// Registry type for external registries
    pub registry_type: Option<RegistryType>,
    /// Origin URL where the artifact was fetched from
    pub origin_url: Option<String>,
    /// Named outputs declared in [pack.outputs] (key → relative path within pack)
    pub outputs: HashMap<String, String>,
}

impl Default for PackMetadata {
    fn default() -> Self {
        Self {
            signature: None,
            trust_tier: TrustTier::Experimental,
            checksum: None,
            registry_type: None,
            origin_url: None,
            outputs: HashMap::new(),
        }
    }
}

/// Package.toml format
#[derive(Debug, Deserialize)]
struct PackageToml {
    package: PackageSection,
    #[serde(default)]
    security: Option<SecuritySection>,
    /// Named outputs: [pack.outputs] table mapping key → relative path
    #[serde(default)]
    pack: Option<PackSection>,
}

/// [pack] section containing [pack.outputs]
#[derive(Debug, Deserialize, Default)]
struct PackSection {
    /// Named outputs: key → relative path within the pack directory
    #[serde(default)]
    outputs: HashMap<String, String>,
}

#[derive(Debug, Deserialize)]
struct PackageSection {
    name: String,
    version: String,
    #[serde(default)]
    registry_type: Option<String>,
    #[serde(default)]
    origin_url: Option<String>,
}

#[derive(Debug, Deserialize)]
struct SecuritySection {
    #[serde(default)]
    signature: Option<String>,
    #[serde(default)]
    trust_tier: Option<String>,
    #[serde(default)]
    checksum: Option<String>,
}

/// Metadata.json format
#[derive(Debug, Deserialize)]
struct MetadataJson {
    #[serde(default)]
    signature: Option<String>,
    #[serde(default)]
    trust_tier: Option<String>,
    #[serde(default)]
    checksum: Option<String>,
    #[serde(default)]
    registry_type: Option<String>,
    #[serde(default)]
    origin_url: Option<String>,
}

/// Load pack metadata from cache directory
///
/// This function attempts to load metadata from a cached pack directory.
/// It checks for both `package.toml` and `metadata.json` files, with
/// `package.toml` taking precedence if both exist.
///
/// # Arguments
///
/// * `cache_dir` - The pack cache directory, e.g. the path returned by
///   [`pack_cache_dir`] (env-overridable via `GGEN_PACK_CACHE_DIR`, else the
///   platform cache directory joined with `ggen/packs/{pack_id}/{version}/`
///   -- `~/Library/Caches/ggen/packs/...` on macOS, `~/.cache/ggen/packs/...`
///   on Linux)
///
/// # Errors
///
/// * [`Error::IoError`] - When file system operations fail
/// * [`Error::SerializationError`] - When parsing TOML or JSON fails
///
/// # Examples
///
/// ```
/// use ggen_marketplace::marketplace::metadata::load_pack_metadata;
///
/// // Real pack cache directory containing a package.toml with a signature.
/// let cache_dir = tempfile::tempdir().unwrap();
/// std::fs::write(
///     cache_dir.path().join("package.toml"),
///     r#"
/// [package]
/// name = "surface-mcp"
/// version = "1.0.0"
///
/// [security]
/// signature = "deadbeef"
/// trust_tier = "productionready"
/// "#,
/// )
/// .unwrap();
///
/// let metadata = load_pack_metadata(cache_dir.path()).unwrap();
/// assert_eq!(metadata.signature.as_deref(), Some("deadbeef"));
/// ```
pub fn load_pack_metadata(cache_dir: &Path) -> Result<PackMetadata> {
    debug!("Loading pack metadata from: {:?}", cache_dir);

    // Try package.toml first (preferred format)
    let toml_path = cache_dir.join("package.toml");
    if toml_path.exists() {
        debug!("Found package.toml, loading metadata");
        return load_from_toml(&toml_path);
    }

    // Fallback to metadata.json
    let json_path = cache_dir.join("metadata.json");
    if json_path.exists() {
        debug!("Found metadata.json, loading metadata");
        return load_from_json(&json_path);
    }

    // No metadata file found - return defaults
    warn!("No metadata file found in {:?}, using defaults", cache_dir);
    Ok(PackMetadata::default())
}

/// Load metadata from package.toml
fn load_from_toml(toml_path: &Path) -> Result<PackMetadata> {
    let content = fs::read_to_string(toml_path).map_err(Error::IoError)?;

    let package_toml: PackageToml = toml::from_str(&content).map_err(Error::TomlError)?;

    let signature = package_toml
        .security
        .as_ref()
        .and_then(|s| s.signature.clone());

    let trust_tier = package_toml
        .security
        .as_ref()
        .and_then(|s| s.trust_tier.as_ref())
        .and_then(|t| parse_trust_tier(t))
        .unwrap_or(TrustTier::Experimental);

    let checksum = package_toml
        .security
        .as_ref()
        .and_then(|s| s.checksum.clone());

    let registry_type =
        package_toml
            .package
            .registry_type
            .as_ref()
            .map(|s| match s.to_lowercase().as_str() {
                "crates.io" | "cratesio" => RegistryType::CratesIo,
                "npm" => RegistryType::Npm,
                "pypi" => RegistryType::PyPi,
                "github" => RegistryType::GitHub,
                _ => RegistryType::Ggen,
            });

    let origin_url = package_toml.package.origin_url.clone();

    let outputs = package_toml.pack.map(|p| p.outputs).unwrap_or_default();

    debug!(
        "Loaded metadata from package.toml: signature={}, trust_tier={:?}, checksum={}, registry={:?}, outputs={}",
        signature.is_some(),
        trust_tier,
        checksum.is_some(),
        registry_type,
        outputs.len()
    );

    Ok(PackMetadata {
        signature,
        trust_tier,
        checksum,
        registry_type,
        origin_url,
        outputs,
    })
}

/// Load metadata from metadata.json
fn load_from_json(json_path: &Path) -> Result<PackMetadata> {
    let content = fs::read_to_string(json_path).map_err(Error::IoError)?;

    let metadata_json: MetadataJson =
        serde_json::from_str(&content).map_err(Error::SerializationError)?;

    let signature = metadata_json.signature;
    let trust_tier = metadata_json
        .trust_tier
        .and_then(|t| parse_trust_tier(&t))
        .unwrap_or(TrustTier::Experimental);
    let checksum = metadata_json.checksum;

    let registry_type =
        metadata_json
            .registry_type
            .as_ref()
            .map(|s| match s.to_lowercase().as_str() {
                "crates.io" | "cratesio" => RegistryType::CratesIo,
                "npm" => RegistryType::Npm,
                "pypi" => RegistryType::PyPi,
                "github" => RegistryType::GitHub,
                _ => RegistryType::Ggen,
            });

    let origin_url = metadata_json.origin_url;

    debug!(
        "Loaded metadata from metadata.json: signature={}, trust_tier={:?}, checksum={}, registry={:?}",
        signature.is_some(),
        trust_tier,
        checksum.is_some(),
        registry_type
    );

    Ok(PackMetadata {
        signature,
        trust_tier,
        checksum,
        registry_type,
        origin_url,
        outputs: HashMap::new(),
    })
}

/// Parse trust tier from string
pub fn parse_trust_tier(s: &str) -> Option<TrustTier> {
    match s.to_lowercase().as_str() {
        "blocked" => Some(TrustTier::Blocked),
        "experimental" => Some(TrustTier::Experimental),
        "quarantined" => Some(TrustTier::Quarantined),
        "enterpriseapproved" => Some(TrustTier::EnterpriseApproved),
        "enterprisecertified" => Some(TrustTier::EnterpriseCertified),
        // Legacy string labels (same names as current variants, case-insensitive)
        "communityreviewed" => Some(TrustTier::CommunityReviewed),
        "productionready" => Some(TrustTier::ProductionReady),
        _ => {
            warn!("Unknown trust tier: {}, defaulting to Experimental", s);
            Some(TrustTier::Experimental)
        }
    }
}

/// Canonical root directory for the transient pack download/extract cache.
///
/// Resolution order (first match wins):
/// 1. `GGEN_PACK_CACHE_DIR` environment variable -- an unconditional override
///    (no existence check), matching the one resolver of the previously
///    three independent implementations that already honored this var
///    ([`crate::marketplace::install::Installer::persistent_cache_path`]).
/// 2. The platform cache directory (`dirs::cache_dir()`) joined with
///    `ggen/packs` -- e.g. `~/Library/Caches/ggen/packs` on macOS,
///    `~/.cache/ggen/packs` on Linux. Falls back to a bare `.cache` root if
///    the platform cache directory cannot be determined, matching this
///    module's and [`crate::marketplace::cache::CacheConfig::default`]'s
///    pre-existing fallback convention.
///
/// This is the single canonical resolver for the transient pack cache root.
/// [`get_pack_cache_dir`], `Installer::persistent_cache_path`, and
/// `CacheConfig::default` all delegate to this function so those three
/// previously-independent resolvers (which used to disagree on both the
/// default directory and whether the env override was honored) cannot drift
/// apart again.
///
/// Not to be confused with `~/.ggen/packs` (the separate installed-pack
/// catalog root used by `install_pack_by_id`/`get_packs_dir` in
/// `packs_registry`/`ggen-cli`) -- that is an architecturally distinct
/// concern (the durable installed-pack catalog vs. this transient
/// download/extract cache) and is untouched by this function.
#[must_use]
pub fn pack_cache_root() -> PathBuf {
    std::env::var("GGEN_PACK_CACHE_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| {
            dirs::cache_dir()
                .unwrap_or_else(|| PathBuf::from(".cache"))
                .join("ggen")
                .join("packs")
        })
}

/// Canonical per-pack-version cache directory: [`pack_cache_root`] joined
/// with `<id>/<version>`.
#[must_use]
pub fn pack_cache_dir(id: &str, version: &str) -> PathBuf {
    pack_cache_root().join(id).join(version)
}

/// Get the cache directory for a specific pack version
///
/// # Arguments
///
/// * `package_id` - The package identifier
/// * `version` - The package version string
///
/// # Returns
///
/// The path to the pack cache directory -- see [`pack_cache_root`] for the
/// resolution order (env-overridable via `GGEN_PACK_CACHE_DIR`, else the
/// platform cache directory). Thin wrapper around [`pack_cache_dir`], kept as
/// a separate function so existing callers (which pass a [`PackageId`], not
/// a bare `&str`) don't need to change.
#[must_use]
pub fn get_pack_cache_dir(package_id: &PackageId, version: &str) -> PathBuf {
    pack_cache_dir(package_id.as_str(), version)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serial_test::serial;
    use tempfile::TempDir;

    #[test]
    fn test_load_from_package_toml() {
        let temp_dir = TempDir::new().unwrap();
        let toml_path = temp_dir.path().join("package.toml");

        let toml_content = r#"
[package]
name = "test-package"
version = "1.0.0"

[security]
signature = "abc123def456"
trust_tier = "EnterpriseCertified"
checksum = "sha256:789xyz"
"#;

        fs::write(&toml_path, toml_content).unwrap();

        let metadata = load_from_toml(&toml_path).unwrap();

        assert_eq!(metadata.signature, Some("abc123def456".to_string()));
        assert_eq!(metadata.trust_tier, TrustTier::EnterpriseCertified);
        assert_eq!(metadata.checksum, Some("sha256:789xyz".to_string()));
    }

    #[test]
    fn test_load_from_metadata_json() {
        let temp_dir = TempDir::new().unwrap();
        let json_path = temp_dir.path().join("metadata.json");

        let json_content = r#"{
  "signature": "json123",
  "trust_tier": "EnterpriseApproved",
  "checksum": "checksum456"
}"#;

        fs::write(&json_path, json_content).unwrap();

        let metadata = load_from_json(&json_path).unwrap();

        assert_eq!(metadata.signature, Some("json123".to_string()));
        assert_eq!(metadata.trust_tier, TrustTier::EnterpriseApproved);
        assert_eq!(metadata.checksum, Some("checksum456".to_string()));
    }

    #[test]
    fn test_load_pack_metadata_prefers_toml() {
        let temp_dir = TempDir::new().unwrap();

        // Create both files
        let toml_path = temp_dir.path().join("package.toml");
        let json_path = temp_dir.path().join("metadata.json");

        fs::write(
            &toml_path,
            r#"
[package]
name = "test"
version = "1.0.0"

[security]
signature = "toml_signature"
trust_tier = "EnterpriseCertified"
"#,
        )
        .unwrap();

        fs::write(
            &json_path,
            r#"{"signature": "json_signature", "trust_tier": "Experimental"}"#,
        )
        .unwrap();

        let metadata = load_pack_metadata(temp_dir.path()).unwrap();

        // Should prefer TOML
        assert_eq!(metadata.signature, Some("toml_signature".to_string()));
        assert_eq!(metadata.trust_tier, TrustTier::EnterpriseCertified);
    }

    #[test]
    fn test_load_pack_metadata_defaults_when_missing() {
        let temp_dir = TempDir::new().unwrap();
        // Don't create any metadata files

        let metadata = load_pack_metadata(temp_dir.path()).unwrap();

        assert_eq!(metadata.signature, None);
        assert_eq!(metadata.trust_tier, TrustTier::Experimental); // Default
        assert_eq!(metadata.checksum, None);
    }

    #[test]
    fn test_parse_trust_tier() {
        assert_eq!(parse_trust_tier("Blocked"), Some(TrustTier::Blocked));
        assert_eq!(
            parse_trust_tier("Experimental"),
            Some(TrustTier::Experimental)
        );
        assert_eq!(
            parse_trust_tier("CommunityReviewed"),
            Some(TrustTier::CommunityReviewed)
        );
        assert_eq!(
            parse_trust_tier("EnterpriseApproved"),
            Some(TrustTier::EnterpriseApproved)
        );
        assert_eq!(
            parse_trust_tier("EnterpriseCertified"),
            Some(TrustTier::EnterpriseCertified)
        );
        // Unknown tiers default to Experimental with a warning
        assert_eq!(
            parse_trust_tier("UnknownTier"),
            Some(TrustTier::Experimental)
        );
    }

    /// `#[serial]`: `get_pack_cache_dir` now delegates through
    /// `pack_cache_root`, which reads `GGEN_PACK_CACHE_DIR` -- without
    /// serialization this races against other tests (e.g. the E2 drift-guard
    /// tests in `marketplace::install::tests`) that set/unset that same
    /// process-wide env var.
    #[test]
    #[serial]
    fn test_get_pack_cache_dir() {
        std::env::remove_var("GGEN_PACK_CACHE_DIR");
        let package_id = PackageId::new("test-package").unwrap();
        let version = "1.0.0";

        let cache_dir = get_pack_cache_dir(&package_id, version);

        assert!(cache_dir.ends_with("ggen/packs/test-package/1.0.0"));
    }

    #[test]
    fn test_load_from_toml_minimal() {
        let temp_dir = TempDir::new().unwrap();
        let toml_path = temp_dir.path().join("package.toml");

        // Minimal TOML without security section
        let toml_content = r#"
[package]
name = "minimal-package"
version = "1.0.0"
"#;

        fs::write(&toml_path, toml_content).unwrap();

        let metadata = load_from_toml(&toml_path).unwrap();

        assert_eq!(metadata.signature, None);
        assert_eq!(metadata.trust_tier, TrustTier::Experimental); // Default
        assert_eq!(metadata.checksum, None);
    }

    #[test]
    fn test_load_from_json_minimal() {
        let temp_dir = TempDir::new().unwrap();
        let json_path = temp_dir.path().join("metadata.json");

        // Empty JSON
        fs::write(&json_path, "{}").unwrap();

        let metadata = load_from_json(&json_path).unwrap();

        assert_eq!(metadata.signature, None);
        assert_eq!(metadata.trust_tier, TrustTier::Experimental); // Default
        assert_eq!(metadata.checksum, None);
    }
}
