//! Pack metadata loading from package.toml or metadata.json
//!
//! This module provides functionality to load pack metadata (signatures, trust tiers, checksums)
//! from cached pack directories. Packs can store metadata in either:
//! - `package.toml` - TOML format with [package] and [security] sections
//! - `metadata.json` - JSON format with signature, trust_tier, checksum fields
//!
//! Both formats are supported with fallback logic for backwards compatibility.

use crate::error::{Error, Result};
use crate::models::PackageId;
use crate::trust::TrustTier;
use serde::Deserialize;
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
}

impl Default for PackMetadata {
    fn default() -> Self {
        Self {
            signature: None,
            trust_tier: TrustTier::Experimental,
            checksum: None,
        }
    }
}

/// Package.toml format
#[derive(Debug, Deserialize)]
struct PackageToml {
    package: PackageSection,
    #[serde(default)]
    security: Option<SecuritySection>,
}

#[derive(Debug, Deserialize)]
struct PackageSection {
    name: String,
    version: String,
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
}

/// Load pack metadata from cache directory
///
/// This function attempts to load metadata from a cached pack directory.
/// It checks for both `package.toml` and `metadata.json` files, with
/// `package.toml` taking precedence if both exist.
///
/// # Arguments
///
/// * `cache_dir` - The pack cache directory (e.g., `~/.cache/ggen/packs/{pack_id}/{version}/`)
///
/// # Errors
///
/// * [`Error::IoError`] - When file system operations fail
/// * [`Error::SerializationError`] - When parsing TOML or JSON fails
///
/// # Examples
///
/// ```ignore
/// use ggen_marketplace::metadata::load_pack_metadata;
///
/// let metadata = load_pack_metadata("/home/user/.cache/ggen/packs/surface-mcp/1.0.0/")?;
/// if let Some(signature) = metadata.signature {
///     println!("Pack signature: {}", signature);
/// }
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
    warn!(
        "No metadata file found in {:?}, using defaults",
        cache_dir
    );
    Ok(PackMetadata::default())
}

/// Load metadata from package.toml
fn load_from_toml(toml_path: &Path) -> Result<PackMetadata> {
    let content = fs::read_to_string(toml_path).map_err(|e| Error::IoError(e))?;

    let package_toml: PackageToml =
        toml::from_str(&content).map_err(Error::TomlError)?;

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

    debug!(
        "Loaded metadata from package.toml: signature={}, trust_tier={:?}, checksum={}",
        signature.is_some(),
        trust_tier,
        checksum.is_some()
    );

    Ok(PackMetadata {
        signature,
        trust_tier,
        checksum,
    })
}

/// Load metadata from metadata.json
fn load_from_json(json_path: &Path) -> Result<PackMetadata> {
    let content = fs::read_to_string(json_path).map_err(|e| Error::IoError(e))?;

    let metadata_json: MetadataJson =
        serde_json::from_str(&content).map_err(|e| Error::SerializationError(e.into()))?;

    let signature = metadata_json.signature;
    let trust_tier = metadata_json
        .trust_tier
        .and_then(|t| parse_trust_tier(&t))
        .unwrap_or(TrustTier::Experimental);
    let checksum = metadata_json.checksum;

    debug!(
        "Loaded metadata from metadata.json: signature={}, trust_tier={:?}, checksum={}",
        signature.is_some(),
        trust_tier,
        checksum.is_some()
    );

    Ok(PackMetadata {
        signature,
        trust_tier,
        checksum,
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

/// Get the cache directory for a specific pack version
///
/// # Arguments
///
/// * `package_id` - The package identifier
/// * `version` - The package version string
///
/// # Returns
///
/// The path to the pack cache directory
#[must_use]
pub fn get_pack_cache_dir(package_id: &PackageId, version: &str) -> PathBuf {
    dirs::cache_dir()
        .unwrap_or_else(|| PathBuf::from(".cache"))
        .join("ggen")
        .join("packs")
        .join(package_id.as_str())
        .join(version)
}

#[cfg(test)]
mod tests {
    use super::*;
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
        assert_eq!(
            parse_trust_tier("Blocked"),
            Some(TrustTier::Blocked)
        );
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

    #[test]
    fn test_get_pack_cache_dir() {
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
