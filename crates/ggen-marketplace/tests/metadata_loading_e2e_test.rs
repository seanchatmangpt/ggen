//! End-to-end test for pack metadata loading integration
//!
//! This test verifies that pack metadata (signatures, trust tiers, checksums)
//! is correctly loaded from cache directories and integrated into the
//! RDF mapper's ReleaseInfo construction.

use ggen_marketplace::metadata::{get_pack_cache_dir, load_pack_metadata};
use ggen_marketplace::models::PackageId;
use tempfile::TempDir;

#[test]
fn test_metadata_loading_from_package_toml() {
    let temp_dir = TempDir::new().unwrap();
    let cache_dir = temp_dir.path().join("test-pack").join("1.0.0");
    std::fs::create_dir_all(&cache_dir).unwrap();

    // Create package.toml with security metadata
    let toml_content = r#"
[package]
name = "test-pack"
version = "1.0.0"

[security]
signature = "abc123def456789"
trust_tier = "EnterpriseCertified"
checksum = "sha256:checksum123"
"#;

    std::fs::write(cache_dir.join("package.toml"), toml_content).unwrap();

    // Load metadata
    let metadata = load_pack_metadata(&cache_dir).unwrap();

    assert_eq!(metadata.signature, Some("abc123def456789".to_string()));
    assert_eq!(format!("{:?}", metadata.trust_tier), "EnterpriseCertified");
    assert_eq!(metadata.checksum, Some("sha256:checksum123".to_string()));
}

#[test]
fn test_metadata_loading_from_metadata_json() {
    let temp_dir = TempDir::new().unwrap();
    let cache_dir = temp_dir.path().join("test-pack").join("1.0.0");
    std::fs::create_dir_all(&cache_dir).unwrap();

    // Create metadata.json with security metadata
    let json_content = r#"{
  "signature": "json_signature_789",
  "trust_tier": "EnterpriseApproved",
  "checksum": "sha256:json_checksum"
}"#;

    std::fs::write(cache_dir.join("metadata.json"), json_content).unwrap();

    // Load metadata
    let metadata = load_pack_metadata(&cache_dir).unwrap();

    assert_eq!(metadata.signature, Some("json_signature_789".to_string()));
    assert_eq!(format!("{:?}", metadata.trust_tier), "EnterpriseApproved");
    assert_eq!(metadata.checksum, Some("sha256:json_checksum".to_string()));
}

#[test]
fn test_metadata_loading_defaults_when_no_files() {
    let temp_dir = TempDir::new().unwrap();
    let cache_dir = temp_dir.path().join("test-pack").join("1.0.0");
    std::fs::create_dir_all(&cache_dir).unwrap();

    // Don't create any metadata files
    let metadata = load_pack_metadata(&cache_dir).unwrap();

    // Should have defaults
    assert_eq!(metadata.signature, None);
    assert_eq!(format!("{:?}", metadata.trust_tier), "Experimental");
    assert_eq!(metadata.checksum, None);
}

#[test]
fn test_metadata_loading_prefers_toml_over_json() {
    let temp_dir = TempDir::new().unwrap();
    let cache_dir = temp_dir.path().join("test-pack").join("1.0.0");
    std::fs::create_dir_all(&cache_dir).unwrap();

    // Create both files
    let toml_content = r#"
[package]
name = "test-pack"
version = "1.0.0"

[security]
signature = "toml_sig"
trust_tier = "EnterpriseCertified"
checksum = "toml_checksum"
"#;

    let json_content = r#"{
  "signature": "json_sig",
  "trust_tier": "Experimental",
  "checksum": "json_checksum"
}"#;

    std::fs::write(cache_dir.join("package.toml"), toml_content).unwrap();
    std::fs::write(cache_dir.join("metadata.json"), json_content).unwrap();

    // Should prefer TOML
    let metadata = load_pack_metadata(&cache_dir).unwrap();

    assert_eq!(metadata.signature, Some("toml_sig".to_string()));
    assert_eq!(format!("{:?}", metadata.trust_tier), "EnterpriseCertified");
    assert_eq!(metadata.checksum, Some("toml_checksum".to_string()));
}

#[test]
fn test_get_pack_cache_dir() {
    let package_id = PackageId::new("surface-mcp").unwrap();
    let version = "1.0.0";

    let cache_dir = get_pack_cache_dir(&package_id, version);

    // Should end with the correct path
    let path_str = cache_dir.to_string_lossy();
    assert!(path_str.contains("ggen/packs/surface-mcp/1.0.0"));
}

#[test]
fn test_metadata_loading_with_minimal_toml() {
    let temp_dir = TempDir::new().unwrap();
    let cache_dir = temp_dir.path().join("test-pack").join("1.0.0");
    std::fs::create_dir_all(&cache_dir).unwrap();

    // Create minimal package.toml without security section
    let toml_content = r#"
[package]
name = "minimal-pack"
version = "1.0.0"
"#;

    std::fs::write(cache_dir.join("package.toml"), toml_content).unwrap();

    let metadata = load_pack_metadata(&cache_dir).unwrap();

    // Should have defaults
    assert_eq!(metadata.signature, None);
    assert_eq!(format!("{:?}", metadata.trust_tier), "Experimental");
    assert_eq!(metadata.checksum, None);
}

#[test]
fn test_metadata_loading_with_empty_json() {
    let temp_dir = TempDir::new().unwrap();
    let cache_dir = temp_dir.path().join("test-pack").join("1.0.0");
    std::fs::create_dir_all(&cache_dir).unwrap();

    // Create empty JSON
    std::fs::write(cache_dir.join("metadata.json"), "{}").unwrap();

    let metadata = load_pack_metadata(&cache_dir).unwrap();

    // Should have defaults
    assert_eq!(metadata.signature, None);
    assert_eq!(format!("{:?}", metadata.trust_tier), "Experimental");
    assert_eq!(metadata.checksum, None);
}

#[test]
fn test_metadata_loading_all_trust_tiers() {
    let temp_dir = TempDir::new().unwrap();

    let trust_tiers = vec![
        ("Blocked", "Blocked"),
        ("Experimental", "Experimental"),
        ("Quarantined", "Quarantined"),
        ("CommunityReviewed", "CommunityReviewed"),
        ("ProductionReady", "ProductionReady"),
        ("EnterpriseApproved", "EnterpriseApproved"),
        ("EnterpriseCertified", "EnterpriseCertified"),
    ];

    for (tier_string, expected_tier) in trust_tiers {
        let cache_dir = temp_dir.path().join(format!("test-{}", tier_string));
        std::fs::create_dir_all(&cache_dir).unwrap();

        let json_content = serde_json::json!({
            "trust_tier": tier_string
        });

        std::fs::write(cache_dir.join("metadata.json"), json_content.to_string()).unwrap();

        let metadata = load_pack_metadata(&cache_dir).unwrap();
        assert_eq!(format!("{:?}", metadata.trust_tier), expected_tier);
    }
}
