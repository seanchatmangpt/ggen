//! Unit tests for error handling paths

use anyhow::{Context, Result};
use ggen_core::registry::{RegistryClient, PackMetadata, VersionMetadata};
use std::collections::HashMap;
use tempfile::TempDir;
use url::Url;

#[tokio::test]
async fn test_missing_pack_error() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("index.json");

    let mock_index = r#"{
        "updated": "2024-01-01T00:00:00Z",
        "packs": {}
    }"#;

    std::fs::write(&index_path, mock_index)?;

    let base_url = Url::from_file_path(temp_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))?;
    let client = RegistryClient::with_base_url(base_url)?;

    // Should fail with context about missing pack
    let result = client.resolve("nonexistent-pack", None).await;
    assert!(result.is_err());

    let error_msg = format!("{:?}", result.err().unwrap());
    assert!(error_msg.contains("not found"));

    Ok(())
}

#[tokio::test]
async fn test_missing_version_error() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("index.json");

    let mock_index = r#"{
        "updated": "2024-01-01T00:00:00Z",
        "packs": {
            "test-pack": {
                "id": "test-pack",
                "name": "Test Pack",
                "description": "Test",
                "tags": [],
                "keywords": [],
                "category": null,
                "author": null,
                "latest_version": "1.0.0",
                "versions": {
                    "1.0.0": {
                        "version": "1.0.0",
                        "git_url": "https://github.com/test/repo.git",
                        "git_rev": "main",
                        "manifest_url": null,
                        "sha256": "abc123"
                    }
                },
                "downloads": null,
                "updated": null,
                "license": null,
                "homepage": null,
                "repository": null,
                "documentation": null
            }
        }
    }"#;

    std::fs::write(&index_path, mock_index)?;

    let base_url = Url::from_file_path(temp_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))?;
    let client = RegistryClient::with_base_url(base_url)?;

    // Should fail with context about missing version
    let result = client.resolve("test-pack", Some("2.0.0")).await;
    assert!(result.is_err());

    let error_msg = format!("{:?}", result.err().unwrap());
    assert!(error_msg.contains("Version"));
    assert!(error_msg.contains("not found"));

    Ok(())
}

#[test]
fn test_invalid_semver_error() {
    let result = semver::Version::parse("not-a-version");
    assert!(result.is_err());
}

#[test]
fn test_json_parsing_error() {
    let invalid_json = "{ invalid json }";
    let result: Result<PackMetadata, _> = serde_json::from_str(invalid_json);
    assert!(result.is_err());
}

#[test]
fn test_missing_required_fields_error() {
    // Missing required fields should fail deserialization
    let incomplete_json = r#"{
        "id": "test-pack"
    }"#;

    let result: Result<PackMetadata, _> = serde_json::from_str(incomplete_json);
    assert!(result.is_err());
}

#[tokio::test]
async fn test_malformed_index_error() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("index.json");

    // Write malformed JSON
    std::fs::write(&index_path, "{ malformed json }")?;

    let base_url = Url::from_file_path(temp_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))?;
    let client = RegistryClient::with_base_url(base_url)?;

    // Should fail to parse
    let result = client.fetch_index().await;
    assert!(result.is_err());

    Ok(())
}

#[tokio::test]
async fn test_missing_index_file_error() -> Result<()> {
    let temp_dir = TempDir::new()?;
    // Don't create index.json

    let base_url = Url::from_file_path(temp_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))?;
    let client = RegistryClient::with_base_url(base_url)?;

    // Should fail to read file
    let result = client.fetch_index().await;
    assert!(result.is_err());

    Ok(())
}

#[tokio::test]
async fn test_invalid_version_comparison_error() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("index.json");

    let mock_index = r#"{
        "updated": "2024-01-01T00:00:00Z",
        "packs": {
            "test-pack": {
                "id": "test-pack",
                "name": "Test Pack",
                "description": "Test",
                "tags": [],
                "keywords": [],
                "category": null,
                "author": null,
                "latest_version": "invalid-version",
                "versions": {},
                "downloads": null,
                "updated": null,
                "license": null,
                "homepage": null,
                "repository": null,
                "documentation": null
            }
        }
    }"#;

    std::fs::write(&index_path, mock_index)?;

    let base_url = Url::from_file_path(temp_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))?;
    let client = RegistryClient::with_base_url(base_url)?;

    // Should fail due to invalid semver
    let result = client.check_updates("test-pack", "1.0.0").await;
    assert!(result.is_err());

    Ok(())
}

#[test]
fn test_error_context_preservation() {
    let result: Result<()> = Err(anyhow::anyhow!("Original error"))
        .context("Additional context");

    assert!(result.is_err());
    let error_msg = format!("{:?}", result.err().unwrap());
    assert!(error_msg.contains("Additional context"));
    assert!(error_msg.contains("Original error"));
}

#[test]
fn test_empty_version_map_error() {
    let pack = PackMetadata {
        id: "test-pack".to_string(),
        name: "Test Pack".to_string(),
        description: "Test".to_string(),
        tags: vec![],
        keywords: vec![],
        category: None,
        author: None,
        latest_version: "1.0.0".to_string(),
        versions: HashMap::new(), // Empty versions!
        downloads: None,
        updated: None,
        license: None,
        homepage: None,
        repository: None,
        documentation: None,
    };

    // Latest version should not be in versions map
    assert!(!pack.versions.contains_key(&pack.latest_version));
}
