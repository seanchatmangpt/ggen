#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic, clippy::needless_raw_string_hashes, clippy::duration_suboptimal_units, clippy::branches_sharing_code, clippy::used_underscore_binding, clippy::single_char_pattern, clippy::ignore_without_reason, clippy::cloned_ref_to_slice_refs, clippy::doc_overindented_list_items, clippy::match_wildcard_for_single_variants, clippy::ignored_unit_patterns, clippy::needless_collect, clippy::unnecessary_map_or, clippy::manual_flatten, clippy::manual_strip, clippy::future_not_send, clippy::unnested_or_patterns, clippy::no_effect_underscore_binding, clippy::literal_string_with_formatting_args)]
//! Unit tests for error handling paths

use chicago_tdd_tools::prelude::*;
use ggen_core::registry::{PackMetadata, RegistryClient, VersionMetadata};
use std::collections::HashMap;
use tempfile::TempDir;
use url::Url;

async_test!(test_missing_pack_error, async {
    // Arrange
    let temp_dir = TempDir::new().unwrap();
    let index_path = temp_dir.path().join("index.json");

    let mock_index = r#"{
        "updated": "2024-01-01T00:00:00Z",
        "packs": {}
    }"#;

    std::fs::write(&index_path, mock_index).unwrap();

    let base_url = Url::from_file_path(temp_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))
        .unwrap();
    let client = RegistryClient::with_base_url(base_url).unwrap();

    // Act
    let result = client.resolve("nonexistent-pack", None).await;

    // Assert
    assert_err!(&result);
    let error_msg = format!("{:?}", result.err().unwrap());
    assert!(error_msg.contains("not found"));
});

async_test!(test_missing_version_error, async {
    // Arrange
    let temp_dir = TempDir::new().unwrap();
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
                "latest_version": "26.6.6",
                "versions": {
                    "26.6.6": {
                        "version": "26.6.6",
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

    std::fs::write(&index_path, mock_index).unwrap();

    let base_url = Url::from_file_path(temp_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))
        .unwrap();
    let client = RegistryClient::with_base_url(base_url).unwrap();

    // Act
    let result = client.resolve("test-pack", Some("2.0.0")).await;

    // Assert
    assert_err!(&result);
    let error_msg = format!("{:?}", result.err().unwrap());
    assert!(error_msg.contains("Version"));
    assert!(error_msg.contains("not found"));
});

test!(test_invalid_semver_error, {
    // Arrange & Act
    let result = semver::Version::parse("not-a-version");

    // Assert
    assert_err!(&result);
});

test!(test_json_parsing_error, {
    // Arrange
    let invalid_json = "{ invalid json }";

    // Act
    let result: Result<PackMetadata, _> = serde_json::from_str(invalid_json);

    // Assert
    assert_err!(&result);
});

test!(test_missing_required_fields_error, {
    // Arrange
    let incomplete_json = r#"{
        "id": "test-pack"
    }"#;

    // Act
    let result: Result<PackMetadata, _> = serde_json::from_str(incomplete_json);

    // Assert
    assert_err!(&result);
});

async_test!(test_malformed_index_error, async {
    // Arrange
    let temp_dir = TempDir::new().unwrap();
    let index_path = temp_dir.path().join("index.json");

    std::fs::write(&index_path, "{ malformed json }").unwrap();

    let base_url = Url::from_file_path(temp_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))
        .unwrap();
    let client = RegistryClient::with_base_url(base_url).unwrap();

    // Act
    let result = client.fetch_index().await;

    // Assert
    assert_err!(&result);
});

async_test!(test_missing_index_file_error, async {
    // Arrange
    let temp_dir = TempDir::new().unwrap();

    let base_url = Url::from_file_path(temp_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))
        .unwrap();
    let client = RegistryClient::with_base_url(base_url).unwrap();

    // Act
    let result = client.fetch_index().await;

    // Assert
    assert_err!(&result);
});

async_test!(test_invalid_version_comparison_error, async {
    // Arrange
    let temp_dir = TempDir::new().unwrap();
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

    std::fs::write(&index_path, mock_index).unwrap();

    let base_url = Url::from_file_path(temp_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))
        .unwrap();
    let client = RegistryClient::with_base_url(base_url).unwrap();

    // Act
    let result = client.check_updates("test-pack", "26.6.6").await;

    // Assert
    assert_err!(&result);
});

test!(test_error_context_preservation, {
    // Arrange & Act
    let result: Result<()> = Err(anyhow::anyhow!("Original error")).context("Additional context");

    // Assert
    assert_err!(&result);
    let error_msg = format!("{:?}", result.err().unwrap());
    assert!(error_msg.contains("Additional context"));
    assert!(error_msg.contains("Original error"));
});

test!(test_empty_version_map_error, {
    // Arrange
    let pack = PackMetadata {
        id: "test-pack".to_string(),
        name: "Test Pack".to_string(),
        description: "Test".to_string(),
        tags: vec![],
        keywords: vec![],
        category: None,
        author: None,
        latest_version: "26.6.6".to_string(),
        versions: HashMap::new(),
        downloads: None,
        updated: None,
        license: None,
        homepage: None,
        repository: None,
        documentation: None,
    };

    // Assert
    assert!(!pack.versions.contains_key(&pack.latest_version));
});
