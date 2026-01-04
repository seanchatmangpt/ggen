//! HTTP mocking helpers for gpack tests
//!
//! Provides utilities for mocking crates.io API responses
//! and other HTTP interactions.
//!
//! Feature: 014-marketplace-gpack T003

use std::collections::HashMap;

/// Mock response for crates.io crate info
#[derive(Debug, Clone)]
pub struct MockCrateInfo {
    pub name: String,
    pub max_version: String,
    pub description: Option<String>,
    pub downloads: u64,
    pub repository: Option<String>,
    pub versions: Vec<MockVersionInfo>,
}

/// Mock version info
#[derive(Debug, Clone)]
pub struct MockVersionInfo {
    pub num: String,
    pub checksum: String,
    pub yanked: bool,
    pub features: HashMap<String, Vec<String>>,
}

impl MockCrateInfo {
    /// Create a new mock crate with sensible defaults
    pub fn new(name: &str, version: &str) -> Self {
        Self {
            name: name.to_string(),
            max_version: version.to_string(),
            description: Some(format!("Mock crate: {}", name)),
            downloads: 1000,
            repository: Some(format!("https://github.com/example/{}", name)),
            versions: vec![MockVersionInfo {
                num: version.to_string(),
                checksum: format!("sha256:mock_{}_checksum", name),
                yanked: false,
                features: HashMap::new(),
            }],
        }
    }

    /// Add a version to the mock crate
    pub fn with_version(mut self, version: &str, yanked: bool) -> Self {
        self.versions.push(MockVersionInfo {
            num: version.to_string(),
            checksum: format!("sha256:mock_{}_{}_checksum", self.name, version),
            yanked,
            features: HashMap::new(),
        });
        self
    }

    /// Convert to JSON response
    pub fn to_json(&self) -> String {
        let versions: Vec<String> = self.versions.iter().map(|v| {
            format!(r#"{{
                "num": "{}",
                "dl_path": "/api/v1/crates/{}/{}/download",
                "checksum": "{}",
                "yanked": {},
                "features": {{}}
            }}"#, v.num, self.name, v.num, v.checksum, v.yanked)
        }).collect();

        format!(r#"{{
            "crate": {{
                "name": "{}",
                "max_version": "{}",
                "description": {},
                "downloads": {},
                "recent_downloads": 100,
                "repository": {},
                "categories": [],
                "keywords": []
            }},
            "versions": [{}]
        }}"#,
            self.name,
            self.max_version,
            self.description.as_ref().map(|d| format!(r#""{}""#, d)).unwrap_or_else(|| "null".to_string()),
            self.downloads,
            self.repository.as_ref().map(|r| format!(r#""{}""#, r)).unwrap_or_else(|| "null".to_string()),
            versions.join(",")
        )
    }
}

/// Mock search response
pub struct MockSearchResponse {
    pub crates: Vec<MockCrateInfo>,
}

impl MockSearchResponse {
    pub fn new() -> Self {
        Self { crates: Vec::new() }
    }

    pub fn with_crate(mut self, info: MockCrateInfo) -> Self {
        self.crates.push(info);
        self
    }

    pub fn to_json(&self) -> String {
        let crates: Vec<String> = self.crates.iter().map(|c| {
            format!(r#"{{
                "name": "{}",
                "max_version": "{}",
                "description": {},
                "downloads": {},
                "categories": [],
                "keywords": []
            }}"#,
                c.name,
                c.max_version,
                c.description.as_ref().map(|d| format!(r#""{}""#, d)).unwrap_or_else(|| "null".to_string()),
                c.downloads
            )
        }).collect();

        format!(r#"{{"crates": [{}]}}"#, crates.join(","))
    }
}

impl Default for MockSearchResponse {
    fn default() -> Self {
        Self::new()
    }
}

/// Generate mock package content (tarball bytes)
pub fn mock_package_content(name: &str, version: &str) -> Vec<u8> {
    // Create minimal valid content for testing
    let content = format!("MOCK_PACKAGE:{}:{}", name, version);
    content.into_bytes()
}

/// Generate mock SHA-256 checksum
pub fn mock_checksum(data: &[u8]) -> String {
    use sha2::{Sha256, Digest};
    let hash = Sha256::digest(data);
    hex::encode(hash)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mock_crate_info() {
        let info = MockCrateInfo::new("serde", "1.0.0")
            .with_version("0.9.0", false);

        assert_eq!(info.name, "serde");
        assert_eq!(info.versions.len(), 2);
    }

    #[test]
    fn test_mock_crate_json() {
        let info = MockCrateInfo::new("serde", "1.0.0");
        let json = info.to_json();

        assert!(json.contains("serde"));
        assert!(json.contains("1.0.0"));
    }

    #[test]
    fn test_mock_search_response() {
        let response = MockSearchResponse::new()
            .with_crate(MockCrateInfo::new("serde", "1.0.0"))
            .with_crate(MockCrateInfo::new("tokio", "1.0.0"));

        let json = response.to_json();
        assert!(json.contains("serde"));
        assert!(json.contains("tokio"));
    }

    #[test]
    fn test_mock_checksum() {
        let data = b"test content";
        let checksum = mock_checksum(data);
        assert_eq!(checksum.len(), 64); // SHA-256 = 32 bytes = 64 hex chars
    }
}
