//! Crates.io registry client for package fetching
//!
//! This module provides HTTP client functionality for interacting with
//! crates.io and other compatible package registries.

use serde::{Deserialize, Serialize};
use std::time::Duration;

use super::error::{GpackError, GpackResult};

/// Default crates.io API endpoint
pub const CRATES_IO_API: &str = "https://crates.io/api/v1";

/// Default request timeout
pub const DEFAULT_TIMEOUT: Duration = Duration::from_secs(30);

/// User agent for API requests
pub const USER_AGENT: &str = "ggen-marketplace/5.0";

/// Client for interacting with crates.io registry
#[derive(Debug, Clone)]
pub struct CratesClient {
    /// Base URL for the registry API
    base_url: String,
    /// HTTP client
    client: reqwest::Client,
    /// Request timeout
    timeout: Duration,
}

/// Crate metadata from registry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CrateInfo {
    /// Crate name
    pub name: String,
    /// Latest version
    pub max_version: String,
    /// Crate description
    pub description: Option<String>,
    /// Documentation URL
    pub documentation: Option<String>,
    /// Repository URL
    pub repository: Option<String>,
    /// Download count
    pub downloads: u64,
    /// Recent downloads
    pub recent_downloads: Option<u64>,
    /// Categories
    pub categories: Vec<String>,
    /// Keywords
    pub keywords: Vec<String>,
}

/// Version metadata from registry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VersionInfo {
    /// Version number
    pub num: String,
    /// Download URL for the crate
    pub dl_path: String,
    /// SHA256 checksum
    pub checksum: String,
    /// Whether version is yanked
    pub yanked: bool,
    /// Rust version requirement
    pub rust_version: Option<String>,
    /// Features available
    pub features: std::collections::HashMap<String, Vec<String>>,
}

/// Response wrapper for crate info
#[derive(Debug, Deserialize)]
pub struct CrateResponse {
    /// The crate info
    #[serde(rename = "crate")]
    pub krate: CrateInfo,
    /// Available versions
    pub versions: Vec<VersionInfo>,
}

impl Default for CratesClient {
    fn default() -> Self {
        Self::new()
    }
}

impl CratesClient {
    /// Create a new crates.io client with default settings
    pub fn new() -> Self {
        Self::with_config(CRATES_IO_API, DEFAULT_TIMEOUT)
    }

    /// Create a client with custom configuration
    pub fn with_config(base_url: &str, timeout: Duration) -> Self {
        let client = reqwest::Client::builder()
            .timeout(timeout)
            .user_agent(USER_AGENT)
            .build()
            .unwrap_or_else(|_| reqwest::Client::new());

        Self {
            base_url: base_url.to_string(),
            client,
            timeout,
        }
    }

    /// Fetch crate information by name
    pub async fn get_crate(&self, name: &str) -> GpackResult<CrateResponse> {
        let url = format!("{}/crates/{}", self.base_url, name);

        let response = self
            .client
            .get(&url)
            .send()
            .await
            .map_err(|e| GpackError::NetworkError(e.to_string()))?;

        if response.status() == reqwest::StatusCode::NOT_FOUND {
            return Err(GpackError::PackageNotFound(name.to_string()));
        }

        if !response.status().is_success() {
            return Err(GpackError::RegistryError(format!(
                "Registry returned status {}",
                response.status()
            )));
        }

        response
            .json()
            .await
            .map_err(|e| GpackError::SerializationError(e.to_string()))
    }

    /// Search for crates by query
    pub async fn search(&self, query: &str, per_page: u32) -> GpackResult<Vec<CrateInfo>> {
        let url = format!(
            "{}/crates?q={}&per_page={}",
            self.base_url, query, per_page
        );

        let response = self
            .client
            .get(&url)
            .send()
            .await
            .map_err(|e| GpackError::NetworkError(e.to_string()))?;

        if !response.status().is_success() {
            return Err(GpackError::RegistryError(format!(
                "Search failed with status {}",
                response.status()
            )));
        }

        #[derive(Deserialize)]
        struct SearchResponse {
            crates: Vec<CrateInfo>,
        }

        let search_response: SearchResponse = response
            .json()
            .await
            .map_err(|e| GpackError::SerializationError(e.to_string()))?;

        Ok(search_response.crates)
    }

    /// Download crate tarball
    pub async fn download_crate(&self, name: &str, version: &str) -> GpackResult<Vec<u8>> {
        // First get the crate info to find the download path
        let crate_info = self.get_crate(name).await?;

        let version_info = crate_info
            .versions
            .iter()
            .find(|v| v.num == version)
            .ok_or_else(|| {
                GpackError::PackageNotFound(format!("{}@{}", name, version))
            })?;

        let download_url = format!("https://crates.io{}", version_info.dl_path);

        let response = self
            .client
            .get(&download_url)
            .send()
            .await
            .map_err(|e| GpackError::NetworkError(e.to_string()))?;

        if !response.status().is_success() {
            return Err(GpackError::NetworkError(format!(
                "Download failed with status {}",
                response.status()
            )));
        }

        response
            .bytes()
            .await
            .map(|b| b.to_vec())
            .map_err(|e| GpackError::NetworkError(e.to_string()))
    }

    /// Get the base URL
    pub fn base_url(&self) -> &str {
        &self.base_url
    }

    /// Get the timeout
    pub fn timeout(&self) -> Duration {
        self.timeout
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_client() {
        let client = CratesClient::new();
        assert_eq!(client.base_url(), CRATES_IO_API);
        assert_eq!(client.timeout(), DEFAULT_TIMEOUT);
    }

    #[test]
    fn test_custom_client() {
        let timeout = Duration::from_secs(60);
        let client = CratesClient::with_config("https://example.com/api", timeout);
        assert_eq!(client.base_url(), "https://example.com/api");
        assert_eq!(client.timeout(), timeout);
    }
}
