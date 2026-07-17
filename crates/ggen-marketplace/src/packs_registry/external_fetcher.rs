//! External registry fetchers for crates.io, npm, and PyPi
//!
//! This module provides traits and implementations for fetching package metadata
//! and artifacts from external registries.

use crate::marketplace::error::{Error, Result};
use async_trait::async_trait;
use reqwest::header::{HeaderMap, HeaderValue, USER_AGENT};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use tracing::info;

/// Internal Package domain model for remote packages
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Package {
    pub id: String,
    pub name: String,
    pub latest_version: String,
    pub versions: Vec<String>,
    pub description: Option<String>,
    pub homepage: Option<String>,
    pub repository: Option<String>,
    pub license: Option<String>,
    /// Maps version string to download URL
    pub download_urls: HashMap<String, String>,
    /// Maps version string to SHA256 checksum
    pub checksums: HashMap<String, String>,
}

/// Trait for fetching metadata and artifacts from external registries
#[async_trait]
pub trait ExternalRegistryFetcher: Send + Sync {
    /// Fetch metadata for a package
    async fn fetch_metadata(&self, package_id: &str) -> Result<Package>;

    /// Fetch the artifact (tarball/zip) for a specific version
    async fn fetch_artifact(&self, package_id: &str, version: &str) -> Result<Vec<u8>>;

    /// Get the registry prefix (e.g., "cratesio", "npm", "pypi")
    fn registry_prefix(&self) -> &str;
}

/// Fetcher for crates.io
pub struct CratesIoFetcher {
    client: reqwest::Client,
}

impl CratesIoFetcher {
    pub fn new() -> Self {
        let mut headers = HeaderMap::new();
        headers.insert(
            USER_AGENT,
            HeaderValue::from_static("ggen (https://github.com/seanchatmangpt/ggen)"),
        );

        Self {
            client: reqwest::Client::builder()
                .default_headers(headers)
                .build()
                .unwrap_or_default(),
        }
    }
}

impl Default for CratesIoFetcher {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl ExternalRegistryFetcher for CratesIoFetcher {
    async fn fetch_metadata(&self, package_id: &str) -> Result<Package> {
        info!(
            "Fetching metadata for crate '{}' from crates.io",
            package_id
        );
        let url = format!("https://crates.io/api/v1/crates/{}", package_id);

        let response =
            self.client.get(&url).send().await.map_err(|e| {
                Error::Other(format!("Failed to fetch metadata from crates.io: {}", e))
            })?;

        if !response.status().is_success() {
            return Err(Error::Other(format!(
                "crates.io API returned error status: {}",
                response.status()
            )));
        }

        let data: serde_json::Value = response
            .json()
            .await
            .map_err(|e| Error::Other(format!("Failed to parse crates.io response: {}", e)))?;

        Self::parse_cratesio_response(package_id, data)
    }

    async fn fetch_artifact(&self, package_id: &str, version: &str) -> Result<Vec<u8>> {
        let metadata = self.fetch_metadata(package_id).await?;
        let url = metadata.download_urls.get(version).ok_or_else(|| {
            Error::Other(format!("Download URL not found for version {}", version))
        })?;

        let response = self.client.get(url).send().await.map_err(|e| {
            Error::Other(format!("Failed to download artifact from crates.io: {}", e))
        })?;

        if !response.status().is_success() {
            return Err(Error::Other(format!(
                "Failed to download artifact: status {}",
                response.status()
            )));
        }

        let bytes = response
            .bytes()
            .await
            .map_err(|e| Error::Other(format!("Failed to read artifact bytes: {}", e)))?;

        Ok(bytes.to_vec())
    }

    fn registry_prefix(&self) -> &str {
        "cratesio"
    }
}

impl CratesIoFetcher {
    pub fn parse_cratesio_response(package_id: &str, data: serde_json::Value) -> Result<Package> {
        let crate_data = data.get("crate").ok_or_else(|| {
            Error::Other("Missing 'crate' field in crates.io response".to_string())
        })?;
        let name = crate_data
            .get("name")
            .and_then(|v| v.as_str())
            .unwrap_or(package_id)
            .to_string();
        let latest_version = crate_data
            .get("max_version")
            .and_then(|v| v.as_str())
            .unwrap_or("0.0.0")
            .to_string();

        let mut versions = Vec::new();
        let mut download_urls = HashMap::new();
        let mut checksums = HashMap::new();

        if let Some(versions_array) = data.get("versions").and_then(|v| v.as_array()) {
            for v in versions_array {
                if let (Some(num), Some(dl_path), Some(checksum)) = (
                    v.get("num").and_then(|n| n.as_str()),
                    v.get("dl_path").and_then(|d| d.as_str()),
                    v.get("checksum").and_then(|c| c.as_str()),
                ) {
                    versions.push(num.to_string());
                    download_urls.insert(num.to_string(), format!("https://crates.io{}", dl_path));
                    checksums.insert(num.to_string(), checksum.to_string());
                }
            }
        }

        Ok(Package {
            id: package_id.to_string(),
            name,
            latest_version,
            versions,
            description: crate_data
                .get("description")
                .and_then(|v| v.as_str())
                .map(|s| s.to_string()),
            homepage: crate_data
                .get("homepage")
                .and_then(|v| v.as_str())
                .map(|s| s.to_string()),
            repository: crate_data
                .get("repository")
                .and_then(|v| v.as_str())
                .map(|s| s.to_string()),
            license: crate_data
                .get("license")
                .and_then(|v| v.as_str())
                .map(|s| s.to_string()),
            download_urls,
            checksums,
        })
    }
}

/// Fetcher for npm
pub struct NpmFetcher {
    client: reqwest::Client,
}

impl NpmFetcher {
    pub fn new() -> Self {
        Self {
            client: reqwest::Client::new(),
        }
    }
}

impl Default for NpmFetcher {
    fn default() -> Self {
        Self::new()
    }
}

impl NpmFetcher {
    pub fn parse_npm_response(package_id: &str, data: serde_json::Value) -> Result<Package> {
        let name = data
            .get("name")
            .and_then(|v| v.as_str())
            .unwrap_or(package_id)
            .to_string();
        let latest_version = data
            .get("dist-tags")
            .and_then(|v| v.get("latest"))
            .and_then(|v| v.as_str())
            .unwrap_or("0.0.0")
            .to_string();

        let mut versions = Vec::new();
        let mut download_urls = HashMap::new();
        let mut checksums = HashMap::new();

        if let Some(versions_map) = data.get("versions").and_then(|v| v.as_object()) {
            for (version, v_data) in versions_map {
                versions.push(version.clone());
                if let Some(dist) = v_data.get("dist") {
                    if let Some(tarball) = dist.get("tarball").and_then(|v| v.as_str()) {
                        download_urls.insert(version.clone(), tarball.to_string());
                    }
                    if let Some(shasum) = dist.get("shasum").and_then(|v| v.as_str()) {
                        checksums.insert(version.clone(), shasum.to_string());
                    }
                }
            }
        }

        Ok(Package {
            id: package_id.to_string(),
            name,
            latest_version,
            versions,
            description: data
                .get("description")
                .and_then(|v| v.as_str())
                .map(|s| s.to_string()),
            homepage: data
                .get("homepage")
                .and_then(|v| v.as_str())
                .map(|s| s.to_string()),
            repository: data
                .get("repository")
                .and_then(|v| v.get("url"))
                .and_then(|v| v.as_str())
                .map(|s| s.to_string()),
            license: data
                .get("license")
                .and_then(|v| v.as_str())
                .map(|s| s.to_string()),
            download_urls,
            checksums,
        })
    }
}

#[async_trait]
impl ExternalRegistryFetcher for NpmFetcher {
    async fn fetch_metadata(&self, package_id: &str) -> Result<Package> {
        info!(
            "Fetching metadata for package '{}' from npm registry",
            package_id
        );
        let url = format!("https://registry.npmjs.org/{}", package_id);

        let response = self
            .client
            .get(&url)
            .send()
            .await
            .map_err(|e| Error::Other(format!("Failed to fetch metadata from npm: {}", e)))?;

        if !response.status().is_success() {
            return Err(Error::Other(format!(
                "npm registry returned error status: {}",
                response.status()
            )));
        }

        let data: serde_json::Value = response
            .json()
            .await
            .map_err(|e| Error::Other(format!("Failed to parse npm response: {}", e)))?;

        Self::parse_npm_response(package_id, data)
    }

    async fn fetch_artifact(&self, package_id: &str, version: &str) -> Result<Vec<u8>> {
        let metadata = self.fetch_metadata(package_id).await?;
        let url = metadata.download_urls.get(version).ok_or_else(|| {
            Error::Other(format!("Download URL not found for version {}", version))
        })?;

        let response =
            self.client.get(url).send().await.map_err(|e| {
                Error::Other(format!("Failed to download artifact from npm: {}", e))
            })?;

        let bytes = response
            .bytes()
            .await
            .map_err(|e| Error::Other(format!("Failed to read artifact bytes: {}", e)))?;

        Ok(bytes.to_vec())
    }

    fn registry_prefix(&self) -> &str {
        "npm"
    }
}

/// Fetcher for PyPi
pub struct PyPiFetcher {
    client: reqwest::Client,
}

impl PyPiFetcher {
    pub fn new() -> Self {
        Self {
            client: reqwest::Client::new(),
        }
    }
}

impl Default for PyPiFetcher {
    fn default() -> Self {
        Self::new()
    }
}

impl PyPiFetcher {
    pub fn parse_pypi_response(package_id: &str, data: serde_json::Value) -> Result<Package> {
        let info = data
            .get("info")
            .ok_or_else(|| Error::Other("Missing 'info' field in PyPi response".to_string()))?;
        let name = info
            .get("name")
            .and_then(|v| v.as_str())
            .unwrap_or(package_id)
            .to_string();
        let latest_version = info
            .get("version")
            .and_then(|v| v.as_str())
            .unwrap_or("0.0.0")
            .to_string();

        let mut versions = Vec::new();
        let mut download_urls = HashMap::new();
        let mut checksums = HashMap::new();

        if let Some(releases) = data.get("releases").and_then(|v| v.as_object()) {
            for (version, files) in releases {
                versions.push(version.clone());
                if let Some(files_array) = files.as_array() {
                    // Prefer sdist (source distribution)
                    let file_info = files_array
                        .iter()
                        .find(|f| f.get("packagetype").and_then(|v| v.as_str()) == Some("sdist"))
                        .or_else(|| files_array.first());

                    if let Some(file) = file_info {
                        if let Some(url) = file.get("url").and_then(|v| v.as_str()) {
                            download_urls.insert(version.clone(), url.to_string());
                        }
                        if let Some(digests) = file.get("digests") {
                            if let Some(sha256) = digests.get("sha256").and_then(|v| v.as_str()) {
                                checksums.insert(version.clone(), sha256.to_string());
                            }
                        }
                    }
                }
            }
        }

        Ok(Package {
            id: package_id.to_string(),
            name,
            latest_version,
            versions,
            description: info
                .get("summary")
                .and_then(|v| v.as_str())
                .map(|s| s.to_string()),
            homepage: info
                .get("home_page")
                .and_then(|v| v.as_str())
                .map(|s| s.to_string()),
            repository: info
                .get("project_urls")
                .and_then(|v| v.get("Repository"))
                .and_then(|v| v.as_str())
                .map(|s| s.to_string()),
            license: info
                .get("license")
                .and_then(|v| v.as_str())
                .map(|s| s.to_string()),
            download_urls,
            checksums,
        })
    }
}

#[async_trait]
impl ExternalRegistryFetcher for PyPiFetcher {
    async fn fetch_metadata(&self, package_id: &str) -> Result<Package> {
        info!("Fetching metadata for package '{}' from PyPi", package_id);
        let url = format!("https://pypi.org/pypi/{}/json", package_id);

        let response = self
            .client
            .get(&url)
            .send()
            .await
            .map_err(|e| Error::Other(format!("Failed to fetch metadata from PyPi: {}", e)))?;

        if !response.status().is_success() {
            return Err(Error::Other(format!(
                "PyPi returned error status: {}",
                response.status()
            )));
        }

        let data: serde_json::Value = response
            .json()
            .await
            .map_err(|e| Error::Other(format!("Failed to parse PyPi response: {}", e)))?;

        Self::parse_pypi_response(package_id, data)
    }

    async fn fetch_artifact(&self, package_id: &str, version: &str) -> Result<Vec<u8>> {
        let metadata = self.fetch_metadata(package_id).await?;
        let url = metadata.download_urls.get(version).ok_or_else(|| {
            Error::Other(format!("Download URL not found for version {}", version))
        })?;

        let response =
            self.client.get(url).send().await.map_err(|e| {
                Error::Other(format!("Failed to download artifact from PyPi: {}", e))
            })?;

        let bytes = response
            .bytes()
            .await
            .map_err(|e| Error::Other(format!("Failed to read artifact bytes: {}", e)))?;

        Ok(bytes.to_vec())
    }

    fn registry_prefix(&self) -> &str {
        "pypi"
    }
}

/// Factory for creating external registry fetchers
pub struct ExternalFetcherFactory;

impl ExternalFetcherFactory {
    pub fn get_fetcher(registry_type: &str) -> Result<Box<dyn ExternalRegistryFetcher>> {
        match registry_type {
            "cratesio" | "crates.io" => Ok(Box::new(CratesIoFetcher::new())),
            "npm" => Ok(Box::new(NpmFetcher::new())),
            "pypi" => Ok(Box::new(PyPiFetcher::new())),
            _ => Err(Error::Other(format!(
                "Unsupported registry type: {}",
                registry_type
            ))),
        }
    }

    pub fn get_fetcher_by_prefix(
        package_id: &str,
    ) -> Result<(Box<dyn ExternalRegistryFetcher>, String)> {
        if let Some(idx) = package_id.find(':') {
            let (prefix, id) = package_id.split_at(idx);
            let id = &id[1..];
            let fetcher = Self::get_fetcher(prefix)?;
            Ok((fetcher, id.to_string()))
        } else {
            Err(Error::Other(
                "Package ID must contain a prefix (e.g., 'npm:lodash')".to_string(),
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_parse_npm_response() {
        let data = json!({
            "name": "lodash",
            "description": "Lodash modular utilities.",
            "dist-tags": { "latest": "4.17.21" },
            "homepage": "https://lodash.com/",
            "license": "MIT",
            "repository": { "type": "git", "url": "git+https://github.com/lodash/lodash.git" },
            "versions": {
                "4.17.21": {
                    "dist": {
                        "tarball": "https://registry.npmjs.org/lodash/-/lodash-4.17.21.tgz",
                        "shasum": "764c58b577159a99264c07d6ff52561ad337922e"
                    }
                }
            }
        });

        let pkg = NpmFetcher::parse_npm_response("lodash", data).unwrap();
        assert_eq!(pkg.name, "lodash");
        assert_eq!(pkg.latest_version, "4.17.21");
        assert_eq!(
            pkg.download_urls.get("4.17.21").unwrap(),
            "https://registry.npmjs.org/lodash/-/lodash-4.17.21.tgz"
        );
        assert_eq!(
            pkg.checksums.get("4.17.21").unwrap(),
            "764c58b577159a99264c07d6ff52561ad337922e"
        );
    }

    #[test]
    fn test_parse_cratesio_response() {
        let data = json!({
            "crate": {
                "name": "serde",
                "description": "A generic serialization/deserialization framework",
                "max_version": "1.0.152",
                "homepage": "https://serde.rs",
                "repository": "https://github.com/serde-rs/serde",
                "license": "MIT OR Apache-2.0"
            },
            "versions": [
                {
                    "num": "1.0.152",
                    "dl_path": "/api/v1/crates/serde/1.0.152/download",
                    "checksum": "bbccd84351247a9f2f0561114d39ec3312fddaba5f28c5a3c051b8b23f0047c2"
                }
            ]
        });

        let pkg = CratesIoFetcher::parse_cratesio_response("serde", data).unwrap();
        assert_eq!(pkg.name, "serde");
        assert_eq!(pkg.latest_version, "1.0.152");
        assert_eq!(
            pkg.download_urls.get("1.0.152").unwrap(),
            "https://crates.io/api/v1/crates/serde/1.0.152/download"
        );
    }
}
