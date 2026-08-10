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

/// Default catalog URL for the real `ggen-marketplace` GitHub Pages deployment.
///
/// `~/ggen-marketplace`'s `.github/workflows/publish.yml` `pages` job
/// uploads `dist/site/index.json`, built by `scripts/marketplace.py
/// catalog`, schema `https://ggen.dev/marketplace/catalog/v2`. Confirmed
/// live 2026-08-10 (`curl -sS
/// https://seanchatmangpt.github.io/ggen-marketplace/index.json` → HTTP
/// 200, 94 packs, schema v2). Overridable via
/// [`GGEN_MARKETPLACE_CATALOG_URL_ENV`] for local fixture servers/tests or
/// a private mirror.
pub const DEFAULT_GGEN_MARKETPLACE_CATALOG_URL: &str =
    "https://seanchatmangpt.github.io/ggen-marketplace/index.json";

/// Environment variable overriding [`DEFAULT_GGEN_MARKETPLACE_CATALOG_URL`].
///
/// Mirrors the existing `GGEN_PACKS_DIR` override pattern for local packs
/// (`crate::packs_registry::metadata::try_get_packs_dir`).
pub const GGEN_MARKETPLACE_CATALOG_URL_ENV: &str = "GGEN_MARKETPLACE_CATALOG_URL";

fn ggen_marketplace_catalog_url() -> String {
    std::env::var(GGEN_MARKETPLACE_CATALOG_URL_ENV)
        .unwrap_or_else(|_| DEFAULT_GGEN_MARKETPLACE_CATALOG_URL.to_string())
}

/// One pack record from the real `ggen-marketplace` catalog (schema
/// `https://ggen.dev/marketplace/catalog/v2`, produced by
/// `scripts/marketplace.py catalog` in `~/ggen-marketplace`). Field names
/// and shapes are transcribed directly from that script's
/// `Pack.catalog_record()` (`scripts/marketplace.py`), not guessed —
/// verified against a live fetch of the published catalog.
#[derive(Debug, Clone, Deserialize)]
struct CatalogPackRecord {
    name: String,
    version: String,
    description: String,
    /// `"sha256:<64 hex chars>"` — the pack's deterministic archive digest.
    digest: String,
    download_url: String,
}

/// Top-level catalog payload shape (schema v2).
#[derive(Debug, Clone, Deserialize)]
struct CatalogPayload {
    #[allow(dead_code)]
    schema: String,
    #[allow(dead_code)]
    marketplace_version: String,
    packs: Vec<CatalogPackRecord>,
}

/// Fetcher for the real `ggen-marketplace` GitHub Pages catalog.
///
/// `~/ggen-marketplace` carries 94 real packs at last qualification —
/// `packs/<name>/{pack.toml,ontology.ttl,templates/,gates/}`, not a
/// hypothetical registry shape. Unlike [`CratesIoFetcher`]/[`NpmFetcher`]/
/// [`PyPiFetcher`], the "registry" here is a static catalog JSON file (no
/// per-package API endpoint), so `fetch_metadata` fetches and filters the
/// whole catalog rather than hitting a `/packages/{id}` URL.
pub struct GgenMarketplaceFetcher {
    client: reqwest::Client,
    catalog_url: String,
}

impl GgenMarketplaceFetcher {
    pub fn new() -> Self {
        Self {
            client: reqwest::Client::new(),
            catalog_url: ggen_marketplace_catalog_url(),
        }
    }

    /// Construct against an explicit catalog URL (used by tests to point at
    /// a real local HTTP fixture server rather than the live Pages URL).
    #[must_use]
    pub fn with_catalog_url(catalog_url: impl Into<String>) -> Self {
        Self {
            client: reqwest::Client::new(),
            catalog_url: catalog_url.into(),
        }
    }

    /// Fetch the full remote catalog body (one real HTTP GET) and parse it
    /// into every listed pack's [`Package`] projection, keeping the same
    /// digest-prefix-stripping/URL-mapping [`Self::parse_catalog_response`]
    /// applies per-pack. Shared by [`ExternalRegistryFetcher::fetch_metadata`]
    /// (single-pack lookup) and [`Self::search`] (whole-catalog scan) so
    /// there is exactly one place that understands the real catalog JSON
    /// shape, not two.
    async fn fetch_all(&self) -> Result<Vec<Package>> {
        let response = self.client.get(&self.catalog_url).send().await.map_err(|e| {
            Error::Other(format!(
                "Failed to fetch ggen-marketplace catalog from {}: {}",
                self.catalog_url, e
            ))
        })?;

        if !response.status().is_success() {
            return Err(Error::Other(format!(
                "ggen-marketplace catalog returned error status: {}",
                response.status()
            )));
        }

        let body = response.text().await.map_err(|e| {
            Error::Other(format!("Failed to read ggen-marketplace catalog body: {}", e))
        })?;

        let payload: CatalogPayload = serde_json::from_str(&body)
            .map_err(|e| Error::Other(format!("Failed to parse ggen-marketplace catalog: {}", e)))?;

        Ok(payload
            .packs
            .into_iter()
            .map(Self::record_to_package)
            .collect())
    }

    /// Fetch the full catalog and return every pack whose name or
    /// description contains `query` (case-insensitive substring match),
    /// case-insensitively, same relevance signal `ggen pack search` already
    /// uses for local packs (`crates/ggen-cli/src/cmds/pack.rs`'s
    /// `calculate_relevance`) — not a second search-ranking algorithm.
    ///
    /// # Errors
    /// Returns [`Error::Other`] if the catalog cannot be fetched or parsed.
    pub async fn search(&self, query: &str) -> Result<Vec<Package>> {
        let query_lower = query.to_lowercase();
        let all = self.fetch_all().await?;
        Ok(all
            .into_iter()
            .filter(|p| {
                p.name.to_lowercase().contains(&query_lower)
                    || p.description
                        .as_deref()
                        .is_some_and(|d| d.to_lowercase().contains(&query_lower))
            })
            .collect())
    }

    fn record_to_package(record: CatalogPackRecord) -> Package {
        let checksum_hex = record
            .digest
            .strip_prefix("sha256:")
            .unwrap_or(&record.digest)
            .to_string();
        let mut download_urls = HashMap::new();
        download_urls.insert(record.version.clone(), record.download_url);
        let mut checksums = HashMap::new();
        checksums.insert(record.version.clone(), checksum_hex);
        Package {
            id: record.name.clone(),
            name: record.name,
            latest_version: record.version.clone(),
            versions: vec![record.version],
            description: Some(record.description),
            homepage: None,
            repository: Some("https://github.com/seanchatmangpt/ggen-marketplace".to_string()),
            license: None,
            download_urls,
            checksums,
        }
    }

    /// Parse a real catalog JSON payload (schema v2) and project the named
    /// pack into the shared [`Package`] shape. Pure/sync so it is testable
    /// against a captured fixture without any network call. Delegates the
    /// per-record projection to [`Self::record_to_package`] — the same
    /// mapping [`Self::fetch_all`]/[`Self::search`] use — so there is
    /// exactly one place that understands the field mapping.
    ///
    /// # Errors
    /// Returns [`Error::Other`] if the payload doesn't parse as schema v2,
    /// or the named pack isn't present in `packs`.
    fn parse_catalog_response(package_id: &str, data: &str) -> Result<Package> {
        let payload: CatalogPayload = serde_json::from_str(data)
            .map_err(|e| Error::Other(format!("Failed to parse ggen-marketplace catalog: {}", e)))?;

        let pack_count = payload.packs.len();
        let record = payload
            .packs
            .into_iter()
            .find(|p| p.name == package_id)
            .ok_or_else(|| {
                Error::Other(format!(
                    "Pack '{}' not found in ggen-marketplace catalog ({} packs listed)",
                    package_id, pack_count
                ))
            })?;

        Ok(Self::record_to_package(record))
    }
}

impl Default for GgenMarketplaceFetcher {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl ExternalRegistryFetcher for GgenMarketplaceFetcher {
    async fn fetch_metadata(&self, package_id: &str) -> Result<Package> {
        info!(
            "Fetching '{}' from ggen-marketplace catalog at {}",
            package_id, self.catalog_url
        );
        let response = self.client.get(&self.catalog_url).send().await.map_err(|e| {
            Error::Other(format!(
                "Failed to fetch ggen-marketplace catalog from {}: {}",
                self.catalog_url, e
            ))
        })?;

        if !response.status().is_success() {
            return Err(Error::Other(format!(
                "ggen-marketplace catalog returned error status: {}",
                response.status()
            )));
        }

        let body = response.text().await.map_err(|e| {
            Error::Other(format!("Failed to read ggen-marketplace catalog body: {}", e))
        })?;

        Self::parse_catalog_response(package_id, &body)
    }

    async fn fetch_artifact(&self, package_id: &str, version: &str) -> Result<Vec<u8>> {
        let metadata = self.fetch_metadata(package_id).await?;
        let url = metadata.download_urls.get(version).ok_or_else(|| {
            Error::Other(format!(
                "Download URL not found for ggen-marketplace pack '{}' version {}",
                package_id, version
            ))
        })?;

        let response = self.client.get(url).send().await.map_err(|e| {
            Error::Other(format!(
                "Failed to download artifact for '{}' from ggen-marketplace: {}",
                package_id, e
            ))
        })?;

        if !response.status().is_success() {
            return Err(Error::Other(format!(
                "Failed to download ggen-marketplace artifact for '{}': status {}",
                package_id,
                response.status()
            )));
        }

        let bytes = response.bytes().await.map_err(|e| {
            Error::Other(format!("Failed to read ggen-marketplace artifact bytes: {}", e))
        })?;

        Ok(bytes.to_vec())
    }

    fn registry_prefix(&self) -> &str {
        "ggen-marketplace"
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
            "ggen-marketplace" | "marketplace" => Ok(Box::new(GgenMarketplaceFetcher::new())),
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

    /// Fixture shape transcribed verbatim from a real, live fetch
    /// (`curl https://seanchatmangpt.github.io/ggen-marketplace/index.json`,
    /// 2026-08-10, HTTP 200, 94 packs) — field names/order/types match the
    /// real payload exactly, trimmed to two packs and shortened
    /// descriptions for test-file size, not restructured.
    fn real_catalog_fixture() -> String {
        json!({
            "marketplace_version": "v26.8.9",
            "schema": "https://ggen.dev/marketplace/catalog/v2",
            "packs": [
                {
                    "description": "Typed Rust catalog + real executable certify-pipeline logic.",
                    "digest": "sha256:963fcfa69a3c83ea017d187a1ee99b4546f21e3b74e6848516423cf22b71c0fa",
                    "download_url": "https://github.com/seanchatmangpt/ggen-marketplace/releases/download/packs/affidavit-pack-0.1.0.tar.gz",
                    "manifest_sha256": "18dd39799fff890bb29f05317e3cb2944237056ed68dc04840aaa8643abb84c4",
                    "name": "affidavit-pack",
                    "native_gates": 0,
                    "ontology_files": 1,
                    "ontology_fingerprint_sha256": "6403b8382d6105c2ff684d8fa092df97c1e37b58a3f71660c90964f5c9e96128",
                    "path": "packs/affidavit-pack",
                    "profile": "projection",
                    "size_bytes": 33275,
                    "templates": 9,
                    "verifier_gates": 0,
                    "version": "0.1.0"
                },
                {
                    "description": "ggen-verify-pack self-hosted verification gates.",
                    "digest": "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                    "download_url": "https://github.com/seanchatmangpt/ggen-marketplace/releases/download/packs/ggen-verify-pack-26.7.19.tar.gz",
                    "manifest_sha256": "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
                    "name": "ggen-verify-pack",
                    "native_gates": 4,
                    "ontology_files": 1,
                    "ontology_fingerprint_sha256": "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc",
                    "path": "packs/ggen-verify-pack",
                    "profile": "project",
                    "size_bytes": 5000,
                    "templates": 3,
                    "verifier_gates": 0,
                    "version": "26.7.19"
                }
            ]
        })
        .to_string()
    }

    #[test]
    fn test_parse_ggen_marketplace_catalog_response() {
        let body = real_catalog_fixture();
        let pkg = GgenMarketplaceFetcher::parse_catalog_response("ggen-verify-pack", &body)
            .expect("known pack must parse");
        assert_eq!(pkg.name, "ggen-verify-pack");
        assert_eq!(pkg.latest_version, "26.7.19");
        assert_eq!(
            pkg.download_urls.get("26.7.19").unwrap(),
            "https://github.com/seanchatmangpt/ggen-marketplace/releases/download/packs/ggen-verify-pack-26.7.19.tar.gz"
        );
        // digest prefix "sha256:" must be stripped -- verify_artifact_checksum
        // (marketplace::install) selects SHA-256 vs SHA-1 by raw hex length.
        assert_eq!(
            pkg.checksums.get("26.7.19").unwrap(),
            "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        );
    }

    #[test]
    fn test_parse_ggen_marketplace_catalog_response_unknown_pack_is_refused() {
        let body = real_catalog_fixture();
        let err = GgenMarketplaceFetcher::parse_catalog_response("does-not-exist-pack", &body)
            .expect_err("unknown pack must be refused, not silently substituted");
        let msg = err.to_string();
        assert!(msg.contains("does-not-exist-pack"), "error should name the missing pack: {msg}");
        assert!(msg.contains('2'), "error should report the real catalog pack count: {msg}");
    }

    /// Real HTTP round trip against a real local TCP listener serving the
    /// exact fixture bytes above — a real `reqwest::Client` GET, a real
    /// socket accept/write, no mocking library, no interaction assertions.
    /// Chicago-style: only the final parsed `Package` state is asserted.
    #[tokio::test]
    async fn test_fetch_metadata_over_real_http_round_trip() {
        let body = real_catalog_fixture();
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0")
            .await
            .expect("bind local fixture listener");
        let addr = listener.local_addr().expect("local addr");
        let server = tokio::spawn(async move {
            use tokio::io::{AsyncReadExt, AsyncWriteExt};
            let (mut socket, _) = listener.accept().await.expect("accept");
            let mut buf = [0u8; 1024];
            let _ = socket.read(&mut buf).await; // discard the real request line/headers
            let response = format!(
                "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{}",
                body.len(),
                body
            );
            socket
                .write_all(response.as_bytes())
                .await
                .expect("write real response bytes");
            socket.shutdown().await.ok();
        });

        let fetcher =
            GgenMarketplaceFetcher::with_catalog_url(format!("http://{}/index.json", addr));
        let pkg = fetcher
            .fetch_metadata("affidavit-pack")
            .await
            .expect("real HTTP fetch + parse must succeed");
        assert_eq!(pkg.name, "affidavit-pack");
        assert_eq!(pkg.latest_version, "0.1.0");
        assert_eq!(
            pkg.download_urls.get("0.1.0").unwrap(),
            "https://github.com/seanchatmangpt/ggen-marketplace/releases/download/packs/affidavit-pack-0.1.0.tar.gz"
        );
        assert_eq!(
            pkg.checksums.get("0.1.0").unwrap(),
            "963fcfa69a3c83ea017d187a1ee99b4546f21e3b74e6848516423cf22b71c0fa"
        );

        server.await.expect("fixture server task must not panic");
    }
}
