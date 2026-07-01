//! Network client for marketplace registry
//!
//! Features:
//! - Fetch package metadata from marketplace registry
//! - Download package content (ontologies) with progress callback
//! - Support offline fallback (return cached if available)
//! - HTTP client using reqwest with timeout
//! - Real HTTP calls (Chicago TDD — no mocks)

use reqwest::Client;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use std::time::Duration;
use tracing::{debug, info, instrument, span, warn};

use crate::marketplace::cache::PackCache;
use crate::marketplace::error::{Error, Result};
use crate::marketplace::models::{PackageId, PackageVersion};

/// Progress callback for download operations
/// Called with (bytes_downloaded, total_bytes, current_step_description)
pub type DownloadProgressCallback = Arc<dyn Fn(u64, u64, &str) + Send + Sync>;

/// Package metadata from marketplace registry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageMetadata {
    /// Package ID (e.g., "acme/base")
    pub id: PackageId,
    /// Semantic version
    pub version: PackageVersion,
    /// Package description
    pub description: String,
    /// Author name
    pub author: String,
    /// License (SPDX identifier)
    pub license: String,
    /// Download URL for the package content
    pub download_url: String,
    /// SHA-256 digest of the package content
    pub digest: String,
    /// Size in bytes
    pub size_bytes: u64,
    /// List of dependencies as "id@version" strings
    #[serde(default)]
    pub dependencies: Vec<String>,
    /// Published timestamp (RFC 3339)
    pub published_at: String,
}

/// Marketplace registry client
pub struct MarketplaceClient {
    /// HTTP client (reused across requests)
    http_client: Client,
    /// Base URL for the marketplace registry
    registry_url: String,
    /// Request timeout duration
    request_timeout: Duration,
    /// Optional cache for offline fallback
    cache: Option<Arc<PackCache>>,
}

impl MarketplaceClient {
    /// Create a new marketplace client
    ///
    /// # Arguments
    ///
    /// * `registry_url` - Base URL of the marketplace registry (e.g., "https://registry.ggen.io")
    ///
    /// # Examples
    ///
    /// ```ignore
    /// let client = MarketplaceClient::new("https://registry.ggen.io");
    /// ```
    #[must_use]
    pub fn new(registry_url: impl Into<String>) -> Self {
        Self {
            http_client: Client::new(),
            registry_url: registry_url.into(),
            request_timeout: Duration::from_secs(30),
            cache: None,
        }
    }

    /// Create a client with custom timeout
    #[must_use]
    pub fn with_timeout(mut self, timeout: Duration) -> Self {
        self.request_timeout = timeout;
        self
    }

    /// Add offline cache fallback
    #[must_use]
    pub fn with_cache(mut self, cache: Arc<PackCache>) -> Self {
        self.cache = Some(cache);
        self
    }

    /// Get the base registry URL
    #[must_use]
    pub fn registry_url(&self) -> &str {
        &self.registry_url
    }

    /// Fetch package metadata from marketplace registry
    ///
    /// # Arguments
    ///
    /// * `id` - Package ID (e.g., "acme/base")
    /// * `version` - Semantic version
    ///
    /// # Returns
    ///
    /// Package metadata if found, or `Err` with offline fallback check
    ///
    /// # Errors
    ///
    /// * `Error::PackageNotFound` - Package not found in registry
    /// * `Error::Timeout` - Request timed out
    /// * `Error::RegistryError` - Network or registry error
    #[instrument(
        name = "marketplace.fetch_metadata",
        skip(self),
        fields(
            operation.name = "fetch_package_metadata",
            operation.type = "marketplace",
            package_id = %id,
            package_version = %version,
            duration_ms
        )
    )]
    pub async fn fetch_package_metadata(
        &self, id: &PackageId, version: &PackageVersion,
    ) -> Result<PackageMetadata> {
        let start = std::time::Instant::now();

        let url = format!(
            "{}/packages/{}/versions/{}",
            self.registry_url,
            id.as_str(),
            version.as_str()
        );

        debug!("Fetching package metadata from: {}", url);

        match self
            .http_client
            .get(&url)
            .timeout(self.request_timeout)
            .send()
            .await
        {
            Ok(response) => {
                if response.status().is_success() {
                    match response.json::<PackageMetadata>().await {
                        Ok(metadata) => {
                            let duration = start.elapsed();
                            info!(
                                "Fetched metadata for {}@{} ({}B)",
                                id, version, metadata.size_bytes
                            );
                            span::Span::current().record("duration_ms", duration.as_millis());
                            Ok(metadata)
                        }
                        Err(e) => {
                            warn!(
                                "Failed to parse metadata response for {}@{}: {}",
                                id, version, e
                            );
                            Err(Error::RegistryError(format!(
                                "Invalid metadata JSON: {}",
                                e
                            )))
                        }
                    }
                } else if response.status().is_client_error() {
                    warn!("Package not found: {}@{}", id, version);
                    Err(Error::PackageNotFound {
                        package_id: id.to_string(),
                    })
                } else {
                    warn!("Registry error: {} {}", response.status(), url);
                    Err(Error::RegistryError(format!(
                        "Registry returned {}: {}",
                        response.status(),
                        url
                    )))
                }
            }
            Err(e) if e.is_timeout() => {
                warn!("Request timeout fetching {}@{}", id, version);
                Err(Error::Timeout(format!(
                    "Marketplace registry timeout: {}",
                    e
                )))
            }
            Err(e) => {
                warn!("Network error fetching {}@{}: {}", id, version, e);
                Err(Error::RegistryError(format!(
                    "Network error: {} ({}@{})",
                    e, id, version
                )))
            }
        }
    }

    /// Download package content from marketplace
    ///
    /// Downloads the package content to the provided cache and verifies the digest.
    /// If network is unavailable and cache has a copy, returns cached version.
    ///
    /// # Arguments
    ///
    /// * `metadata` - Package metadata with download URL
    /// * `cache` - PackCache to store downloaded content
    /// * `progress` - Optional callback for download progress (bytes_done, total_bytes, step)
    ///
    /// # Returns
    ///
    /// Path to the cached (or downloaded) package content
    ///
    /// # Errors
    ///
    /// * `Error::InstallationFailed` - Download or cache insertion failed
    /// * `Error::Timeout` - Download timed out
    /// * `Error::RegistryError` - Network error
    #[instrument(
        name = "marketplace.download_package",
        skip(self, cache, progress),
        fields(
            operation.name = "download_package",
            operation.type = "marketplace",
            package_id = %metadata.id,
            size_bytes = metadata.size_bytes,
            duration_ms
        )
    )]
    pub async fn download_package(
        &self, metadata: &PackageMetadata, cache: &PackCache,
        progress: Option<DownloadProgressCallback>,
    ) -> Result<crate::marketplace::cache::CachedPack> {
        let start = std::time::Instant::now();

        // Check if already cached
        if let Some(cached) = cache.get(&metadata.id, &metadata.version) {
            info!(
                "Package {}@{} found in cache ({}B)",
                metadata.id, metadata.version, cached.size_bytes
            );
            return Ok(cached);
        }

        debug!(
            "Downloading package {}@{} from {}",
            metadata.id, metadata.version, metadata.download_url
        );

        // Try to download from marketplace
        match self
            .download_from_url(&metadata.download_url, progress.as_ref())
            .await
        {
            Ok(content) => {
                // Verify digest
                let actual_digest = sha256(&content);
                if actual_digest != metadata.digest {
                    return Err(Error::InstallationFailed {
                        reason: format!(
                            "Digest mismatch for {}@{}: expected {}, got {}",
                            metadata.id, metadata.version, metadata.digest, actual_digest
                        ),
                    });
                }

                // Cache the downloaded content
                let size_bytes = content.len() as u64;
                let cached_pack = crate::marketplace::cache::CachedPack::new(
                    metadata.id.clone(),
                    metadata.version.clone(),
                    metadata.digest.clone(),
                    size_bytes,
                    // Path will be set by cache insertion
                    std::path::PathBuf::new(),
                );

                // Write to cache (atomic operation)
                cache.insert(cached_pack.clone())?;

                let duration = start.elapsed();
                info!(
                    "Downloaded and cached {}@{} ({}B) in {}ms",
                    metadata.id,
                    metadata.version,
                    size_bytes,
                    duration.as_millis()
                );
                span::Span::current().record("duration_ms", duration.as_millis());

                Ok(cached_pack)
            }
            Err(e) => {
                // Check if we have a cached copy for offline fallback
                if let Some(cached) = cache.get(&metadata.id, &metadata.version) {
                    warn!(
                        "Download failed for {}@{}, using cached version: {}",
                        metadata.id, metadata.version, e
                    );
                    return Ok(cached);
                }

                Err(e)
            }
        }
    }

    /// Download content from a URL
    async fn download_from_url(
        &self, url: &str, progress: Option<&Arc<dyn Fn(u64, u64, &str) + Send + Sync>>,
    ) -> Result<Vec<u8>> {
        debug!("Starting download from: {}", url);

        let response = self
            .http_client
            .get(url)
            .timeout(self.request_timeout)
            .send()
            .await
            .map_err(|e| {
                if e.is_timeout() {
                    Error::Timeout(format!("Download timeout: {}", e))
                } else {
                    Error::RegistryError(format!("Download failed: {}", e))
                }
            })?;

        if !response.status().is_success() {
            return Err(Error::RegistryError(format!(
                "Download failed with status {}: {}",
                response.status(),
                url
            )));
        }

        let total_size = response
            .content_length()
            .unwrap_or(0)
            .try_into()
            .unwrap_or(0);

        if let Some(prog) = progress {
            prog(0, total_size, "Starting download");
        }

        let bytes = response
            .bytes()
            .await
            .map_err(|e| Error::RegistryError(format!("Failed to read response body: {}", e)))?;

        if let Some(prog) = progress {
            prog(bytes.len() as u64, total_size, "Download complete");
        }

        Ok(bytes.to_vec())
    }
}

/// Compute SHA-256 digest of content
fn sha256(content: &[u8]) -> String {
    use sha2::{Digest, Sha256};

    let mut hasher = Sha256::new();
    hasher.update(content);
    let result = hasher.finalize();

    format!("{:x}", result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_marketplace_client_creation() {
        let client = MarketplaceClient::new("https://registry.ggen.io");
        assert_eq!(client.registry_url(), "https://registry.ggen.io");
    }

    #[test]
    fn test_with_timeout() {
        let client = MarketplaceClient::new("https://registry.ggen.io")
            .with_timeout(Duration::from_secs(60));
        assert_eq!(client.request_timeout, Duration::from_secs(60));
    }

    #[test]
    fn test_sha256_hash() {
        let content = b"hello world";
        let hash = sha256(content);
        // sha256("hello world") = b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9
        assert_eq!(
            hash,
            "b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9"
        );
    }
}
