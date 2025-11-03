use crate::error::{MarketplaceError, Result};
use crate::models::{Package, PackageId, Query, RegistryMetadata};
use crate::traits::Registry;
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::time::Duration;

/// Centralized HTTP/HTTPS registry backend
///
/// Connects to a remote marketplace registry via HTTP(S) and provides
/// package discovery, retrieval, and management capabilities.
///
/// # Example
///
/// ```no_run
/// use ggen_marketplace::backend::CentralizedRegistry;
/// use ggen_marketplace::traits::Registry;
///
/// # async fn example() -> Result<(), Box<dyn std::error::Error>> {
/// let registry = CentralizedRegistry::new("https://marketplace.ggen.dev")?;
/// let packages = registry.search(&Query::new("rust web")).await?;
/// # Ok(())
/// # }
/// ```
pub struct CentralizedRegistry {
    base_url: String,
    client: reqwest::Client,
    cache_ttl: Duration,
}

impl CentralizedRegistry {
    /// Create a new centralized registry client
    ///
    /// # Arguments
    ///
    /// * `base_url` - The base URL of the registry (e.g., "https://marketplace.ggen.dev")
    ///
    /// # Errors
    ///
    /// Returns an error if the HTTP client cannot be initialized.
    pub fn new(base_url: impl Into<String>) -> Result<Self> {
        let client = reqwest::Client::builder()
            .timeout(Duration::from_secs(30))
            .connect_timeout(Duration::from_secs(10))
            .pool_idle_timeout(Duration::from_secs(90))
            .pool_max_idle_per_host(10)
            .user_agent(concat!(
                env!("CARGO_PKG_NAME"),
                "/",
                env!("CARGO_PKG_VERSION")
            ))
            .build()
            .map_err(|e| MarketplaceError::network_error(format!("registry client: {}", e)))?;

        Ok(Self {
            base_url: base_url.into(),
            client,
            cache_ttl: Duration::from_secs(300), // 5 minutes default
        })
    }

    /// Set the cache TTL for registry responses
    pub fn with_cache_ttl(mut self, ttl: Duration) -> Self {
        self.cache_ttl = ttl;
        self
    }

    /// Fetch the registry index
    async fn fetch_index(&self) -> Result<RegistryIndex> {
        let url = format!("{}/api/v1/index", self.base_url);

        let response = self
            .client
            .get(&url)
            .send()
            .await
            .map_err(|e| MarketplaceError::network_error(format!("{} ({})", e, url)))?;

        if !response.status().is_success() {
            return Err(MarketplaceError::registry_error(
                format!("HTTP {}", response.status()),
                &url,
            ));
        }

        response
            .json::<RegistryIndex>()
            .await
            .map_err(|e| MarketplaceError::parse_error(format!("registry index: {}", e)))
    }

    /// Search packages with retry logic
    async fn search_with_retry(&self, query: &Query, retries: usize) -> Result<Vec<Package>> {
        let mut last_error = None;

        for attempt in 0..=retries {
            if attempt > 0 {
                // Exponential backoff
                let delay = Duration::from_millis(100 * 2u64.pow(attempt as u32));
                tokio::time::sleep(delay).await;
            }

            match self.search_internal(query).await {
                Ok(packages) => return Ok(packages),
                Err(e) => last_error = Some(e),
            }
        }

        Err(last_error
            .unwrap_or_else(|| MarketplaceError::network_error("search: all retries failed")))
    }

    /// Internal search implementation
    async fn search_internal(&self, query: &Query) -> Result<Vec<Package>> {
        let url = format!("{}/api/v1/packages/search", self.base_url);

        let response = self
            .client
            .get(&url)
            .query(&[("q", &query.text)])
            .send()
            .await
            .map_err(|e| MarketplaceError::network_error(format!("{} ({})", e, url)))?;

        if !response.status().is_success() {
            return Err(MarketplaceError::registry_error(
                format!("HTTP {}", response.status()),
                &url,
            ));
        }

        let search_response: SearchResponse = response
            .json()
            .await
            .map_err(|e| MarketplaceError::parse_error(format!("search response: {}", e)))?;

        Ok(search_response.packages)
    }
}

#[async_trait]
impl Registry for CentralizedRegistry {
    async fn search(&self, query: &Query) -> Result<Vec<Package>> {
        self.search_with_retry(query, 3).await
    }

    async fn get_package(&self, id: &PackageId) -> Result<Package> {
        let url = format!(
            "{}/api/v1/packages/{}/{}",
            self.base_url, id.namespace, id.name
        );

        let response = self
            .client
            .get(&url)
            .send()
            .await
            .map_err(|e| MarketplaceError::network_error(format!("{} ({})", e, url)))?;

        if response.status() == reqwest::StatusCode::NOT_FOUND {
            return Err(MarketplaceError::not_found(format!("{}", &id)));
        }

        if !response.status().is_success() {
            return Err(MarketplaceError::registry_error(
                format!("HTTP {}", response.status()),
                &url,
            ));
        }

        response
            .json::<Package>()
            .await
            .map_err(|e| MarketplaceError::parse_error(format!("package: {}", e)))
    }

    async fn get_package_version(&self, id: &PackageId, version: &str) -> Result<Package> {
        let url = format!(
            "{}/api/v1/packages/{}/{}/versions/{}",
            self.base_url, id.namespace, id.name, version
        );

        let response = self
            .client
            .get(&url)
            .send()
            .await
            .map_err(|e| MarketplaceError::network_error(format!("{} ({})", e, url)))?;

        if response.status() == reqwest::StatusCode::NOT_FOUND {
            return Err(MarketplaceError::not_found(format!(
                "package version {}@{}",
                id, version
            )));
        }

        if !response.status().is_success() {
            return Err(MarketplaceError::registry_error(
                format!("HTTP {}", response.status()),
                &url,
            ));
        }

        response
            .json::<Package>()
            .await
            .map_err(|e| MarketplaceError::parse_error(format!("package version: {}", e)))
    }

    async fn list_versions(&self, id: &PackageId) -> Result<Vec<Package>> {
        let url = format!(
            "{}/api/v1/packages/{}/{}/versions",
            self.base_url, id.namespace, id.name
        );

        let response = self
            .client
            .get(&url)
            .send()
            .await
            .map_err(|e| MarketplaceError::network_error(format!("{} ({})", e, url)))?;

        if response.status() == reqwest::StatusCode::NOT_FOUND {
            return Err(MarketplaceError::not_found(format!("{}", &id)));
        }

        if !response.status().is_success() {
            return Err(MarketplaceError::registry_error(
                format!("HTTP {}", response.status()),
                &url,
            ));
        }

        let versions_response: VersionsResponse = response
            .json()
            .await
            .map_err(|e| MarketplaceError::parse_error(format!("versions list: {}", e)))?;

        Ok(versions_response.versions)
    }

    async fn publish(&self, package: Package) -> Result<()> {
        let url = format!("{}/api/v1/packages", self.base_url);

        let response = self
            .client
            .post(&url)
            .json(&package)
            .send()
            .await
            .map_err(|e| MarketplaceError::network_error(format!("{} ({})", e, url)))?;

        if !response.status().is_success() {
            return Err(MarketplaceError::registry_error(
                format!(
                    "HTTP {}: {}",
                    response.status(),
                    response.text().await.unwrap_or_default()
                ),
                &url,
            ));
        }

        Ok(())
    }

    async fn delete(&self, id: &PackageId, version: &str) -> Result<()> {
        let url = format!(
            "{}/api/v1/packages/{}/{}/versions/{}",
            self.base_url, id.namespace, id.name, version
        );

        let response = self
            .client
            .delete(&url)
            .send()
            .await
            .map_err(|e| MarketplaceError::network_error(format!("{} ({})", e, url)))?;

        if response.status() == reqwest::StatusCode::NOT_FOUND {
            return Err(MarketplaceError::not_found(format!(
                "package version {}@{}",
                id, version
            )));
        }

        if !response.status().is_success() {
            return Err(MarketplaceError::registry_error(
                format!("HTTP {}", response.status()),
                &url,
            ));
        }

        Ok(())
    }

    async fn exists(&self, id: &PackageId) -> Result<bool> {
        let url = format!(
            "{}/api/v1/packages/{}/{}",
            self.base_url, id.namespace, id.name
        );

        let response = self
            .client
            .head(&url)
            .send()
            .await
            .map_err(|e| MarketplaceError::network_error(format!("{} ({})", e, url)))?;

        Ok(response.status().is_success())
    }

    async fn metadata(&self) -> Result<RegistryMetadata> {
        let url = format!("{}/api/v1/metadata", self.base_url);

        let response = self
            .client
            .get(&url)
            .send()
            .await
            .map_err(|e| MarketplaceError::network_error(format!("{} ({})", e, url)))?;

        if !response.status().is_success() {
            return Err(MarketplaceError::registry_error(
                format!("HTTP {}", response.status()),
                &url,
            ));
        }

        response
            .json::<RegistryMetadata>()
            .await
            .map_err(|e| MarketplaceError::parse_error(format!("registry metadata: {}", e)))
    }
}

/// Registry index structure
#[derive(Debug, Clone, Serialize, Deserialize)]
struct RegistryIndex {
    pub version: String,
    pub packages: Vec<PackageEntry>,
}

/// Package entry in the index
#[derive(Debug, Clone, Serialize, Deserialize)]
struct PackageEntry {
    pub id: PackageId,
    pub latest_version: String,
}

/// Search response from the API
#[derive(Debug, Clone, Serialize, Deserialize)]
struct SearchResponse {
    pub packages: Vec<Package>,
    pub total: usize,
}

/// Versions list response
#[derive(Debug, Clone, Serialize, Deserialize)]
struct VersionsResponse {
    pub versions: Vec<Package>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_registry() {
        let registry = CentralizedRegistry::new("https://marketplace.ggen.dev");
        assert!(registry.is_ok());
    }

    #[test]
    fn test_with_cache_ttl() {
        let registry = CentralizedRegistry::new("https://marketplace.ggen.dev")
            .unwrap()
            .with_cache_ttl(Duration::from_secs(600));

        assert_eq!(registry.cache_ttl, Duration::from_secs(600));
    }
}
