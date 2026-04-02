//! Service registry for dynamic endpoint management

use crate::error::{Error, Result};
use dashmap::DashMap;
use serde::{Deserialize, Serialize};
use std::net::SocketAddr;
use std::sync::Arc;

/// Represents a single backend endpoint
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Endpoint {
    /// Network address
    pub address: SocketAddr,
    /// Optional metadata (region, version, labels)
    pub metadata: Option<EndpointMetadata>,
    /// Optional weight for weighted load balancing
    pub weight: Option<u32>,
}

impl Endpoint {
    /// Create a new endpoint
    pub fn new(address: SocketAddr, metadata: Option<EndpointMetadata>) -> Self {
        Self {
            address,
            metadata,
            weight: None,
        }
    }

    /// Create endpoint with weight
    pub fn with_weight(address: SocketAddr, metadata: Option<EndpointMetadata>, weight: u32) -> Self {
        Self {
            address,
            metadata,
            weight: Some(weight),
        }
    }

    /// Set metadata
    pub fn with_metadata(mut self, metadata: EndpointMetadata) -> Self {
        self.metadata = Some(metadata);
        self
    }
}

/// Endpoint metadata for advanced routing decisions
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct EndpointMetadata {
    /// Geographic region
    pub region: Option<String>,
    /// Service version
    pub version: Option<String>,
    /// Custom labels (e.g., "zone=us-west-2a", "tier=gold")
    pub labels: Option<Vec<(String, String)>>,
}

impl EndpointMetadata {
    /// Create new metadata
    pub fn new() -> Self {
        Self {
            region: None,
            version: None,
            labels: None,
        }
    }

    /// Set region
    pub fn with_region(mut self, region: impl Into<String>) -> Self {
        self.region = Some(region.into());
        self
    }

    /// Set version
    pub fn with_version(mut self, version: impl Into<String>) -> Self {
        self.version = Some(version.into());
        self
    }

    /// Add label
    pub fn with_label(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        if self.labels.is_none() {
            self.labels = Some(Vec::new());
        }
        if let Some(ref mut labels) = self.labels {
            labels.push((key.into(), value.into()));
        }
        self
    }
}

impl Default for EndpointMetadata {
    fn default() -> Self {
        Self::new()
    }
}

/// Dynamic service registry
#[derive(Debug)]
pub struct ServiceRegistry {
    /// Map of service name → list of endpoints
    services: Arc<DashMap<String, Vec<Endpoint>>>,
}

impl ServiceRegistry {
    /// Create a new service registry
    pub fn new() -> Self {
        Self {
            services: Arc::new(DashMap::new()),
        }
    }

    /// Register an endpoint for a service
    pub async fn register(&self, service: String, endpoint: Endpoint) -> Result<()> {
        self.services
            .entry(service)
            .or_insert_with(Vec::new)
            .push(endpoint);
        Ok(())
    }

    /// Register multiple endpoints at once
    pub async fn register_batch(&self, service: String, endpoints: Vec<Endpoint>) -> Result<()> {
        if endpoints.is_empty() {
            return Err(Error::invalid_config("Cannot register empty endpoint list"));
        }

        self.services.insert(service, endpoints);
        Ok(())
    }

    /// Deregister an endpoint from a service
    pub async fn deregister(&self, service: &str, endpoint: &Endpoint) -> Result<()> {
        if let Some(mut endpoints) = self.services.get_mut(service) {
            endpoints.retain(|e| e != endpoint);
            if endpoints.is_empty() {
                drop(endpoints);
                self.services.remove(service);
            }
        }
        Ok(())
    }

    /// Get all endpoints for a service
    pub async fn get_endpoints(&self, service: &str) -> Result<Vec<Endpoint>> {
        self.services
            .get(service)
            .map(|entry| entry.value().clone())
            .ok_or_else(|| Error::service_not_found(service))
    }

    /// Check if service has any endpoints
    pub async fn has_service(&self, service: &str) -> bool {
        self.services.contains_key(service)
    }

    /// Get list of registered services
    pub async fn list_services(&self) -> Vec<String> {
        self.services.iter().map(|entry| entry.key().clone()).collect()
    }

    /// Update endpoints for a service (replace all endpoints)
    pub async fn update_endpoints(&self, service: String, endpoints: Vec<Endpoint>) -> Result<()> {
        if endpoints.is_empty() {
            return Err(Error::invalid_config("Cannot update with empty endpoint list"));
        }
        self.services.insert(service, endpoints);
        Ok(())
    }

    /// Clear all services
    pub async fn clear(&self) {
        self.services.clear();
    }

    /// Get endpoint count for a service
    pub async fn endpoint_count(&self, service: &str) -> Result<usize> {
        self.services
            .get(service)
            .map(|entry| entry.len())
            .ok_or_else(|| Error::service_not_found(service))
    }

    /// Find endpoints matching metadata filters
    pub async fn find_matching(
        &self,
        service: &str,
        region: Option<&str>,
        version: Option<&str>,
    ) -> Result<Vec<Endpoint>> {
        let endpoints = self.get_endpoints(service).await?;

        Ok(endpoints
            .into_iter()
            .filter(|ep| {
                if let Some(meta) = &ep.metadata {
                    // Check region match
                    if let Some(r) = region {
                        if meta.region.as_ref().map(|v| v.as_str()) != Some(r) {
                            return false;
                        }
                    }
                    // Check version match
                    if let Some(v) = version {
                        if meta.version.as_ref().map(|ver| ver.as_str()) != Some(v) {
                            return false;
                        }
                    }
                    true
                } else {
                    region.is_none() && version.is_none()
                }
            })
            .collect())
    }
}

impl Default for ServiceRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_register_and_get_endpoints() {
        let registry = ServiceRegistry::new();
        let addr: SocketAddr = "127.0.0.1:5001".parse().unwrap();
        let endpoint = Endpoint::new(addr, None);

        registry.register("test-service".to_string(), endpoint.clone()).await.unwrap();
        let endpoints = registry.get_endpoints("test-service").await.unwrap();
        assert_eq!(endpoints.len(), 1);
        assert_eq!(endpoints[0], endpoint);
    }

    #[tokio::test]
    async fn test_deregister_endpoint() {
        let registry = ServiceRegistry::new();
        let addr: SocketAddr = "127.0.0.1:5001".parse().unwrap();
        let endpoint = Endpoint::new(addr, None);

        registry.register("test-service".to_string(), endpoint.clone()).await.unwrap();
        registry.deregister("test-service", &endpoint).await.unwrap();
        let result = registry.get_endpoints("test-service").await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_service_not_found() {
        let registry = ServiceRegistry::new();
        let result = registry.get_endpoints("nonexistent").await;
        assert!(matches!(result, Err(Error::ServiceNotFound(_))));
    }

    #[tokio::test]
    async fn test_register_batch() {
        let registry = ServiceRegistry::new();
        let endpoints = vec![
            Endpoint::new("127.0.0.1:5001".parse().unwrap(), None),
            Endpoint::new("127.0.0.1:5002".parse().unwrap(), None),
            Endpoint::new("127.0.0.1:5003".parse().unwrap(), None),
        ];

        registry.register_batch("test-service".to_string(), endpoints.clone()).await.unwrap();
        let retrieved = registry.get_endpoints("test-service").await.unwrap();
        assert_eq!(retrieved.len(), 3);
    }
}
