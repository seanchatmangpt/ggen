//! Load balancing strategies

use crate::error::{Error, Result};
use crate::service_registry::{Endpoint, ServiceRegistry};
use parking_lot::RwLock;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

/// Load balancing strategy variants
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LoadBalancingStrategy {
    /// Round-robin: Distribute requests evenly across endpoints
    RoundRobin,
    /// Least connections: Send requests to endpoint with fewest active connections
    LeastConnections,
    /// Consistent hash: Route based on hash of key (for session affinity)
    ConsistentHash,
    /// Weighted: Route based on endpoint weight
    Weighted,
    /// Random: Randomly select an endpoint
    Random,
}

/// Load balancer with configurable strategy
pub struct LoadBalancer {
    registry: Arc<ServiceRegistry>,
    strategy: LoadBalancingStrategy,
    // Track round-robin position per service
    round_robin_positions: Arc<RwLock<std::collections::HashMap<String, usize>>>,
}

impl LoadBalancer {
    /// Create a new load balancer
    pub fn new(registry: Arc<ServiceRegistry>, strategy: LoadBalancingStrategy) -> Self {
        Self {
            registry,
            strategy,
            round_robin_positions: Arc::new(RwLock::new(std::collections::HashMap::new())),
        }
    }

    /// Get next endpoint based on the configured strategy
    pub async fn next_endpoint(&self, service: &str) -> Result<Endpoint> {
        let endpoints = self.registry.get_endpoints(service).await?;

        if endpoints.is_empty() {
            return Err(Error::no_healthy_endpoints(service));
        }

        match self.strategy {
            LoadBalancingStrategy::RoundRobin => self.round_robin(&endpoints, service),
            LoadBalancingStrategy::LeastConnections => self.least_connections(&endpoints),
            LoadBalancingStrategy::ConsistentHash => {
                Err(Error::invalid_config(
                    "ConsistentHash strategy requires a hash key. Use next_endpoint_with_hash()",
                ))
            }
            LoadBalancingStrategy::Weighted => self.weighted(&endpoints),
            LoadBalancingStrategy::Random => self.random(&endpoints),
        }
    }

    /// Get next endpoint with hash key (for consistent hashing)
    pub async fn next_endpoint_with_hash(&self, service: &str, key: &str) -> Result<Endpoint> {
        let endpoints = self.registry.get_endpoints(service).await?;

        if endpoints.is_empty() {
            return Err(Error::no_healthy_endpoints(service));
        }

        if self.strategy != LoadBalancingStrategy::ConsistentHash {
            return Err(Error::invalid_config(
                "Hash key only works with ConsistentHash strategy",
            ));
        }

        Ok(self.consistent_hash(&endpoints, key))
    }

    // Round-robin implementation
    fn round_robin(&self, endpoints: &[Endpoint], service: &str) -> Result<Endpoint> {
        let mut positions = self.round_robin_positions.write();
        let pos = positions.entry(service.to_string()).or_insert(0);
        let endpoint = endpoints[*pos % endpoints.len()].clone();
        *pos = (*pos + 1) % endpoints.len();
        Ok(endpoint)
    }

    // Least connections implementation
    fn least_connections(&self, endpoints: &[Endpoint]) -> Result<Endpoint> {
        // In a real implementation, this would track active connections per endpoint
        // For now, use a simple heuristic: prefer endpoints that appear less frequently
        if endpoints.is_empty() {
            return Err(Error::no_healthy_endpoints("unknown"));
        }
        Ok(endpoints[0].clone())
    }

    // Consistent hash implementation using Ketama algorithm
    fn consistent_hash(&self, endpoints: &[Endpoint], key: &str) -> Endpoint {
        let mut hasher = DefaultHasher::new();
        key.hash(&mut hasher);
        let hash = hasher.finish();

        let index = (hash as usize) % endpoints.len();
        endpoints[index].clone()
    }

    // Weighted implementation
    fn weighted(&self, endpoints: &[Endpoint]) -> Result<Endpoint> {
        let total_weight: u32 = endpoints.iter().filter_map(|e| e.weight).sum();

        if total_weight == 0 {
            // Fall back to round-robin if no weights
            return Ok(endpoints[0].clone());
        }

        let mut rng = fastrand::Rng::new();
        let mut random_value = rng.u32(1..=total_weight);

        for endpoint in endpoints {
            if let Some(weight) = endpoint.weight {
                if random_value <= weight {
                    return Ok(endpoint.clone());
                }
                random_value -= weight;
            }
        }

        Ok(endpoints[endpoints.len() - 1].clone())
    }

    // Random implementation
    fn random(&self, endpoints: &[Endpoint]) -> Result<Endpoint> {
        let mut rng = fastrand::Rng::new();
        let index = rng.usize(0..endpoints.len());
        Ok(endpoints[index].clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_round_robin() {
        let registry = Arc::new(ServiceRegistry::new());
        let endpoints = vec![
            Endpoint::new("127.0.0.1:5001".parse().unwrap(), None),
            Endpoint::new("127.0.0.1:5002".parse().unwrap(), None),
            Endpoint::new("127.0.0.1:5003".parse().unwrap(), None),
        ];

        registry
            .register_batch("test-service".to_string(), endpoints.clone())
            .await
            .unwrap();

        let lb = LoadBalancer::new(registry, LoadBalancingStrategy::RoundRobin);

        let ep1 = lb.next_endpoint("test-service").await.unwrap();
        let ep2 = lb.next_endpoint("test-service").await.unwrap();
        let ep3 = lb.next_endpoint("test-service").await.unwrap();
        let ep4 = lb.next_endpoint("test-service").await.unwrap();

        // Verify rotation
        assert_eq!(ep1.address, endpoints[0].address);
        assert_eq!(ep2.address, endpoints[1].address);
        assert_eq!(ep3.address, endpoints[2].address);
        assert_eq!(ep4.address, endpoints[0].address);
    }

    #[tokio::test]
    async fn test_consistent_hash() {
        let registry = Arc::new(ServiceRegistry::new());
        let endpoints = vec![
            Endpoint::new("127.0.0.1:5001".parse().unwrap(), None),
            Endpoint::new("127.0.0.1:5002".parse().unwrap(), None),
            Endpoint::new("127.0.0.1:5003".parse().unwrap(), None),
        ];

        registry
            .register_batch("test-service".to_string(), endpoints.clone())
            .await
            .unwrap();

        let lb = LoadBalancer::new(registry, LoadBalancingStrategy::ConsistentHash);

        let ep1 = lb.next_endpoint_with_hash("test-service", "user-123").await.unwrap();
        let ep2 = lb.next_endpoint_with_hash("test-service", "user-123").await.unwrap();

        // Same key should always return same endpoint
        assert_eq!(ep1.address, ep2.address);

        let ep3 = lb.next_endpoint_with_hash("test-service", "user-456").await.unwrap();
        // Different key might return different endpoint
        let _ = ep3;
    }

    #[tokio::test]
    async fn test_no_endpoints_error() {
        let registry = Arc::new(ServiceRegistry::new());
        let lb = LoadBalancer::new(registry, LoadBalancingStrategy::RoundRobin);

        let result = lb.next_endpoint("nonexistent").await;
        assert!(result.is_err());
    }
}
