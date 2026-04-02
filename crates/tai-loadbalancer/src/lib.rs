#![deny(unsafe_code)]
#![warn(missing_docs)]

//! # tai-loadbalancer: Production-Grade Load Balancing & Service Discovery
//!
//! This crate provides comprehensive load balancing and service discovery for gRPC microservices:
//!
//! - **Dynamic Service Discovery**: Register and deregister endpoints with automatic updates
//! - **Multiple Load Balancing Strategies**: Round-robin, least-connections, hash-based (consistent hashing)
//! - **Health-Aware Routing**: Periodic health checks, automatic unhealthy instance exclusion
//! - **Connection Pooling**: Configurable pool sizes with backpressure handling
//! - **Session Affinity**: Sticky routing for stateful services (user-based, cookie-based)
//! - **Automatic Rebalancing**: Dynamic instance topology changes with gradual rebalancing
//! - **Backup/Failover**: Secondary instances with automatic switchover
//! - **Circuit Breaker Integration**: Works seamlessly with tai-resilience
//! - **Observable Metrics**: Prometheus metrics for load distribution, failures, latency
//!
//! ## Architecture
//!
//! ```text
//! Client
//!   ↓
//! Load Balancer (routing decision)
//!   ↓ (health check)
//! Service Registry (endpoint metadata)
//!   ↓ (selection strategy)
//! Connection Pool (connection management)
//!   ↓
//! Healthy Backend Instances
//! ```
//!
//! ## Example: Basic Load Balancing
//!
//! ```no_run
//! use tai_loadbalancer::{
//!     LoadBalancer, LoadBalancingStrategy, ServiceRegistry, Endpoint
//! };
//! use std::net::SocketAddr;
//!
//! #[tokio::main]
//! async fn main() -> Result<(), Box<dyn std::error::Error>> {
//!     // Create service registry
//!     let registry = ServiceRegistry::new();
//!
//!     // Register endpoints
//!     registry.register(
//!         "payment-service".to_string(),
//!         Endpoint::new("127.0.0.1:5001".parse()?, None),
//!     ).await?;
//!
//!     // Create load balancer with round-robin strategy
//!     let lb = LoadBalancer::new(
//!         registry,
//!         LoadBalancingStrategy::RoundRobin,
//!     );
//!
//!     // Get next endpoint
//!     let endpoint = lb.next_endpoint("payment-service").await?;
//!     println!("Selected endpoint: {}", endpoint.address);
//!
//!     Ok(())
//! }
//! ```

pub mod affinity;
pub mod connection_pool;
pub mod error;
pub mod failover;
pub mod health_check;
pub mod metrics;
pub mod rebalance;
pub mod service_registry;
pub mod strategy;

pub use affinity::{AffinityConfig, AffinityManager, AffinityStrategy};
pub use connection_pool::{ConnectionPool, ConnectionPoolConfig};
pub use error::{Error, Result};
pub use failover::{FailoverConfig, FailoverManager};
pub use health_check::{HealthCheck, HealthCheckConfig, HealthCheckManager, HealthStatus};
pub use metrics::LoadBalancerMetrics;
pub use rebalance::{RebalanceConfig, RebalanceManager, RebalanceStrategy};
pub use service_registry::{Endpoint, ServiceRegistry};
pub use strategy::{LoadBalancer, LoadBalancingStrategy};

use parking_lot::RwLock;
use std::sync::Arc;

/// Main load balancer configuration
#[derive(Debug, Clone)]
pub struct LoadBalancerConfig {
    /// Connection pool configuration
    pub pool_config: ConnectionPoolConfig,
    /// Health check configuration
    pub health_check_config: HealthCheckConfig,
    /// Affinity configuration
    pub affinity_config: AffinityConfig,
    /// Failover configuration
    pub failover_config: FailoverConfig,
    /// Rebalance configuration
    pub rebalance_config: RebalanceConfig,
}

impl Default for LoadBalancerConfig {
    fn default() -> Self {
        Self {
            pool_config: ConnectionPoolConfig::default(),
            health_check_config: HealthCheckConfig::default(),
            affinity_config: AffinityConfig::default(),
            failover_config: FailoverConfig::default(),
            rebalance_config: RebalanceConfig::default(),
        }
    }
}

/// Unified load balancer with all integrated components
pub struct IntegratedLoadBalancer {
    strategy: Arc<LoadBalancer>,
    health_manager: Arc<HealthCheckManager>,
    connection_pool: Arc<ConnectionPool>,
    affinity_manager: Arc<AffinityManager>,
    failover_manager: Arc<FailoverManager>,
    rebalance_manager: Arc<RebalanceManager>,
    metrics: Arc<RwLock<LoadBalancerMetrics>>,
}

impl IntegratedLoadBalancer {
    /// Create a new integrated load balancer
    pub async fn new(
        registry: Arc<ServiceRegistry>,
        strategy: LoadBalancingStrategy,
        config: LoadBalancerConfig,
    ) -> Result<Self> {
        let lb_strategy = Arc::new(LoadBalancer::new(registry.clone(), strategy));
        let health_manager =
            Arc::new(HealthCheckManager::new(registry.clone(), config.health_check_config).await?);
        let connection_pool = Arc::new(ConnectionPool::new(config.pool_config));
        let affinity_manager = Arc::new(AffinityManager::new(config.affinity_config));
        let failover_manager = Arc::new(FailoverManager::new(config.failover_config));
        let rebalance_manager = Arc::new(RebalanceManager::new(registry.clone(), config.rebalance_config).await?);

        Ok(Self {
            strategy: lb_strategy,
            health_manager,
            connection_pool,
            affinity_manager,
            failover_manager,
            rebalance_manager,
            metrics: Arc::new(RwLock::new(LoadBalancerMetrics::default())),
        })
    }

    /// Start all background processes
    pub async fn start(&self) -> Result<()> {
        self.health_manager.start().await?;
        self.rebalance_manager.start().await?;
        Ok(())
    }

    /// Stop all background processes
    pub async fn stop(&self) -> Result<()> {
        self.health_manager.stop().await?;
        self.rebalance_manager.stop().await?;
        Ok(())
    }

    /// Get next healthy endpoint for a service
    pub async fn next_endpoint(&self, service: &str) -> Result<Endpoint> {
        self.strategy.next_endpoint(service).await
    }

    /// Get next healthy endpoint with session affinity
    pub async fn next_endpoint_with_affinity(
        &self,
        service: &str,
        affinity_key: &str,
    ) -> Result<Endpoint> {
        // Check affinity cache first
        if let Some(endpoint) = self
            .affinity_manager
            .get_affinity(service, affinity_key)
            .await
        {
            // Verify endpoint is still healthy
            if self
                .health_manager
                .is_healthy(service, &endpoint)
                .await?
            {
                return Ok(endpoint);
            }
        }

        // Get next endpoint and store affinity
        let endpoint = self.strategy.next_endpoint(service).await?;
        self.affinity_manager
            .set_affinity(service, affinity_key, endpoint.clone())
            .await;
        Ok(endpoint)
    }

    /// Get metrics
    pub fn metrics(&self) -> LoadBalancerMetrics {
        self.metrics.read().clone()
    }
}
