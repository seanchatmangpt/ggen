//! # tai-resilience: Advanced Service Mesh & Resilience Patterns
//!
//! This crate provides production-grade resilience patterns for microservices deployed on Kubernetes
//! with Istio service mesh. It includes:
//!
//! - **Advanced Circuit Breaker**: Multi-state machine (Closed → Open → Half-Open → Closed)
//!   with configurable failure thresholds, health probes, and slow-start patterns
//! - **Service Mesh Integration**: Istio/Envoy configuration generation for VirtualServices,
//!   DestinationRules, and AuthorizationPolicies
//! - **Traffic Management**: Canary deployments, A/B testing, blue-green deployments,
//!   traffic mirroring, and intelligent routing
//! - **Outlier Detection**: Automatic ejection of unhealthy instances with periodic re-injection
//!
//! ## Architecture
//!
//! The crate follows a layered architecture:
//! - **Circuit Breaker Layer**: Monitors individual services and manages request flow
//! - **Outlier Detection Layer**: Detects and ejects unhealthy instances
//! - **Traffic Management Layer**: Orchestrates safe deployments and traffic splits
//! - **Service Mesh Integration**: Configures Istio resources for Kubernetes deployments
//!
//! ## Example: Advanced Circuit Breaker
//!
//! ```no_run
//! use tai_resilience::circuit_breaker_v2::{CircuitBreakerConfig, CircuitBreaker};
//! use std::time::Duration;
//!
//! #[tokio::main]
//! async fn main() -> Result<(), Box<dyn std::error::Error>> {
//!     let config = CircuitBreakerConfig {
//!         name: "payment-service".to_string(),
//!         failure_threshold: 5,
//!         success_threshold: 2,
//!         timeout_duration: Duration::from_secs(30),
//!         half_open_max_requests: 3,
//!         slow_start_duration: Duration::from_secs(60),
//!         ..Default::default()
//!     };
//!
//!     let mut circuit_breaker = CircuitBreaker::new(config)?;
//!
//!     // Use the circuit breaker
//!     let result = circuit_breaker.execute(|| async {
//!         // Your service call here
//!         Ok::<String, String>("Success".to_string())
//!     }).await;
//!
//!     Ok(())
//! }
//! ```
//!
//! ## Example: Canary Deployment
//!
//! ```no_run
//! use tai_resilience::traffic_management::{CanaryDeployment, TrafficSplit};
//! use std::time::Duration;
//!
//! #[tokio::main]
//! async fn main() -> Result<(), Box<dyn std::error::Error>> {
//!     let mut canary = CanaryDeployment::new(
//!         "my-service".to_string(),
//!         "1.0.0".to_string(),
//!         "1.1.0".to_string(),
//!     );
//!
//!     // Run canary with automatic traffic increase
//!     canary.start(
//!         vec![10, 50, 100], // Traffic percentages
//!         Duration::from_secs(300), // Duration per stage
//!     ).await?;
//!
//!     Ok(())
//! }
//! ```

#![deny(warnings)] // Poka-Yoke: Prevent warnings at compile time

pub mod circuit_breaker_v2;
pub mod error;
pub mod outlier_detection;
pub mod service_mesh;
pub mod traffic_management;

pub use circuit_breaker_v2::{
    CircuitBreaker, CircuitBreakerConfig, CircuitBreakerMetrics, CircuitBreakerState,
};
pub use error::{Error, Result};
pub use outlier_detection::{
    OutlierDetection, OutlierDetectionConfig, OutlierDetectionMetrics, OutlierDetectionState,
};
pub use service_mesh::{
    AuthorizationPolicy, DestinationRule, Gateway, PeerAuthentication, ServiceMeshConfig,
    VirtualService,
};
pub use traffic_management::{
    BlueGreenDeployment, CanaryDeployment, LoadBalancingAlgorithm, TrafficMirror, TrafficSplit,
};
