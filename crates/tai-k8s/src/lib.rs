#![doc = include_str!("../../../docs/tai-k8s/80-kubernetes.md")]
#![warn(missing_docs)]
#![allow(clippy::multiple_crate_versions)]

//! TAI Kubernetes Integration
//!
//! Provides production-grade Kubernetes deployment patterns including:
//! - Kubernetes client operations (pods, deployments, services)
//! - KEDA autoscaling (metric-based and event-driven)
//! - Istio traffic management (VirtualServices, DestinationRules)
//! - Network and security policy enforcement (RBAC, NetworkPolicy)

pub mod istio_traffic;
pub mod k8s_client;
pub mod keda_autoscaling;
pub mod policy_enforcement;

pub use istio_traffic::{IstioTrafficManager, VirtualServiceConfig};
pub use k8s_client::{DeploymentOperation, KubernetesClient, PodOperation};
pub use keda_autoscaling::{EventDrivenScaler, KedaAutoscaler, MetricType};
pub use policy_enforcement::{PolicyEnforcer, PolicyType};

/// Version of tai-k8s
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Kubernetes API version for Deployment
pub const K8S_API_VERSION: &str = "apps/v1";

/// Kubernetes API version for KEDA ScaledObject
pub const KEDA_API_VERSION: &str = "keda.sh/v1alpha1";

/// Kubernetes API version for Istio VirtualService
pub const ISTIO_API_VERSION: &str = "networking.istio.io/v1beta1";

/// Result type for tai-k8s operations
pub type Result<T> = std::result::Result<T, Error>;

/// Error type for tai-k8s operations
#[derive(Debug, thiserror::Error)]
pub enum Error {
    /// Kubernetes API error
    #[error("Kubernetes API error: {0}")]
    KubernetesApi(String),

    /// KEDA configuration error
    #[error("KEDA configuration error: {0}")]
    KedaConfig(String),

    /// Istio configuration error
    #[error("Istio configuration error: {0}")]
    IstioConfig(String),

    /// Policy enforcement error
    #[error("Policy enforcement error: {0}")]
    PolicyEnforcement(String),

    /// YAML serialization error
    #[error("YAML serialization error: {0}")]
    YamlSerialization(#[from] serde_yaml::Error),

    /// JSON serialization error
    #[error("JSON serialization error: {0}")]
    JsonSerialization(#[from] serde_json::Error),

    /// IO error
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    /// Anyhow error wrapper
    #[error("Internal error: {0}")]
    Internal(#[from] anyhow::Error),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version() {
        assert_eq!(VERSION, "0.1.0");
    }

    #[test]
    fn test_api_versions() {
        assert_eq!(K8S_API_VERSION, "apps/v1");
        assert_eq!(KEDA_API_VERSION, "keda.sh/v1alpha1");
        assert_eq!(ISTIO_API_VERSION, "networking.istio.io/v1beta1");
    }
}
