//! Service mesh integration for Istio/Envoy configuration
//!
//! This module provides types and utilities for generating Istio service mesh resources
//! including VirtualServices, DestinationRules, Gateways, and AuthorizationPolicies.

use crate::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use tracing::debug;

/// Load balancing algorithm for traffic distribution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LoadBalancingAlgorithm {
    /// Round-robin load balancing (default)
    RoundRobin,
    /// Least connections
    LeastRequest,
    /// Random selection
    Random,
    /// Weighted load balancing
    Weighted(HashMap<String, u32>),
}

impl Default for LoadBalancingAlgorithm {
    fn default() -> Self {
        LoadBalancingAlgorithm::RoundRobin
    }
}

/// Connection pool settings
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConnectionPool {
    /// Maximum number of HTTP/1.1 connections
    pub tcp_max_connections: u32,
    /// Maximum number of pending HTTP requests
    pub http_max_pending_requests: u32,
    /// Maximum number of requests per connection
    pub http_max_requests: u32,
    /// Maximum number of requests per connection pool
    pub http2_max_requests: u32,
    /// Connection idle timeout
    pub max_connection_duration: String,
    /// Whether to use HTTP/2
    pub h2_upgrade_policy: String,
}

impl Default for ConnectionPool {
    fn default() -> Self {
        Self {
            tcp_max_connections: 100,
            http_max_pending_requests: 100,
            http_max_requests: 100,
            http2_max_requests: 1000,
            max_connection_duration: "5m".to_string(),
            h2_upgrade_policy: "UPGRADE".to_string(),
        }
    }
}

/// Outlier detection settings
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OutlierDetectionConfig {
    /// Minimum request volume to calculate outlier detection
    pub min_request_volume: u32,
    /// Minimum success rate threshold
    pub min_success_rate: f32,
    /// Maximum success rate to consider unhealthy
    pub max_failure_percentage: f32,
    /// Consecutive error threshold
    pub consecutive_errors: u32,
    /// Interval to eject unhealthy hosts
    pub split_external_local_orig_dest_traffic: bool,
    /// Time interval for ejection analysis
    pub interval: String,
    /// Base ejection time
    pub base_ejection_time: String,
    /// Maximum ejection percentage
    pub max_ejection_percentage: u32,
    /// Minimum ejection percentage
    pub min_ejection_percentage: u32,
}

impl Default for OutlierDetectionConfig {
    fn default() -> Self {
        Self {
            min_request_volume: 100,
            min_success_rate: 0.95,
            max_failure_percentage: 100.0,
            consecutive_errors: 5,
            split_external_local_orig_dest_traffic: false,
            interval: "30s".to_string(),
            base_ejection_time: "30s".to_string(),
            max_ejection_percentage: 50,
            min_ejection_percentage: 10,
        }
    }
}

/// Destination rule defines load balancing and connection pool settings
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DestinationRule {
    /// Name of the destination rule
    pub name: String,
    /// Host that the destination rule applies to
    pub host: String,
    /// Namespace for the destination rule
    pub namespace: String,
    /// Traffic policy for load balancing
    pub traffic_policy: TrafficPolicy,
    /// Subsets for different service versions
    pub subsets: Vec<Subset>,
}

/// Traffic policy for load balancing and connection settings
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrafficPolicy {
    /// Load balancing algorithm
    pub load_balancer: LoadBalancingAlgorithm,
    /// Connection pool settings
    pub connection_pool: ConnectionPool,
    /// Outlier detection
    pub outlier_detection: OutlierDetectionConfig,
}

impl Default for TrafficPolicy {
    fn default() -> Self {
        Self {
            load_balancer: LoadBalancingAlgorithm::default(),
            connection_pool: ConnectionPool::default(),
            outlier_detection: OutlierDetectionConfig::default(),
        }
    }
}

/// Subset defines traffic routing to specific service versions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Subset {
    /// Name of the subset
    pub name: String,
    /// Labels to match for this subset
    pub labels: HashMap<String, String>,
}

/// HTTP route for VirtualService
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HttpRoute {
    /// Name of the route
    pub name: String,
    /// Match conditions
    pub match_conditions: Vec<RouteMatch>,
    /// Route destinations
    pub route: Vec<RouteDestination>,
    /// Timeout for requests
    pub timeout: String,
    /// Retry policy
    pub retries: Option<RetryPolicy>,
}

/// Route match condition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RouteMatch {
    /// URI match
    pub uri: Option<StringMatch>,
    /// Header match
    pub headers: Option<HashMap<String, StringMatch>>,
    /// Query parameter match
    pub query_params: Option<HashMap<String, StringMatch>>,
}

/// String match for routing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum StringMatch {
    /// Exact string match
    Exact(String),
    /// Prefix match
    Prefix(String),
    /// Regex match
    Regex(String),
}

/// Route destination with traffic weight
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RouteDestination {
    /// Host to route to
    pub host: String,
    /// Subset of the host
    pub subset: Option<String>,
    /// Port number
    pub port: Option<u32>,
    /// Traffic weight (0-100)
    pub weight: u32,
}

/// Retry policy for failed requests
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RetryPolicy {
    /// Maximum number of retries
    pub max_retries: u32,
    /// Base delay between retries
    pub backoff: String,
    /// Maximum delay for exponential backoff
    pub max_delay: String,
}

/// VirtualService routes traffic to destinations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VirtualService {
    /// Name of the virtual service
    pub name: String,
    /// Namespace
    pub namespace: String,
    /// Hosts that this virtual service applies to
    pub hosts: Vec<String>,
    /// Gateways this applies to
    pub gateways: Vec<String>,
    /// HTTP routes
    pub http_routes: Vec<HttpRoute>,
    /// TCP routes
    pub tcp_routes: Vec<TcpRoute>,
}

/// TCP route for non-HTTP traffic
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TcpRoute {
    /// Route name
    pub name: String,
    /// Match conditions
    pub match_conditions: Vec<L4Match>,
    /// Route destinations
    pub route: Vec<RouteDestination>,
}

/// Layer 4 match condition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct L4Match {
    /// Source labels to match
    pub source_labels: HashMap<String, String>,
    /// Destination ports to match
    pub destination_ports: Vec<u32>,
}

/// Gateway configuration for ingress
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Gateway {
    /// Name of the gateway
    pub name: String,
    /// Namespace
    pub namespace: String,
    /// Selector for gateway pods
    pub selector: HashMap<String, String>,
    /// Servers (ports and protocols)
    pub servers: Vec<Server>,
}

/// Server definition in gateway
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Server {
    /// Port configuration
    pub port: PortConfig,
    /// Protocols (HTTP, HTTPS, TCP, etc.)
    pub protocol: String,
    /// Host names
    pub hosts: Vec<String>,
    /// TLS configuration
    pub tls: Option<TlsConfig>,
}

/// Port configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PortConfig {
    /// Port number
    pub number: u32,
    /// Protocol (TCP, UDP)
    pub protocol: String,
    /// Port name
    pub name: String,
}

/// TLS configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TlsConfig {
    /// TLS mode (SIMPLE, MUTUAL, AUTO_PASSTHROUGH, PASSTHROUGH)
    pub mode: String,
    /// Certificate file
    pub certificate_chain: Option<String>,
    /// Private key file
    pub private_key: Option<String>,
    /// CA certificate for mTLS
    pub ca_certificates: Option<String>,
}

/// PeerAuthentication enables mTLS between services
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PeerAuthentication {
    /// Name
    pub name: String,
    /// Namespace
    pub namespace: String,
    /// mTLS mode (UNSET, DISABLE, PERMISSIVE, STRICT)
    pub mtls_mode: String,
    /// Selector for services
    pub selector: Option<HashMap<String, String>>,
}

/// AuthorizationPolicy controls traffic access
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuthorizationPolicy {
    /// Name
    pub name: String,
    /// Namespace
    pub namespace: String,
    /// Action (ALLOW, DENY, CUSTOM, AUDIT)
    pub action: String,
    /// Rules for authorization
    pub rules: Vec<AuthRule>,
    /// Selector for services
    pub selector: Option<HashMap<String, String>>,
}

/// Authorization rule
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuthRule {
    /// Source
    pub from: Option<Vec<Source>>,
    /// Operation
    pub to: Option<Vec<Operation>>,
    /// Conditions
    pub when: Option<Vec<Condition>>,
}

/// Source definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Source {
    /// Principals (service accounts)
    pub principals: Option<Vec<String>>,
    /// Namespaces
    pub namespaces: Option<Vec<String>>,
    /// IP blocks
    pub ip_blocks: Option<Vec<String>>,
}

/// Operation definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Operation {
    /// Methods (GET, POST, etc.)
    pub methods: Option<Vec<String>>,
    /// Hosts
    pub hosts: Option<Vec<String>>,
    /// Ports
    pub ports: Option<Vec<String>>,
    /// Paths
    pub paths: Option<Vec<String>>,
}

/// Condition for authorization
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Condition {
    /// Key
    pub key: String,
    /// Values
    pub values: Vec<String>,
    /// Not values
    pub not_values: Option<Vec<String>>,
}

/// Service mesh configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ServiceMeshConfig {
    /// Namespace for all resources
    pub namespace: String,
    /// Enable mTLS globally
    pub enable_mtls: bool,
    /// Default TLS mode
    pub mtls_mode: String,
    /// Virtual services
    pub virtual_services: Vec<VirtualService>,
    /// Destination rules
    pub destination_rules: Vec<DestinationRule>,
    /// Gateways
    pub gateways: Vec<Gateway>,
    /// PeerAuthentication policies
    pub peer_authentications: Vec<PeerAuthentication>,
    /// AuthorizationPolicies
    pub authorization_policies: Vec<AuthorizationPolicy>,
}

impl ServiceMeshConfig {
    /// Create a new service mesh configuration
    pub fn new(namespace: impl Into<String>) -> Self {
        Self {
            namespace: namespace.into(),
            enable_mtls: true,
            mtls_mode: "STRICT".to_string(),
            virtual_services: Vec::new(),
            destination_rules: Vec::new(),
            gateways: Vec::new(),
            peer_authentications: Vec::new(),
            authorization_policies: Vec::new(),
        }
    }

    /// Generate Istio manifests as YAML
    pub fn to_yaml(&self) -> Result<String> {
        serde_yaml::to_string(self).map_err(|e| Error::IstioConfigError {
            reason: format!("Failed to serialize to YAML: {}", e),
        })
    }

    /// Generate Kubernetes kubectl apply manifests
    pub fn to_kubectl_manifests(&self) -> Result<String> {
        let mut manifests = String::new();

        // Add PeerAuthentication
        if self.enable_mtls {
            let peer_auth = PeerAuthentication {
                name: "default".to_string(),
                namespace: self.namespace.clone(),
                mtls_mode: self.mtls_mode.clone(),
                selector: None,
            };

            manifests.push_str(&format!(
                "---\napiVersion: security.istio.io/v1beta1\nkind: PeerAuthentication\n"
            ));
            manifests.push_str(&format!(
                "metadata:\n  name: {}\n  namespace: {}\n",
                peer_auth.name, peer_auth.namespace
            ));
            manifests.push_str(&format!(
                "spec:\n  mtls:\n    mode: {}\n",
                peer_auth.mtls_mode
            ));
        }

        // Add VirtualServices
        for vs in &self.virtual_services {
            manifests.push_str("---\n");
            manifests.push_str("apiVersion: networking.istio.io/v1beta1\n");
            manifests.push_str("kind: VirtualService\n");
            manifests.push_str(&format!(
                "metadata:\n  name: {}\n  namespace: {}\n",
                vs.name, vs.namespace
            ));
            manifests.push_str(&format!("spec:\n  hosts:\n"));
            for host in &vs.hosts {
                manifests.push_str(&format!("  - {}\n", host));
            }
        }

        // Add DestinationRules
        for dr in &self.destination_rules {
            manifests.push_str("---\n");
            manifests.push_str("apiVersion: networking.istio.io/v1beta1\n");
            manifests.push_str("kind: DestinationRule\n");
            manifests.push_str(&format!(
                "metadata:\n  name: {}\n  namespace: {}\n",
                dr.name, dr.namespace
            ));
            manifests.push_str(&format!("spec:\n  host: {}\n", dr.host));
        }

        debug!("Generated kubectl manifests for service mesh");
        Ok(manifests)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_service_mesh_config_creation() {
        let config = ServiceMeshConfig::new("default");
        assert_eq!(config.namespace, "default");
        assert!(config.enable_mtls);
    }

    #[test]
    fn test_destination_rule_creation() {
        let rule = DestinationRule {
            name: "my-service".to_string(),
            host: "my-service.default.svc.cluster.local".to_string(),
            namespace: "default".to_string(),
            traffic_policy: TrafficPolicy::default(),
            subsets: vec![Subset {
                name: "v1".to_string(),
                labels: vec![("version".to_string(), "v1".to_string())]
                    .into_iter()
                    .collect(),
            }],
        };

        assert_eq!(rule.name, "my-service");
        assert_eq!(rule.subsets.len(), 1);
    }

    #[test]
    fn test_virtual_service_creation() {
        let vs = VirtualService {
            name: "my-service".to_string(),
            namespace: "default".to_string(),
            hosts: vec!["my-service".to_string()],
            gateways: vec![],
            http_routes: vec![],
            tcp_routes: vec![],
        };

        assert_eq!(vs.name, "my-service");
        assert_eq!(vs.hosts.len(), 1);
    }

    #[test]
    fn test_service_mesh_config_to_yaml() {
        let config = ServiceMeshConfig::new("default");
        let yaml = config.to_yaml();
        assert!(yaml.is_ok());
    }
}
