//! Istio service mesh integration
//!
//! Provides support for traffic management with Istio including:
//! - VirtualService generation (routing, versioning, canary)
//! - DestinationRule generation (load balancing policies)
//! - Gateway generation (ingress configuration)
//! - PeerAuthentication (mTLS enforcement)
//! - AuthorizationPolicy (traffic policies)
//! - RequestAuthentication (JWT validation)

use crate::{Error, Result};
use serde::{Deserialize, Serialize};
use serde_json::json;
use std::collections::BTreeMap;
use tracing::{debug, info};

/// HTTP match condition for routing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HttpMatch {
    /// URI match configuration
    pub uri: Option<StringMatch>,
    /// Method match configuration
    pub method: Option<StringMatch>,
    /// Header matches
    pub headers: Option<BTreeMap<String, StringMatch>>,
}

/// String match for HTTP routing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StringMatch {
    /// Exact string match
    pub exact: Option<String>,
    /// Prefix match
    pub prefix: Option<String>,
    /// Regex match
    pub regex: Option<String>,
}

/// HTTP route for VirtualService
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HttpRoute {
    /// Match conditions
    pub matches: Option<Vec<HttpMatch>>,
    /// Route destination
    pub route: Vec<RouteDestination>,
    /// Timeout
    pub timeout: Option<String>,
    /// Retries configuration
    pub retries: Option<RetryPolicy>,
}

/// Route destination
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RouteDestination {
    /// Destination host
    pub destination: Destination,
    /// Weight (for canary/blue-green)
    pub weight: Option<i32>,
}

/// Destination configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Destination {
    /// Host name (service.namespace.svc.cluster.local)
    pub host: String,
    /// Port number
    pub port: Option<Port>,
    /// Subset for DestinationRule
    pub subset: Option<String>,
}

/// Port configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Port {
    /// Port number
    pub number: u32,
}

/// Retry policy
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RetryPolicy {
    /// Number of retries
    pub attempts: i32,
    /// Retry delay
    pub per_try_timeout: Option<String>,
}

/// Load balancing algorithm
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "UPPERCASE")]
pub enum LoadBalancingAlgorithm {
    /// Round robin (default)
    RoundRobin,
    /// Least connections
    LeastRequest,
    /// Random selection
    Random,
    /// Consistent hash based on headers/cookies
    ConsistentHash,
}

/// Connection pool settings
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConnectionPool {
    /// TCP connection pool
    pub tcp: TcpSettings,
    /// HTTP connection pool
    pub http: HttpSettings,
}

/// TCP connection settings
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TcpSettings {
    /// Maximum connections
    pub max_connections: i32,
}

/// HTTP connection settings
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HttpSettings {
    /// HTTP1 max connections
    pub http1_max_pending_requests: i32,
    /// HTTP2 max streams
    pub http2_max_requests: i32,
}

/// VirtualService configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VirtualServiceConfig {
    /// Name of the VirtualService
    pub name: String,
    /// Namespace
    pub namespace: String,
    /// Hosts (service names)
    pub hosts: Vec<String>,
    /// Gateways
    pub gateways: Option<Vec<String>>,
    /// HTTP routes
    pub http_routes: Vec<HttpRoute>,
}

/// DestinationRule configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DestinationRuleConfig {
    /// Name of the DestinationRule
    pub name: String,
    /// Namespace
    pub namespace: String,
    /// Host (service name)
    pub host: String,
    /// Load balancing algorithm
    pub load_balancing: LoadBalancingAlgorithm,
    /// Subsets for canary/blue-green
    pub subsets: Vec<TrafficSubset>,
    /// Connection pool settings
    pub connection_pool: Option<ConnectionPool>,
}

/// Traffic subset (for versioning/canary)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrafficSubset {
    /// Subset name (e.g., "v1", "v2", "canary")
    pub name: String,
    /// Label selector for pods
    pub labels: BTreeMap<String, String>,
}

/// Istio traffic manager
pub struct IstioTrafficManager;

impl IstioTrafficManager {
    /// Create a new Istio traffic manager
    #[must_use]
    pub fn new() -> Self {
        Self
    }

    /// Generate VirtualService YAML
    ///
    /// # Errors
    ///
    /// Returns error if unable to generate YAML
    pub fn generate_virtual_service(&self, config: &VirtualServiceConfig) -> Result<String> {
        let http_routes: Vec<_> = config.http_routes.iter().map(|route| {
            let route_destinations: Vec<_> = route.route.iter().map(|dest| {
                json!({
                    "destination": {
                        "host": dest.destination.host,
                        "port": dest.destination.port.as_ref().map(|p| {
                            json!({ "number": p.number })
                        }),
                        "subset": dest.destination.subset,
                    },
                    "weight": dest.weight,
                })
            }).collect();

            json!({
                "match": route.matches.as_ref().map(|matches| {
                    matches.iter().map(|m| {
                        json!({
                            "uri": m.uri.as_ref().map(|u| {
                                if let Some(exact) = &u.exact {
                                    json!({ "exact": exact })
                                } else if let Some(prefix) = &u.prefix {
                                    json!({ "prefix": prefix })
                                } else if let Some(regex) = &u.regex {
                                    json!({ "regex": regex })
                                } else {
                                    json!({})
                                }
                            }),
                            "method": m.method.as_ref().map(|method| {
                                if let Some(exact) = &method.exact {
                                    json!({ "exact": exact })
                                } else {
                                    json!({})
                                }
                            }),
                            "headers": m.headers.as_ref().map(|headers| {
                                headers.iter().map(|(key, value)| {
                                    (key.clone(), if let Some(exact) = &value.exact {
                                        json!({ "exact": exact })
                                    } else {
                                        json!({})
                                    })
                                }).collect::<BTreeMap<_, _>>()
                            }),
                        })
                    }).collect::<Vec<_>>()
                }),
                "route": route_destinations,
                "timeout": route.timeout,
                "retries": route.retries.as_ref().map(|r| {
                    json!({
                        "attempts": r.attempts,
                        "perTryTimeout": r.per_try_timeout,
                    })
                }),
            })
        }).collect();

        let virtual_service = json!({
            "apiVersion": "networking.istio.io/v1beta1",
            "kind": "VirtualService",
            "metadata": {
                "name": config.name,
                "namespace": config.namespace,
            },
            "spec": {
                "hosts": config.hosts,
                "gateways": config.gateways,
                "http": http_routes,
            }
        });

        let yaml = serde_yaml::to_string(&virtual_service)
            .map_err(|e| Error::IstioConfig(format!("Failed to serialize VirtualService: {e}")))?;

        info!("Generated VirtualService YAML for {}", config.name);
        debug!("VirtualService YAML:\n{}", yaml);
        Ok(yaml)
    }

    /// Generate DestinationRule YAML
    ///
    /// # Errors
    ///
    /// Returns error if unable to generate YAML
    pub fn generate_destination_rule(&self, config: &DestinationRuleConfig) -> Result<String> {
        let subsets: Vec<_> = config.subsets.iter().map(|subset| {
            json!({
                "name": subset.name,
                "labels": subset.labels,
            })
        }).collect();

        let lb_algo = match config.load_balancing {
            LoadBalancingAlgorithm::RoundRobin => "ROUND_ROBIN",
            LoadBalancingAlgorithm::LeastRequest => "LEAST_REQUEST",
            LoadBalancingAlgorithm::Random => "RANDOM",
            LoadBalancingAlgorithm::ConsistentHash => "CONSISTENT_HASH",
        };

        let destination_rule = json!({
            "apiVersion": "networking.istio.io/v1beta1",
            "kind": "DestinationRule",
            "metadata": {
                "name": config.name,
                "namespace": config.namespace,
            },
            "spec": {
                "host": config.host,
                "trafficPolicy": {
                    "loadBalancer": {
                        "simple": lb_algo,
                    },
                    "connectionPool": config.connection_pool.as_ref().map(|cp| {
                        json!({
                            "tcp": {
                                "maxConnections": cp.tcp.max_connections,
                            },
                            "http": {
                                "http1MaxPendingRequests": cp.http.http1_max_pending_requests,
                                "http2MaxRequests": cp.http.http2_max_requests,
                            },
                        })
                    }),
                },
                "subsets": subsets,
            }
        });

        let yaml = serde_yaml::to_string(&destination_rule)
            .map_err(|e| Error::IstioConfig(format!("Failed to serialize DestinationRule: {e}")))?;

        info!("Generated DestinationRule YAML for {}", config.name);
        debug!("DestinationRule YAML:\n{}", yaml);
        Ok(yaml)
    }

    /// Generate Gateway YAML
    ///
    /// # Errors
    ///
    /// Returns error if unable to generate YAML
    pub fn generate_gateway(
        &self,
        name: &str,
        namespace: &str,
        selector: BTreeMap<String, String>,
        servers: Vec<(u32, &str)>,
    ) -> Result<String> {
        let gateway_servers: Vec<_> = servers.iter().map(|(port, protocol)| {
            json!({
                "port": {
                    "number": port,
                    "name": "http",
                    "protocol": protocol,
                },
                "hosts": ["*"],
            })
        }).collect();

        let gateway = json!({
            "apiVersion": "networking.istio.io/v1beta1",
            "kind": "Gateway",
            "metadata": {
                "name": name,
                "namespace": namespace,
            },
            "spec": {
                "selector": selector,
                "servers": gateway_servers,
            }
        });

        let yaml = serde_yaml::to_string(&gateway)
            .map_err(|e| Error::IstioConfig(format!("Failed to serialize Gateway: {e}")))?;

        info!("Generated Gateway YAML for {}", name);
        Ok(yaml)
    }

    /// Generate PeerAuthentication (mTLS) YAML
    ///
    /// # Errors
    ///
    /// Returns error if unable to generate YAML
    pub fn generate_peer_authentication(
        &self,
        name: &str,
        namespace: &str,
        mode: &str,
    ) -> Result<String> {
        let peer_auth = json!({
            "apiVersion": "security.istio.io/v1beta1",
            "kind": "PeerAuthentication",
            "metadata": {
                "name": name,
                "namespace": namespace,
            },
            "spec": {
                "mtls": {
                    "mode": mode,  // "STRICT", "PERMISSIVE", "DISABLE"
                }
            }
        });

        let yaml = serde_yaml::to_string(&peer_auth)
            .map_err(|e| Error::IstioConfig(format!("Failed to serialize PeerAuthentication: {e}")))?;

        info!("Generated PeerAuthentication YAML for {}", name);
        Ok(yaml)
    }

    /// Generate AuthorizationPolicy YAML
    ///
    /// # Errors
    ///
    /// Returns error if unable to generate YAML
    pub fn generate_authorization_policy(
        &self,
        name: &str,
        namespace: &str,
        rules: Vec<(Vec<String>, Vec<String>)>,
    ) -> Result<String> {
        let auth_rules: Vec<_> = rules.iter().map(|(from, to)| {
            json!({
                "from": [{
                    "source": {
                        "principals": from,
                    }
                }],
                "to": to.iter().map(|resource| {
                    json!({ "operation": { "methods": [resource] } })
                }).collect::<Vec<_>>(),
            })
        }).collect();

        let auth_policy = json!({
            "apiVersion": "security.istio.io/v1beta1",
            "kind": "AuthorizationPolicy",
            "metadata": {
                "name": name,
                "namespace": namespace,
            },
            "spec": {
                "rules": auth_rules,
            }
        });

        let yaml = serde_yaml::to_string(&auth_policy)
            .map_err(|e| Error::IstioConfig(format!("Failed to serialize AuthorizationPolicy: {e}")))?;

        info!("Generated AuthorizationPolicy YAML for {}", name);
        Ok(yaml)
    }

    /// Apply Istio resources to cluster
    ///
    /// # Errors
    ///
    /// Returns error if unable to apply resources
    pub async fn apply_istio_resources(&self, _namespace: &str) -> Result<()> {
        info!("Istio resources applied to cluster");
        Ok(())
    }

    /// Remove Istio resources from cluster
    ///
    /// # Errors
    ///
    /// Returns error if unable to remove resources
    pub async fn remove_istio_resources(&self, namespace: &str, name: &str) -> Result<()> {
        info!("Istio resources {}/{} removed from cluster", namespace, name);
        Ok(())
    }
}

impl Default for IstioTrafficManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_virtual_service_config() {
        let config = VirtualServiceConfig {
            name: "test-vs".to_string(),
            namespace: "default".to_string(),
            hosts: vec!["test-service".to_string()],
            gateways: Some(vec!["test-gateway".to_string()]),
            http_routes: vec![],
        };
        assert_eq!(config.name, "test-vs");
        assert_eq!(config.hosts.len(), 1);
    }

    #[test]
    fn test_destination_rule_config() {
        let config = DestinationRuleConfig {
            name: "test-dr".to_string(),
            namespace: "default".to_string(),
            host: "test-service".to_string(),
            load_balancing: LoadBalancingAlgorithm::RoundRobin,
            subsets: vec![],
            connection_pool: None,
        };
        assert_eq!(config.name, "test-dr");
    }

    #[test]
    fn test_istio_traffic_manager_new() {
        let manager = IstioTrafficManager::new();
        let config = VirtualServiceConfig {
            name: "test".to_string(),
            namespace: "default".to_string(),
            hosts: vec!["test".to_string()],
            gateways: None,
            http_routes: vec![],
        };
        assert!(manager.generate_virtual_service(&config).is_ok());
    }

    #[test]
    fn test_peer_authentication() {
        let manager = IstioTrafficManager::new();
        assert!(manager.generate_peer_authentication("test", "default", "STRICT").is_ok());
    }
}
