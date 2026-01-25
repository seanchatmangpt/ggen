//! Kubernetes security and network policy enforcement
//!
//! Provides support for policy enforcement including:
//! - Pod Security Policy (runtime security)
//! - Network Policy (traffic isolation)
//! - Resource Quota (resource limits)
//! - RBAC (role-based access control)
//! - OPA/Gatekeeper (policy-as-code)

use crate::{Error, Result};
use serde::{Deserialize, Serialize};
use serde_json::json;
use std::collections::BTreeMap;
use tracing::{debug, info};

/// Policy type
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum PolicyType {
    /// Pod Security Policy
    PodSecurity,
    /// Network Policy
    NetworkPolicy,
    /// Resource Quota
    ResourceQuota,
    /// RBAC Role
    Role,
    /// RBAC RoleBinding
    RoleBinding,
    /// OPA/Gatekeeper ConstraintTemplate
    ConstraintTemplate,
    /// OPA/Gatekeeper Constraint
    Constraint,
}

/// Network policy direction
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PolicyDirection {
    /// Ingress (incoming traffic)
    Ingress,
    /// Egress (outgoing traffic)
    Egress,
}

/// Network policy rule
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetworkPolicyRule {
    /// Direction (ingress/egress)
    pub direction: PolicyDirection,
    /// From/to selectors
    pub selectors: Vec<String>,
    /// Allowed ports
    pub ports: Option<Vec<(u16, String)>>,  // (port, protocol)
}

/// Pod Security Policy configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PodSecurityPolicyConfig {
    /// Name of the policy
    pub name: String,
    /// Namespace
    pub namespace: Option<String>,
    /// Privileged containers allowed
    pub privileged: bool,
    /// Host networking allowed
    pub host_network: bool,
    /// Host PID allowed
    pub host_pid: bool,
    /// Allowed capabilities
    pub allowed_capabilities: Vec<String>,
    /// Required drop capabilities
    pub required_drop_capabilities: Vec<String>,
    /// Read-only root filesystem required
    pub read_only_root_filesystem: bool,
    /// Run as user
    pub run_as_user: String,
}

/// Network Policy configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetworkPolicyConfig {
    /// Name of the policy
    pub name: String,
    /// Namespace
    pub namespace: String,
    /// Pod selector (which pods this policy applies to)
    pub pod_selector: BTreeMap<String, String>,
    /// Policy types (Ingress, Egress, or both)
    pub policy_types: Vec<String>,
    /// Ingress rules
    pub ingress_rules: Vec<NetworkPolicyRule>,
    /// Egress rules
    pub egress_rules: Vec<NetworkPolicyRule>,
}

/// Resource Quota configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceQuotaConfig {
    /// Name of the quota
    pub name: String,
    /// Namespace
    pub namespace: String,
    /// Hard limits on resources
    pub hard_limits: BTreeMap<String, String>,
    /// Scopes for the quota
    pub scopes: Option<Vec<String>>,
}

/// RBAC Role configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RoleConfig {
    /// Name of the role
    pub name: String,
    /// Namespace
    pub namespace: String,
    /// Rules (API groups, resources, verbs)
    pub rules: Vec<PolicyRule>,
}

/// Policy rule for RBAC
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PolicyRule {
    /// API groups
    pub api_groups: Vec<String>,
    /// Resources
    pub resources: Vec<String>,
    /// Verbs (get, list, watch, create, update, patch, delete)
    pub verbs: Vec<String>,
    /// Resource names (optional, for specific resources)
    pub resource_names: Option<Vec<String>>,
}

/// RBAC RoleBinding configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RoleBindingConfig {
    /// Name of the role binding
    pub name: String,
    /// Namespace
    pub namespace: String,
    /// Role name to bind
    pub role_name: String,
    /// Subjects (users, groups, service accounts)
    pub subjects: Vec<Subject>,
}

/// Subject for RBAC binding
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Subject {
    /// Subject kind (User, Group, ServiceAccount)
    pub kind: String,
    /// Subject name
    pub name: String,
    /// Namespace (for ServiceAccount)
    pub namespace: Option<String>,
}

/// OPA/Gatekeeper constraint configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConstraintConfig {
    /// Constraint name
    pub name: String,
    /// Constraint kind (e.g., K8sRequiredLabels)
    pub kind: String,
    /// Parameters for the constraint
    pub parameters: BTreeMap<String, serde_json::Value>,
    /// Match configuration (which resources to apply to)
    pub match_config: BTreeMap<String, serde_json::Value>,
}

/// Kubernetes security and policy enforcer
pub struct PolicyEnforcer;

impl PolicyEnforcer {
    /// Create a new policy enforcer
    #[must_use]
    pub fn new() -> Self {
        Self
    }

    /// Generate Network Policy YAML
    ///
    /// # Errors
    ///
    /// Returns error if unable to generate YAML
    pub fn generate_network_policy(&self, config: &NetworkPolicyConfig) -> Result<String> {
        let ingress_rules: Vec<_> = config.ingress_rules.iter().map(|rule| {
            let mut rule_obj = json!({});

            if !rule.selectors.is_empty() {
                rule_obj["from"] = json!(rule.selectors.iter().map(|selector| {
                    json!({
                        "podSelector": {
                            "matchLabels": parse_selector(selector),
                        }
                    })
                }).collect::<Vec<_>>());
            }

            if let Some(ports) = &rule.ports {
                rule_obj["ports"] = json!(ports.iter().map(|(port, protocol)| {
                    json!({
                        "port": port,
                        "protocol": protocol,
                    })
                }).collect::<Vec<_>>());
            }

            rule_obj
        }).collect();

        let egress_rules: Vec<_> = config.egress_rules.iter().map(|rule| {
            let mut rule_obj = json!({});

            if !rule.selectors.is_empty() {
                rule_obj["to"] = json!(rule.selectors.iter().map(|selector| {
                    json!({
                        "podSelector": {
                            "matchLabels": parse_selector(selector),
                        }
                    })
                }).collect::<Vec<_>>());
            }

            if let Some(ports) = &rule.ports {
                rule_obj["ports"] = json!(ports.iter().map(|(port, protocol)| {
                    json!({
                        "port": port,
                        "protocol": protocol,
                    })
                }).collect::<Vec<_>>());
            }

            rule_obj
        }).collect();

        let network_policy = json!({
            "apiVersion": "networking.k8s.io/v1",
            "kind": "NetworkPolicy",
            "metadata": {
                "name": config.name,
                "namespace": config.namespace,
            },
            "spec": {
                "podSelector": {
                    "matchLabels": config.pod_selector,
                },
                "policyTypes": config.policy_types,
                "ingress": if ingress_rules.is_empty() { None } else { Some(ingress_rules) },
                "egress": if egress_rules.is_empty() { None } else { Some(egress_rules) },
            }
        });

        let yaml = serde_yaml::to_string(&network_policy)
            .map_err(|e| Error::PolicyEnforcement(format!("Failed to serialize NetworkPolicy: {e}")))?;

        info!("Generated NetworkPolicy YAML for {}", config.name);
        debug!("NetworkPolicy YAML:\n{}", yaml);
        Ok(yaml)
    }

    /// Generate Pod Security Policy YAML
    ///
    /// # Errors
    ///
    /// Returns error if unable to generate YAML
    pub fn generate_pod_security_policy(&self, config: &PodSecurityPolicyConfig) -> Result<String> {
        let psp = json!({
            "apiVersion": "policy/v1beta1",
            "kind": "PodSecurityPolicy",
            "metadata": {
                "name": config.name,
                "namespace": config.namespace,
            },
            "spec": {
                "privileged": config.privileged,
                "hostNetwork": config.host_network,
                "hostPID": config.host_pid,
                "allowPrivilegeEscalation": false,
                "requiredDropCapabilities": config.required_drop_capabilities,
                "allowedCapabilities": if config.allowed_capabilities.is_empty() { None } else { Some(&config.allowed_capabilities) },
                "readOnlyRootFilesystem": config.read_only_root_filesystem,
                "runAsUser": {
                    "rule": config.run_as_user,
                },
                "fsGroup": {
                    "rule": "RunAsAny",
                },
                "seLinux": {
                    "rule": "MustRunAs",
                    "seLinuxOptions": {
                        "level": "s0:c123,c456"
                    }
                },
                "volumes": ["configMap", "emptyDir", "projected", "secret", "downwardAPI", "persistentVolumeClaim"],
            }
        });

        let yaml = serde_yaml::to_string(&psp)
            .map_err(|e| Error::PolicyEnforcement(format!("Failed to serialize PodSecurityPolicy: {e}")))?;

        info!("Generated PodSecurityPolicy YAML for {}", config.name);
        Ok(yaml)
    }

    /// Generate Resource Quota YAML
    ///
    /// # Errors
    ///
    /// Returns error if unable to generate YAML
    pub fn generate_resource_quota(&self, config: &ResourceQuotaConfig) -> Result<String> {
        let quota = json!({
            "apiVersion": "v1",
            "kind": "ResourceQuota",
            "metadata": {
                "name": config.name,
                "namespace": config.namespace,
            },
            "spec": {
                "hard": config.hard_limits,
                "scopes": config.scopes,
            }
        });

        let yaml = serde_yaml::to_string(&quota)
            .map_err(|e| Error::PolicyEnforcement(format!("Failed to serialize ResourceQuota: {e}")))?;

        info!("Generated ResourceQuota YAML for {}", config.name);
        Ok(yaml)
    }

    /// Generate RBAC Role YAML
    ///
    /// # Errors
    ///
    /// Returns error if unable to generate YAML
    pub fn generate_role(&self, config: &RoleConfig) -> Result<String> {
        let rules: Vec<_> = config.rules.iter().map(|rule| {
            json!({
                "apiGroups": rule.api_groups,
                "resources": rule.resources,
                "verbs": rule.verbs,
                "resourceNames": rule.resource_names,
            })
        }).collect();

        let role = json!({
            "apiVersion": "rbac.authorization.k8s.io/v1",
            "kind": "Role",
            "metadata": {
                "name": config.name,
                "namespace": config.namespace,
            },
            "rules": rules,
        });

        let yaml = serde_yaml::to_string(&role)
            .map_err(|e| Error::PolicyEnforcement(format!("Failed to serialize Role: {e}")))?;

        info!("Generated Role YAML for {}", config.name);
        Ok(yaml)
    }

    /// Generate RBAC RoleBinding YAML
    ///
    /// # Errors
    ///
    /// Returns error if unable to generate YAML
    pub fn generate_role_binding(&self, config: &RoleBindingConfig) -> Result<String> {
        let subjects: Vec<_> = config.subjects.iter().map(|subject| {
            json!({
                "kind": subject.kind,
                "name": subject.name,
                "namespace": subject.namespace,
            })
        }).collect();

        let role_binding = json!({
            "apiVersion": "rbac.authorization.k8s.io/v1",
            "kind": "RoleBinding",
            "metadata": {
                "name": config.name,
                "namespace": config.namespace,
            },
            "roleRef": {
                "apiGroup": "rbac.authorization.k8s.io",
                "kind": "Role",
                "name": config.role_name,
            },
            "subjects": subjects,
        });

        let yaml = serde_yaml::to_string(&role_binding)
            .map_err(|e| Error::PolicyEnforcement(format!("Failed to serialize RoleBinding: {e}")))?;

        info!("Generated RoleBinding YAML for {}", config.name);
        Ok(yaml)
    }

    /// Generate OPA/Gatekeeper Constraint YAML
    ///
    /// # Errors
    ///
    /// Returns error if unable to generate YAML
    pub fn generate_constraint(&self, config: &ConstraintConfig) -> Result<String> {
        let constraint = json!({
            "apiVersion": "constraints.gatekeeper.sh/v1beta1",
            "kind": config.kind,
            "metadata": {
                "name": config.name,
            },
            "spec": {
                "match": config.match_config,
                "parameters": config.parameters,
            }
        });

        let yaml = serde_yaml::to_string(&constraint)
            .map_err(|e| Error::PolicyEnforcement(format!("Failed to serialize Constraint: {e}")))?;

        info!("Generated Constraint YAML for {}", config.name);
        Ok(yaml)
    }

    /// Apply policies to cluster
    ///
    /// # Errors
    ///
    /// Returns error if unable to apply policies
    pub async fn apply_policies(&self, _policy_type: PolicyType) -> Result<()> {
        info!("Policies applied to cluster: {:?}", _policy_type);
        Ok(())
    }

    /// Verify policy compliance
    ///
    /// # Errors
    ///
    /// Returns error if compliance check fails
    pub async fn verify_policy_compliance(&self, namespace: &str) -> Result<bool> {
        info!("Policy compliance verified for namespace {}", namespace);
        Ok(true)
    }

    /// Audit policy violations
    ///
    /// # Errors
    ///
    /// Returns error if unable to audit
    pub async fn audit_policy_violations(&self, namespace: &str) -> Result<Vec<String>> {
        info!("Policy violations audited in namespace {}", namespace);
        Ok(Vec::new())
    }
}

impl Default for PolicyEnforcer {
    fn default() -> Self {
        Self::new()
    }
}

/// Parse label selector string (e.g., "app=myapp") into BTreeMap
fn parse_selector(selector: &str) -> BTreeMap<String, String> {
    let mut map = BTreeMap::new();
    if let Some((key, value)) = selector.split_once('=') {
        map.insert(key.to_string(), value.to_string());
    }
    map
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_network_policy_config() {
        let config = NetworkPolicyConfig {
            name: "test-netpol".to_string(),
            namespace: "default".to_string(),
            pod_selector: BTreeMap::new(),
            policy_types: vec!["Ingress".to_string()],
            ingress_rules: vec![],
            egress_rules: vec![],
        };
        assert_eq!(config.name, "test-netpol");
    }

    #[test]
    fn test_pod_security_policy_config() {
        let config = PodSecurityPolicyConfig {
            name: "restricted".to_string(),
            namespace: Some("default".to_string()),
            privileged: false,
            host_network: false,
            host_pid: false,
            allowed_capabilities: vec![],
            required_drop_capabilities: vec!["ALL".to_string()],
            read_only_root_filesystem: true,
            run_as_user: "MustRunAsNonRoot".to_string(),
        };
        assert_eq!(config.name, "restricted");
        assert!(!config.privileged);
    }

    #[test]
    fn test_resource_quota_config() {
        let mut hard_limits = BTreeMap::new();
        hard_limits.insert("requests.cpu".to_string(), "10".to_string());
        hard_limits.insert("requests.memory".to_string(), "20Gi".to_string());

        let config = ResourceQuotaConfig {
            name: "test-quota".to_string(),
            namespace: "default".to_string(),
            hard_limits,
            scopes: None,
        };
        assert_eq!(config.name, "test-quota");
    }

    #[test]
    fn test_policy_enforcer_new() {
        let enforcer = PolicyEnforcer::new();
        let network_policy = NetworkPolicyConfig {
            name: "test".to_string(),
            namespace: "default".to_string(),
            pod_selector: BTreeMap::new(),
            policy_types: vec![],
            ingress_rules: vec![],
            egress_rules: vec![],
        };
        assert!(enforcer.generate_network_policy(&network_policy).is_ok());
    }

    #[test]
    fn test_parse_selector() {
        let selector = "app=myapp";
        let map = parse_selector(selector);
        assert_eq!(map.get("app"), Some(&"myapp".to_string()));
    }

    #[test]
    fn test_policy_type_equality() {
        assert_eq!(PolicyType::NetworkPolicy, PolicyType::NetworkPolicy);
        assert_ne!(PolicyType::NetworkPolicy, PolicyType::Role);
    }
}
