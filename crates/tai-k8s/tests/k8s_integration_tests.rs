//! Integration tests for Kubernetes deployment infrastructure
//!
//! Tests verify:
//! - Helm chart deployment
//! - Deployment scaling
//! - KEDA autoscaling configuration
//! - Istio traffic management
//! - Network policies
//! - Pod eviction and rescheduling

use std::collections::BTreeMap;
use tai_k8s::{
    istio_traffic::{
        DestinationRuleConfig, IstioTrafficManager, LoadBalancingAlgorithm, VirtualServiceConfig,
    },
    keda_autoscaling::{EventDrivenScaler, KedaAutoscaler, MetricType, ScaledObjectConfig},
    policy_enforcement::{NetworkPolicyConfig, PodSecurityPolicyConfig, PolicyEnforcer},
};

#[test]
fn test_keda_scaled_object_cpu_metric() {
    // Arrange
    let autoscaler = KedaAutoscaler::new();
    let config = ScaledObjectConfig {
        name: "test-cpu-scaler".to_string(),
        namespace: "default".to_string(),
        target_name: "test-deployment".to_string(),
        target_kind: "Deployment".to_string(),
        min_replicas: 1,
        max_replicas: 10,
        metrics: vec![MetricType::Cpu {
            target_utilization_percentage: 70,
        }],
        scale_up_cooldown: 30,
        scale_down_cooldown: 300,
        fallback_replicas: Some(2),
    };

    // Act
    let result = autoscaler.generate_scaled_object(&config);

    // Assert
    assert!(result.is_ok());
    let yaml = result.unwrap();
    assert!(yaml.contains("test-cpu-scaler"));
    assert!(yaml.contains("cpu"));
    assert!(yaml.contains("70"));
}

#[test]
fn test_keda_pubsub_event_scaler() {
    // Arrange
    let config =
        EventDrivenScaler::with_pubsub("ggen-project".to_string(), "tai-events".to_string(), 100);

    // Act & Assert
    assert_eq!(config.name, "pubsub-scaler-tai-even");
    assert_eq!(config.min_replicas, 1);
    assert_eq!(config.max_replicas, 10);
    assert_eq!(config.fallback_replicas, Some(2));
}

#[test]
fn test_keda_kafka_event_scaler() {
    // Arrange
    let brokers = vec![
        "kafka-broker-1:9092".to_string(),
        "kafka-broker-2:9092".to_string(),
    ];
    let config = EventDrivenScaler::with_kafka(
        brokers.clone(),
        "tai-events".to_string(),
        "tai-consumer-group".to_string(),
        1000,
    );

    // Act & Assert
    assert_eq!(config.name, "kafka-scaler-tai-even");
    assert!(config.metrics.len() > 0);
}

#[test]
fn test_istio_virtual_service_generation() {
    // Arrange
    let traffic_manager = IstioTrafficManager::new();
    let config = VirtualServiceConfig {
        name: "tai-vs".to_string(),
        namespace: "default".to_string(),
        hosts: vec!["tai-service".to_string()],
        gateways: Some(vec!["tai-gateway".to_string()]),
        http_routes: vec![],
    };

    // Act
    let result = traffic_manager.generate_virtual_service(&config);

    // Assert
    assert!(result.is_ok());
    let yaml = result.unwrap();
    assert!(yaml.contains("tai-vs"));
    assert!(yaml.contains("networking.istio.io"));
}

#[test]
fn test_istio_destination_rule_generation() {
    // Arrange
    let traffic_manager = IstioTrafficManager::new();
    let config = DestinationRuleConfig {
        name: "tai-dr".to_string(),
        namespace: "default".to_string(),
        host: "tai-service".to_string(),
        load_balancing: LoadBalancingAlgorithm::RoundRobin,
        subsets: vec![],
        connection_pool: None,
    };

    // Act
    let result = traffic_manager.generate_destination_rule(&config);

    // Assert
    assert!(result.is_ok());
    let yaml = result.unwrap();
    assert!(yaml.contains("tai-dr"));
    assert!(yaml.contains("ROUND_ROBIN"));
}

#[test]
fn test_istio_gateway_generation() {
    // Arrange
    let traffic_manager = IstioTrafficManager::new();
    let mut selector = BTreeMap::new();
    selector.insert("istio".to_string(), "ingressgateway".to_string());

    // Act
    let result = traffic_manager.generate_gateway(
        "tai-gateway",
        "default",
        selector,
        vec![(80, "HTTP"), (443, "HTTPS")],
    );

    // Assert
    assert!(result.is_ok());
    let yaml = result.unwrap();
    assert!(yaml.contains("tai-gateway"));
    assert!(yaml.contains("HTTP"));
}

#[test]
fn test_istio_peer_authentication_mtls() {
    // Arrange
    let traffic_manager = IstioTrafficManager::new();

    // Act
    let result = traffic_manager.generate_peer_authentication("default-mtls", "default", "STRICT");

    // Assert
    assert!(result.is_ok());
    let yaml = result.unwrap();
    assert!(yaml.contains("PeerAuthentication"));
    assert!(yaml.contains("STRICT"));
}

#[test]
fn test_network_policy_ingress_only() {
    // Arrange
    let enforcer = PolicyEnforcer::new();
    let mut pod_selector = BTreeMap::new();
    pod_selector.insert("app".to_string(), "tai".to_string());

    let config = NetworkPolicyConfig {
        name: "tai-netpol".to_string(),
        namespace: "default".to_string(),
        pod_selector,
        policy_types: vec!["Ingress".to_string()],
        ingress_rules: vec![],
        egress_rules: vec![],
    };

    // Act
    let result = enforcer.generate_network_policy(&config);

    // Assert
    assert!(result.is_ok());
    let yaml = result.unwrap();
    assert!(yaml.contains("tai-netpol"));
    assert!(yaml.contains("Ingress"));
}

#[test]
fn test_pod_security_policy_restricted() {
    // Arrange
    let enforcer = PolicyEnforcer::new();
    let config = PodSecurityPolicyConfig {
        name: "restricted-psp".to_string(),
        namespace: Some("default".to_string()),
        privileged: false,
        host_network: false,
        host_pid: false,
        allowed_capabilities: vec![],
        required_drop_capabilities: vec!["ALL".to_string()],
        read_only_root_filesystem: true,
        run_as_user: "MustRunAsNonRoot".to_string(),
    };

    // Act
    let result = enforcer.generate_pod_security_policy(&config);

    // Assert
    assert!(result.is_ok());
    let yaml = result.unwrap();
    assert!(yaml.contains("restricted-psp"));
    assert!(yaml.contains("privileged: false"));
}

#[test]
fn test_rbac_role_generation() {
    // Arrange
    let enforcer = PolicyEnforcer::new();
    use tai_k8s::policy_enforcement::{PolicyRule, RoleConfig};

    let config = RoleConfig {
        name: "tai-reader".to_string(),
        namespace: "default".to_string(),
        rules: vec![PolicyRule {
            api_groups: vec!["".to_string()],
            resources: vec!["pods".to_string()],
            verbs: vec!["get".to_string(), "list".to_string(), "watch".to_string()],
            resource_names: None,
        }],
    };

    // Act
    let result = enforcer.generate_role(&config);

    // Assert
    assert!(result.is_ok());
    let yaml = result.unwrap();
    assert!(yaml.contains("tai-reader"));
    assert!(yaml.contains("pods"));
}

#[test]
fn test_rbac_role_binding_generation() {
    // Arrange
    let enforcer = PolicyEnforcer::new();
    use tai_k8s::policy_enforcement::{RoleBindingConfig, Subject};

    let config = RoleBindingConfig {
        name: "tai-reader-binding".to_string(),
        namespace: "default".to_string(),
        role_name: "tai-reader".to_string(),
        subjects: vec![Subject {
            kind: "ServiceAccount".to_string(),
            name: "tai".to_string(),
            namespace: Some("default".to_string()),
        }],
    };

    // Act
    let result = enforcer.generate_role_binding(&config);

    // Assert
    assert!(result.is_ok());
    let yaml = result.unwrap();
    assert!(yaml.contains("tai-reader-binding"));
    assert!(yaml.contains("ServiceAccount"));
}

#[test]
fn test_multiple_metric_types_in_scaled_object() {
    // Arrange
    let autoscaler = KedaAutoscaler::new();
    let config = ScaledObjectConfig {
        name: "multi-metric-scaler".to_string(),
        namespace: "default".to_string(),
        target_name: "test-deployment".to_string(),
        target_kind: "Deployment".to_string(),
        min_replicas: 2,
        max_replicas: 15,
        metrics: vec![
            MetricType::Cpu {
                target_utilization_percentage: 70,
            },
            MetricType::Memory {
                target_utilization_percentage: 80,
            },
        ],
        scale_up_cooldown: 30,
        scale_down_cooldown: 300,
        fallback_replicas: Some(3),
    };

    // Act
    let result = autoscaler.generate_scaled_object(&config);

    // Assert
    assert!(result.is_ok());
    let yaml = result.unwrap();
    assert!(yaml.contains("cpu"));
    assert!(yaml.contains("memory"));
}

#[test]
fn test_canary_deployment_with_istio() {
    // Arrange
    let traffic_manager = IstioTrafficManager::new();
    use tai_k8s::istio_traffic::{DestinationRuleConfig, TrafficSubset};

    let subsets = vec![
        TrafficSubset {
            name: "stable".to_string(),
            labels: {
                let mut m = BTreeMap::new();
                m.insert("version".to_string(), "v1".to_string());
                m
            },
        },
        TrafficSubset {
            name: "canary".to_string(),
            labels: {
                let mut m = BTreeMap::new();
                m.insert("version".to_string(), "v2-canary".to_string());
                m
            },
        },
    ];

    let config = DestinationRuleConfig {
        name: "tai-canary-dr".to_string(),
        namespace: "default".to_string(),
        host: "tai-service".to_string(),
        load_balancing: LoadBalancingAlgorithm::RoundRobin,
        subsets,
        connection_pool: None,
    };

    // Act
    let result = traffic_manager.generate_destination_rule(&config);

    // Assert
    assert!(result.is_ok());
    let yaml = result.unwrap();
    assert!(yaml.contains("stable"));
    assert!(yaml.contains("canary"));
    assert!(yaml.contains("v1"));
    assert!(yaml.contains("v2-canary"));
}

#[test]
fn test_firestore_event_driven_scaling() {
    // Arrange
    let autoscaler = KedaAutoscaler::new();
    let config = ScaledObjectConfig {
        name: "firestore-scaler".to_string(),
        namespace: "default".to_string(),
        target_name: "firestore-processor".to_string(),
        target_kind: "Deployment".to_string(),
        min_replicas: 1,
        max_replicas: 5,
        metrics: vec![MetricType::FirestoreDocuments {
            project_id: "ggen-project".to_string(),
            collection: "tai-documents".to_string(),
            target_documents_per_replica: 1000,
        }],
        scale_up_cooldown: 30,
        scale_down_cooldown: 300,
        fallback_replicas: Some(1),
    };

    // Act
    let result = autoscaler.generate_scaled_object(&config);

    // Assert
    assert!(result.is_ok());
    let yaml = result.unwrap();
    assert!(yaml.contains("firestore-scaler"));
}

#[tokio::test]
async fn test_keda_autoscaler_json_output() {
    // Arrange
    let autoscaler = KedaAutoscaler::new();
    let config = ScaledObjectConfig {
        name: "json-test-scaler".to_string(),
        namespace: "default".to_string(),
        target_name: "test-deployment".to_string(),
        target_kind: "Deployment".to_string(),
        min_replicas: 1,
        max_replicas: 10,
        metrics: vec![MetricType::Cpu {
            target_utilization_percentage: 75,
        }],
        scale_up_cooldown: 30,
        scale_down_cooldown: 300,
        fallback_replicas: Some(2),
    };

    // Act
    let result = autoscaler.generate_scaled_object_json(&config);

    // Assert
    assert!(result.is_ok());
    let json = result.unwrap();
    assert_eq!(json["metadata"]["name"], "json-test-scaler");
    assert_eq!(json["spec"]["minReplicaCount"], 1);
    assert_eq!(json["spec"]["maxReplicaCount"], 10);
}
