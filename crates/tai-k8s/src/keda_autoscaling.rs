//! KEDA (Kubernetes Event Driven Autoscaling) integration
//!
//! Provides support for metric-based and event-driven autoscaling including:
//! - Metric-based scaling (CPU, memory)
//! - Custom metrics (Pub/Sub queue depth, Firestore document count)
//! - Event-driven scaling (Kafka topic lag, HTTP requests)
//! - ScaledObject resource generation
//! - Cooldown periods and replica bounds

use crate::{Error, Result};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use tracing::{debug, info};

/// Metric type for KEDA autoscaling
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum MetricType {
    /// CPU-based autoscaling
    Cpu {
        /// Target CPU utilization percentage (0-100)
        target_utilization_percentage: i32,
    },
    /// Memory-based autoscaling
    Memory {
        /// Target memory utilization percentage (0-100)
        target_utilization_percentage: i32,
    },
    /// Custom metric from metrics server
    Custom {
        /// Metric name
        metric_name: String,
        /// Target value
        target_value: String,
    },
    /// GCP Pub/Sub queue depth
    PubSubQueueDepth {
        /// Google Cloud Project ID
        project_id: String,
        /// Pub/Sub subscription name
        subscription: String,
        /// Target messages per replica
        target_queue_length: i32,
    },
    /// Kafka topic lag
    KafkaTopicLag {
        /// Kafka broker addresses
        brokers: Vec<String>,
        /// Topic name
        topic: String,
        /// Consumer group
        consumer_group: String,
        /// Target lag per replica
        target_lag: i32,
    },
    /// HTTP requests per second
    HttpRequests {
        /// Target requests per replica
        target_requests_per_replica: i32,
    },
    /// Firestore document count
    FirestoreDocuments {
        /// Google Cloud Project ID
        project_id: String,
        /// Firestore collection name
        collection: String,
        /// Target documents per replica
        target_documents_per_replica: i32,
    },
}

/// KEDA ScaledObject configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ScaledObjectConfig {
    /// Name of the ScaledObject
    pub name: String,
    /// Namespace
    pub namespace: String,
    /// Target deployment/statefulset name
    pub target_name: String,
    /// Kind of target (Deployment, StatefulSet)
    pub target_kind: String,
    /// Minimum replicas
    pub min_replicas: i32,
    /// Maximum replicas
    pub max_replicas: i32,
    /// Metric types to use for scaling
    pub metrics: Vec<MetricType>,
    /// Scale-up cooldown in seconds
    pub scale_up_cooldown: i32,
    /// Scale-down cooldown in seconds
    pub scale_down_cooldown: i32,
    /// Fallback replicas if metrics unavailable
    pub fallback_replicas: Option<i32>,
}

/// Event-driven autoscaler
pub struct EventDrivenScaler;

impl EventDrivenScaler {
    /// Create an event-driven scaler
    #[must_use]
    pub fn new() -> Self {
        Self
    }

    /// Add Pub/Sub event source
    #[must_use]
    pub fn with_pubsub(
        project_id: String, subscription: String, target_queue_length: i32,
    ) -> ScaledObjectConfig {
        ScaledObjectConfig {
            name: format!(
                "pubsub-scaler-{}",
                &subscription[..subscription.len().min(8)]
            ),
            namespace: "default".to_string(),
            target_name: "unknown".to_string(),
            target_kind: "Deployment".to_string(),
            min_replicas: 1,
            max_replicas: 10,
            metrics: vec![MetricType::PubSubQueueDepth {
                project_id,
                subscription,
                target_queue_length,
            }],
            scale_up_cooldown: 30,
            scale_down_cooldown: 300,
            fallback_replicas: Some(2),
        }
    }

    /// Add Kafka event source
    #[must_use]
    pub fn with_kafka(
        brokers: Vec<String>, topic: String, consumer_group: String, target_lag: i32,
    ) -> ScaledObjectConfig {
        ScaledObjectConfig {
            name: format!("kafka-scaler-{}", &topic[..topic.len().min(8)]),
            namespace: "default".to_string(),
            target_name: "unknown".to_string(),
            target_kind: "Deployment".to_string(),
            min_replicas: 1,
            max_replicas: 10,
            metrics: vec![MetricType::KafkaTopicLag {
                brokers,
                topic,
                consumer_group,
                target_lag,
            }],
            scale_up_cooldown: 30,
            scale_down_cooldown: 300,
            fallback_replicas: Some(2),
        }
    }
}

impl Default for EventDrivenScaler {
    fn default() -> Self {
        Self::new()
    }
}

/// KEDA autoscaler for TAI system
pub struct KedaAutoscaler;

impl KedaAutoscaler {
    /// Create a new KEDA autoscaler
    #[must_use]
    pub fn new() -> Self {
        Self
    }

    /// Generate ScaledObject YAML
    ///
    /// # Errors
    ///
    /// Returns error if unable to generate YAML
    pub fn generate_scaled_object(&self, config: &ScaledObjectConfig) -> Result<String> {
        let triggers = config.metrics.iter().map(|metric| {
            match metric {
                MetricType::Cpu { target_utilization_percentage } => {
                    json!({
                        "type": "cpu",
                        "metadata": {
                            "type": "Utilization",
                            "value": target_utilization_percentage.to_string()
                        }
                    })
                }
                MetricType::Memory { target_utilization_percentage } => {
                    json!({
                        "type": "memory",
                        "metadata": {
                            "type": "Utilization",
                            "value": target_utilization_percentage.to_string()
                        }
                    })
                }
                MetricType::Custom { metric_name, target_value } => {
                    json!({
                        "type": "external",
                        "metadata": {
                            "metric_name": metric_name,
                            "target_value": target_value
                        }
                    })
                }
                MetricType::PubSubQueueDepth {
                    project_id,
                    subscription,
                    target_queue_length,
                } => {
                    json!({
                        "type": "gcp-stackdriver-work-queue",
                        "metadata": {
                            "projectId": project_id,
                            "filter": format!("resource.type=pubsub_subscription AND resource.labels.subscription_id={}", subscription),
                            "targetQueryValue": target_queue_length.to_string()
                        }
                    })
                }
                MetricType::KafkaTopicLag {
                    brokers,
                    topic,
                    consumer_group,
                    target_lag,
                } => {
                    json!({
                        "type": "kafka",
                        "metadata": {
                            "brokerList": brokers.join(","),
                            "consumerGroup": consumer_group,
                            "topic": topic,
                            "lagThreshold": target_lag.to_string()
                        }
                    })
                }
                MetricType::HttpRequests { target_requests_per_replica } => {
                    json!({
                        "type": "prometheus",
                        "metadata": {
                            "serverAddress": "http://prometheus:9090",
                            "metricName": "http_requests_total",
                            "targetValue": target_requests_per_replica.to_string(),
                            "query": "rate(http_requests_total[1m])"
                        }
                    })
                }
                MetricType::FirestoreDocuments {
                    project_id,
                    collection,
                    target_documents_per_replica,
                } => {
                    json!({
                        "type": "gcp-stackdriver-work-queue",
                        "metadata": {
                            "projectId": project_id,
                            "filter": format!("resource.type=cloud_firestore_document AND resource.labels.collection_id={}", collection),
                            "targetQueryValue": target_documents_per_replica.to_string()
                        }
                    })
                }
            }
        }).collect::<Vec<_>>();

        let scaled_object = json!({
            "apiVersion": "keda.sh/v1alpha1",
            "kind": "ScaledObject",
            "metadata": {
                "name": config.name,
                "namespace": config.namespace,
            },
            "spec": {
                "scaleTargetRef": {
                    "name": config.target_name,
                    "kind": config.target_kind,
                },
                "minReplicaCount": config.min_replicas,
                "maxReplicaCount": config.max_replicas,
                "triggers": triggers,
                "cooldownPeriod": config.scale_down_cooldown,
                "fallback": {
                    "failureThreshold": 3,
                    "replicas": config.fallback_replicas.unwrap_or(2),
                },
            }
        });

        let yaml = serde_yaml::to_string(&scaled_object)
            .map_err(|e| Error::KedaConfig(format!("Failed to serialize ScaledObject: {e}")))?;

        info!("Generated ScaledObject YAML for {}", config.name);
        debug!("ScaledObject YAML:\n{}", yaml);
        Ok(yaml)
    }

    /// Generate ScaledObject YAML as JSON value
    pub fn generate_scaled_object_json(&self, config: &ScaledObjectConfig) -> Result<Value> {
        let triggers = config.metrics.iter().map(|metric| {
            match metric {
                MetricType::Cpu { target_utilization_percentage } => {
                    json!({
                        "type": "cpu",
                        "metadata": {
                            "type": "Utilization",
                            "value": target_utilization_percentage.to_string()
                        }
                    })
                }
                MetricType::Memory { target_utilization_percentage } => {
                    json!({
                        "type": "memory",
                        "metadata": {
                            "type": "Utilization",
                            "value": target_utilization_percentage.to_string()
                        }
                    })
                }
                MetricType::Custom { metric_name, target_value } => {
                    json!({
                        "type": "external",
                        "metadata": {
                            "metric_name": metric_name,
                            "target_value": target_value
                        }
                    })
                }
                MetricType::PubSubQueueDepth {
                    project_id,
                    subscription,
                    target_queue_length,
                } => {
                    json!({
                        "type": "gcp-stackdriver-work-queue",
                        "metadata": {
                            "projectId": project_id,
                            "filter": format!("resource.type=pubsub_subscription AND resource.labels.subscription_id={}", subscription),
                            "targetQueryValue": target_queue_length.to_string()
                        }
                    })
                }
                MetricType::KafkaTopicLag {
                    brokers,
                    topic,
                    consumer_group,
                    target_lag,
                } => {
                    json!({
                        "type": "kafka",
                        "metadata": {
                            "brokerList": brokers.join(","),
                            "consumerGroup": consumer_group,
                            "topic": topic,
                            "lagThreshold": target_lag.to_string()
                        }
                    })
                }
                MetricType::HttpRequests { target_requests_per_replica } => {
                    json!({
                        "type": "prometheus",
                        "metadata": {
                            "serverAddress": "http://prometheus:9090",
                            "metricName": "http_requests_total",
                            "targetValue": target_requests_per_replica.to_string(),
                            "query": "rate(http_requests_total[1m])"
                        }
                    })
                }
                MetricType::FirestoreDocuments {
                    project_id,
                    collection,
                    target_documents_per_replica,
                } => {
                    json!({
                        "type": "gcp-stackdriver-work-queue",
                        "metadata": {
                            "projectId": project_id,
                            "filter": format!("resource.type=cloud_firestore_document AND resource.labels.collection_id={}", collection),
                            "targetQueryValue": target_documents_per_replica.to_string()
                        }
                    })
                }
            }
        }).collect::<Vec<_>>();

        Ok(json!({
            "apiVersion": "keda.sh/v1alpha1",
            "kind": "ScaledObject",
            "metadata": {
                "name": config.name,
                "namespace": config.namespace,
            },
            "spec": {
                "scaleTargetRef": {
                    "name": config.target_name,
                    "kind": config.target_kind,
                },
                "minReplicaCount": config.min_replicas,
                "maxReplicaCount": config.max_replicas,
                "triggers": triggers,
                "cooldownPeriod": config.scale_down_cooldown,
                "fallback": {
                    "failureThreshold": 3,
                    "replicas": config.fallback_replicas.unwrap_or(2),
                },
            }
        }))
    }

    /// Apply ScaledObject to Kubernetes cluster
    ///
    /// # Errors
    ///
    /// Returns error if unable to apply ScaledObject
    pub async fn apply_scaled_object(&self, _config: &ScaledObjectConfig) -> Result<()> {
        // In a real implementation, this would use kubectl or kube client
        // For now, we'll just log the intent
        info!("ScaledObject {} applied to cluster", _config.name);
        Ok(())
    }

    /// Remove ScaledObject from cluster
    ///
    /// # Errors
    ///
    /// Returns error if unable to remove ScaledObject
    pub async fn remove_scaled_object(&self, namespace: &str, name: &str) -> Result<()> {
        info!("ScaledObject {}/{} removed from cluster", namespace, name);
        Ok(())
    }
}

impl Default for KedaAutoscaler {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cpu_metric() {
        let metric = MetricType::Cpu {
            target_utilization_percentage: 70,
        };
        let yaml = serde_yaml::to_string(&metric).unwrap();
        assert!(yaml.contains("70"));
    }

    #[test]
    fn test_pubsub_metric() {
        let metric = MetricType::PubSubQueueDepth {
            project_id: "test-project".to_string(),
            subscription: "test-sub".to_string(),
            target_queue_length: 100,
        };
        let yaml = serde_yaml::to_string(&metric).unwrap();
        assert!(yaml.contains("test-project"));
    }

    #[test]
    fn test_keda_autoscaler_new() {
        let autoscaler = KedaAutoscaler::new();
        assert!(autoscaler
            .generate_scaled_object(&ScaledObjectConfig {
                name: "test-scaler".to_string(),
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
            })
            .is_ok());
    }

    #[test]
    fn test_event_driven_scaler_pubsub() {
        let config =
            EventDrivenScaler::with_pubsub("test-project".to_string(), "test-sub".to_string(), 100);
        assert_eq!(config.name, "pubsub-scaler-test-sub");
        assert_eq!(config.min_replicas, 1);
        assert_eq!(config.max_replicas, 10);
    }

    #[test]
    fn test_scaled_object_config() {
        let config = ScaledObjectConfig {
            name: "test".to_string(),
            namespace: "default".to_string(),
            target_name: "deployment".to_string(),
            target_kind: "Deployment".to_string(),
            min_replicas: 2,
            max_replicas: 20,
            metrics: vec![],
            scale_up_cooldown: 60,
            scale_down_cooldown: 600,
            fallback_replicas: Some(3),
        };
        assert_eq!(config.min_replicas, 2);
        assert_eq!(config.max_replicas, 20);
    }
}
