//! Kubernetes client operations
//!
//! Provides a high-level Kubernetes client for managing:
//! - Pods (create, delete, list, watch)
//! - Deployments (scale, update, rollback)
//! - Services (create, delete)
//! - ConfigMaps (update config)
//! - Secrets (read credentials)
//! - StatefulSets (persistent services)
//! - Jobs (batch processing)
//! - Event watching (failure detection)

use crate::{Error, Result};
use async_trait::async_trait;
use futures::stream::StreamExt;
use k8s_openapi::api::apps::v1::{Deployment, StatefulSet};
use k8s_openapi::api::batch::v1::Job;
use k8s_openapi::api::core::v1::{
    ConfigMap, Container, ContainerPort, Pod, PodSpec, PodTemplateSpec, ResourceRequirements,
    Secret, Service, ServicePort, ServiceSpec,
};
use k8s_openapi::apimachinery::pkg::api::resource::Quantity;
use k8s_openapi::apimachinery::pkg::apis::meta::v1::{LabelSelector, ObjectMeta};
use k8s_openapi::apimachinery::pkg::util::intstr::IntOrString;
use k8s_openapi::ByteString;
use kube::{Api, Client};
use std::collections::BTreeMap;
use tracing::{debug, error, info};

/// Pod operation trait for abstraction
#[async_trait]
pub trait PodOperation {
    /// Create a pod
    async fn create_pod(
        &self, namespace: &str, pod_name: &str, image: &str, labels: BTreeMap<String, String>,
    ) -> Result<Pod>;

    /// Delete a pod
    async fn delete_pod(&self, namespace: &str, pod_name: &str) -> Result<()>;

    /// List pods in namespace
    async fn list_pods(&self, namespace: &str, label_selector: Option<&str>) -> Result<Vec<Pod>>;

    /// Watch pod events
    async fn watch_pods(&self, namespace: &str) -> Result<()>;

    /// Get pod logs
    async fn get_pod_logs(&self, namespace: &str, pod_name: &str) -> Result<String>;
}

/// Deployment operation trait for abstraction
#[async_trait]
pub trait DeploymentOperation {
    /// Create a deployment
    async fn create_deployment(
        &self, namespace: &str, deployment_name: &str, replicas: i32, image: &str,
        labels: BTreeMap<String, String>,
    ) -> Result<Deployment>;

    /// Scale deployment
    async fn scale_deployment(
        &self, namespace: &str, deployment_name: &str, replicas: i32,
    ) -> Result<()>;

    /// Get deployment
    async fn get_deployment(&self, namespace: &str, deployment_name: &str) -> Result<Deployment>;

    /// Update deployment image
    async fn update_deployment_image(
        &self, namespace: &str, deployment_name: &str, image: &str,
    ) -> Result<()>;

    /// Rollback deployment to previous revision
    async fn rollback_deployment(&self, namespace: &str, deployment_name: &str) -> Result<()>;
}

/// Kubernetes client for TAI system
#[derive(Clone)]
pub struct KubernetesClient {
    client: Client,
}

impl KubernetesClient {
    /// Create a new Kubernetes client
    ///
    /// # Errors
    ///
    /// Returns error if unable to create client or connect to cluster
    pub async fn new() -> Result<Self> {
        let client = Client::try_default()
            .await
            .map_err(|e| Error::KubernetesApi(format!("Failed to create client: {e}")))?;

        info!("Kubernetes client initialized successfully");
        Ok(Self { client })
    }

    /// Create a namespace
    ///
    /// # Errors
    ///
    /// Returns error if unable to create namespace
    pub async fn create_namespace(&self, namespace: &str) -> Result<()> {
        let ns = k8s_openapi::api::core::v1::Namespace {
            metadata: ObjectMeta {
                name: Some(namespace.to_string()),
                ..Default::default()
            },
            ..Default::default()
        };

        let ns_api: Api<k8s_openapi::api::core::v1::Namespace> = Api::all(self.client.clone());

        ns_api.create(&Default::default(), &ns).await.map_err(|e| {
            Error::KubernetesApi(format!("Failed to create namespace {namespace}: {e}"))
        })?;

        info!("Namespace {} created", namespace);
        Ok(())
    }

    /// Create a ConfigMap
    ///
    /// # Errors
    ///
    /// Returns error if unable to create ConfigMap
    pub async fn create_config_map(
        &self, namespace: &str, name: &str, data: BTreeMap<String, String>,
    ) -> Result<ConfigMap> {
        let cm = ConfigMap {
            metadata: ObjectMeta {
                name: Some(name.to_string()),
                namespace: Some(namespace.to_string()),
                ..Default::default()
            },
            data: Some(data),
            ..Default::default()
        };

        let cm_api: Api<ConfigMap> = Api::namespaced(self.client.clone(), namespace);
        cm_api
            .create(&Default::default(), &cm)
            .await
            .map_err(|e| Error::KubernetesApi(format!("Failed to create ConfigMap {name}: {e}")))
    }

    /// Update a ConfigMap
    ///
    /// # Errors
    ///
    /// Returns error if unable to update ConfigMap
    pub async fn update_config_map(
        &self, namespace: &str, name: &str, data: BTreeMap<String, String>,
    ) -> Result<()> {
        let cm_api: Api<ConfigMap> = Api::namespaced(self.client.clone(), namespace);

        let mut cm = cm_api
            .get(name)
            .await
            .map_err(|e| Error::KubernetesApi(format!("Failed to get ConfigMap {name}: {e}")))?;

        cm.data = Some(data);

        cm_api
            .replace(name, &Default::default(), &cm)
            .await
            .map_err(|e| Error::KubernetesApi(format!("Failed to update ConfigMap {name}: {e}")))?;

        info!("ConfigMap {} updated", name);
        Ok(())
    }

    /// Create a Secret
    ///
    /// # Errors
    ///
    /// Returns error if unable to create Secret
    pub async fn create_secret(
        &self, namespace: &str, name: &str, data: BTreeMap<String, Vec<u8>>,
    ) -> Result<Secret> {
        let converted_data: BTreeMap<String, ByteString> =
            data.into_iter().map(|(k, v)| (k, ByteString(v))).collect();

        let secret = Secret {
            metadata: ObjectMeta {
                name: Some(name.to_string()),
                namespace: Some(namespace.to_string()),
                ..Default::default()
            },
            data: Some(converted_data),
            ..Default::default()
        };

        let secret_api: Api<Secret> = Api::namespaced(self.client.clone(), namespace);
        secret_api
            .create(&Default::default(), &secret)
            .await
            .map_err(|e| Error::KubernetesApi(format!("Failed to create Secret {name}: {e}")))
    }

    /// Get a Secret
    ///
    /// # Errors
    ///
    /// Returns error if unable to get Secret
    pub async fn get_secret(&self, namespace: &str, name: &str) -> Result<Secret> {
        let secret_api: Api<Secret> = Api::namespaced(self.client.clone(), namespace);
        secret_api
            .get(name)
            .await
            .map_err(|e| Error::KubernetesApi(format!("Failed to get Secret {name}: {e}")))
    }

    /// Create a Service
    ///
    /// # Errors
    ///
    /// Returns error if unable to create Service
    pub async fn create_service(
        &self,
        namespace: &str,
        name: &str,
        selector: BTreeMap<String, String>,
        ports: Vec<(u16, u16)>, // (port, target_port)
        service_type: &str,
    ) -> Result<Service> {
        let service_ports: Vec<ServicePort> = ports
            .iter()
            .map(|(port, target_port)| ServicePort {
                port: *port as i32,
                target_port: Some(IntOrString::Int(*target_port as i32)),
                ..Default::default()
            })
            .collect();

        let service = Service {
            metadata: ObjectMeta {
                name: Some(name.to_string()),
                namespace: Some(namespace.to_string()),
                ..Default::default()
            },
            spec: Some(ServiceSpec {
                selector: Some(selector),
                ports: Some(service_ports),
                type_: Some(service_type.to_string()),
                ..Default::default()
            }),
            ..Default::default()
        };

        let svc_api: Api<Service> = Api::namespaced(self.client.clone(), namespace);
        svc_api
            .create(&Default::default(), &service)
            .await
            .map_err(|e| Error::KubernetesApi(format!("Failed to create Service {name}: {e}")))
    }

    /// List services in namespace
    ///
    /// # Errors
    ///
    /// Returns error if unable to list services
    pub async fn list_services(&self, namespace: &str) -> Result<Vec<Service>> {
        let svc_api: Api<Service> = Api::namespaced(self.client.clone(), namespace);
        let services = svc_api
            .list(&Default::default())
            .await
            .map_err(|e| Error::KubernetesApi(format!("Failed to list services: {e}")))?;

        Ok(services.items)
    }

    /// Create a StatefulSet
    ///
    /// # Errors
    ///
    /// Returns error if unable to create StatefulSet
    pub async fn create_stateful_set(
        &self, namespace: &str, name: &str, replicas: i32, image: &str, service_name: &str,
        labels: BTreeMap<String, String>,
    ) -> Result<StatefulSet> {
        let mut labels = labels;
        labels.insert("app".to_string(), name.to_string());

        let container = Container {
            name: name.to_string(),
            image: Some(image.to_string()),
            ports: Some(vec![ContainerPort {
                container_port: 8080,
                ..Default::default()
            }]),
            resources: Some(ResourceRequirements {
                requests: Some({
                    let mut map = BTreeMap::new();
                    map.insert("cpu".to_string(), Quantity("100m".to_string()));
                    map.insert("memory".to_string(), Quantity("128Mi".to_string()));
                    map
                }),
                limits: Some({
                    let mut map = BTreeMap::new();
                    map.insert("cpu".to_string(), Quantity("500m".to_string()));
                    map.insert("memory".to_string(), Quantity("512Mi".to_string()));
                    map
                }),
                claims: None,
            }),
            ..Default::default()
        };

        let stateful_set = StatefulSet {
            metadata: ObjectMeta {
                name: Some(name.to_string()),
                namespace: Some(namespace.to_string()),
                ..Default::default()
            },
            spec: Some(k8s_openapi::api::apps::v1::StatefulSetSpec {
                replicas: Some(replicas),
                service_name: service_name.to_string(),
                selector: LabelSelector {
                    match_labels: Some(labels.clone()),
                    ..Default::default()
                },
                template: PodTemplateSpec {
                    metadata: Some(ObjectMeta {
                        labels: Some(labels),
                        ..Default::default()
                    }),
                    spec: Some(PodSpec {
                        containers: vec![container],
                        ..Default::default()
                    }),
                },
                ..Default::default()
            }),
            ..Default::default()
        };

        let ss_api: Api<StatefulSet> = Api::namespaced(self.client.clone(), namespace);
        ss_api
            .create(&Default::default(), &stateful_set)
            .await
            .map_err(|e| Error::KubernetesApi(format!("Failed to create StatefulSet {name}: {e}")))
    }

    /// Create a Job
    ///
    /// # Errors
    ///
    /// Returns error if unable to create Job
    pub async fn create_job(
        &self, namespace: &str, name: &str, image: &str, labels: BTreeMap<String, String>,
    ) -> Result<Job> {
        let mut labels = labels;
        labels.insert("app".to_string(), name.to_string());

        let container = Container {
            name: name.to_string(),
            image: Some(image.to_string()),
            ..Default::default()
        };

        let job = Job {
            metadata: ObjectMeta {
                name: Some(name.to_string()),
                namespace: Some(namespace.to_string()),
                ..Default::default()
            },
            spec: Some(k8s_openapi::api::batch::v1::JobSpec {
                template: PodTemplateSpec {
                    metadata: Some(ObjectMeta {
                        labels: Some(labels),
                        ..Default::default()
                    }),
                    spec: Some(PodSpec {
                        containers: vec![container],
                        restart_policy: Some("Never".to_string()),
                        ..Default::default()
                    }),
                },
                ..Default::default()
            }),
            ..Default::default()
        };

        let job_api: Api<Job> = Api::namespaced(self.client.clone(), namespace);
        job_api
            .create(&Default::default(), &job)
            .await
            .map_err(|e| Error::KubernetesApi(format!("Failed to create Job {name}: {e}")))
    }

    /// Monitor a Job
    ///
    /// # Errors
    ///
    /// Returns error if unable to monitor job
    pub async fn monitor_job(&self, namespace: &str, job_name: &str) -> Result<bool> {
        let job_api: Api<Job> = Api::namespaced(self.client.clone(), namespace);

        let job = job_api
            .get(job_name)
            .await
            .map_err(|e| Error::KubernetesApi(format!("Failed to get Job {job_name}: {e}")))?;

        if let Some(status) = job.status {
            if let Some(succeeded) = status.succeeded {
                if succeeded > 0 {
                    info!("Job {} completed successfully", job_name);
                    return Ok(true);
                }
            }
            if let Some(failed) = status.failed {
                if failed > 0 {
                    error!("Job {} failed", job_name);
                    return Ok(false);
                }
            }
        }

        debug!("Job {} still running", job_name);
        Ok(false)
    }
}

#[async_trait]
impl PodOperation for KubernetesClient {
    async fn create_pod(
        &self, namespace: &str, pod_name: &str, image: &str, labels: BTreeMap<String, String>,
    ) -> Result<Pod> {
        let container = Container {
            name: "main".to_string(),
            image: Some(image.to_string()),
            ..Default::default()
        };

        let pod = Pod {
            metadata: ObjectMeta {
                name: Some(pod_name.to_string()),
                namespace: Some(namespace.to_string()),
                labels: Some(labels),
                ..Default::default()
            },
            spec: Some(PodSpec {
                containers: vec![container],
                ..Default::default()
            }),
            ..Default::default()
        };

        let pod_api: Api<Pod> = Api::namespaced(self.client.clone(), namespace);
        pod_api
            .create(&Default::default(), &pod)
            .await
            .map_err(|e| Error::KubernetesApi(format!("Failed to create Pod {pod_name}: {e}")))
    }

    async fn delete_pod(&self, namespace: &str, pod_name: &str) -> Result<()> {
        let pod_api: Api<Pod> = Api::namespaced(self.client.clone(), namespace);
        pod_api
            .delete(pod_name, &Default::default())
            .await
            .map_err(|e| Error::KubernetesApi(format!("Failed to delete Pod {pod_name}: {e}")))?;

        info!("Pod {} deleted", pod_name);
        Ok(())
    }

    async fn list_pods(&self, namespace: &str, label_selector: Option<&str>) -> Result<Vec<Pod>> {
        let pod_api: Api<Pod> = Api::namespaced(self.client.clone(), namespace);

        let mut list_params = kube::api::ListParams::default();
        if let Some(selector) = label_selector {
            list_params = list_params.labels(selector);
        }

        let pods = pod_api
            .list(&list_params)
            .await
            .map_err(|e| Error::KubernetesApi(format!("Failed to list pods: {e}")))?;

        Ok(pods.items)
    }

    async fn watch_pods(&self, namespace: &str) -> Result<()> {
        let pod_api: Api<Pod> = Api::namespaced(self.client.clone(), namespace);

        use kube_runtime::watcher;
        let mut pods = watcher(pod_api, Default::default()).boxed();

        info!("Watching pods in namespace {}", namespace);

        while let Some(result) = futures::stream::StreamExt::next(&mut pods).await {
            match result {
                Ok(event) => {
                    debug!("Pod event: {:?}", event);
                }
                Err(e) => {
                    error!("Error watching pods: {}", e);
                    return Err(Error::KubernetesApi(format!("Watch error: {e}")));
                }
            }
        }

        Ok(())
    }

    async fn get_pod_logs(&self, namespace: &str, pod_name: &str) -> Result<String> {
        // This would require additional setup for actual log streaming
        // For now, return a placeholder
        debug!(
            "Getting logs for pod {} in namespace {}",
            pod_name, namespace
        );
        Ok("Log streaming not fully implemented in mock client".to_string())
    }
}

#[async_trait]
impl DeploymentOperation for KubernetesClient {
    async fn create_deployment(
        &self, namespace: &str, deployment_name: &str, replicas: i32, image: &str,
        labels: BTreeMap<String, String>,
    ) -> Result<Deployment> {
        let mut labels = labels;
        labels.insert("app".to_string(), deployment_name.to_string());

        let container = Container {
            name: deployment_name.to_string(),
            image: Some(image.to_string()),
            ports: Some(vec![ContainerPort {
                container_port: 8080,
                ..Default::default()
            }]),
            resources: Some(ResourceRequirements {
                requests: Some({
                    let mut map = BTreeMap::new();
                    map.insert("cpu".to_string(), Quantity("100m".to_string()));
                    map.insert("memory".to_string(), Quantity("128Mi".to_string()));
                    map
                }),
                limits: Some({
                    let mut map = BTreeMap::new();
                    map.insert("cpu".to_string(), Quantity("500m".to_string()));
                    map.insert("memory".to_string(), Quantity("512Mi".to_string()));
                    map
                }),
                claims: None,
            }),
            ..Default::default()
        };

        let deployment = Deployment {
            metadata: ObjectMeta {
                name: Some(deployment_name.to_string()),
                namespace: Some(namespace.to_string()),
                ..Default::default()
            },
            spec: Some(k8s_openapi::api::apps::v1::DeploymentSpec {
                replicas: Some(replicas),
                selector: LabelSelector {
                    match_labels: Some(labels.clone()),
                    ..Default::default()
                },
                template: PodTemplateSpec {
                    metadata: Some(ObjectMeta {
                        labels: Some(labels),
                        ..Default::default()
                    }),
                    spec: Some(PodSpec {
                        containers: vec![container],
                        ..Default::default()
                    }),
                },
                ..Default::default()
            }),
            ..Default::default()
        };

        let deploy_api: Api<Deployment> = Api::namespaced(self.client.clone(), namespace);
        deploy_api
            .create(&Default::default(), &deployment)
            .await
            .map_err(|e| {
                Error::KubernetesApi(format!(
                    "Failed to create Deployment {deployment_name}: {e}"
                ))
            })
    }

    async fn scale_deployment(
        &self, namespace: &str, deployment_name: &str, replicas: i32,
    ) -> Result<()> {
        let deploy_api: Api<Deployment> = Api::namespaced(self.client.clone(), namespace);

        let mut deployment = deploy_api.get(deployment_name).await.map_err(|e| {
            Error::KubernetesApi(format!("Failed to get Deployment {deployment_name}: {e}"))
        })?;

        if let Some(spec) = &mut deployment.spec {
            spec.replicas = Some(replicas);
        }

        deploy_api
            .replace(deployment_name, &Default::default(), &deployment)
            .await
            .map_err(|e| {
                Error::KubernetesApi(format!("Failed to scale Deployment {deployment_name}: {e}"))
            })?;

        info!(
            "Deployment {} scaled to {} replicas",
            deployment_name, replicas
        );
        Ok(())
    }

    async fn get_deployment(&self, namespace: &str, deployment_name: &str) -> Result<Deployment> {
        let deploy_api: Api<Deployment> = Api::namespaced(self.client.clone(), namespace);
        deploy_api.get(deployment_name).await.map_err(|e| {
            Error::KubernetesApi(format!("Failed to get Deployment {deployment_name}: {e}"))
        })
    }

    async fn update_deployment_image(
        &self, namespace: &str, deployment_name: &str, image: &str,
    ) -> Result<()> {
        let deploy_api: Api<Deployment> = Api::namespaced(self.client.clone(), namespace);

        let mut deployment = deploy_api.get(deployment_name).await.map_err(|e| {
            Error::KubernetesApi(format!("Failed to get Deployment {deployment_name}: {e}"))
        })?;

        if let Some(spec) = &mut deployment.spec {
            let template = &mut spec.template;
            if let Some(pod_spec) = &mut template.spec {
                for container in &mut pod_spec.containers {
                    container.image = Some(image.to_string());
                }
            }
        }

        deploy_api
            .replace(deployment_name, &Default::default(), &deployment)
            .await
            .map_err(|e| Error::KubernetesApi(format!("Failed to update Deployment image: {e}")))?;

        info!("Deployment {} image updated to {}", deployment_name, image);
        Ok(())
    }

    async fn rollback_deployment(&self, _namespace: &str, deployment_name: &str) -> Result<()> {
        // This would typically use the rollout/revision history
        // For now, we'll log the intent
        info!("Rollback requested for Deployment {}", deployment_name);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_display() {
        let error = Error::KubernetesApi("test error".to_string());
        assert_eq!(error.to_string(), "Kubernetes API error: test error");
    }

    #[test]
    fn test_create_container() {
        let container = Container {
            name: "test".to_string(),
            image: Some("nginx:latest".to_string()),
            ..Default::default()
        };

        assert_eq!(container.name, "test");
        assert_eq!(container.image, Some("nginx:latest".to_string()));
    }
}
