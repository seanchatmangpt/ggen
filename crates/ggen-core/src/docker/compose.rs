//! Docker Compose file generation from RDF
//!
//! This module provides the core DockerComposeGenerator that transforms
//! RDF service definitions into docker-compose.yml files.

use super::service_config::{extract_services, ServiceConfig};
use crate::graph::Graph;
use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Docker Compose generator
pub struct DockerComposeGenerator<'a> {
    graph: &'a Graph,
    namespace: String,
}

impl<'a> DockerComposeGenerator<'a> {
    /// Create a new Docker Compose generator
    pub fn new(graph: &'a Graph) -> Result<Self> {
        Ok(Self {
            graph,
            namespace: "http://ggen.io/docker#".to_string(),
        })
    }

    /// Create a generator with custom namespace
    pub fn with_namespace(graph: &'a Graph, namespace: impl Into<String>) -> Result<Self> {
        Ok(Self {
            graph,
            namespace: namespace.into(),
        })
    }

    /// Generate docker-compose.yml content
    pub async fn generate(&self) -> Result<String> {
        // Extract services from RDF
        let services = extract_services(self.graph, &self.namespace)?;

        if services.is_empty() {
            return Err(Error::invalid_input(
                "No services found in RDF graph. Ensure services are defined with docker:Service type.",
            ));
        }

        // Build compose structure
        let compose = DockerCompose::from_services(services)?;

        // Serialize to YAML
        let yaml = serde_yaml::to_string(&compose)
            .map_err(|e| Error::with_source("Failed to serialize Docker Compose YAML", Box::new(e)))?;

        Ok(yaml)
    }

    /// Generate and write docker-compose.yml to file
    pub async fn generate_to_file(&self, output_path: &std::path::Path) -> Result<()> {
        let yaml = self.generate().await?;

        std::fs::write(output_path, yaml).map_err(|e| {
            Error::with_source(
                &format!("Failed to write docker-compose.yml to {:?}", output_path),
                Box::new(e),
            )
        })?;

        Ok(())
    }

    /// Extract unique networks from services
    fn extract_networks(services: &[ServiceConfig]) -> Vec<String> {
        let mut networks = std::collections::HashSet::new();
        for service in services {
            for network in &service.networks {
                if network != "default" {
                    networks.insert(network.clone());
                }
            }
        }
        let mut result: Vec<String> = networks.into_iter().collect();
        result.sort();
        result
    }

    /// Extract unique volumes from services
    fn extract_volumes(services: &[ServiceConfig]) -> Vec<String> {
        let mut volumes = std::collections::HashSet::new();
        for service in services {
            for volume in &service.volumes {
                // Extract named volume (before colon)
                if let Some(vol_name) = volume.split(':').next() {
                    // Only include if it's a named volume (not a bind mount)
                    if !vol_name.starts_with('/') && !vol_name.starts_with('.') {
                        volumes.insert(vol_name.to_string());
                    }
                }
            }
        }
        let mut result: Vec<String> = volumes.into_iter().collect();
        result.sort();
        result
    }
}

/// Docker Compose file structure
#[derive(Debug, Serialize, Deserialize)]
struct DockerCompose {
    version: String,
    services: HashMap<String, ComposeService>,
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    networks: HashMap<String, NetworkConfig>,
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    volumes: HashMap<String, VolumeConfig>,
}

impl DockerCompose {
    fn from_services(services: Vec<ServiceConfig>) -> Result<Self> {
        let mut compose_services = HashMap::new();

        for service in &services {
            let compose_service = ComposeService::from_config(service)?;
            compose_services.insert(service.name.clone(), compose_service);
        }

        // Extract networks
        let networks: HashMap<String, NetworkConfig> = DockerComposeGenerator::extract_networks(&services)
            .into_iter()
            .map(|name| (name, NetworkConfig::default()))
            .collect();

        // Extract volumes
        let volumes: HashMap<String, VolumeConfig> = DockerComposeGenerator::extract_volumes(&services)
            .into_iter()
            .map(|name| (name, VolumeConfig::default()))
            .collect();

        Ok(Self {
            version: "3.8".to_string(),
            services: compose_services,
            networks,
            volumes,
        })
    }
}

/// Docker Compose service definition
#[derive(Debug, Serialize, Deserialize)]
struct ComposeService {
    image: String,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    ports: Vec<String>,
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    environment: HashMap<String, String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    volumes: Vec<String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    networks: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    healthcheck: Option<ComposeHealthCheck>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    depends_on: Vec<String>,
    restart: String,
}

impl ComposeService {
    fn from_config(config: &ServiceConfig) -> Result<Self> {
        config.validate()?;

        let ports = config
            .ports
            .iter()
            .map(|p| p.to_compose_format())
            .collect();

        let healthcheck = config.health_check.as_ref().map(|hc| ComposeHealthCheck {
            test: hc.test.clone(),
            interval: hc.interval.clone(),
            timeout: hc.timeout.clone(),
            retries: hc.retries,
            start_period: hc.start_period.clone(),
        });

        Ok(Self {
            image: config.image.clone(),
            ports,
            environment: config.environment.clone(),
            volumes: config.volumes.clone(),
            networks: config.networks.clone(),
            healthcheck,
            depends_on: config.depends_on.clone(),
            restart: config.restart.clone(),
        })
    }
}

/// Docker Compose health check
#[derive(Debug, Serialize, Deserialize)]
struct ComposeHealthCheck {
    test: Vec<String>,
    interval: String,
    timeout: String,
    retries: u32,
    start_period: String,
}

/// Network configuration
#[derive(Debug, Default, Serialize, Deserialize)]
struct NetworkConfig {
    driver: String,
}

impl NetworkConfig {
    fn default() -> Self {
        Self {
            driver: "bridge".to_string(),
        }
    }
}

/// Volume configuration
#[derive(Debug, Default, Serialize, Deserialize)]
struct VolumeConfig {
    driver: String,
}

impl VolumeConfig {
    fn default() -> Self {
        Self {
            driver: "local".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::docker::service_config::{HealthCheck, ServicePort};

    #[test]
    fn test_docker_compose_from_services() {
        // Arrange
        let services = vec![
            ServiceConfig::new("redis", "redis:7-alpine")
                .with_port(ServicePort::tcp(6379, 6379))
                .with_health_check(HealthCheck::redis())
                .with_network("backend"),
            ServiceConfig::new("postgres", "postgres:15-alpine")
                .with_port(ServicePort::tcp(5432, 5432))
                .with_env("POSTGRES_PASSWORD", "secret")
                .with_volume("postgres-data:/var/lib/postgresql/data")
                .with_health_check(HealthCheck::postgres("postgres"))
                .with_network("backend"),
        ];

        // Act
        let compose = DockerCompose::from_services(services);

        // Assert
        assert!(compose.is_ok());
        let compose = compose.unwrap();
        assert_eq!(compose.version, "3.8");
        assert_eq!(compose.services.len(), 2);
        assert!(compose.services.contains_key("redis"));
        assert!(compose.services.contains_key("postgres"));
        assert_eq!(compose.networks.len(), 1);
        assert!(compose.networks.contains_key("backend"));
        assert_eq!(compose.volumes.len(), 1);
        assert!(compose.volumes.contains_key("postgres-data"));
    }

    #[test]
    fn test_compose_service_from_config() {
        // Arrange
        let config = ServiceConfig::new("redis", "redis:7-alpine")
            .with_port(ServicePort::tcp(6379, 6379))
            .with_env("REDIS_PASSWORD", "secret")
            .with_volume("redis-data:/data")
            .with_health_check(HealthCheck::redis())
            .with_dependency("postgres")
            .with_restart("always");

        // Act
        let result = ComposeService::from_config(&config);

        // Assert
        assert!(result.is_ok());
        let service = result.unwrap();
        assert_eq!(service.image, "redis:7-alpine");
        assert_eq!(service.ports, vec!["6379:6379"]);
        assert_eq!(service.environment.get("REDIS_PASSWORD"), Some(&"secret".to_string()));
        assert_eq!(service.volumes, vec!["redis-data:/data"]);
        assert!(service.healthcheck.is_some());
        assert_eq!(service.depends_on, vec!["postgres"]);
        assert_eq!(service.restart, "always");
    }

    #[tokio::test]
    async fn test_docker_compose_generator_with_services() {
        // Arrange
        let graph = Graph::new().expect("Failed to create graph");
        graph
            .insert_turtle(
                r#"
                @prefix docker: <http://ggen.io/docker#> .

                <http://example.org/redis> a docker:Service ;
                    docker:name "redis" ;
                    docker:image "redis:7-alpine" ;
                    docker:port "6379" ;
                    docker:healthCheck "redis-cli ping" .
                "#,
            )
            .expect("Failed to insert RDF");

        let generator = DockerComposeGenerator::new(&graph).expect("Failed to create generator");

        // Act
        let result = generator.generate().await;

        // Assert
        assert!(result.is_ok());
        let yaml = result.unwrap();
        assert!(yaml.contains("version:"));
        assert!(yaml.contains("services:"));
        assert!(yaml.contains("redis:"));
        assert!(yaml.contains("image: redis:7-alpine"));
        assert!(yaml.contains("6379:6379"));
    }

    #[tokio::test]
    async fn test_docker_compose_generator_no_services() {
        // Arrange
        let graph = Graph::new().expect("Failed to create graph");
        let generator = DockerComposeGenerator::new(&graph).expect("Failed to create generator");

        // Act
        let result = generator.generate().await;

        // Assert
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("No services found"));
    }

    #[test]
    fn test_extract_networks() {
        // Arrange
        let services = vec![
            ServiceConfig::new("redis", "redis:7")
                .with_network("backend")
                .with_network("cache"),
            ServiceConfig::new("postgres", "postgres:15")
                .with_network("backend")
                .with_network("database"),
        ];

        // Act
        let networks = DockerComposeGenerator::extract_networks(&services);

        // Assert
        assert_eq!(networks.len(), 3);
        assert!(networks.contains(&"backend".to_string()));
        assert!(networks.contains(&"cache".to_string()));
        assert!(networks.contains(&"database".to_string()));
    }

    #[test]
    fn test_extract_volumes() {
        // Arrange
        let services = vec![
            ServiceConfig::new("redis", "redis:7")
                .with_volume("redis-data:/data"),
            ServiceConfig::new("postgres", "postgres:15")
                .with_volume("postgres-data:/var/lib/postgresql/data")
                .with_volume("./config:/etc/postgresql"), // bind mount, should be excluded
        ];

        // Act
        let volumes = DockerComposeGenerator::extract_volumes(&services);

        // Assert
        assert_eq!(volumes.len(), 2);
        assert!(volumes.contains(&"redis-data".to_string()));
        assert!(volumes.contains(&"postgres-data".to_string()));
    }

    #[test]
    fn test_network_config_default() {
        // Arrange & Act
        let network = NetworkConfig::default();

        // Assert
        assert_eq!(network.driver, "bridge");
    }

    #[test]
    fn test_volume_config_default() {
        // Arrange & Act
        let volume = VolumeConfig::default();

        // Assert
        assert_eq!(volume.driver, "local");
    }
}
