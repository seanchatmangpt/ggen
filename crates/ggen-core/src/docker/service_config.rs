//! Service configuration types and RDF extraction
//!
//! This module defines service configuration structures and provides
//! SPARQL-based extraction from RDF ontologies.

use crate::graph::Graph;
use ggen_utils::error::{Error, Result};
use oxigraph::sparql::QueryResults;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Docker service port configuration
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ServicePort {
    /// Host port (external)
    pub host: u16,
    /// Container port (internal)
    pub container: u16,
    /// Protocol (tcp/udp)
    pub protocol: String,
}

impl ServicePort {
    /// Create a new service port with TCP protocol
    pub fn tcp(host: u16, container: u16) -> Self {
        Self {
            host,
            container,
            protocol: "tcp".to_string(),
        }
    }

    /// Create a new service port with UDP protocol
    pub fn udp(host: u16, container: u16) -> Self {
        Self {
            host,
            container,
            protocol: "udp".to_string(),
        }
    }

    /// Format port mapping for docker-compose
    pub fn to_compose_format(&self) -> String {
        format!("{}:{}", self.host, self.container)
    }
}

/// Docker health check configuration
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct HealthCheck {
    /// Health check command
    pub test: Vec<String>,
    /// Interval between health checks
    pub interval: String,
    /// Timeout for health check
    pub timeout: String,
    /// Number of retries before marking unhealthy
    pub retries: u32,
    /// Start period before health checks begin
    pub start_period: String,
}

impl Default for HealthCheck {
    fn default() -> Self {
        Self {
            test: vec!["CMD".to_string(), "true".to_string()],
            interval: "30s".to_string(),
            timeout: "10s".to_string(),
            retries: 3,
            start_period: "40s".to_string(),
        }
    }
}

impl HealthCheck {
    /// Create Redis health check
    pub fn redis() -> Self {
        Self {
            test: vec!["CMD".to_string(), "redis-cli".to_string(), "ping".to_string()],
            interval: "10s".to_string(),
            timeout: "5s".to_string(),
            retries: 5,
            start_period: "30s".to_string(),
        }
    }

    /// Create PostgreSQL health check
    pub fn postgres(db_name: &str) -> Self {
        Self {
            test: vec![
                "CMD-SHELL".to_string(),
                format!("pg_isready -U postgres -d {}", db_name),
            ],
            interval: "10s".to_string(),
            timeout: "5s".to_string(),
            retries: 5,
            start_period: "30s".to_string(),
        }
    }

    /// Create HTTP health check
    pub fn http(endpoint: &str) -> Self {
        Self {
            test: vec![
                "CMD".to_string(),
                "curl".to_string(),
                "-f".to_string(),
                format!("http://localhost{}", endpoint),
            ],
            interval: "15s".to_string(),
            timeout: "5s".to_string(),
            retries: 3,
            start_period: "40s".to_string(),
        }
    }
}

/// Docker service configuration
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ServiceConfig {
    /// Service name
    pub name: String,
    /// Docker image
    pub image: String,
    /// Port mappings
    pub ports: Vec<ServicePort>,
    /// Environment variables
    pub environment: HashMap<String, String>,
    /// Volumes
    pub volumes: Vec<String>,
    /// Networks
    pub networks: Vec<String>,
    /// Health check configuration
    pub health_check: Option<HealthCheck>,
    /// Dependencies (service names this service depends on)
    pub depends_on: Vec<String>,
    /// Restart policy
    pub restart: String,
}

impl ServiceConfig {
    /// Create a new service configuration
    pub fn new(name: impl Into<String>, image: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            image: image.into(),
            ports: Vec::new(),
            environment: HashMap::new(),
            volumes: Vec::new(),
            networks: vec!["default".to_string()],
            health_check: None,
            depends_on: Vec::new(),
            restart: "unless-stopped".to_string(),
        }
    }

    /// Add a port mapping
    pub fn with_port(mut self, port: ServicePort) -> Self {
        self.ports.push(port);
        self
    }

    /// Add an environment variable
    pub fn with_env(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.environment.insert(key.into(), value.into());
        self
    }

    /// Add a volume
    pub fn with_volume(mut self, volume: impl Into<String>) -> Self {
        self.volumes.push(volume.into());
        self
    }

    /// Add a network
    pub fn with_network(mut self, network: impl Into<String>) -> Self {
        self.networks.push(network.into());
        self
    }

    /// Set health check
    pub fn with_health_check(mut self, health_check: HealthCheck) -> Self {
        self.health_check = Some(health_check);
        self
    }

    /// Add a dependency
    pub fn with_dependency(mut self, service: impl Into<String>) -> Self {
        self.depends_on.push(service.into());
        self
    }

    /// Set restart policy
    pub fn with_restart(mut self, policy: impl Into<String>) -> Self {
        self.restart = policy.into();
        self
    }

    /// Validate service configuration
    pub fn validate(&self) -> Result<()> {
        if self.name.is_empty() {
            return Err(Error::invalid_input("Service name cannot be empty"));
        }
        if self.image.is_empty() {
            return Err(Error::invalid_input("Service image cannot be empty"));
        }
        // Validate port numbers
        for port in &self.ports {
            if port.host == 0 || port.container == 0 {
                return Err(Error::invalid_input(
                    "Port numbers must be greater than 0",
                ));
            }
        }
        Ok(())
    }
}

/// Extract service configurations from RDF graph
pub fn extract_services(graph: &Graph, namespace: &str) -> Result<Vec<ServiceConfig>> {
    // SPARQL query to extract service definitions
    let query = format!(
        r#"
        PREFIX docker: <{namespace}>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

        SELECT DISTINCT ?service ?name ?image ?port ?env ?envValue ?volume ?network ?healthCmd ?depends
        WHERE {{
            ?service a docker:Service .
            ?service docker:name ?name .
            ?service docker:image ?image .
            OPTIONAL {{ ?service docker:port ?port }}
            OPTIONAL {{
                ?service docker:environment ?envPair .
                ?envPair docker:key ?env .
                ?envPair docker:value ?envValue .
            }}
            OPTIONAL {{ ?service docker:volume ?volume }}
            OPTIONAL {{ ?service docker:network ?network }}
            OPTIONAL {{ ?service docker:healthCheck ?healthCmd }}
            OPTIONAL {{ ?service docker:dependsOn ?depends }}
        }}
        "#,
        namespace = namespace
    );

    let results = graph.query(&query).map_err(|e| {
        Error::with_source("Failed to query services from RDF", Box::new(e))
    })?;

    let mut services_map: HashMap<String, ServiceConfig> = HashMap::new();

    if let QueryResults::Solutions(solutions) = results {
        for solution in solutions {
            let bindings = solution.map_err(|e| {
                Error::with_source("Failed to read query bindings", Box::new(e))
            })?;

            // Extract service URI
            let service_uri = bindings
                .get("service")
                .ok_or_else(|| Error::invalid_input("Service URI missing"))?
                .to_string();

            // Get or create service config
            let service = services_map
                .entry(service_uri.clone())
                .or_insert_with(|| {
                    let name = bindings
                        .get("name")
                        .map(|t| extract_literal(t))
                        .unwrap_or_else(|| "unknown".to_string());
                    let image = bindings
                        .get("image")
                        .map(|t| extract_literal(t))
                        .unwrap_or_else(|| "alpine:latest".to_string());

                    ServiceConfig::new(name, image)
                });

            // Add port if present
            if let Some(port_term) = bindings.get("port") {
                let port_str = extract_literal(port_term);
                if let Ok(port_num) = port_str.parse::<u16>() {
                    service.ports.push(ServicePort::tcp(port_num, port_num));
                }
            }

            // Add environment variable if present
            if let Some(env_term) = bindings.get("env") {
                let key = extract_literal(env_term);
                let value = bindings
                    .get("envValue")
                    .map(|t| extract_literal(t))
                    .unwrap_or_default();
                service.environment.insert(key, value);
            }

            // Add volume if present
            if let Some(volume_term) = bindings.get("volume") {
                let volume = extract_literal(volume_term);
                if !service.volumes.contains(&volume) {
                    service.volumes.push(volume);
                }
            }

            // Add network if present
            if let Some(network_term) = bindings.get("network") {
                let network = extract_literal(network_term);
                if !service.networks.contains(&network) {
                    service.networks.push(network);
                }
            }

            // Set health check if present
            if let Some(health_term) = bindings.get("healthCmd") {
                let health_cmd = extract_literal(health_term);
                service.health_check = Some(parse_health_check(&health_cmd));
            }

            // Add dependency if present
            if let Some(depends_term) = bindings.get("depends") {
                let depends = extract_literal(depends_term);
                if !service.depends_on.contains(&depends) {
                    service.depends_on.push(depends);
                }
            }
        }
    }

    let services: Vec<ServiceConfig> = services_map.into_values().collect();

    // Validate all services
    for service in &services {
        service.validate()?;
    }

    Ok(services)
}

/// Extract literal value from RDF term
fn extract_literal(term: &oxigraph::model::Term) -> String {
    match term {
        oxigraph::model::Term::Literal(lit) => lit.value().to_string(),
        oxigraph::model::Term::NamedNode(node) => node.as_str().to_string(),
        _ => String::new(),
    }
}

/// Parse health check command into HealthCheck struct
fn parse_health_check(cmd: &str) -> HealthCheck {
    if cmd.contains("redis-cli") {
        HealthCheck::redis()
    } else if cmd.contains("pg_isready") {
        HealthCheck::postgres("postgres")
    } else if cmd.contains("curl") || cmd.contains("http") {
        HealthCheck::http("/health")
    } else {
        let mut health = HealthCheck::default();
        health.test = vec!["CMD-SHELL".to_string(), cmd.to_string()];
        health
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_service_port_tcp() {
        // Arrange & Act
        let port = ServicePort::tcp(8080, 80);

        // Assert
        assert_eq!(port.host, 8080);
        assert_eq!(port.container, 80);
        assert_eq!(port.protocol, "tcp");
        assert_eq!(port.to_compose_format(), "8080:80");
    }

    #[test]
    fn test_service_port_udp() {
        // Arrange & Act
        let port = ServicePort::udp(5353, 53);

        // Assert
        assert_eq!(port.host, 5353);
        assert_eq!(port.container, 53);
        assert_eq!(port.protocol, "udp");
    }

    #[test]
    fn test_health_check_redis() {
        // Arrange & Act
        let health = HealthCheck::redis();

        // Assert
        assert_eq!(health.test, vec!["CMD", "redis-cli", "ping"]);
        assert_eq!(health.interval, "10s");
        assert_eq!(health.retries, 5);
    }

    #[test]
    fn test_health_check_postgres() {
        // Arrange & Act
        let health = HealthCheck::postgres("mydb");

        // Assert
        assert!(health.test[0] == "CMD-SHELL");
        assert!(health.test[1].contains("pg_isready"));
        assert!(health.test[1].contains("mydb"));
    }

    #[test]
    fn test_service_config_builder() {
        // Arrange & Act
        let service = ServiceConfig::new("redis", "redis:7-alpine")
            .with_port(ServicePort::tcp(6379, 6379))
            .with_env("REDIS_PASSWORD", "secret")
            .with_volume("redis-data:/data")
            .with_network("backend")
            .with_health_check(HealthCheck::redis())
            .with_restart("always");

        // Assert
        assert_eq!(service.name, "redis");
        assert_eq!(service.image, "redis:7-alpine");
        assert_eq!(service.ports.len(), 1);
        assert_eq!(service.environment.get("REDIS_PASSWORD"), Some(&"secret".to_string()));
        assert_eq!(service.volumes, vec!["redis-data:/data"]);
        assert!(service.networks.contains(&"backend".to_string()));
        assert!(service.health_check.is_some());
        assert_eq!(service.restart, "always");
    }

    #[test]
    fn test_service_config_validation_empty_name() {
        // Arrange
        let service = ServiceConfig::new("", "redis:7");

        // Act
        let result = service.validate();

        // Assert
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("name cannot be empty"));
    }

    #[test]
    fn test_service_config_validation_empty_image() {
        // Arrange
        let service = ServiceConfig::new("redis", "");

        // Act
        let result = service.validate();

        // Assert
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("image cannot be empty"));
    }

    #[test]
    fn test_service_config_validation_invalid_port() {
        // Arrange
        let service = ServiceConfig::new("redis", "redis:7")
            .with_port(ServicePort::tcp(0, 6379));

        // Act
        let result = service.validate();

        // Assert
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("Port numbers must be greater than 0"));
    }

    #[test]
    fn test_parse_health_check_redis() {
        // Arrange & Act
        let health = parse_health_check("redis-cli ping");

        // Assert
        assert_eq!(health.test, vec!["CMD", "redis-cli", "ping"]);
    }

    #[test]
    fn test_parse_health_check_postgres() {
        // Arrange & Act
        let health = parse_health_check("pg_isready -U postgres");

        // Assert
        assert!(health.test[0] == "CMD-SHELL");
        assert!(health.test[1].contains("pg_isready"));
    }

    #[test]
    fn test_parse_health_check_http() {
        // Arrange & Act
        let health = parse_health_check("curl -f http://localhost/health");

        // Assert
        assert!(health.test.len() > 0);
        assert_eq!(health.interval, "15s");
    }

    #[test]
    fn test_extract_literal_from_literal_term() {
        // Arrange
        use oxigraph::model::Literal;
        let term = oxigraph::model::Term::Literal(Literal::new_simple_literal("test-value"));

        // Act
        let result = extract_literal(&term);

        // Assert
        assert_eq!(result, "test-value");
    }
}
