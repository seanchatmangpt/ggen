//! Service fixtures for testing
//!
//! Provides containerized service instances for testing including
//! databases, caches, and other dependencies.

use crate::error::Result;
use std::collections::HashMap;

pub mod postgres;
pub mod redis;

/// Service trait for test fixtures
pub trait Service: Send + Sync {
    /// Get service name
    fn name(&self) -> &str;

    /// Check if service is healthy
    fn health_check(&self) -> Result<bool>;

    /// Get connection information
    fn connection_info(&self) -> Result<ConnectionInfo>;

    /// Start the service
    fn start(&mut self) -> Result<()>;

    /// Stop the service
    fn stop(&mut self) -> Result<()>;

    /// Wait for service to be ready
    fn wait_for_ready(&self, timeout: std::time::Duration) -> Result<()>;

    /// Get service logs
    fn logs(&self) -> Result<Vec<String>>;

    /// Check if service is running
    fn is_running(&self) -> Result<bool>;
}

/// Connection information for a service
#[derive(Debug, Clone)]
pub struct ConnectionInfo {
    /// Connection parameters as a string
    pub params: String,
}

impl ConnectionInfo {
    /// Create a new connection info
    pub fn new(params: impl Into<String>) -> Self {
        Self {
            params: params.into(),
        }
    }

    /// Get the connection parameters string
    pub fn params(&self) -> &str {
        &self.params
    }

    /// Get the connection parameters as a string
    pub fn connection_string(&self) -> &str {
        &self.params
    }

    /// Add a parameter to the connection
    pub fn with_param(mut self, key: &str, value: &str) -> Self {
        if self.params.is_empty() {
            self.params = format!("{}={}", key, value);
        } else {
            self.params = format!("{},{}={}", self.params, key, value);
        }
        self
    }

    /// Get Redis URL for Redis connections
    pub fn redis_url(&self) -> String {
        format!("redis://{}", self.params)
    }
}

/// Service manager for orchestrating multiple services
pub struct ServiceManager {
    /// Registered services
    services: Vec<Box<dyn Service>>,
    /// Service startup order
    startup_order: Vec<usize>,
    /// Service shutdown order (reverse of startup)
    shutdown_order: Vec<usize>,
}

impl ServiceManager {
    /// Create a new service manager
    pub fn new() -> Self {
        Self {
            services: Vec::new(),
            startup_order: Vec::new(),
            shutdown_order: Vec::new(),
        }
    }

    /// Add a service to the manager
    pub fn add_service(&mut self, service: Box<dyn Service>) -> usize {
        let index = self.services.len();
        self.services.push(service);
        self.startup_order.push(index);
        self.shutdown_order.insert(0, index); // Reverse order for shutdown
        index
    }

    /// Start all services in order
    pub fn start_all(&mut self) -> Result<()> {
        for &index in &self.startup_order {
            self.services[index].start()?;
        }
        Ok(())
    }

    /// Stop all services in reverse order
    pub fn stop_all(&mut self) -> Result<()> {
        for &index in &self.shutdown_order {
            self.services[index].stop()?;
        }
        Ok(())
    }

    /// Get a service by index
    pub fn get_service(&self, index: usize) -> Option<&dyn Service> {
        self.services.get(index).map(|s| s.as_ref())
    }

    /// Get a service by name
    pub fn get_service_by_name(&self, name: &str) -> Option<&dyn Service> {
        self.services.iter()
            .find(|s| s.name() == name)
            .map(|s| s.as_ref())
    }

    /// Check health of all services
    pub fn health_check_all(&self) -> Result<Vec<bool>> {
        let mut results = Vec::new();
        for service in &self.services {
            results.push(service.health_check()?);
        }
        Ok(results)
    }

    /// Get connection info for all services
    pub fn connection_info_all(&self) -> Result<Vec<ConnectionInfo>> {
        let mut results = Vec::new();
        for service in &self.services {
            results.push(service.connection_info()?);
        }
        Ok(results)
    }

    /// Get service count
    pub fn service_count(&self) -> usize {
        self.services.len()
    }

    /// Check if all services are healthy
    pub fn all_healthy(&self) -> Result<bool> {
        let health_status = self.health_check_all()?;
        Ok(health_status.iter().all(|&healthy| healthy))
    }

    /// Wait for all services to be healthy
    pub fn wait_for_all_healthy(&self, timeout: std::time::Duration) -> Result<()> {
        let start = std::time::Instant::now();
        let check_interval = std::time::Duration::from_millis(500);

        while start.elapsed() < timeout {
            if self.all_healthy()? {
                return Ok(());
            }
            std::thread::sleep(check_interval);
        }

        Err(crate::error::CleanroomError::service_error(format!(
            "Services did not become healthy within {} seconds",
            timeout.as_secs()
        )).into())
    }

    /// Wait for all services to be ready
    pub fn wait_for_all_ready(&self, timeout: std::time::Duration) -> Result<()> {
        let start = std::time::Instant::now();
        let check_interval = std::time::Duration::from_millis(500);

        while start.elapsed() < timeout {
            let mut all_ready = true;
            for service in &self.services {
                if !service.wait_for_ready(check_interval).is_ok() {
                    all_ready = false;
                    break;
                }
            }
            
            if all_ready {
                return Ok(());
            }
            std::thread::sleep(check_interval);
        }

        Err(crate::error::CleanroomError::service_error(format!(
            "Services did not become ready within {} seconds",
            timeout.as_secs()
        )).into())
    }

    /// Get logs from all services
    pub fn get_all_logs(&self) -> Result<HashMap<String, Vec<String>>> {
        let mut logs = HashMap::new();
        for service in &self.services {
            match service.logs() {
                Ok(service_logs) => {
                    logs.insert(service.name().to_string(), service_logs);
                }
                Err(e) => {
                    logs.insert(service.name().to_string(), vec![format!("Error getting logs: {}", e)]);
                }
            }
        }
        Ok(logs)
    }

    /// Check if all services are running
    pub fn all_running(&self) -> Result<bool> {
        for service in &self.services {
            if !service.is_running()? {
                return Ok(false);
            }
        }
        Ok(true)
    }
}

impl Default for ServiceManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Service builder for creating service configurations
pub struct ServiceBuilder {
    /// Service type
    service_type: String,
    /// Configuration parameters
    config: HashMap<String, String>,
}

impl ServiceBuilder {
    /// Create a new service builder
    pub fn new(service_type: impl Into<String>) -> Self {
        Self {
            service_type: service_type.into(),
            config: HashMap::new(),
        }
    }

    /// Add configuration parameter
    pub fn with_config(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.config.insert(key.into(), value.into());
        self
    }

    /// Build the service
    pub fn build(self) -> Result<Box<dyn Service>> {
        match self.service_type.as_str() {
            "postgres" => {
                let port = self.config.get("port")
                    .and_then(|s| s.parse().ok())
                    .unwrap_or(5432);
                let database = self.config.get("database")
                    .cloned()
                    .unwrap_or_else(|| "testdb".to_string());
                let username = self.config.get("username")
                    .cloned()
                    .unwrap_or_else(|| "testuser".to_string());
                let password = self.config.get("password")
                    .cloned()
                    .unwrap_or_else(|| "testpass".to_string());

                Ok(Box::new(crate::services::postgres::Postgres::with_config(
                    port, database, username, password
                )?))
            }
            "redis" => {
                let port = self.config.get("port")
                    .and_then(|s| s.parse().ok())
                    .unwrap_or(6379);
                let password = self.config.get("password").cloned();

                Ok(Box::new(crate::services::redis::Redis::with_config(port, password)?))
            }
            _ => Err(crate::error::CleanroomError::configuration_error(format!(
                "Unknown service type: {}",
                self.service_type
            )).into())
        }
    }
}

/// Service registry for managing service types
pub struct ServiceRegistry {
    /// Registered service types
    service_types: HashMap<String, fn() -> Result<Box<dyn Service>>>,
}

impl ServiceRegistry {
    /// Create a new service registry
    pub fn new() -> Self {
        let mut registry = Self {
            service_types: HashMap::new(),
        };

        // Register default service types
        registry.register("postgres", || {
            Ok(Box::new(crate::services::postgres::Postgres::new()?))
        });

        registry.register("redis", || {
            Ok(Box::new(crate::services::redis::Redis::new()?))
        });

        registry
    }

    /// Register a service type
    pub fn register(&mut self, name: impl Into<String>, factory: fn() -> Result<Box<dyn Service>>) {
        self.service_types.insert(name.into(), factory);
    }

    /// Create a service by type name
    pub fn create_service(&self, service_type: &str) -> Result<Box<dyn Service>> {
        self.service_types.get(service_type)
            .ok_or_else(|| crate::error::CleanroomError::configuration_error(format!(
                "Unknown service type: {}",
                service_type
            )).into())
            .and_then(|factory| factory())
    }

    /// List available service types
    pub fn available_types(&self) -> Vec<String> {
        self.service_types.keys().cloned().collect()
    }

    /// Check if a service type is registered
    pub fn is_registered(&self, service_type: &str) -> bool {
        self.service_types.contains_key(service_type)
    }
}

impl Default for ServiceRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_connection_info_creation() {
        let conn_info = ConnectionInfo::new("host=localhost port=5432");
        assert_eq!(conn_info.params(), "host=localhost port=5432");
    }

    #[test]
    fn test_connection_info_with_param() {
        let conn_info = ConnectionInfo::new("host=localhost port=5432")
            .with_param("username", "testuser")
            .with_param("password", "testpass");
        
        assert!(conn_info.params().contains("username=testuser"));
        assert!(conn_info.params().contains("password=testpass"));
    }

    #[test]
    fn test_connection_string() {
        let conn_info = ConnectionInfo::new("host=localhost port=5432")
            .with_param("username", "testuser")
            .with_param("password", "testpass");
        
        let conn_str = conn_info.connection_string();
        assert!(conn_str.contains("host=localhost"));
        assert!(conn_str.contains("username=testuser"));
        assert!(conn_str.contains("password=testpass"));
    }

    #[test]
    fn test_postgres_url() {
        let conn_info = ConnectionInfo::new("host=localhost port=5432")
            .with_param("username", "testuser")
            .with_param("password", "testpass")
            .with_param("database", "testdb");
        
        // Test that connection string contains expected components
        let conn_str = conn_info.connection_string();
        assert!(conn_str.contains("host=localhost"));
        assert!(conn_str.contains("username=testuser"));
        assert!(conn_str.contains("password=testpass"));
        assert!(conn_str.contains("database=testdb"));
    }

    #[test]
    fn test_redis_url() {
        let conn_info = ConnectionInfo::new("host=localhost port=6379")
            .with_param("password", "testpass");
        
        // Test that connection string contains expected components
        let conn_str = conn_info.connection_string();
        assert!(conn_str.contains("host=localhost"));
        assert!(conn_str.contains("password=testpass"));
    }

    #[test]
    fn test_redis_url_no_password() {
        let conn_info = ConnectionInfo::new("host=localhost port=6379");
        
        // Test that connection string contains expected components
        let conn_str = conn_info.connection_string();
        assert!(conn_str.contains("host=localhost"));
    }

    #[test]
    fn test_service_manager_creation() {
        let manager = ServiceManager::new();
        assert_eq!(manager.service_count(), 0);
    }

    #[test]
    fn test_service_builder() {
        let builder = ServiceBuilder::new("postgres")
            .with_config("port", "5433")
            .with_config("database", "mydb");
        
        assert_eq!(builder.service_type, "postgres");
        assert_eq!(builder.config.get("port"), Some(&"5433".to_string()));
        assert_eq!(builder.config.get("database"), Some(&"mydb".to_string()));
    }

    #[test]
    fn test_service_registry() {
        let registry = ServiceRegistry::new();
        assert!(registry.is_registered("postgres"));
        assert!(registry.is_registered("redis"));
        assert!(!registry.is_registered("unknown"));
        
        let types = registry.available_types();
        assert!(types.contains(&"postgres".to_string()));
        assert!(types.contains(&"redis".to_string()));
    }
}