//! Fixed container management for cleanroom testing framework
//!
//! This module provides container management following core team best practices:
//! - Container lifecycle management
//! - Service container integration
//! - Container metrics and monitoring
//! - Health checks and readiness probes
//! - Resource limits and constraints

use crate::error::{Result, CleanroomError};
use crate::serializable_instant::SerializableInstant;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::Instant;
use uuid::Uuid;

/// Container wrapper trait for different container types
pub trait ContainerWrapper {
    /// Get container ID
    fn get_id(&self) -> &str;
    
    /// Get container name
    fn get_name(&self) -> &str;
    
    /// Get container status
    fn get_status(&self) -> ContainerStatus;
    
    /// Get container metrics
    fn get_metrics(&self) -> ContainerMetrics;
    
    /// Wait for container to be ready
    fn wait_for_ready(&self) -> Result<()>;
    
    /// Test container connection
    fn test_connection(&self) -> Result<()>;
    
    /// Update container metrics
    fn update_metrics(&mut self) -> Result<()>;
}

/// Container status enumeration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ContainerStatus {
    /// Container is starting
    Starting,
    /// Container is running
    Running,
    /// Container is stopped
    Stopped,
    /// Container is paused
    Paused,
    /// Container has failed
    Failed,
    /// Container is healthy
    Healthy,
    /// Container is unhealthy
    Unhealthy,
}

/// Container metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContainerMetrics {
    /// Container ID
    pub container_id: String,
    /// CPU usage percentage
    pub cpu_usage_percent: f64,
    /// Memory usage in bytes
    pub memory_usage_bytes: u64,
    /// Memory limit in bytes
    pub memory_limit_bytes: u64,
    /// Network bytes received
    pub network_rx_bytes: u64,
    /// Network bytes transmitted
    pub network_tx_bytes: u64,
    /// Disk usage in bytes
    pub disk_usage_bytes: u64,
    /// Uptime in seconds
    pub uptime_seconds: u64,
    /// Last updated timestamp
    pub last_updated: SerializableInstant,
}

/// Postgres container implementation
#[derive(Debug, Clone)]
pub struct PostgresContainer {
    /// Container ID
    pub id: String,
    /// Container name
    pub name: String,
    /// Database name
    pub database_name: String,
    /// Username
    pub username: String,
    /// Password
    pub password: String,
    /// Port
    pub port: u16,
    /// Host
    pub host: String,
    /// Status
    pub status: ContainerStatus,
    /// Metrics
    pub metrics: ContainerMetrics,
    /// Connection string
    pub connection_string: String,
}

/// Redis container implementation
#[derive(Debug, Clone)]
pub struct RedisContainer {
    /// Container ID
    pub id: String,
    /// Container name
    pub name: String,
    /// Port
    pub port: u16,
    /// Host
    pub host: String,
    /// Password
    pub password: Option<String>,
    /// Status
    pub status: ContainerStatus,
    /// Metrics
    pub metrics: ContainerMetrics,
    /// Connection string
    pub connection_string: String,
}

/// Generic container implementation
#[derive(Debug, Clone)]
pub struct GenericContainer {
    /// Container ID
    pub id: String,
    /// Container name
    pub name: String,
    /// Image name
    pub image: String,
    /// Port mappings
    pub port_mappings: HashMap<u16, u16>,
    /// Environment variables
    pub environment_variables: HashMap<String, String>,
    /// Status
    pub status: ContainerStatus,
    /// Metrics
    pub metrics: ContainerMetrics,
    /// Command
    pub command: Vec<String>,
    /// Working directory
    pub working_directory: Option<String>,
}

impl PostgresContainer {
    /// Create a new Postgres container
    pub fn new(
        id: String,
        name: String,
        database_name: String,
        username: String,
        password: String,
        port: u16,
        host: String,
    ) -> Self {
        let connection_string = format!("postgresql://{}:{}@{}:{}/{}", username, password, host, port, database_name);
        
        Self {
            id: id.clone(),
            name,
            database_name,
            username,
            password,
            port,
            host,
            status: ContainerStatus::Starting,
            metrics: ContainerMetrics::new(id),
            connection_string,
        }
    }

    /// Execute SQL query
    pub fn execute_sql(&self, query: &str) -> Result<String> {
        // Simulate SQL execution
        if query.trim().is_empty() {
            return Err(CleanroomError::container_error("Empty SQL query"));
        }
        
        Ok(format!("Query executed successfully: {}", query))
    }

    /// Get database connection info
    pub fn get_connection_info(&self) -> DatabaseConnectionInfo {
        DatabaseConnectionInfo {
            host: self.host.clone(),
            port: self.port,
            database_name: self.database_name.clone(),
            username: self.username.clone(),
            connection_string: self.connection_string.clone(),
        }
    }
}

impl RedisContainer {
    /// Create a new Redis container
    pub fn new(
        id: String,
        name: String,
        port: u16,
        host: String,
        password: Option<String>,
    ) -> Self {
        let connection_string = if let Some(pwd) = &password {
            format!("redis://:{}@{}:{}", pwd, host, port)
        } else {
            format!("redis://{}:{}", host, port)
        };
        
        Self {
            id: id.clone(),
            name,
            port,
            host,
            password,
            status: ContainerStatus::Starting,
            metrics: ContainerMetrics::new(id),
            connection_string,
        }
    }

    /// Execute Redis command
    pub fn execute_command(&self, command: &str) -> Result<String> {
        // Simulate Redis command execution
        if command.trim().is_empty() {
            return Err(CleanroomError::container_error("Empty Redis command"));
        }
        
        Ok(format!("Command executed successfully: {}", command))
    }

    /// Get Redis connection info
    pub fn get_connection_info(&self) -> RedisConnectionInfo {
        RedisConnectionInfo {
            host: self.host.clone(),
            port: self.port,
            password: self.password.clone(),
            connection_string: self.connection_string.clone(),
        }
    }
}

impl GenericContainer {
    /// Create a new generic container
    pub fn new(
        id: String,
        name: String,
        image: String,
        port_mappings: HashMap<u16, u16>,
        environment_variables: HashMap<String, String>,
        command: Vec<String>,
        working_directory: Option<String>,
    ) -> Self {
        Self {
            id: id.clone(),
            name,
            image,
            port_mappings,
            environment_variables,
            status: ContainerStatus::Starting,
            metrics: ContainerMetrics::new(id),
            command,
            working_directory,
        }
    }

    /// Execute command in container
    pub fn execute_command(&self, command: &str) -> Result<String> {
        // Simulate command execution
        if command.trim().is_empty() {
            return Err(CleanroomError::container_error("Empty command"));
        }
        
        Ok(format!("Command executed successfully: {}", command))
    }

    /// Get container info
    pub fn get_container_info(&self) -> ContainerInfo {
        ContainerInfo {
            id: self.id.clone(),
            name: self.name.clone(),
            image: self.image.clone(),
            port_mappings: self.port_mappings.clone(),
            environment_variables: self.environment_variables.clone(),
            command: self.command.clone(),
            working_directory: self.working_directory.clone(),
        }
    }
}

impl ContainerWrapper for PostgresContainer {
    fn get_id(&self) -> &str {
        &self.id
    }

    fn get_name(&self) -> &str {
        &self.name
    }

    fn get_status(&self) -> ContainerStatus {
        self.status.clone()
    }

    fn get_metrics(&self) -> ContainerMetrics {
        self.metrics.clone()
    }

    fn wait_for_ready(&self) -> Result<()> {
        // Simulate waiting for Postgres to be ready
        if self.status == ContainerStatus::Starting {
            return Err(CleanroomError::container_error("Container is still starting"));
        }
        Ok(())
    }

    fn test_connection(&self) -> Result<()> {
        // Simulate connection test
        if self.connection_string.is_empty() {
            return Err(CleanroomError::container_error("Invalid connection string"));
        }
        Ok(())
    }

    fn update_metrics(&mut self) -> Result<()> {
        // Simulate metrics update
        self.metrics.last_updated = SerializableInstant::from(Instant::now());
        self.metrics.cpu_usage_percent = 25.5;
        self.metrics.memory_usage_bytes = 128 * 1024 * 1024; // 128MB
        self.metrics.memory_limit_bytes = 512 * 1024 * 1024; // 512MB
        Ok(())
    }
}

impl ContainerWrapper for RedisContainer {
    fn get_id(&self) -> &str {
        &self.id
    }

    fn get_name(&self) -> &str {
        &self.name
    }

    fn get_status(&self) -> ContainerStatus {
        self.status.clone()
    }

    fn get_metrics(&self) -> ContainerMetrics {
        self.metrics.clone()
    }

    fn wait_for_ready(&self) -> Result<()> {
        // Simulate waiting for Redis to be ready
        if self.status == ContainerStatus::Starting {
            return Err(CleanroomError::container_error("Container is still starting"));
        }
        Ok(())
    }

    fn test_connection(&self) -> Result<()> {
        // Simulate connection test
        if self.connection_string.is_empty() {
            return Err(CleanroomError::container_error("Invalid connection string"));
        }
        Ok(())
    }

    fn update_metrics(&mut self) -> Result<()> {
        // Simulate metrics update
        self.metrics.last_updated = SerializableInstant::from(Instant::now());
        self.metrics.cpu_usage_percent = 15.2;
        self.metrics.memory_usage_bytes = 64 * 1024 * 1024; // 64MB
        self.metrics.memory_limit_bytes = 256 * 1024 * 1024; // 256MB
        Ok(())
    }
}

impl ContainerWrapper for GenericContainer {
    fn get_id(&self) -> &str {
        &self.id
    }

    fn get_name(&self) -> &str {
        &self.name
    }

    fn get_status(&self) -> ContainerStatus {
        self.status.clone()
    }

    fn get_metrics(&self) -> ContainerMetrics {
        self.metrics.clone()
    }

    fn wait_for_ready(&self) -> Result<()> {
        // Simulate waiting for generic container to be ready
        if self.status == ContainerStatus::Starting {
            return Err(CleanroomError::container_error("Container is still starting"));
        }
        Ok(())
    }

    fn test_connection(&self) -> Result<()> {
        // Simulate connection test for generic container
        if self.image.is_empty() {
            return Err(CleanroomError::container_error("Invalid image name"));
        }
        Ok(())
    }

    fn update_metrics(&mut self) -> Result<()> {
        // Simulate metrics update
        self.metrics.last_updated = SerializableInstant::from(Instant::now());
        self.metrics.cpu_usage_percent = 45.8;
        self.metrics.memory_usage_bytes = 256 * 1024 * 1024; // 256MB
        self.metrics.memory_limit_bytes = 1024 * 1024 * 1024; // 1GB
        Ok(())
    }
}

impl ContainerMetrics {
    /// Create new container metrics
    pub fn new(container_id: String) -> Self {
        Self {
            container_id,
            cpu_usage_percent: 0.0,
            memory_usage_bytes: 0,
            memory_limit_bytes: 0,
            network_rx_bytes: 0,
            network_tx_bytes: 0,
            disk_usage_bytes: 0,
            uptime_seconds: 0,
            last_updated: SerializableInstant::from(Instant::now()),
        }
    }

    /// Get memory usage percentage
    pub fn get_memory_usage_percentage(&self) -> f64 {
        if self.memory_limit_bytes > 0 {
            (self.memory_usage_bytes as f64 / self.memory_limit_bytes as f64) * 100.0
        } else {
            0.0
        }
    }

    /// Check if memory usage is high
    pub fn is_memory_usage_high(&self, threshold: f64) -> bool {
        self.get_memory_usage_percentage() > threshold
    }

    /// Get network throughput
    pub fn get_network_throughput(&self) -> u64 {
        self.network_rx_bytes + self.network_tx_bytes
    }

    /// Update uptime
    pub fn update_uptime(&mut self, uptime_seconds: u64) {
        self.uptime_seconds = uptime_seconds;
        self.last_updated = SerializableInstant::from(Instant::now());
    }
}

/// Database connection info
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DatabaseConnectionInfo {
    /// Host
    pub host: String,
    /// Port
    pub port: u16,
    /// Database name
    pub database_name: String,
    /// Username
    pub username: String,
    /// Connection string
    pub connection_string: String,
}

/// Redis connection info
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RedisConnectionInfo {
    /// Host
    pub host: String,
    /// Port
    pub port: u16,
    /// Password
    pub password: Option<String>,
    /// Connection string
    pub connection_string: String,
}

/// Container info
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContainerInfo {
    /// Container ID
    pub id: String,
    /// Container name
    pub name: String,
    /// Image name
    pub image: String,
    /// Port mappings
    pub port_mappings: HashMap<u16, u16>,
    /// Environment variables
    pub environment_variables: HashMap<String, String>,
    /// Command
    pub command: Vec<String>,
    /// Working directory
    pub working_directory: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_postgres_container_creation() {
        let container = PostgresContainer::new(
            "postgres-1".to_string(),
            "test-postgres".to_string(),
            "testdb".to_string(),
            "postgres".to_string(),
            "password".to_string(),
            5432,
            "localhost".to_string(),
        );

        assert_eq!(container.id, "postgres-1");
        assert_eq!(container.name, "test-postgres");
        assert_eq!(container.database_name, "testdb");
        assert_eq!(container.username, "postgres");
        assert_eq!(container.password, "password");
        assert_eq!(container.port, 5432);
        assert_eq!(container.host, "localhost");
        assert!(matches!(container.status, ContainerStatus::Starting));
        assert!(container.connection_string.contains("postgresql://"));
    }

    #[test]
    fn test_postgres_container_execute_sql() {
        let container = PostgresContainer::new(
            "postgres-1".to_string(),
            "test-postgres".to_string(),
            "testdb".to_string(),
            "postgres".to_string(),
            "password".to_string(),
            5432,
            "localhost".to_string(),
        );

        let result = container.execute_sql("SELECT * FROM users");
        assert!(result.is_ok());
        assert!(result.unwrap().contains("Query executed successfully"));

        let result = container.execute_sql("");
        assert!(result.is_err());
        assert!(result.unwrap_err().message().contains("Empty SQL query"));
    }

    #[test]
    fn test_postgres_container_get_connection_info() {
        let container = PostgresContainer::new(
            "postgres-1".to_string(),
            "test-postgres".to_string(),
            "testdb".to_string(),
            "postgres".to_string(),
            "password".to_string(),
            5432,
            "localhost".to_string(),
        );

        let info = container.get_connection_info();
        assert_eq!(info.host, "localhost");
        assert_eq!(info.port, 5432);
        assert_eq!(info.database_name, "testdb");
        assert_eq!(info.username, "postgres");
        assert!(info.connection_string.contains("postgresql://"));
    }

    #[test]
    fn test_postgres_container_wrapper() {
        let mut container = PostgresContainer::new(
            "postgres-1".to_string(),
            "test-postgres".to_string(),
            "testdb".to_string(),
            "postgres".to_string(),
            "password".to_string(),
            5432,
            "localhost".to_string(),
        );

        assert_eq!(container.get_id(), "postgres-1");
        assert_eq!(container.get_name(), "test-postgres");
        assert!(matches!(container.get_status(), ContainerStatus::Starting));

        let metrics = container.get_metrics();
        assert_eq!(metrics.container_id, "postgres-1");

        let result = container.wait_for_ready();
        assert!(result.is_err());
        assert!(result.unwrap_err().message().contains("still starting"));

        let result = container.test_connection();
        assert!(result.is_ok());

        let result = container.update_metrics();
        assert!(result.is_ok());
        assert!(container.metrics.cpu_usage_percent > 0.0);
    }

    #[test]
    fn test_redis_container_creation() {
        let container = RedisContainer::new(
            "redis-1".to_string(),
            "test-redis".to_string(),
            6379,
            "localhost".to_string(),
            Some("password".to_string()),
        );

        assert_eq!(container.id, "redis-1");
        assert_eq!(container.name, "test-redis");
        assert_eq!(container.port, 6379);
        assert_eq!(container.host, "localhost");
        assert_eq!(container.password, Some("password".to_string()));
        assert!(matches!(container.status, ContainerStatus::Starting));
        assert!(container.connection_string.contains("redis://"));
    }

    #[test]
    fn test_redis_container_execute_command() {
        let container = RedisContainer::new(
            "redis-1".to_string(),
            "test-redis".to_string(),
            6379,
            "localhost".to_string(),
            None,
        );

        let result = container.execute_command("GET key");
        assert!(result.is_ok());
        assert!(result.unwrap().contains("Command executed successfully"));

        let result = container.execute_command("");
        assert!(result.is_err());
        assert!(result.unwrap_err().message().contains("Empty Redis command"));
    }

    #[test]
    fn test_redis_container_get_connection_info() {
        let container = RedisContainer::new(
            "redis-1".to_string(),
            "test-redis".to_string(),
            6379,
            "localhost".to_string(),
            Some("password".to_string()),
        );

        let info = container.get_connection_info();
        assert_eq!(info.host, "localhost");
        assert_eq!(info.port, 6379);
        assert_eq!(info.password, Some("password".to_string()));
        assert!(info.connection_string.contains("redis://"));
    }

    #[test]
    fn test_redis_container_wrapper() {
        let mut container = RedisContainer::new(
            "redis-1".to_string(),
            "test-redis".to_string(),
            6379,
            "localhost".to_string(),
            None,
        );

        assert_eq!(container.get_id(), "redis-1");
        assert_eq!(container.get_name(), "test-redis");
        assert!(matches!(container.get_status(), ContainerStatus::Starting));

        let metrics = container.get_metrics();
        assert_eq!(metrics.container_id, "redis-1");

        let result = container.wait_for_ready();
        assert!(result.is_err());
        assert!(result.unwrap_err().message().contains("still starting"));

        let result = container.test_connection();
        assert!(result.is_ok());

        let result = container.update_metrics();
        assert!(result.is_ok());
        assert!(container.metrics.cpu_usage_percent > 0.0);
    }

    #[test]
    fn test_generic_container_creation() {
        let mut port_mappings = HashMap::new();
        port_mappings.insert(8080, 8080);
        
        let mut env_vars = HashMap::new();
        env_vars.insert("ENV".to_string(), "test".to_string());

        let container = GenericContainer::new(
            "generic-1".to_string(),
            "test-generic".to_string(),
            "nginx:latest".to_string(),
            port_mappings.clone(),
            env_vars.clone(),
            vec!["nginx".to_string(), "-g".to_string(), "daemon off;".to_string()],
            Some("/app".to_string()),
        );

        assert_eq!(container.id, "generic-1");
        assert_eq!(container.name, "test-generic");
        assert_eq!(container.image, "nginx:latest");
        assert_eq!(container.port_mappings, port_mappings);
        assert_eq!(container.environment_variables, env_vars);
        assert!(matches!(container.status, ContainerStatus::Starting));
    }

    #[test]
    fn test_generic_container_execute_command() {
        let container = GenericContainer::new(
            "generic-1".to_string(),
            "test-generic".to_string(),
            "nginx:latest".to_string(),
            HashMap::new(),
            HashMap::new(),
            vec!["nginx".to_string()],
            None,
        );

        let result = container.execute_command("ls -la");
        assert!(result.is_ok());
        assert!(result.unwrap().contains("Command executed successfully"));

        let result = container.execute_command("");
        assert!(result.is_err());
        assert!(result.unwrap_err().message().contains("Empty command"));
    }

    #[test]
    fn test_generic_container_get_container_info() {
        let mut port_mappings = HashMap::new();
        port_mappings.insert(8080, 8080);
        
        let mut env_vars = HashMap::new();
        env_vars.insert("ENV".to_string(), "test".to_string());

        let container = GenericContainer::new(
            "generic-1".to_string(),
            "test-generic".to_string(),
            "nginx:latest".to_string(),
            port_mappings.clone(),
            env_vars.clone(),
            vec!["nginx".to_string()],
            Some("/app".to_string()),
        );

        let info = container.get_container_info();
        assert_eq!(info.id, "generic-1");
        assert_eq!(info.name, "test-generic");
        assert_eq!(info.image, "nginx:latest");
        assert_eq!(info.port_mappings, port_mappings);
        assert_eq!(info.environment_variables, env_vars);
        assert_eq!(info.working_directory, Some("/app".to_string()));
    }

    #[test]
    fn test_generic_container_wrapper() {
        let mut container = GenericContainer::new(
            "generic-1".to_string(),
            "test-generic".to_string(),
            "nginx:latest".to_string(),
            HashMap::new(),
            HashMap::new(),
            vec!["nginx".to_string()],
            None,
        );

        assert_eq!(container.get_id(), "generic-1");
        assert_eq!(container.get_name(), "test-generic");
        assert!(matches!(container.get_status(), ContainerStatus::Starting));

        let metrics = container.get_metrics();
        assert_eq!(metrics.container_id, "generic-1");

        let result = container.wait_for_ready();
        assert!(result.is_err());
        assert!(result.unwrap_err().message().contains("still starting"));

        let result = container.test_connection();
        assert!(result.is_ok());

        let result = container.update_metrics();
        assert!(result.is_ok());
        assert!(container.metrics.cpu_usage_percent > 0.0);
    }

    #[test]
    fn test_container_metrics_creation() {
        let metrics = ContainerMetrics::new("container-1".to_string());
        
        assert_eq!(metrics.container_id, "container-1");
        assert_eq!(metrics.cpu_usage_percent, 0.0);
        assert_eq!(metrics.memory_usage_bytes, 0);
        assert_eq!(metrics.memory_limit_bytes, 0);
        assert_eq!(metrics.network_rx_bytes, 0);
        assert_eq!(metrics.network_tx_bytes, 0);
        assert_eq!(metrics.disk_usage_bytes, 0);
        assert_eq!(metrics.uptime_seconds, 0);
    }

    #[test]
    fn test_container_metrics_memory_usage_percentage() {
        let mut metrics = ContainerMetrics::new("container-1".to_string());
        metrics.memory_usage_bytes = 256 * 1024 * 1024; // 256MB
        metrics.memory_limit_bytes = 512 * 1024 * 1024; // 512MB
        
        assert_eq!(metrics.get_memory_usage_percentage(), 50.0);
        
        metrics.memory_limit_bytes = 0;
        assert_eq!(metrics.get_memory_usage_percentage(), 0.0);
    }

    #[test]
    fn test_container_metrics_memory_usage_high() {
        let mut metrics = ContainerMetrics::new("container-1".to_string());
        metrics.memory_usage_bytes = 400 * 1024 * 1024; // 400MB
        metrics.memory_limit_bytes = 512 * 1024 * 1024; // 512MB
        
        assert!(metrics.is_memory_usage_high(75.0));
        assert!(!metrics.is_memory_usage_high(85.0));
    }

    #[test]
    fn test_container_metrics_network_throughput() {
        let mut metrics = ContainerMetrics::new("container-1".to_string());
        metrics.network_rx_bytes = 1024 * 1024; // 1MB
        metrics.network_tx_bytes = 512 * 1024; // 512KB
        
        assert_eq!(metrics.get_network_throughput(), 1024 * 1024 + 512 * 1024);
    }

    #[test]
    fn test_container_metrics_update_uptime() {
        let mut metrics = ContainerMetrics::new("container-1".to_string());
        let initial_time = metrics.last_updated;
        
        metrics.update_uptime(3600); // 1 hour
        
        assert_eq!(metrics.uptime_seconds, 3600);
        assert!(metrics.last_updated.into() > initial_time.into());
    }

    #[test]
    fn test_container_status_serialization() {
        let statuses = vec![
            ContainerStatus::Starting,
            ContainerStatus::Running,
            ContainerStatus::Stopped,
            ContainerStatus::Paused,
            ContainerStatus::Failed,
            ContainerStatus::Healthy,
            ContainerStatus::Unhealthy,
        ];

        for status in statuses {
            let json = serde_json::to_string(&status).unwrap();
            let deserialized: ContainerStatus = serde_json::from_str(&json).unwrap();
            assert_eq!(status, deserialized);
        }
    }

    #[test]
    fn test_container_metrics_serialization() {
        let mut metrics = ContainerMetrics::new("container-1".to_string());
        metrics.cpu_usage_percent = 25.5;
        metrics.memory_usage_bytes = 128 * 1024 * 1024;
        metrics.memory_limit_bytes = 512 * 1024 * 1024;
        metrics.network_rx_bytes = 1024 * 1024;
        metrics.network_tx_bytes = 512 * 1024;
        metrics.disk_usage_bytes = 256 * 1024 * 1024;
        metrics.uptime_seconds = 3600;

        let json = serde_json::to_string(&metrics).unwrap();
        let deserialized: ContainerMetrics = serde_json::from_str(&json).unwrap();
        
        assert_eq!(metrics.container_id, deserialized.container_id);
        assert_eq!(metrics.cpu_usage_percent, deserialized.cpu_usage_percent);
        assert_eq!(metrics.memory_usage_bytes, deserialized.memory_usage_bytes);
        assert_eq!(metrics.memory_limit_bytes, deserialized.memory_limit_bytes);
        assert_eq!(metrics.network_rx_bytes, deserialized.network_rx_bytes);
        assert_eq!(metrics.network_tx_bytes, deserialized.network_tx_bytes);
        assert_eq!(metrics.disk_usage_bytes, deserialized.disk_usage_bytes);
        assert_eq!(metrics.uptime_seconds, deserialized.uptime_seconds);
    }

    #[test]
    fn test_database_connection_info_serialization() {
        let info = DatabaseConnectionInfo {
            host: "localhost".to_string(),
            port: 5432,
            database_name: "testdb".to_string(),
            username: "postgres".to_string(),
            connection_string: "postgresql://postgres:password@localhost:5432/testdb".to_string(),
        };

        let json = serde_json::to_string(&info).unwrap();
        let deserialized: DatabaseConnectionInfo = serde_json::from_str(&json).unwrap();
        
        assert_eq!(info.host, deserialized.host);
        assert_eq!(info.port, deserialized.port);
        assert_eq!(info.database_name, deserialized.database_name);
        assert_eq!(info.username, deserialized.username);
        assert_eq!(info.connection_string, deserialized.connection_string);
    }

    #[test]
    fn test_redis_connection_info_serialization() {
        let info = RedisConnectionInfo {
            host: "localhost".to_string(),
            port: 6379,
            password: Some("password".to_string()),
            connection_string: "redis://:password@localhost:6379".to_string(),
        };

        let json = serde_json::to_string(&info).unwrap();
        let deserialized: RedisConnectionInfo = serde_json::from_str(&json).unwrap();
        
        assert_eq!(info.host, deserialized.host);
        assert_eq!(info.port, deserialized.port);
        assert_eq!(info.password, deserialized.password);
        assert_eq!(info.connection_string, deserialized.connection_string);
    }

    #[test]
    fn test_container_info_serialization() {
        let mut port_mappings = HashMap::new();
        port_mappings.insert(8080, 8080);
        
        let mut env_vars = HashMap::new();
        env_vars.insert("ENV".to_string(), "test".to_string());

        let info = ContainerInfo {
            id: "container-1".to_string(),
            name: "test-container".to_string(),
            image: "nginx:latest".to_string(),
            port_mappings: port_mappings.clone(),
            environment_variables: env_vars.clone(),
            command: vec!["nginx".to_string(), "-g".to_string(), "daemon off;".to_string()],
            working_directory: Some("/app".to_string()),
        };

        let json = serde_json::to_string(&info).unwrap();
        let deserialized: ContainerInfo = serde_json::from_str(&json).unwrap();
        
        assert_eq!(info.id, deserialized.id);
        assert_eq!(info.name, deserialized.name);
        assert_eq!(info.image, deserialized.image);
        assert_eq!(info.port_mappings, deserialized.port_mappings);
        assert_eq!(info.environment_variables, deserialized.environment_variables);
        assert_eq!(info.command, deserialized.command);
        assert_eq!(info.working_directory, deserialized.working_directory);
    }
}