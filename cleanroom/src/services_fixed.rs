//! Fixed service implementations for cleanroom testing framework
//!
//! This module provides service implementations following core team best practices:
//! - Postgres service with connection management
//! - Redis service with command execution
//! - Service health checks and monitoring
//! - Connection pooling and management
//! - Service metrics and observability

use crate::error::{Result, CleanroomError};
use crate::serializable_instant::SerializableInstant;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::Instant;
use uuid::Uuid;

/// Postgres service implementation
#[derive(Debug, Clone)]
pub struct PostgresService {
    /// Service ID
    pub id: Uuid,
    /// Service name
    pub name: String,
    /// Database connection info
    pub connection_info: PostgresConnectionInfo,
    /// Service status
    pub status: ServiceStatus,
    /// Service metrics
    pub metrics: ServiceMetrics,
    /// Connection pool
    pub connection_pool: ConnectionPool,
    /// Health check configuration
    pub health_check: HealthCheckConfig,
}

/// Redis service implementation
#[derive(Debug, Clone)]
pub struct RedisService {
    /// Service ID
    pub id: Uuid,
    /// Service name
    pub name: String,
    /// Redis connection info
    pub connection_info: RedisConnectionInfo,
    /// Service status
    pub status: ServiceStatus,
    /// Service metrics
    pub metrics: ServiceMetrics,
    /// Connection pool
    pub connection_pool: ConnectionPool,
    /// Health check configuration
    pub health_check: HealthCheckConfig,
}

/// Postgres connection info
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PostgresConnectionInfo {
    /// Host
    pub host: String,
    /// Port
    pub port: u16,
    /// Database name
    pub database_name: String,
    /// Username
    pub username: String,
    /// Password
    pub password: String,
    /// Connection string
    pub connection_string: String,
    /// SSL mode
    pub ssl_mode: SslMode,
    /// Connection timeout
    pub connection_timeout: std::time::Duration,
    /// Query timeout
    pub query_timeout: std::time::Duration,
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
    /// Database number
    pub database_number: u8,
    /// Connection timeout
    pub connection_timeout: std::time::Duration,
    /// Command timeout
    pub command_timeout: std::time::Duration,
}

/// Service status enumeration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ServiceStatus {
    /// Service is starting
    Starting,
    /// Service is running
    Running,
    /// Service is stopped
    Stopped,
    /// Service has failed
    Failed,
    /// Service is healthy
    Healthy,
    /// Service is unhealthy
    Unhealthy,
    /// Service is degraded
    Degraded,
}

/// SSL mode enumeration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SslMode {
    /// Disable SSL
    Disable,
    /// Allow SSL
    Allow,
    /// Prefer SSL
    Prefer,
    /// Require SSL
    Require,
}

/// Service metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ServiceMetrics {
    /// Service ID
    pub service_id: Uuid,
    /// Total connections
    pub total_connections: usize,
    /// Active connections
    pub active_connections: usize,
    /// Failed connections
    pub failed_connections: usize,
    /// Total queries/commands
    pub total_queries: usize,
    /// Successful queries/commands
    pub successful_queries: usize,
    /// Failed queries/commands
    pub failed_queries: usize,
    /// Average response time in milliseconds
    pub average_response_time_ms: f64,
    /// Last health check time
    pub last_health_check: SerializableInstant,
    /// Health check status
    pub health_check_status: HealthCheckStatus,
    /// Performance metrics
    pub performance_metrics: HashMap<String, f64>,
}

/// Connection pool
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConnectionPool {
    /// Pool ID
    pub pool_id: Uuid,
    /// Maximum connections
    pub max_connections: usize,
    /// Minimum connections
    pub min_connections: usize,
    /// Current connections
    pub current_connections: usize,
    /// Available connections
    pub available_connections: usize,
    /// Pool configuration
    pub pool_config: PoolConfig,
}

/// Pool configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PoolConfig {
    /// Connection timeout
    pub connection_timeout: std::time::Duration,
    /// Idle timeout
    pub idle_timeout: std::time::Duration,
    /// Max lifetime
    pub max_lifetime: std::time::Duration,
    /// Health check interval
    pub health_check_interval: std::time::Duration,
}

/// Health check configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HealthCheckConfig {
    /// Health check command
    pub command: String,
    /// Health check interval
    pub interval: std::time::Duration,
    /// Health check timeout
    pub timeout: std::time::Duration,
    /// Number of retries
    pub retries: u32,
    /// Start period before health checks begin
    pub start_period: std::time::Duration,
}

/// Health check status
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HealthCheckStatus {
    /// Health check passed
    Passed,
    /// Health check failed
    Failed,
    /// Health check in progress
    InProgress,
    /// Health check not started
    NotStarted,
}

impl PostgresService {
    /// Create a new Postgres service
    pub fn new(
        name: String,
        host: String,
        port: u16,
        database_name: String,
        username: String,
        password: String,
    ) -> Self {
        let id = Uuid::new_v4();
        let connection_string = format!("postgresql://{}:{}@{}:{}/{}", username, password, host, port, database_name);
        
        let connection_info = PostgresConnectionInfo {
            host,
            port,
            database_name,
            username,
            password,
            connection_string,
            ssl_mode: SslMode::Prefer,
            connection_timeout: std::time::Duration::from_secs(30),
            query_timeout: std::time::Duration::from_secs(60),
        };

        let connection_pool = ConnectionPool::new(10, 2);
        let health_check = HealthCheckConfig::new("SELECT 1".to_string());

        Self {
            id,
            name,
            connection_info,
            status: ServiceStatus::Starting,
            metrics: ServiceMetrics::new(id),
            connection_pool,
            health_check,
        }
    }

    /// Connect to the database
    pub async fn connect(&mut self) -> Result<()> {
        if self.connection_info.connection_string.is_empty() {
            return Err(CleanroomError::service_error("Invalid connection string"));
        }

        // Simulate connection
        self.status = ServiceStatus::Running;
        self.metrics.total_connections += 1;
        self.metrics.active_connections += 1;
        self.metrics.last_updated = SerializableInstant::from(Instant::now());

        Ok(())
    }

    /// Disconnect from the database
    pub async fn disconnect(&mut self) -> Result<()> {
        self.status = ServiceStatus::Stopped;
        self.metrics.active_connections = self.metrics.active_connections.saturating_sub(1);
        self.metrics.last_updated = SerializableInstant::from(Instant::now());

        Ok(())
    }

    /// Execute SQL query
    pub async fn execute_query(&mut self, query: &str) -> Result<String> {
        if query.trim().is_empty() {
            return Err(CleanroomError::service_error("Empty SQL query"));
        }

        if !matches!(self.status, ServiceStatus::Running) {
            return Err(CleanroomError::service_error("Service is not running"));
        }

        let start_time = Instant::now();
        
        // Simulate query execution
        let result = format!("Query executed successfully: {}", query);
        
        let duration = start_time.elapsed();
        self.metrics.total_queries += 1;
        self.metrics.successful_queries += 1;
        self.metrics.average_response_time_ms = 
            (self.metrics.average_response_time_ms * (self.metrics.successful_queries - 1) as f64 + duration.as_millis() as f64) 
            / self.metrics.successful_queries as f64;
        self.metrics.last_updated = SerializableInstant::from(Instant::now());

        Ok(result)
    }

    /// Get connection info
    pub fn get_connection_info(&self) -> &PostgresConnectionInfo {
        &self.connection_info
    }

    /// Perform health check
    pub async fn health_check(&mut self) -> Result<bool> {
        let start_time = Instant::now();
        
        // Simulate health check
        let is_healthy = matches!(self.status, ServiceStatus::Running);
        
        let duration = start_time.elapsed();
        self.metrics.last_health_check = SerializableInstant::from(Instant::now());
        self.metrics.health_check_status = if is_healthy {
            HealthCheckStatus::Passed
        } else {
            HealthCheckStatus::Failed
        };

        if is_healthy {
            self.status = ServiceStatus::Healthy;
        } else {
            self.status = ServiceStatus::Unhealthy;
        }

        Ok(is_healthy)
    }

    /// Get service metrics
    pub fn get_metrics(&self) -> &ServiceMetrics {
        &self.metrics
    }

    /// Update service metrics
    pub fn update_metrics(&mut self) {
        self.metrics.last_updated = SerializableInstant::from(Instant::now());
    }
}

impl RedisService {
    /// Create a new Redis service
    pub fn new(
        name: String,
        host: String,
        port: u16,
        password: Option<String>,
    ) -> Self {
        let id = Uuid::new_v4();
        let connection_string = if let Some(pwd) = &password {
            format!("redis://:{}@{}:{}", pwd, host, port)
        } else {
            format!("redis://{}:{}", host, port)
        };
        
        let connection_info = RedisConnectionInfo {
            host,
            port,
            password,
            connection_string,
            database_number: 0,
            connection_timeout: std::time::Duration::from_secs(30),
            command_timeout: std::time::Duration::from_secs(60),
        };

        let connection_pool = ConnectionPool::new(10, 2);
        let health_check = HealthCheckConfig::new("PING".to_string());

        Self {
            id,
            name,
            connection_info,
            status: ServiceStatus::Starting,
            metrics: ServiceMetrics::new(id),
            connection_pool,
            health_check,
        }
    }

    /// Connect to Redis
    pub async fn connect(&mut self) -> Result<()> {
        if self.connection_info.connection_string.is_empty() {
            return Err(CleanroomError::service_error("Invalid connection string"));
        }

        // Simulate connection
        self.status = ServiceStatus::Running;
        self.metrics.total_connections += 1;
        self.metrics.active_connections += 1;
        self.metrics.last_updated = SerializableInstant::from(Instant::now());

        Ok(())
    }

    /// Disconnect from Redis
    pub async fn disconnect(&mut self) -> Result<()> {
        self.status = ServiceStatus::Stopped;
        self.metrics.active_connections = self.metrics.active_connections.saturating_sub(1);
        self.metrics.last_updated = SerializableInstant::from(Instant::now());

        Ok(())
    }

    /// Execute Redis command
    pub async fn execute_command(&mut self, command: &str) -> Result<String> {
        if command.trim().is_empty() {
            return Err(CleanroomError::service_error("Empty Redis command"));
        }

        if !matches!(self.status, ServiceStatus::Running) {
            return Err(CleanroomError::service_error("Service is not running"));
        }

        let start_time = Instant::now();
        
        // Simulate command execution
        let result = format!("Command executed successfully: {}", command);
        
        let duration = start_time.elapsed();
        self.metrics.total_queries += 1;
        self.metrics.successful_queries += 1;
        self.metrics.average_response_time_ms = 
            (self.metrics.average_response_time_ms * (self.metrics.successful_queries - 1) as f64 + duration.as_millis() as f64) 
            / self.metrics.successful_queries as f64;
        self.metrics.last_updated = SerializableInstant::from(Instant::now());

        Ok(result)
    }

    /// Get connection info
    pub fn get_connection_info(&self) -> &RedisConnectionInfo {
        &self.connection_info
    }

    /// Perform health check
    pub async fn health_check(&mut self) -> Result<bool> {
        let start_time = Instant::now();
        
        // Simulate health check
        let is_healthy = matches!(self.status, ServiceStatus::Running);
        
        let duration = start_time.elapsed();
        self.metrics.last_health_check = SerializableInstant::from(Instant::now());
        self.metrics.health_check_status = if is_healthy {
            HealthCheckStatus::Passed
        } else {
            HealthCheckStatus::Failed
        };

        if is_healthy {
            self.status = ServiceStatus::Healthy;
        } else {
            self.status = ServiceStatus::Unhealthy;
        }

        Ok(is_healthy)
    }

    /// Get service metrics
    pub fn get_metrics(&self) -> &ServiceMetrics {
        &self.metrics
    }

    /// Update service metrics
    pub fn update_metrics(&mut self) {
        self.metrics.last_updated = SerializableInstant::from(Instant::now());
    }
}

impl ServiceMetrics {
    /// Create new service metrics
    pub fn new(service_id: Uuid) -> Self {
        Self {
            service_id,
            total_connections: 0,
            active_connections: 0,
            failed_connections: 0,
            total_queries: 0,
            successful_queries: 0,
            failed_queries: 0,
            average_response_time_ms: 0.0,
            last_health_check: SerializableInstant::from(Instant::now()),
            health_check_status: HealthCheckStatus::NotStarted,
            performance_metrics: HashMap::new(),
        }
    }

    /// Get success rate
    pub fn get_success_rate(&self) -> f64 {
        if self.total_queries > 0 {
            (self.successful_queries as f64 / self.total_queries as f64) * 100.0
        } else {
            0.0
        }
    }

    /// Get connection success rate
    pub fn get_connection_success_rate(&self) -> f64 {
        let total_attempts = self.total_connections + self.failed_connections;
        if total_attempts > 0 {
            (self.total_connections as f64 / total_attempts as f64) * 100.0
        } else {
            0.0
        }
    }

    /// Check if service is healthy
    pub fn is_healthy(&self) -> bool {
        matches!(self.health_check_status, HealthCheckStatus::Passed)
    }

    /// Update performance metric
    pub fn update_performance_metric(&mut self, name: String, value: f64) {
        self.performance_metrics.insert(name, value);
    }
}

impl ConnectionPool {
    /// Create new connection pool
    pub fn new(max_connections: usize, min_connections: usize) -> Self {
        Self {
            pool_id: Uuid::new_v4(),
            max_connections,
            min_connections,
            current_connections: 0,
            available_connections: 0,
            pool_config: PoolConfig::default(),
        }
    }

    /// Get connection from pool
    pub fn get_connection(&mut self) -> Result<()> {
        if self.available_connections > 0 {
            self.available_connections -= 1;
            Ok(())
        } else if self.current_connections < self.max_connections {
            self.current_connections += 1;
            Ok(())
        } else {
            Err(CleanroomError::service_error("No available connections"))
        }
    }

    /// Return connection to pool
    pub fn return_connection(&mut self) -> Result<()> {
        if self.current_connections > 0 {
            self.current_connections -= 1;
            self.available_connections += 1;
            Ok(())
        } else {
            Err(CleanroomError::service_error("No connections to return"))
        }
    }

    /// Get pool status
    pub fn get_pool_status(&self) -> PoolStatus {
        PoolStatus {
            max_connections: self.max_connections,
            min_connections: self.min_connections,
            current_connections: self.current_connections,
            available_connections: self.available_connections,
            utilization_percentage: (self.current_connections as f64 / self.max_connections as f64) * 100.0,
        }
    }
}

impl Default for PoolConfig {
    fn default() -> Self {
        Self {
            connection_timeout: std::time::Duration::from_secs(30),
            idle_timeout: std::time::Duration::from_secs(300),
            max_lifetime: std::time::Duration::from_secs(3600),
            health_check_interval: std::time::Duration::from_secs(60),
        }
    }
}

impl HealthCheckConfig {
    /// Create new health check configuration
    pub fn new(command: String) -> Self {
        Self {
            command,
            interval: std::time::Duration::from_secs(30),
            timeout: std::time::Duration::from_secs(10),
            retries: 3,
            start_period: std::time::Duration::from_secs(30),
        }
    }
}

/// Pool status
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PoolStatus {
    /// Maximum connections
    pub max_connections: usize,
    /// Minimum connections
    pub min_connections: usize,
    /// Current connections
    pub current_connections: usize,
    /// Available connections
    pub available_connections: usize,
    /// Utilization percentage
    pub utilization_percentage: f64,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_postgres_service_creation() {
        let service = PostgresService::new(
            "test-postgres".to_string(),
            "localhost".to_string(),
            5432,
            "testdb".to_string(),
            "postgres".to_string(),
            "password".to_string(),
        );

        assert_eq!(service.name, "test-postgres");
        assert_eq!(service.connection_info.host, "localhost");
        assert_eq!(service.connection_info.port, 5432);
        assert_eq!(service.connection_info.database_name, "testdb");
        assert_eq!(service.connection_info.username, "postgres");
        assert_eq!(service.connection_info.password, "password");
        assert!(matches!(service.status, ServiceStatus::Starting));
        assert!(service.connection_info.connection_string.contains("postgresql://"));
    }

    #[tokio::test]
    async fn test_postgres_service_connect() {
        let mut service = PostgresService::new(
            "test-postgres".to_string(),
            "localhost".to_string(),
            5432,
            "testdb".to_string(),
            "postgres".to_string(),
            "password".to_string(),
        );

        let result = service.connect().await;
        assert!(result.is_ok());
        assert!(matches!(service.status, ServiceStatus::Running));
        assert_eq!(service.metrics.total_connections, 1);
        assert_eq!(service.metrics.active_connections, 1);
    }

    #[tokio::test]
    async fn test_postgres_service_disconnect() {
        let mut service = PostgresService::new(
            "test-postgres".to_string(),
            "localhost".to_string(),
            5432,
            "testdb".to_string(),
            "postgres".to_string(),
            "password".to_string(),
        );

        service.connect().await.unwrap();
        let result = service.disconnect().await;
        assert!(result.is_ok());
        assert!(matches!(service.status, ServiceStatus::Stopped));
        assert_eq!(service.metrics.active_connections, 0);
    }

    #[tokio::test]
    async fn test_postgres_service_execute_query() {
        let mut service = PostgresService::new(
            "test-postgres".to_string(),
            "localhost".to_string(),
            5432,
            "testdb".to_string(),
            "postgres".to_string(),
            "password".to_string(),
        );

        service.connect().await.unwrap();
        
        let result = service.execute_query("SELECT * FROM users").await;
        assert!(result.is_ok());
        assert!(result.unwrap().contains("Query executed successfully"));
        assert_eq!(service.metrics.total_queries, 1);
        assert_eq!(service.metrics.successful_queries, 1);
        assert!(service.metrics.average_response_time_ms > 0.0);
    }

    #[tokio::test]
    async fn test_postgres_service_execute_empty_query() {
        let mut service = PostgresService::new(
            "test-postgres".to_string(),
            "localhost".to_string(),
            5432,
            "testdb".to_string(),
            "postgres".to_string(),
            "password".to_string(),
        );

        service.connect().await.unwrap();
        
        let result = service.execute_query("").await;
        assert!(result.is_err());
        assert!(result.unwrap_err().message().contains("Empty SQL query"));
    }

    #[tokio::test]
    async fn test_postgres_service_execute_query_not_running() {
        let mut service = PostgresService::new(
            "test-postgres".to_string(),
            "localhost".to_string(),
            5432,
            "testdb".to_string(),
            "postgres".to_string(),
            "password".to_string(),
        );

        let result = service.execute_query("SELECT * FROM users").await;
        assert!(result.is_err());
        assert!(result.unwrap_err().message().contains("not running"));
    }

    #[tokio::test]
    async fn test_postgres_service_health_check() {
        let mut service = PostgresService::new(
            "test-postgres".to_string(),
            "localhost".to_string(),
            5432,
            "testdb".to_string(),
            "postgres".to_string(),
            "password".to_string(),
        );

        service.connect().await.unwrap();
        
        let result = service.health_check().await;
        assert!(result.is_ok());
        assert!(result.unwrap());
        assert!(matches!(service.status, ServiceStatus::Healthy));
        assert!(matches!(service.metrics.health_check_status, HealthCheckStatus::Passed));
    }

    #[test]
    fn test_postgres_service_get_connection_info() {
        let service = PostgresService::new(
            "test-postgres".to_string(),
            "localhost".to_string(),
            5432,
            "testdb".to_string(),
            "postgres".to_string(),
            "password".to_string(),
        );

        let info = service.get_connection_info();
        assert_eq!(info.host, "localhost");
        assert_eq!(info.port, 5432);
        assert_eq!(info.database_name, "testdb");
        assert_eq!(info.username, "postgres");
        assert_eq!(info.password, "password");
    }

    #[test]
    fn test_redis_service_creation() {
        let service = RedisService::new(
            "test-redis".to_string(),
            "localhost".to_string(),
            6379,
            Some("password".to_string()),
        );

        assert_eq!(service.name, "test-redis");
        assert_eq!(service.connection_info.host, "localhost");
        assert_eq!(service.connection_info.port, 6379);
        assert_eq!(service.connection_info.password, Some("password".to_string()));
        assert!(matches!(service.status, ServiceStatus::Starting));
        assert!(service.connection_info.connection_string.contains("redis://"));
    }

    #[tokio::test]
    async fn test_redis_service_connect() {
        let mut service = RedisService::new(
            "test-redis".to_string(),
            "localhost".to_string(),
            6379,
            None,
        );

        let result = service.connect().await;
        assert!(result.is_ok());
        assert!(matches!(service.status, ServiceStatus::Running));
        assert_eq!(service.metrics.total_connections, 1);
        assert_eq!(service.metrics.active_connections, 1);
    }

    #[tokio::test]
    async fn test_redis_service_disconnect() {
        let mut service = RedisService::new(
            "test-redis".to_string(),
            "localhost".to_string(),
            6379,
            None,
        );

        service.connect().await.unwrap();
        let result = service.disconnect().await;
        assert!(result.is_ok());
        assert!(matches!(service.status, ServiceStatus::Stopped));
        assert_eq!(service.metrics.active_connections, 0);
    }

    #[tokio::test]
    async fn test_redis_service_execute_command() {
        let mut service = RedisService::new(
            "test-redis".to_string(),
            "localhost".to_string(),
            6379,
            None,
        );

        service.connect().await.unwrap();
        
        let result = service.execute_command("GET key").await;
        assert!(result.is_ok());
        assert!(result.unwrap().contains("Command executed successfully"));
        assert_eq!(service.metrics.total_queries, 1);
        assert_eq!(service.metrics.successful_queries, 1);
        assert!(service.metrics.average_response_time_ms > 0.0);
    }

    #[tokio::test]
    async fn test_redis_service_execute_empty_command() {
        let mut service = RedisService::new(
            "test-redis".to_string(),
            "localhost".to_string(),
            6379,
            None,
        );

        service.connect().await.unwrap();
        
        let result = service.execute_command("").await;
        assert!(result.is_err());
        assert!(result.unwrap_err().message().contains("Empty Redis command"));
    }

    #[tokio::test]
    async fn test_redis_service_execute_command_not_running() {
        let mut service = RedisService::new(
            "test-redis".to_string(),
            "localhost".to_string(),
            6379,
            None,
        );

        let result = service.execute_command("GET key").await;
        assert!(result.is_err());
        assert!(result.unwrap_err().message().contains("not running"));
    }

    #[tokio::test]
    async fn test_redis_service_health_check() {
        let mut service = RedisService::new(
            "test-redis".to_string(),
            "localhost".to_string(),
            6379,
            None,
        );

        service.connect().await.unwrap();
        
        let result = service.health_check().await;
        assert!(result.is_ok());
        assert!(result.unwrap());
        assert!(matches!(service.status, ServiceStatus::Healthy));
        assert!(matches!(service.metrics.health_check_status, HealthCheckStatus::Passed));
    }

    #[test]
    fn test_redis_service_get_connection_info() {
        let service = RedisService::new(
            "test-redis".to_string(),
            "localhost".to_string(),
            6379,
            Some("password".to_string()),
        );

        let info = service.get_connection_info();
        assert_eq!(info.host, "localhost");
        assert_eq!(info.port, 6379);
        assert_eq!(info.password, Some("password".to_string()));
        assert_eq!(info.database_number, 0);
    }

    #[test]
    fn test_service_metrics_creation() {
        let service_id = Uuid::new_v4();
        let metrics = ServiceMetrics::new(service_id);
        
        assert_eq!(metrics.service_id, service_id);
        assert_eq!(metrics.total_connections, 0);
        assert_eq!(metrics.active_connections, 0);
        assert_eq!(metrics.failed_connections, 0);
        assert_eq!(metrics.total_queries, 0);
        assert_eq!(metrics.successful_queries, 0);
        assert_eq!(metrics.failed_queries, 0);
        assert_eq!(metrics.average_response_time_ms, 0.0);
        assert!(matches!(metrics.health_check_status, HealthCheckStatus::NotStarted));
    }

    #[test]
    fn test_service_metrics_get_success_rate() {
        let service_id = Uuid::new_v4();
        let mut metrics = ServiceMetrics::new(service_id);
        
        metrics.total_queries = 10;
        metrics.successful_queries = 8;
        
        assert_eq!(metrics.get_success_rate(), 80.0);
        
        metrics.total_queries = 0;
        assert_eq!(metrics.get_success_rate(), 0.0);
    }

    #[test]
    fn test_service_metrics_get_connection_success_rate() {
        let service_id = Uuid::new_v4();
        let mut metrics = ServiceMetrics::new(service_id);
        
        metrics.total_connections = 8;
        metrics.failed_connections = 2;
        
        assert_eq!(metrics.get_connection_success_rate(), 80.0);
        
        metrics.total_connections = 0;
        metrics.failed_connections = 0;
        assert_eq!(metrics.get_connection_success_rate(), 0.0);
    }

    #[test]
    fn test_service_metrics_is_healthy() {
        let service_id = Uuid::new_v4();
        let mut metrics = ServiceMetrics::new(service_id);
        
        assert!(!metrics.is_healthy());
        
        metrics.health_check_status = HealthCheckStatus::Passed;
        assert!(metrics.is_healthy());
        
        metrics.health_check_status = HealthCheckStatus::Failed;
        assert!(!metrics.is_healthy());
    }

    #[test]
    fn test_service_metrics_update_performance_metric() {
        let service_id = Uuid::new_v4();
        let mut metrics = ServiceMetrics::new(service_id);
        
        metrics.update_performance_metric("cpu_usage".to_string(), 75.5);
        assert_eq!(metrics.performance_metrics.get("cpu_usage"), Some(&75.5));
    }

    #[test]
    fn test_connection_pool_creation() {
        let pool = ConnectionPool::new(10, 2);
        
        assert_eq!(pool.max_connections, 10);
        assert_eq!(pool.min_connections, 2);
        assert_eq!(pool.current_connections, 0);
        assert_eq!(pool.available_connections, 0);
    }

    #[test]
    fn test_connection_pool_get_connection() {
        let mut pool = ConnectionPool::new(10, 2);
        
        let result = pool.get_connection();
        assert!(result.is_ok());
        assert_eq!(pool.current_connections, 1);
        
        // Fill up the pool
        for _ in 0..9 {
            pool.get_connection().unwrap();
        }
        
        let result = pool.get_connection();
        assert!(result.is_err());
        assert!(result.unwrap_err().message().contains("No available connections"));
    }

    #[test]
    fn test_connection_pool_return_connection() {
        let mut pool = ConnectionPool::new(10, 2);
        
        pool.get_connection().unwrap();
        assert_eq!(pool.current_connections, 1);
        
        let result = pool.return_connection();
        assert!(result.is_ok());
        assert_eq!(pool.current_connections, 0);
        assert_eq!(pool.available_connections, 1);
        
        let result = pool.return_connection();
        assert!(result.is_err());
        assert!(result.unwrap_err().message().contains("No connections to return"));
    }

    #[test]
    fn test_connection_pool_get_pool_status() {
        let mut pool = ConnectionPool::new(10, 2);
        
        pool.get_connection().unwrap();
        pool.get_connection().unwrap();
        
        let status = pool.get_pool_status();
        assert_eq!(status.max_connections, 10);
        assert_eq!(status.min_connections, 2);
        assert_eq!(status.current_connections, 2);
        assert_eq!(status.available_connections, 0);
        assert_eq!(status.utilization_percentage, 20.0);
    }

    #[test]
    fn test_health_check_config_creation() {
        let config = HealthCheckConfig::new("SELECT 1".to_string());
        
        assert_eq!(config.command, "SELECT 1");
        assert_eq!(config.interval.as_secs(), 30);
        assert_eq!(config.timeout.as_secs(), 10);
        assert_eq!(config.retries, 3);
        assert_eq!(config.start_period.as_secs(), 30);
    }

    #[test]
    fn test_service_status_serialization() {
        let statuses = vec![
            ServiceStatus::Starting,
            ServiceStatus::Running,
            ServiceStatus::Stopped,
            ServiceStatus::Failed,
            ServiceStatus::Healthy,
            ServiceStatus::Unhealthy,
            ServiceStatus::Degraded,
        ];

        for status in statuses {
            let json = serde_json::to_string(&status).unwrap();
            let deserialized: ServiceStatus = serde_json::from_str(&json).unwrap();
            assert_eq!(status, deserialized);
        }
    }

    #[test]
    fn test_ssl_mode_serialization() {
        let modes = vec![
            SslMode::Disable,
            SslMode::Allow,
            SslMode::Prefer,
            SslMode::Require,
        ];

        for mode in modes {
            let json = serde_json::to_string(&mode).unwrap();
            let deserialized: SslMode = serde_json::from_str(&json).unwrap();
            assert_eq!(mode, deserialized);
        }
    }

    #[test]
    fn test_health_check_status_serialization() {
        let statuses = vec![
            HealthCheckStatus::Passed,
            HealthCheckStatus::Failed,
            HealthCheckStatus::InProgress,
            HealthCheckStatus::NotStarted,
        ];

        for status in statuses {
            let json = serde_json::to_string(&status).unwrap();
            let deserialized: HealthCheckStatus = serde_json::from_str(&json).unwrap();
            assert_eq!(status, deserialized);
        }
    }

    #[test]
    fn test_postgres_connection_info_serialization() {
        let info = PostgresConnectionInfo {
            host: "localhost".to_string(),
            port: 5432,
            database_name: "testdb".to_string(),
            username: "postgres".to_string(),
            password: "password".to_string(),
            connection_string: "postgresql://postgres:password@localhost:5432/testdb".to_string(),
            ssl_mode: SslMode::Prefer,
            connection_timeout: std::time::Duration::from_secs(30),
            query_timeout: std::time::Duration::from_secs(60),
        };

        let json = serde_json::to_string(&info).unwrap();
        let deserialized: PostgresConnectionInfo = serde_json::from_str(&json).unwrap();
        
        assert_eq!(info.host, deserialized.host);
        assert_eq!(info.port, deserialized.port);
        assert_eq!(info.database_name, deserialized.database_name);
        assert_eq!(info.username, deserialized.username);
        assert_eq!(info.password, deserialized.password);
        assert_eq!(info.connection_string, deserialized.connection_string);
        assert_eq!(info.ssl_mode, deserialized.ssl_mode);
    }

    #[test]
    fn test_redis_connection_info_serialization() {
        let info = RedisConnectionInfo {
            host: "localhost".to_string(),
            port: 6379,
            password: Some("password".to_string()),
            connection_string: "redis://:password@localhost:6379".to_string(),
            database_number: 0,
            connection_timeout: std::time::Duration::from_secs(30),
            command_timeout: std::time::Duration::from_secs(60),
        };

        let json = serde_json::to_string(&info).unwrap();
        let deserialized: RedisConnectionInfo = serde_json::from_str(&json).unwrap();
        
        assert_eq!(info.host, deserialized.host);
        assert_eq!(info.port, deserialized.port);
        assert_eq!(info.password, deserialized.password);
        assert_eq!(info.connection_string, deserialized.connection_string);
        assert_eq!(info.database_number, deserialized.database_number);
    }

    #[test]
    fn test_service_metrics_serialization() {
        let service_id = Uuid::new_v4();
        let mut metrics = ServiceMetrics::new(service_id);
        metrics.total_connections = 5;
        metrics.active_connections = 3;
        metrics.failed_connections = 1;
        metrics.total_queries = 10;
        metrics.successful_queries = 8;
        metrics.failed_queries = 2;
        metrics.average_response_time_ms = 150.5;
        metrics.performance_metrics.insert("cpu_usage".to_string(), 75.5);

        let json = serde_json::to_string(&metrics).unwrap();
        let deserialized: ServiceMetrics = serde_json::from_str(&json).unwrap();
        
        assert_eq!(metrics.service_id, deserialized.service_id);
        assert_eq!(metrics.total_connections, deserialized.total_connections);
        assert_eq!(metrics.active_connections, deserialized.active_connections);
        assert_eq!(metrics.failed_connections, deserialized.failed_connections);
        assert_eq!(metrics.total_queries, deserialized.total_queries);
        assert_eq!(metrics.successful_queries, deserialized.successful_queries);
        assert_eq!(metrics.failed_queries, deserialized.failed_queries);
        assert_eq!(metrics.average_response_time_ms, deserialized.average_response_time_ms);
        assert_eq!(metrics.performance_metrics, deserialized.performance_metrics);
    }

    #[test]
    fn test_connection_pool_serialization() {
        let mut pool = ConnectionPool::new(10, 2);
        pool.get_connection().unwrap();
        pool.get_connection().unwrap();

        let json = serde_json::to_string(&pool).unwrap();
        let deserialized: ConnectionPool = serde_json::from_str(&json).unwrap();
        
        assert_eq!(pool.pool_id, deserialized.pool_id);
        assert_eq!(pool.max_connections, deserialized.max_connections);
        assert_eq!(pool.min_connections, deserialized.min_connections);
        assert_eq!(pool.current_connections, deserialized.current_connections);
        assert_eq!(pool.available_connections, deserialized.available_connections);
    }

    #[test]
    fn test_pool_config_serialization() {
        let config = PoolConfig::default();

        let json = serde_json::to_string(&config).unwrap();
        let deserialized: PoolConfig = serde_json::from_str(&json).unwrap();
        
        assert_eq!(config.connection_timeout, deserialized.connection_timeout);
        assert_eq!(config.idle_timeout, deserialized.idle_timeout);
        assert_eq!(config.max_lifetime, deserialized.max_lifetime);
        assert_eq!(config.health_check_interval, deserialized.health_check_interval);
    }

    #[test]
    fn test_health_check_config_serialization() {
        let config = HealthCheckConfig::new("SELECT 1".to_string());

        let json = serde_json::to_string(&config).unwrap();
        let deserialized: HealthCheckConfig = serde_json::from_str(&json).unwrap();
        
        assert_eq!(config.command, deserialized.command);
        assert_eq!(config.interval, deserialized.interval);
        assert_eq!(config.timeout, deserialized.timeout);
        assert_eq!(config.retries, deserialized.retries);
        assert_eq!(config.start_period, deserialized.start_period);
    }

    #[test]
    fn test_pool_status_serialization() {
        let status = PoolStatus {
            max_connections: 10,
            min_connections: 2,
            current_connections: 5,
            available_connections: 3,
            utilization_percentage: 50.0,
        };

        let json = serde_json::to_string(&status).unwrap();
        let deserialized: PoolStatus = serde_json::from_str(&json).unwrap();
        
        assert_eq!(status.max_connections, deserialized.max_connections);
        assert_eq!(status.min_connections, deserialized.min_connections);
        assert_eq!(status.current_connections, deserialized.current_connections);
        assert_eq!(status.available_connections, deserialized.available_connections);
        assert_eq!(status.utilization_percentage, deserialized.utilization_percentage);
    }
}
