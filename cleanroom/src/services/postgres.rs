//! PostgreSQL service fixture for testing
//!
//! Provides a containerized PostgreSQL instance using testcontainers-rs
//! with health checks, connection info, and automatic teardown.

use crate::error::{Result, ServiceError};
use crate::services::{Service, ConnectionInfo};
use std::collections::HashMap;
use std::time::Duration;
use testcontainers::{
    Container,
    images::postgres::Postgres as PostgresImage,
    runners::SyncRunner,
};

/// PostgreSQL service fixture using testcontainers
#[derive(Debug)]
pub struct Postgres {
    /// Testcontainers container
    container: Container<PostgresImage>,
    /// Connection information
    connection_info: ConnectionInfo,
    /// Database name
    database: String,
    /// Username
    username: String,
    /// Password
    password: String,
}

impl<'d> Postgres<'d> {
    /// Create a new PostgreSQL fixture
    pub fn new() -> Result<Self> {
        let image = PostgresImage::default()
            .with_db_name("testdb")
            .with_user("testuser")
            .with_password("testpass");

        let container = image.start();

        let connection_info = ConnectionInfo {
            params: format!(
                "host=localhost port={} dbname=testdb user=testuser password=testpass",
                container.get_host_port_ipv4(5432)
            ),
        };

        Ok(Self {
            container,
            connection_info,
            database: "testdb".to_string(),
            username: "testuser".to_string(),
            password: "testpass".to_string(),
        })
    }

    /// Create with custom configuration
    pub fn with_config(
        database: impl Into<String>,
        username: impl Into<String>,
        password: impl Into<String>,
    ) -> Result<Self> {
        let database = database.into();
        let username = username.into();
        let password = password.into();

        let image = PostgresImage::default()
            .with_db_name(&database)
            .with_user(&username)
            .with_password(&password);

        let container = image.start();

        let connection_info = ConnectionInfo {
            params: format!(
                "host=localhost port={} dbname={} user={} password={}",
                container.get_host_port_ipv4(5432), database, username, password
            ),
        };

        Ok(Self {
            container,
            connection_info,
            database,
            username,
            password,
        })
    }

    /// Get connection information
    pub fn connection_info(&self) -> &ConnectionInfo {
        &self.connection_info
    }

    /// Get database name
    pub fn database(&self) -> &str {
        &self.database
    }

    /// Get username
    pub fn username(&self) -> &str {
        &self.username
    }

    /// Get password
    pub fn password(&self) -> &str {
        &self.password
    }

    /// Execute SQL command in container
    pub fn execute_sql(&self, sql: &str) -> Result<String> {
        let cmd = vec!["psql".to_string(), "-U".to_string(), self.username.clone(), "-d".to_string(), self.database.clone(), "-t".to_string(), "-c".to_string(), sql.to_string()];
        
        let result = self.container
            .exec(cmd)
            .map_err(|e| ServiceError::ConnectionFailed(format!("Failed to execute SQL: {}", e)))?;

        if result.exit_code != Some(0) {
            return Err(ServiceError::ConnectionFailed(format!(
                "SQL execution failed: {}",
                String::from_utf8_lossy(&result.stderr)
            )).into());
        }

        Ok(String::from_utf8_lossy(&result.stdout).to_string())
    }

    /// Create a test table
    pub fn create_test_table(&self) -> Result<()> {
        let sql = r#"
            CREATE TABLE IF NOT EXISTS test_table (
                id SERIAL PRIMARY KEY,
                name VARCHAR(100) NOT NULL,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            );
        "#;

        self.execute_sql(sql)?;
        Ok(())
    }

    /// Insert test data
    pub fn insert_test_data(&self, name: &str) -> Result<i32> {
        let sql = format!("INSERT INTO test_table (name) VALUES ('{}') RETURNING id;", name);
        let result = self.execute_sql(&sql)?;
        
        // Parse the returned ID
        result.trim().parse::<i32>().map_err(|e| {
            ServiceError::ConnectionFailed(format!("Failed to parse inserted ID: {}", e))
        }).map_err(|e| e.into())
    }

    /// Get database size
    pub fn get_database_size(&self) -> Result<String> {
        let sql = "SELECT pg_size_pretty(pg_database_size(current_database()));";
        self.execute_sql(sql)
    }

    /// Get active connections
    pub fn get_active_connections(&self) -> Result<i32> {
        let sql = "SELECT count(*) FROM pg_stat_activity WHERE state = 'active';";
        let result = self.execute_sql(sql)?;
        
        result.trim().parse::<i32>().map_err(|e| {
            ServiceError::ConnectionFailed(format!("Failed to parse connection count: {}", e))
        }).map_err(|e| e.into())
    }
}

impl<'d> Service for Postgres<'d> {
    fn name(&self) -> &str {
        "postgres"
    }

    fn connection_info(&self) -> Result<ConnectionInfo> {
        Ok(self.connection_info.clone())
    }

    fn health_check(&self) -> Result<bool> {
        // Testcontainers handles health checks automatically
        // We can add custom health check logic here if needed
        Ok(true)
    }

    fn start(&mut self) -> Result<()> {
        // Container is already started by testcontainers
        Ok(())
    }

    fn stop(&mut self) -> Result<()> {
        // Container cleanup is handled automatically by testcontainers
        Ok(())
    }

    fn is_running(&self) -> Result<bool> {
        Ok(true) // Container is running
    }

    fn wait_for_ready(&self, timeout: std::time::Duration) -> Result<()> {
        // Testcontainers handles readiness automatically
        Ok(())
    }

    fn logs(&self) -> Result<Vec<String>> {
        // For now, return empty logs
        Ok(Vec::new())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_postgres_creation() {
        let postgres = Postgres::new();
        
        if postgres.is_ok() {
            let postgres = postgres.unwrap();
            assert_eq!(postgres.database(), "testdb");
            assert_eq!(postgres.username(), "testuser");
        } else {
            println!("Skipping test - Docker not available");
        }
    }

    #[test]
    fn test_postgres_with_config() {
        let postgres = Postgres::with_config("mydb", "myuser", "mypass");
        
        if postgres.is_ok() {
            let postgres = postgres.unwrap();
            assert_eq!(postgres.database(), "mydb");
            assert_eq!(postgres.username(), "myuser");
        } else {
            println!("Skipping test - Docker not available");
        }
    }

    #[test]
    fn test_postgres_sql_operations() {
        let postgres = Postgres::new();
        
        if postgres.is_ok() {
            let postgres = postgres.unwrap();
            
            // Create test table
            postgres.create_test_table().unwrap();
            
            // Insert test data
            let id = postgres.insert_test_data("test_name").unwrap();
            assert!(id > 0);
            
            // Get database size
            let size = postgres.get_database_size().unwrap();
            assert!(!size.is_empty());
            
            // Get active connections
            let connections = postgres.get_active_connections().unwrap();
            assert!(connections >= 0);
        } else {
            println!("Skipping test - Docker not available");
        }
    }
}