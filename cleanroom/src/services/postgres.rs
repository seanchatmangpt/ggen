//! PostgreSQL service fixture for testing
//!
//! Provides a containerized PostgreSQL instance with health checks,
//! connection info, and automatic teardown.

use crate::error::{Result, ServiceError};
use crate::services::{ConnectionInfo, Service};
use std::collections::HashMap;
use std::process::Command;
use std::thread;
use std::time::{Duration, Instant};

/// PostgreSQL service fixture
/// WIP: Implement PostgreSQL service fixture
#[derive(Debug)]
pub struct Postgres {
    /// Container name for cleanup
    container_name: String,
    /// Port mapping
    port: u16,
    /// Database name
    database: String,
    /// Username
    username: String,
    /// Password
    password: String,
    /// Started flag
    started: bool,
}

impl Postgres {
    /// Create a new PostgreSQL fixture
    pub fn new() -> Result<Self> {
        let container_name = format!("cleanroom-postgres-{}", rand::random::<u32>());

        Ok(Self {
            container_name,
            port: 5432,
            database: "testdb".to_string(),
            username: "testuser".to_string(),
            password: "testpass".to_string(),
            started: false,
        })
    }

    /// Create with custom configuration
    pub fn with_config(
        port: u16, database: impl Into<String>, username: impl Into<String>,
        password: impl Into<String>,
    ) -> Result<Self> {
        let container_name = format!("cleanroom-postgres-{}", rand::random::<u32>());

        Ok(Self {
            container_name,
            port,
            database: database.into(),
            username: username.into(),
            password: password.into(),
            started: false,
        })
    }

    /// Start PostgreSQL container
    fn start_container(&mut self) -> Result<()> {
        if self.started {
            return Ok(());
        }

        // Check if Docker is available
        let docker_available = Command::new("docker")
            .arg("version")
            .output()
            .map(|output| output.status.success())
            .unwrap_or(false);

        if !docker_available {
            return Err(
                ServiceError::Msg("Docker not available for PostgreSQL fixture".into()).into(),
            );
        }

        // Pull PostgreSQL image
        let pull_output = Command::new("docker")
            .arg("pull")
            .arg("postgres:15-alpine")
            .output()
            .map_err(|e| ServiceError::Msg(format!("Failed to pull PostgreSQL image: {}", e)))?;

        if !pull_output.status.success() {
            return Err(ServiceError::Msg(format!(
                "Failed to pull PostgreSQL image: {}",
                String::from_utf8_lossy(&pull_output.stderr)
            ))
            .into());
        }

        // Start PostgreSQL container
        let start_output = Command::new("docker")
            .arg("run")
            .arg("-d")
            .arg("--name")
            .arg(&self.container_name)
            .arg("-p")
            .arg(format!("{}:5432", self.port))
            .arg("-e")
            .arg(format!("POSTGRES_DB={}", self.database))
            .arg("-e")
            .arg(format!("POSTGRES_USER={}", self.username))
            .arg("-e")
            .arg(format!("POSTGRES_PASSWORD={}", self.password))
            .arg("postgres:15-alpine")
            .output()
            .map_err(|e| {
                ServiceError::Msg(format!("Failed to start PostgreSQL container: {}", e))
            })?;

        if !start_output.status.success() {
            return Err(ServiceError::Msg(format!(
                "Failed to start PostgreSQL container: {}",
                String::from_utf8_lossy(&start_output.stderr)
            ))
            .into());
        }

        self.started = true;
        Ok(())
    }

    /// Wait for PostgreSQL to be ready
    fn wait_for_ready(&self) -> Result<()> {
        let max_wait = Duration::from_secs(30);
        let start = Instant::now();
        let check_interval = Duration::from_millis(500);

        while start.elapsed() < max_wait {
            if self.health_check()? {
                return Ok(());
            }
            thread::sleep(check_interval);
        }

        Err(ServiceError::StartupTimeout(format!(
            "PostgreSQL container {} did not become ready within {} seconds",
            self.container_name,
            max_wait.as_secs()
        ))
        .into())
    }
}

impl Service for Postgres {
    fn name(&self) -> &str {
        "postgres"
    }

    fn health_check(&self) -> Result<bool> {
        if !self.started {
            return Ok(false);
        }

        // Check if container is running
        let status_output = Command::new("docker")
            .arg("ps")
            .arg("-q")
            .arg("-f")
            .arg(format!("name={}", self.container_name))
            .output()
            .map_err(|e| {
                ServiceError::HealthCheckFailed(format!("Failed to check container status: {}", e))
            })?;

        if status_output.stdout.is_empty() {
            return Ok(false);
        }

        // Try to connect to PostgreSQL
        let psql_output = Command::new("docker")
            .arg("exec")
            .arg(&self.container_name)
            .arg("psql")
            .arg("-U")
            .arg(&self.username)
            .arg("-d")
            .arg(&self.database)
            .arg("-c")
            .arg("SELECT 1;")
            .output()
            .map_err(|e| {
                ServiceError::HealthCheckFailed(format!("Failed to connect to PostgreSQL: {}", e))
            })?;

        Ok(psql_output.status.success())
    }

    fn connection_info(&self) -> Result<ConnectionInfo> {
        if !self.started {
            return Err(ServiceError::Msg("PostgreSQL service not started".into()).into());
        }

        let mut params = HashMap::new();
        params.insert("database".to_string(), self.database.clone());
        params.insert("username".to_string(), self.username.clone());
        params.insert("password".to_string(), self.password.clone());

        Ok(ConnectionInfo {
            host: "localhost".to_string(),
            port: self.port,
            params,
        })
    }

    fn start(&mut self) -> Result<()> {
        self.start_container()?;
        self.wait_for_ready()?;
        Ok(())
    }

    fn stop(&mut self) -> Result<()> {
        if !self.started {
            return Ok(());
        }

        // Stop and remove container
        let _stop_output = Command::new("docker")
            .arg("stop")
            .arg(&self.container_name)
            .output();

        let _remove_output = Command::new("docker")
            .arg("rm")
            .arg(&self.container_name)
            .output();

        self.started = false;
        Ok(())
    }
}

impl Drop for Postgres {
    fn drop(&mut self) {
        let _ = self.stop();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_postgres_creation() {
        let postgres = Postgres::new().unwrap();
        assert_eq!(postgres.name(), "postgres");
        assert_eq!(postgres.port, 5432);
        assert_eq!(postgres.database, "testdb");
        assert_eq!(postgres.username, "testuser");
        assert!(!postgres.started);
    }

    #[test]
    fn test_postgres_with_config() {
        let postgres = Postgres::with_config(5433, "mydb", "myuser", "mypass").unwrap();
        assert_eq!(postgres.port, 5433);
        assert_eq!(postgres.database, "mydb");
        assert_eq!(postgres.username, "myuser");
        assert_eq!(postgres.password, "mypass");
    }

    #[test]
    fn test_postgres_health_check_not_started() {
        let postgres = Postgres::new().unwrap();
        assert!(!postgres.health_check().unwrap());
    }

    #[test]
    fn test_postgres_connection_info_not_started() {
        let postgres = Postgres::new().unwrap();
        assert!(postgres.connection_info().is_err());
    }
}
