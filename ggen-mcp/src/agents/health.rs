//! Health Monitor Agent
//!
//! Monitors system health and coordinates health checks

use super::*;
use serde_json::Value;

/// Health Monitor Agent
pub struct HealthMonitor {
    config: AgentConfig,
    status: AgentStatus,
    health_checks: Vec<HealthCheck>,
}

/// Health check
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HealthCheck {
    pub id: String,
    pub name: String,
    pub check_type: HealthCheckType,
    pub interval_ms: u64,
    pub timeout_ms: u64,
    pub last_run: Option<chrono::DateTime<chrono::Utc>>,
    pub last_result: Option<HealthCheckResult>,
}

/// Health check types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum HealthCheckType {
    Ping,
    Database,
    Network,
    Disk,
    Memory,
    Custom,
}

/// Health check result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HealthCheckResult {
    pub success: bool,
    pub duration_ms: u64,
    pub message: String,
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

#[async_trait::async_trait]
impl Agent for HealthMonitor {
    async fn initialize(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        tracing::info!("Initializing Health Monitor");
        Ok(())
    }

    async fn start(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        tracing::info!("Starting Health Monitor");
        self.status = AgentStatus::Healthy;
        Ok(())
    }

    async fn stop(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        tracing::info!("Stopping Health Monitor");
        self.status = AgentStatus::Unhealthy;
        Ok(())
    }

    async fn status(&self) -> AgentStatus {
        self.status.clone()
    }

    fn config(&self) -> &AgentConfig {
        &self.config
    }

    async fn handle_message(
        &mut self, message: AgentMessage,
    ) -> Result<AgentMessage, Box<dyn std::error::Error + Send + Sync>> {
        match message {
            AgentMessage::HealthCheck { from } => Ok(AgentMessage::HealthResponse {
                status: self.status.clone(),
                metrics: Some(self.get_metrics().await?),
            }),
            _ => {
                tracing::warn!("Health Monitor received unhandled message type");
                Ok(AgentMessage::ErrorNotification {
                    error: "Unhandled message type".to_string(),
                    severity: ErrorSeverity::Medium,
                })
            }
        }
    }
}

impl HealthMonitor {
    pub fn new(config: AgentConfig) -> Self {
        Self {
            config,
            status: AgentStatus::Healthy,
            health_checks: Vec::new(),
        }
    }

    async fn get_metrics(&self) -> Result<Value, Box<dyn std::error::Error + Send + Sync>> {
        Ok(serde_json::json!({
            "health_checks": self.health_checks.len(),
            "status": self.status
        }))
    }
}
