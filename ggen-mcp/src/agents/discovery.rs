//! Service Discovery Agent
//!
//! Discovers and registers services in the distributed system

use super::*;
use serde_json::Value;

/// Service Discovery Agent for finding and registering services in the system
pub struct ServiceDiscovery {
    config: AgentConfig,
    status: AgentStatus,
    registered_services: Vec<ServiceRecord>,
}

/// Record of a discovered service with its metadata and health status
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ServiceRecord {
    pub id: Uuid,
    pub name: String,
    pub service_type: ServiceType,
    pub endpoint: String,
    pub health_status: AgentStatus,
    pub registered_at: chrono::DateTime<chrono::Utc>,
    pub last_heartbeat: chrono::DateTime<chrono::Utc>,
}

/// Service types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ServiceType {
    Agent,
    Database,
    Cache,
    Queue,
    Storage,
    External,
}

#[async_trait::async_trait]
impl Agent for ServiceDiscovery {
    async fn initialize(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        tracing::info!("Initializing Service Discovery");
        Ok(())
    }

    async fn start(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        tracing::info!("Starting Service Discovery");
        self.status = AgentStatus::Healthy;
        Ok(())
    }

    async fn stop(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        tracing::info!("Stopping Service Discovery");
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
                tracing::warn!("Service Discovery received unhandled message type");
                Ok(AgentMessage::ErrorNotification {
                    error: "Unhandled message type".to_string(),
                    severity: ErrorSeverity::Medium,
                })
            }
        }
    }
}

impl ServiceDiscovery {
    pub fn new(config: AgentConfig) -> Self {
        Self {
            config,
            status: AgentStatus::Healthy,
            registered_services: Vec::new(),
        }
    }

    async fn get_metrics(&self) -> Result<Value, Box<dyn std::error::Error + Send + Sync>> {
        Ok(serde_json::json!({
            "registered_services": self.registered_services.len(),
            "status": self.status
        }))
    }
}
