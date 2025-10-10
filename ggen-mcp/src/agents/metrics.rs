//! Metrics Collector Agent
//! 
//! Collects and aggregates system metrics

use super::*;
use serde_json::Value;

/// Metrics Collector Agent
pub struct MetricsCollector {
    config: AgentConfig,
    status: AgentStatus,
    system_metrics: SystemMetrics,
}

/// System metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SystemMetrics {
    pub cpu_usage: f64,
    pub memory_usage: f64,
    pub disk_usage: f64,
    pub network_io: NetworkMetrics,
    pub application_metrics: ApplicationMetrics,
}

/// Network metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetworkMetrics {
    pub bytes_sent: u64,
    pub bytes_received: u64,
    pub connections: usize,
}

/// Application metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApplicationMetrics {
    pub requests_total: u64,
    pub requests_successful: u64,
    pub requests_failed: u64,
    pub average_response_time_ms: f64,
}

#[async_trait::async_trait]
impl Agent for MetricsCollector {
    async fn initialize(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        tracing::info!("Initializing Metrics Collector");
        Ok(())
    }
    
    async fn start(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        tracing::info!("Starting Metrics Collector");
        self.status = AgentStatus::Healthy;
        Ok(())
    }
    
    async fn stop(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        tracing::info!("Stopping Metrics Collector");
        self.status = AgentStatus::Unhealthy;
        Ok(())
    }
    
    async fn status(&self) -> AgentStatus {
        self.status.clone()
    }
    
    fn config(&self) -> &AgentConfig {
        &self.config
    }
    
    async fn handle_message(&mut self, message: AgentMessage) -> Result<AgentMessage, Box<dyn std::error::Error + Send + Sync>> {
        match message {
            AgentMessage::HealthCheck { from } => {
                Ok(AgentMessage::HealthResponse {
                    status: self.status.clone(),
                    metrics: Some(self.get_metrics().await?),
                })
            }
            _ => {
                tracing::warn!("Metrics Collector received unhandled message type");
                Ok(AgentMessage::ErrorNotification {
                    error: "Unhandled message type".to_string(),
                    severity: ErrorSeverity::Medium,
                })
            }
        }
    }
}

impl MetricsCollector {
    pub fn new(config: AgentConfig) -> Self {
        Self {
            config,
            status: AgentStatus::Healthy,
            system_metrics: SystemMetrics {
                cpu_usage: 0.0,
                memory_usage: 0.0,
                disk_usage: 0.0,
                network_io: NetworkMetrics {
                    bytes_sent: 0,
                    bytes_received: 0,
                    connections: 0,
                },
                application_metrics: ApplicationMetrics {
                    requests_total: 0,
                    requests_successful: 0,
                    requests_failed: 0,
                    average_response_time_ms: 0.0,
                },
            },
        }
    }
    
    async fn get_metrics(&self) -> Result<Value, Box<dyn std::error::Error + Send + Sync>> {
        Ok(serde_json::json!({
            "system": {
                "cpu_usage": self.system_metrics.cpu_usage,
                "memory_usage": self.system_metrics.memory_usage,
                "disk_usage": self.system_metrics.disk_usage
            },
            "network": {
                "bytes_sent": self.system_metrics.network_io.bytes_sent,
                "bytes_received": self.system_metrics.network_io.bytes_received,
                "connections": self.system_metrics.network_io.connections
            },
            "application": {
                "requests_total": self.system_metrics.application_metrics.requests_total,
                "requests_successful": self.system_metrics.application_metrics.requests_successful,
                "requests_failed": self.system_metrics.application_metrics.requests_failed,
                "average_response_time_ms": self.system_metrics.application_metrics.average_response_time_ms
            },
            "status": self.status
        }))
    }
}

