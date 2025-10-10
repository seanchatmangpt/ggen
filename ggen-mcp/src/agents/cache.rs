//! Cache Manager Agent
//! 
//! Manages template and result caching with intelligent invalidation

use super::*;
use std::collections::HashMap;
use tokio::time::{Duration, Instant};
use serde_json::Value;

/// Cache Manager Agent
pub struct CacheManager {
    config: AgentConfig,
    status: AgentStatus,
    template_cache: HashMap<String, CachedTemplate>,
    result_cache: HashMap<String, CachedResult>,
    cache_metrics: CacheMetrics,
}

/// Cached template
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CachedTemplate {
    pub path: String,
    pub hash: String,
    pub last_modified: chrono::DateTime<chrono::Utc>,
    pub size_bytes: usize,
    pub access_count: u64,
    pub last_accessed: chrono::DateTime<chrono::Utc>,
}

/// Cached result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CachedResult {
    pub key: String,
    pub result: Value,
    pub created_at: chrono::DateTime<chrono::Utc>,
    pub expires_at: Option<chrono::DateTime<chrono::Utc>>,
    pub access_count: u64,
    pub last_accessed: chrono::DateTime<chrono::Utc>,
}

/// Cache metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CacheMetrics {
    pub template_hits: u64,
    pub template_misses: u64,
    pub result_hits: u64,
    pub result_misses: u64,
    pub total_size_bytes: usize,
    pub eviction_count: u64,
}

#[async_trait::async_trait]
impl Agent for CacheManager {
    async fn initialize(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        tracing::info!("Initializing Cache Manager");
        Ok(())
    }
    
    async fn start(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        tracing::info!("Starting Cache Manager");
        self.status = AgentStatus::Healthy;
        Ok(())
    }
    
    async fn stop(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        tracing::info!("Stopping Cache Manager");
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
                tracing::warn!("Cache Manager received unhandled message type");
                Ok(AgentMessage::ErrorNotification {
                    error: "Unhandled message type".to_string(),
                    severity: ErrorSeverity::Medium,
                })
            }
        }
    }
}

impl CacheManager {
    pub fn new(config: AgentConfig) -> Self {
        Self {
            config,
            status: AgentStatus::Healthy,
            template_cache: HashMap::new(),
            result_cache: HashMap::new(),
            cache_metrics: CacheMetrics {
                template_hits: 0,
                template_misses: 0,
                result_hits: 0,
                result_misses: 0,
                total_size_bytes: 0,
                eviction_count: 0,
            },
        }
    }
    
    async fn get_metrics(&self) -> Result<Value, Box<dyn std::error::Error + Send + Sync>> {
        Ok(serde_json::json!({
            "template_cache": {
                "size": self.template_cache.len(),
                "hits": self.cache_metrics.template_hits,
                "misses": self.cache_metrics.template_misses
            },
            "result_cache": {
                "size": self.result_cache.len(),
                "hits": self.cache_metrics.result_hits,
                "misses": self.cache_metrics.result_misses
            },
            "total_size_bytes": self.cache_metrics.total_size_bytes,
            "eviction_count": self.cache_metrics.eviction_count,
            "status": self.status
        }))
    }
}

