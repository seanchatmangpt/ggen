//! Performance Agent - SLO Compliance and Optimization
//!
//! This agent monitors and optimizes MCP server performance to meet SLOs:
//! - First build ≤ 15s, Incremental ≤ 2s
//! - RDF processing ≤ 5s for 1k+ triples
//! - Generation memory ≤ 100MB
//! - CLI scaffolding ≤ 3s end-to-end
//! - 100% reproducible outputs
//!
//! # Performance Patterns
//!
//! ## SLO Monitoring
//! - **Latency tracking** - Measure operation response times
//! - **Throughput monitoring** - Track operations per second
//! - **Resource usage** - Monitor CPU, memory, disk I/O
//! - **Error rates** - Track failure percentages
//!
//! ## Optimization Strategies
//! - **Caching** - Cache expensive operations
//! - **Parallelization** - Execute operations concurrently
//! - **Resource pooling** - Reuse expensive resources
//! - **Lazy loading** - Load resources on demand
//!
//! ## Performance Budgets
//! - **Memory budget** - Limit memory usage per operation
//! - **Time budget** - Set maximum execution times
//! - **CPU budget** - Limit CPU usage per operation
//! - **I/O budget** - Limit disk and network I/O

use crate::agents::{Agent, AgentMetadata, AgentStatus, AgentId};
use crate::error::{GgenMcpError, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::{Duration, Instant};
use uuid::Uuid;
use chrono::{DateTime, Utc};

/// Performance metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceMetrics {
    pub operation_name: String,
    pub duration_ms: u64,
    pub memory_usage_mb: f64,
    pub cpu_usage_percent: f64,
    pub disk_io_bytes: u64,
    pub network_io_bytes: u64,
    pub cache_hits: u32,
    pub cache_misses: u32,
    pub error_count: u32,
    pub success_count: u32,
    pub timestamp: chrono::DateTime<Utc>,
}

/// SLO compliance result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SloComplianceResult {
    pub slo_name: String,
    pub target_value: f64,
    pub actual_value: f64,
    pub compliance_percentage: f64,
    pub is_compliant: bool,
    pub violation_severity: ViolationSeverity,
    pub recommendations: Vec<String>,
}

/// Violation severity levels
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ViolationSeverity {
    None,
    Low,
    Medium,
    High,
    Critical,
}

/// Performance budget
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceBudget {
    pub max_duration_ms: u64,
    pub max_memory_mb: f64,
    pub max_cpu_percent: f64,
    pub max_disk_io_mb: f64,
    pub max_network_io_mb: f64,
}

/// Cache entry for performance optimization
#[derive(Debug, Clone)]
pub struct CacheEntry {
    pub key: String,
    pub value: serde_json::Value,
    pub created_at: DateTime<Utc>,
    pub access_count: u32,
    pub last_accessed: DateTime<Utc>,
}

/// Performance Agent implementation
pub struct PerformanceAgent {
    id: AgentId,
    metrics_history: Vec<PerformanceMetrics>,
    slo_targets: HashMap<String, f64>,
    performance_budgets: HashMap<String, PerformanceBudget>,
    cache: HashMap<String, CacheEntry>,
    cache_max_size: usize,
    cache_ttl: Duration,
}

impl PerformanceAgent {
    pub fn new() -> Self {
        let mut agent = Self {
            id: Uuid::new_v4(),
            metrics_history: Vec::new(),
            slo_targets: HashMap::new(),
            performance_budgets: HashMap::new(),
            cache: HashMap::new(),
            cache_max_size: 1000,
            cache_ttl: Duration::from_secs(300), // 5 minutes
        };

        // Initialize SLO targets
        agent.initialize_slo_targets();
        
        // Initialize performance budgets
        agent.initialize_performance_budgets();

        agent
    }

    /// Initialize SLO targets
    fn initialize_slo_targets(&mut self) {
        self.slo_targets.insert("first_build_duration_ms".to_string(), 15000.0);
        self.slo_targets.insert("incremental_build_duration_ms".to_string(), 2000.0);
        self.slo_targets.insert("rdf_processing_duration_ms".to_string(), 5000.0);
        self.slo_targets.insert("generation_memory_mb".to_string(), 100.0);
        self.slo_targets.insert("cli_scaffolding_duration_ms".to_string(), 3000.0);
        self.slo_targets.insert("error_rate_percent".to_string(), 0.0);
        self.slo_targets.insert("cache_hit_rate_percent".to_string(), 80.0);
    }

    /// Initialize performance budgets
    fn initialize_performance_budgets(&mut self) {
        self.performance_budgets.insert("project_gen".to_string(), PerformanceBudget {
            max_duration_ms: 30000,
            max_memory_mb: 200.0,
            max_cpu_percent: 80.0,
            max_disk_io_mb: 50.0,
            max_network_io_mb: 10.0,
        });

        self.performance_budgets.insert("graph_query".to_string(), PerformanceBudget {
            max_duration_ms: 10000,
            max_memory_mb: 150.0,
            max_cpu_percent: 70.0,
            max_disk_io_mb: 20.0,
            max_network_io_mb: 5.0,
        });

        self.performance_budgets.insert("market_search".to_string(), PerformanceBudget {
            max_duration_ms: 5000,
            max_memory_mb: 50.0,
            max_cpu_percent: 60.0,
            max_disk_io_mb: 10.0,
            max_network_io_mb: 20.0,
        });
    }

    /// Record performance metrics
    pub fn record_metrics(&mut self, metrics: PerformanceMetrics) {
        self.metrics_history.push(metrics.clone());
        
        // Keep only last 10000 metrics
        if self.metrics_history.len() > 10000 {
            self.metrics_history.remove(0);
        }

        // Update cache statistics
        if let Some(cache_entry) = self.cache.get_mut(&metrics.operation_name) {
            cache_entry.access_count += 1;
            cache_entry.last_accessed = Utc::now();
        }
    }

    /// Check SLO compliance
    pub fn check_slo_compliance(&self, operation_name: &str) -> Vec<SloComplianceResult> {
        let mut results = Vec::new();
        
        // Get recent metrics for this operation
        let recent_metrics: Vec<&PerformanceMetrics> = self.metrics_history
            .iter()
            .filter(|m| m.operation_name == operation_name)
            .rev()
            .take(100)
            .collect();

        if recent_metrics.is_empty() {
            return results;
        }

        // Check duration SLO
        if let Some(target_duration) = self.slo_targets.get("first_build_duration_ms") {
            let avg_duration = recent_metrics.iter().map(|m| m.duration_ms as f64).sum::<f64>() / recent_metrics.len() as f64;
            let compliance = (target_duration / avg_duration * 100.0).min(100.0);
            
            results.push(SloComplianceResult {
                slo_name: "Duration".to_string(),
                target_value: *target_duration,
                actual_value: avg_duration,
                compliance_percentage: compliance,
                is_compliant: avg_duration <= *target_duration,
                violation_severity: self.get_violation_severity(compliance),
                recommendations: if avg_duration > *target_duration {
                    vec!["Optimize operation".to_string(), "Add caching".to_string()]
                } else {
                    vec![]
                },
            });
        }

        // Check memory SLO
        if let Some(target_memory) = self.slo_targets.get("generation_memory_mb") {
            let avg_memory = recent_metrics.iter().map(|m| m.memory_usage_mb).sum::<f64>() / recent_metrics.len() as f64;
            let compliance = (target_memory / avg_memory * 100.0).min(100.0);
            
            results.push(SloComplianceResult {
                slo_name: "Memory Usage".to_string(),
                target_value: *target_memory,
                actual_value: avg_memory,
                compliance_percentage: compliance,
                is_compliant: avg_memory <= *target_memory,
                violation_severity: self.get_violation_severity(compliance),
                recommendations: if avg_memory > *target_memory {
                    vec!["Reduce memory allocation".to_string(), "Use streaming".to_string()]
                } else {
                    vec![]
                },
            });
        }

        // Check error rate SLO
        let total_operations = recent_metrics.iter().map(|m| m.error_count + m.success_count).sum::<u32>();
        let total_errors = recent_metrics.iter().map(|m| m.error_count).sum::<u32>();
        let error_rate = if total_operations > 0 {
            (total_errors as f64 / total_operations as f64) * 100.0
        } else {
            0.0
        };

        results.push(SloComplianceResult {
            slo_name: "Error Rate".to_string(),
            target_value: 0.0,
            actual_value: error_rate,
            compliance_percentage: if error_rate == 0.0 { 100.0 } else { 0.0 },
            is_compliant: error_rate == 0.0,
            violation_severity: if error_rate == 0.0 { ViolationSeverity::None } else { ViolationSeverity::High },
            recommendations: if error_rate > 0.0 {
                vec!["Investigate errors".to_string(), "Improve error handling".to_string()]
            } else {
                vec![]
            },
        });

        results
    }

    /// Get violation severity based on compliance percentage
    fn get_violation_severity(&self, compliance_percentage: f64) -> ViolationSeverity {
        match compliance_percentage {
            p if p >= 95.0 => ViolationSeverity::None,
            p if p >= 90.0 => ViolationSeverity::Low,
            p if p >= 80.0 => ViolationSeverity::Medium,
            p if p >= 70.0 => ViolationSeverity::High,
            _ => ViolationSeverity::Critical,
        }
    }

    /// Check performance budget compliance
    pub fn check_performance_budget(&self, operation_name: &str) -> Option<PerformanceBudget> {
        self.performance_budgets.get(operation_name).cloned()
    }

    /// Cache a value for performance optimization
    pub fn cache_value(&mut self, key: String, value: serde_json::Value) {
        // Clean up expired entries
        self.cleanup_expired_cache();

        // Remove oldest entries if cache is full
        if self.cache.len() >= self.cache_max_size {
            let oldest_key = self.cache
                .iter()
                .min_by_key(|(_, entry)| entry.last_accessed)
                .map(|(key, _)| key.clone());

            if let Some(key) = oldest_key {
                self.cache.remove(&key);
            }
        }

        let entry = CacheEntry {
            key: key.clone(),
            value,
            created_at: Utc::now(),
            access_count: 0,
            last_accessed: Utc::now(),
        };

        self.cache.insert(key, entry);
    }

    /// Get cached value
    pub fn get_cached_value(&mut self, key: &str) -> Option<serde_json::Value> {
        if let Some(entry) = self.cache.get_mut(key) {
            // Check if entry is expired
            if entry.created_at.elapsed() > self.cache_ttl {
                self.cache.remove(key);
                return None;
            }

            // Update access statistics
            entry.access_count += 1;
            entry.last_accessed = Utc::now();

            Some(entry.value.clone())
        } else {
            None
        }
    }

    /// Clean up expired cache entries
    fn cleanup_expired_cache(&mut self) {
        let now = Utc::now();
        self.cache.retain(|_, entry| now.duration_since(entry.created_at) <= self.cache_ttl);
    }

    /// Get performance statistics
    pub fn get_performance_stats(&self) -> serde_json::Value {
        let total_operations = self.metrics_history.len();
        let successful_operations = self.metrics_history.iter().filter(|m| m.error_count == 0).count();
        let failed_operations = total_operations - successful_operations;

        let avg_duration = if total_operations > 0 {
            self.metrics_history.iter().map(|m| m.duration_ms).sum::<u64>() as f64 / total_operations as f64
        } else {
            0.0
        };

        let avg_memory = if total_operations > 0 {
            self.metrics_history.iter().map(|m| m.memory_usage_mb).sum::<f64>() / total_operations as f64
        } else {
            0.0
        };

        let cache_hit_rate = if self.cache.is_empty() {
            0.0
        } else {
            let total_hits = self.cache.values().map(|entry| entry.access_count).sum::<u32>();
            let total_requests = total_hits + self.cache.len() as u32;
            if total_requests > 0 {
                (total_hits as f64 / total_requests as f64) * 100.0
            } else {
                0.0
            }
        };

        serde_json::json!({
            "summary": {
                "total_operations": total_operations,
                "successful_operations": successful_operations,
                "failed_operations": failed_operations,
                "success_rate": if total_operations > 0 { successful_operations as f64 / total_operations as f64 } else { 0.0 },
                "average_duration_ms": avg_duration,
                "average_memory_mb": avg_memory,
                "cache_size": self.cache.len(),
                "cache_hit_rate_percent": cache_hit_rate
            },
            "slo_targets": self.slo_targets,
            "performance_budgets": self.performance_budgets
        })
    }

    /// Get metrics history
    pub fn get_metrics_history(&self) -> &Vec<PerformanceMetrics> {
        &self.metrics_history
    }
}

#[async_trait::async_trait]
impl Agent for PerformanceAgent {
    async fn initialize(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        tracing::info!("Performance Agent initialized with ID: {}", self.id);
        tracing::info!("SLO monitoring enabled for {} targets", self.slo_targets.len());
        Ok(())
    }

    async fn execute(&self, input: serde_json::Value) -> Result<serde_json::Value, Box<dyn std::error::Error>> {
        let operation = input.get("operation")
            .and_then(|v| v.as_str())
            .ok_or("Missing operation")?;

        let mut agent = PerformanceAgent::new();
        
        let result = match operation {
            "check_slo_compliance" => {
                let operation_name = input.get("operation_name")
                    .and_then(|v| v.as_str())
                    .unwrap_or("unknown");
                serde_json::to_value(agent.check_slo_compliance(operation_name))?
            }
            "get_performance_stats" => {
                serde_json::to_value(agent.get_performance_stats())?
            }
            "check_performance_budget" => {
                let operation_name = input.get("operation_name")
                    .and_then(|v| v.as_str())
                    .unwrap_or("unknown");
                serde_json::to_value(agent.check_performance_budget(operation_name))?
            }
            _ => return Err("Unknown operation".into()),
        };

        Ok(result)
    }

    fn metadata(&self) -> AgentMetadata {
        AgentMetadata {
            id: self.id,
            name: "PerformanceAgent".to_string(),
            version: "1.0.0".to_string(),
            status: AgentStatus::Healthy,
            capabilities: vec![
                "slo_monitoring".to_string(),
                "performance_optimization".to_string(),
                "caching".to_string(),
                "metrics_collection".to_string(),
            ],
            last_heartbeat: Utc::now(),
        }
    }

    async fn health_check(&self) -> AgentStatus {
        // Performance agent is always healthy unless explicitly failed
        AgentStatus::Healthy
    }

    async fn shutdown(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        tracing::info!("Performance Agent shutting down");
        tracing::info!("Collected {} metrics", self.metrics_history.len());
        tracing::info!("Cache size: {}", self.cache.len());
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_performance_agent_creation() {
        let agent = PerformanceAgent::new();
        
        assert_eq!(agent.metrics_history.len(), 0);
        assert!(!agent.slo_targets.is_empty());
        assert!(!agent.performance_budgets.is_empty());
    }

    #[test]
    fn test_metrics_recording() {
        let mut agent = PerformanceAgent::new();
        
        let metrics = PerformanceMetrics {
            operation_name: "test_operation".to_string(),
            duration_ms: 1000,
            memory_usage_mb: 50.0,
            cpu_usage_percent: 25.0,
            disk_io_bytes: 1024,
            network_io_bytes: 512,
            cache_hits: 5,
            cache_misses: 2,
            error_count: 0,
            success_count: 1,
            timestamp: Utc::now(),
        };
        
        agent.record_metrics(metrics);
        assert_eq!(agent.metrics_history.len(), 1);
    }

    #[test]
    fn test_slo_compliance_check() {
        let mut agent = PerformanceAgent::new();
        
        // Add some metrics
        for i in 0..10 {
            let metrics = PerformanceMetrics {
                operation_name: "project_gen".to_string(),
                duration_ms: 2000 + i * 100,
                memory_usage_mb: 80.0 + i as f64,
                cpu_usage_percent: 30.0 + i as f64,
                disk_io_bytes: 1024 + i * 100,
                network_io_bytes: 512 + i * 50,
                cache_hits: i,
                cache_misses: 0,
                error_count: 0,
                success_count: 1,
                timestamp: Utc::now(),
            };
            agent.record_metrics(metrics);
        }
        
        let compliance_results = agent.check_slo_compliance("project_gen");
        assert!(!compliance_results.is_empty());
    }

    #[test]
    fn test_caching() {
        let mut agent = PerformanceAgent::new();
        
        // Cache a value
        agent.cache_value("test_key".to_string(), json!("test_value"));
        assert_eq!(agent.cache.len(), 1);
        
        // Retrieve cached value
        let cached_value = agent.get_cached_value("test_key");
        assert_eq!(cached_value, Some(json!("test_value")));
        
        // Retrieve non-existent value
        let non_existent = agent.get_cached_value("non_existent");
        assert_eq!(non_existent, None);
    }

    #[test]
    fn test_performance_stats() {
        let mut agent = PerformanceAgent::new();
        
        // Add some metrics
        let metrics = PerformanceMetrics {
            operation_name: "test_operation".to_string(),
            duration_ms: 1000,
            memory_usage_mb: 50.0,
            cpu_usage_percent: 25.0,
            disk_io_bytes: 1024,
            network_io_bytes: 512,
            cache_hits: 5,
            cache_misses: 2,
            error_count: 0,
            success_count: 1,
            timestamp: Utc::now(),
        };
        agent.record_metrics(metrics);
        
        let stats = agent.get_performance_stats();
        assert!(stats.get("summary").is_some());
        assert!(stats.get("slo_targets").is_some());
    }

    #[tokio::test]
    async fn test_agent_execution() {
        let mut agent = PerformanceAgent::new();
        agent.initialize().await.unwrap();
        
        let input = json!({
            "operation": "get_performance_stats"
        });
        
        let result = agent.execute(input).await.unwrap();
        assert!(result.get("summary").is_some());
    }
}
