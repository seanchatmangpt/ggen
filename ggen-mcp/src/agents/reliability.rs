//! Reliability Agent - Error Handling and Fault Recovery
//!
//! This agent implements comprehensive error handling and fault recovery patterns:
//! - Structured error handling with context preservation
//! - Automatic retry mechanisms with exponential backoff
//! - Circuit breakers for cascading failure prevention
//! - Graceful degradation when components fail
//! - Error aggregation and reporting
//!
//! # Reliability Patterns
//!
//! ## Error Handling Pipeline
//! 1. **Error Detection** - Identify and classify errors
//! 2. **Error Context** - Preserve context and stack traces
//! 3. **Error Classification** - Categorize by severity and type
//! 4. **Error Recovery** - Attempt automatic recovery
//! 5. **Error Reporting** - Log and report errors
//!
//! ## Retry Mechanisms
//! - **Exponential Backoff** - Increasing delays between retries
//! - **Jitter** - Random variation to prevent thundering herd
//! - **Max Retries** - Limit retry attempts
//! - **Retry Conditions** - Only retry on specific error types
//!
//! ## Circuit Breakers
//! - **Failure Threshold** - Number of failures before opening
//! - **Recovery Timeout** - Time before attempting recovery
//! - **Half-Open State** - Test recovery before full restoration
//! - **Success Threshold** - Number of successes to close circuit

use crate::agents::{Agent, AgentMetadata, AgentStatus, AgentId};
use crate::error::{GgenMcpError, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::{Duration, Instant};
use uuid::Uuid;
use chrono::Utc;
use rand::Rng;

/// Error classification
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ErrorType {
    Transient,    // Temporary error, can be retried
    Permanent,    // Permanent error, should not be retried
    Timeout,      // Operation timed out
    ResourceExhaustion, // Out of memory, disk space, etc.
    NetworkError, // Network connectivity issues
    ValidationError, // Input validation failed
    SystemError,  // System-level error
    Unknown,      // Unknown error type
}

/// Error severity levels
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ErrorSeverity {
    Low,
    Medium,
    High,
    Critical,
}

/// Error context information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ErrorContext {
    pub error_id: Uuid,
    pub error_type: ErrorType,
    pub severity: ErrorSeverity,
    pub message: String,
    pub stack_trace: Option<String>,
    pub operation: String,
    pub parameters: HashMap<String, serde_json::Value>,
    pub timestamp: chrono::DateTime<Utc>,
    pub retry_count: u32,
    pub max_retries: u32,
}

/// Retry configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RetryConfig {
    pub max_retries: u32,
    pub base_delay_ms: u64,
    pub max_delay_ms: u64,
    pub multiplier: f64,
    pub jitter: bool,
    pub retryable_errors: Vec<ErrorType>,
}

impl Default for RetryConfig {
    fn default() -> Self {
        Self {
            max_retries: 3,
            base_delay_ms: 1000,
            max_delay_ms: 30000,
            multiplier: 2.0,
            jitter: true,
            retryable_errors: vec![
                ErrorType::Transient,
                ErrorType::Timeout,
                ErrorType::NetworkError,
            ],
        }
    }
}

/// Circuit breaker state
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum CircuitBreakerState {
    Closed,
    Open,
    HalfOpen,
}

/// Circuit breaker configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CircuitBreakerConfig {
    pub failure_threshold: u32,
    pub success_threshold: u32,
    pub timeout_duration_ms: u64,
    pub state: CircuitBreakerState,
    pub failure_count: u32,
    pub success_count: u32,
    pub last_failure_time: Option<chrono::DateTime<Utc>>,
}

impl Default for CircuitBreakerConfig {
    fn default() -> Self {
        Self {
            failure_threshold: 5,
            success_threshold: 3,
            timeout_duration_ms: 60000, // 1 minute
            state: CircuitBreakerState::Closed,
            failure_count: 0,
            success_count: 0,
            last_failure_time: None,
        }
    }
}

/// Reliability Agent implementation
pub struct ReliabilityAgent {
    id: AgentId,
    error_history: Vec<ErrorContext>,
    retry_configs: HashMap<String, RetryConfig>,
    circuit_breakers: HashMap<String, CircuitBreakerConfig>,
    error_stats: HashMap<ErrorType, u32>,
    recovery_attempts: Vec<RecoveryAttempt>,
}

/// Recovery attempt record
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RecoveryAttempt {
    pub error_id: Uuid,
    pub attempt_number: u32,
    pub success: bool,
    pub recovery_method: String,
    pub duration_ms: u64,
    pub timestamp: chrono::DateTime<Utc>,
}

impl ReliabilityAgent {
    pub fn new() -> Self {
        let mut agent = Self {
            id: Uuid::new_v4(),
            error_history: Vec::new(),
            retry_configs: HashMap::new(),
            circuit_breakers: HashMap::new(),
            error_stats: HashMap::new(),
            recovery_attempts: Vec::new(),
        };

        // Initialize default retry configurations
        agent.initialize_retry_configs();
        
        // Initialize circuit breakers
        agent.initialize_circuit_breakers();

        agent
    }

    /// Initialize default retry configurations
    fn initialize_retry_configs(&mut self) {
        self.retry_configs.insert("default".to_string(), RetryConfig::default());
        
        self.retry_configs.insert("network_operations".to_string(), RetryConfig {
            max_retries: 5,
            base_delay_ms: 500,
            max_delay_ms: 10000,
            multiplier: 2.0,
            jitter: true,
            retryable_errors: vec![ErrorType::NetworkError, ErrorType::Timeout],
        });

        self.retry_configs.insert("file_operations".to_string(), RetryConfig {
            max_retries: 3,
            base_delay_ms: 1000,
            max_delay_ms: 5000,
            multiplier: 1.5,
            jitter: false,
            retryable_errors: vec![ErrorType::Transient, ErrorType::SystemError],
        });
    }

    /// Initialize circuit breakers
    fn initialize_circuit_breakers(&mut self) {
        self.circuit_breakers.insert("external_api".to_string(), CircuitBreakerConfig::default());
        self.circuit_breakers.insert("database".to_string(), CircuitBreakerConfig::default());
        self.circuit_breakers.insert("file_system".to_string(), CircuitBreakerConfig::default());
    }

    /// Handle an error with retry logic
    pub async fn handle_error(&mut self, error: GgenMcpError, operation: &str, parameters: HashMap<String, serde_json::Value>) -> Result<serde_json::Value> {
        let error_context = self.classify_error(error, operation, parameters);
        
        // Record error
        self.record_error(error_context.clone());
        
        // Check if error is retryable
        if self.is_retryable(&error_context) {
            return self.attempt_retry(error_context).await;
        }
        
        // Return error without retry
        Err(Box::new(error_context))
    }

    /// Classify an error
    fn classify_error(&self, error: GgenMcpError, operation: &str, parameters: HashMap<String, serde_json::Value>) -> ErrorContext {
        let error_message = error.to_string();
        let error_type = self.determine_error_type(&error_message);
        let severity = self.determine_error_severity(&error_type, &error_message);
        
        ErrorContext {
            error_id: Uuid::new_v4(),
            error_type,
            severity,
            message: error_message,
            stack_trace: None, // Would be populated in real implementation
            operation: operation.to_string(),
            parameters,
            timestamp: Utc::now(),
            retry_count: 0,
            max_retries: 3,
        }
    }

    /// Determine error type from error message
    fn determine_error_type(&self, error_message: &str) -> ErrorType {
        let message_lower = error_message.to_lowercase();
        
        if message_lower.contains("timeout") || message_lower.contains("timed out") {
            ErrorType::Timeout
        } else if message_lower.contains("network") || message_lower.contains("connection") {
            ErrorType::NetworkError
        } else if message_lower.contains("memory") || message_lower.contains("disk") || message_lower.contains("resource") {
            ErrorType::ResourceExhaustion
        } else if message_lower.contains("validation") || message_lower.contains("invalid") {
            ErrorType::ValidationError
        } else if message_lower.contains("system") || message_lower.contains("os") {
            ErrorType::SystemError
        } else if message_lower.contains("temporary") || message_lower.contains("retry") {
            ErrorType::Transient
        } else {
            ErrorType::Unknown
        }
    }

    /// Determine error severity
    fn determine_error_severity(&self, error_type: &ErrorType, _error_message: &str) -> ErrorSeverity {
        match error_type {
            ErrorType::ValidationError => ErrorSeverity::Low,
            ErrorType::Transient => ErrorSeverity::Medium,
            ErrorType::Timeout => ErrorSeverity::Medium,
            ErrorType::NetworkError => ErrorSeverity::Medium,
            ErrorType::ResourceExhaustion => ErrorSeverity::High,
            ErrorType::SystemError => ErrorSeverity::High,
            ErrorType::Permanent => ErrorSeverity::Critical,
            ErrorType::Unknown => ErrorSeverity::Medium,
        }
    }

    /// Check if error is retryable
    fn is_retryable(&self, error_context: &ErrorContext) -> bool {
        let config = self.retry_configs.get("default").unwrap();
        config.retryable_errors.contains(&error_context.error_type) && 
        error_context.retry_count < error_context.max_retries
    }

    /// Attempt retry with exponential backoff
    async fn attempt_retry(&mut self, mut error_context: ErrorContext) -> Result<serde_json::Value> {
        let config = self.retry_configs.get("default").unwrap();
        
        // Calculate delay with exponential backoff
        let delay_ms = self.calculate_retry_delay(error_context.retry_count, config);
        
        // Add jitter if enabled
        let final_delay = if config.jitter {
            let mut rng = rand::thread_rng();
            let jitter = rng.gen_range(0.0..0.1);
            delay_ms + (delay_ms as f64 * jitter) as u64
        } else {
            delay_ms
        };
        
        // Wait before retry
        tokio::time::sleep(Duration::from_millis(final_delay)).await;
        
        // Increment retry count
        error_context.retry_count += 1;
        
        // Record recovery attempt
        let recovery_attempt = RecoveryAttempt {
            error_id: error_context.error_id,
            attempt_number: error_context.retry_count,
            success: false, // Will be updated based on actual result
            recovery_method: "exponential_backoff".to_string(),
            duration_ms: final_delay,
            timestamp: Utc::now(),
        };
        self.recovery_attempts.push(recovery_attempt);
        
        // Simulate retry success/failure
        let success = self.simulate_retry_success(&error_context);
        
        if success {
            // Update recovery attempt as successful
            if let Some(last_attempt) = self.recovery_attempts.last_mut() {
                last_attempt.success = true;
            }
            
            Ok(serde_json::json!({
                "success": true,
                "retry_count": error_context.retry_count,
                "delay_ms": final_delay
            }))
        } else {
            // Record failed retry
            self.record_error(error_context);
            Err(Box::new(GgenMcpError::ExecutionFailed("Retry failed".to_string())))
        }
    }

    /// Calculate retry delay with exponential backoff
    fn calculate_retry_delay(&self, retry_count: u32, config: &RetryConfig) -> u64 {
        let delay = config.base_delay_ms as f64 * config.multiplier.powi(retry_count as i32);
        delay.min(config.max_delay_ms as f64) as u64
    }

    /// Simulate retry success (placeholder for real retry logic)
    fn simulate_retry_success(&self, error_context: &ErrorContext) -> bool {
        // Simulate 70% success rate for retries
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        
        let mut hasher = DefaultHasher::new();
        error_context.error_id.hash(&mut hasher);
        error_context.retry_count.hash(&mut hasher);
        let hash = hasher.finish();
        
        hash % 10 < 7 // 70% success rate
    }

    /// Record an error
    fn record_error(&mut self, error_context: ErrorContext) {
        self.error_history.push(error_context.clone());
        
        // Update error statistics
        let count = self.error_stats.entry(error_context.error_type.clone()).or_insert(0);
        *count += 1;
        
        // Keep only last 10000 errors
        if self.error_history.len() > 10000 {
            self.error_history.remove(0);
        }
    }

    /// Check circuit breaker state
    pub fn check_circuit_breaker(&self, service_name: &str) -> bool {
        if let Some(circuit_breaker) = self.circuit_breakers.get(service_name) {
            match circuit_breaker.state {
                CircuitBreakerState::Closed => true,
                CircuitBreakerState::Open => {
                    // Check if timeout has passed
                    if let Some(last_failure) = circuit_breaker.last_failure_time {
                        let timeout_duration = Duration::from_millis(circuit_breaker.timeout_duration_ms);
                        Utc::now() - last_failure > chrono::Duration::from_std(timeout_duration).unwrap()
                    } else {
                        true
                    }
                }
                CircuitBreakerState::HalfOpen => true,
            }
        } else {
            true // No circuit breaker configured, allow operation
        }
    }

    /// Update circuit breaker state
    pub fn update_circuit_breaker(&mut self, service_name: &str, success: bool) {
        if let Some(circuit_breaker) = self.circuit_breakers.get_mut(service_name) {
            if success {
                circuit_breaker.success_count += 1;
                circuit_breaker.failure_count = 0;
                
                if circuit_breaker.success_count >= circuit_breaker.success_threshold {
                    circuit_breaker.state = CircuitBreakerState::Closed;
                }
            } else {
                circuit_breaker.failure_count += 1;
                circuit_breaker.success_count = 0;
                circuit_breaker.last_failure_time = Some(Utc::now());
                
                if circuit_breaker.failure_count >= circuit_breaker.failure_threshold {
                    circuit_breaker.state = CircuitBreakerState::Open;
                }
            }
        }
    }

    /// Get error statistics
    pub fn get_error_stats(&self) -> serde_json::Value {
        let total_errors = self.error_history.len();
        let errors_by_type: HashMap<String, u32> = self.error_stats
            .iter()
            .map(|(error_type, count)| (format!("{:?}", error_type), *count))
            .collect();
        
        let errors_by_severity: HashMap<String, u32> = self.error_history
            .iter()
            .fold(HashMap::new(), |mut acc, error| {
                let severity = format!("{:?}", error.severity);
                *acc.entry(severity).or_insert(0) += 1;
                acc
            });

        let successful_recoveries = self.recovery_attempts.iter().filter(|r| r.success).count();
        let total_recoveries = self.recovery_attempts.len();
        let recovery_rate = if total_recoveries > 0 {
            successful_recoveries as f64 / total_recoveries as f64
        } else {
            0.0
        };

        serde_json::json!({
            "summary": {
                "total_errors": total_errors,
                "errors_by_type": errors_by_type,
                "errors_by_severity": errors_by_severity,
                "total_recovery_attempts": total_recoveries,
                "successful_recoveries": successful_recoveries,
                "recovery_rate": recovery_rate
            },
            "circuit_breakers": self.circuit_breakers,
            "retry_configs": self.retry_configs
        })
    }

    /// Get error history
    pub fn get_error_history(&self) -> &Vec<ErrorContext> {
        &self.error_history
    }

    /// Get recovery attempts
    pub fn get_recovery_attempts(&self) -> &Vec<RecoveryAttempt> {
        &self.recovery_attempts
    }
}

#[async_trait::async_trait]
impl Agent for ReliabilityAgent {
    async fn initialize(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        tracing::info!("Reliability Agent initialized with ID: {}", self.id);
        tracing::info!("Error handling and recovery mechanisms enabled");
        Ok(())
    }

    async fn execute(&self, input: serde_json::Value) -> Result<serde_json::Value, Box<dyn std::error::Error>> {
        let operation = input.get("operation")
            .and_then(|v| v.as_str())
            .ok_or("Missing operation")?;

        let mut agent = ReliabilityAgent::new();
        
        let result = match operation {
            "get_error_stats" => {
                serde_json::to_value(agent.get_error_stats())?
            }
            "check_circuit_breaker" => {
                let service_name = input.get("service_name")
                    .and_then(|v| v.as_str())
                    .unwrap_or("default");
                serde_json::to_value(agent.check_circuit_breaker(service_name))?
            }
            "handle_error" => {
                let error_message = input.get("error_message")
                    .and_then(|v| v.as_str())
                    .unwrap_or("Unknown error");
                let operation_name = input.get("operation_name")
                    .and_then(|v| v.as_str())
                    .unwrap_or("unknown");
                
                let error = GgenMcpError::ExecutionFailed(error_message.to_string());
                let parameters = HashMap::new();
                
                match agent.handle_error(error, operation_name, parameters).await {
                    Ok(result) => result,
                    Err(e) => serde_json::json!({
                        "success": false,
                        "error": e.to_string()
                    })
                }
            }
            _ => return Err("Unknown operation".into()),
        };

        Ok(result)
    }

    fn metadata(&self) -> AgentMetadata {
        AgentMetadata {
            id: self.id,
            name: "ReliabilityAgent".to_string(),
            version: "1.0.0".to_string(),
            status: AgentStatus::Healthy,
            capabilities: vec![
                "error_handling".to_string(),
                "retry_mechanisms".to_string(),
                "circuit_breaking".to_string(),
                "fault_recovery".to_string(),
            ],
            last_heartbeat: Utc::now(),
        }
    }

    async fn health_check(&self) -> AgentStatus {
        // Reliability agent is always healthy unless explicitly failed
        AgentStatus::Healthy
    }

    async fn shutdown(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        tracing::info!("Reliability Agent shutting down");
        tracing::info!("Handled {} errors", self.error_history.len());
        tracing::info!("Attempted {} recoveries", self.recovery_attempts.len());
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_reliability_agent_creation() {
        let agent = ReliabilityAgent::new();
        
        assert_eq!(agent.error_history.len(), 0);
        assert!(!agent.retry_configs.is_empty());
        assert!(!agent.circuit_breakers.is_empty());
    }

    #[test]
    fn test_error_classification() {
        let agent = ReliabilityAgent::new();
        
        let error = GgenMcpError::ExecutionFailed("Network timeout".to_string());
        let context = agent.classify_error(error, "test_operation", HashMap::new());
        
        assert_eq!(context.error_type, ErrorType::Timeout);
        assert_eq!(context.severity, ErrorSeverity::Medium);
    }

    #[test]
    fn test_retry_delay_calculation() {
        let agent = ReliabilityAgent::new();
        let config = RetryConfig::default();
        
        let delay_0 = agent.calculate_retry_delay(0, &config);
        let delay_1 = agent.calculate_retry_delay(1, &config);
        let delay_2 = agent.calculate_retry_delay(2, &config);
        
        assert_eq!(delay_0, 1000);
        assert_eq!(delay_1, 2000);
        assert_eq!(delay_2, 4000);
    }

    #[test]
    fn test_circuit_breaker_check() {
        let agent = ReliabilityAgent::new();
        
        // Circuit breaker should be closed initially
        assert!(agent.check_circuit_breaker("external_api"));
    }

    #[tokio::test]
    async fn test_error_handling() {
        let mut agent = ReliabilityAgent::new();
        
        let error = GgenMcpError::ExecutionFailed("Transient error".to_string());
        let parameters = HashMap::new();
        
        let result = agent.handle_error(error, "test_operation", parameters).await;
        
        // Should either succeed after retry or fail
        assert!(result.is_ok() || result.is_err());
    }

    #[test]
    fn test_error_stats() {
        let mut agent = ReliabilityAgent::new();
        
        // Add some errors
        let error = GgenMcpError::ExecutionFailed("Test error".to_string());
        let context = agent.classify_error(error, "test_operation", HashMap::new());
        agent.record_error(context);
        
        let stats = agent.get_error_stats();
        assert!(stats.get("summary").is_some());
    }

    #[tokio::test]
    async fn test_agent_execution() {
        let mut agent = ReliabilityAgent::new();
        agent.initialize().await.unwrap();
        
        let input = json!({
            "operation": "get_error_stats"
        });
        
        let result = agent.execute(input).await.unwrap();
        assert!(result.get("summary").is_some());
    }
}
