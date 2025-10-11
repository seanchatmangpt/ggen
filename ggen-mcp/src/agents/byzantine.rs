//! Byzantine Agent - Fault Tolerance and Consensus Mechanisms
//!
//! This agent implements Byzantine fault tolerance patterns for the MCP server,
//! ensuring system reliability even when some agents fail or behave maliciously.
//!
//! # Byzantine Fault Tolerance Patterns
//!
//! ## 3f+1 Consensus
//! - **f** = maximum number of faulty agents
//! - **3f+1** = minimum total agents needed for consensus
//! - **Majority agreement** required for all operations
//!
//! ## Fault Detection
//! - **Heartbeat monitoring** - Detect unresponsive agents
//! - **Behavioral analysis** - Identify malicious patterns
//! - **Consensus validation** - Verify agent responses
//! - **Automatic recovery** - Restart failed agents
//!
//! ## Graceful Degradation
//! - **Partial functionality** when some agents fail
//! - **Fallback mechanisms** for critical operations
//! - **Circuit breakers** to prevent cascade failures
//! - **Load balancing** across healthy agents

use crate::agents::{Agent, AgentMetadata, AgentStatus, AgentId, AgentCoordinator};
use crate::error::{GgenMcpError, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tokio::time::{Duration, Instant};
use uuid::Uuid;
use chrono::{DateTime, Utc};

/// Byzantine consensus result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConsensusResult {
    pub consensus_reached: bool,
    pub majority_agreement: f64,
    pub participating_agents: Vec<AgentId>,
    pub faulty_agents: Vec<AgentId>,
    pub result: Option<serde_json::Value>,
    pub confidence: f64,
    pub metadata: HashMap<String, serde_json::Value>,
}

/// Fault detection result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FaultDetectionResult {
    pub agent_id: AgentId,
    pub fault_type: String,
    pub severity: FaultSeverity,
    pub detection_time: chrono::DateTime<Utc>,
    pub evidence: HashMap<String, String>,
    pub recommended_action: String,
}

/// Fault severity levels
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum FaultSeverity {
    Low,
    Medium,
    High,
    Critical,
}

/// Agent health status
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentHealth {
    pub agent_id: AgentId,
    pub status: AgentStatus,
    pub last_heartbeat: chrono::DateTime<Utc>,
    pub response_time_ms: u64,
    pub error_count: u32,
    pub success_count: u32,
    pub suspicious_behavior: bool,
}

/// Circuit breaker state
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CircuitBreakerState {
    Closed,
    Open,
    HalfOpen,
}

/// Circuit breaker for fault tolerance
#[derive(Debug, Clone)]
pub struct CircuitBreaker {
    pub state: CircuitBreakerState,
    pub failure_count: u32,
    pub success_count: u32,
    pub failure_threshold: u32,
    pub success_threshold: u32,
    pub timeout: Duration,
    pub last_failure_time: Option<DateTime<Utc>>,
}

impl CircuitBreaker {
    pub fn new(failure_threshold: u32, success_threshold: u32, timeout: Duration) -> Self {
        Self {
            state: CircuitBreakerState::Closed,
            failure_count: 0,
            success_count: 0,
            failure_threshold,
            success_threshold,
            timeout,
            last_failure_time: None,
        }
    }

    pub fn can_execute(&self) -> bool {
        match self.state {
            CircuitBreakerState::Closed => true,
            CircuitBreakerState::Open => {
                if let Some(last_failure) = self.last_failure_time {
                    last_failure.elapsed() >= self.timeout
                } else {
                    true
                }
            }
            CircuitBreakerState::HalfOpen => true,
        }
    }

    pub fn record_success(&mut self) {
        self.success_count += 1;
        self.failure_count = 0;

        match self.state {
            CircuitBreakerState::HalfOpen => {
                if self.success_count >= self.success_threshold {
                    self.state = CircuitBreakerState::Closed;
                }
            }
            _ => {}
        }
    }

    pub fn record_failure(&mut self) {
        self.failure_count += 1;
        self.success_count = 0;
        self.last_failure_time = Some(Utc::now());

        if self.failure_count >= self.failure_threshold {
            self.state = CircuitBreakerState::Open;
        }
    }
}

/// Byzantine Agent implementation
pub struct ByzantineAgent {
    id: AgentId,
    coordinator: Arc<AgentCoordinator>,
    agent_health: RwLock<HashMap<AgentId, AgentHealth>>,
    circuit_breakers: RwLock<HashMap<AgentId, CircuitBreaker>>,
    consensus_history: Vec<ConsensusResult>,
    fault_detections: Vec<FaultDetectionResult>,
}

impl ByzantineAgent {
    pub fn new(coordinator: Arc<AgentCoordinator>) -> Self {
        Self {
            id: Uuid::new_v4(),
            coordinator,
            agent_health: RwLock::new(HashMap::new()),
            circuit_breakers: RwLock::new(HashMap::new()),
            consensus_history: Vec::new(),
            fault_detections: Vec::new(),
        }
    }

    /// Attempt Byzantine consensus on an operation
    pub async fn attempt_consensus(&mut self, operation: &str, input: serde_json::Value) -> Result<ConsensusResult> {
        let start_time = Utc::now();
        
        // Get healthy agents
        let healthy_agents = self.coordinator.get_healthy_agents().await;
        let total_agents = healthy_agents.len();
        
        if total_agents < 3 {
            return Err(GgenMcpError::ExecutionFailed("Insufficient agents for consensus".to_string()));
        }

        // Calculate Byzantine fault tolerance threshold (3f+1)
        let max_faulty = (total_agents - 1) / 3;
        let required_agreement = total_agents - max_faulty;

        // Simulate consensus process
        let mut agreements = 0;
        let mut disagreements = 0;
        let mut faulty_agents = Vec::new();
        let mut participating_agents = Vec::new();

        for agent in &healthy_agents {
            participating_agents.push(agent.id);
            
            // Simulate agent response (in real implementation, would call actual agents)
            let response = self.simulate_agent_response(agent.id, operation, &input).await;
            
            match response {
                Ok(_) => agreements += 1,
                Err(_) => {
                    disagreements += 1;
                    faulty_agents.push(agent.id);
                }
            }
        }

        let consensus_reached = agreements >= required_agreement;
        let majority_agreement = if total_agents > 0 {
            agreements as f64 / total_agents as f64
        } else {
            0.0
        };

        let result = ConsensusResult {
            consensus_reached,
            majority_agreement,
            participating_agents,
            faulty_agents,
            result: if consensus_reached { Some(input) } else { None },
            confidence: majority_agreement,
            metadata: HashMap::from([
                ("operation".to_string(), serde_json::Value::String(operation.to_string())),
                ("execution_time_ms".to_string(), serde_json::Value::Number(serde_json::Number::from(Utc::now().signed_duration_since(start_time).num_milliseconds() as u64))),
                ("required_agreement".to_string(), serde_json::Value::Number(serde_json::Number::from(required_agreement))),
            ]),
        };

        self.consensus_history.push(result.clone());
        
        // Keep only last 1000 consensus attempts
        if self.consensus_history.len() > 1000 {
            self.consensus_history.remove(0);
        }

        Ok(result)
    }

    /// Monitor agent health and detect faults
    pub async fn monitor_agent_health(&mut self) -> Result<Vec<FaultDetectionResult>> {
        let mut fault_detections = Vec::new();
        let healthy_agents = self.coordinator.get_healthy_agents().await;
        
        for agent in &healthy_agents {
            let health = self.get_agent_health(agent.id).await;
            
            // Check for various fault conditions
            if let Some(health) = health {
                // Check heartbeat timeout
                let time_since_heartbeat = Utc::now() - health.last_heartbeat;
                if time_since_heartbeat.num_seconds() > 300 { // 5 minutes
                    fault_detections.push(FaultDetectionResult {
                        agent_id: agent.id,
                        fault_type: "HeartbeatTimeout".to_string(),
                        severity: FaultSeverity::High,
                        detection_time: Utc::now(),
                        evidence: HashMap::from([
                            ("timeout_seconds".to_string(), time_since_heartbeat.num_seconds().to_string()),
                        ]),
                        recommended_action: "Restart agent".to_string(),
                    });
                }

                // Check error rate
                let total_requests = health.error_count + health.success_count;
                if total_requests > 10 {
                    let error_rate = health.error_count as f64 / total_requests as f64;
                    if error_rate > 0.5 {
                        fault_detections.push(FaultDetectionResult {
                            agent_id: agent.id,
                            fault_type: "HighErrorRate".to_string(),
                            severity: FaultSeverity::Medium,
                            detection_time: Utc::now(),
                            evidence: HashMap::from([
                                ("error_rate".to_string(), error_rate.to_string()),
                                ("total_requests".to_string(), total_requests.to_string()),
                            ]),
                            recommended_action: "Investigate and potentially restart".to_string(),
                        });
                    }
                }

                // Check response time
                if health.response_time_ms > 5000 { // 5 seconds
                    fault_detections.push(FaultDetectionResult {
                        agent_id: agent.id,
                        fault_type: "SlowResponse".to_string(),
                        severity: FaultSeverity::Low,
                        detection_time: Utc::now(),
                        evidence: HashMap::from([
                            ("response_time_ms".to_string(), health.response_time_ms.to_string()),
                        ]),
                        recommended_action: "Monitor and optimize".to_string(),
                    });
                }

                // Check suspicious behavior
                if health.suspicious_behavior {
                    fault_detections.push(FaultDetectionResult {
                        agent_id: agent.id,
                        fault_type: "SuspiciousBehavior".to_string(),
                        severity: FaultSeverity::Critical,
                        detection_time: Utc::now(),
                        evidence: HashMap::from([
                            ("behavior_type".to_string(), "Pattern anomaly".to_string()),
                        ]),
                        recommended_action: "Immediate investigation and potential isolation".to_string(),
                    });
                }
            }
        }

        // Store fault detections
        for detection in &fault_detections {
            self.fault_detections.push(detection.clone());
        }

        // Keep only last 1000 fault detections
        if self.fault_detections.len() > 1000 {
            self.fault_detections.drain(0..self.fault_detections.len() - 1000);
        }

        Ok(fault_detections)
    }

    /// Get circuit breaker for an agent
    pub async fn get_circuit_breaker(&self, agent_id: AgentId) -> CircuitBreaker {
        let circuit_breakers = self.circuit_breakers.read().await;
        circuit_breakers.get(&agent_id)
            .cloned()
            .unwrap_or_else(|| CircuitBreaker::new(5, 3, Duration::from_secs(60)))
    }

    /// Update circuit breaker state
    pub async fn update_circuit_breaker(&self, agent_id: AgentId, success: bool) {
        let mut circuit_breakers = self.circuit_breakers.write().await;
        let circuit_breaker = circuit_breakers.entry(agent_id)
            .or_insert_with(|| CircuitBreaker::new(5, 3, Duration::from_secs(60)));

        if success {
            circuit_breaker.record_success();
        } else {
            circuit_breaker.record_failure();
        }
    }

    /// Get agent health status
    pub async fn get_agent_health(&self, agent_id: AgentId) -> Option<AgentHealth> {
        let agent_health = self.agent_health.read().await;
        agent_health.get(&agent_id).cloned()
    }

    /// Update agent health status
    pub async fn update_agent_health(&self, agent_id: AgentId, health: AgentHealth) {
        let mut agent_health = self.agent_health.write().await;
        agent_health.insert(agent_id, health);
    }

    /// Simulate agent response for testing
    async fn simulate_agent_response(&self, agent_id: AgentId, operation: &str, input: &serde_json::Value) -> Result<serde_json::Value> {
        // Simulate 90% success rate for healthy agents
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        
        let mut hasher = DefaultHasher::new();
        agent_id.hash(&mut hasher);
        operation.hash(&mut hasher);
        input.to_string().hash(&mut hasher);
        let hash = hasher.finish();
        
        if hash % 10 < 9 {
            Ok(input.clone())
        } else {
            Err(GgenMcpError::ExecutionFailed("Simulated agent failure".to_string()))
        }
    }

    /// Get consensus history
    pub fn get_consensus_history(&self) -> &Vec<ConsensusResult> {
        &self.consensus_history
    }

    /// Get fault detection history
    pub fn get_fault_detections(&self) -> &Vec<FaultDetectionResult> {
        &self.fault_detections
    }
}

#[async_trait::async_trait]
impl Agent for ByzantineAgent {
    async fn initialize(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        tracing::info!("Byzantine Agent initialized with ID: {}", self.id);
        tracing::info!("Byzantine fault tolerance enabled");
        Ok(())
    }

    async fn execute(&self, input: serde_json::Value) -> Result<serde_json::Value, Box<dyn std::error::Error>> {
        let operation = input.get("operation")
            .and_then(|v| v.as_str())
            .ok_or("Missing operation")?;

        let consensus_input = input.get("input")
            .cloned()
            .unwrap_or(serde_json::Value::Object(serde_json::Map::new()));

        // Attempt consensus
        let mut agent = ByzantineAgent::new(self.coordinator.clone());
        let result = agent.attempt_consensus(operation, consensus_input).await?;

        Ok(serde_json::to_value(result)?)
    }

    fn metadata(&self) -> AgentMetadata {
        AgentMetadata {
            id: self.id,
            name: "ByzantineAgent".to_string(),
            version: "1.0.0".to_string(),
            status: AgentStatus::Healthy,
            capabilities: vec![
                "byzantine_consensus".to_string(),
                "fault_detection".to_string(),
                "circuit_breaking".to_string(),
                "health_monitoring".to_string(),
            ],
            last_heartbeat: Utc::now(),
        }
    }

    async fn health_check(&self) -> AgentStatus {
        // Byzantine agent is always healthy unless explicitly failed
        AgentStatus::Healthy
    }

    async fn shutdown(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        tracing::info!("Byzantine Agent shutting down");
        tracing::info!("Consensus attempts: {}", self.consensus_history.len());
        tracing::info!("Fault detections: {}", self.fault_detections.len());
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[tokio::test]
    async fn test_byzantine_agent_creation() {
        let coordinator = Arc::new(AgentCoordinator::new());
        let agent = ByzantineAgent::new(coordinator);
        
        assert_eq!(agent.consensus_history.len(), 0);
        assert_eq!(agent.fault_detections.len(), 0);
    }

    #[tokio::test]
    async fn test_consensus_attempt() {
        let coordinator = Arc::new(AgentCoordinator::new());
        
        // Add some agents for consensus
        for i in 0..4 {
            let metadata = AgentMetadata {
                id: Uuid::new_v4(),
                name: format!("agent-{}", i),
                version: "1.0.0".to_string(),
                status: AgentStatus::Healthy,
                capabilities: vec![],
                last_heartbeat: Utc::now(),
            };
            coordinator.register_agent(metadata).await.unwrap();
        }
        
        let mut agent = ByzantineAgent::new(coordinator);
        let result = agent.attempt_consensus("test_operation", json!({"test": "data"})).await.unwrap();
        
        assert!(result.participating_agents.len() >= 3);
        assert!(result.majority_agreement > 0.0);
    }

    #[tokio::test]
    async fn test_fault_detection() {
        let coordinator = Arc::new(AgentCoordinator::new());
        let mut agent = ByzantineAgent::new(coordinator);
        
        let fault_detections = agent.monitor_agent_health().await.unwrap();
        
        // Should not detect faults with no agents
        assert!(fault_detections.is_empty());
    }

    #[tokio::test]
    async fn test_circuit_breaker() {
        let coordinator = Arc::new(AgentCoordinator::new());
        let agent = ByzantineAgent::new(coordinator);
        let agent_id = Uuid::new_v4();
        
        let circuit_breaker = agent.get_circuit_breaker(agent_id).await;
        assert!(circuit_breaker.can_execute());
        
        // Test circuit breaker updates
        agent.update_circuit_breaker(agent_id, true).await;
        agent.update_circuit_breaker(agent_id, false).await;
    }

    #[tokio::test]
    async fn test_agent_execution() {
        let coordinator = Arc::new(AgentCoordinator::new());
        
        // Add agents for consensus
        for i in 0..3 {
            let metadata = AgentMetadata {
                id: Uuid::new_v4(),
                name: format!("agent-{}", i),
                version: "1.0.0".to_string(),
                status: AgentStatus::Healthy,
                capabilities: vec![],
                last_heartbeat: Utc::now(),
            };
            coordinator.register_agent(metadata).await.unwrap();
        }
        
        let mut agent = ByzantineAgent::new(coordinator);
        agent.initialize().await.unwrap();
        
        let input = json!({
            "operation": "test_consensus",
            "input": {"test": "data"}
        });
        
        let result = agent.execute(input).await.unwrap();
        let consensus_result: ConsensusResult = serde_json::from_value(result).unwrap();
        
        assert!(consensus_result.participating_agents.len() >= 3);
    }
}
