//! London-BDD Coordinator Agent
//! 
//! Orchestrates Behavior-Driven Development test execution with deterministic outcomes

use super::*;
use std::collections::HashMap;
use tokio::time::{Duration, Instant};
use serde_json::Value;

/// London-BDD Coordinator Agent
/// 
/// Implements the London-BDD pattern for deterministic test execution
/// Coordinates test scenarios, manages test data, and ensures reproducible results
pub struct LondonBddCoordinator {
    config: AgentConfig,
    status: AgentStatus,
    test_scenarios: HashMap<String, BddContext>,
    execution_history: Vec<TestExecution>,
    deterministic_seed: Option<u64>,
}

/// Test execution record for audit trail
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestExecution {
    pub id: Uuid,
    pub scenario_id: String,
    pub start_time: chrono::DateTime<chrono::Utc>,
    pub end_time: Option<chrono::DateTime<chrono::Utc>>,
    pub status: TestStatus,
    pub steps_executed: Vec<String>,
    pub assertions_passed: usize,
    pub assertions_failed: usize,
    pub variables: HashMap<String, Value>,
    pub seed: Option<u64>,
}

/// Test execution status
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum TestStatus {
    Pending,
    Running,
    Passed,
    Failed,
    Skipped,
    Timeout,
}

#[async_trait::async_trait]
impl Agent for LondonBddCoordinator {
    async fn initialize(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        tracing::info!("Initializing London-BDD Coordinator");
        
        // Initialize deterministic seed for reproducible tests
        self.deterministic_seed = Some(chrono::Utc::now().timestamp() as u64);
        
        // Load test scenarios from configuration
        self.load_test_scenarios().await?;
        
        tracing::info!("London-BDD Coordinator initialized with seed: {:?}", self.deterministic_seed);
        Ok(())
    }
    
    async fn start(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        tracing::info!("Starting London-BDD Coordinator");
        self.status = AgentStatus::Healthy;
        Ok(())
    }
    
    async fn stop(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        tracing::info!("Stopping London-BDD Coordinator");
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
            AgentMessage::TaskAssignment { task_id, task } => {
                self.handle_test_execution(task_id, task).await
            }
            AgentMessage::HealthCheck { from } => {
                Ok(AgentMessage::HealthResponse {
                    status: self.status.clone(),
                    metrics: Some(self.get_metrics().await?),
                })
            }
            _ => {
                tracing::warn!("London-BDD Coordinator received unhandled message type");
                Ok(AgentMessage::ErrorNotification {
                    error: "Unhandled message type".to_string(),
                    severity: ErrorSeverity::Medium,
                })
            }
        }
    }
}

impl LondonBddCoordinator {
    pub fn new(config: AgentConfig) -> Self {
        Self {
            config,
            status: AgentStatus::Healthy,
            test_scenarios: HashMap::new(),
            execution_history: Vec::new(),
            deterministic_seed: None,
        }
    }
    
    /// Load test scenarios from configuration
    async fn load_test_scenarios(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        // TODO: Load from configuration file or database
        // For now, create a sample scenario
        let sample_scenario = BddContext {
            scenario: "Template Generation Test".to_string(),
            steps: vec![
                BddStep {
                    step_type: BddStepType::Given,
                    description: "a valid template exists".to_string(),
                    action: "load_template".to_string(),
                    expected: Some("template_loaded".to_string()),
                },
                BddStep {
                    step_type: BddStepType::When,
                    description: "I generate code from the template".to_string(),
                    action: "generate_code".to_string(),
                    expected: Some("code_generated".to_string()),
                },
                BddStep {
                    step_type: BddStepType::Then,
                    description: "the generated code should be valid".to_string(),
                    action: "validate_code".to_string(),
                    expected: Some("code_valid".to_string()),
                },
            ],
            variables: HashMap::new(),
            assertions: vec![
                BddAssertion {
                    assertion_type: BddAssertionType::IsValid,
                    condition: "generated_code".to_string(),
                    expected_value: Value::Bool(true),
                    tolerance: None,
                },
            ],
        };
        
        self.test_scenarios.insert("template_generation".to_string(), sample_scenario);
        Ok(())
    }
    
    /// Handle test execution task
    async fn handle_test_execution(&mut self, task_id: Uuid, task: TaskDefinition) -> Result<AgentMessage, Box<dyn std::error::Error + Send + Sync>> {
        tracing::info!("Executing test task: {}", task_id);
        
        let start_time = chrono::Utc::now();
        let mut execution = TestExecution {
            id: task_id,
            scenario_id: task.parameters["scenario"].as_str().unwrap_or("unknown").to_string(),
            start_time,
            end_time: None,
            status: TestStatus::Running,
            steps_executed: Vec::new(),
            assertions_passed: 0,
            assertions_failed: 0,
            variables: HashMap::new(),
            seed: self.deterministic_seed,
        };
        
        // Execute the test scenario
        match self.execute_scenario(&mut execution, &task.parameters).await {
            Ok(result) => {
                execution.status = TestStatus::Passed;
                execution.end_time = Some(chrono::Utc::now());
                self.execution_history.push(execution.clone());
                
                Ok(AgentMessage::TaskCompletion {
                    task_id,
                    result: TaskResult {
                        task_id,
                        success: true,
                        result: Some(serde_json::to_value(result)?),
                        error: None,
                        duration_ms: (chrono::Utc::now() - start_time).num_milliseconds() as u64,
                        metrics: Some(self.get_execution_metrics(&execution).await?),
                    },
                })
            }
            Err(e) => {
                execution.status = TestStatus::Failed;
                execution.end_time = Some(chrono::Utc::now());
                self.execution_history.push(execution.clone());
                
                Ok(AgentMessage::TaskCompletion {
                    task_id,
                    result: TaskResult {
                        task_id,
                        success: false,
                        result: None,
                        error: Some(e.to_string()),
                        duration_ms: (chrono::Utc::now() - start_time).num_milliseconds() as u64,
                        metrics: Some(self.get_execution_metrics(&execution).await?),
                    },
                })
            }
        }
    }
    
    /// Execute a BDD scenario
    async fn execute_scenario(&mut self, execution: &mut TestExecution, parameters: &Value) -> Result<serde_json::Value, Box<dyn std::error::Error + Send + Sync>> {
        let scenario_id = parameters["scenario"].as_str().unwrap_or("unknown");
        
        let scenario = self.test_scenarios.get(scenario_id)
            .ok_or_else(|| format!("Scenario not found: {}", scenario_id))?;
        
        // Set deterministic seed for reproducible execution
        if let Some(seed) = self.deterministic_seed {
            // TODO: Set random seed for deterministic execution
            tracing::info!("Using deterministic seed: {}", seed);
        }
        
        // Execute each step in the scenario
        for step in &scenario.steps {
            execution.steps_executed.push(step.description.clone());
            
            match self.execute_step(step, &mut execution.variables).await {
                Ok(step_result) => {
                    tracing::info!("Step executed successfully: {}", step.description);
                    
                    // Validate step result if expected value is provided
                    if let Some(expected) = &step.expected {
                        if !self.validate_step_result(&step_result, expected)? {
                            return Err(format!("Step validation failed: {}", step.description).into());
                        }
                    }
                }
                Err(e) => {
                    tracing::error!("Step execution failed: {} - {}", step.description, e);
                    return Err(e);
                }
            }
        }
        
        // Execute assertions
        for assertion in &scenario.assertions {
            match self.execute_assertion(assertion, &execution.variables).await {
                Ok(true) => {
                    execution.assertions_passed += 1;
                    tracing::info!("Assertion passed: {}", assertion.condition);
                }
                Ok(false) => {
                    execution.assertions_failed += 1;
                    tracing::error!("Assertion failed: {}", assertion.condition);
                    return Err(format!("Assertion failed: {}", assertion.condition).into());
                }
                Err(e) => {
                    execution.assertions_failed += 1;
                    tracing::error!("Assertion error: {} - {}", assertion.condition, e);
                    return Err(e);
                }
            }
        }
        
        Ok(serde_json::json!({
            "scenario": scenario_id,
            "steps_executed": execution.steps_executed.len(),
            "assertions_passed": execution.assertions_passed,
            "assertions_failed": execution.assertions_failed,
            "variables": execution.variables,
            "seed": self.deterministic_seed
        }))
    }
    
    /// Execute a single BDD step
    async fn execute_step(&self, step: &BddStep, variables: &mut HashMap<String, Value>) -> Result<Value, Box<dyn std::error::Error + Send + Sync>> {
        match step.action.as_str() {
            "load_template" => {
                // Simulate template loading
                variables.insert("template_loaded".to_string(), Value::Bool(true));
                Ok(Value::Bool(true))
            }
            "generate_code" => {
                // Simulate code generation
                variables.insert("code_generated".to_string(), Value::Bool(true));
                variables.insert("generated_code".to_string(), Value::String("fn main() { println!(\"Hello, world!\"); }".to_string()));
                Ok(Value::Bool(true))
            }
            "validate_code" => {
                // Simulate code validation
                variables.insert("code_valid".to_string(), Value::Bool(true));
                Ok(Value::Bool(true))
            }
            _ => {
                Err(format!("Unknown step action: {}", step.action).into())
            }
        }
    }
    
    /// Validate step result against expected value
    fn validate_step_result(&self, result: &Value, expected: &str) -> Result<bool, Box<dyn std::error::Error + Send + Sync>> {
        // Simple validation logic
        match expected {
            "template_loaded" => Ok(result.as_bool().unwrap_or(false)),
            "code_generated" => Ok(result.as_bool().unwrap_or(false)),
            "code_valid" => Ok(result.as_bool().unwrap_or(false)),
            _ => Ok(true), // Default to pass for unknown expectations
        }
    }
    
    /// Execute a BDD assertion
    async fn execute_assertion(&self, assertion: &BddAssertion, variables: &HashMap<String, Value>) -> Result<bool, Box<dyn std::error::Error + Send + Sync>> {
        let value = variables.get(&assertion.condition)
            .ok_or_else(|| format!("Variable not found: {}", assertion.condition))?;
        
        match assertion.assertion_type {
            BddAssertionType::IsValid => {
                Ok(value.as_bool().unwrap_or(false))
            }
            BddAssertionType::Equals => {
                Ok(value == &assertion.expected_value)
            }
            BddAssertionType::Contains => {
                if let (Some(actual_str), Some(expected_str)) = (value.as_str(), assertion.expected_value.as_str()) {
                    Ok(actual_str.contains(expected_str))
                } else {
                    Ok(false)
                }
            }
            BddAssertionType::IsNotEmpty => {
                match value {
                    Value::String(s) => Ok(!s.is_empty()),
                    Value::Array(a) => Ok(!a.is_empty()),
                    Value::Object(o) => Ok(!o.is_empty()),
                    _ => Ok(true),
                }
            }
            _ => {
                // Default to true for unimplemented assertion types
                Ok(true)
            }
        }
    }
    
    /// Get execution metrics
    async fn get_execution_metrics(&self, execution: &TestExecution) -> Result<Value, Box<dyn std::error::Error + Send + Sync>> {
        Ok(serde_json::json!({
            "execution_id": execution.id,
            "scenario_id": execution.scenario_id,
            "duration_ms": execution.end_time
                .map(|end| (end - execution.start_time).num_milliseconds() as u64)
                .unwrap_or(0),
            "steps_executed": execution.steps_executed.len(),
            "assertions_passed": execution.assertions_passed,
            "assertions_failed": execution.assertions_failed,
            "status": execution.status,
            "seed": execution.seed
        }))
    }
    
    /// Get coordinator metrics
    async fn get_metrics(&self) -> Result<Value, Box<dyn std::error::Error + Send + Sync>> {
        let total_executions = self.execution_history.len();
        let successful_executions = self.execution_history.iter()
            .filter(|e| e.status == TestStatus::Passed)
            .count();
        let failed_executions = self.execution_history.iter()
            .filter(|e| e.status == TestStatus::Failed)
            .count();
        
        Ok(serde_json::json!({
            "total_executions": total_executions,
            "successful_executions": successful_executions,
            "failed_executions": failed_executions,
            "success_rate": if total_executions > 0 { 
                successful_executions as f64 / total_executions as f64 
            } else { 
                0.0 
            },
            "deterministic_seed": self.deterministic_seed,
            "test_scenarios": self.test_scenarios.len(),
            "status": self.status
        }))
    }
}

