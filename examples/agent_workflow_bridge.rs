//! Agent Workflow Bridge Example
//!
//! This example demonstrates the complete integration between agents and workflows,
//! showing how agents can trigger workflows, monitor progress, and handle errors
//! through the event bridge system.
//!
//! ## What This Demonstrates
//!
//! - Agent-workflow bridge system with event channels
//! - Workflow registration and execution
//! - Agent-based vs system-based step execution
//! - Progress tracking via event bridge
//! - Status monitoring for workflow execution
//!
//! ## How to Run
//!
//! ```bash
//! cargo run --example agent_workflow_bridge
//! ```
//!
//! ## Expected Output
//!
//! The example will:
//! 1. Create an agent-workflow bridge
//! 2. Register a workflow with multiple steps
//! 3. Execute the workflow with agent context
//! 4. Display progress updates and final status
//! 5. Generate bridge code for the workflow

use std::collections::HashMap;
use std::time::Duration;
use tokio::sync::mpsc;

/// Agent workflow bridge system
pub struct AgentWorkflowBridge {
    workflow_registry: HashMap<String, WorkflowDefinition>,
    event_tx: mpsc::Sender<WorkflowEvent>,
    _event_rx: mpsc::Receiver<WorkflowEvent>,
}

/// Workflow definition structure
pub struct WorkflowDefinition {
    pub id: String,
    pub name: String,
    pub spec_path: String,
    pub steps: Vec<WorkflowStep>,
    pub timeout: Duration,
}

/// Individual workflow step
pub struct WorkflowStep {
    pub id: String,
    pub name: String,
    pub agent_required: bool,
    pub dependencies: Vec<String>,
}

/// Workflow events
#[derive(Debug, Clone)]
pub enum WorkflowEvent {
    Start {
        workflow_id: String,
        agent_id: String,
    },
    Complete {
        workflow_id: String,
    },
    Error {
        workflow_id: String,
        error: String,
    },
    Progress {
        workflow_id: String,
        step: String,
        percentage: u8,
    },
}

/// Workflow status
#[derive(Debug, Clone)]
pub struct WorkflowStatus {
    pub workflow_id: String,
    pub current_step: String,
    pub progress: u8,
    pub is_running: bool,
    pub has_errors: bool,
}

/// Workflow result
#[derive(Debug, Clone)]
pub struct WorkflowResult {
    pub workflow_id: String,
    pub output: String,
    pub artifacts: Vec<String>,
    pub duration_ms: u64,
}

impl AgentWorkflowBridge {
    /// Create a new agent workflow bridge
    pub fn new() -> Self {
        let (event_tx, event_rx) = mpsc::channel(100);

        Self {
            workflow_registry: HashMap::new(),
            event_tx,
            _event_rx: event_rx,
        }
    }

    /// Register a new workflow
    pub fn register_workflow(&mut self, workflow: WorkflowDefinition) {
        self.workflow_registry.insert(workflow.id.clone(), workflow);
        println!("Workflow registered: {}", self.workflow_registry.len());
    }

    /// Execute workflow with agent integration
    pub async fn execute_workflow(
        &self, workflow_id: &str, agent_context: AgentContext,
    ) -> Result<WorkflowResult, String> {
        let workflow = self
            .workflow_registry
            .get(workflow_id)
            .ok_or_else(|| format!("Workflow {} not found", workflow_id))?;

        // Notify workflow start
        let _ = self
            .event_tx
            .send(WorkflowEvent::Start {
                workflow_id: workflow_id.to_string(),
                agent_id: agent_context.id.clone(),
            })
            .await;

        // Initialize workflow execution
        let mut step_results = Vec::new();
        let start_time = std::time::Instant::now();

        // Execute workflow steps
        for (step_index, step) in workflow.steps.iter().enumerate() {
            // Check dependencies
            if !self.check_dependencies(step, &step_results).await {
                return Err(format!("Dependency check failed for step {}", step.id));
            }

            // Execute step
            if step.agent_required {
                self.execute_agent_step(step, &agent_context).await?;
            } else {
                self.execute_system_step(step).await?;
            }

            // Update progress
            let progress = ((step_index + 1) * 100 / workflow.steps.len()) as u8;
            let _ = self
                .event_tx
                .send(WorkflowEvent::Progress {
                    workflow_id: workflow_id.to_string(),
                    step: step.name.clone(),
                    percentage: progress,
                })
                .await;

            step_results.push(StepResult {
                step_id: step.id.clone(),
                artifact: format!("{}.result", step.id),
                output: format!("Step {} completed", step.name),
            });
        }

        // Complete workflow
        let result = WorkflowResult {
            workflow_id: workflow_id.to_string(),
            output: format!("Workflow {} completed", workflow_id),
            artifacts: step_results.iter().map(|r| r.artifact.clone()).collect(),
            duration_ms: start_time.elapsed().as_millis() as u64,
        };

        let _ = self
            .event_tx
            .send(WorkflowEvent::Complete {
                workflow_id: workflow_id.to_string(),
            })
            .await;

        Ok(result)
    }

    /// Check workflow dependencies
    async fn check_dependencies(&self, step: &WorkflowStep, step_results: &[StepResult]) -> bool {
        for dep_id in &step.dependencies {
            let dep_completed = step_results.iter().any(|r| r.step_id == *dep_id);
            if !dep_completed {
                return false;
            }
        }
        true
    }

    /// Execute agent-based step
    async fn execute_agent_step(
        &self, step: &WorkflowStep, context: &AgentContext,
    ) -> Result<StepResult, String> {
        println!(
            "Executing agent step: {} with agent: {}",
            step.name, context.id
        );

        // Simulate agent execution
        tokio::time::sleep(Duration::from_millis(1000)).await;

        Ok(StepResult {
            step_id: step.id.clone(),
            artifact: format!("{}.result", step.id),
            output: format!("Step {} completed by agent {}", step.name, context.id),
        })
    }

    /// Execute system-based step
    async fn execute_system_step(&self, step: &WorkflowStep) -> Result<StepResult, String> {
        println!("Executing system step: {}", step.name);

        // Simulate system execution
        tokio::time::sleep(Duration::from_millis(500)).await;

        Ok(StepResult {
            step_id: step.id.clone(),
            artifact: format!("{}.system_result", step.id),
            output: format!("Step {} completed by system", step.name),
        })
    }

    /// Get workflow status
    pub fn get_workflow_status(&self, workflow_id: &str) -> Option<WorkflowStatus> {
        Some(WorkflowStatus {
            workflow_id: workflow_id.to_string(),
            current_step: "idle".to_string(),
            progress: 0,
            is_running: false,
            has_errors: false,
        })
    }

    /// Generate agent bridge code (mock implementation)
    pub fn generate_bridge_code(&self, _workflow_id: &str) -> Result<String, String> {
        Ok("// Generated agent bridge code\nfn bridge() { println!(\"Bridged\"); }".to_string())
    }
}

/// Step result
pub struct StepResult {
    pub step_id: String,
    pub artifact: String,
    pub output: String,
}

/// Agent context
pub struct AgentContext {
    pub id: String,
    pub capabilities: Vec<String>,
    pub timeout: Duration,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("Agent Workflow Bridge Example");
    println!("================================");

    // Initialize bridge
    let mut bridge = AgentWorkflowBridge::new();

    // Define workflow steps
    let steps = vec![
        WorkflowStep {
            id: "data_collection".to_string(),
            name: "Data Collection".to_string(),
            agent_required: true,
            dependencies: vec![],
        },
        WorkflowStep {
            id: "data_validation".to_string(),
            name: "Data Validation".to_string(),
            agent_required: false,
            dependencies: vec!["data_collection".to_string()],
        },
        WorkflowStep {
            id: "model_training".to_string(),
            name: "Model Training".to_string(),
            agent_required: true,
            dependencies: vec!["data_validation".to_string()],
        },
    ];

    // Register workflow
    let workflow = WorkflowDefinition {
        id: "ml_pipeline".to_string(),
        name: "Machine Learning Pipeline".to_string(),
        spec_path: "/path/to/workflow/spec.ttl".to_string(),
        steps,
        timeout: Duration::from_secs(300),
    };

    bridge.register_workflow(workflow);

    // Define agent context
    let agent_context = AgentContext {
        id: "ml-agent".to_string(),
        capabilities: vec!["data_processing".to_string(), "ml_training".to_string()],
        timeout: Duration::from_secs(60),
    };

    // Execute workflow
    println!("\nExecuting workflow...");
    let result = bridge
        .execute_workflow("ml_pipeline", agent_context)
        .await?;

    println!("Workflow result: {}", result.output);
    println!("Generated artifacts: {:?}", result.artifacts);

    // Get status
    let status = bridge.get_workflow_status("ml_pipeline");
    if let Some(s) = status {
        println!("Status: {} - {}% complete", s.current_step, s.progress);
    }

    // Generate bridge code
    println!("\nGenerating bridge code...");
    let bridge_code = bridge.generate_bridge_code("ml_pipeline")?;
    println!(
        "Generated {} lines of bridge code",
        bridge_code.lines().count()
    );

    println!("\nExample completed successfully!");
    Ok(())
}
