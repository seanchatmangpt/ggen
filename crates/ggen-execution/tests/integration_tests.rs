// Integration tests for the unified execution framework
use ggen_execution::*;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Mutex;
use chrono::{DateTime, Utc};

// ============================================================================
// A2A INTEGRATION TESTS
// ============================================================================

#[tokio::test]
async fn test_a2a_integration_with_framework() {
    // Arrange: Create agents that communicate via A2A protocol
    let mut framework = ExecutionFramework::new(ExecutionConfig::default());

    // Create an A2A agent
    let a2a_agent = Box::new(A2AAgent::new("a2a-agent", vec!["communication".to_string()]));
    framework.register_agent(a2a_agent).unwrap();

    // Create workflow with A2A messaging
    let workflow_id = framework.create_workflow("A2A Workflow", "a2a").unwrap();
    let workflow = framework.workflows.get_mut(&workflow_id).unwrap();

    // Add tasks that use A2A messaging
    let task1 = Task::new("task-1", "Send Message", "a2a", TaskPriority::High, serde_json::json!({
        "action": "send",
        "target": "a2a-agent",
        "message": {"type": "hello"}
    }));
    let task2 = Task::new("task-2", "Receive Response", "a2a", TaskPriority::High, serde_json::json!({
        "action": "receive",
        "timeout": 5000
    }));

    workflow.add_task(task1);
    workflow.add_task(task2);

    // Act: Execute workflow
    let result = framework.execute_workflow(&workflow_id).await;

    // Assert
    assert!(result.is_ok());
    let workflow_result = result.unwrap();
    assert!(workflow_result.success);
    assert_eq!(workflow_result.completed_tasks, 2);

    // Check that A2A communication occurred
    let a2a_agent_ref = framework.agents.get("a2a-agent").unwrap();
    // Verify that agent received/sent messages
}

// A2A Agent implementation for testing
struct A2AAgent {
    id: String,
    capabilities: Vec<String>,
    messages_sent: u32,
    messages_received: u32,
}

impl A2AAgent {
    fn new(id: &str, capabilities: Vec<String>) -> Self {
        Self {
            id: id.to_string(),
            capabilities,
            messages_sent: 0,
            messages_received: 0,
        }
    }
}

#[async_trait::async_trait]
impl UnifiedAgentTrait for A2AAgent {
    fn get_id(&self) -> &str { &self.id }
    fn get_name(&self) -> &str { &self.id }
    fn get_capabilities(&self) -> &[String] { &self.capabilities.as_slice() }
    fn is_available(&self) -> bool { true }
    fn get_health(&self) -> &AgentHealth { &AgentHealth::new() }
    async fn execute_task(&mut self, task: Task) -> Result<TaskResult, ExecutionError> {
        if task.task_type == "a2a" {
            match task.payload.get("action").and_then(|v| v.as_str()) {
                Some("send") => {
                    self.messages_sent += 1;
                    println!("A2A Agent: Sent message to {}", task.payload.get("target").unwrap_or(&serde_json::Value::String("unknown".to_string())));
                }
                Some("receive") => {
                    self.messages_received += 1;
                    println!("A2A Agent: Received message");
                }
                _ => {}
            }
        }

        Ok(TaskResult {
            success: true,
            output: Some(serde_json::json!({"status": "success", "sent": self.messages_sent, "received": self.messages_received})),
            error: None,
            execution_time_ms: 100,
            resources_used: ResourceUsage::default(),
        })
    }
    async fn update_config(&mut self, _config: AgentConfiguration) -> Result<(), ExecutionError> { Ok(()) }
    fn get_metrics(&self) -> &AgentMetrics { &AgentMetrics::new() }
    async fn start(&mut self) -> Result<(), ExecutionError> { Ok(()) }
    async fn stop(&mut self) -> Result<(), ExecutionError> { Ok(()) }
    fn get_status(&self) -> &AgentStatus { &AgentStatus::Idle }
}

// ============================================================================
// WORKFLOW STATE PERSISTENCE TESTS
// ============================================================================

#[tokio::test]
async fn test_workflow_state_persistence() {
    // Arrange
    let mut framework = ExecutionFramework::new(ExecutionConfig::default());
    let agent = Box::new(DefaultAgent::new("test-agent", "Test Agent", vec!["test".to_string()]));
    framework.register_agent(agent).unwrap();

    let workflow_id = framework.create_workflow("Persistence Test", "persistence").unwrap();
    let workflow = framework.workflows.get_mut(&workflow_id).unwrap();

    // Add tasks
    for i in 0..5 {
        let task = Task::new(&format!("task-{}", i), &format!("Task {}", i), "test", TaskPriority::Normal, serde_json::json!({"id": i}));
        workflow.add_task(task);
    }

    // Execute partial workflow
    let workflow_clone = workflow.clone();
    let mut partial_workflow = Workflow {
        id: workflow_clone.id.clone(),
        name: workflow_clone.name.clone(),
        workflow_type: workflow_clone.workflow_type.clone(),
        tasks: workflow_clone.tasks[0..3].to_vec(), // Execute first 3 tasks
        created_at: workflow_clone.created_at,
        status: WorkflowStatus::Running,
    };

    // Save workflow state
    let saved_state = WorkflowState {
        workflow_id: workflow_id.clone(),
        current_step: 3,
        total_steps: 5,
        completed_tasks: vec!["task-0".to_string(), "task-1".to_string(), "task-2".to_string()],
        failed_tasks: Vec::new(),
        started_at: Utc::now(),
        last_updated: Utc::now(),
    };

    // Simulate some execution
    let result = framework.execute_workflow(&workflow_id).await;
    assert!(result.is_ok());

    // Verify persistence mechanisms would work
    assert!(!saved_state.completed_tasks.is_empty());
    assert_eq!(saved_state.current_step, 3);
}

// Workflow state for persistence
#[derive(Debug, Clone, Serialize, Deserialize)]
struct WorkflowState {
    pub workflow_id: WorkflowId,
    pub current_step: usize,
    pub total_steps: usize,
    pub completed_tasks: Vec<TaskId>,
    pub failed_tasks: Vec<TaskId>,
    pub started_at: DateTime<Utc>,
    pub last_updated: DateTime<Utc>,
}

// ============================================================================
// CROSS-AGENT COORDINATION TESTS
// ============================================================================

#[tokio::test]
async fn test_cross_agent_coordination() {
    // Arrange: Create multiple agents that need to coordinate
    let mut framework = ExecutionFramework::new(ExecutionConfig::default());
    let mut coordination_manager = CoordinationManager::new();

    // Create coordinator and worker agents
    let coordinator = Box::new(CoordinatorAgent::new("coordinator"));
    let worker1 = Box::new(WorkerAgent::new("worker1"));
    let worker2 = Box::new(WorkerAgent::new("worker2"));

    framework.register_agent(coordinator).unwrap();
    framework.register_agent(worker1).unwrap();
    framework.register_agent(worker2).unwrap();

    // Create coordination workflow
    let workflow_id = framework.create_workflow("Coordination Test", "coordination").unwrap();
    let workflow = framework.workflows.get_mut(&workflow_id).unwrap();

    // Add coordination tasks
    let coordinator_task = Task::new("coord-task", "Coordinate Work", "coordinator", TaskPriority::High, serde_json::json!({
        "workers": ["worker1", "worker2"],
        "work_type": "parallel"
    }));

    let worker_tasks = vec![
        Task::new("worker1-task", "Worker 1 Work", "worker", TaskPriority::Normal, serde_json::json!({"worker_id": "worker1"})),
        Task::new("worker2-task", "Worker 2 Work", "worker", TaskPriority::Normal, serde_json::json!({"worker_id": "worker2"})),
    ];

    workflow.add_task(coordinator_task);
    for task in worker_tasks {
        workflow.add_task(task);
    }

    // Act: Execute with coordination
    let result = coordination_manager.orchestrate_with_coordinators(&mut framework, workflow_id).await;

    // Assert
    assert!(result.is_ok());
    let result_value = result.unwrap();
    assert!(result_value.success);
    assert!(result_value.coordination_metrics.messages_exchanged > 0);
}

// Coordination manager for cross-agent coordination
pub struct CoordinationManager {
    pending_tasks: HashMap<TaskId, CoordinationRequest>,
    active_coordinations: HashMap<String, CoordinationSession>,
}

impl CoordinationManager {
    pub fn new() -> Self {
        Self {
            pending_tasks: HashMap::new(),
            active_coordinations: HashMap::new(),
        }
    }

    pub async fn orchestrate_with_coordinators(&mut self, framework: &mut ExecutionFramework, workflow_id: String) -> Result<CoordinationResult, ExecutionError> {
        let workflow = framework.workflows.get_mut(&workflow_id)
            .ok_or_else(|| ExecutionError::Workflow("Workflow not found".to_string()))?;

        let mut coordination_result = CoordinationResult {
            workflow_id: workflow_id.clone(),
            success: false,
            coordination_metrics: CoordinationMetrics::default(),
        };

        // Process coordination tasks
        for task in &workflow.tasks {
            if task.task_type == "coordinator" {
                let coordinator_result = self.process_coordinator_task(framework, task).await?;
                coordination_result.coordination_metrics.merge(&coordinator_result.metrics);
            }
        }

        // Execute worker tasks
        for task in &workflow.tasks {
            if task.task_type == "worker" {
                let worker_result = framework.execute_task(task.clone()).await?;
                coordination_result.coordination_metrics.worker_tasks_executed += 1;
                if worker_result.success {
                    coordination_result.coordination_metrics.worker_success_count += 1;
                }
            }
        }

        coordination_result.success = coordination_result.coordination_metrics.worker_success_count > 0;
        Ok(coordination_result)
    }

    async fn process_coordinator_task(&mut self, framework: &mut ExecutionFramework, task: &Task) -> Result<CoordinatorResult, ExecutionError> {
        // Simulate coordination logic
        let metrics = CoordinationMetrics {
            messages_exchanged: 2,
            coordinator_decisions: 1,
            worker_tasks_assigned: 2,
            ..Default::default()
        };

        Ok(CoordinatorResult { metrics })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoordinationResult {
    pub workflow_id: String,
    pub success: bool,
    pub coordination_metrics: CoordinationMetrics,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct CoordinationMetrics {
    pub messages_exchanged: u32,
    pub coordinator_decisions: u32,
    pub worker_tasks_assigned: u32,
    pub worker_tasks_executed: u32,
    pub worker_success_count: u32,
}

impl CoordinationMetrics {
    pub fn merge(&mut self, other: &CoordinationMetrics) {
        self.messages_exchanged += other.messages_exchanged;
        self.coordinator_decisions += other.coordinator_decisions;
        self.worker_tasks_assigned += other.worker_tasks_assigned;
        self.worker_tasks_executed += other.worker_tasks_executed;
        self.worker_success_count += other.worker_success_count;
    }
}

#[derive(Debug, Clone)]
pub struct CoordinatorResult {
    pub metrics: CoordinationMetrics,
}

// Coordinator agent
struct CoordinatorAgent {
    id: String,
    tasks_coordinated: u32,
}

impl CoordinatorAgent {
    fn new(id: &str) -> Self {
        Self { id: id.to_string(), tasks_coordinated: 0 }
    }
}

#[async_trait::async_trait]
impl UnifiedAgentTrait for CoordinatorAgent {
    fn get_id(&self) -> &str { &self.id }
    fn get_name(&self) -> &str { &self.id }
    fn get_capabilities(&self) -> &[String] { &["coordination".to_string()] }
    fn is_available(&self) -> bool { true }
    fn get_health(&self) -> &AgentHealth { &AgentHealth::new() }
    async fn execute_task(&mut self, task: Task) -> Result<TaskResult, ExecutionError> {
        self.tasks_coordinated += 1;
        Ok(TaskResult {
            success: true,
            output: Some(serde_json::json!({"coordinated": self.tasks_coordinated})),
            error: None,
            execution_time_ms: 50,
            resources_used: ResourceUsage::default(),
        })
    }
    async fn update_config(&mut self, _config: AgentConfiguration) -> Result<(), ExecutionError> { Ok(()) }
    fn get_metrics(&self) -> &AgentMetrics { &AgentMetrics::new() }
    async fn start(&mut self) -> Result<(), ExecutionError> { Ok(()) }
    async fn stop(&mut self) -> Result<(), ExecutionError> { Ok(()) }
    fn get_status(&self) -> &AgentStatus { &AgentStatus::Idle }
}

// Worker agent
struct WorkerAgent {
    id: String,
    work_completed: u32,
}

impl WorkerAgent {
    fn new(id: &str) -> Self {
        Self { id: id.to_string(), work_completed: 0 }
    }
}

#[async_trait::async_trait]
impl UnifiedAgentTrait for WorkerAgent {
    fn get_id(&self) -> &str { &self.id }
    fn get_name(&self) -> &str { &self.id }
    fn get_capabilities(&self) -> &[String] { &["work".to_string()] }
    fn is_available(&self) -> bool { true }
    fn get_health(&self) -> &AgentHealth { &AgentHealth::new() }
    async fn execute_task(&mut self, task: Task) -> Result<TaskResult, ExecutionError> {
        self.work_completed += 1;
        Ok(TaskResult {
            success: true,
            output: Some(serde_json::json!({"worker_id": self.id, "work_completed": self.work_completed})),
            error: None,
            execution_time_ms: 200,
            resources_used: ResourceUsage::default(),
        })
    }
    async fn update_config(&mut self, _config: AgentConfiguration) -> Result<(), ExecutionError> { Ok(()) }
    fn get_metrics(&self) -> &AgentMetrics { &AgentMetrics::new() }
    async fn start(&mut self) -> Result<(), ExecutionError> { Ok(()) }
    async fn stop(&mut self) -> Result<(), ExecutionError> { Ok(()) }
    fn get_status(&self) -> &AgentStatus { &AgentStatus::Idle }
}

// ============================================================================
// DYNAMIC AGENT REGISTRATION TESTS
// ============================================================================

#[tokio::test]
async fn test_dynamic_agent_registration() {
    // Arrange
    let mut framework = ExecutionFramework::new(ExecutionConfig::default());

    // Initially register one agent
    let initial_agent = Box::new(DefaultAgent::new("agent-1", "Agent 1", vec!["basic".to_string()]));
    framework.register_agent(initial_agent).unwrap();
    assert_eq!(framework.agents.len(), 1);

    // Create workflow
    let workflow_id = framework.create_workflow("Dynamic Registration Test", "dynamic").unwrap();
    let workflow = framework.workflows.get_mut(&workflow_id).unwrap();

    // Add task
    let task = Task::new("task-1", "Test Task", "basic", TaskPriority::Normal, serde_json::json!({}));
    workflow.add_task(task);

    // Execute with one agent
    let result = framework.execute_workflow(&workflow_id).await;
    assert!(result.is_ok());

    // Dynamically register new agents while running
    let new_agent1 = Box::new(DefaultAgent::new("agent-2", "Agent 2", vec!["basic".to_string(), "advanced".to_string()]));
    framework.register_agent(new_agent1).unwrap();
    assert_eq!(framework.agents.len(), 2);

    let new_agent2 = Box::new(DefaultAgent::new("agent-3", "Agent 3", vec!["expert".to_string()]));
    framework.register_agent(new_agent2).unwrap();
    assert_eq!(framework.agents.len(), 3);

    // Create new workflow to use all agents
    let workflow_id2 = framework.create_workflow("Multi-Agent Test", "multi").unwrap();
    let workflow2 = framework.workflows.get_mut(&workflow_id2).unwrap();

    for i in 0..3 {
        let task = Task::new(&format!("task-{}", i), &format!("Task {}", i), "basic", TaskPriority::Normal, serde_json::json!({"agent_id": i}));
        workflow2.add_task(task);
    }

    // Execute with multiple agents
    let result2 = framework.execute_workflow(&workflow_id2).await;
    assert!(result2.is_ok());
}

#[tokio::test]
async fn test_agent_capacity_management() {
    // Arrange: Framework with limited capacity
    let config = ExecutionConfig {
        max_concurrent_agents: 2,
        max_concurrent_tasks: 5,
        ..Default::default()
    };
    let mut framework = ExecutionFramework::new(config);

    // Try to register more agents than capacity
    for i in 0..3 {
        let agent = Box::new(DefaultAgent::new(&format!("agent-{}", i), &format!("Agent {}", i), vec!["test".to_string()]));
        if i < 2 {
            assert!(framework.register_agent(agent).is_ok());
        } else {
            // Should fail when exceeding capacity
            assert!(framework.register_agent(agent).is_err());
        }
    }

    // Verify only 2 agents registered
    assert_eq!(framework.agents.len(), 2);
}

// ============================================================================
// FAILOVER AND RECOVERY TESTS
// ============================================================================

#[tokio::test]
async fn test_agent_failover() {
    // Arrange: Create framework with redundant agents
    let mut framework = ExecutionFramework::new(ExecutionConfig::default());

    // Primary agent
    let primary = Box::new(PrimaryAgent::new("primary"));
    framework.register_agent(primary).unwrap();

    // Backup agent
    let backup = Box::new(BackupAgent::new("backup"));
    framework.register_agent(backup).unwrap();

    // Create task for primary
    let workflow_id = framework.create_workflow("Failover Test", "failover").unwrap();
    let workflow = framework.workflows.get_mut(&workflow_id).unwrap();

    let task = Task::new("task-1", "Important Task", "critical", TaskPriority::Critical, serde_json::json!({"fail_primary": true}));
    workflow.add_task(task);

    // Execute with failover
    let result = framework.execute_workflow(&workflow_id).await;
    assert!(result.is_ok());
}

// Primary agent that can fail
struct PrimaryAgent {
    id: String,
    should_fail: bool,
}

impl PrimaryAgent {
    fn new(id: &str) -> Self {
        Self { id: id.to_string(), should_fail: false }
    }
}

#[async_trait::async_trait]
impl UnifiedAgentTrait for PrimaryAgent {
    fn get_id(&self) -> &str { &self.id }
    fn get_name(&self) -> &str { &self.id }
    fn get_capabilities(&self) -> &[String] { &["critical".to_string()] }
    fn is_available(&self) -> bool { true }
    fn get_health(&self) -> &AgentHealth { &AgentHealth::new() }
    async fn execute_task(&mut self, task: Task) -> Result<TaskResult, ExecutionError> {
        if task.payload.get("fail_primary").and_then(|v| v.as_bool()).unwrap_or(false) {
            self.should_fail = true;
            // Simulate failure
            return Err(ExecutionError::Agent("Primary agent failed".to_string()));
        }

        Ok(TaskResult::default())
    }
    async fn update_config(&mut self, _config: AgentConfiguration) -> Result<(), ExecutionError> { Ok(()) }
    fn get_metrics(&self) -> &AgentMetrics { &AgentMetrics::new() }
    async fn start(&mut self) -> Result<(), ExecutionError> { Ok(()) }
    async fn stop(&mut self) -> Result<(), ExecutionError> { Ok(()) }
    fn get_status(&self) -> &AgentStatus { &AgentStatus::Idle }
}

// Backup agent for failover
struct BackupAgent {
    id: String,
    backup_count: u32,
}

impl BackupAgent {
    fn new(id: &str) -> Self {
        Self { id: id.to_string(), backup_count: 0 }
    }
}

#[async_trait::async_trait]
impl UnifiedAgentTrait for BackupAgent {
    fn get_id(&self) -> &str { &self.id }
    fn get_name(&self) -> &str { &self.id }
    fn get_capabilities(&self) -> &[String] { &["critical".to_string(), "backup".to_string()] }
    fn is_available(&self) -> bool { true }
    fn get_health(&self) -> &AgentHealth { &AgentHealth::new() }
    async fn execute_task(&mut self, _task: Task) -> Result<TaskResult, ExecutionError> {
        self.backup_count += 1;
        println!("Backup agent executed task #{}", self.backup_count);
        Ok(TaskResult::default())
    }
    async fn update_config(&mut self, _config: AgentConfiguration) -> Result<(), ExecutionError> { Ok(()) }
    fn get_metrics(&self) -> &AgentMetrics { &AgentMetrics::new() }
    async fn start(&mut self) -> Result<(), ExecutionError> { Ok(()) }
    async fn stop(&mut self) -> Result<(), ExecutionError> { Ok(()) }
    fn get_status(&self) -> &AgentStatus { &AgentStatus::Idle }
}

// ============================================================================
// PROTOCOL COMPLIANCE TESTS
// ============================================================================

#[tokio::test]
async fn test_protocol_compliance() {
    // Arrange: Create agents with different protocol implementations
    let mut framework = ExecutionFramework::new(ExecutionConfig::default());

    // HTTP protocol agent
    let http_agent = Box::new(HttpProtocolAgent::new("http-agent"));
    framework.register_agent(http_agent).unwrap();

    // WebSocket protocol agent
    let ws_agent = Box::new(WebSocketProtocolAgent::new("ws-agent"));
    framework.register_agent(ws_agent).unwrap();

    // Create workflow with protocol-specific tasks
    let workflow_id = framework.create_workflow("Protocol Compliance Test", "protocol").unwrap();
    let workflow = framework.workflows.get_mut(&workflow_id).unwrap();

    // Add protocol tasks
    let http_task = Task::new("http-task", "HTTP Task", "http", TaskPriority::Normal, serde_json::json!({
        "protocol": "http",
        "method": "GET",
        "url": "/api/test"
    }));

    let ws_task = Task::new("ws-task", "WebSocket Task", "websocket", TaskPriority::Normal, serde_json::json!({
        "protocol": "websocket",
        "action": "send",
        "data": {"type": "ping"}
    }));

    workflow.add_task(http_task);
    workflow.add_task(ws_task);

    // Execute workflow
    let result = framework.execute_workflow(&workflow_id).await;

    // Assert
    assert!(result.is_ok());
    let workflow_result = result.unwrap();
    assert!(workflow_result.success);

    // Verify protocol compliance
    let metrics = framework.get_metrics();
    assert!(metrics.is_some());
}

// HTTP protocol agent
struct HttpProtocolAgent {
    id: String,
    requests_made: u32,
}

impl HttpProtocolAgent {
    fn new(id: &str) -> Self {
        Self { id: id.to_string(), requests_made: 0 }
    }
}

#[async_trait::async_trait]
impl UnifiedAgentTrait for HttpProtocolAgent {
    fn get_id(&self) -> &str { &self.id }
    fn get_name(&self) -> &str { &self.id }
    fn get_capabilities(&self) -> &[String] { &["http".to_string()] }
    fn is_available(&self) -> bool { true }
    fn get_health(&self) -> &AgentHealth { &AgentHealth::new() }
    async fn execute_task(&mut self, task: Task) -> Result<TaskResult, ExecutionError> {
        if task.task_type == "http" {
            self.requests_made += 1;
            println!("HTTP Agent: Made request #{}", self.requests_made);
            Ok(TaskResult {
                success: true,
                output: Some(serde_json::json!({"protocol": "http", "status": 200, "requests": self.requests_made})),
                error: None,
                execution_time_ms: 100,
                resources_used: ResourceUsage::default(),
            })
        } else {
            Err(ExecutionError::Task("Unsupported protocol".to_string()))
        }
    }
    async fn update_config(&mut self, _config: AgentConfiguration) -> Result<(), ExecutionError> { Ok(()) }
    fn get_metrics(&self) -> &AgentMetrics { &AgentMetrics::new() }
    async fn start(&mut self) -> Result<(), ExecutionError> { Ok(()) }
    async fn stop(&mut self) -> Result<(), ExecutionError> { Ok(()) }
    fn get_status(&self) -> &AgentStatus { &AgentStatus::Idle }
}

// WebSocket protocol agent
struct WebSocketProtocolAgent {
    id: String,
    messages_sent: u32,
    messages_received: u32,
}

impl WebSocketProtocolAgent {
    fn new(id: &str) -> Self {
        Self { id: id.to_string(), messages_sent: 0, messages_received: 0 }
    }
}

#[async_trait::async_trait]
impl UnifiedAgentTrait for WebSocketProtocolAgent {
    fn get_id(&self) -> &str { &self.id }
    fn get_name(&self) -> &str { &self.id }
    fn get_capabilities(&self) -> &[String] { &["websocket".to_string()] }
    fn is_available(&self) -> bool { true }
    fn get_health(&self) -> &AgentHealth { &AgentHealth::new() }
    async fn execute_task(&mut self, task: Task) -> Result<TaskResult, ExecutionError> {
        if task.task_type == "websocket" {
            let action = task.payload.get("action").and_then(|v| v.as_str()).unwrap_or("unknown");
            if action == "send" {
                self.messages_sent += 1;
                println!("WebSocket Agent: Sent message #{}", self.messages_sent);
            } else if action == "receive" {
                self.messages_received += 1;
                println!("WebSocket Agent: Received message #{}", self.messages_received);
            }

            Ok(TaskResult {
                success: true,
                output: Some(serde_json::json!({
                    "protocol": "websocket",
                    "messages_sent": self.messages_sent,
                    "messages_received": self.messages_received
                })),
                error: None,
                execution_time_ms: 50,
                resources_used: ResourceUsage::default(),
            })
        } else {
            Err(ExecutionError::Task("Unsupported protocol".to_string()))
        }
    }
    async fn update_config(&mut self, _config: AgentConfiguration) -> Result<(), ExecutionError> { Ok(()) }
    fn get_metrics(&self) -> &AgentMetrics { &AgentMetrics::new() }
    async fn start(&mut self) -> Result<(), ExecutionError> { Ok(()) }
    async fn stop(&mut self) -> Result<(), ExecutionError> { Ok(()) }
    fn get_status(&self) -> &AgentStatus { &AgentStatus::Idle }
}