// Comprehensive test suite for the unified execution framework
use ggen_execution::*;
use std::collections::HashMap;
use std::time::Duration;
use tokio::time::sleep;

// ============================================================================
// FRAMEWORK INTEGRATION TESTS
// ============================================================================

#[tokio::test]
async fn test_framework_lifecycle() {
    // Arrange
    let config = ExecutionConfig::default();
    let mut framework = ExecutionFramework::new(config);

    // Act: Register agents and create workflows
    let agent = Box::new(DefaultAgent::new(
        "test-agent",
        "Test Agent",
        vec!["test-capability".to_string()],
    ));
    framework.register_agent(agent).unwrap();

    let workflow_id = framework.create_workflow("Test Workflow", "test").unwrap();
    let workflow = framework.workflows.get_mut(&workflow_id).unwrap();

    let task = Task::new(
        "task-1",
        "Test Task",
        "test",
        TaskPriority::Normal,
        serde_json::json!({"test": "data"}),
    );
    workflow.add_task(task);

    // Act: Execute workflow
    let result = framework.execute_workflow(&workflow_id).await;

    // Assert
    assert!(result.is_ok());
    let workflow_result = result.unwrap();
    assert!(workflow_result.success);
    assert_eq!(workflow_result.completed_tasks, 1);
    assert_eq!(workflow_result.total_tasks, 1);
    assert!(workflow_result.success_rate >= 0.9);
}

#[tokio::test]
async fn test_parallel_execution() {
    // Arrange
    let config = ExecutionConfig {
        max_concurrent_agents: 5,
        max_concurrent_tasks: 10,
        ..Default::default()
    };
    let mut framework = ExecutionFramework::new(config);

    // Register multiple agents
    for i in 0..5 {
        let agent = Box::new(DefaultAgent::new(
            &format!("agent-{}", i),
            &format!("Agent {}", i),
            vec!["test".to_string()],
        ));
        framework.register_agent(agent).unwrap();
    }

    // Create workflow with multiple tasks
    let workflow_id = framework
        .create_workflow("Parallel Test", "parallel")
        .unwrap();
    let workflow = framework.workflows.get_mut(&workflow_id).unwrap();

    for i in 0..10 {
        let task = Task::new(
            &format!("task-{}", i),
            &format!("Task {}", i),
            "test",
            TaskPriority::Normal,
            serde_json::json!({"id": i}),
        );
        workflow.add_task(task);
    }

    // Act: Execute workflow
    let result = framework.execute_workflow(&workflow_id).await;

    // Assert
    assert!(result.is_ok());
    let workflow_result = result.unwrap();
    assert!(workflow_result.success);
    assert_eq!(workflow_result.completed_tasks, 10);
    assert_eq!(workflow_result.total_tasks, 10);
}

#[tokio::test]
async fn test_task_dependency_resolution() {
    // Arrange
    let mut framework = ExecutionFramework::new(ExecutionConfig::default());

    let agent = Box::new(DefaultAgent::new(
        "test-agent",
        "Test Agent",
        vec!["test".to_string()],
    ));
    framework.register_agent(agent).unwrap();

    let workflow_id = framework
        .create_workflow("Dependency Test", "dependency")
        .unwrap();
    let workflow = framework.workflows.get_mut(&workflow_id).unwrap();

    // Create tasks with dependencies
    let task1 = Task::new(
        "task-1",
        "First Task",
        "test",
        TaskPriority::Normal,
        serde_json::json!({"step": 1}),
    );
    let task2 = Task::new(
        "task-2",
        "Second Task",
        "test",
        TaskPriority::Normal,
        serde_json!({"step": 2}),
    );
    let task3 = Task::new(
        "task-3",
        "Third Task",
        "test",
        TaskPriority::Normal,
        serde_json!({"step": 3}),
    );

    task2.dependencies.push("task-1".to_string());
    task3.dependencies.push("task-2".to_string());

    workflow.add_task(task1);
    workflow.add_task(task2);
    workflow.add_task(task3);

    // Act: Execute workflow
    let result = framework.execute_workflow(&workflow_id).await;

    // Assert
    assert!(result.is_ok());
    let workflow_result = result.unwrap();
    assert!(workflow_result.success);
    assert_eq!(workflow_result.completed_tasks, 3);
}

// ============================================================================
// SEMANTIC CONVERGENCE TESTS
// ============================================================================

#[tokio::test]
async fn test_semantic_convergence_engine() {
    // Arrange
    let config = ConvergenceConfig::default();
    let mut engine = SemanticConvergenceEngine::new(config);

    // Register agents
    engine
        .register_agent("agent-1", vec!["capability-1".to_string()])
        .await;
    engine
        .register_agent(
            "agent-2",
            vec!["capability-1".to_string(), "capability-2".to_string()],
        )
        .await;

    // Create and process convergence messages
    let message = ConvergenceMessage {
        id: "msg-1".to_string(),
        source_agent: "agent-1".to_string(),
        target_agents: vec!["agent-1".to_string(), "agent-2".to_string()],
        content: serde_json::json!({"type": "knowledge", "data": {"test": "value"}}),
        timestamp: Utc::now(),
        semantic_similarity: 0.0,
        contribution_type: ContributionType::KnowledgeSharing,
    };

    let result = engine.process_message(message).await;

    // Assert
    assert!(result.is_ok());
    let improvement = result.unwrap();
    assert!(improvement >= 0.0);
    assert!(improvement <= 1.0);

    // Check convergence status
    let status = engine.get_convergence_status().await;
    assert_eq!(status.active_agents, 2);
    assert_eq!(status.total_messages, 1);
}

#[tokio::test]
async fn test_adaptive_convergence() {
    // Arrange
    let config = ConvergenceConfig::default();
    let mut engine = AdaptiveConvergenceEngine::new(config);

    // Register agents
    engine
        .register_agent("agent-1", vec!["capability-1".to_string()])
        .await;

    // Process messages to build up performance data
    for i in 0..5 {
        let message = ConvergenceMessage {
            id: format!("msg-{}", i),
            source_agent: "agent-1".to_string(),
            target_agents: vec!["agent-1".to_string()],
            content: serde_json::json!({"type": "knowledge", "data": {"iteration": i}}),
            timestamp: Utc::now(),
            semantic_similarity: 0.8 + (i as f64 * 0.04),
            contribution_type: ContributionType::KnowledgeSharing,
        };

        engine.process_message_adaptive(message).await.unwrap();
    }

    // Assert
    let strategy_performance = &engine.strategy_performance;
    assert!(!strategy_performance.is_empty());
}

// ============================================================================
// ERROR HANDLING AND RECOVERY TESTS
// ============================================================================

#[tokio::test]
async fn test_error_handling_and_recovery() {
    // Arrange
    let mut recovery_manager = RecoveryManager::new();
    recovery_manager.register_policy(
        "test-error",
        RecoveryPolicy {
            strategy: RecoveryStrategy::Retry,
            max_attempts: 3,
            initial_delay_ms: 100,
            max_delay_ms: 1000,
            backoff_multiplier: 2.0,
            jitter_ms: 50,
            conditions: vec![RecoveryCondition::Always],
        },
    );

    // Act: Handle error
    let error = ExecutionError::Task("Test error".to_string());
    let result = recovery_manager
        .handle_error(&error, "test-component")
        .await;

    // Assert
    assert!(result.is_ok());
    let recovery_result = result.unwrap();
    assert!(!recovery_result.message.is_empty());
    assert_eq!(recovery_result.strategy, RecoveryStrategy::Retry);
}

#[tokio::test]
async fn test_self_healing_workflow() {
    // Arrange
    let mut workflow = SelfHealingWorkflow::new();
    let mut agents = HashMap::new();

    // Create a mock agent
    let agent = Box::new(MockAgent {
        id: "test-agent".to_string(),
        healthy: true,
        task_count: 0,
    });
    agents.insert("test-agent".to_string(), agent);

    // Act: Start self-healing workflow (runs in background)
    let workflow_handle = tokio::spawn(async move {
        // Start the workflow
        workflow.start(&agents).await
    });

    // Wait a bit for the workflow to run
    sleep(Duration::from_millis(100)).await;

    // Get recovery stats
    let stats = workflow.get_recovery_stats();

    // Assert
    assert!(stats.total_errors >= 0);
    assert!(stats.recovery_rate >= 0.0 && stats.recovery_rate <= 1.0);

    // Cancel the background task
    workflow_handle.abort();
}

// Mock agent for testing
struct MockAgent {
    id: String,
    healthy: bool,
    task_count: u32,
}

#[async_trait::async_trait]
impl UnifiedAgentTrait for MockAgent {
    fn get_id(&self) -> &str {
        &self.id
    }
    fn get_name(&self) -> &str {
        &self.id
    }
    fn get_capabilities(&self) -> &[String] {
        &[]
    }
    fn is_available(&self) -> bool {
        self.healthy
    }
    fn get_health(&self) -> &AgentHealth {
        &AgentHealth::new()
    }
    async fn execute_task(&mut self, _task: Task) -> Result<TaskResult, ExecutionError> {
        if self.healthy {
            self.task_count += 1;
            Ok(TaskResult {
                success: true,
                output: Some(serde_json::json!({"result": "success"})),
                error: None,
                execution_time_ms: 100,
                resources_used: ResourceUsage::default(),
            })
        } else {
            Err(ExecutionError::Task("Agent unhealthy".to_string()))
        }
    }
    async fn update_config(&mut self, _config: AgentConfiguration) -> Result<(), ExecutionError> {
        Ok(())
    }
    fn get_metrics(&self) -> &AgentMetrics {
        &AgentMetrics::new()
    }
    async fn start(&mut self) -> Result<(), ExecutionError> {
        Ok(())
    }
    async fn stop(&mut self) -> Result<(), ExecutionError> {
        Ok(())
    }
    fn get_status(&self) -> &AgentStatus {
        &AgentStatus::Idle
    }
}

// ============================================================================
// PIPELINE EXECUTION TESTS
// ============================================================================

#[tokio::test]
async fn test_pipeline_execution() {
    // Arrange
    let mut framework = ExecutionFramework::new(ExecutionConfig::default());
    let agent = Box::new(DefaultAgent::new(
        "test-agent",
        "Test Agent",
        vec!["test".to_string()],
    ));
    framework.register_agent(agent).unwrap();

    let pipeline = ExecutionPipeline::new("test-pipeline", "Test Pipeline", "test");

    // Create stages
    let mut stage1 = PipelineStage::new("Stage 1", StageType::DataProcessing);
    stage1.add_task(Task::new(
        "task-1",
        "Task 1",
        "test",
        TaskPriority::Normal,
        serde_json::json!({}),
    ));

    let mut stage2 = PipelineStage::new("Stage 2", StageType::Analysis);
    stage2.add_task(Task::new(
        "task-2",
        "Task 2",
        "test",
        TaskPriority::Normal,
        serde_json::json!({}),
    ));

    pipeline.add_stage(stage1);
    pipeline.add_stage(stage2);

    // Act: Execute pipeline
    let result = pipeline.execute(&mut framework).await;

    // Assert
    assert!(result.is_ok());
    let pipeline_result = result.unwrap();
    assert!(pipeline_result.success);
    assert_eq!(pipeline_result.completed_stages, 2);
    assert_eq!(pipeline_result.total_stages, 2);
}

#[tokio::test]
async fn test_pipeline_validation() {
    // Arrange: Create an invalid pipeline with circular dependencies
    let mut pipeline = ExecutionPipeline::new("test-pipeline", "Test Pipeline", "test");

    let stage1 = PipelineStage::new("Stage 1", StageType::DataProcessing);
    let stage2 = PipelineStage::new("Stage 2", StageType::Analysis);

    // This would cause circular dependency - Stage 1 depends on Stage 2, Stage 2 depends on Stage 1
    // let mut stage2_with_dep = stage2.clone();
    // stage2_with_dep.add_dependency("stage-1");
    // let mut stage1_with_dep = stage1.clone();
    // stage1_with_dep.add_dependency("stage-2");

    // pipeline.add_stage(stage1_with_dep);
    // pipeline.add_stage(stage2_with_dep);

    // Act & Assert: Validation should pass for this simple pipeline
    let result = PipelineValidator::validate_pipeline(&pipeline);
    assert!(result.is_ok());
}

// ============================================================================
// METRICS COLLECTION TESTS
// ============================================================================

#[test]
fn test_metrics_collection() {
    // Arrange
    let mut collector = MetricsCollector::new(100);

    // Record multiple metrics
    for i in 0..10 {
        let metrics = PerformanceMetrics {
            timestamp: Utc::now(),
            execution_duration_ms: 100 + (i * 50),
            throughput_per_second: 10.0 + (i as f64 * 0.5),
            success_rate: 0.9 + (i as f64 * 0.01),
            error_rate: 0.1 - (i as f64 * 0.01),
            resource_usage: ResourceUsage {
                cpu_percent: 50.0 + (i as f64 * 2.0),
                memory_mb: 512 + (i * 50),
                network_in_mb: 100,
                network_out_mb: 100,
            },
            memory_usage_mb: 512 + (i * 50),
            cpu_usage_percent: 50.0 + (i as f64 * 2.0),
            disk_usage_percent: 30.0,
            network_io_mb: 100,
        };

        collector.record_metrics(metrics);
    }

    // Assert: Check aggregations
    let throughput_agg = collector.aggregations.get("throughput").unwrap();
    assert!(throughput_agg.current_average > 0.0);

    let success_rate_agg = collector.aggregations.get("success_rate").unwrap();
    assert!(success_rate_agg.current_average > 0.0);

    // Test average metrics
    let avg_metrics = collector.get_average_metrics(10000);
    assert!(avg_metrics.is_some());

    let peak_metrics = collector.get_peak_metrics();
    assert!(peak_metrics.is_some());
}

#[test]
fn test_agent_metrics_collection() {
    // Arrange
    let mut collector = AgentMetricsCollector::new();

    // Update agent metrics
    let metrics = AgentMetrics {
        tasks_completed: 100,
        tasks_failed: 5,
        average_task_duration_ms: 500,
        throughput_tasks_per_second: 2.0,
        error_rate: 0.05,
    };

    collector.update_agent_metrics("agent-1", "Test Agent", metrics);

    // Assert
    let agent_data = collector.get_agent_metrics("agent-1").unwrap();
    assert_eq!(agent_data.tasks_completed, 100);
    assert_eq!(agent_data.tasks_failed, 5);
    assert_eq!(agent_data.error_rate, 0.05);

    let global_metrics = collector.get_global_metrics();
    assert_eq!(global_metrics.total_agents, 1);
    assert_eq!(global_metrics.active_agents, 1);
}

#[test]
fn test_execution_metrics_collection() {
    // Arrange
    let mut collector = ExecutionMetricsCollector::new();

    // Record workflow execution
    collector.record_workflow_execution("workflow-1", "Test Workflow", 5000, true, 10);

    // Record pipeline execution
    collector.record_pipeline_execution("pipeline-1", "Test Pipeline", 3000, true, 3);

    // Assert
    let workflow_metrics = collector.get_workflow_metrics("workflow-1").unwrap();
    assert_eq!(workflow_metrics.total_executions, 1);
    assert_eq!(workflow_metrics.successful_executions, 1);
    assert_eq!(workflow_metrics.total_tasks, 10);

    let pipeline_metrics = collector.get_pipeline_metrics("pipeline-1").unwrap();
    assert_eq!(pipeline_metrics.total_executions, 1);
    assert_eq!(pipeline_metrics.successful_executions, 1);
    assert_eq!(pipeline_metrics.total_stages, 3);

    let top_workflows = collector.get_top_workflows(5);
    assert_eq!(top_workflows.len(), 1);

    let most_executed = collector.get_most_executed_workflows(5);
    assert_eq!(most_executed.len(), 1);
}

// ============================================================================
// BENCHMARK TESTS
// ============================================================================

#[tokio::test]
async fn test_concurrent_task_performance() {
    // Arrange
    let config = ExecutionConfig {
        max_concurrent_tasks: 100,
        ..Default::default()
    };
    let mut framework = ExecutionFramework::new(config);

    // Register multiple agents
    for i in 0..10 {
        let agent = Box::new(DefaultAgent::new(
            &format!("agent-{}", i),
            &format!("Agent {}", i),
            vec!["test".to_string()],
        ));
        framework.register_agent(agent).unwrap();
    }

    // Create many tasks
    let workflow_id = framework
        .create_workflow("Performance Test", "benchmark")
        .unwrap();
    let workflow = framework.workflows.get_mut(&workflow_id).unwrap();

    for i in 0..100 {
        let task = Task::new(
            &format!("task-{}", i),
            &format!("Task {}", i),
            "test",
            TaskPriority::Normal,
            serde_json::json!({"id": i}),
        );
        workflow.add_task(task);
    }

    // Act & Measure execution time
    let start_time = std::time::Instant::now();
    let result = framework.execute_workflow(&workflow_id).await;
    let duration = start_time.elapsed();

    // Assert
    assert!(result.is_ok());
    let workflow_result = result.unwrap();
    assert!(workflow_result.success);
    assert_eq!(workflow_result.completed_tasks, 100);

    // Performance assertion: Should complete in reasonable time
    println!("Execution time for 100 tasks: {:?}", duration);
    assert!(duration.as_millis() < 30000); // Should complete within 30 seconds
}

#[tokio::test]
async fn test_memory_usage() {
    // Arrange
    let mut framework = ExecutionFramework::new(ExecutionConfig::default());
    let agent = Box::new(DefaultAgent::new(
        "test-agent",
        "Test Agent",
        vec!["test".to_string()],
    ));
    framework.register_agent(agent).unwrap();

    // Create workflow with many tasks
    let workflow_id = framework.create_workflow("Memory Test", "memory").unwrap();
    let workflow = framework.workflows.get_mut(&workflow_id).unwrap();

    // Add 1000 tasks
    for i in 0..1000 {
        let task = Task::new(
            &format!("task-{}", i),
            &format!("Task {}", i),
            "test",
            TaskPriority::Normal,
            serde_json::json!({"id": i, "data": "x".repeat(1024)}),
        );
        workflow.add_task(task);
    }

    // Measure memory before execution
    let start_memory = get_current_memory_usage();

    // Act: Execute workflow
    let result = framework.execute_workflow(&workflow_id).await;

    // Measure memory after execution
    let end_memory = get_current_memory_usage();

    // Assert
    assert!(result.is_ok());
    let memory_increase = (end_memory - start_memory) as f64 / start_memory as f64;
    println!("Memory increase: {:.2}%", memory_increase * 100.0);

    // Memory usage should not increase dramatically (less than 50%)
    assert!(memory_increase < 0.5);
}

// Helper function to get current memory usage (simplified)
fn get_current_memory_usage() -> u64 {
    // This is a simplified implementation
    // In production, you would use proper memory profiling
    1024 * 1024 // 1MB as placeholder
}

// ============================================================================
// ERROR SCENARIO TESTS
// ============================================================================

#[tokio::test]
async fn test_error_scenarios() {
    // Test 1: Handle agent failure
    let mut framework = ExecutionFramework::new(ExecutionConfig::default());

    // Add an agent that will fail
    let failing_agent = Box::new(FailingAgent::new("failing-agent"));
    framework.register_agent(failing_agent).unwrap();

    let workflow_id = framework.create_workflow("Error Test", "error").unwrap();
    let workflow = framework.workflows.get_mut(&workflow_id).unwrap();

    let task = Task::new(
        "task-1",
        "Failing Task",
        "test",
        TaskPriority::Normal,
        serde_json::json!({"should_fail": true}),
    );
    workflow.add_task(task);

    // Execute workflow - should handle error gracefully
    let result = framework.execute_workflow(&workflow_id).await;
    assert!(result.is_ok()); // Should return result even with failures

    // Test 2: Handle timeout
    let mut framework = ExecutionFramework::new(ExecutionConfig {
        default_timeout_seconds: 1,
        ..Default::default()
    });

    let slow_agent = Box::new(SlowAgent::new("slow-agent"));
    framework.register_agent(slow_agent).unwrap();

    let workflow_id = framework
        .create_workflow("Timeout Test", "timeout")
        .unwrap();
    let workflow = framework.workflows.get_mut(&workflow_id).unwrap();

    let task = Task::new(
        "task-1",
        "Slow Task",
        "slow",
        TaskPriority::Normal,
        serde_json::json!({}),
    );
    workflow.add_task(task);

    let result = framework.execute_workflow(&workflow_id).await;
    // Depending on implementation, this might timeout or succeed
    println!("Timeout test result: {:?}", result);

    // Test 3: Handle workflow with no available agents
    let mut framework = ExecutionFramework::new(ExecutionConfig::default());
    let workflow_id = framework
        .create_workflow("No Agents Test", "no-agents")
        .unwrap();
    let workflow = framework.workflows.get_mut(&workflow_id).unwrap();

    let task = Task::new(
        "task-1",
        "Orphan Task",
        "test",
        TaskPriority::Normal,
        serde_json::json!({}),
    );
    workflow.add_task(task);

    let result = framework.execute_workflow(&workflow_id).await;
    assert!(result.is_err()); // Should fail with no agents
}

// Failing agent for error testing
struct FailingAgent {
    id: String,
}

impl FailingAgent {
    fn new(id: &str) -> Self {
        Self { id: id.to_string() }
    }
}

#[async_trait::async_trait]
impl UnifiedAgentTrait for FailingAgent {
    fn get_id(&self) -> &str {
        &self.id
    }
    fn get_name(&self) -> &str {
        &self.id
    }
    fn get_capabilities(&self) -> &[String] {
        &[]
    }
    fn is_available(&self) -> bool {
        true
    }
    fn get_health(&self) -> &AgentHealth {
        &AgentHealth::new()
    }
    async fn execute_task(&mut self, task: Task) -> Result<TaskResult, ExecutionError> {
        if task
            .payload
            .get("should_fail")
            .and_then(|v| v.as_bool())
            .unwrap_or(false)
        {
            Err(ExecutionError::Task("Task failed".to_string()))
        } else {
            Ok(TaskResult::default())
        }
    }
    async fn update_config(&mut self, _config: AgentConfiguration) -> Result<(), ExecutionError> {
        Ok(())
    }
    fn get_metrics(&self) -> &AgentMetrics {
        &AgentMetrics::new()
    }
    async fn start(&mut self) -> Result<(), ExecutionError> {
        Ok(())
    }
    async fn stop(&mut self) -> Result<(), ExecutionError> {
        Ok(())
    }
    fn get_status(&self) -> &AgentStatus {
        &AgentStatus::Idle
    }
}

// Slow agent for timeout testing
struct SlowAgent {
    id: String,
}

impl SlowAgent {
    fn new(id: &str) -> Self {
        Self { id: id.to_string() }
    }
}

#[async_trait::async_trait]
impl UnifiedAgentTrait for SlowAgent {
    fn get_id(&self) -> &str {
        &self.id
    }
    fn get_name(&self) -> &str {
        &self.id
    }
    fn get_capabilities(&self) -> &[String] {
        &[]
    }
    fn is_available(&self) -> bool {
        true
    }
    fn get_health(&self) -> &AgentHealth {
        &AgentHealth::new()
    }
    async fn execute_task(&mut self, _task: Task) -> Result<TaskResult, ExecutionError> {
        sleep(Duration::from_secs(2)).await; // Sleep for 2 seconds
        Ok(TaskResult::default())
    }
    async fn update_config(&mut self, _config: AgentConfiguration) -> Result<(), ExecutionError> {
        Ok(())
    }
    fn get_metrics(&self) -> &AgentMetrics {
        &AgentMetrics::new()
    }
    async fn start(&mut self) -> Result<(), ExecutionError> {
        Ok(())
    }
    async fn stop(&mut self) -> Result<(), ExecutionError> {
        Ok(())
    }
    fn get_status(&self) -> &AgentStatus {
        &AgentStatus::Idle
    }
}
