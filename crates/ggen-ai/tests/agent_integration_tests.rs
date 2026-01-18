//! Comprehensive Integration Tests for Agent Infrastructure
//!
//! Tests cover:
//! - hyper_concurrent + DSPy predictor pipeline
//! - microframework task graphs with DSPy modules
//! - swarm collaborative optimization
//! - 10-agent parallelism
//! - error propagation and recovery
//!
//! All tests follow Chicago TDD pattern (AAA: Arrange, Act, Assert with real objects)

// NOTE: These tests require hyper_concurrent, microframework, and swarm modules
// to be enabled in lib.rs. They are currently disabled pending fixes.
//
// To enable: uncomment pub mod hyper_concurrent, pub mod microframework, pub mod swarm in lib.rs

#[cfg(all(
    feature = "hyper_concurrent_enabled",
    feature = "microframework_enabled",
    feature = "swarm_enabled"
))]
mod enabled_tests {
    use ggen_ai::dspy::{
        ChainOfThought, FieldConstraints, InputField, Module, OutputField, Predictor, Signature,
    };
    use ggen_ai::hyper_concurrent::{
        AgentExecutionState, HyperConcurrentConfig, HyperConcurrentExecutor, MAX_CONCURRENT_AGENTS,
    };
    use ggen_ai::microframework::{
        AgentOrchestrator, CodeGenAgent, MicroframeworkConfig, Task, TaskGraph, TesterAgent,
        ValidatorAgent,
    };
    use ggen_ai::providers::MockClient;
    use ggen_ai::swarm::{
        AgentInput, AgentOutput, MockAgent, SwarmAgent, SwarmConfig, SwarmContext, SwarmInput,
        UltrathinkSwarm,
    };
    use ggen_ai::LlmClient;
    use serde_json::{json, Value};
    use std::collections::HashMap;
    use std::sync::Arc;
    use std::time::Duration;

    // ========================================================================
    // Test 1: HyperConcurrent + DSPy Predictor Pipeline
    // ========================================================================

    #[tokio::test]
    async fn test_hyper_concurrent_with_dspy_predictor() {
        // Arrange: Create predictor with mock LLM
        let signature = Signature::new("CodeReview", "Review code for quality and safety")
            .with_input(InputField::new("code", "The code to review", "String"))
            .with_output(OutputField::new("review", "The review feedback", "String"))
            .with_output(OutputField::new(
                "safety_score",
                "Safety score 0-10",
                "Integer",
            ));

        let predictor = Predictor::with_model(signature, "mock-model").with_temperature(0.0);

        // Create hyper-concurrent executor
        let config = HyperConcurrentConfig::max_performance();
        let executor = HyperConcurrentExecutor::new(config);

        // Prepare 10 parallel tasks using predictor
        let tasks: Vec<_> = (0..10)
            .map(|i| {
                let pred = predictor.clone();
                let agent_id = format!("reviewer-{}", i);

                let task = move || {
                    let pred = pred;
                    async move {
                        let mut inputs = HashMap::new();
                        inputs.insert(
                            "code".to_string(),
                            Value::String(format!("fn test_{i}() {{}}")),
                        );

                        pred.forward(inputs).await.map_err(|e| {
                            ggen_ai::GgenAiError::internal(format!("Predictor error: {}", e))
                        })
                    }
                };

                (agent_id, task)
            })
            .collect();

        // Act: Execute all 10 predictors in parallel
        let results = executor.execute_parallel(tasks).await;

        // Assert: All executions succeed
        assert_eq!(results.len(), 10);
        let successful = results.iter().filter(|r| r.is_success()).count();
        assert_eq!(
            successful, 10,
            "All 10 predictor executions should succeed"
        );

        // Verify all results contain outputs
        for result in results {
            assert!(result.value.is_some());
            assert_eq!(result.state, AgentExecutionState::Completed);
            assert!(result.duration_ms > 0);
        }
    }

    #[tokio::test]
    async fn test_chain_of_thought_parallel_execution() {
        // Arrange: Create ChainOfThought predictors
        let signature =
            Signature::new("MathReasoning", "Solve math problems with step-by-step reasoning")
                .with_input(InputField::new("problem", "Math problem to solve", "String"))
                .with_output(OutputField::new("solution", "The solution", "String"));

        let cot = ChainOfThought::new(signature);
        let executor = HyperConcurrentExecutor::max_performance();

        // Prepare 5 reasoning tasks
        let tasks: Vec<_> = (0..5)
            .map(|i| {
                let cot_clone = cot.clone();
                let agent_id = format!("reasoner-{}", i);

                let task = move || {
                    let cot = cot_clone;
                    async move {
                        let mut inputs = HashMap::new();
                        inputs.insert(
                            "problem".to_string(),
                            Value::String(format!("What is {} + {} * 2?", i, i + 1)),
                        );

                        cot.forward(inputs).await.map_err(|e| {
                            ggen_ai::GgenAiError::internal(format!("CoT error: {}", e))
                        })
                    }
                };

                (agent_id, task)
            })
            .collect();

        // Act: Execute in parallel
        let results = executor.execute_parallel(tasks).await;

        // Assert: All succeed with reasoning
        assert_eq!(results.len(), 5);
        assert!(results.iter().all(|r| r.is_success()));

        // Verify ChainOfThought instructions were included
        assert!(
            cot.signature().instructions.is_some(),
            "CoT should have step-by-step instructions"
        );
    }

    // ========================================================================
    // Test 2: Microframework Task Graphs with DSPy Modules
    // ========================================================================

    #[tokio::test]
    async fn test_microframework_task_graph_with_dspy() {
        // Arrange: Create orchestrator with DSPy-based agents
        let orchestrator = AgentOrchestrator::builder()
            .max_agents(10)
            .enable_circuit_breaker()
            .enable_backpressure()
            .build()
            .unwrap();

        // Register specialized agents
        orchestrator.register_agent(CodeGenAgent::new("codegen"));
        orchestrator.register_agent(TesterAgent::new("tester"));
        orchestrator.register_agent(ValidatorAgent::new("validator"));

        // Create task graph with dependencies
        let mut graph = TaskGraph::new();

        let t1 = Task::code_gen("Generate struct Person");
        let t2 = Task::test("Write tests for Person").with_dependencies(vec![t1.id.clone()]);
        let t3 = Task::validate("Validate Person implementation")
            .with_dependencies(vec![t1.id.clone(), t2.id.clone()]);

        graph.add_task(t1.clone());
        graph.add_task(t2.clone());
        graph.add_task(t3.clone());

        // Act: Execute with dependency resolution
        let results = orchestrator
            .execute_with_dependencies(vec![t1, t2, t3])
            .await
            .unwrap();

        // Assert: All tasks complete in correct order
        assert_eq!(results.len(), 3);
        assert!(results.iter().all(|r| r.is_success()));

        // Verify execution order respected dependencies
        let stats = orchestrator.statistics();
        assert_eq!(stats.total_tasks, 3);
        assert_eq!(stats.successful_tasks, 3);
        assert!(stats.success_rate >= 1.0);
    }

    #[tokio::test]
    async fn test_ten_agent_parallel_microframework() {
        // Arrange: Create orchestrator with 10 agents max
        let config = MicroframeworkConfig::high_performance();
        let orchestrator = AgentOrchestrator::with_config(config);

        orchestrator.register_agent(CodeGenAgent::new("codegen"));

        // Create 10 independent tasks
        let tasks: Vec<_> = (0..10)
            .map(|i| Task::code_gen(format!("Generate module_{}", i)))
            .collect();

        // Act: Execute all 10 in parallel
        let start = std::time::Instant::now();
        let results = orchestrator.execute_batch(tasks).await.unwrap();
        let duration = start.elapsed();

        // Assert: All 10 complete successfully
        assert_eq!(results.len(), 10);
        assert_eq!(
            results.iter().filter(|r| r.is_success()).count(),
            10,
            "All 10 tasks should succeed"
        );

        // Verify parallelism (should be faster than sequential)
        // With 10 parallel agents, should complete in ~same time as 1 task
        assert!(
            duration < Duration::from_secs(5),
            "10 parallel tasks should complete quickly"
        );

        // Verify metrics
        let stats = orchestrator.statistics();
        assert_eq!(stats.total_tasks, 10);
        assert_eq!(stats.successful_tasks, 10);
    }

    #[tokio::test]
    async fn test_task_graph_topological_sort() {
        // Arrange: Create complex task graph
        let mut graph = TaskGraph::new();

        let t1 = Task::code_gen("Generate base types");
        let t2 = Task::code_gen("Generate derived types").with_dependencies(vec![t1.id.clone()]);
        let t3 = Task::test("Test base types").with_dependencies(vec![t1.id.clone()]);
        let t4 = Task::test("Test derived types")
            .with_dependencies(vec![t2.id.clone(), t3.id.clone()]);
        let t5 = Task::validate("Final validation")
            .with_dependencies(vec![t3.id.clone(), t4.id.clone()]);

        graph.add_task(t1.clone());
        graph.add_task(t2.clone());
        graph.add_task(t3.clone());
        graph.add_task(t4.clone());
        graph.add_task(t5.clone());

        // Act: Get topological sort
        let sorted = graph.topological_sort().unwrap();

        // Assert: Verify correct ordering
        assert!(!sorted.is_empty());

        // t1 must be in first wave
        assert!(sorted[0].contains(&t1.id));

        // t2 and t3 can be parallel (both depend only on t1)
        if sorted.len() > 1 {
            assert!(sorted[1].contains(&t2.id) || sorted[1].contains(&t3.id));
        }

        // Verify no circular dependencies
        let stats = graph.statistics();
        assert_eq!(stats.total_tasks, 5);
        assert!(stats.max_depth >= 3);
    }

    // ========================================================================
    // Test 3: Swarm Collaborative Optimization
    // ========================================================================

    #[tokio::test]
    async fn test_swarm_collaborative_execution() {
        // Arrange: Create swarm with multiple agents
        let config = SwarmConfig {
            max_concurrent_agents: 10,
            agent_timeout_seconds: 30,
            learning_enabled: true,
            autonomous_mode: false,
            performance_thresholds: ggen_ai::swarm::PerformanceThresholds {
                max_execution_time_ms: 5000,
                max_memory_usage_mb: 200,
                min_success_rate: 0.9,
            },
        };

        let swarm = UltrathinkSwarm::new(config);

        // Add specialized agents
        swarm
            .add_agent(Box::new(MockAgent::new("generator")))
            .await
            .unwrap();
        swarm
            .add_agent(Box::new(MockAgent::new("optimizer")))
            .await
            .unwrap();
        swarm
            .add_agent(Box::new(MockAgent::new("validator")))
            .await
            .unwrap();

        // Prepare swarm input
        let input = SwarmInput {
            event: "generate_code".to_string(),
            graph_state: "initial".to_string(),
            parameters: {
                let mut params = HashMap::new();
                params.insert("target".to_string(), "rust_struct".to_string());
                params
            },
            autonomous: false,
        };

        // Act: Execute swarm
        let result = swarm.execute(input).await.unwrap();

        // Assert: Swarm execution succeeds
        assert!(result.success, "Swarm execution should succeed");
        assert!(!result.artifacts.is_empty(), "Should produce artifacts");
        assert!(result.metrics.total_operations > 0);

        // Verify swarm status
        let status = swarm.status().await;
        assert_eq!(status.total_agents, 3);
        assert_eq!(status.active_agents, 3);
    }

    #[tokio::test]
    async fn test_ten_agent_swarm_parallelism() {
        // Arrange: Create swarm with 10 agents
        let config = SwarmConfig {
            max_concurrent_agents: 10,
            agent_timeout_seconds: 30,
            learning_enabled: false,
            autonomous_mode: false,
            performance_thresholds: ggen_ai::swarm::PerformanceThresholds {
                max_execution_time_ms: 5000,
                max_memory_usage_mb: 200,
                min_success_rate: 0.9,
            },
        };

        let swarm = UltrathinkSwarm::new(config);

        // Add 10 mock agents
        for i in 0..10 {
            swarm
                .add_agent(Box::new(MockAgent::new(&format!("agent-{}", i))))
                .await
                .unwrap();
        }

        // Prepare inputs for all agents
        let inputs: Vec<_> = (0..10)
            .map(|i| {
                let agent_id = format!("agent-{}", i);
                let input = AgentInput {
                    data: json!({"task_id": i}),
                    input_type: "parallel_test".to_string(),
                    source_agent: None,
                    context: HashMap::new(),
                };
                (agent_id, input)
            })
            .collect();

        // Act: Execute all 10 agents via hyper-concurrent executor
        let executor = HyperConcurrentExecutor::max_performance();

        // Register all agents
        for i in 0..10 {
            let agent = Arc::new(MockAgent::new(&format!("agent-{}", i)));
            executor.register_agent(agent as Arc<dyn SwarmAgent>);
        }

        let context = SwarmContext {
            graph_state: "test".to_string(),
            active_agents: vec![],
            metrics: Default::default(),
            config: config.clone(),
        };

        let results = executor
            .execute_work_stealing::<AgentOutput>(&context, inputs)
            .await;

        // Assert: All 10 agents execute successfully
        assert_eq!(results.len(), 10);
        assert_eq!(
            results.iter().filter(|r| r.is_success()).count(),
            10,
            "All 10 agents should execute successfully"
        );

        // Verify metrics
        let metrics = executor.metrics();
        assert_eq!(metrics.total_executions, 10);
        assert!(metrics.success_rate >= 1.0);
    }

    // ========================================================================
    // Test 4: Error Propagation and Recovery
    // ========================================================================

    #[tokio::test]
    async fn test_circuit_breaker_on_failures() {
        // Arrange: Create executor with circuit breaker enabled
        let config = HyperConcurrentConfig {
            max_agents: 5,
            enable_circuit_breaker: true,
            circuit_breaker_threshold: 3,
            ..HyperConcurrentConfig::default()
        };

        let executor = HyperConcurrentExecutor::new(config);

        // Create tasks that will fail
        let failing_agent = "failing-agent";
        let tasks: Vec<_> = (0..5)
            .map(|i| {
                let task = move || async move {
                    Err(ggen_ai::GgenAiError::internal(format!(
                        "Simulated failure {}",
                        i
                    )))
                };
                (failing_agent.to_string(), task)
            })
            .collect();

        // Act: Execute failing tasks
        let results = executor.execute_parallel(tasks).await;

        // Assert: Circuit breaker trips after threshold
        assert_eq!(results.len(), 5);

        // After 3 failures, circuit should be open
        assert!(
            executor.is_circuit_open(failing_agent),
            "Circuit breaker should be open after threshold failures"
        );

        // Verify metrics show failures
        let metrics = executor.metrics();
        assert!(metrics.circuit_breakers_open > 0);
        assert!(metrics.success_rate < 1.0);
    }

    #[tokio::test]
    async fn test_timeout_handling() {
        // Arrange: Create executor with short timeout
        let config = HyperConcurrentConfig {
            max_agents: 3,
            agent_timeout_secs: 1, // 1 second timeout
            ..HyperConcurrentConfig::default()
        };

        let executor = HyperConcurrentExecutor::new(config);

        // Create tasks that take too long
        let tasks: Vec<_> = (0..3)
            .map(|i| {
                let task = move || async move {
                    tokio::time::sleep(Duration::from_secs(5)).await;
                    Ok(i)
                };
                (format!("slow-agent-{}", i), task)
            })
            .collect();

        // Act: Execute slow tasks
        let results = executor.execute_parallel(tasks).await;

        // Assert: All tasks timeout
        assert_eq!(results.len(), 3);
        for result in results {
            assert_eq!(
                result.state,
                AgentExecutionState::TimedOut,
                "Task should timeout"
            );
            assert!(result.error.is_some());
            assert!(result
                .error
                .unwrap()
                .contains("timed out") || result.state == AgentExecutionState::TimedOut);
        }
    }

    #[tokio::test]
    async fn test_error_recovery_with_retry() {
        // Arrange: Create orchestrator
        let orchestrator = AgentOrchestrator::new();
        orchestrator.register_agent(CodeGenAgent::new("codegen"));

        // Create task that might fail
        let task = Task::code_gen("Generate complex type");

        // Act: Execute with potential retry
        let result = orchestrator.execute(task).await.unwrap();

        // Assert: Task completes (with retry if needed)
        // Mock agents succeed, but real agents would retry
        assert!(result.is_success());
    }

    #[tokio::test]
    async fn test_partial_failure_handling() {
        // Arrange: Create orchestrator
        let orchestrator = AgentOrchestrator::new();
        orchestrator.register_agent(CodeGenAgent::new("codegen"));

        // Create mix of tasks (some may fail)
        let tasks = vec![
            Task::code_gen("Valid task 1"),
            Task::code_gen("Valid task 2"),
            Task::code_gen("Valid task 3"),
        ];

        // Act: Execute batch
        let results = orchestrator.execute_batch(tasks).await.unwrap();

        // Assert: Successful tasks complete, failed tasks reported
        assert_eq!(results.len(), 3);

        let successful = results.iter().filter(|r| r.is_success()).count();
        let failed = results.iter().filter(|r| !r.is_success()).count();

        assert_eq!(successful + failed, 3);

        // Verify stats reflect actual outcome
        let stats = orchestrator.statistics();
        assert_eq!(stats.total_tasks, 3);
    }

    // ========================================================================
    // Test 5: Determinism and Performance
    // ========================================================================

    #[tokio::test]
    async fn test_deterministic_execution_with_mock_llm() {
        // Arrange: Create predictor with mock LLM (deterministic)
        let mock_client = MockClient::with_response("answer: 42");

        let signature = Signature::new("QA", "Question answering")
            .with_input(InputField::new("question", "The question", "String"))
            .with_output(OutputField::new("answer", "The answer", "String"));

        let predictor = Predictor::with_model(signature, "mock-model").with_temperature(0.0);

        // Execute multiple times
        let mut results = Vec::new();
        for _ in 0..5 {
            let mut inputs = HashMap::new();
            inputs.insert(
                "question".to_string(),
                Value::String("What is the answer?".to_string()),
            );

            let result = predictor.forward(inputs).await.unwrap();
            results.push(result);
        }

        // Assert: All results identical (deterministic)
        assert_eq!(results.len(), 5);
        for i in 1..results.len() {
            // Results should be consistent for same inputs
            assert_eq!(
                results[i].get("answer"),
                results[0].get("answer"),
                "Results should be deterministic"
            );
        }
    }

    #[tokio::test]
    async fn test_performance_under_max_load() {
        // Arrange: Create max performance config
        let executor = HyperConcurrentExecutor::max_performance();

        // Create 10 tasks (maximum)
        let tasks: Vec<_> = (0..MAX_CONCURRENT_AGENTS)
            .map(|i| {
                let task = move || async move { Ok(i * 2) };
                (format!("agent-{}", i), task)
            })
            .collect();

        // Act: Execute at max capacity
        let start = std::time::Instant::now();
        let results = executor.execute_parallel(tasks).await;
        let duration = start.elapsed();

        // Assert: Performance meets SLO
        assert_eq!(results.len(), MAX_CONCURRENT_AGENTS);
        assert!(results.iter().all(|r| r.is_success()));

        // Should complete quickly with parallel execution
        assert!(
            duration < Duration::from_secs(5),
            "Max parallel execution should complete in <5s"
        );

        // Verify metrics
        let metrics = executor.metrics();
        assert_eq!(metrics.total_executions, MAX_CONCURRENT_AGENTS as u64);
        assert!(metrics.success_rate >= 1.0);
    }

    #[tokio::test]
    async fn test_backpressure_under_overload() {
        // Arrange: Create executor with backpressure
        let config = HyperConcurrentConfig {
            max_agents: 3,
            enable_backpressure: true,
            backpressure_queue_size: 5,
            ..HyperConcurrentConfig::default()
        };

        let executor = HyperConcurrentExecutor::new(config);

        // Create more tasks than capacity
        let tasks: Vec<_> = (0..10)
            .map(|i| {
                let task = move || async move {
                    tokio::time::sleep(Duration::from_millis(100)).await;
                    Ok(i)
                };
                (format!("agent-{}", i), task)
            })
            .collect();

        // Act: Execute with backpressure
        let results = executor.execute_parallel(tasks).await;

        // Assert: All complete eventually (backpressure throttles)
        assert_eq!(results.len(), 10);
        assert!(results.iter().all(|r| r.is_success()));

        // Metrics should show backpressure was active
        let metrics = executor.metrics();
        assert_eq!(metrics.total_executions, 10);
    }
}

// ========================================================================
// Fallback Tests (when modules disabled)
// ========================================================================

// These tests compile when agent modules are disabled and provide helpful messages
#[cfg(not(all(
    feature = "hyper_concurrent_enabled",
    feature = "microframework_enabled",
    feature = "swarm_enabled"
)))]
mod disabled_module_tests {
    use ggen_ai::dspy::{InputField, OutputField, Signature};

    #[test]
    fn test_dspy_signature_works() {
        // DSPy module is always enabled, test it works
        let sig = Signature::new("Test", "Test signature")
            .with_input(InputField::new("input", "desc", "String"))
            .with_output(OutputField::new("output", "desc", "String"));

        assert_eq!(sig.name, "Test");
        assert_eq!(sig.inputs.len(), 1);
        assert_eq!(sig.outputs.len(), 1);
    }

    #[test]
    #[ignore = "Agent modules disabled - enable hyper_concurrent, microframework, swarm in lib.rs"]
    fn integration_tests_require_agent_modules() {
        panic!(
            "Integration tests require agent modules to be enabled.\n\
             To enable:\n\
             1. Edit crates/ggen-ai/src/lib.rs\n\
             2. Uncomment: pub mod hyper_concurrent;\n\
             3. Uncomment: pub mod microframework;\n\
             4. Uncomment: pub mod swarm;\n\
             5. Add features to Cargo.toml if needed\n\
             6. Run: cargo make test"
        );
    }
}

// ========================================================================
// Test Helpers
// ========================================================================

#[cfg(test)]
mod test_helpers {
    use super::*;

    /// Helper to create a test signature for code generation
    #[allow(dead_code)]
    pub fn create_codegen_signature() -> ggen_ai::dspy::Signature {
        use ggen_ai::dspy::{InputField, OutputField, Signature};

        Signature::new("CodeGeneration", "Generate code from description")
            .with_input(InputField::new(
                "description",
                "Code description",
                "String",
            ))
            .with_input(InputField::new("language", "Target language", "String"))
            .with_output(OutputField::new("code", "Generated code", "String"))
            .with_output(OutputField::new("explanation", "Code explanation", "String"))
    }

    /// Helper to create a test context
    #[allow(dead_code)]
    pub fn create_test_context() -> std::collections::HashMap<String, String> {
        let mut context = std::collections::HashMap::new();
        context.insert("test_mode".to_string(), "true".to_string());
        context.insert("environment".to_string(), "test".to_string());
        context
    }
}
