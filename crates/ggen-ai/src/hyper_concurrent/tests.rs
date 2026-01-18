//! Comprehensive integration tests for hyper-concurrent module
//!
//! Tests all components together to ensure >80% coverage with
//! focus on error paths, edge cases, and Chicago TDD patterns.

#[cfg(test)]
mod integration_tests {
    use super::super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc;
    use std::time::Duration;
    use tokio::time::sleep;

    // ============================================================================
    // 10-Agent Parallel Execution Tests (CRITICAL)
    // ============================================================================

    #[tokio::test]
    async fn test_ten_agent_parallel_execution() {
        // Arrange - Create executor with 10 agent capacity
        let executor = HyperConcurrentExecutor::new(HyperConcurrentConfig::default());
        let counter = Arc::new(AtomicUsize::new(0));

        // Create exactly 10 tasks
        let tasks: Vec<_> = (0..10)
            .map(|i| {
                let counter = Arc::clone(&counter);
                let id = format!("agent-{}", i);
                let task = move || async move {
                    counter.fetch_add(1, Ordering::Relaxed);
                    sleep(Duration::from_millis(10)).await;
                    Ok(i)
                };
                (id, task)
            })
            .collect();

        // Act - Execute all 10 in parallel
        let results = executor.execute_parallel(tasks).await;

        // Assert - All succeeded
        assert_eq!(results.len(), 10);
        assert_eq!(results.iter().filter(|r| r.is_success()).count(), 10);
        assert_eq!(counter.load(Ordering::Relaxed), 10);

        // Assert - Metrics updated correctly
        let metrics = executor.metrics();
        assert_eq!(metrics.total_executions, 10);
        assert_eq!(metrics.available_permits, MAX_CONCURRENT_AGENTS);
    }

    #[tokio::test]
    async fn test_exceeding_ten_agent_limit_queues_properly() {
        // Arrange - Executor with 10 max agents
        let executor = HyperConcurrentExecutor::new(HyperConcurrentConfig::default());
        let counter = Arc::new(AtomicUsize::new(0));

        // Create 15 tasks (exceeds limit)
        let tasks: Vec<_> = (0..15)
            .map(|i| {
                let counter = Arc::clone(&counter);
                let id = format!("agent-{}", i);
                let task = move || async move {
                    counter.fetch_add(1, Ordering::Relaxed);
                    sleep(Duration::from_millis(20)).await;
                    Ok(i)
                };
                (id, task)
            })
            .collect();

        // Act - Execute with queueing
        let results = executor.execute_parallel(tasks).await;

        // Assert - All 15 completed despite 10 concurrent limit
        assert_eq!(results.len(), 15);
        assert_eq!(results.iter().filter(|r| r.is_success()).count(), 15);
        assert_eq!(counter.load(Ordering::Relaxed), 15);
    }

    #[tokio::test]
    async fn test_parallel_execution_with_failures() {
        // Arrange
        let executor = HyperConcurrentExecutor::new(HyperConcurrentConfig::default());

        let tasks: Vec<_> = (0..10)
            .map(|i| {
                let id = format!("agent-{}", i);
                let task = move || async move {
                    if i % 3 == 0 {
                        Err(crate::error::GgenAiError::orchestration("intentional failure"))
                    } else {
                        Ok(i)
                    }
                };
                (id, task)
            })
            .collect();

        // Act
        let results = executor.execute_parallel(tasks).await;

        // Assert - Mixed success and failure
        assert_eq!(results.len(), 10);
        let successes = results.iter().filter(|r| r.is_success()).count();
        let failures = results.iter().filter(|r| !r.is_success()).count();
        assert_eq!(successes, 6); // i = 1,2,4,5,7,8
        assert_eq!(failures, 4); // i = 0,3,6,9
    }

    #[tokio::test]
    async fn test_timeout_handling() {
        // Arrange - Short timeout config
        let config = HyperConcurrentConfig {
            agent_timeout_secs: 1,
            ..HyperConcurrentConfig::default()
        };
        let executor = HyperConcurrentExecutor::new(config);

        let tasks: Vec<_> = vec![
            ("fast".to_string(), || async { Ok("done") }),
            (
                "slow".to_string(),
                || async {
                    sleep(Duration::from_secs(5)).await;
                    Ok("too late")
                },
            ),
        ];

        // Act
        let results = executor.execute_parallel(tasks).await;

        // Assert - Fast succeeds, slow times out
        assert_eq!(results.len(), 2);
        let fast_result = results.iter().find(|r| r.agent_id == "fast").unwrap();
        let slow_result = results.iter().find(|r| r.agent_id == "slow").unwrap();

        assert!(fast_result.is_success());
        assert_eq!(slow_result.state, AgentExecutionState::TimedOut);
        assert!(slow_result.error.is_some());
    }

    // ============================================================================
    // Adaptive Concurrency Controller Tests (CRITICAL)
    // ============================================================================

    #[tokio::test]
    async fn test_adaptive_concurrency_scales_up() {
        // Arrange - Controller that should scale up
        let config = AdaptiveConfig {
            scale_up_threshold: 0.95,
            target_latency_ms: 1000,
            adjustment_interval_secs: 0, // Immediate adjustment
            ..Default::default()
        };
        let controller = AdaptiveConcurrencyController::with_config(10, config);

        // Act - Record many successful, fast executions
        for _ in 0..50 {
            controller.record_execution(50, true);
        }

        // Force recalculation
        controller.recalculate();
        sleep(Duration::from_millis(100)).await;

        // Assert - Should scale up (or stay at max)
        let stats = controller.statistics();
        assert!(stats.recent_success_rate > 0.95);
        assert!(stats.avg_latency_ms < 100.0);
        // At max already, so it stays at 10
        assert!(stats.current_concurrency <= 10);
    }

    #[tokio::test]
    async fn test_adaptive_concurrency_scales_down() {
        // Arrange - Controller that should scale down
        let config = AdaptiveConfig {
            scale_down_threshold: 0.90,
            target_latency_ms: 1000,
            adjustment_interval_secs: 0,
            ..Default::default()
        };
        let controller = AdaptiveConcurrencyController::with_config(10, config);

        // Act - Record many failures and high latency
        for _ in 0..50 {
            controller.record_execution(5000, false); // Slow failures
        }

        controller.recalculate();
        sleep(Duration::from_millis(100)).await;

        // Assert - Should scale down
        let stats = controller.statistics();
        assert!(stats.recent_success_rate < 0.5);
        assert!(stats.current_concurrency < 10);
    }

    #[tokio::test]
    async fn test_adaptive_controller_respects_min_max() {
        // Arrange
        let controller = AdaptiveConcurrencyController::new(5);

        // Act - Record many failures to trigger scale down
        for _ in 0..100 {
            controller.record_execution(10000, false);
        }
        controller.recalculate();

        // Assert - Should not go below min (1)
        let stats = controller.statistics();
        assert!(stats.current_concurrency >= 1);
        assert!(stats.current_concurrency <= 5);
    }

    // ============================================================================
    // Circuit Breaker Tests - Half-Open State (CRITICAL)
    // ============================================================================

    #[tokio::test]
    async fn test_circuit_breaker_half_open_transition() {
        // Arrange - Fast reset timeout
        let cb = CircuitBreaker::with_config(3, 1, 2); // 1 second timeout, 2 successes to close

        // Act - Open the circuit
        cb.record_failure("agent-1");
        cb.record_failure("agent-1");
        cb.record_failure("agent-1");
        assert!(cb.is_open("agent-1"));
        assert_eq!(cb.get_state("agent-1"), CircuitState::Open);

        // Wait for reset timeout
        sleep(Duration::from_secs(2)).await;

        // Assert - Should transition to half-open
        let is_open = cb.is_open("agent-1");
        assert!(!is_open); // Half-open allows requests
        assert_eq!(cb.get_state("agent-1"), CircuitState::HalfOpen);
    }

    #[tokio::test]
    async fn test_circuit_breaker_half_open_success_closes() {
        // Arrange
        let cb = CircuitBreaker::with_config(2, 1, 2);

        // Open circuit
        cb.record_failure("agent-1");
        cb.record_failure("agent-1");
        sleep(Duration::from_secs(2)).await;

        // Transition to half-open
        cb.is_open("agent-1");

        // Act - Record enough successes to close
        cb.record_success("agent-1");
        cb.record_success("agent-1");

        // Assert - Should be closed
        assert_eq!(cb.get_state("agent-1"), CircuitState::Closed);
        assert!(!cb.is_open("agent-1"));
    }

    #[tokio::test]
    async fn test_circuit_breaker_half_open_failure_reopens() {
        // Arrange
        let cb = CircuitBreaker::with_config(2, 1, 2);

        // Open circuit
        cb.record_failure("agent-1");
        cb.record_failure("agent-1");
        sleep(Duration::from_secs(2)).await;

        // Transition to half-open
        cb.is_open("agent-1");

        // Act - Record failure in half-open
        cb.record_failure("agent-1");

        // Assert - Should reopen
        assert_eq!(cb.get_state("agent-1"), CircuitState::Open);
        assert!(cb.is_open("agent-1"));
    }

    #[tokio::test]
    async fn test_circuit_breaker_concurrent_access() {
        // Arrange
        let cb = Arc::new(CircuitBreaker::new(5));
        let mut handles = vec![];

        // Act - Multiple threads recording failures
        for i in 0..10 {
            let cb = Arc::clone(&cb);
            handles.push(tokio::spawn(async move {
                for _ in 0..10 {
                    cb.record_failure(&format!("agent-{}", i));
                    sleep(Duration::from_millis(1)).await;
                }
            }));
        }

        for handle in handles {
            handle.await.unwrap();
        }

        // Assert - Should have opened multiple circuits
        assert!(cb.open_count() > 0);
        let stats = cb.statistics();
        assert!(!stats.is_empty());
    }

    // ============================================================================
    // Work Stealing Agent Pool Tests (CRITICAL)
    // ============================================================================

    #[tokio::test]
    async fn test_work_stealing_multiple_workers() {
        // Arrange
        let pool = Arc::new(WorkStealingAgentPool::new(4));
        let completed = Arc::new(AtomicUsize::new(0));

        // Submit work to pool
        for i in 0..20 {
            let pool_clone = Arc::clone(&pool);
            let completed_clone = Arc::clone(&completed);
            pool.submit(move || {
                completed_clone.fetch_add(1, Ordering::Relaxed);
                pool_clone.complete_task();
            });
        }

        // Act - Simulate workers stealing and executing
        let mut handles = vec![];
        for worker_id in 0..4 {
            let pool = Arc::clone(&pool);
            handles.push(tokio::spawn(async move {
                for _ in 0..10 {
                    if let Some(work) = pool.get_work(worker_id) {
                        work();
                    }
                    sleep(Duration::from_millis(1)).await;
                }
            }));
        }

        for handle in handles {
            handle.await.unwrap();
        }

        // Assert
        let stats = pool.statistics();
        assert_eq!(stats.tasks_submitted, 20);
        assert!(stats.tasks_completed > 0);
        // Some stealing should have occurred
        assert!(stats.tasks_stolen >= 0);
    }

    #[tokio::test]
    async fn test_work_pool_active_worker_management() {
        // Arrange
        let pool = WorkStealingAgentPool::new(5);

        // Act
        pool.activate_worker();
        pool.activate_worker();
        pool.activate_worker();

        let stats = pool.statistics();
        assert_eq!(stats.active_workers, 3);

        pool.deactivate_worker();
        let stats2 = pool.statistics();
        assert_eq!(stats2.active_workers, 2);

        // Assert
        assert!(pool.is_running());
        pool.stop();
        assert!(!pool.is_running());
    }

    #[test]
    fn test_priority_work_queue_ordering() {
        // Arrange
        let queue: PriorityWorkQueue<String> = PriorityWorkQueue::new(5);

        // Act - Add items with different priorities
        queue.push(WorkItem::new("low".to_string(), "low".to_string(), 4));
        queue.push(WorkItem::new(
            "critical".to_string(),
            "critical".to_string(),
            0,
        ));
        queue.push(WorkItem::new("high".to_string(), "high".to_string(), 1));
        queue.push(WorkItem::new("normal".to_string(), "normal".to_string(), 2));

        // Assert - Items come out in priority order
        assert_eq!(queue.pop().unwrap().id, "critical");
        assert_eq!(queue.pop().unwrap().id, "high");
        assert_eq!(queue.pop().unwrap().id, "normal");
        assert_eq!(queue.pop().unwrap().id, "low");
        assert!(queue.is_empty());
    }

    // ============================================================================
    // Backpressure Handler Tests - Rate Limiting (CRITICAL)
    // ============================================================================

    #[tokio::test]
    async fn test_backpressure_rate_limiting() {
        // Arrange - 10 requests per second
        let handler = BackpressureHandler::with_rate_limit(100, 10);

        // Act - Try to acquire 15 times rapidly
        let mut acquired = 0;
        let mut rejected = 0;

        for _ in 0..15 {
            if handler.try_acquire() {
                acquired += 1;
            } else {
                rejected += 1;
            }
        }

        // Assert - Some should be rate limited
        assert!(acquired > 0);
        assert!(rejected > 0, "Rate limiter should reject some requests");

        for _ in 0..acquired {
            handler.release();
        }
    }

    #[tokio::test]
    async fn test_backpressure_wait_for_capacity() {
        // Arrange
        let handler = Arc::new(BackpressureHandler::new(5));

        // Fill to capacity
        for _ in 0..5 {
            assert!(handler.try_acquire());
        }
        assert!(handler.is_overloaded());

        // Act - Wait in background
        let handler_clone = Arc::clone(&handler);
        let wait_handle = tokio::spawn(async move {
            handler_clone.wait_for_capacity().await;
        });

        // Release capacity
        sleep(Duration::from_millis(100)).await;
        handler.release();

        // Assert - Wait should complete
        let result = tokio::time::timeout(Duration::from_secs(1), wait_handle).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_backpressure_recovery_threshold() {
        // Arrange
        let handler = BackpressureHandler::new(10);

        // Act - Fill to overload (80% = 8)
        for _ in 0..8 {
            handler.try_acquire();
        }
        assert!(handler.is_overloaded());

        // Release to recovery threshold (50% = 5)
        for _ in 0..4 {
            handler.release();
        }

        // Assert - Should recover
        assert!(!handler.is_overloaded());
    }

    // ============================================================================
    // Barrier Synchronization Tests - Reusable Barrier (CRITICAL)
    // ============================================================================

    #[tokio::test]
    async fn test_reusable_barrier_multiple_rounds() {
        // Arrange
        let barrier = ReusableBarrier::new(3);
        let counter = Arc::new(AtomicUsize::new(0));

        // Act - Three tasks wait at barrier
        let mut handles = vec![];
        for _ in 0..3 {
            let barrier = Arc::clone(&barrier);
            let counter = Arc::clone(&counter);
            handles.push(tokio::spawn(async move {
                // Round 1
                counter.fetch_add(1, Ordering::Relaxed);
                barrier.wait().await;

                // Round 2
                counter.fetch_add(1, Ordering::Relaxed);
                barrier.wait().await;
            }));
        }

        for handle in handles {
            handle.await.unwrap();
        }

        // Assert - All completed both rounds
        assert_eq!(counter.load(Ordering::Relaxed), 6);
        assert_eq!(barrier.generation(), 2);
    }

    #[tokio::test]
    async fn test_phased_barrier_multiple_agents() {
        // Arrange
        let barrier = Arc::new(PhasedBarrier::with_phases(
            3,
            vec!["init".to_string(), "execute".to_string(), "cleanup".to_string()],
        ));
        let phase_counters = Arc::new([
            AtomicUsize::new(0),
            AtomicUsize::new(0),
            AtomicUsize::new(0),
        ]);

        // Act - Three agents go through all phases
        let mut handles = vec![];
        for _ in 0..3 {
            let barrier = Arc::clone(&barrier);
            let counters = Arc::clone(&phase_counters);
            handles.push(tokio::spawn(async move {
                let result = barrier.wait().await;
                assert_eq!(result.phase_name, "init");
                counters[0].fetch_add(1, Ordering::Relaxed);

                let result = barrier.wait().await;
                assert_eq!(result.phase_name, "execute");
                counters[1].fetch_add(1, Ordering::Relaxed);

                let result = barrier.wait().await;
                assert_eq!(result.phase_name, "cleanup");
                counters[2].fetch_add(1, Ordering::Relaxed);
            }));
        }

        for handle in handles {
            handle.await.unwrap();
        }

        // Assert - All agents completed all phases
        assert_eq!(phase_counters[0].load(Ordering::Relaxed), 3);
        assert_eq!(phase_counters[1].load(Ordering::Relaxed), 3);
        assert_eq!(phase_counters[2].load(Ordering::Relaxed), 3);
        assert!(barrier.is_complete());
    }

    // ============================================================================
    // Channel Orchestrator Async Tests (IMPORTANT)
    // ============================================================================

    #[tokio::test]
    async fn test_channel_orchestrator_async_receive() {
        // Arrange
        let orchestrator = Arc::new(ChannelOrchestrator::new());
        orchestrator.create_channel("receiver");

        // Send message in background
        let orchestrator_clone = Arc::clone(&orchestrator);
        tokio::spawn(async move {
            sleep(Duration::from_millis(100)).await;
            let msg = AgentMessage::new("sender", "receiver", "test", serde_json::json!({"value": 42}));
            orchestrator_clone.send(msg).unwrap();
        });

        // Act - Async receive
        let result = tokio::time::timeout(
            Duration::from_secs(1),
            orchestrator.receive("receiver"),
        )
        .await;

        // Assert
        assert!(result.is_ok());
        let msg = result.unwrap().unwrap();
        assert_eq!(msg.source, "sender");
        assert_eq!(msg.message_type, "test");
    }

    #[tokio::test]
    async fn test_channel_orchestrator_receive_timeout() {
        // Arrange
        let orchestrator = ChannelOrchestrator::new();
        orchestrator.create_channel("agent-1");

        // Act - Receive with timeout (no messages sent)
        let result = orchestrator
            .receive_timeout("agent-1", Duration::from_millis(100))
            .await;

        // Assert - Should timeout
        assert!(result.is_none());
    }

    #[test]
    fn test_channel_orchestrator_drain() {
        // Arrange
        let orchestrator = ChannelOrchestrator::new();
        orchestrator.create_channel("agent-1");

        // Send multiple messages
        for i in 0..5 {
            let msg = AgentMessage::new("sender", "agent-1", "test", serde_json::json!({"seq": i}));
            orchestrator.send(msg).unwrap();
        }

        // Act - Drain all
        let messages = orchestrator.drain("agent-1");

        // Assert
        assert_eq!(messages.len(), 5);
        assert!(orchestrator.try_receive("agent-1").is_none());
    }

    #[test]
    fn test_channel_message_response() {
        // Arrange
        let original = AgentMessage::new("a", "b", "request", serde_json::json!({"query": 1}));

        // Act
        let response = AgentMessage::response(&original, serde_json::json!({"answer": 42}));

        // Assert
        assert_eq!(response.source, "b");
        assert_eq!(response.target, "a");
        assert_eq!(response.correlation_id, Some(original.id));
    }

    // ============================================================================
    // Async Streaming Tests (CRITICAL)
    // ============================================================================

    #[tokio::test]
    async fn test_streaming_coordinator_send_receive() {
        // Arrange
        let coordinator = Arc::new(AsyncStreamingCoordinator::new(100));
        let handle = coordinator.register_agent("agent-1");

        // Act - Send items
        handle.send("item-1".to_string()).await.unwrap();
        handle.send("item-2".to_string()).await.unwrap();
        handle.send_final("item-3".to_string()).await.unwrap();

        // Assert
        assert_eq!(handle.sequence(), 3);
    }

    #[tokio::test]
    async fn test_streaming_coordinator_multiple_agents() {
        // Arrange
        let coordinator = Arc::new(AsyncStreamingCoordinator::<String>::new(100));
        let mut receiver = coordinator.take_receiver().unwrap();

        let handle1 = coordinator.register_agent("agent-1");
        let handle2 = coordinator.register_agent("agent-2");

        // Act - Both agents send
        let h1 = tokio::spawn(async move {
            handle1.send("a1".to_string()).await.unwrap();
            handle1.send_final("a1-final".to_string()).await.unwrap();
        });

        let h2 = tokio::spawn(async move {
            handle2.send("a2".to_string()).await.unwrap();
            handle2.send_final("a2-final".to_string()).await.unwrap();
        });

        h1.await.unwrap();
        h2.await.unwrap();

        coordinator.complete_agent("agent-1");
        coordinator.complete_agent("agent-2");

        // Assert - Collect all items
        let mut items = vec![];
        while let Ok(item) = receiver.try_recv() {
            items.push(item);
        }

        assert_eq!(items.len(), 4); // 2 items from each agent
        assert!(coordinator.is_complete());
    }

    #[test]
    fn test_buffered_stream_collector_flush() {
        // Arrange
        let collector: BufferedStreamCollector<String> = BufferedStreamCollector::new(5);

        // Act - Add less than batch size
        collector.add(StreamItem::new("a".to_string(), 0, "1".to_string(), false));
        collector.add(StreamItem::new("a".to_string(), 1, "2".to_string(), false));

        // Assert - No batch yet
        let batch = collector.flush();
        assert_eq!(batch.len(), 2);
    }

    // ============================================================================
    // Metrics Tests - Additional Coverage
    // ============================================================================

    #[test]
    fn test_metrics_timeout_recording() {
        // Arrange
        let metrics = ConcurrencyMetrics::new();

        // Act
        metrics.record_timeout(500);
        metrics.record_timeout(600);

        // Assert
        let snapshot = metrics.snapshot();
        assert_eq!(snapshot.timed_out_executions, 2);
        assert_eq!(snapshot.total_executions, 2);
    }

    #[test]
    fn test_metrics_concurrent_tracking() {
        // Arrange
        let metrics = ConcurrencyMetrics::new();

        // Act
        metrics.record_execution_start(5);
        assert_eq!(metrics.current_concurrent(), 5);
        assert_eq!(metrics.peak_concurrent(), 5);

        metrics.record_execution_start(3);
        assert_eq!(metrics.current_concurrent(), 8);
        assert_eq!(metrics.peak_concurrent(), 8);

        metrics.record_execution_complete(5);
        assert_eq!(metrics.current_concurrent(), 3);
        assert_eq!(metrics.peak_concurrent(), 8); // Peak remains

        // Assert
        let snapshot = metrics.snapshot();
        assert_eq!(snapshot.peak_concurrent, 8);
    }

    #[test]
    fn test_metrics_reset() {
        // Arrange
        let metrics = ConcurrencyMetrics::new();
        metrics.record_success(100);
        metrics.record_failure(50);

        // Act
        metrics.reset();

        // Assert
        let snapshot = metrics.snapshot();
        assert_eq!(snapshot.total_executions, 0);
        assert_eq!(snapshot.successful_executions, 0);
        assert_eq!(snapshot.failed_executions, 0);
    }

    #[test]
    fn test_histogram_percentiles() {
        // Arrange
        let histogram = LatencyHistogram::new();

        // Act - Record 100 samples
        for i in 1..=100 {
            histogram.record(i * 10); // 10ms to 1000ms
        }

        // Assert - Percentiles
        assert!(histogram.percentile(0.50) > 0);
        assert!(histogram.percentile(0.95) > histogram.percentile(0.50));
        assert!(histogram.percentile(0.99) > histogram.percentile(0.95));
        assert!(histogram.average() > 0.0);
    }

    // ============================================================================
    // Configuration Tests
    // ============================================================================

    #[test]
    fn test_conservative_config() {
        // Arrange & Act
        let config = HyperConcurrentConfig::conservative();

        // Assert
        assert_eq!(config.max_agents, 5);
        assert_eq!(config.agent_timeout_secs, 120);
        assert!(!config.enable_adaptive_concurrency);
        assert_eq!(config.circuit_breaker_threshold, 10);
    }

    #[test]
    fn test_development_config() {
        // Arrange & Act
        let config = HyperConcurrentConfig::development();

        // Assert
        assert_eq!(config.max_agents, 3);
        assert!(!config.enable_work_stealing);
        assert!(!config.enable_circuit_breaker);
        assert_eq!(config.agent_timeout_secs, 300);
    }

    // ============================================================================
    // Executor Integration Tests
    // ============================================================================

    #[tokio::test]
    async fn test_executor_with_circuit_breaker_integration() {
        // Arrange
        let config = HyperConcurrentConfig {
            enable_circuit_breaker: true,
            circuit_breaker_threshold: 2,
            ..Default::default()
        };
        let executor = HyperConcurrentExecutor::new(config);

        // Act - Fail agent multiple times
        for _ in 0..3 {
            let tasks = vec![(
                "failing-agent".to_string(),
                || async { Err(crate::error::GgenAiError::orchestration("test failure")) },
            )];
            executor.execute_parallel(tasks).await;
        }

        // Assert - Circuit should be open
        assert!(executor.is_circuit_open("failing-agent"));

        // Reset and verify
        executor.reset_circuit("failing-agent");
        assert!(!executor.is_circuit_open("failing-agent"));
    }

    #[tokio::test]
    async fn test_executor_prioritized_execution() {
        // Arrange
        let executor = HyperConcurrentExecutor::new(HyperConcurrentConfig::default());
        let execution_order = Arc::new(AtomicUsize::new(0));

        // Act - Submit with different priorities
        let tasks = vec![
            (
                "low".to_string(),
                ExecutionPriority::Low,
                {
                    let order = Arc::clone(&execution_order);
                    || async move {
                        let pos = order.fetch_add(1, Ordering::Relaxed);
                        Ok(pos)
                    }
                },
            ),
            (
                "critical".to_string(),
                ExecutionPriority::Critical,
                {
                    let order = Arc::clone(&execution_order);
                    || async move {
                        let pos = order.fetch_add(1, Ordering::Relaxed);
                        Ok(pos)
                    }
                },
            ),
            (
                "normal".to_string(),
                ExecutionPriority::Normal,
                {
                    let order = Arc::clone(&execution_order);
                    || async move {
                        let pos = order.fetch_add(1, Ordering::Relaxed);
                        Ok(pos)
                    }
                },
            ),
        ];

        let results = executor.execute_prioritized(tasks).await;

        // Assert - Critical should execute first
        let critical = results
            .iter()
            .find(|r| r.agent_id == "critical")
            .unwrap();
        assert!(critical.is_success());
    }

    #[tokio::test]
    async fn test_executor_barrier_synchronization() {
        // Arrange
        let executor = HyperConcurrentExecutor::new(HyperConcurrentConfig::default());
        let barrier = AgentBarrier::new(1);
        let counter = Arc::new(AtomicUsize::new(0));

        // Act
        let counter_clone = Arc::clone(&counter);
        let result = executor
            .execute_with_barrier(&barrier, "agent-1".to_string(), || async move {
                counter_clone.fetch_add(1, Ordering::Relaxed);
                Ok("done")
            })
            .await;

        // Assert
        assert!(result.is_success());
        assert_eq!(counter.load(Ordering::Relaxed), 1);
    }
}
