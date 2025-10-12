//! Examples demonstrating async execution patterns
//!
//! This example shows how to use the AsyncExecutor with various async patterns,
//! including timeout handling, cancellation, and concurrent execution.

use cleanroom::executor::{AsyncExecutor, ExecutionContext};
use cleanroom::executor::timeout::{TimeoutStrategy, RetryConfig, TimeoutExecutor, CircuitBreaker};
use cleanroom::builder::CleanroomBuilder;
use std::time::Duration;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("Cleanroom Async Patterns Examples");
    println!("==================================");

    // Create environment using builder
    let environment = CleanroomBuilder::new()
        .with_timeout(Duration::from_secs(60))
        .build()
        .await?;

    // Example 1: Basic async execution
    println!("\n1. Basic Async Execution");
    let executor = AsyncExecutor::new(environment);
    
    let result = executor.run_async(async {
        tokio::time::sleep(Duration::from_millis(100)).await;
        Ok::<i32, cleanroom::error::CleanroomError>(42)
    }).await?;
    
    println!("✓ Result: {}, Duration: {:?}", result.result, result.duration);

    // Example 2: Execution with timeout
    println!("\n2. Execution with Timeout");
    let result = executor.run_async_with_timeout(
        async {
            tokio::time::sleep(Duration::from_millis(50)).await;
            Ok::<String, cleanroom::error::CleanroomError>("Hello World".to_string())
        },
        Duration::from_millis(100)
    ).await?;
    
    println!("✓ Result: {}, Duration: {:?}", result.result, result.duration);

    // Example 3: Execution context with custom settings
    println!("\n3. Execution Context");
    let result = executor.context()
        .with_timeout(Duration::from_millis(200))
        .run_async(async {
            tokio::time::sleep(Duration::from_millis(50)).await;
            Ok::<Vec<i32>, cleanroom::error::CleanroomError>(vec![1, 2, 3])
        })
        .await?;
    
    println!("✓ Result: {:?}, Duration: {:?}", result.result, result.duration);

    // Example 4: Concurrent execution
    println!("\n4. Concurrent Execution");
    let futures = vec![
        async { 
            tokio::time::sleep(Duration::from_millis(100)).await;
            Ok::<i32, cleanroom::error::CleanroomError>(1) 
        },
        async { 
            tokio::time::sleep(Duration::from_millis(150)).await;
            Ok::<i32, cleanroom::error::CleanroomError>(2) 
        },
        async { 
            tokio::time::sleep(Duration::from_millis(200)).await;
            Ok::<i32, cleanroom::error::CleanroomError>(3) 
        },
    ];
    
    let results = executor.context()
        .run_concurrent(futures)
        .await?;
    
    println!("✓ Concurrent results:");
    for (i, result) in results.iter().enumerate() {
        println!("  Task {}: {}, Duration: {:?}", i + 1, result.result, result.duration);
    }

    // Example 5: Test execution wrapper
    println!("\n5. Test Execution Wrapper");
    let result = executor.context()
        .execute_test("example_test", async {
            tokio::time::sleep(Duration::from_millis(50)).await;
            Ok::<bool, cleanroom::error::CleanroomError>(true)
        })
        .await?;
    
    println!("✓ Test result: {}, Duration: {:?}", result.result, result.duration);

    // Example 6: Timeout strategies
    println!("\n6. Timeout Strategies");
    
    // Fixed timeout
    let fixed_strategy = TimeoutStrategy::fixed(Duration::from_millis(100));
    println!("Fixed timeout (attempt 0): {:?}", fixed_strategy.timeout_for_attempt(0));
    
    // Exponential backoff
    let exp_strategy = TimeoutStrategy::exponential_backoff(
        Duration::from_millis(50),
        Duration::from_secs(1),
        2.0,
    );
    for i in 0..5 {
        println!("Exponential backoff (attempt {}): {:?}", i, exp_strategy.timeout_for_attempt(i));
    }
    
    // Adaptive timeout
    let adaptive_strategy = TimeoutStrategy::adaptive(
        Duration::from_millis(100),
        1.5,
        Duration::from_secs(2),
    );
    for i in 0..5 {
        println!("Adaptive timeout (attempt {}): {:?}", i, adaptive_strategy.timeout_for_attempt(i));
    }

    // Example 7: Retry configuration
    println!("\n7. Retry Configuration");
    let retry_config = RetryConfig::new(3)
        .with_timeout_strategy(TimeoutStrategy::exponential_backoff(
            Duration::from_millis(100),
            Duration::from_secs(1),
            2.0,
        ))
        .with_retry_delay(Duration::from_millis(50))
        .with_retry_on_timeout(true);
    
    let timeout_executor = TimeoutExecutor::new(retry_config);
    
    let result = timeout_executor.execute_with_retry(async {
        tokio::time::sleep(Duration::from_millis(50)).await;
        Ok::<i32, cleanroom::error::CleanroomError>(42)
    }).await?;
    
    println!("✓ Retry execution result: {}", result);

    // Example 8: Circuit breaker pattern
    println!("\n8. Circuit Breaker Pattern");
    let mut circuit_breaker = CircuitBreaker::new(3, 2, Duration::from_millis(100));
    
    // Simulate some failures
    for i in 0..5 {
        let can_execute = circuit_breaker.can_execute();
        println!("Attempt {}: Can execute: {}, State: {:?}", 
                i + 1, can_execute, circuit_breaker.state());
        
        if can_execute {
            // Simulate execution
            tokio::time::sleep(Duration::from_millis(10)).await;
            
            if i < 3 {
                circuit_breaker.record_failure(Duration::from_millis(10));
            } else {
                circuit_breaker.record_success(Duration::from_millis(10));
            }
        } else {
            // Wait for circuit to potentially reset
            tokio::time::sleep(Duration::from_millis(150)).await;
        }
    }
    
    println!("Final state: {:?}, Failures: {}, Successes: {}", 
            circuit_breaker.state(), 
            circuit_breaker.failure_count(), 
            circuit_breaker.success_count());

    // Example 9: Adaptive execution based on previous duration
    println!("\n9. Adaptive Execution");
    let previous_duration = Some(Duration::from_millis(200));
    let result = timeout_executor.execute_adaptive(
        async {
            tokio::time::sleep(Duration::from_millis(100)).await;
            Ok::<String, cleanroom::error::CleanroomError>("Adaptive execution".to_string())
        },
        previous_duration,
    ).await?;
    
    println!("✓ Adaptive execution result: {}", result);

    // Example 10: Cancellation support
    println!("\n10. Cancellation Support");
    let (tx, rx) = tokio::sync::oneshot::channel();
    
    // Spawn a task that will cancel after 100ms
    let executor_clone = AsyncExecutor::new(
        CleanroomBuilder::new()
            .with_timeout(Duration::from_secs(30))
            .build()
            .await?
    );
    
    let cancel_task = tokio::spawn(async move {
        tokio::time::sleep(Duration::from_millis(100)).await;
        let _ = tx.send(());
    });
    
    let result = executor_clone.run_async_with_cancellation(async {
        tokio::time::sleep(Duration::from_millis(200)).await;
        Ok::<i32, cleanroom::error::CleanroomError>(42)
    }).await;
    
    cancel_task.await?;
    
    match result {
        Ok(r) => println!("✓ Execution completed: {}", r.result),
        Err(e) => println!("✓ Execution cancelled: {}", e),
    }

    println!("\n=== All Async Pattern Examples Completed Successfully ===");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_async_executor_examples() {
        let environment = CleanroomBuilder::new()
            .build_minimal()
            .await
            .expect("Should build environment");
        
        let executor = AsyncExecutor::new(environment);
        
        // Test basic execution
        let result = executor.run_async(async {
            Ok::<i32, cleanroom::error::CleanroomError>(42)
        }).await.expect("Should execute successfully");
        
        assert_eq!(result.result, 42);
        assert!(result.is_success());
    }

    #[tokio::test]
    async fn test_timeout_strategies() {
        let fixed = TimeoutStrategy::fixed(Duration::from_millis(100));
        assert_eq!(fixed.timeout_for_attempt(0), Duration::from_millis(100));
        
        let exp = TimeoutStrategy::exponential_backoff(
            Duration::from_millis(50),
            Duration::from_secs(1),
            2.0,
        );
        assert_eq!(exp.timeout_for_attempt(0), Duration::from_millis(50));
        assert_eq!(exp.timeout_for_attempt(1), Duration::from_millis(100));
        assert_eq!(exp.timeout_for_attempt(2), Duration::from_millis(200));
    }

    #[tokio::test]
    async fn test_circuit_breaker() {
        let mut breaker = CircuitBreaker::new(2, 1, Duration::from_millis(50));
        
        assert!(breaker.can_execute());
        breaker.record_failure(Duration::from_millis(10));
        assert!(breaker.can_execute());
        breaker.record_failure(Duration::from_millis(10));
        assert!(!breaker.can_execute());
        
        tokio::time::sleep(Duration::from_millis(100)).await;
        assert!(breaker.can_execute());
        
        breaker.record_success(Duration::from_millis(10));
        assert!(breaker.can_execute());
    }

    #[tokio::test]
    async fn test_concurrent_execution() {
        let environment = CleanroomBuilder::new()
            .build_minimal()
            .await
            .expect("Should build environment");
        
        let executor = AsyncExecutor::new(environment);
        
        let futures = vec![
            async { Ok::<i32, cleanroom::error::CleanroomError>(1) },
            async { Ok::<i32, cleanroom::error::CleanroomError>(2) },
            async { Ok::<i32, cleanroom::error::CleanroomError>(3) },
        ];
        
        let results = executor.context()
            .run_concurrent(futures)
            .await
            .expect("Should execute concurrently");
        
        assert_eq!(results.len(), 3);
        assert_eq!(results[0].result, 1);
        assert_eq!(results[1].result, 2);
        assert_eq!(results[2].result, 3);
    }
}
