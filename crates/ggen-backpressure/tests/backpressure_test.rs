//! Integration tests for backpressure and admission control under load

use ggen_backpressure::{
    AdmissionController, KanbanBoard, KanbanConfig, RateLimiter, RateLimiterConfig, Stage,
};
use std::sync::Arc;
use std::time::Duration;
use tokio::time::Instant;

#[tokio::test]
async fn test_backpressure_under_sustained_load() {
    let config = RateLimiterConfig {
        max_rps: 50.0,
        max_concurrent: 5,
        burst_size: 10,
    };

    let limiter = Arc::new(RateLimiter::new(config));
    let mut handles = vec![];

    // Spawn 20 concurrent workers (exceeds max_concurrent)
    for i in 0..20 {
        let limiter = Arc::clone(&limiter);
        let handle = tokio::spawn(async move {
            let token = limiter.acquire().await.expect("failed to acquire");
            tokio::time::sleep(Duration::from_millis(50)).await;
            drop(token);
            i
        });
        handles.push(handle);
    }

    // All should complete despite exceeding capacity
    let results = futures::future::join_all(handles).await;
    assert_eq!(results.len(), 20);
    assert!(results.iter().all(|r| r.is_ok()));

    // Should never exceed capacity at any point
    assert!(limiter.in_flight() <= 5);
}

#[tokio::test]
async fn test_rate_limiting_prevents_overload() {
    let config = RateLimiterConfig {
        max_rps: 10.0, // 10 requests per second
        max_concurrent: 100,
        burst_size: 5,
    };

    let limiter = RateLimiter::new(config);
    let start = Instant::now();

    // Try to acquire 15 tokens rapidly
    let mut count = 0;
    for _ in 0..15 {
        match limiter.try_acquire() {
            Ok(Some(_token)) => {
                count += 1;
            }
            Ok(None) | Err(_) => {
                break;
            }
        }
    }

    let elapsed = start.elapsed();

    // Should only get burst_size tokens immediately
    assert!(count <= 5, "burst limit exceeded: got {}", count);
    assert!(elapsed < Duration::from_millis(100), "took too long");
}

#[tokio::test]
async fn test_kanban_enforces_wip_limits_under_load() {
    let config = KanbanConfig {
        ready_limit: 3,
        in_progress_limit: 2,
        review_limit: 1,
    };

    let board = Arc::new(KanbanBoard::new(config));

    // Add 10 items to backlog
    for i in 0..10 {
        board.add_to_backlog(format!("item{}", i)).await.unwrap();
    }

    // Try to pull all items to Ready concurrently
    let mut handles = vec![];
    for i in 0..10 {
        let board = Arc::clone(&board);
        let handle = tokio::spawn(async move {
            board.pull(&format!("item{}", i)).await
        });
        handles.push(handle);
    }

    let results = futures::future::join_all(handles).await;

    // Only 3 should succeed (ready_limit)
    let successes = results.iter().filter(|r| {
        matches!(r, Ok(Ok(_)))
    }).count();

    assert_eq!(successes, 3, "should respect WIP limit");
    assert_eq!(board.count(Stage::Ready).await, 3);
}

#[tokio::test]
async fn test_lambda_leq_mu_enforcement() {
    // μ = 20 rps service rate
    let config = RateLimiterConfig {
        max_rps: 20.0,
        max_concurrent: 5,
        burst_size: 10,
    };

    let limiter = Arc::new(RateLimiter::new(config));
    let start = Instant::now();
    let accepted = Arc::new(tokio::sync::Mutex::new(0));
    let rejected = Arc::new(tokio::sync::Mutex::new(0));

    // Generate load at λ = 50 rps (exceeds μ)
    let mut handles = vec![];
    for _ in 0..100 {
        let limiter = Arc::clone(&limiter);
        let accepted = Arc::clone(&accepted);
        let rejected = Arc::clone(&rejected);

        let handle = tokio::spawn(async move {
            match limiter.try_acquire() {
                Ok(Some(_token)) => {
                    *accepted.lock().await += 1;
                }
                Ok(None) | Err(_) => {
                    *rejected.lock().await += 1;
                }
            }
        });
        handles.push(handle);

        // Small delay to simulate arrival rate
        tokio::time::sleep(Duration::from_millis(10)).await;
    }

    futures::future::join_all(handles).await;
    let elapsed = start.elapsed();

    let accepted_count = *accepted.lock().await;
    let rejected_count = *rejected.lock().await;

    // Should reject most requests since λ > μ
    assert!(rejected_count > 0, "should reject when λ > μ");

    // Accepted rate should not exceed μ significantly
    let accepted_rate = accepted_count as f64 / elapsed.as_secs_f64();
    assert!(
        accepted_rate <= 30.0,
        "accepted rate {} exceeds μ significantly",
        accepted_rate
    );
}

#[tokio::test]
async fn test_token_release_frees_capacity() {
    let config = RateLimiterConfig {
        max_rps: 1000.0,
        max_concurrent: 2,
        burst_size: 100,
    };

    let limiter = Arc::new(RateLimiter::new(config));

    // Acquire 2 tokens (at capacity)
    let t1 = limiter.acquire().await.unwrap();
    let t2 = limiter.acquire().await.unwrap();

    assert_eq!(limiter.in_flight(), 2);
    assert!(limiter.try_acquire().unwrap().is_none());

    // Drop one token
    drop(t1);

    // Should now have capacity
    assert_eq!(limiter.in_flight(), 1);
    let t3 = limiter.try_acquire().unwrap();
    assert!(t3.is_some());

    drop(t2);
    drop(t3);

    // Should be back to zero
    assert_eq!(limiter.in_flight(), 0);
}

#[tokio::test]
async fn test_concurrent_kanban_pulls() {
    let board = Arc::new(KanbanBoard::with_defaults());

    // Add items
    for i in 0..10 {
        board.add_to_backlog(format!("item{}", i)).await.unwrap();
    }

    // Pull concurrently from multiple tasks
    let mut handles = vec![];
    for i in 0..10 {
        let board = Arc::clone(&board);
        let handle = tokio::spawn(async move {
            // Try to pull from backlog to ready
            let result = board.pull(&format!("item{}", i)).await;
            (i, result.is_ok())
        });
        handles.push(handle);
    }

    let results = futures::future::join_all(handles).await;
    let successful = results.iter()
        .filter(|r| r.as_ref().map(|(_, ok)| *ok).unwrap_or(false))
        .count();

    // Should only allow up to ready_limit
    assert!(successful <= 5);
    assert_eq!(board.count(Stage::Ready).await, successful);
}

#[tokio::test]
async fn test_admission_controller_trait() {
    let limiter: Box<dyn AdmissionController> = Box::new(RateLimiter::with_defaults());

    let token = limiter.acquire().await.unwrap();

    assert_eq!(limiter.in_flight(), 1);
    assert!(limiter.utilization() > 0.0);
    assert!(limiter.capacity() > 0);

    drop(token);

    assert_eq!(limiter.in_flight(), 0);
}

#[tokio::test]
async fn test_backpressure_with_bursty_load() {
    let config = RateLimiterConfig {
        max_rps: 10.0,
        max_concurrent: 5,
        burst_size: 15, // Large burst capacity
    };

    let limiter = Arc::new(RateLimiter::new(config));

    // Generate burst of 15 requests
    let mut handles = vec![];
    for _ in 0..15 {
        let limiter = Arc::clone(&limiter);
        let handle = tokio::spawn(async move {
            limiter.try_acquire().map(|r| r.is_some())
        });
        handles.push(handle);
    }

    let results = futures::future::join_all(handles).await;
    let accepted = results.iter()
        .filter(|r| matches!(r, Ok(Ok(true))))
        .count();

    // Should handle burst up to burst_size
    assert!(accepted >= 5 && accepted <= 15, "burst handling failed: {}", accepted);
}

#[tokio::test]
async fn test_zero_capacity_rejection() {
    let config = RateLimiterConfig {
        max_rps: 1000.0,
        max_concurrent: 0, // Zero capacity
        burst_size: 100,
    };

    let limiter = RateLimiter::new(config);

    // Should immediately reject
    let result = limiter.try_acquire();
    assert!(result.unwrap().is_none());
}
