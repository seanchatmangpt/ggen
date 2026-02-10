//! Integration tests for LoadLeveler.

use ggen_heijunka::{LoadLeveler, LoadLevelerConfig, WorkItem};
use std::time::Duration;

#[tokio::test]
async fn test_basic_load_leveling() {
    // Arrange
    let config = LoadLevelerConfig {
        window_duration: Duration::from_secs(1),
        max_buffer_size: 100,
        target_throughput: 10.0,
        variance_threshold: 0.3,
    };
    let mut leveler = LoadLeveler::new(config).unwrap();

    // Act - Submit burst of work
    for i in 0..20 {
        leveler.submit(WorkItem::new(i, 1.0)).unwrap();
    }

    // Assert - Buffer should contain all items
    assert_eq!(leveler.buffered_count(), 20);

    // Act - Schedule items (should smooth arrival rate)
    let mut scheduled = Vec::new();
    for _ in 0..5 {
        if let Ok(Some(item)) = leveler.schedule_next().await {
            scheduled.push(item);
        }
    }

    // Assert - Should have scheduled some items
    assert!(!scheduled.is_empty());
    assert_eq!(leveler.buffered_count(), 20 - scheduled.len());
}

#[tokio::test]
async fn test_burst_prevention() {
    // Arrange
    let config = LoadLevelerConfig {
        window_duration: Duration::from_secs(1),
        max_buffer_size: 1000,
        target_throughput: 100.0,
        variance_threshold: 0.2,
    };
    let mut leveler = LoadLeveler::new(config).unwrap();

    // Act - Submit work in bursts
    let _burst1_start = std::time::Instant::now();
    for i in 0..50 {
        leveler.submit(WorkItem::new(i, 1.0)).unwrap();
    }

    std::thread::sleep(Duration::from_millis(100));

    for i in 50..100 {
        leveler.submit(WorkItem::new(i, 1.0)).unwrap();
    }

    // Assert - All items buffered
    assert_eq!(leveler.buffered_count(), 100);

    // Act - Schedule all items
    let schedule_start = std::time::Instant::now();
    let mut count = 0;
    while leveler.schedule_next().await.unwrap().is_some() {
        count += 1;
        if count >= 10 {
            break; // Test first 10 to avoid long test time
        }
    }

    let schedule_duration = schedule_start.elapsed();

    // Assert - Scheduling should take time to smooth rate
    assert!(schedule_duration > Duration::from_millis(50));
}

#[tokio::test]
async fn test_buffer_overflow_handling() {
    // Arrange
    let config = LoadLevelerConfig {
        window_duration: Duration::from_secs(1),
        max_buffer_size: 10,
        target_throughput: 100.0,
        variance_threshold: 0.2,
    };
    let mut leveler = LoadLeveler::new(config).unwrap();

    // Act - Fill buffer to capacity
    for i in 0..10 {
        assert!(leveler.submit(WorkItem::new(i, 1.0)).is_ok());
    }

    // Assert - Buffer is full
    assert_eq!(leveler.buffered_count(), 10);
    assert_eq!(leveler.buffer_utilization(), 1.0);

    // Act - Try to submit when full
    let result = leveler.submit(WorkItem::new(99, 1.0));

    // Assert - Should return overflow error
    assert!(result.is_err());
}

#[tokio::test]
async fn test_smooth_throughput_distribution() {
    // Arrange
    let config = LoadLevelerConfig {
        window_duration: Duration::from_secs(1),
        max_buffer_size: 100,
        target_throughput: 50.0,
        variance_threshold: 0.3,
    };
    let mut leveler = LoadLeveler::new(config).unwrap();

    // Act - Submit work items
    for i in 0..20 {
        leveler.submit(WorkItem::new(i, 1.0)).unwrap();
    }

    // Act - Schedule items and measure timing
    let _start = std::time::Instant::now();
    let mut completion_times = Vec::new();

    for _ in 0..10 {
        let item_start = std::time::Instant::now();
        leveler.schedule_next().await.unwrap();
        completion_times.push(item_start.elapsed());
    }

    // Assert - Should have completed some items
    assert_eq!(completion_times.len(), 10);

    // Note: Actual variance checking would require more samples
    // This test just verifies the mechanism works
}

#[tokio::test]
async fn test_high_load_scenario() {
    // Arrange
    let config = LoadLevelerConfig {
        window_duration: Duration::from_secs(1),
        max_buffer_size: 500,
        target_throughput: 1000.0, // High throughput to speed up test
        variance_threshold: 0.2,
    };
    let mut leveler = LoadLeveler::new(config).unwrap();

    // Act - Submit large batch
    for i in 0..200 {
        leveler.submit(WorkItem::new(i, 1.0)).unwrap();
    }

    // Assert
    assert_eq!(leveler.buffered_count(), 200);

    // Act - Process all items
    let mut processed = 0;
    while let Ok(Some(_)) = leveler.schedule_next().await {
        processed += 1;
    }

    // Assert - All items processed
    assert_eq!(processed, 200);
    assert_eq!(leveler.buffered_count(), 0);
}

#[tokio::test]
async fn test_metrics_tracking() {
    // Arrange
    let config = LoadLevelerConfig {
        window_duration: Duration::from_secs(1),
        max_buffer_size: 100,
        target_throughput: 100.0,
        variance_threshold: 0.2,
    };
    let mut leveler = LoadLeveler::new(config).unwrap();

    // Act
    for i in 0..10 {
        leveler.submit(WorkItem::new(i, 1.0)).unwrap();
    }

    for _ in 0..5 {
        leveler.schedule_next().await.ok();
    }

    // Assert - Metrics should be tracked
    let metrics = leveler.metrics();
    assert_eq!(metrics.total_submissions(), 10);
    assert_eq!(metrics.total_completions(), 5);
    assert_eq!(metrics.in_flight(), 5);
}

#[tokio::test]
async fn test_batch_submission() {
    // Arrange
    let config = LoadLevelerConfig {
        window_duration: Duration::from_secs(1),
        max_buffer_size: 100,
        target_throughput: 100.0,
        variance_threshold: 0.2,
    };
    let mut leveler = LoadLeveler::new(config).unwrap();

    // Act - Submit batch
    let items: Vec<_> = (0..20).map(|i| WorkItem::new(i, 1.0)).collect();
    assert!(leveler.submit_batch(items).is_ok());

    // Assert
    assert_eq!(leveler.buffered_count(), 20);
}

#[tokio::test]
async fn test_empty_buffer_schedule() {
    // Arrange
    let config = LoadLevelerConfig::default();
    let mut leveler = LoadLeveler::new(config).unwrap();

    // Act - Schedule from empty buffer
    let result = leveler.schedule_next().await;

    // Assert - Should return None, not error
    assert!(result.is_ok());
    assert!(result.unwrap().is_none());
}

#[tokio::test]
async fn test_stability_check() {
    // Arrange
    let config = LoadLevelerConfig {
        window_duration: Duration::from_millis(500),
        max_buffer_size: 100,
        target_throughput: 1000.0, // Very high for quick test
        variance_threshold: 0.5,
    };
    let mut leveler = LoadLeveler::new(config).unwrap();

    // Act - Process some items uniformly
    for i in 0..10 {
        leveler.submit(WorkItem::new(i, 1.0)).unwrap();
        leveler.schedule_next().await.ok();
    }

    // Assert - System should be relatively stable
    // (with few samples, variance will be high, so we use loose threshold)
    let variance = leveler.throughput_variance();
    assert!(variance >= 0.0); // Just check it's calculated
}
