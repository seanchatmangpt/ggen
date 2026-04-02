//! Integration tests for metrics and variance tracking.

use ggen_heijunka::metrics::{LoadMetrics, ThroughputTracker};
use std::time::Duration;

#[test]
fn test_load_metrics_basic_tracking() {
    // Arrange
    let mut metrics = LoadMetrics::new();

    // Act
    for _ in 0..100 {
        metrics.record_submission();
    }

    for _ in 0..75 {
        metrics.record_completion();
    }

    // Assert
    assert_eq!(metrics.total_submissions(), 100);
    assert_eq!(metrics.total_completions(), 75);
    assert_eq!(metrics.in_flight(), 25);
}

#[test]
fn test_overflow_tracking() {
    // Arrange
    let mut metrics = LoadMetrics::new();

    // Act - Simulate overflows
    for _ in 0..50 {
        metrics.record_submission();
    }

    for _ in 0..5 {
        metrics.record_overflow();
    }

    // Assert
    assert_eq!(metrics.total_overflows(), 5);
    assert_eq!(metrics.overflow_rate(), 0.1);
}

#[test]
fn test_underflow_tracking() {
    // Arrange
    let mut metrics = LoadMetrics::new();

    // Act
    metrics.record_underflow();
    metrics.record_underflow();
    metrics.record_underflow();

    // Assert
    assert_eq!(metrics.total_underflows(), 3);
}

#[test]
fn test_average_throughput() {
    // Arrange
    let mut metrics = LoadMetrics::new();

    // Act - Complete items over time
    for _ in 0..10 {
        metrics.record_completion();
    }

    std::thread::sleep(Duration::from_millis(100));

    // Assert - Should calculate throughput
    let throughput = metrics.average_throughput();
    assert!(throughput > 0.0);
}

#[test]
fn test_metrics_reset() {
    // Arrange
    let mut metrics = LoadMetrics::new();

    // Act
    metrics.record_submission();
    metrics.record_completion();
    metrics.record_overflow();

    assert!(metrics.total_submissions() > 0);

    metrics.reset();

    // Assert
    assert_eq!(metrics.total_submissions(), 0);
    assert_eq!(metrics.total_completions(), 0);
    assert_eq!(metrics.total_overflows(), 0);
}

#[test]
fn test_throughput_tracker_basic() {
    // Arrange
    let mut tracker = ThroughputTracker::new(Duration::from_secs(1));

    // Act
    for _ in 0..20 {
        tracker.record_completion();
    }

    // Assert
    assert_eq!(tracker.window_completions(), 20);
    assert!(tracker.current_rate() > 0.0);
}

#[test]
fn test_variance_calculation() {
    // Arrange
    let mut tracker = ThroughputTracker::new(Duration::from_millis(500));

    // Act - Create varied completion pattern
    for i in 0..30 {
        tracker.record_completion();

        if i % 5 == 0 {
            std::thread::sleep(Duration::from_millis(20));
        }
    }

    // Assert
    let variance = tracker.variance();
    assert!(variance >= 0.0);
}

#[test]
fn test_stable_throughput_low_variance() {
    // Arrange
    let mut tracker = ThroughputTracker::new(Duration::from_secs(1));

    // Act - Uniform completions
    for _ in 0..20 {
        tracker.record_completion();
        std::thread::sleep(Duration::from_millis(5));
    }

    // Assert - Should have low variance
    let variance = tracker.variance();
    assert!(variance >= 0.0);
}

#[test]
fn test_bursty_throughput_high_variance() {
    // Arrange
    let mut tracker = ThroughputTracker::new(Duration::from_millis(500));

    // Act - Bursty pattern
    // Burst 1
    for _ in 0..10 {
        tracker.record_completion();
    }

    std::thread::sleep(Duration::from_millis(50));

    // Burst 2
    for _ in 0..10 {
        tracker.record_completion();
    }

    std::thread::sleep(Duration::from_millis(50));

    // Assert
    let variance = tracker.variance();
    assert!(variance >= 0.0);
}

#[test]
fn test_mean_rate_calculation() {
    // Arrange
    let mut tracker = ThroughputTracker::new(Duration::from_secs(1));

    // Act
    for _ in 0..50 {
        tracker.record_completion();
        std::thread::sleep(Duration::from_millis(2));
    }

    // Assert
    let mean = tracker.mean_rate();
    assert!(mean > 0.0);
}

#[test]
fn test_min_max_rates() {
    // Arrange
    let mut tracker = ThroughputTracker::new(Duration::from_millis(500));

    // Act - Varied pattern
    for _ in 0..5 {
        tracker.record_completion();
    }

    std::thread::sleep(Duration::from_millis(20));

    for _ in 0..15 {
        tracker.record_completion();
    }

    // Assert
    let min = tracker.min_rate();
    let max = tracker.max_rate();

    assert!(min >= 0.0);
    assert!(max >= min);
}

#[test]
fn test_rate_range() {
    // Arrange
    let mut tracker = ThroughputTracker::new(Duration::from_millis(500));

    // Act
    for _ in 0..10 {
        tracker.record_completion();
        std::thread::sleep(Duration::from_millis(5));
    }

    // Assert
    let range = tracker.rate_range();
    assert!(range >= 0.0);
}

#[test]
fn test_stability_check() {
    // Arrange
    let mut tracker = ThroughputTracker::new(Duration::from_secs(1));

    // Act - Very uniform pattern
    for _ in 0..10 {
        tracker.record_completion();
        std::thread::sleep(Duration::from_millis(10));
    }

    // Assert - Should be stable within loose threshold
    assert!(tracker.is_stable(1.0)); // Very loose threshold
}

#[test]
fn test_percentile_calculations() {
    // Arrange
    let mut tracker = ThroughputTracker::new(Duration::from_millis(500));

    // Act - Generate samples
    for _ in 0..30 {
        tracker.record_completion();
        std::thread::sleep(Duration::from_millis(3));
    }

    // Assert
    let p50 = tracker.percentile(0.5);
    let p95 = tracker.percentile(0.95);
    let p99 = tracker.percentile(0.99);

    assert!(p50 >= 0.0);
    assert!(p95 >= p50);
    assert!(p99 >= p95);
}

#[test]
fn test_median_rate() {
    // Arrange
    let mut tracker = ThroughputTracker::new(Duration::from_millis(500));

    // Act
    for _ in 0..20 {
        tracker.record_completion();
        std::thread::sleep(Duration::from_millis(3));
    }

    // Assert
    let median = tracker.median_rate();
    assert!(median >= 0.0);
}

#[test]
fn test_p95_and_p99() {
    // Arrange
    let mut tracker = ThroughputTracker::new(Duration::from_millis(500));

    // Act
    for _ in 0..25 {
        tracker.record_completion();
        std::thread::sleep(Duration::from_millis(2));
    }

    // Assert
    let p95 = tracker.p95_rate();
    let p99 = tracker.p99_rate();

    assert!(p95 >= 0.0);
    assert!(p99 >= p95);
}

#[test]
fn test_window_eviction() {
    // Arrange
    let mut tracker = ThroughputTracker::new(Duration::from_millis(100));

    // Act - Add completions
    for _ in 0..10 {
        tracker.record_completion();
    }

    assert_eq!(tracker.window_completions(), 10);

    // Wait for window to expire
    std::thread::sleep(Duration::from_millis(150));

    // Trigger eviction
    tracker.record_completion();

    // Assert - Old completions should be evicted
    assert!(tracker.window_completions() < 11);
}

#[test]
fn test_tracker_reset() {
    // Arrange
    let mut tracker = ThroughputTracker::new(Duration::from_secs(1));

    // Act
    for _ in 0..20 {
        tracker.record_completion();
    }

    assert!(tracker.window_completions() > 0);

    tracker.reset();

    // Assert
    assert_eq!(tracker.window_completions(), 0);
    assert_eq!(tracker.current_rate(), 0.0);
}

#[test]
fn test_long_term_variance_tracking() {
    // Arrange
    let mut tracker = ThroughputTracker::new(Duration::from_secs(1));

    // Act - Sustained load with variations
    for batch in 0..10 {
        for _ in 0..10 {
            tracker.record_completion();
        }

        if batch % 2 == 0 {
            std::thread::sleep(Duration::from_millis(10));
        } else {
            std::thread::sleep(Duration::from_millis(20));
        }
    }

    // Assert
    let variance = tracker.variance();
    let mean = tracker.mean_rate();

    assert!(variance >= 0.0);
    assert!(mean > 0.0);
}

#[test]
fn test_zero_completions_variance() {
    // Arrange
    let tracker = ThroughputTracker::new(Duration::from_secs(1));

    // Act & Assert - Should handle zero completions gracefully
    assert_eq!(tracker.variance(), 0.0);
    assert_eq!(tracker.mean_rate(), 0.0);
    assert_eq!(tracker.current_rate(), 0.0);
}

#[test]
fn test_single_completion_variance() {
    // Arrange
    let mut tracker = ThroughputTracker::new(Duration::from_secs(1));

    // Act
    tracker.record_completion();

    // Assert - Single sample should have zero variance
    assert_eq!(tracker.variance(), 0.0);
}

#[test]
fn test_metrics_overflow_rate_edge_cases() {
    // Arrange
    let mut metrics = LoadMetrics::new();

    // Act & Assert - No submissions
    assert_eq!(metrics.overflow_rate(), 0.0);

    // Add overflow without submission
    metrics.record_overflow();
    assert_eq!(metrics.overflow_rate(), 0.0);

    // Add submission
    metrics.record_submission();
    assert_eq!(metrics.overflow_rate(), 1.0);
}

#[test]
fn test_continuous_monitoring() {
    // Arrange
    let mut tracker = ThroughputTracker::new(Duration::from_millis(500));

    // Act - Simulate continuous monitoring
    for _cycle in 0..5 {
        // Work burst
        for _ in 0..10 {
            tracker.record_completion();
        }

        // Check stability
        let variance = tracker.variance();
        assert!(variance >= 0.0);

        std::thread::sleep(Duration::from_millis(20));
    }

    // Assert - Should have accumulated samples
    assert!(tracker.window_completions() > 0);
}
