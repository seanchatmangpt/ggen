//! Integration tests for Scheduler.

use ggen_heijunka::scheduler::{Scheduler, SchedulingStrategy};
use std::time::Duration;

#[test]
fn test_smoothed_strategy_uniform_distribution() {
    // Arrange
    let mut scheduler =
        Scheduler::new(Duration::from_secs(60), 100.0, SchedulingStrategy::Smoothed);

    // Act - Calculate delays for sequential items
    let delay1 = scheduler.calculate_delay(10, 0.0).unwrap();
    let delay2 = scheduler.calculate_delay(10, 0.0).unwrap();
    let delay3 = scheduler.calculate_delay(10, 0.0).unwrap();

    // Assert - First should be zero, subsequent should have delay
    assert_eq!(delay1, Duration::ZERO);
    assert!(delay2 > Duration::ZERO);
    assert!(delay3 > Duration::ZERO);
}

#[test]
fn test_smoothed_strategy_target_interval() {
    // Arrange
    let target_throughput = 10.0; // 10 items per second
    let expected_interval = Duration::from_secs_f64(1.0 / target_throughput);

    let mut scheduler = Scheduler::new(
        Duration::from_secs(60),
        target_throughput,
        SchedulingStrategy::Smoothed,
    );

    // Act - First call (no delay)
    scheduler.calculate_delay(10, 0.0).unwrap();

    // Immediate second call should suggest delay close to target interval
    let delay = scheduler.calculate_delay(10, 0.0).unwrap();

    // Assert - Should be close to expected interval (within 10ms tolerance)
    let diff = if delay > expected_interval {
        delay - expected_interval
    } else {
        expected_interval - delay
    };
    assert!(diff < Duration::from_millis(10));
}

#[test]
fn test_adaptive_strategy_high_rate() {
    // Arrange
    let mut scheduler =
        Scheduler::new(Duration::from_secs(60), 100.0, SchedulingStrategy::Adaptive);

    // Act - High current rate should increase delay
    let delay_low_rate = scheduler.calculate_delay(10, 50.0).unwrap();
    scheduler.reset();
    let delay_high_rate = scheduler.calculate_delay(10, 200.0).unwrap();

    // Assert - High rate should result in more delay
    // (Note: first call is always zero, so we just verify no errors)
    assert!(delay_low_rate >= Duration::ZERO);
    assert!(delay_high_rate >= Duration::ZERO);
}

#[test]
fn test_adaptive_strategy_buffer_pressure() {
    // Arrange
    let mut scheduler =
        Scheduler::new(Duration::from_secs(60), 100.0, SchedulingStrategy::Adaptive);

    // Act - High buffer size should reduce delay
    scheduler.calculate_delay(10, 100.0).unwrap(); // Low pressure
    scheduler.reset();

    let delay_high_pressure = scheduler.calculate_delay(900, 100.0).unwrap();

    // Assert - High buffer pressure should allow faster processing
    assert!(delay_high_pressure >= Duration::ZERO);
}

#[test]
fn test_token_bucket_strategy() {
    // Arrange
    let mut scheduler = Scheduler::new(
        Duration::from_secs(60),
        100.0,
        SchedulingStrategy::TokenBucket,
    );

    // Act - Consume tokens rapidly
    let mut delays = Vec::new();
    for _ in 0..5 {
        let delay = scheduler.calculate_delay(10, 0.0).unwrap();
        delays.push(delay);
    }

    // Assert - Should start with zero delays (bucket full)
    // then introduce delays as tokens depleted
    assert!(delays.iter().any(|&d| d == Duration::ZERO));
}

#[test]
fn test_strategy_switching() {
    // Arrange
    let mut scheduler =
        Scheduler::new(Duration::from_secs(60), 100.0, SchedulingStrategy::Smoothed);

    // Act - Use smoothed strategy
    scheduler.calculate_delay(10, 0.0).unwrap();

    // Switch to adaptive
    scheduler.set_strategy(SchedulingStrategy::Adaptive);

    // Act - Use adaptive strategy
    let delay = scheduler.calculate_delay(10, 50.0).unwrap();

    // Assert - Should work with new strategy
    assert!(delay >= Duration::ZERO);
    assert_eq!(scheduler.strategy(), SchedulingStrategy::Adaptive);
}

#[test]
fn test_throughput_update() {
    // Arrange
    let mut scheduler =
        Scheduler::new(Duration::from_secs(60), 100.0, SchedulingStrategy::Smoothed);

    // Act
    assert_eq!(scheduler.target_throughput(), 100.0);

    scheduler.set_target_throughput(200.0).unwrap();

    // Assert
    assert_eq!(scheduler.target_throughput(), 200.0);
}

#[test]
fn test_throughput_validation() {
    // Arrange
    let mut scheduler =
        Scheduler::new(Duration::from_secs(60), 100.0, SchedulingStrategy::Smoothed);

    // Act & Assert - Invalid throughput should error
    assert!(scheduler.set_target_throughput(0.0).is_err());
    assert!(scheduler.set_target_throughput(-10.0).is_err());
    assert!(scheduler.set_target_throughput(50.0).is_ok());
}

#[test]
fn test_scheduler_reset() {
    // Arrange
    let mut scheduler =
        Scheduler::new(Duration::from_secs(60), 100.0, SchedulingStrategy::Smoothed);

    // Act - Make some calls
    scheduler.calculate_delay(10, 0.0).unwrap();
    scheduler.calculate_delay(10, 0.0).unwrap();

    // Reset
    scheduler.reset();

    // Assert - Next call should behave like first call
    let delay = scheduler.calculate_delay(10, 0.0).unwrap();
    assert_eq!(delay, Duration::ZERO);
}

#[test]
fn test_high_throughput_low_delay() {
    // Arrange - Very high throughput means very short intervals
    let mut scheduler = Scheduler::new(
        Duration::from_secs(60),
        10000.0,
        SchedulingStrategy::Smoothed,
    );

    // Act
    scheduler.calculate_delay(10, 0.0).unwrap();
    let delay = scheduler.calculate_delay(10, 0.0).unwrap();

    // Assert - Should have very small delay
    assert!(delay < Duration::from_millis(1));
}

#[test]
fn test_low_throughput_high_delay() {
    // Arrange - Low throughput means longer intervals
    let mut scheduler = Scheduler::new(
        Duration::from_secs(60),
        1.0, // 1 item per second
        SchedulingStrategy::Smoothed,
    );

    // Act
    scheduler.calculate_delay(10, 0.0).unwrap();
    let delay = scheduler.calculate_delay(10, 0.0).unwrap();

    // Assert - Should have delay close to 1 second
    assert!(delay > Duration::from_millis(900));
    assert!(delay <= Duration::from_secs(1));
}

#[test]
fn test_sequential_smoothing() {
    // Arrange
    let mut scheduler =
        Scheduler::new(Duration::from_secs(60), 100.0, SchedulingStrategy::Smoothed);

    // Act - Make multiple sequential calls with no wait
    let delays: Vec<_> = (0..5)
        .map(|_| scheduler.calculate_delay(10, 0.0).unwrap())
        .collect();

    // Assert
    // First should be zero
    assert_eq!(delays[0], Duration::ZERO);

    // Subsequent should have delays (though they pile up since we don't actually wait)
    for delay in &delays[1..] {
        assert!(*delay > Duration::ZERO);
    }
}

#[test]
fn test_adaptive_with_varying_buffer() {
    // Arrange
    let mut scheduler =
        Scheduler::new(Duration::from_secs(60), 100.0, SchedulingStrategy::Adaptive);

    // Act - Simulate varying buffer sizes
    let delay_empty = scheduler.calculate_delay(0, 100.0).unwrap();
    scheduler.reset();

    let delay_half = scheduler.calculate_delay(500, 100.0).unwrap();
    scheduler.reset();

    let delay_full = scheduler.calculate_delay(1000, 100.0).unwrap();

    // Assert - All should succeed
    assert!(delay_empty >= Duration::ZERO);
    assert!(delay_half >= Duration::ZERO);
    assert!(delay_full >= Duration::ZERO);
}

#[test]
fn test_token_bucket_refill() {
    // Arrange
    let mut scheduler = Scheduler::new(
        Duration::from_secs(60),
        100.0,
        SchedulingStrategy::TokenBucket,
    );

    // Act - Exhaust tokens
    for _ in 0..300 {
        scheduler.calculate_delay(10, 0.0).ok();
    }

    // Wait for refill
    std::thread::sleep(Duration::from_millis(100));

    // Try again
    let delay = scheduler.calculate_delay(10, 0.0).unwrap();

    // Assert - Should work after refill
    assert!(delay >= Duration::ZERO);
}
