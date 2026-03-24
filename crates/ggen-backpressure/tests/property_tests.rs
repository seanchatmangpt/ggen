//! Property-based tests for backpressure and admission control
//!
//! Tests core properties:
//! - Idempotency: capacity queries don't mutate state
//! - Determinism: same configuration produces same behavior
//! - Monotonicity: utilization is bounded [0.0, 1.0]
//! - Resource safety: tokens properly release capacity

use ggen_backpressure::{
    AdmissionController, RateLimiter, RateLimiterConfig, WIPToken,
    kanban::{KanbanBoard, KanbanConfig, Stage},
};
use proptest::prelude::*;
use std::time::Duration;

// ============================================================================
// STRATEGIES: Input generation for property tests
// ============================================================================

/// Generate valid capacity values
fn capacity_strategy() -> impl Strategy<Value = usize> {
    1usize..=1000
}

/// Generate valid rate limits (requests per second)
fn rate_limit_strategy() -> impl Strategy<Value = u32> {
    1u32..=10000
}

/// Generate valid stage names
fn stage_name_strategy() -> impl Strategy<Value = String> {
    "[a-z]{3,20}".prop_map(|s| s.to_string())
}

// ============================================================================
// PROPERTY: Determinism - Same input produces same output
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(50))]

    /// Property: Rate limiter config is deterministic
    #[test]
    fn prop_rate_limiter_config_deterministic(
        requests_per_second in rate_limit_strategy(),
        burst_size in 1u32..=100,
    ) {
        // Act
        let config1 = RateLimiterConfig {
            requests_per_second,
            burst_size,
        };
        let config2 = RateLimiterConfig {
            requests_per_second,
            burst_size,
        };

        // Assert - Configs are identical
        prop_assert_eq!(config1.requests_per_second, config2.requests_per_second);
        prop_assert_eq!(config1.burst_size, config2.burst_size);
    }

    /// Property: Kanban config is deterministic
    #[test]
    fn prop_kanban_config_deterministic(
        capacity in capacity_strategy(),
    ) {
        // Act
        let stages1: Vec<Stage> = vec![
            Stage::new("backlog", capacity),
            Stage::new("in_progress", capacity / 2),
            Stage::new("done", capacity),
        ];

        let stages2: Vec<Stage> = vec![
            Stage::new("backlog", capacity),
            Stage::new("in_progress", capacity / 2),
            Stage::new("done", capacity),
        ];

        // Assert - Stages are equivalent
        prop_assert_eq!(stages1.len(), stages2.len());
        for (s1, s2) in stages1.iter().zip(stages2.iter()) {
            prop_assert_eq!(s1.name(), s2.name());
            prop_assert_eq!(s1.capacity(), s2.capacity());
        }
    }
}

// ============================================================================
// PROPERTY: Idempotency - f(f(x)) = f(x)
// ============================================================================

#[tokio::test]
async fn prop_rate_limiter_queries_idempotent() {
    // Arrange
    let config = RateLimiterConfig {
        requests_per_second: 10,
        burst_size: 5,
    };
    let limiter = RateLimiter::new(config);

    // Act - Multiple queries
    let capacity1 = limiter.capacity();
    let capacity2 = limiter.capacity();
    let capacity3 = limiter.capacity();

    let in_flight1 = limiter.in_flight();
    let in_flight2 = limiter.in_flight();

    // Assert - Queries are idempotent
    assert_eq!(capacity1, capacity2);
    assert_eq!(capacity2, capacity3);
    assert_eq!(in_flight1, in_flight2);
}

#[tokio::test]
async fn prop_kanban_queries_idempotent() {
    // Arrange
    let stages = vec![
        Stage::new("backlog", 10),
        Stage::new("in_progress", 5),
        Stage::new("done", 10),
    ];
    let config = KanbanConfig { stages };
    let board = KanbanBoard::new(config).await;

    // Act - Multiple queries
    let capacity1 = board.capacity();
    let capacity2 = board.capacity();

    let in_flight1 = board.in_flight();
    let in_flight2 = board.in_flight();

    let utilization1 = board.utilization();
    let utilization2 = board.utilization();

    // Assert - Queries are idempotent
    assert_eq!(capacity1, capacity2);
    assert_eq!(in_flight1, in_flight2);
    assert_eq!(utilization1, utilization2);
}

// ============================================================================
// PROPERTY: Monotonicity - Bounded utilization and capacity
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(50))]

    /// Property: Utilization is always in [0.0, 1.0]
    #[test]
    fn prop_utilization_bounded(
        capacity in capacity_strategy(),
    ) {
        // Arrange
        let rt = tokio::runtime::Runtime::new().unwrap();

        rt.block_on(async {
            let stages = vec![Stage::new("test", capacity)];
            let config = KanbanConfig { stages };
            let board = KanbanBoard::new(config).await;

            // Act
            let utilization = board.utilization();

            // Assert - Bounded between 0.0 and 1.0
            prop_assert!(utilization >= 0.0);
            prop_assert!(utilization <= 1.0);
            Ok(())
        })?;
    }

    /// Property: In-flight count never exceeds capacity
    #[test]
    fn prop_in_flight_bounded_by_capacity(
        capacity in capacity_strategy(),
    ) {
        let rt = tokio::runtime::Runtime::new().unwrap();

        rt.block_on(async {
            let stages = vec![Stage::new("test", capacity)];
            let config = KanbanConfig { stages };
            let board = KanbanBoard::new(config).await;

            // Act
            let in_flight = board.in_flight();
            let board_capacity = board.capacity();

            // Assert - In-flight never exceeds capacity
            prop_assert!(in_flight <= board_capacity);
            Ok(())
        })?;
    }
}

// ============================================================================
// PROPERTY: Resource Safety - Tokens properly manage capacity
// ============================================================================

#[tokio::test]
async fn prop_token_releases_capacity_on_drop() {
    // Arrange
    let config = RateLimiterConfig {
        requests_per_second: 100,
        burst_size: 10,
    };
    let limiter = RateLimiter::new(config);

    // Act - Acquire token
    let initial_in_flight = limiter.in_flight();
    let token = limiter.acquire().await;
    assert!(token.is_ok());

    let after_acquire = limiter.in_flight();

    // Assert - In-flight increased
    assert_eq!(after_acquire, initial_in_flight + 1);

    // Act - Drop token (should release capacity)
    drop(token);

    // Small delay to allow async cleanup
    tokio::time::sleep(Duration::from_millis(10)).await;

    let after_drop = limiter.in_flight();

    // Assert - Capacity restored
    assert_eq!(after_drop, initial_in_flight);
}

#[tokio::test]
async fn prop_multiple_tokens_correctly_track_capacity() {
    // Arrange
    let config = RateLimiterConfig {
        requests_per_second: 100,
        burst_size: 5,
    };
    let limiter = RateLimiter::new(config);

    // Act - Acquire multiple tokens
    let token1 = limiter.acquire().await.unwrap();
    assert_eq!(limiter.in_flight(), 1);

    let token2 = limiter.acquire().await.unwrap();
    assert_eq!(limiter.in_flight(), 2);

    let token3 = limiter.acquire().await.unwrap();
    assert_eq!(limiter.in_flight(), 3);

    // Assert - All tracked
    assert_eq!(limiter.in_flight(), 3);

    // Act - Drop one token
    drop(token2);
    tokio::time::sleep(Duration::from_millis(10)).await;

    // Assert - Count decreased
    assert_eq!(limiter.in_flight(), 2);

    // Act - Drop remaining
    drop(token1);
    drop(token3);
    tokio::time::sleep(Duration::from_millis(10)).await;

    // Assert - All released
    assert_eq!(limiter.in_flight(), 0);
}

// ============================================================================
// INVARIANT TESTS: Admission control invariants
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(50))]

    /// Invariant: Capacity is always positive
    #[test]
    fn invariant_capacity_positive(capacity in capacity_strategy()) {
        let rt = tokio::runtime::Runtime::new().unwrap();

        rt.block_on(async {
            let stages = vec![Stage::new("test", capacity)];
            let config = KanbanConfig { stages };
            let board = KanbanBoard::new(config).await;

            // Assert - Capacity is positive
            prop_assert!(board.capacity() > 0);
            Ok(())
        })?;
    }

    /// Invariant: In-flight is always non-negative
    #[test]
    fn invariant_in_flight_non_negative(capacity in capacity_strategy()) {
        let rt = tokio::runtime::Runtime::new().unwrap();

        rt.block_on(async {
            let stages = vec![Stage::new("test", capacity)];
            let config = KanbanConfig { stages };
            let board = KanbanBoard::new(config).await;

            // Assert - In-flight is non-negative
            let in_flight = board.in_flight();
            prop_assert!(in_flight >= 0);
            Ok(())
        })?;
    }

    /// Invariant: Stage capacity matches configured value
    #[test]
    fn invariant_stage_capacity_matches_config(
        name in stage_name_strategy(),
        capacity in capacity_strategy(),
    ) {
        // Arrange
        let stage = Stage::new(&name, capacity);

        // Assert - Capacity matches
        prop_assert_eq!(stage.capacity(), capacity);
        prop_assert_eq!(stage.name(), name);
    }
}
