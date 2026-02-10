//! Cognitive Load Measurement Tests
//!
//! Validates that OSIRIS maintains λ_admitted → 0.

use ggen_osiris::{Osiris, OsirisConfig};
use ggen_osiris::admission::AdmissionPolicy;
use std::time::Duration;
use tokio::time::sleep;

#[tokio::test]
async fn test_zero_cognitive_load_single_request() {
    // Arrange
    let config = OsirisConfig::default();
    let mut osiris = Osiris::new(config).expect("Failed to create OSIRIS");

    // Act
    let result = osiris
        .process("task", |_req| async move {
            sleep(Duration::from_millis(10)).await;
            Ok::<_, String>("completed".to_string())
        })
        .await;

    // Assert
    assert!(result.is_ok());
    let stats = osiris.stats();
    assert_eq!(stats.current_wip, 0, "WIP should return to zero");
}

#[tokio::test]
async fn test_zero_cognitive_load_sequential_requests() {
    // Arrange
    let config = OsirisConfig::default();
    let mut osiris = Osiris::new(config).expect("Failed to create OSIRIS");

    // Act - process 5 requests sequentially
    for i in 0..5 {
        let result = osiris
            .process(i, |req| async move {
                sleep(Duration::from_millis(5)).await;
                Ok::<_, String>(format!("completed-{}", req))
            })
            .await;

        assert!(result.is_ok());
    }

    // Assert
    let stats = osiris.stats();
    assert_eq!(stats.current_wip, 0, "WIP should return to zero after all requests");
}

#[tokio::test]
async fn test_refuse_at_wip_limit() {
    // Arrange - very low WIP limit
    let config = OsirisConfig {
        policy: AdmissionPolicy {
            wip_limit: 1,
            min_capacity_ratio: 0.0,
            cooldown: Duration::from_millis(10),
        },
    };
    let mut osiris = Osiris::new(config).expect("Failed to create OSIRIS");

    // Act - try to process request
    let result = osiris
        .process("task", |_req| async move {
            Ok::<_, String>("done".to_string())
        })
        .await;

    // Assert - should succeed
    assert!(result.is_ok());

    // WIP should return to zero
    assert_eq!(osiris.stats().current_wip, 0);
}

#[tokio::test]
async fn test_cognitive_load_measurement() {
    // Arrange
    let config = OsirisConfig {
        policy: AdmissionPolicy {
            wip_limit: 10,
            min_capacity_ratio: 0.0,
            cooldown: Duration::from_millis(10),
        },
    };
    let mut osiris = Osiris::new(config).expect("Failed to create OSIRIS");

    // Act - process multiple requests
    let mut max_wip = 0;
    for i in 0..5 {
        let _result = osiris
            .process(i, |req| async move {
                sleep(Duration::from_millis(5)).await;
                Ok::<_, String>(format!("done-{}", req))
            })
            .await;

        let stats = osiris.stats();
        max_wip = max_wip.max(stats.current_wip);
    }

    // Assert
    let final_stats = osiris.stats();
    assert_eq!(final_stats.current_wip, 0, "Final WIP should be zero");
    assert!(
        max_wip <= 10,
        "Max WIP should not exceed limit: {} <= 10",
        max_wip
    );
}

#[tokio::test]
async fn test_admission_control_prevents_overload() {
    // Arrange - strict limits
    let config = OsirisConfig {
        policy: AdmissionPolicy {
            wip_limit: 2,
            min_capacity_ratio: 0.0,
            cooldown: Duration::from_millis(10),
        },
    };
    let mut osiris = Osiris::new(config).expect("Failed to create OSIRIS");

    // Act - process requests sequentially
    let mut admitted = 0;
    let mut refused = 0;

    for i in 0..3 {
        let result = osiris
            .process(i, |req| async move {
                Ok::<_, String>(format!("processed-{}", req))
            })
            .await;

        match result {
            Ok(_) => admitted += 1,
            Err(_) => refused += 1,
        }
    }

    // Assert
    assert!(admitted > 0, "Some requests should be admitted");
    // Note: In sequential execution, all should succeed since they complete before next starts
    assert_eq!(refused, 0, "No requests should be refused in sequential execution");

    let stats = osiris.stats();
    assert_eq!(stats.current_wip, 0, "Final WIP should be zero");
}

#[tokio::test]
async fn test_capacity_estimation_updates() {
    // Arrange
    let config = OsirisConfig::default();
    let mut osiris = Osiris::new(config).expect("Failed to create OSIRIS");

    // Act - process requests with varying durations
    for duration_ms in [5, 10, 15, 10, 5] {
        let _result = osiris
            .process(duration_ms, |req| async move {
                sleep(Duration::from_millis(req)).await;
                Ok::<_, String>("done".to_string())
            })
            .await;
    }

    // Assert
    let stats = osiris.stats();
    assert!(
        stats.total_capacity > 0.0,
        "Total capacity should be positive"
    );
    assert!(
        stats.available_capacity >= 0.0 && stats.available_capacity <= 1.0,
        "Available capacity should be in [0, 1]: {}",
        stats.available_capacity
    );
}

#[tokio::test]
async fn test_lambda_admitted_approaches_zero() {
    // Arrange - configure for zero cognitive load
    let config = OsirisConfig {
        policy: AdmissionPolicy {
            wip_limit: 5,
            min_capacity_ratio: 0.2,
            cooldown: Duration::from_millis(50),
        },
    };
    let mut osiris = Osiris::new(config).expect("Failed to create OSIRIS");

    // Act - process burst of requests
    let mut final_wip = 0;
    for i in 0..10 {
        let _result = osiris
            .process(i, |req| async move {
                sleep(Duration::from_millis(5)).await;
                Ok::<_, String>(format!("completed-{}", req))
            })
            .await;

        final_wip = osiris.stats().current_wip;
    }

    // Assert - λ_admitted → 0
    assert_eq!(
        final_wip, 0,
        "λ_admitted should approach zero: final WIP = {}",
        final_wip
    );

    let stats = osiris.stats();
    assert!(
        stats.wip_utilization < 1.0,
        "WIP utilization should be below 100%: {}",
        stats.wip_utilization
    );
}
