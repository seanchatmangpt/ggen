//! Integration Tests for OSIRIS
//!
//! Tests the complete OSIRIS architecture end-to-end.

use ggen_osiris::{Osiris, OsirisConfig};
use ggen_osiris::admission::AdmissionPolicy;
use ggen_osiris::routing::{Router, RequestHandler};
use async_trait::async_trait;
use std::time::Duration;
use tokio::time::sleep;

#[tokio::test]
async fn test_osiris_end_to_end() {
    // Arrange
    let config = OsirisConfig::default();
    let mut osiris = Osiris::new(config).expect("Failed to create OSIRIS");

    // Act
    let response = osiris
        .process("integration-test", |req| async move {
            sleep(Duration::from_millis(10)).await;
            Ok::<_, String>(format!("Processed: {}", req))
        })
        .await;

    // Assert
    assert!(response.is_ok());
    assert_eq!(response.unwrap(), "Processed: integration-test");
    assert_eq!(osiris.stats().current_wip, 0);
}

#[tokio::test]
async fn test_router_with_handler() {
    // Arrange
    struct EchoHandler;

    #[async_trait]
    impl RequestHandler for EchoHandler {
        type Request = String;
        type Response = String;

        async fn handle(&self, request: Self::Request) -> Result<Self::Response, String> {
            sleep(Duration::from_millis(5)).await;
            Ok(format!("Echo: {}", request))
        }
    }

    let handler = EchoHandler;
    let policy = AdmissionPolicy::default();
    let mut router = Router::new(handler, policy).expect("Failed to create router");

    // Act
    let response = router.route("hello".to_string()).await;

    // Assert
    assert!(response.is_ok());
    assert_eq!(response.unwrap(), "Echo: hello");
}

#[tokio::test]
async fn test_concurrent_request_handling() {
    // Arrange
    let config = OsirisConfig {
        policy: AdmissionPolicy {
            wip_limit: 10,
            min_capacity_ratio: 0.0,
            cooldown: Duration::from_millis(10),
        },
    };
    let mut osiris = Osiris::new(config).expect("Failed to create OSIRIS");

    // Act - simulate concurrent requests (sequential in this test)
    let mut results = Vec::new();
    for i in 0..5 {
        let result = osiris
            .process(i, |req| async move {
                sleep(Duration::from_millis(10)).await;
                Ok::<_, String>(req * 2)
            })
            .await;

        results.push(result);
    }

    // Assert
    assert_eq!(results.len(), 5);
    for (i, result) in results.iter().enumerate() {
        assert!(result.is_ok());
        assert_eq!(result.as_ref().unwrap(), &(i * 2));
    }

    let stats = osiris.stats();
    assert_eq!(stats.current_wip, 0);
}

#[tokio::test]
async fn test_error_handling() {
    // Arrange
    struct FailingHandler;

    #[async_trait]
    impl RequestHandler for FailingHandler {
        type Request = bool;
        type Response = String;

        async fn handle(&self, request: Self::Request) -> Result<Self::Response, String> {
            if request {
                Ok("success".to_string())
            } else {
                Err("intentional failure".to_string())
            }
        }
    }

    let handler = FailingHandler;
    let policy = AdmissionPolicy::default();
    let mut router = Router::new(handler, policy).expect("Failed to create router");

    // Act & Assert - success case
    let success_result = router.route(true).await;
    assert!(success_result.is_ok());
    assert_eq!(success_result.unwrap(), "success");

    // Act & Assert - failure case
    let failure_result = router.route(false).await;
    assert!(failure_result.is_err());
}

#[tokio::test]
async fn test_admission_policy_enforcement() {
    // Arrange - strict policy
    let config = OsirisConfig {
        policy: AdmissionPolicy {
            wip_limit: 1,
            min_capacity_ratio: 0.0,
            cooldown: Duration::from_millis(100),
        },
    };
    let mut osiris = Osiris::new(config).expect("Failed to create OSIRIS");

    // Act - process first request
    let result1 = osiris
        .process("req1", |req| async move {
            Ok::<_, String>(format!("Done: {}", req))
        })
        .await;

    // Assert
    assert!(result1.is_ok());
    assert_eq!(osiris.stats().current_wip, 0); // Completed
}

#[tokio::test]
async fn test_stats_accuracy() {
    // Arrange
    let config = OsirisConfig {
        policy: AdmissionPolicy {
            wip_limit: 5,
            min_capacity_ratio: 0.0,
            cooldown: Duration::from_millis(10),
        },
    };
    let mut osiris = Osiris::new(config).expect("Failed to create OSIRIS");

    // Act
    let initial_stats = osiris.stats();
    assert_eq!(initial_stats.current_wip, 0);
    assert_eq!(initial_stats.wip_limit, 5);

    // Process request
    let _result = osiris
        .process("test", |req| async move {
            Ok::<_, String>(format!("Done: {}", req))
        })
        .await;

    // Assert final state
    let final_stats = osiris.stats();
    assert_eq!(final_stats.current_wip, 0);
    assert_eq!(final_stats.wip_limit, 5);
}

#[tokio::test]
async fn test_capacity_based_admission() {
    // Arrange - configure with capacity threshold
    let config = OsirisConfig {
        policy: AdmissionPolicy {
            wip_limit: 100,
            min_capacity_ratio: 0.1, // Require 10% available capacity
            cooldown: Duration::from_millis(10),
        },
    };
    let mut osiris = Osiris::new(config).expect("Failed to create OSIRIS");

    // Act - process requests
    for i in 0..5 {
        let result = osiris
            .process(i, |req| async move {
                sleep(Duration::from_millis(5)).await;
                Ok::<_, String>(format!("processed-{}", req))
            })
            .await;

        // Should succeed due to low WIP limit
        assert!(result.is_ok());
    }

    // Assert
    let stats = osiris.stats();
    assert_eq!(stats.current_wip, 0);
}

#[tokio::test]
async fn test_full_pipeline_flow() {
    // Arrange
    let config = OsirisConfig {
        policy: AdmissionPolicy {
            wip_limit: 3,
            min_capacity_ratio: 0.0,
            cooldown: Duration::from_millis(50),
        },
    };
    let mut osiris = Osiris::new(config).expect("Failed to create OSIRIS");

    // Act - simulate realistic workflow
    let workflow = vec![
        ("parse", 5),
        ("validate", 10),
        ("transform", 8),
        ("generate", 12),
        ("write", 6),
    ];

    for (stage, duration_ms) in workflow {
        let result = osiris
            .process(stage, |s| async move {
                sleep(Duration::from_millis(duration_ms)).await;
                Ok::<_, String>(format!("Completed: {}", s))
            })
            .await;

        assert!(result.is_ok(), "Stage {} should succeed", stage);
    }

    // Assert
    let final_stats = osiris.stats();
    assert_eq!(final_stats.current_wip, 0, "All work should be completed");
    assert!(
        final_stats.wip_utilization < 1.0,
        "Should maintain headroom"
    );
}
