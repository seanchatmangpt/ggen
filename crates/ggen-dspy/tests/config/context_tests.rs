//! Tests for thread-local context management

use ggen_dspy::config::{get_dspy_config, init_dspy_config, with_context};
use ggen_dspy::config::context::{
    get_current_temperature, get_current_max_tokens, get_current_top_p,
    is_cache_enabled, is_tracking_enabled, is_streaming_enabled
};

#[tokio::test]
async fn test_temperature_override() {
    init_dspy_config();
    let global_temp = get_dspy_config().default_temperature;

    with_context()
        .temperature(0.0)
        .run(async {
            let current = get_current_temperature();
            assert_eq!(current, 0.0);
            Ok::<(), ()>(())
        })
        .await
        .unwrap();

    // Should revert to global
    let reverted = get_current_temperature();
    assert_eq!(reverted, global_temp);
}

#[tokio::test]
async fn test_max_tokens_override() {
    init_dspy_config();

    with_context()
        .max_tokens(4096)
        .run(async {
            assert_eq!(get_current_max_tokens(), 4096);
            Ok::<(), ()>(())
        })
        .await
        .unwrap();
}

#[tokio::test]
async fn test_top_p_override() {
    init_dspy_config();

    with_context()
        .top_p(0.95)
        .run(async {
            assert_eq!(get_current_top_p(), 0.95);
            Ok::<(), ()>(())
        })
        .await
        .unwrap();
}

#[tokio::test]
async fn test_cache_override() {
    init_dspy_config();

    with_context()
        .cache_enabled(false)
        .run(async {
            assert!(!is_cache_enabled());
            Ok::<(), ()>(())
        })
        .await
        .unwrap();

    // Should revert to default
    assert!(is_cache_enabled());
}

#[tokio::test]
async fn test_multiple_overrides() {
    init_dspy_config();

    with_context()
        .temperature(0.5)
        .max_tokens(4096)
        .top_p(0.95)
        .cache_enabled(false)
        .track_usage(true)
        .streaming(true)
        .run(async {
            assert_eq!(get_current_temperature(), 0.5);
            assert_eq!(get_current_max_tokens(), 4096);
            assert_eq!(get_current_top_p(), 0.95);
            assert!(!is_cache_enabled());
            assert!(is_tracking_enabled());
            assert!(is_streaming_enabled());
            Ok::<(), ()>(())
        })
        .await
        .unwrap();
}

#[tokio::test]
async fn test_nested_contexts() {
    init_dspy_config();

    with_context()
        .temperature(0.8)
        .run(async {
            assert_eq!(get_current_temperature(), 0.8);

            // Nested override
            with_context()
                .temperature(0.2)
                .run(async {
                    assert_eq!(get_current_temperature(), 0.2);
                    Ok::<(), ()>(())
                })
                .await
                .unwrap();

            // Back to outer context
            assert_eq!(get_current_temperature(), 0.8);
            Ok::<(), ()>(())
        })
        .await
        .unwrap();
}

#[tokio::test]
async fn test_error_propagation() {
    init_dspy_config();

    let result: Result<(), String> = with_context()
        .temperature(0.0)
        .run(async {
            Err("Test error".to_string())
        })
        .await;

    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), "Test error");
}

#[tokio::test]
async fn test_context_with_spawned_task() {
    init_dspy_config();

    with_context()
        .temperature(0.3)
        .run(async {
            // Spawn a task - should inherit context
            let handle = tokio::spawn(async {
                get_current_temperature()
            });

            let temp = handle.await.unwrap();
            assert_eq!(temp, 0.3);
            Ok::<(), ()>(())
        })
        .await
        .unwrap();
}

#[tokio::test]
async fn test_deterministic_context() {
    init_dspy_config();

    // Deterministic settings for testing
    with_context()
        .temperature(0.0)
        .cache_enabled(false)
        .run(async {
            assert_eq!(get_current_temperature(), 0.0);
            assert!(!is_cache_enabled());
            Ok::<(), ()>(())
        })
        .await
        .unwrap();
}

#[tokio::test]
async fn test_tracking_override() {
    init_dspy_config();

    with_context()
        .track_usage(true)
        .run(async {
            assert!(is_tracking_enabled());
            Ok::<(), ()>(())
        })
        .await
        .unwrap();
}

#[tokio::test]
async fn test_streaming_override() {
    init_dspy_config();

    with_context()
        .streaming(true)
        .run(async {
            assert!(is_streaming_enabled());
            Ok::<(), ()>(())
        })
        .await
        .unwrap();
}

#[tokio::test]
async fn test_context_isolation_between_tasks() {
    init_dspy_config();

    // Task 1 with temperature 0.1
    let task1 = tokio::spawn(async {
        with_context()
            .temperature(0.1)
            .run(async {
                tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
                get_current_temperature()
            })
            .await
    });

    // Task 2 with temperature 0.9
    let task2 = tokio::spawn(async {
        with_context()
            .temperature(0.9)
            .run(async {
                tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
                get_current_temperature()
            })
            .await
    });

    let temp1 = task1.await.unwrap().unwrap();
    let temp2 = task2.await.unwrap().unwrap();

    // Each task should have its own isolated context
    assert_eq!(temp1, 0.1);
    assert_eq!(temp2, 0.9);
}
