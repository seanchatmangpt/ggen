//! Integration tests for full DSPy configuration system
//!
//! Tests the interaction between:
//! - Global settings
//! - Thread-local context
//! - Cache manager
//! - Usage tracker
//! - ggen-ai GlobalLlmConfig

use ggen_dspy::config::{
    get_dspy_config, init_dspy_config, with_context, CacheConfig, CacheManager, DspySettings,
    UsageTracker,
};
use ggen_dspy::config::context::{get_current_temperature, is_cache_enabled, is_tracking_enabled};
use tempfile::TempDir;

#[tokio::test]
async fn test_end_to_end_configuration() {
    // Initialize global config
    init_dspy_config();

    // Create cache manager
    let temp_dir = TempDir::new().unwrap();
    let cache_config = CacheConfig {
        enable_memory: true,
        enable_disk: true,
        memory_max_entries: 100,
        disk_cache_dir: temp_dir.path().to_path_buf(),
        disk_size_limit_bytes: 1_000_000,
        ttl_seconds: None,
    };
    let cache = CacheManager::new(cache_config).unwrap();

    // Create usage tracker
    let tracker = UsageTracker::new(true);

    // Simulate an LLM call with context override
    with_context()
        .temperature(0.0)
        .cache_enabled(true)
        .track_usage(true)
        .run(async {
            // Check context
            assert_eq!(get_current_temperature(), 0.0);
            assert!(is_cache_enabled());
            assert!(is_tracking_enabled());

            // Cache a value
            let cache_key = "test_prompt_hash";
            let response = "Mocked LLM response";
            cache.set(cache_key.to_string(), response.to_string()).await.unwrap();

            // Track usage
            tracker.track("openai/gpt-4o-mini", 100, 50, false);

            Ok::<(), String>(())
        })
        .await
        .unwrap();

    // Verify cache
    let cached_value = cache.get("test_prompt_hash").await;
    assert_eq!(cached_value, Some("Mocked LLM response".to_string()));

    // Verify tracking
    let stats = tracker.get_model_stats("openai/gpt-4o-mini").unwrap();
    assert_eq!(stats.total_tokens, 150);
}

#[tokio::test]
async fn test_context_propagation_with_cache_and_tracking() {
    init_dspy_config();

    let temp_dir = TempDir::new().unwrap();
    let cache_config = CacheConfig {
        enable_memory: true,
        enable_disk: false,
        memory_max_entries: 100,
        disk_cache_dir: temp_dir.path().to_path_buf(),
        disk_size_limit_bytes: 0,
        ttl_seconds: None,
    };
    let cache = CacheManager::new(cache_config).unwrap();
    let tracker = UsageTracker::new(true);

    // Parent context
    with_context()
        .temperature(0.8)
        .cache_enabled(true)
        .run(async {
            // Simulate first call (cache miss)
            cache.set("key1".to_string(), "value1".to_string()).await.unwrap();
            tracker.track("openai/gpt-4o-mini", 100, 50, false);

            // Child context
            with_context()
                .temperature(0.2)
                .run(async {
                    assert_eq!(get_current_temperature(), 0.2);

                    // Simulate second call (cache hit)
                    let value = cache.get("key1").await;
                    assert_eq!(value, Some("value1".to_string()));
                    tracker.track("openai/gpt-4o-mini", 100, 50, true);

                    Ok::<(), ()>(())
                })
                .await
                .unwrap();

            // Back to parent context
            assert_eq!(get_current_temperature(), 0.8);

            Ok::<(), ()>(())
        })
        .await
        .unwrap();

    // Verify tracking
    let stats = tracker.get_model_stats("openai/gpt-4o-mini").unwrap();
    assert_eq!(stats.requests, 2);
    assert_eq!(stats.cached_requests, 1);
    assert_eq!(stats.total_tokens, 300);
}

#[tokio::test]
async fn test_concurrent_contexts_with_isolated_state() {
    init_dspy_config();

    // Task 1: Low temperature, cache disabled
    let task1 = tokio::spawn(async {
        with_context()
            .temperature(0.1)
            .cache_enabled(false)
            .run(async {
                tokio::time::sleep(tokio::time::Duration::from_millis(50)).await;
                (get_current_temperature(), is_cache_enabled())
            })
            .await
    });

    // Task 2: High temperature, cache enabled
    let task2 = tokio::spawn(async {
        with_context()
            .temperature(0.9)
            .cache_enabled(true)
            .run(async {
                tokio::time::sleep(tokio::time::Duration::from_millis(50)).await;
                (get_current_temperature(), is_cache_enabled())
            })
            .await
    });

    let (temp1, cache1) = task1.await.unwrap().unwrap();
    let (temp2, cache2) = task2.await.unwrap().unwrap();

    // Each task maintains isolated context
    assert_eq!(temp1, 0.1);
    assert!(!cache1);
    assert_eq!(temp2, 0.9);
    assert!(cache2);
}

#[tokio::test]
async fn test_cache_with_usage_tracking() {
    let temp_dir = TempDir::new().unwrap();
    let cache_config = CacheConfig {
        enable_memory: true,
        enable_disk: true,
        memory_max_entries: 100,
        disk_cache_dir: temp_dir.path().to_path_buf(),
        disk_size_limit_bytes: 1_000_000,
        ttl_seconds: None,
    };
    let cache = CacheManager::new(cache_config).unwrap();
    let tracker = UsageTracker::new(true);

    // First call - cache miss
    let response1 = "Response 1";
    cache.set("prompt1".to_string(), response1.to_string()).await.unwrap();
    tracker.track("openai/gpt-4o-mini", 100, 50, false);

    // Second call - cache hit
    let cached = cache.get("prompt1").await;
    assert_eq!(cached, Some(response1.to_string()));
    tracker.track("openai/gpt-4o-mini", 100, 50, true);

    // Verify stats
    let cache_stats = cache.stats();
    assert_eq!(cache_stats.hits, 1);
    assert_eq!(cache_stats.misses, 0);

    let usage_stats = tracker.get_model_stats("openai/gpt-4o-mini").unwrap();
    assert_eq!(usage_stats.requests, 2);
    assert_eq!(usage_stats.cached_requests, 1);
    assert_eq!(usage_stats.cache_hit_rate(), 0.5);
}

#[tokio::test]
async fn test_global_and_local_config_interaction() {
    init_dspy_config();

    let global_temp = get_dspy_config().default_temperature;

    // Override globally configured value
    with_context()
        .temperature(global_temp + 0.1)
        .run(async {
            let overridden = get_current_temperature();
            assert_eq!(overridden, global_temp + 0.1);
            Ok::<(), ()>(())
        })
        .await
        .unwrap();

    // Verify reversion
    assert_eq!(get_current_temperature(), global_temp);
}

#[tokio::test]
async fn test_settings_validation_in_context() {
    init_dspy_config();

    // Valid override
    let result = with_context()
        .temperature(0.5)
        .run(async {
            assert_eq!(get_current_temperature(), 0.5);
            Ok::<(), ()>(())
        })
        .await;

    assert!(result.is_ok());
}

#[test]
fn test_llm_config_integration() {
    init_dspy_config();
    let config = get_dspy_config();

    // Should integrate with ggen-ai global config
    let llm_config = config.llm_config();
    assert!(llm_config.provider_name().len() > 0);

    // Verify provider config exists
    let provider_config = llm_config.get_default_config();
    assert!(provider_config.is_some());
}

#[tokio::test]
async fn test_multiple_models_tracking() {
    let tracker = UsageTracker::new(true);

    // Track different models
    tracker.track("openai/gpt-4o-mini", 100, 50, false);
    tracker.track("anthropic/claude-3-haiku-20240307", 200, 100, false);
    tracker.track("openai/gpt-4o", 500, 250, false);

    let stats = tracker.get_stats();
    assert_eq!(stats.len(), 3);

    let total = tracker.get_total_stats();
    assert_eq!(total.prompt_tokens, 800);
    assert_eq!(total.completion_tokens, 400);
    assert_eq!(total.requests, 3);
}

#[tokio::test]
async fn test_cache_persistence_across_contexts() {
    let temp_dir = TempDir::new().unwrap();
    let cache_config = CacheConfig {
        enable_memory: true,
        enable_disk: true,
        memory_max_entries: 100,
        disk_cache_dir: temp_dir.path().to_path_buf(),
        disk_size_limit_bytes: 1_000_000,
        ttl_seconds: None,
    };
    let cache = CacheManager::new(cache_config).unwrap();

    // Set value in one context
    with_context()
        .temperature(0.1)
        .run(async {
            cache.set("persistent_key".to_string(), "persistent_value".to_string()).await.unwrap();
            Ok::<(), ()>(())
        })
        .await
        .unwrap();

    // Retrieve in different context
    with_context()
        .temperature(0.9)
        .run(async {
            let value = cache.get("persistent_key").await;
            assert_eq!(value, Some("persistent_value".to_string()));
            Ok::<(), ()>(())
        })
        .await
        .unwrap();
}

#[test]
fn test_usage_tracker_json_persistence() {
    let tracker = UsageTracker::new(true);

    tracker.track("openai/gpt-4o-mini", 100, 50, false);
    tracker.track("anthropic/claude-3-haiku-20240307", 200, 100, true);

    // Export to JSON
    let json = tracker.export_json().unwrap();

    // Import into new tracker
    let new_tracker = UsageTracker::new(true);
    new_tracker.import_json(&json).unwrap();

    // Verify all stats preserved
    let original_total = tracker.get_total_stats();
    let imported_total = new_tracker.get_total_stats();

    assert_eq!(original_total.total_tokens, imported_total.total_tokens);
    assert_eq!(original_total.requests, imported_total.requests);
    assert_eq!(original_total.cached_requests, imported_total.cached_requests);
}

#[tokio::test]
async fn test_deterministic_mode() {
    init_dspy_config();

    // Deterministic configuration for reproducible results
    with_context()
        .temperature(0.0)
        .cache_enabled(false)
        .track_usage(true)
        .streaming(false)
        .run(async {
            assert_eq!(get_current_temperature(), 0.0);
            assert!(!is_cache_enabled());
            assert!(is_tracking_enabled());
            assert!(!ggen_dspy::config::context::is_streaming_enabled());
            Ok::<(), ()>(())
        })
        .await
        .unwrap();
}
