//! Tests for usage tracking and cost estimation

use ggen_dspy::config::{UsageStats, UsageTracker};

#[test]
fn test_basic_tracking() {
    let tracker = UsageTracker::new(true);

    tracker.track("openai/gpt-4o-mini", 100, 50, false);

    let stats = tracker.get_model_stats("openai/gpt-4o-mini").unwrap();
    assert_eq!(stats.prompt_tokens, 100);
    assert_eq!(stats.completion_tokens, 50);
    assert_eq!(stats.total_tokens, 150);
    assert_eq!(stats.requests, 1);
    assert_eq!(stats.cached_requests, 0);
}

#[test]
fn test_disabled_tracking() {
    let tracker = UsageTracker::new(false);

    tracker.track("openai/gpt-4o-mini", 100, 50, false);

    let stats = tracker.get_stats();
    assert!(stats.is_empty());
}

#[test]
fn test_multiple_models() {
    let tracker = UsageTracker::new(true);

    tracker.track("openai/gpt-4o-mini", 100, 50, false);
    tracker.track("anthropic/claude-3-haiku-20240307", 200, 100, false);

    let stats = tracker.get_stats();
    assert_eq!(stats.len(), 2);

    let gpt_stats = tracker.get_model_stats("openai/gpt-4o-mini").unwrap();
    assert_eq!(gpt_stats.total_tokens, 150);

    let claude_stats = tracker.get_model_stats("anthropic/claude-3-haiku-20240307").unwrap();
    assert_eq!(claude_stats.total_tokens, 300);
}

#[test]
fn test_cost_estimation_gpt4o_mini() {
    let tracker = UsageTracker::new(true);

    // GPT-4o-mini: $0.15 per 1M prompt, $0.60 per 1M completion
    // 1000 prompt + 500 completion = (1000 * 0.15 + 500 * 0.60) / 1M
    tracker.track("openai/gpt-4o-mini", 1000, 500, false);

    let stats = tracker.get_model_stats("openai/gpt-4o-mini").unwrap();
    let expected_cost = (1000.0 * 0.15 + 500.0 * 0.60) / 1_000_000.0;
    assert!((stats.cost_usd - expected_cost).abs() < 0.000001);
}

#[test]
fn test_cost_estimation_claude3_haiku() {
    let tracker = UsageTracker::new(true);

    // Claude 3 Haiku: $0.25 per 1M prompt, $1.25 per 1M completion
    tracker.track("anthropic/claude-3-haiku-20240307", 1000, 500, false);

    let stats = tracker.get_model_stats("anthropic/claude-3-haiku-20240307").unwrap();
    let expected_cost = (1000.0 * 0.25 + 500.0 * 1.25) / 1_000_000.0;
    assert!((stats.cost_usd - expected_cost).abs() < 0.000001);
}

#[test]
fn test_ollama_free() {
    let tracker = UsageTracker::new(true);

    tracker.track("ollama/llama3", 1000, 500, false);

    let stats = tracker.get_model_stats("ollama/llama3").unwrap();
    assert_eq!(stats.cost_usd, 0.0);
}

#[test]
fn test_cache_hit_tracking() {
    let tracker = UsageTracker::new(true);

    // 3 requests, 2 cached
    tracker.track("openai/gpt-4o-mini", 100, 50, false);
    tracker.track("openai/gpt-4o-mini", 100, 50, true);
    tracker.track("openai/gpt-4o-mini", 100, 50, true);

    let stats = tracker.get_model_stats("openai/gpt-4o-mini").unwrap();
    assert_eq!(stats.requests, 3);
    assert_eq!(stats.cached_requests, 2);
    assert_eq!(stats.cache_hit_rate(), 2.0 / 3.0);
}

#[test]
fn test_total_stats() {
    let tracker = UsageTracker::new(true);

    tracker.track("openai/gpt-4o-mini", 100, 50, false);
    tracker.track("anthropic/claude-3-haiku-20240307", 200, 100, false);
    tracker.track("openai/gpt-4o-mini", 50, 25, true);

    let total = tracker.get_total_stats();
    assert_eq!(total.prompt_tokens, 350);
    assert_eq!(total.completion_tokens, 175);
    assert_eq!(total.total_tokens, 525);
    assert_eq!(total.requests, 3);
    assert_eq!(total.cached_requests, 1);
}

#[test]
fn test_reset() {
    let tracker = UsageTracker::new(true);

    tracker.track("openai/gpt-4o-mini", 100, 50, false);
    assert!(!tracker.get_stats().is_empty());

    tracker.reset();
    assert!(tracker.get_stats().is_empty());
}

#[test]
fn test_model_specific_reset() {
    let tracker = UsageTracker::new(true);

    tracker.track("openai/gpt-4o-mini", 100, 50, false);
    tracker.track("anthropic/claude-3-haiku-20240307", 200, 100, false);

    tracker.reset_model("openai/gpt-4o-mini");

    assert!(tracker.get_model_stats("openai/gpt-4o-mini").is_none());
    assert!(tracker.get_model_stats("anthropic/claude-3-haiku-20240307").is_some());
}

#[test]
fn test_json_export_import() {
    let tracker = UsageTracker::new(true);

    tracker.track("openai/gpt-4o-mini", 100, 50, false);
    tracker.track("anthropic/claude-3-haiku-20240307", 200, 100, true);

    let json = tracker.export_json().unwrap();

    let new_tracker = UsageTracker::new(true);
    new_tracker.import_json(&json).unwrap();

    let original_stats = tracker.get_stats();
    let imported_stats = new_tracker.get_stats();

    assert_eq!(original_stats.len(), imported_stats.len());

    for (model, stats) in &original_stats {
        let imported = imported_stats.get(model).unwrap();
        assert_eq!(stats.total_tokens, imported.total_tokens);
        assert_eq!(stats.requests, imported.requests);
        assert_eq!(stats.cached_requests, imported.cached_requests);
    }
}

#[test]
fn test_is_enabled() {
    let enabled_tracker = UsageTracker::new(true);
    assert!(enabled_tracker.is_enabled());

    let disabled_tracker = UsageTracker::new(false);
    assert!(!disabled_tracker.is_enabled());
}

#[test]
fn test_usage_stats_add() {
    let mut stats1 = UsageStats {
        prompt_tokens: 100,
        completion_tokens: 50,
        total_tokens: 150,
        requests: 1,
        cached_requests: 0,
        cost_usd: 0.001,
    };

    let stats2 = UsageStats {
        prompt_tokens: 200,
        completion_tokens: 100,
        total_tokens: 300,
        requests: 2,
        cached_requests: 1,
        cost_usd: 0.002,
    };

    stats1.add(&stats2);

    assert_eq!(stats1.prompt_tokens, 300);
    assert_eq!(stats1.completion_tokens, 150);
    assert_eq!(stats1.total_tokens, 450);
    assert_eq!(stats1.requests, 3);
    assert_eq!(stats1.cached_requests, 1);
    assert!((stats1.cost_usd - 0.003).abs() < 0.000001);
}
