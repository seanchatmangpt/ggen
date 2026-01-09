//! Integration tests for metrics and caching in codegen module

use ggen_ai::codegen::{ProcessMetrics, TTLToSignatureTranspiler, Timer};

#[test]
fn test_metrics_creation() {
    let metrics = ProcessMetrics::new();
    assert_eq!(metrics.signatures_generated, 0);
    assert_eq!(metrics.processing_time_ms, 0.0);
    assert_eq!(metrics.error_count, 0);
    assert_eq!(metrics.cache_hits, 0);
    assert_eq!(metrics.cache_misses, 0);
}

#[test]
fn test_metrics_error_rate() {
    let metrics = ProcessMetrics {
        signatures_generated: 100,
        processing_time_ms: 100.0,
        error_count: 5,
        cache_hits: 50,
        cache_misses: 50,
    };

    let error_rate = metrics.error_rate();
    assert!((error_rate - 5.0).abs() < 0.1);
}

#[test]
fn test_metrics_cache_hit_rate() {
    let metrics = ProcessMetrics {
        signatures_generated: 100,
        processing_time_ms: 100.0,
        error_count: 0,
        cache_hits: 75,
        cache_misses: 25,
    };

    let hit_rate = metrics.cache_hit_rate();
    assert!((hit_rate - 75.0).abs() < 0.1);
}

#[test]
fn test_metrics_sigma_level_three_sigma() {
    let metrics = ProcessMetrics {
        signatures_generated: 10000,
        processing_time_ms: 1000.0,
        error_count: 27,  // ~0.27% error rate
        cache_hits: 0,
        cache_misses: 0,
    };

    let sigma = metrics.sigma_level();
    // Should be around 3 sigma (0.27% error rate)
    assert!(sigma >= 2.9 && sigma <= 3.1);
}

#[test]
fn test_metrics_throughput() {
    let metrics = ProcessMetrics {
        signatures_generated: 100,
        processing_time_ms: 1000.0,
        error_count: 0,
        cache_hits: 0,
        cache_misses: 0,
    };

    let throughput = metrics.throughput_sps();
    assert!((throughput - 100.0).abs() < 0.1);
}

#[test]
fn test_metrics_summary() {
    let metrics = ProcessMetrics {
        signatures_generated: 50,
        processing_time_ms: 250.0,
        error_count: 2,
        cache_hits: 40,
        cache_misses: 10,
    };

    let summary = metrics.summary();
    assert!(summary.contains("50 signatures"));
    assert!(summary.contains("Sigma"));
}

#[test]
fn test_timer_basic() {
    let timer = Timer::start();
    std::thread::sleep(std::time::Duration::from_millis(50));
    let elapsed = timer.stop();
    assert!(elapsed >= 50.0);
    assert!(elapsed < 200.0);
}

#[test]
fn test_transpiler_creation() {
    let transpiler = TTLToSignatureTranspiler::new();
    assert_eq!(transpiler.signature_count(), 0);
}

#[test]
fn test_transpiler_with_custom_cache_size() {
    let transpiler = TTLToSignatureTranspiler::with_cache_size(100)
        .expect("Should create transpiler with cache size 100");
    assert_eq!(transpiler.cache_size().unwrap(), 0);
}

#[test]
fn test_transpiler_cache_size_zero_error() {
    let result = TTLToSignatureTranspiler::with_cache_size(0);
    assert!(result.is_err());
}

#[test]
fn test_transpiler_metrics_initial_state() {
    let transpiler = TTLToSignatureTranspiler::new();
    let metrics = transpiler.metrics().unwrap();

    assert_eq!(metrics.signatures_generated, 0);
    assert_eq!(metrics.error_count, 0);
    assert_eq!(metrics.cache_hits, 0);
    assert_eq!(metrics.cache_misses, 0);
}

#[test]
fn test_transpiler_clear() {
    let transpiler = TTLToSignatureTranspiler::new();

    // Do some work to populate metrics
    let _ = transpiler.check_field_collision("test".to_string());

    // Clear should reset everything
    transpiler.clear().unwrap();

    assert_eq!(transpiler.cache_size().unwrap(), 0);
    let metrics = transpiler.metrics().unwrap();
    assert_eq!(metrics.signatures_generated, 0);
}

#[test]
fn test_transpiler_clone_shares_state() {
    let transpiler1 = TTLToSignatureTranspiler::new();
    let transpiler2 = transpiler1.clone();

    // Both clones should share the same metrics and cache
    let metrics1 = transpiler1.metrics().unwrap();
    let metrics2 = transpiler2.metrics().unwrap();

    assert_eq!(metrics1.cache_hits, metrics2.cache_hits);
    assert_eq!(metrics1.cache_misses, metrics2.cache_misses);
}

#[test]
fn test_metrics_reset() {
    let mut metrics = ProcessMetrics {
        signatures_generated: 100,
        processing_time_ms: 1000.0,
        error_count: 10,
        cache_hits: 50,
        cache_misses: 50,
    };

    metrics.reset();

    assert_eq!(metrics.signatures_generated, 0);
    assert_eq!(metrics.processing_time_ms, 0.0);
    assert_eq!(metrics.error_count, 0);
    assert_eq!(metrics.cache_hits, 0);
    assert_eq!(metrics.cache_misses, 0);
}

#[test]
fn test_metrics_default() {
    let metrics = ProcessMetrics::default();
    assert_eq!(metrics.signatures_generated, 0);
    assert_eq!(metrics.cache_hits + metrics.cache_misses, 0);
}

#[test]
fn test_sigma_level_two_sigma() {
    let metrics = ProcessMetrics {
        signatures_generated: 1000,
        processing_time_ms: 500.0,
        error_count: 45,  // ~4.5% error rate
        cache_hits: 0,
        cache_misses: 0,
    };

    let sigma = metrics.sigma_level();
    // Should be around 2 sigma (4.5% error rate)
    assert!(sigma >= 1.9 && sigma <= 2.1);
}

#[test]
fn test_metrics_perfect_quality() {
    let metrics = ProcessMetrics {
        signatures_generated: 1000,
        processing_time_ms: 500.0,
        error_count: 0,
        cache_hits: 0,
        cache_misses: 0,
    };

    let sigma = metrics.sigma_level();
    // Should be high sigma (5+) with zero errors
    assert!(sigma >= 5.0);
}

#[test]
fn test_transpiler_check_field_collision() {
    let transpiler = TTLToSignatureTranspiler::new();

    let name1 = transpiler.check_field_collision("test".to_string()).unwrap();
    assert_eq!(name1, "test");

    let name2 = transpiler.check_field_collision("test".to_string()).unwrap();
    assert_eq!(name2, "test_1");
}

#[test]
fn test_metrics_error_rate_no_signatures() {
    let metrics = ProcessMetrics {
        signatures_generated: 0,
        processing_time_ms: 0.0,
        error_count: 5,
        cache_hits: 0,
        cache_misses: 0,
    };

    let error_rate = metrics.error_rate();
    assert_eq!(error_rate, 0.0);
}
