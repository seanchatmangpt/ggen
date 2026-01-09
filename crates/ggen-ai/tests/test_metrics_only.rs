//! Integration tests for ProcessMetrics and Timer

use ggen_ai::codegen::{ProcessMetrics, Timer};

#[test]
fn test_process_metrics_creation() {
    let metrics = ProcessMetrics::new();
    assert_eq!(metrics.signatures_generated, 0);
    assert_eq!(metrics.processing_time_ms, 0.0);
    assert_eq!(metrics.error_count, 0);
    assert_eq!(metrics.cache_hits, 0);
    assert_eq!(metrics.cache_misses, 0);
}

#[test]
fn test_error_rate_calculation() {
    let metrics = ProcessMetrics {
        signatures_generated: 100,
        processing_time_ms: 100.0,
        error_count: 5,
        cache_hits: 0,
        cache_misses: 0,
    };
    assert!((metrics.error_rate() - 5.0).abs() < 0.01);
}

#[test]
fn test_error_rate_no_signatures() {
    let metrics = ProcessMetrics {
        signatures_generated: 0,
        processing_time_ms: 0.0,
        error_count: 5,
        cache_hits: 0,
        cache_misses: 0,
    };
    assert_eq!(metrics.error_rate(), 0.0);
}

#[test]
fn test_cache_hit_rate_calculation() {
    let metrics = ProcessMetrics {
        signatures_generated: 0,
        processing_time_ms: 0.0,
        error_count: 0,
        cache_hits: 80,
        cache_misses: 20,
    };
    assert!((metrics.cache_hit_rate() - 80.0).abs() < 0.01);
}

#[test]
fn test_cache_hit_rate_zero_total() {
    let metrics = ProcessMetrics {
        signatures_generated: 0,
        processing_time_ms: 0.0,
        error_count: 0,
        cache_hits: 0,
        cache_misses: 0,
    };
    assert_eq!(metrics.cache_hit_rate(), 0.0);
}

#[test]
fn test_cache_hit_rate_perfect() {
    let metrics = ProcessMetrics {
        signatures_generated: 0,
        processing_time_ms: 0.0,
        error_count: 0,
        cache_hits: 100,
        cache_misses: 0,
    };
    assert_eq!(metrics.cache_hit_rate(), 100.0);
}

#[test]
fn test_sigma_level_three_sigma() {
    // 0.27% error rate = 3 Sigma, DPMO = 2700
    let metrics = ProcessMetrics {
        signatures_generated: 10000,
        processing_time_ms: 1000.0,
        error_count: 27,
        cache_hits: 0,
        cache_misses: 0,
    };
    let sigma = metrics.sigma_level();
    // DPMO = 0.0027 * 1_000_000 = 2700, which is < 6000, so should be 3
    assert_eq!(sigma, 3.0);
}

#[test]
fn test_sigma_level_two_sigma() {
    // 4.5% error rate = 2 Sigma, DPMO = 45,000
    let metrics = ProcessMetrics {
        signatures_generated: 1000,
        processing_time_ms: 500.0,
        error_count: 45,
        cache_hits: 0,
        cache_misses: 0,
    };
    let sigma = metrics.sigma_level();
    // DPMO = 0.045 * 1_000_000 = 45000, which is < 66000, so should be 2
    assert_eq!(sigma, 2.0);
}

#[test]
fn test_sigma_level_perfect_success() {
    let metrics = ProcessMetrics {
        signatures_generated: 1000,
        processing_time_ms: 500.0,
        error_count: 0,
        cache_hits: 0,
        cache_misses: 0,
    };
    let sigma = metrics.sigma_level();
    assert!(sigma >= 5.0);
}

#[test]
fn test_throughput_sps() {
    let metrics = ProcessMetrics {
        signatures_generated: 100,
        processing_time_ms: 1000.0,
        error_count: 0,
        cache_hits: 0,
        cache_misses: 0,
    };
    let throughput = metrics.throughput_sps();
    assert!((throughput - 100.0).abs() < 0.01);
}

#[test]
fn test_throughput_zero_time() {
    let metrics = ProcessMetrics {
        signatures_generated: 100,
        processing_time_ms: 0.0,
        error_count: 0,
        cache_hits: 0,
        cache_misses: 0,
    };
    assert_eq!(metrics.throughput_sps(), 0.0);
}

#[test]
fn test_metrics_reset() {
    let mut metrics = ProcessMetrics {
        signatures_generated: 100,
        processing_time_ms: 500.0,
        error_count: 5,
        cache_hits: 80,
        cache_misses: 20,
    };
    metrics.reset();

    assert_eq!(metrics.signatures_generated, 0);
    assert_eq!(metrics.processing_time_ms, 0.0);
    assert_eq!(metrics.error_count, 0);
    assert_eq!(metrics.cache_hits, 0);
    assert_eq!(metrics.cache_misses, 0);
}

#[test]
fn test_metrics_summary() {
    let metrics = ProcessMetrics {
        signatures_generated: 100,
        processing_time_ms: 500.0,
        error_count: 2,
        cache_hits: 80,
        cache_misses: 20,
    };
    let summary = metrics.summary();
    assert!(summary.contains("100 signatures"));
    assert!(summary.contains("500"));
    assert!(summary.contains("2 errors"));
    assert!(summary.contains("Sigma"));
}

#[test]
fn test_timer_elapsed() {
    let timer = Timer::start();
    std::thread::sleep(std::time::Duration::from_millis(50));
    let elapsed = timer.elapsed_ms();
    assert!(elapsed >= 50.0);
    assert!(elapsed < 200.0);
}

#[test]
fn test_timer_stop() {
    let timer = Timer::start();
    std::thread::sleep(std::time::Duration::from_millis(50));
    let elapsed = timer.stop();
    assert!(elapsed >= 50.0);
    assert!(elapsed < 200.0);
}

#[test]
fn test_metrics_default() {
    let metrics = ProcessMetrics::default();
    assert_eq!(metrics.signatures_generated, 0);
    assert_eq!(metrics.processing_time_ms, 0.0);
    assert_eq!(metrics.error_count, 0);
    assert_eq!(metrics.cache_hits, 0);
    assert_eq!(metrics.cache_misses, 0);
}

#[test]
fn test_metrics_clone() {
    let metrics1 = ProcessMetrics {
        signatures_generated: 50,
        processing_time_ms: 250.0,
        error_count: 2,
        cache_hits: 40,
        cache_misses: 10,
    };
    let metrics2 = metrics1.clone();

    assert_eq!(metrics2.signatures_generated, 50);
    assert_eq!(metrics2.error_count, 2);
}

#[test]
fn test_sigma_level_one_sigma() {
    // ~31% error rate = 1 Sigma
    let metrics = ProcessMetrics {
        signatures_generated: 100,
        processing_time_ms: 500.0,
        error_count: 31,
        cache_hits: 0,
        cache_misses: 0,
    };
    let sigma = metrics.sigma_level();
    assert!(sigma >= 0.9 && sigma <= 1.1);
}

#[test]
fn test_sigma_level_high_quality() {
    // 0.006% error rate = 4 Sigma, DPMO = 60
    let metrics = ProcessMetrics {
        signatures_generated: 100000,
        processing_time_ms: 5000.0,
        error_count: 6,
        cache_hits: 0,
        cache_misses: 0,
    };
    let sigma = metrics.sigma_level();
    // DPMO = 0.00006 * 1_000_000 = 60, which is < 200, so should be 4
    assert_eq!(sigma, 4.0);
}
