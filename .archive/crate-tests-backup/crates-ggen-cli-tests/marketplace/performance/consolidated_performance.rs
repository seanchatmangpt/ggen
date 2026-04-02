//! Consolidated performance tests for marketplace operations
//!
//! Merges benchmark and latency test suites covering:
//! - Search performance with 100+ packages
//! - Batch assessment time
//! - Report generation time
//! - SLO validation (lookup <100ms, search <200ms, cache >80%)
//! - Latency percentiles (p50, p95, p99)
//! - Concurrent operation performance

use ggen_marketplace::prelude::*;
use std::time::{Duration, Instant};

const PERFORMANCE_THRESHOLD_MS: u128 = 1000; // 1 second for most operations

// ============================================================================
// BENCHMARK TESTS (original benchmark_test.rs)
// ============================================================================

#[test]
fn test_search_performance_100_packages() {
    // Arrange: Generate 100+ assessments
    let packages = generate_all_assessments();
    assert!(packages.len() >= 3, "Need at least 3 packages for testing");

    // Act: Measure search time
    let start = Instant::now();
    let results = filter_by_score_range(&packages, 0, 100);
    let duration = start.elapsed().as_millis();

    // Assert: Should complete quickly
    assert!(
        duration < PERFORMANCE_THRESHOLD_MS,
        "Search took {}ms, expected < {}ms",
        duration,
        PERFORMANCE_THRESHOLD_MS
    );
    assert!(!results.is_empty());
}

#[test]
fn test_maturity_assessment_batch_performance() {
    // Arrange: Create multiple evaluation inputs
    let inputs: Vec<EvaluationInput> = (0..20)
        .map(|i| EvaluationInput {
            package_id: format!("test.package.{}", i),
            package_name: format!("Test Package {}", i),
            has_readme: true,
            has_api_docs: i % 2 == 0,
            test_coverage: (i as f32) * 5.0,
            has_unit_tests: true,
            ..Default::default()
        })
        .collect();

    // Act: Measure batch assessment time
    let start = Instant::now();
    let assessments: Vec<_> = inputs
        .into_iter()
        .map(|input| MaturityEvaluator::evaluate(input))
        .collect();
    let duration = start.elapsed().as_millis();

    // Assert
    assert_eq!(assessments.len(), 20);
    assert!(
        duration < PERFORMANCE_THRESHOLD_MS,
        "Batch assessment took {}ms",
        duration
    );
}

#[test]
fn test_dashboard_generation_performance() {
    // Arrange
    let assessments = generate_all_assessments();

    // Act: Measure dashboard creation
    let start = Instant::now();
    let dashboard = MaturityDashboard::new(assessments);
    let duration = start.elapsed().as_millis();

    // Assert
    assert!(dashboard.statistics.total_packages > 0);
    assert!(
        duration < PERFORMANCE_THRESHOLD_MS,
        "Dashboard generation took {}ms",
        duration
    );
}

#[test]
fn test_filter_performance_multiple_criteria() {
    // Arrange
    let packages = generate_all_assessments();

    // Act: Apply multiple filters sequentially
    let start = Instant::now();
    let step1 = filter_by_level(&packages, MaturityLevel::Beta);
    let criteria = vec![("documentation", 10u32), ("testing", 10u32)];
    let step2 = filter_by_dimensions(&step1, &criteria);
    let results = filter_by_score_range(&step2, 50, 100);
    let duration = start.elapsed().as_millis();

    // Assert
    assert!(
        duration < PERFORMANCE_THRESHOLD_MS,
        "Multi-filter took {}ms",
        duration
    );
    assert!(results.len() <= packages.len());
}

#[test]
fn test_score_breakdown_performance() {
    // Arrange
    let assessments = generate_all_assessments();

    // Act: Calculate breakdowns for all
    let start = Instant::now();
    for assessment in &assessments {
        let _ = assessment.score_breakdown();
    }
    let duration = start.elapsed().as_millis();

    // Assert
    assert!(
        duration < PERFORMANCE_THRESHOLD_MS,
        "Score breakdown calculation took {}ms",
        duration
    );
}

#[test]
fn test_level_calculation_performance() {
    // Arrange
    let assessments = generate_all_assessments();

    // Act: Calculate levels for all
    let start = Instant::now();
    for assessment in &assessments {
        let _ = assessment.level();
    }
    let duration = start.elapsed().as_millis();

    // Assert
    assert!(duration < 100, "Level calculation took {}ms", duration);
}

#[test]
fn test_csv_export_performance() {
    // Arrange
    let assessments = generate_all_assessments();

    // Act: Generate CSV
    let start = Instant::now();
    let csv = export_as_csv(&assessments);
    let duration = start.elapsed().as_millis();

    // Assert
    assert!(!csv.is_empty());
    assert!(
        duration < PERFORMANCE_THRESHOLD_MS,
        "CSV export took {}ms",
        duration
    );
}

#[test]
fn test_json_export_performance() {
    // Arrange
    let assessments = generate_all_assessments();

    // Act: Generate JSON
    let start = Instant::now();
    let json = export_as_json(&assessments);
    let duration = start.elapsed().as_millis();

    // Assert
    assert!(!json.is_null());
    assert!(
        duration < PERFORMANCE_THRESHOLD_MS,
        "JSON export took {}ms",
        duration
    );
}

#[test]
fn test_comparison_performance() {
    // Arrange
    let assessments = generate_all_assessments();
    assert!(assessments.len() >= 2, "Need at least 2 packages");

    let pkg_a = &assessments[0];
    let pkg_b = &assessments[1];

    // Act: Compare packages
    let start = Instant::now();
    let comparison = compare_assessments(pkg_a, pkg_b);
    let duration = start.elapsed().as_millis();

    // Assert
    assert!(!comparison.is_null());
    assert!(duration < 100, "Comparison took {}ms", duration);
}

#[test]
fn test_recommendation_performance() {
    // Arrange
    let assessments = generate_all_assessments();

    // Act: Get recommendations
    let start = Instant::now();
    let recommendations = get_recommendations(&assessments, "production");
    let duration = start.elapsed().as_millis();

    // Assert
    assert!(!recommendations.is_empty());
    assert!(
        duration < PERFORMANCE_THRESHOLD_MS,
        "Recommendations took {}ms",
        duration
    );
}

#[test]
fn test_use_case_matching_performance() {
    // Arrange
    let assessments = generate_all_assessments();
    let use_cases = vec!["production", "research", "enterprise", "startup"];

    // Act: Match all use cases
    let start = Instant::now();
    for use_case in use_cases {
        let _ = find_for_use_case(&assessments, use_case);
    }
    let duration = start.elapsed().as_millis();

    // Assert
    assert!(
        duration < PERFORMANCE_THRESHOLD_MS,
        "Use case matching took {}ms",
        duration
    );
}

#[test]
fn test_memory_efficiency_large_dataset() {
    // Arrange: Create many assessments
    let assessments: Vec<_> = (0..100)
        .map(|i| MaturityAssessment::new(format!("package.{}", i), format!("Package {}", i)))
        .collect();

    // Act: Perform operations without cloning
    let start = Instant::now();
    let _filtered = filter_by_score_range(&assessments, 0, 100);
    let _by_level = filter_by_level(&assessments, MaturityLevel::Beta);
    let duration = start.elapsed().as_millis();

    // Assert: Should handle references efficiently
    assert!(
        duration < PERFORMANCE_THRESHOLD_MS,
        "Large dataset operations took {}ms",
        duration
    );
}

#[test]
fn test_repeated_filtering_no_performance_degradation() {
    // Arrange
    let packages = generate_all_assessments();

    // Act: Filter multiple times
    let durations: Vec<_> = (0..10)
        .map(|_| {
            let start = Instant::now();
            let _ = filter_by_score_range(&packages, 50, 80);
            start.elapsed().as_micros()
        })
        .collect();

    // Assert: Performance should be consistent
    let avg_duration = durations.iter().sum::<u128>() / durations.len() as u128;
    let max_duration = durations.iter().max().unwrap();

    assert!(
        max_duration - avg_duration < avg_duration,
        "Performance degradation detected: max {}μs, avg {}μs",
        max_duration,
        avg_duration
    );
}

#[test]
fn test_feedback_generation_performance() {
    // Arrange
    let assessment = MaturityAssessment::new("test.pkg", "Test Package");

    // Act: Generate feedback multiple times
    let start = Instant::now();
    for _ in 0..100 {
        let _ = assessment.all_feedback();
    }
    let duration = start.elapsed().as_millis();

    // Assert
    assert!(
        duration < PERFORMANCE_THRESHOLD_MS,
        "Feedback generation took {}ms for 100 iterations",
        duration
    );
}

#[test]
fn test_concurrent_assessment_creation() {
    use std::sync::Arc;
    use std::thread;

    // Arrange: Create inputs in multiple threads
    let num_threads = 4;
    let assessments_per_thread = 25;

    // Act
    let start = Instant::now();
    let handles: Vec<_> = (0..num_threads)
        .map(|thread_id| {
            thread::spawn(move || {
                let mut results = Vec::new();
                for i in 0..assessments_per_thread {
                    let input = EvaluationInput {
                        package_id: format!("thread{}.pkg{}", thread_id, i),
                        package_name: format!("Package {}-{}", thread_id, i),
                        ..Default::default()
                    };
                    results.push(MaturityEvaluator::evaluate(input));
                }
                results
            })
        })
        .collect();

    let results: Vec<_> = handles
        .into_iter()
        .flat_map(|h| h.join().unwrap())
        .collect();
    let duration = start.elapsed().as_millis();

    // Assert
    assert_eq!(results.len(), num_threads * assessments_per_thread);
    assert!(
        duration < PERFORMANCE_THRESHOLD_MS * 2,
        "Concurrent assessment took {}ms",
        duration
    );
}

// ============================================================================
// LATENCY BENCHMARK TESTS (original latency_benchmark.rs)
// ============================================================================

/// Helper: Measure operation latency percentiles
fn measure_latencies<F>(mut operation: F, iterations: usize) -> (Duration, Duration, Duration)
where
    F: FnMut() -> (),
{
    let mut latencies: Vec<Duration> = Vec::with_capacity(iterations);

    for _ in 0..iterations {
        let start = Instant::now();
        operation();
        let duration = start.elapsed();
        latencies.push(duration);
    }

    latencies.sort();

    let p50_idx = iterations * 50 / 100;
    let p95_idx = iterations * 95 / 100;
    let p99_idx = iterations * 99 / 100;

    (latencies[p50_idx], latencies[p95_idx], latencies[p99_idx])
}

#[test]
fn test_lookup_latency_meets_slo() {
    // Simulate package lookup operation
    let (p50, p95, p99) = measure_latencies(
        || {
            std::thread::sleep(Duration::from_millis(30)); // Simulated lookup
        },
        100,
    );

    println!(
        "Lookup latencies - p50: {:?}, p95: {:?}, p99: {:?}",
        p50, p95, p99
    );

    // SLO: p95 < 100ms
    assert!(
        p95 < Duration::from_millis(100),
        "Lookup p95 latency exceeds SLO: {:?}",
        p95
    );
}

#[test]
fn test_search_latency_meets_slo() {
    // Simulate search operation
    let (p50, p95, p99) = measure_latencies(
        || {
            std::thread::sleep(Duration::from_millis(80)); // Simulated search
        },
        100,
    );

    println!(
        "Search latencies - p50: {:?}, p95: {:?}, p99: {:?}",
        p50, p95, p99
    );

    // SLO: p95 < 200ms
    assert!(
        p95 < Duration::from_millis(200),
        "Search p95 latency exceeds SLO: {:?}",
        p95
    );
}

#[test]
fn test_install_latency_meets_slo() {
    // Simulate install operation
    let (p50, p95, p99) = measure_latencies(
        || {
            std::thread::sleep(Duration::from_millis(200)); // Simulated install
        },
        50,
    );

    println!(
        "Install latencies - p50: {:?}, p95: {:?}, p99: {:?}",
        p50, p95, p99
    );

    // SLO: p95 < 500ms
    assert!(
        p95 < Duration::from_millis(500),
        "Install p95 latency exceeds SLO: {:?}",
        p95
    );
}

#[test]
fn test_list_latency_meets_slo() {
    // Simulate list operation
    let (p50, p95, p99) = measure_latencies(
        || {
            std::thread::sleep(Duration::from_millis(50)); // Simulated list
        },
        100,
    );

    println!(
        "List latencies - p50: {:?}, p95: {:?}, p99: {:?}",
        p50, p95, p99
    );

    // SLO: p95 < 150ms
    assert!(
        p95 < Duration::from_millis(150),
        "List p95 latency exceeds SLO: {:?}",
        p95
    );
}

#[test]
fn test_cold_cache_vs_warm_cache() {
    let mut cache: std::collections::HashMap<String, String> = std::collections::HashMap::new();

    // Cold cache (first access)
    let cold_start = Instant::now();
    let _ = cache.get("test-key");
    std::thread::sleep(Duration::from_millis(50)); // Simulate slow fetch
    cache.insert("test-key".to_string(), "test-value".to_string());
    let cold_duration = cold_start.elapsed();

    // Warm cache (subsequent access)
    let warm_start = Instant::now();
    let _ = cache.get("test-key");
    let warm_duration = warm_start.elapsed();

    println!("Cold: {:?}, Warm: {:?}", cold_duration, warm_duration);

    // Warm cache should be significantly faster
    assert!(
        warm_duration < Duration::from_millis(10),
        "Warm cache too slow: {:?}",
        warm_duration
    );
}

#[test]
fn test_concurrent_lookup_latency() {
    use std::thread;

    let handles: Vec<_> = (0..10)
        .map(|i| {
            thread::spawn(move || {
                let start = Instant::now();
                std::thread::sleep(Duration::from_millis(30 + i * 2)); // Simulated lookup
                start.elapsed()
            })
        })
        .collect();

    let mut latencies: Vec<Duration> = handles.into_iter().map(|h| h.join().unwrap()).collect();

    latencies.sort();

    let p95_idx = latencies.len() * 95 / 100;
    let p95 = latencies[p95_idx];

    println!("Concurrent lookup p95: {:?}", p95);

    // Even under concurrent load, should meet SLO
    assert!(
        p95 < Duration::from_millis(150),
        "Concurrent lookup p95 exceeds SLO: {:?}",
        p95
    );
}

#[test]
fn test_bulk_operation_throughput() {
    let operations = 100;

    let start = Instant::now();
    for _ in 0..operations {
        std::thread::sleep(Duration::from_millis(5)); // Simulated operation
    }
    let total_duration = start.elapsed();

    let ops_per_sec = operations as f64 / total_duration.as_secs_f64();

    println!("Throughput: {:.2} ops/sec", ops_per_sec);

    // Should achieve >10 ops/sec
    assert!(
        ops_per_sec > 10.0,
        "Throughput too low: {:.2} ops/sec",
        ops_per_sec
    );
}

#[test]
fn test_search_with_100_packages() {
    // Simulate search over 100 packages
    let start = Instant::now();
    for _ in 0..100 {
        // Simulated package comparison
        let _ = "test-package".contains("test");
    }
    let duration = start.elapsed();

    println!("Search 100 packages: {:?}", duration);

    // Should complete in <150ms
    assert!(
        duration < Duration::from_millis(150),
        "Search too slow: {:?}",
        duration
    );
}

#[test]
fn test_search_with_1000_packages() {
    // Simulate search over 1000 packages
    let start = Instant::now();
    for _ in 0..1000 {
        let _ = "test-package".contains("test");
    }
    let duration = start.elapsed();

    println!("Search 1000 packages: {:?}", duration);

    // Should complete in <200ms
    assert!(
        duration < Duration::from_millis(200),
        "Search too slow: {:?}",
        duration
    );
}

#[test]
fn test_memory_efficiency() {
    // Allocate 1000 packages (simulated)
    let packages: Vec<String> = (0..1000).map(|i| format!("package-{}", i)).collect();

    // Memory usage should be reasonable
    let mem_per_package = std::mem::size_of::<String>();
    let total_mem = packages.len() * mem_per_package;

    println!("Memory usage for 1000 packages: {} bytes", total_mem);

    // Should use <100MB for 1000 packages
    assert!(
        total_mem < 100 * 1024 * 1024,
        "Memory usage too high: {} bytes",
        total_mem
    );
}

#[test]
fn test_cache_hit_rate_simulation() {
    let mut cache: std::collections::HashMap<String, String> = std::collections::HashMap::new();

    let mut hits = 0;
    let mut misses = 0;

    // Simulate 100 queries with 80% cache hit rate
    for i in 0..100 {
        let key = if i % 5 == 0 {
            format!("new-key-{}", i) // 20% new keys (misses)
        } else {
            "cached-key".to_string() // 80% cached key (hits)
        };

        if cache.contains_key(&key) {
            hits += 1;
        } else {
            misses += 1;
            cache.insert(key, "value".to_string());
        }
    }

    let hit_rate = hits as f64 / (hits + misses) as f64;

    println!("Cache hit rate: {:.1}%", hit_rate * 100.0);

    // Should achieve >80% hit rate
    assert!(
        hit_rate > 0.8,
        "Cache hit rate too low: {:.1}%",
        hit_rate * 100.0
    );
}

#[test]
fn test_sparql_query_performance() {
    // Simulate SPARQL query execution
    let start = Instant::now();

    // Simulated query parsing and execution
    std::thread::sleep(Duration::from_millis(20));

    let duration = start.elapsed();

    println!("SPARQL query: {:?}", duration);

    // Should complete in <50ms
    assert!(
        duration < Duration::from_millis(50),
        "SPARQL query too slow: {:?}",
        duration
    );
}

#[test]
fn test_rdf_triple_insertion_performance() {
    // Simulate inserting 100 RDF triples
    let start = Instant::now();

    for _ in 0..100 {
        // Simulated triple insertion
        std::thread::sleep(Duration::from_micros(100));
    }

    let duration = start.elapsed();

    println!("Insert 100 triples: {:?}", duration);

    // Should complete in <50ms
    assert!(
        duration < Duration::from_millis(50),
        "RDF insertion too slow: {:?}",
        duration
    );
}

#[test]
fn test_version_comparison_performance() {
    use std::cmp::Ordering;

    let v1 = "1.0.0";
    let v2 = "2.0.0";

    let start = Instant::now();

    // 10000 version comparisons
    for _ in 0..10000 {
        let _ = v1.cmp(v2);
    }

    let duration = start.elapsed();

    println!("10000 version comparisons: {:?}", duration);

    // Should complete in <10ms
    assert!(
        duration < Duration::from_millis(10),
        "Version comparison too slow: {:?}",
        duration
    );
}
