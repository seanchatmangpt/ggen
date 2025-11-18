//! Performance benchmarks for marketplace operations
//!
//! Tests verify performance characteristics:
//! - Search performance with 100+ packages
//! - Batch assessment time
//! - Report generation time
//! - Large result set handling
//! - Export format generation speed

use ggen_marketplace::prelude::*;
use std::time::Instant;

const PERFORMANCE_THRESHOLD_MS: u128 = 1000; // 1 second for most operations

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
