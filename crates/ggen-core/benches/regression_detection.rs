// Regression Detection Benchmarks
// Establishes baselines and detects performance regressions

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use std::time::Duration;

// ============================================================================
// BASELINE PERFORMANCE TARGETS
// ============================================================================

const ERROR_FIX_LATENCY_TARGET_MS: u128 = 50;
const BUILDER_CONSTRUCTION_TARGET_US: u128 = 1;
const FIXTURE_CREATION_TARGET_MS: u128 = 5;
const SETUP_TEARDOWN_TARGET_MS: u128 = 10;
const TEST_EXECUTION_TARGET_MS: u128 = 100;
const SCORE_CALCULATION_TARGET_MS: u128 = 1;
const OBSERVABILITY_OVERHEAD_PERCENT: f64 = 5.0;
const QUALITY_REPORT_TARGET_MS: u128 = 100;
const RPN_CALCULATION_TARGET_US: u128 = 1;
const DISTRIBUTION_ANALYSIS_TARGET_US: u128 = 100;
const PRIORITY_RANKING_TARGET_MS: u128 = 1;

// ============================================================================
// REGRESSION DETECTION UTILITIES
// ============================================================================

/// Check if duration meets target
fn meets_target(duration: Duration, target_ms: u128) -> bool {
    duration.as_millis() <= target_ms
}

/// Check if duration meets microsecond target
fn meets_us_target(duration: Duration, target_us: u128) -> bool {
    duration.as_micros() <= target_us
}

/// Calculate performance regression percentage
fn calculate_regression(actual: Duration, baseline: Duration) -> f64 {
    if baseline.as_nanos() == 0 {
        return 0.0;
    }

    let actual_ns = actual.as_nanos() as f64;
    let baseline_ns = baseline.as_nanos() as f64;

    ((actual_ns - baseline_ns) / baseline_ns) * 100.0
}

// ============================================================================
// ERROR FIX PATTERN REGRESSION TESTS
// ============================================================================

mod error_fix_regression {
    use super::*;

    fn e0277_fix() {
        trait NeedsClone: Clone {}

        #[derive(Clone)]
        struct FixedType {
            value: i32,
        }

        impl NeedsClone for FixedType {}

        let instance = FixedType { value: 42 };
        let _cloned = instance.clone();
    }

    pub fn detect_regressions(c: &mut Criterion) {
        let mut group = c.benchmark_group("error_fix_regression");
        group.sample_size(1000);
        group.measurement_time(Duration::from_secs(10));

        group.bench_function("E0277_fix_latency_check", |b| {
            b.iter_custom(|iters| {
                let start = std::time::Instant::now();
                for _ in 0..iters {
                    e0277_fix();
                }
                let elapsed = start.elapsed();

                let avg_latency = elapsed / iters as u32;
                assert!(
                    meets_target(avg_latency, ERROR_FIX_LATENCY_TARGET_MS),
                    "E0277 fix exceeded {}ms target: {:?}",
                    ERROR_FIX_LATENCY_TARGET_MS,
                    avg_latency
                );

                elapsed
            });
        });

        group.finish();
    }
}

// ============================================================================
// POKA-YOKE PATTERN REGRESSION TESTS
// ============================================================================

mod poka_yoke_regression {
    use super::*;

    #[derive(Default)]
    struct TypeSafeBuilder {
        field1: Option<i32>,
        field2: Option<String>,
    }

    impl TypeSafeBuilder {
        fn new() -> Self {
            Self::default()
        }

        fn field1(mut self, value: i32) -> Self {
            self.field1 = Some(value);
            self
        }

        fn field2(mut self, value: String) -> Self {
            self.field2 = Some(value);
            self
        }

        fn build(self) -> (i32, String) {
            (self.field1.unwrap(), self.field2.unwrap())
        }
    }

    pub fn detect_regressions(c: &mut Criterion) {
        let mut group = c.benchmark_group("poka_yoke_regression");

        group.bench_function("builder_construction_us_check", |b| {
            b.iter_custom(|iters| {
                let start = std::time::Instant::now();
                for _ in 0..iters {
                    let _built = TypeSafeBuilder::new()
                        .field1(black_box(42))
                        .field2(black_box("test".to_string()))
                        .build();
                }
                let elapsed = start.elapsed();

                let avg_latency = elapsed / iters as u32;
                assert!(
                    meets_us_target(avg_latency, BUILDER_CONSTRUCTION_TARGET_US),
                    "Builder construction exceeded {}μs target: {:?}",
                    BUILDER_CONSTRUCTION_TARGET_US,
                    avg_latency
                );

                elapsed
            });
        });

        group.finish();
    }
}

// ============================================================================
// LEAN TEST PATTERN REGRESSION TESTS
// ============================================================================

mod lean_test_regression {
    use super::*;

    struct TestFixture {
        data: Vec<i32>,
    }

    impl TestFixture {
        fn new() -> Self {
            Self {
                data: vec![1, 2, 3, 4, 5],
            }
        }
    }

    fn setup() -> Vec<i32> {
        vec![1, 2, 3, 4, 5]
    }

    fn teardown(_data: Vec<i32>) {}

    fn run_test(data: &[i32]) -> i32 {
        data.iter().sum()
    }

    pub fn detect_regressions(c: &mut Criterion) {
        let mut group = c.benchmark_group("lean_test_regression");

        // Fixture creation check
        group.bench_function("fixture_creation_ms_check", |b| {
            b.iter_custom(|iters| {
                let start = std::time::Instant::now();
                for _ in 0..iters {
                    let _fixture = TestFixture::new();
                }
                let elapsed = start.elapsed();

                let avg_latency = elapsed / iters as u32;
                assert!(
                    meets_target(avg_latency, FIXTURE_CREATION_TARGET_MS),
                    "Fixture creation exceeded {}ms target: {:?}",
                    FIXTURE_CREATION_TARGET_MS,
                    avg_latency
                );

                elapsed
            });
        });

        // Setup/teardown check
        group.bench_function("setup_teardown_ms_check", |b| {
            b.iter_custom(|iters| {
                let start = std::time::Instant::now();
                for _ in 0..iters {
                    let data = setup();
                    let _result = run_test(&data);
                    teardown(data);
                }
                let elapsed = start.elapsed();

                let avg_latency = elapsed / iters as u32;
                assert!(
                    meets_target(avg_latency, SETUP_TEARDOWN_TARGET_MS),
                    "Setup/teardown exceeded {}ms target: {:?}",
                    SETUP_TEARDOWN_TARGET_MS,
                    avg_latency
                );

                elapsed
            });
        });

        group.finish();
    }
}

// ============================================================================
// GEMBA WALK PATTERN REGRESSION TESTS
// ============================================================================

mod gemba_walk_regression {
    use super::*;

    #[derive(Debug)]
    struct TestScore {
        coverage: f64,
        clarity: f64,
        isolation: f64,
        speed: f64,
    }

    impl TestScore {
        fn calculate(test_name: &str, execution_time_ms: u64, has_assertions: bool) -> Self {
            let coverage = if has_assertions { 0.8 } else { 0.3 };
            let clarity = (test_name.len() as f64 / 100.0).min(1.0);
            let isolation = if execution_time_ms < 100 { 1.0 } else { 0.5 };
            let speed = 1.0 - (execution_time_ms as f64 / 1000.0).min(1.0);

            Self {
                coverage,
                clarity,
                isolation,
                speed,
            }
        }

        fn total(&self) -> f64 {
            (self.coverage + self.clarity + self.isolation + self.speed) / 4.0
        }
    }

    fn generate_quality_report(scores: &[TestScore]) -> String {
        let avg_score = scores.iter().map(|s| s.total()).sum::<f64>() / scores.len() as f64;
        format!("Average quality score: {:.2}", avg_score)
    }

    pub fn detect_regressions(c: &mut Criterion) {
        let mut group = c.benchmark_group("gemba_walk_regression");

        // Score calculation check
        group.bench_function("score_calculation_ms_check", |b| {
            b.iter_custom(|iters| {
                let start = std::time::Instant::now();
                for _ in 0..iters {
                    let _score = TestScore::calculate(
                        black_box("test_comprehensive_validation"),
                        black_box(50),
                        black_box(true),
                    );
                }
                let elapsed = start.elapsed();

                let avg_latency = elapsed / iters as u32;
                assert!(
                    meets_target(avg_latency, SCORE_CALCULATION_TARGET_MS),
                    "Score calculation exceeded {}ms target: {:?}",
                    SCORE_CALCULATION_TARGET_MS,
                    avg_latency
                );

                elapsed
            });
        });

        // Quality report generation check
        let scores: Vec<TestScore> = (0..100)
            .map(|i| TestScore::calculate(&format!("test_{}", i), 50, true))
            .collect();

        group.bench_function("quality_report_100_tests_ms_check", |b| {
            b.iter_custom(|iters| {
                let start = std::time::Instant::now();
                for _ in 0..iters {
                    let _report = generate_quality_report(black_box(&scores));
                }
                let elapsed = start.elapsed();

                let avg_latency = elapsed / iters as u32;
                assert!(
                    meets_target(avg_latency, QUALITY_REPORT_TARGET_MS),
                    "Quality report exceeded {}ms target: {:?}",
                    QUALITY_REPORT_TARGET_MS,
                    avg_latency
                );

                elapsed
            });
        });

        group.finish();
    }
}

// ============================================================================
// FMEA PATTERN REGRESSION TESTS
// ============================================================================

mod fmea_regression {
    use super::*;

    #[derive(Debug, Clone)]
    struct ErrorRPN {
        severity: u32,
        occurrence: u32,
        detection: u32,
    }

    impl ErrorRPN {
        fn calculate(&self) -> u32 {
            self.severity * self.occurrence * self.detection
        }

        fn priority(&self) -> &'static str {
            match self.calculate() {
                0..=100 => "Low",
                101..=500 => "Medium",
                _ => "High",
            }
        }
    }

    fn analyze_distribution(errors: &[ErrorRPN]) -> (u32, u32, u32) {
        let mut low = 0;
        let mut medium = 0;
        let mut high = 0;

        for error in errors {
            match error.priority() {
                "Low" => low += 1,
                "Medium" => medium += 1,
                "High" => high += 1,
                _ => {}
            }
        }

        (low, medium, high)
    }

    fn rank_by_priority(errors: &[ErrorRPN]) -> Vec<(usize, u32)> {
        let mut ranked: Vec<(usize, u32)> = errors
            .iter()
            .enumerate()
            .map(|(i, e)| (i, e.calculate()))
            .collect();

        ranked.sort_by(|a, b| b.1.cmp(&a.1));
        ranked
    }

    pub fn detect_regressions(c: &mut Criterion) {
        let mut group = c.benchmark_group("fmea_regression");

        // RPN calculation check
        group.bench_function("rpn_calculation_us_check", |b| {
            b.iter_custom(|iters| {
                let error = ErrorRPN {
                    severity: 8,
                    occurrence: 6,
                    detection: 4,
                };

                let start = std::time::Instant::now();
                for _ in 0..iters {
                    let _rpn = error.calculate();
                }
                let elapsed = start.elapsed();

                let avg_latency = elapsed / iters as u32;
                assert!(
                    meets_us_target(avg_latency, RPN_CALCULATION_TARGET_US),
                    "RPN calculation exceeded {}μs target: {:?}",
                    RPN_CALCULATION_TARGET_US,
                    avg_latency
                );

                elapsed
            });
        });

        // Distribution analysis check
        let errors: Vec<ErrorRPN> = (0..252)
            .map(|i| ErrorRPN {
                severity: (i % 10) + 1,
                occurrence: ((i / 10) % 10) + 1,
                detection: ((i / 100) % 10) + 1,
            })
            .collect();

        group.bench_function("distribution_analysis_252_errors_us_check", |b| {
            b.iter_custom(|iters| {
                let start = std::time::Instant::now();
                for _ in 0..iters {
                    let _distribution = analyze_distribution(black_box(&errors));
                }
                let elapsed = start.elapsed();

                let avg_latency = elapsed / iters as u32;
                assert!(
                    meets_us_target(avg_latency, DISTRIBUTION_ANALYSIS_TARGET_US),
                    "Distribution analysis exceeded {}μs target: {:?}",
                    DISTRIBUTION_ANALYSIS_TARGET_US,
                    avg_latency
                );

                elapsed
            });
        });

        // Priority ranking check
        group.bench_function("priority_ranking_ms_check", |b| {
            b.iter_custom(|iters| {
                let start = std::time::Instant::now();
                for _ in 0..iters {
                    let _ranked = rank_by_priority(black_box(&errors));
                }
                let elapsed = start.elapsed();

                let avg_latency = elapsed / iters as u32;
                assert!(
                    meets_target(avg_latency, PRIORITY_RANKING_TARGET_MS),
                    "Priority ranking exceeded {}ms target: {:?}",
                    PRIORITY_RANKING_TARGET_MS,
                    avg_latency
                );

                elapsed
            });
        });

        group.finish();
    }
}

// ============================================================================
// BENCHMARK GROUPS
// ============================================================================

criterion_group!(
    benches,
    error_fix_regression::detect_regressions,
    poka_yoke_regression::detect_regressions,
    lean_test_regression::detect_regressions,
    gemba_walk_regression::detect_regressions,
    fmea_regression::detect_regressions,
);

criterion_main!(benches);
