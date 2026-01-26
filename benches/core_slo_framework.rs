use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId, Throughput};
use std::time::{Duration, Instant};
use std::collections::HashMap;
use std::fs;
use std::path::Path;

/// Core SLO targets for ggen project
pub struct SLOTargets {
    /// First build should complete in ≤ 15 seconds
    pub first_build_slo: Duration,
    /// Incremental build should complete in ≤ 2 seconds
    pub incremental_build_slo: Duration,
    /// RDF processing should handle 1k+ triples in ≤ 5 seconds
    pub rdf_processing_slo: Duration,
    /// Generation should use ≤ 100MB memory
    pub generation_memory_slo: usize,
    /// CLI scaffolding should complete in ≤ 3 seconds
    pub cli_scaffolding_slo: Duration,
    /// Binary size should be ≤ target (in bytes)
    pub binary_size_slo: usize,
}

impl Default for SLOTargets {
    fn default() -> Self {
        SLOTargets {
            first_build_slo: Duration::from_secs(15),
            incremental_build_slo: Duration::from_secs(2),
            rdf_processing_slo: Duration::from_secs(5),
            generation_memory_slo: 100 * 1024 * 1024, // 100MB
            cli_scaffolding_slo: Duration::from_secs(3),
            binary_size_slo: 500 * 1024 * 1024, // 500MB default
        }
    }
}

/// SLO tracking framework for collecting and analyzing metrics
pub struct SLOTracker {
    targets: SLOTargets,
    measurements: HashMap<String, Vec<Duration>>,
    memory_measurements: HashMap<String, Vec<usize>>,
    violations: Vec<SLOViolation>,
}

pub struct SLOViolation {
    pub metric_name: String,
    pub target: Duration,
    pub actual: Duration,
    pub percentage_over: f64,
}

impl SLOTracker {
    pub fn new(targets: SLOTargets) -> Self {
        SLOTracker {
            targets,
            measurements: HashMap::new(),
            memory_measurements: HashMap::new(),
            violations: Vec::new(),
        }
    }

    pub fn record_duration(&mut self, metric: &str, duration: Duration) {
        self.measurements
            .entry(metric.to_string())
            .or_insert_with(Vec::new)
            .push(duration);
    }

    pub fn record_memory(&mut self, metric: &str, bytes: usize) {
        self.memory_measurements
            .entry(metric.to_string())
            .or_insert_with(Vec::new)
            .push(bytes);
    }

    pub fn check_build_time_slo(&mut self, duration: Duration, is_incremental: bool) {
        let target = if is_incremental {
            self.targets.incremental_build_slo
        } else {
            self.targets.first_build_slo
        };

        if duration > target {
            let percentage_over = ((duration.as_millis() as f64 - target.as_millis() as f64)
                / target.as_millis() as f64)
                * 100.0;
            self.violations.push(SLOViolation {
                metric_name: if is_incremental {
                    "Incremental Build".to_string()
                } else {
                    "First Build".to_string()
                },
                target,
                actual: duration,
                percentage_over,
            });
        }
    }

    pub fn check_memory_slo(&mut self, bytes: usize) {
        if bytes > self.targets.generation_memory_slo {
            let percentage_over = ((bytes as f64 - self.targets.generation_memory_slo as f64)
                / self.targets.generation_memory_slo as f64)
                * 100.0;
            self.violations.push(SLOViolation {
                metric_name: "Memory Usage".to_string(),
                target: Duration::from_secs(self.targets.generation_memory_slo as u64),
                actual: Duration::from_secs(bytes as u64),
                percentage_over,
            });
        }
    }

    pub fn get_statistics(&self, metric: &str) -> Option<SLOStatistics> {
        self.measurements.get(metric).map(|measurements| {
            let sum: Duration = measurements.iter().sum();
            let mean = sum / measurements.len() as u32;
            let min = *measurements.iter().min().unwrap_or(&Duration::ZERO);
            let max = *measurements.iter().max().unwrap_or(&Duration::ZERO);

            SLOStatistics {
                count: measurements.len(),
                mean,
                min,
                max,
            }
        })
    }

    pub fn report_violations(&self) -> String {
        if self.violations.is_empty() {
            "✅ All SLOs met!".to_string()
        } else {
            let mut report = format!("❌ {} SLO violations detected:\n\n", self.violations.len());
            for violation in &self.violations {
                report.push_str(&format!(
                    "  {}\n    Target: {:?}\n    Actual: {:?}\n    Over: {:.1}%\n\n",
                    violation.metric_name, violation.target, violation.actual, violation.percentage_over
                ));
            }
            report
        }
    }
}

pub struct SLOStatistics {
    pub count: usize,
    pub mean: Duration,
    pub min: Duration,
    pub max: Duration,
}

// Benchmark suite for SLO framework
fn slo_framework_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("slo_framework");
    group.measurement_time(Duration::from_secs(5));

    group.bench_function("tracker_creation", |b| {
        b.iter(|| {
            SLOTracker::new(SLOTargets::default())
        })
    });

    group.bench_function("record_duration", |b| {
        let mut tracker = SLOTracker::new(SLOTargets::default());
        b.iter(|| {
            tracker.record_duration("test_metric", Duration::from_millis(100))
        })
    });

    group.bench_function("record_memory", |b| {
        let mut tracker = SLOTracker::new(SLOTargets::default());
        b.iter(|| {
            tracker.record_memory("test_metric", 50_000_000)
        })
    });

    group.bench_function("check_slo_violations", |b| {
        let mut tracker = SLOTracker::new(SLOTargets::default());
        b.iter(|| {
            tracker.check_build_time_slo(Duration::from_secs(3), false)
        })
    });

    group.bench_function("generate_report", |b| {
        let mut tracker = SLOTracker::new(SLOTargets::default());
        tracker.check_build_time_slo(Duration::from_secs(20), false);
        b.iter(|| {
            black_box(tracker.report_violations())
        })
    });

    group.finish();
}

// Benchmark for large-scale metric collection
fn large_scale_metrics(c: &mut Criterion) {
    let mut group = c.benchmark_group("large_scale_metrics");
    group.sample_size(100);

    group.bench_function("collect_1k_measurements", |b| {
        b.iter(|| {
            let mut tracker = SLOTracker::new(SLOTargets::default());
            for i in 0..1000 {
                tracker.record_duration(&format!("metric_{}", i % 10), Duration::from_millis(50));
            }
            tracker.measurements.len()
        })
    });

    group.bench_function("collect_10k_measurements", |b| {
        b.iter(|| {
            let mut tracker = SLOTracker::new(SLOTargets::default());
            for i in 0..10_000 {
                tracker.record_duration(&format!("metric_{}", i % 50), Duration::from_millis(50));
            }
            tracker.measurements.len()
        })
    });

    group.finish();
}

criterion_group!(benches, slo_framework_benchmark, large_scale_metrics);
criterion_main!(benches);
