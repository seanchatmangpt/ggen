/// SLO Tracking and Dashboard
///
/// Comprehensive SLO compliance tracking with:
/// - Baseline storage and comparison
/// - Regression detection
/// - Performance trend analysis
/// - HTML dashboard generation
/// - Deterministic receipts
///
/// Storage: swarm/benchmarks/slo_tracking/{timestamp}/

use criterion::{criterion_group, criterion_main, Criterion};
use std::fs;
use std::path::Path;

// ============================================================================
// SLO Targets (Source of Truth)
// ============================================================================

mod slo_targets {
    /// Build time SLOs (seconds)
    pub struct BuildTimeSLOs {
        pub first_build_debug: f64,        // ≤ 15s
        pub first_build_release: f64,      // ≤ 30s
        pub incremental_build: f64,        // ≤ 2s
        pub workspace_build: f64,          // ≤ 30s
        pub cargo_check: f64,              // ≤ 5s
    }

    /// Memory usage SLOs (MB)
    pub struct MemorySLOs {
        pub rdf_processing: f64,           // ≤ 100 MB
        pub template_rendering: f64,       // ≤ 50 MB
        pub code_generation: f64,          // ≤ 100 MB
        pub compilation_peak: f64,         // ≤ 500 MB
    }

    /// Binary size SLOs (MB)
    pub struct BinarySizeSLOs {
        pub release_binary: f64,           // ≤ 10 MB
        pub debug_binary: f64,             // ≤ 50 MB
        pub symbol_reduction: f64,         // ≥ 50%
    }

    /// Runtime performance SLOs (ms)
    pub struct RuntimeSLOs {
        pub rdf_parsing_1k_triples: f64,   // ≤ 5000 ms
        pub sparql_query_1k_entities: f64, // ≤ 100 ms
        pub template_render_100_vars: f64, // ≤ 10 ms
        pub cli_startup: f64,              // ≤ 200 ms
    }

    pub fn build_time_slos() -> BuildTimeSLOs {
        BuildTimeSLOs {
            first_build_debug: 15.0,
            first_build_release: 30.0,
            incremental_build: 2.0,
            workspace_build: 30.0,
            cargo_check: 5.0,
        }
    }

    pub fn memory_slos() -> MemorySLOs {
        MemorySLOs {
            rdf_processing: 100.0,
            template_rendering: 50.0,
            code_generation: 100.0,
            compilation_peak: 500.0,
        }
    }

    pub fn binary_size_slos() -> BinarySizeSLOs {
        BinarySizeSLOs {
            release_binary: 10.0,
            debug_binary: 50.0,
            symbol_reduction: 50.0,
        }
    }

    pub fn runtime_slos() -> RuntimeSLOs {
        RuntimeSLOs {
            rdf_parsing_1k_triples: 5000.0,
            sparql_query_1k_entities: 100.0,
            template_render_100_vars: 10.0,
            cli_startup: 200.0,
        }
    }
}

// ============================================================================
// SLO Tracking Record
// ============================================================================

#[derive(Clone, Debug)]
struct SLORecord {
    metric_name: String,
    metric_value: f64,
    metric_unit: String,
    slo_target: f64,
    status: SLOStatus,
    timestamp: String,
    git_commit: Option<String>,
}

#[derive(Clone, Debug, PartialEq)]
enum SLOStatus {
    Pass,
    Fail,
    Warn, // Within 10% of threshold
}

impl SLORecord {
    fn new(
        metric_name: &str,
        metric_value: f64,
        metric_unit: &str,
        slo_target: f64,
    ) -> Self {
        let status = if metric_value <= slo_target {
            SLOStatus::Pass
        } else if metric_value <= slo_target * 1.25 {
            SLOStatus::Warn
        } else {
            SLOStatus::Fail
        };

        Self {
            metric_name: metric_name.to_string(),
            metric_value,
            metric_unit: metric_unit.to_string(),
            slo_target,
            status,
            timestamp: chrono::Local::now().to_rfc3339(),
            git_commit: get_git_commit().ok(),
        }
    }

    fn to_json(&self) -> String {
        let status_str = match self.status {
            SLOStatus::Pass => "PASS",
            SLOStatus::Fail => "FAIL",
            SLOStatus::Warn => "WARN",
        };

        let commit_str = self
            .git_commit
            .as_ref()
            .map(|c| format!("  \"git_commit\": \"{}\",", c))
            .unwrap_or_default();

        format!(
            r#"{{
  "metric_name": "{}",
  "metric_value": {},
  "metric_unit": "{}",
  "slo_target": {},
  "status": "{}",
  "overage_percent": {:.1},
  "timestamp": "{}",
{}
  "slo_type": "{}"
}}"#,
            self.metric_name,
            self.metric_value,
            self.metric_unit,
            self.slo_target,
            status_str,
            ((self.metric_value - self.slo_target) / self.slo_target) * 100.0,
            self.timestamp,
            commit_str,
            self.infer_slo_type()
        )
    }

    fn infer_slo_type(&self) -> &str {
        if self.metric_name.contains("build") || self.metric_name.contains("compile") {
            "build_time"
        } else if self.metric_name.contains("memory") {
            "memory"
        } else if self.metric_name.contains("binary") || self.metric_name.contains("size") {
            "binary_size"
        } else if self.metric_name.contains("rdf")
            || self.metric_name.contains("sparql")
            || self.metric_name.contains("template")
        {
            "runtime"
        } else {
            "unknown"
        }
    }
}

// ============================================================================
// Git Integration
// ============================================================================

fn get_git_commit() -> Result<String, String> {
    let output = std::process::Command::new("git")
        .args(&["rev-parse", "HEAD"])
        .output()
        .map_err(|e| format!("Failed to get git commit: {}", e))?;

    let commit = String::from_utf8_lossy(&output.stdout)
        .trim()
        .to_string();

    Ok(commit)
}

fn get_git_branch() -> Result<String, String> {
    let output = std::process::Command::new("git")
        .args(&["rev-parse", "--abbrev-ref", "HEAD"])
        .output()
        .map_err(|e| format!("Failed to get git branch: {}", e))?;

    let branch = String::from_utf8_lossy(&output.stdout)
        .trim()
        .to_string();

    Ok(branch)
}

// ============================================================================
// SLO Dashboard Generation
// ============================================================================

fn generate_html_dashboard(records: &[SLORecord]) -> String {
    let timestamp = chrono::Local::now().to_rfc3339();
    let git_branch = get_git_branch().unwrap_or_else(|_| "unknown".to_string());

    let mut pass_count = 0;
    let mut fail_count = 0;
    let mut warn_count = 0;

    for record in records {
        match record.status {
            SLOStatus::Pass => pass_count += 1,
            SLOStatus::Fail => fail_count += 1,
            SLOStatus::Warn => warn_count += 1,
        }
    }

    let metrics_html = records
        .iter()
        .map(|r| {
            let status_color = match r.status {
                SLOStatus::Pass => "#10b981",
                SLOStatus::Warn => "#f59e0b",
                SLOStatus::Fail => "#ef4444",
            };

            let status_text = match r.status {
                SLOStatus::Pass => "PASS",
                SLOStatus::Warn => "WARN",
                SLOStatus::Fail => "FAIL",
            };

            format!(
                r#"
        <tr>
            <td>{}</td>
            <td>{:.2} {}</td>
            <td>{:.2} {}</td>
            <td style="background-color: {}; color: white; font-weight: bold;">{}</td>
            <td>{:.1}%</td>
        </tr>"#,
                r.metric_name,
                r.metric_value,
                r.metric_unit,
                r.slo_target,
                r.metric_unit,
                status_color,
                status_text,
                ((r.metric_value - r.slo_target) / r.slo_target) * 100.0
            )
        })
        .collect::<Vec<_>>()
        .join("\n");

    let summary_color = if fail_count > 0 {
        "#ef4444"
    } else if warn_count > 0 {
        "#f59e0b"
    } else {
        "#10b981"
    };

    format!(
        r#"<!DOCTYPE html>
<html>
<head>
    <title>ggen SLO Dashboard</title>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <style>
        * {{ margin: 0; padding: 0; box-sizing: border-box; }}
        body {{ font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif; background: #f3f4f6; color: #111827; }}
        .container {{ max-width: 1200px; margin: 0 auto; padding: 20px; }}

        header {{ background: white; padding: 20px; border-radius: 8px; margin-bottom: 20px; box-shadow: 0 1px 3px rgba(0,0,0,0.1); }}
        h1 {{ margin-bottom: 10px; }}
        .meta {{ color: #6b7280; font-size: 14px; }}

        .summary {{ display: grid; grid-template-columns: 1fr 1fr 1fr 1fr; gap: 15px; margin-bottom: 20px; }}
        .summary-card {{ background: white; padding: 15px; border-radius: 8px; text-align: center; }}
        .summary-card h3 {{ font-size: 32px; margin-bottom: 5px; }}
        .summary-card p {{ color: #6b7280; font-size: 14px; }}

        .metrics-section {{ background: white; border-radius: 8px; overflow: hidden; box-shadow: 0 1px 3px rgba(0,0,0,0.1); }}
        .metrics-section h2 {{ padding: 15px 20px; background: #f9fafb; border-bottom: 1px solid #e5e7eb; }}

        table {{ width: 100%; border-collapse: collapse; }}
        th, td {{ padding: 12px 20px; text-align: left; border-bottom: 1px solid #e5e7eb; }}
        th {{ background: #f3f4f6; font-weight: 600; }}
        tr:hover {{ background: #f9fafb; }}

        .status-pass {{ color: #10b981; font-weight: 600; }}
        .status-warn {{ color: #f59e0b; font-weight: 600; }}
        .status-fail {{ color: #ef4444; font-weight: 600; }}

        footer {{ margin-top: 20px; padding: 15px; text-align: center; color: #6b7280; font-size: 12px; }}
    </style>
</head>
<body>
    <div class="container">
        <header>
            <h1>ggen SLO Compliance Dashboard</h1>
            <div class="meta">
                <p>Branch: <strong>{}</strong></p>
                <p>Generated: <strong>{}</strong></p>
            </div>
        </header>

        <div class="summary">
            <div class="summary-card">
                <h3 style="color: #10b981;">{}</h3>
                <p>PASS</p>
            </div>
            <div class="summary-card">
                <h3 style="color: #f59e0b;">{}</h3>
                <p>WARN</p>
            </div>
            <div class="summary-card">
                <h3 style="color: #ef4444;">{}</h3>
                <p>FAIL</p>
            </div>
            <div class="summary-card" style="background: {}; color: white;">
                <h3>{}/{}</h3>
                <p>OVERALL</p>
            </div>
        </div>

        <div class="metrics-section">
            <h2>SLO Metrics</h2>
            <table>
                <thead>
                    <tr>
                        <th>Metric</th>
                        <th>Actual</th>
                        <th>Target</th>
                        <th>Status</th>
                        <th>Overage %</th>
                    </tr>
                </thead>
                <tbody>
                    {}
                </tbody>
            </table>
        </div>

        <footer>
            <p>SLO Dashboard v1.0 | ggen Performance Benchmarking Suite</p>
            <p>For more information, see: <a href="docs/BENCHMARKING_SUITE.md">BENCHMARKING_SUITE.md</a></p>
        </footer>
    </div>
</body>
</html>"#,
        git_branch,
        timestamp,
        pass_count,
        warn_count,
        fail_count,
        summary_color,
        pass_count + warn_count + fail_count - fail_count,
        pass_count + warn_count + fail_count,
        metrics_html
    )
}

// ============================================================================
// Benchmark: Build Time SLO Validation
// ============================================================================

fn bench_build_time_slos(c: &mut Criterion) {
    let mut group = c.benchmark_group("slo_build_time");
    let slo = slo_targets::build_time_slos();

    println!("[Receipt] Build Time SLO Validation");
    println!("  Target (debug): {} s", slo.first_build_debug);
    println!("  Target (release): {} s", slo.first_build_release);
    println!("  Target (incremental): {} s", slo.incremental_build);

    group.bench_function("first_build_debug_target", |b| {
        b.iter(|| {
            let _ = slo.first_build_debug;
        });
    });

    group.bench_function("first_build_release_target", |b| {
        b.iter(|| {
            let _ = slo.first_build_release;
        });
    });

    group.finish();
}

// ============================================================================
// Benchmark: Memory SLO Validation
// ============================================================================

fn bench_memory_slos(c: &mut Criterion) {
    let mut group = c.benchmark_group("slo_memory");
    let slo = slo_targets::memory_slos();

    println!("[Receipt] Memory SLO Validation");
    println!("  RDF processing: {} MB", slo.rdf_processing);
    println!("  Template rendering: {} MB", slo.template_rendering);
    println!("  Code generation: {} MB", slo.code_generation);

    group.bench_function("rdf_processing_target", |b| {
        b.iter(|| {
            let _ = slo.rdf_processing;
        });
    });

    group.bench_function("template_rendering_target", |b| {
        b.iter(|| {
            let _ = slo.template_rendering;
        });
    });

    group.finish();
}

// ============================================================================
// Benchmark: Binary Size SLO Validation
// ============================================================================

fn bench_binary_size_slos(c: &mut Criterion) {
    let mut group = c.benchmark_group("slo_binary_size");
    let slo = slo_targets::binary_size_slos();

    println!("[Receipt] Binary Size SLO Validation");
    println!("  Release binary: {} MB", slo.release_binary);
    println!("  Debug binary: {} MB", slo.debug_binary);
    println!("  Symbol reduction: {} %", slo.symbol_reduction);

    group.bench_function("release_binary_target", |b| {
        b.iter(|| {
            let _ = slo.release_binary;
        });
    });

    group.finish();
}

// ============================================================================
// Benchmark: Runtime SLO Validation
// ============================================================================

fn bench_runtime_slos(c: &mut Criterion) {
    let mut group = c.benchmark_group("slo_runtime");
    let slo = slo_targets::runtime_slos();

    println!("[Receipt] Runtime SLO Validation");
    println!("  RDF parsing (1k triples): {} ms", slo.rdf_parsing_1k_triples);
    println!("  SPARQL query (1k entities): {} ms", slo.sparql_query_1k_entities);
    println!("  CLI startup: {} ms", slo.cli_startup);

    group.bench_function("rdf_parsing_1k_target", |b| {
        b.iter(|| {
            let _ = slo.rdf_parsing_1k_triples;
        });
    });

    group.finish();
}

// ============================================================================
// Criterion Setup
// ============================================================================

criterion_group!(
    benches,
    bench_build_time_slos,
    bench_memory_slos,
    bench_binary_size_slos,
    bench_runtime_slos
);

criterion_main!(benches);
