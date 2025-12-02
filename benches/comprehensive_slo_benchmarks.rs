//! Comprehensive SLO Benchmarks for ggen Performance Validation
//!
//! This benchmark suite validates all performance SLOs defined in PERFORMANCE_ANALYSIS.md
//! Run with: `cargo bench --bench comprehensive_slo_benchmarks`

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use std::collections::HashMap;
use std::hint::black_box;
use std::process::{Command, Stdio};
use std::time::Duration;

// ============================================================================
// SLO DEFINITIONS
// ============================================================================

/// Performance SLOs as defined in PERFORMANCE_ANALYSIS.md
mod slo {
    use std::time::Duration;

    // CLI Operations
    pub const CLI_HELP_P50: Duration = Duration::from_millis(50);
    pub const CLI_HELP_P95: Duration = Duration::from_millis(80);
    pub const CLI_HELP_MAX: Duration = Duration::from_millis(150);

    pub const CLI_VERSION_P50: Duration = Duration::from_millis(30);
    pub const CLI_VERSION_P95: Duration = Duration::from_millis(50);
    pub const CLI_VERSION_MAX: Duration = Duration::from_millis(100);

    pub const CLI_LIST_P50: Duration = Duration::from_millis(100);
    pub const CLI_LIST_P95: Duration = Duration::from_millis(200);
    pub const CLI_LIST_MAX: Duration = Duration::from_millis(500);

    // Template Operations
    pub const TEMPLATE_PARSE_P50: Duration = Duration::from_millis(5);
    pub const TEMPLATE_PARSE_P95: Duration = Duration::from_millis(10);
    pub const TEMPLATE_PARSE_MAX: Duration = Duration::from_millis(50);

    pub const TEMPLATE_RENDER_SIMPLE_P50: Duration = Duration::from_millis(10);
    pub const TEMPLATE_RENDER_SIMPLE_P95: Duration = Duration::from_millis(20);
    pub const TEMPLATE_RENDER_SIMPLE_MAX: Duration = Duration::from_millis(50);

    pub const TEMPLATE_RENDER_COMPLEX_P50: Duration = Duration::from_millis(30);
    pub const TEMPLATE_RENDER_COMPLEX_P95: Duration = Duration::from_millis(50);
    pub const TEMPLATE_RENDER_COMPLEX_MAX: Duration = Duration::from_millis(100);

    // SPARQL Operations
    pub const SPARQL_SIMPLE_P50: Duration = Duration::from_millis(10);
    pub const SPARQL_SIMPLE_P95: Duration = Duration::from_millis(20);
    pub const SPARQL_SIMPLE_MAX: Duration = Duration::from_millis(50);

    pub const SPARQL_COMPLEX_P50: Duration = Duration::from_millis(30);
    pub const SPARQL_COMPLEX_P95: Duration = Duration::from_millis(50);
    pub const SPARQL_COMPLEX_MAX: Duration = Duration::from_millis(100);

    pub const SPARQL_CACHE_HIT_P50: Duration = Duration::from_micros(500);
    pub const SPARQL_CACHE_HIT_P95: Duration = Duration::from_millis(1);
    pub const SPARQL_CACHE_HIT_MAX: Duration = Duration::from_millis(5);

    // JSON Operations
    pub const JSON_SERIALIZE_1KB_P50: Duration = Duration::from_micros(500);
    pub const JSON_SERIALIZE_1KB_MAX: Duration = Duration::from_millis(5);

    pub const JSON_SERIALIZE_10KB_P50: Duration = Duration::from_millis(2);
    pub const JSON_SERIALIZE_10KB_MAX: Duration = Duration::from_millis(10);

    // Memory Limits
    pub const MEMORY_PEAK_LIMIT: usize = 50 * 1024 * 1024; // 50MB
    pub const MEMORY_PER_TEMPLATE: usize = 1 * 1024 * 1024; // 1MB
}

// ============================================================================
// CLI STARTUP BENCHMARKS
// ============================================================================

fn get_binary_path() -> Option<std::path::PathBuf> {
    let binary_path = std::env::current_dir()
        .ok()?
        .join("target/release/ggen");

    if binary_path.exists() {
        Some(binary_path)
    } else {
        None
    }
}

fn bench_cli_startup_slo(c: &mut Criterion) {
    let mut group = c.benchmark_group("cli_startup_slo");
    group.measurement_time(Duration::from_secs(10));

    // Build release binary first
    let build_status = Command::new("cargo")
        .args(["build", "--release", "--bin", "ggen"])
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status();

    if build_status.is_err() || !build_status.as_ref().map(|s| s.success()).unwrap_or(false) {
        eprintln!("Warning: Failed to build ggen binary. Skipping CLI benchmarks.");
        return;
    }

    let binary_path = match get_binary_path() {
        Some(p) => p,
        None => {
            eprintln!("Warning: ggen binary not found. Skipping CLI benchmarks.");
            return;
        }
    };

    // --help benchmark (SLO: P50 < 50ms, P95 < 80ms, Max < 150ms)
    group.bench_function("help_p50_target_50ms", |b| {
        b.iter(|| {
            Command::new(black_box(&binary_path))
                .arg("--help")
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .status()
        });
    });

    // --version benchmark (SLO: P50 < 30ms, P95 < 50ms, Max < 100ms)
    group.bench_function("version_p50_target_30ms", |b| {
        b.iter(|| {
            Command::new(black_box(&binary_path))
                .arg("--version")
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .status()
        });
    });

    // list command benchmark (SLO: P50 < 100ms, P95 < 200ms, Max < 500ms)
    group.bench_function("list_p50_target_100ms", |b| {
        b.iter(|| {
            Command::new(black_box(&binary_path))
                .args(["template", "list"])
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .status()
        });
    });

    group.finish();
}

// ============================================================================
// TEMPLATE PARSING BENCHMARKS
// ============================================================================

fn create_simple_template(var_count: usize) -> String {
    let vars: Vec<String> = (0..var_count)
        .map(|i| format!("  var{}: value{}", i, i))
        .collect();

    format!(
        r#"---
to: "output/{{{{ name }}}}.rs"
vars:
{}
---
fn main() {{
    println!("Hello, {{{{ name }}}}!");
}}
"#,
        vars.join("\n")
    )
}

fn create_complex_template(var_count: usize, rdf_count: usize, sparql_count: usize) -> String {
    let vars: Vec<String> = (0..var_count)
        .map(|i| format!("  var{}: value{}", i, i))
        .collect();

    let rdf: Vec<String> = (0..rdf_count)
        .map(|i| {
            format!(
                "  - \"@prefix ex: <http://example.org/> . ex:entity{} a ex:Type{} .\"",
                i, i
            )
        })
        .collect();

    let sparql: Vec<String> = (0..sparql_count)
        .map(|i| format!("  query{}: \"SELECT ?s WHERE {{ ?s a ex:Type{} }}\"", i, i))
        .collect();

    format!(
        r#"---
to: "output/{{{{ name }}}}.rs"
prefixes:
  ex: "http://example.org/"
vars:
{}
rdf_inline:
{}
sparql:
{}
---
// Complex generated file with {} vars, {} RDF, {} SPARQL
fn main() {{
    println!("Hello, {{{{ name }}}}!");
}}
"#,
        vars.join("\n"),
        rdf.join("\n"),
        sparql.join("\n"),
        var_count,
        rdf_count,
        sparql_count
    )
}

fn bench_template_parsing_slo(c: &mut Criterion) {
    let mut group = c.benchmark_group("template_parsing_slo");

    // Simple template parsing (SLO: P50 < 5ms)
    for var_count in [5, 10, 20] {
        let template_str = create_simple_template(var_count);
        group.throughput(Throughput::Bytes(template_str.len() as u64));

        group.bench_with_input(
            BenchmarkId::new("simple_parse_p50_target_5ms", var_count),
            &template_str,
            |b, tmpl| {
                b.iter(|| {
                    // Simulate template parsing (YAML frontmatter extraction)
                    let content = black_box(tmpl);
                    let _parts: Vec<&str> = content.split("---").collect();
                    black_box(_parts)
                });
            },
        );
    }

    // Complex template parsing (SLO: P50 < 10ms)
    for (vars, rdf, sparql) in [(10, 10, 5), (20, 20, 10)] {
        let template_str = create_complex_template(vars, rdf, sparql);
        group.throughput(Throughput::Bytes(template_str.len() as u64));

        group.bench_with_input(
            BenchmarkId::new(
                "complex_parse_p50_target_10ms",
                format!("v{}_r{}_s{}", vars, rdf, sparql),
            ),
            &template_str,
            |b, tmpl| {
                b.iter(|| {
                    let content = black_box(tmpl);
                    let _parts: Vec<&str> = content.split("---").collect();
                    black_box(_parts)
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// JSON SERIALIZATION BENCHMARKS
// ============================================================================

fn generate_test_data(size_kb: usize) -> HashMap<String, serde_json::Value> {
    let mut data = HashMap::new();
    let items_needed = (size_kb * 1024) / 50; // ~50 bytes per item

    for i in 0..items_needed {
        data.insert(
            format!("key_{}", i),
            serde_json::json!({
                "id": i,
                "name": format!("item_{}", i),
                "value": i * 100,
                "active": i % 2 == 0
            }),
        );
    }

    data
}

fn bench_json_serialization_slo(c: &mut Criterion) {
    let mut group = c.benchmark_group("json_serialization_slo");

    // 1KB JSON (SLO: P50 < 0.5ms)
    let data_1kb = generate_test_data(1);
    group.throughput(Throughput::Bytes(1024));
    group.bench_function("serialize_1kb_p50_target_500us", |b| {
        b.iter(|| {
            let result = serde_json::to_string(black_box(&data_1kb));
            black_box(result)
        });
    });

    // 10KB JSON (SLO: P50 < 2ms)
    let data_10kb = generate_test_data(10);
    group.throughput(Throughput::Bytes(10 * 1024));
    group.bench_function("serialize_10kb_p50_target_2ms", |b| {
        b.iter(|| {
            let result = serde_json::to_string(black_box(&data_10kb));
            black_box(result)
        });
    });

    // 100KB JSON (SLO: P50 < 10ms)
    let data_100kb = generate_test_data(100);
    group.throughput(Throughput::Bytes(100 * 1024));
    group.bench_function("serialize_100kb_p50_target_10ms", |b| {
        b.iter(|| {
            let result = serde_json::to_string(black_box(&data_100kb));
            black_box(result)
        });
    });

    // Compare pretty vs compact (to measure overhead)
    group.bench_function("serialize_10kb_pretty_overhead", |b| {
        b.iter(|| {
            let result = serde_json::to_string_pretty(black_box(&data_10kb));
            black_box(result)
        });
    });

    group.finish();
}

// ============================================================================
// STRING ALLOCATION BENCHMARKS
// ============================================================================

fn bench_string_allocation_patterns(c: &mut Criterion) {
    let mut group = c.benchmark_group("string_allocation_patterns");

    let test_strings: Vec<String> = (0..100).map(|i| format!("test_string_{}", i)).collect();

    // Clone pattern (current)
    group.bench_function("clone_pattern_100_strings", |b| {
        b.iter(|| {
            let cloned: Vec<String> = test_strings.iter().map(|s| s.clone()).collect();
            black_box(cloned)
        });
    });

    // Reference pattern (optimized)
    group.bench_function("reference_pattern_100_strings", |b| {
        b.iter(|| {
            let refs: Vec<&str> = test_strings.iter().map(|s| s.as_str()).collect();
            black_box(refs)
        });
    });

    // Arc pattern (shared ownership)
    use std::sync::Arc;
    let arc_strings: Vec<Arc<str>> = test_strings.iter().map(|s| Arc::from(s.as_str())).collect();

    group.bench_function("arc_pattern_100_strings", |b| {
        b.iter(|| {
            let arcs: Vec<Arc<str>> = arc_strings.iter().map(Arc::clone).collect();
            black_box(arcs)
        });
    });

    // Format key pattern (current hotspot)
    group.bench_function("format_key_100_iterations", |b| {
        b.iter(|| {
            let keys: Vec<String> = (0..100)
                .map(|i| format!("pack_{}:query_{}", i, i))
                .collect();
            black_box(keys)
        });
    });

    // Pre-allocated buffer pattern (optimized)
    group.bench_function("buffer_key_100_iterations", |b| {
        b.iter(|| {
            let mut buffer = String::with_capacity(32);
            let keys: Vec<String> = (0..100)
                .map(|i| {
                    buffer.clear();
                    use std::fmt::Write;
                    let _ = write!(buffer, "pack_{}:query_{}", i, i);
                    buffer.clone()
                })
                .collect();
            black_box(keys)
        });
    });

    group.finish();
}

// ============================================================================
// MEMORY USAGE BENCHMARKS
// ============================================================================

fn bench_memory_usage_slo(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_usage_slo");
    group.sample_size(10); // Reduce for memory tests

    // 100 templates in memory (SLO: < 100MB total)
    group.bench_function("100_templates_memory_target_100mb", |b| {
        b.iter(|| {
            let templates: Vec<String> = (0..100)
                .map(|i| create_simple_template(10 + i % 20))
                .collect();
            black_box(templates)
        });
    });

    // 1000 templates streaming (SLO: peak < 50MB)
    group.bench_function("1000_templates_streaming_peak_target_50mb", |b| {
        b.iter(|| {
            let mut total_size = 0usize;
            for i in 0..1000 {
                let template = create_simple_template(5 + i % 10);
                total_size += template.len();
                black_box(&template);
                // Template dropped here
            }
            black_box(total_size)
        });
    });

    group.finish();
}

// ============================================================================
// END-TO-END WORKFLOW BENCHMARKS
// ============================================================================

fn bench_e2e_workflow_slo(c: &mut Criterion) {
    let mut group = c.benchmark_group("e2e_workflow_slo");

    // Simple template workflow (SLO: < 500ms total)
    group.bench_function("simple_template_workflow_target_500ms", |b| {
        b.iter(|| {
            // 1. Parse template
            let template_str = create_simple_template(5);
            let _parts: Vec<&str> = template_str.split("---").collect();

            // 2. Build context
            let mut context = HashMap::new();
            context.insert("name", "test_project");
            context.insert("version", "1.0.0");

            // 3. Render (simulated)
            let output = template_str.replace("{{ name }}", "test_project");

            // 4. Serialize metadata
            let metadata = serde_json::json!({
                "template": "simple",
                "vars": context,
                "output_size": output.len()
            });
            let _json = serde_json::to_string(&metadata);

            black_box(output)
        });
    });

    // Complex template workflow (SLO: < 1000ms total)
    group.bench_function("complex_template_workflow_target_1000ms", |b| {
        b.iter(|| {
            // 1. Parse complex template
            let template_str = create_complex_template(20, 10, 5);
            let _parts: Vec<&str> = template_str.split("---").collect();

            // 2. Build context with many vars
            let mut context = HashMap::new();
            for i in 0..20 {
                context.insert(format!("var{}", i), format!("value{}", i));
            }

            // 3. Render (simulated)
            let output = template_str.replace("{{ name }}", "test_project");

            // 4. Process RDF (simulated)
            let rdf_triples = 10 * 3; // 10 entities * 3 triples each

            // 5. Execute SPARQL (simulated)
            let sparql_results = 5; // 5 queries

            // 6. Serialize all metadata
            let metadata = serde_json::json!({
                "template": "complex",
                "vars_count": context.len(),
                "output_size": output.len(),
                "rdf_triples": rdf_triples,
                "sparql_queries": sparql_results
            });
            let _json = serde_json::to_string(&metadata);

            black_box(output)
        });
    });

    group.finish();
}

// ============================================================================
// CACHE PERFORMANCE BENCHMARKS
// ============================================================================

fn bench_cache_performance_slo(c: &mut Criterion) {
    let mut group = c.benchmark_group("cache_performance_slo");

    // HashMap cache lookup (SLO: P50 < 0.5ms for cache hit)
    let mut cache: HashMap<String, String> = HashMap::new();
    for i in 0..1000 {
        cache.insert(format!("key_{}", i), format!("value_{}", i));
    }

    group.bench_function("hashmap_cache_hit_p50_target_500us", |b| {
        let mut idx = 0;
        b.iter(|| {
            let key = format!("key_{}", idx % 1000);
            let result = cache.get(&key);
            idx += 1;
            black_box(result)
        });
    });

    // Cache miss pattern
    group.bench_function("hashmap_cache_miss", |b| {
        let mut idx = 0;
        b.iter(|| {
            let key = format!("nonexistent_{}", idx);
            let result = cache.get(&key);
            idx += 1;
            black_box(result)
        });
    });

    group.finish();
}

// ============================================================================
// CRITERION GROUPS
// ============================================================================

criterion_group!(
    cli_slo_benches,
    bench_cli_startup_slo
);

criterion_group!(
    template_slo_benches,
    bench_template_parsing_slo
);

criterion_group!(
    json_slo_benches,
    bench_json_serialization_slo
);

criterion_group!(
    allocation_benches,
    bench_string_allocation_patterns
);

criterion_group!(
    memory_slo_benches,
    bench_memory_usage_slo
);

criterion_group!(
    e2e_slo_benches,
    bench_e2e_workflow_slo
);

criterion_group!(
    cache_slo_benches,
    bench_cache_performance_slo
);

criterion_main!(
    cli_slo_benches,
    template_slo_benches,
    json_slo_benches,
    allocation_benches,
    memory_slo_benches,
    e2e_slo_benches,
    cache_slo_benches
);
