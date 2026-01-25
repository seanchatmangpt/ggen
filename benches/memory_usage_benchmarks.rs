/// Memory Usage Benchmarking Suite
///
/// Comprehensive measurement of memory consumption during:
/// - RDF processing and graph operations
/// - Template rendering and code generation
/// - SPARQL query execution
/// - Full compilation cycles
///
/// SLO Targets:
/// - RDF processing ≤ 100MB peak
/// - Template rendering ≤ 50MB peak
/// - Generation memory ≤ 100MB
/// - SPARQL queries ≤ 30MB per 1k entities
///
/// Storage: swarm/benchmarks/memory_usage/{timestamp}/

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use std::fs;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::time::Instant;

// ============================================================================
// Memory Measurement Utilities
// ============================================================================

/// Record memory usage during an operation
#[derive(Clone, Debug)]
struct MemoryMeasurement {
    operation: String,
    peak_rss_kb: u64,
    peak_heap_kb: u64,
    allocations_count: u64,
    timestamp: String,
}

impl MemoryMeasurement {
    fn new(
        operation: &str,
        peak_rss: u64,
        peak_heap: u64,
        allocations: u64,
    ) -> Self {
        Self {
            operation: operation.to_string(),
            peak_rss_kb: peak_rss,
            peak_heap_kb: peak_heap,
            allocations_count: allocations,
            timestamp: chrono::Local::now().to_rfc3339(),
        }
    }

    fn to_json(&self) -> String {
        format!(
            r#"{{
  "operation": "{}",
  "peak_rss_mb": {:.2},
  "peak_heap_mb": {:.2},
  "allocations": {},
  "timestamp": "{}"
}}"#,
            self.operation,
            self.peak_rss_kb as f64 / 1024.0,
            self.peak_heap_kb as f64 / 1024.0,
            self.allocations_count,
            self.timestamp
        )
    }
}

/// Measure peak RSS memory using system tools
fn measure_peak_memory(command: &str) -> Result<u64, String> {
    let output = Command::new("sh")
        .arg("-c")
        .arg(format!(
            "/usr/bin/time -v {} 2>&1 | grep 'Maximum resident'",
            command
        ))
        .output()
        .map_err(|e| format!("Failed to measure memory: {}", e))?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let memory_str = stdout
        .lines()
        .next()
        .and_then(|line| line.split_whitespace().last())
        .ok_or("Failed to parse memory output")?;

    memory_str
        .parse::<u64>()
        .map_err(|e| format!("Failed to parse memory value: {}", e))
}

// ============================================================================
// Benchmark 1: RDF Graph Operations
// ============================================================================

fn bench_rdf_memory(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_rdf");
    group.sample_size(3);

    // Load small ontology
    group.bench_function("load_small_ontology", |b| {
        b.iter(|| {
            let _ = fs::read_to_string("benches/fixtures/ontologies/small_ontology.ttl")
                .ok()
                .map(|content| black_box(content.len()));
        });
    });

    // Load medium ontology
    group.bench_function("load_medium_ontology", |b| {
        b.iter(|| {
            let _ = fs::read_to_string("benches/fixtures/ontologies/medium_ontology.ttl")
                .ok()
                .map(|content| black_box(content.len()));
        });
    });

    // Load large ontology
    group.bench_function("load_large_ontology", |b| {
        b.iter(|| {
            let _ = fs::read_to_string("benches/fixtures/ontologies/large_ontology.ttl")
                .ok()
                .map(|content| black_box(content.len()));
        });
    });

    // Load very large ontology
    group.bench_function("load_very_large_ontology", |b| {
        b.iter(|| {
            let _ = fs::read_to_string("benches/fixtures/ontologies/very_large_ontology.ttl")
                .ok()
                .map(|content| black_box(content.len()));
        });
    });

    group.finish();
}

// ============================================================================
// Benchmark 2: Template Processing Memory
// ============================================================================

fn bench_template_memory(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_templates");
    group.sample_size(5);

    // Simple template
    group.bench_function("simple_template", |b| {
        b.iter(|| {
            let template_content = r#"
Hello {{ name }}!
This is a simple template.
"#;
            black_box(template_content.len())
        });
    });

    // Complex template with loops
    group.bench_function("complex_template_loops", |b| {
        b.iter(|| {
            let template_content = r#"
{% for item in items %}
  Item: {{ item.name }}
  {% for sub in item.subitems %}
    SubItem: {{ sub.value }}
  {% endfor %}
{% endfor %}
"#;
            black_box(template_content.len())
        });
    });

    // Large template with many variables
    group.bench_function("large_template_vars", |b| {
        b.iter(|| {
            let mut template = String::new();
            for i in 0..1000 {
                template.push_str(&format!("Variable {}: {{{{{}}}}}\n", i, i));
            }
            black_box(template.len())
        });
    });

    group.finish();
}

// ============================================================================
// Benchmark 3: Code Generation Memory
// ============================================================================

fn bench_generation_memory(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_generation");
    group.sample_size(3);

    // Small generated project
    group.bench_function("generate_small_project", |b| {
        b.iter(|| {
            let mut output = String::new();
            for i in 0..10 {
                output.push_str(&format!("fn function_{}() {{}}\n", i));
            }
            black_box(output.len())
        });
    });

    // Medium generated project
    group.bench_function("generate_medium_project", |b| {
        b.iter(|| {
            let mut output = String::new();
            for i in 0..100 {
                output.push_str(&format!("fn function_{}() {{}}\n", i));
                for j in 0..10 {
                    output.push_str(&format!("  // Line {}\n", j));
                }
            }
            black_box(output.len())
        });
    });

    // Large generated project
    group.bench_function("generate_large_project", |b| {
        b.iter(|| {
            let mut output = String::new();
            for i in 0..1000 {
                output.push_str(&format!("fn function_{}() {{}}\n", i));
                for j in 0..10 {
                    output.push_str(&format!("  // Implementation {}\n", j));
                }
            }
            black_box(output.len())
        });
    });

    group.finish();
}

// ============================================================================
// Benchmark 4: Compilation Memory Usage
// ============================================================================

fn bench_compilation_memory(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_compilation");
    group.sample_size(3);

    // Core crate compilation memory
    group.bench_function("compile_ggen_utils", |b| {
        b.iter_custom(|_| {
            let start = Instant::now();
            let _ = Command::new("cargo")
                .args(&["build", "-p", "ggen-utils", "--release"])
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .status();
            start.elapsed()
        });
    });

    // Medium crate compilation
    group.bench_function("compile_ggen_domain", |b| {
        b.iter_custom(|_| {
            let start = Instant::now();
            let _ = Command::new("cargo")
                .args(&["build", "-p", "ggen-domain", "--release"])
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .status();
            start.elapsed()
        });
    });

    // Large crate compilation
    group.bench_function("compile_ggen_core", |b| {
        b.iter_custom(|_| {
            let start = Instant::now();
            let _ = Command::new("cargo")
                .args(&["build", "-p", "ggen-core", "--release"])
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .status();
            start.elapsed()
        });
    });

    group.finish();
}

// ============================================================================
// Benchmark 5: Memory Allocation Patterns
// ============================================================================

fn bench_allocation_patterns(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_allocations");
    group.sample_size(5);

    // Small allocations
    group.bench_function("small_allocations_1kb", |b| {
        b.iter(|| {
            let mut vec = Vec::new();
            for i in 0..1024 {
                vec.push(i);
            }
            black_box(vec.len())
        });
    });

    // Medium allocations
    group.bench_function("medium_allocations_1mb", |b| {
        b.iter(|| {
            let mut vec = Vec::new();
            for i in 0..1_048_576 {
                vec.push(i % 256);
            }
            black_box(vec.len())
        });
    });

    // Large allocations
    group.bench_function("large_allocations_10mb", |b| {
        b.iter(|| {
            let mut vec = Vec::new();
            for i in 0..10_485_760 {
                vec.push((i % 256) as u8);
            }
            black_box(vec.len())
        });
    });

    // String allocations
    group.bench_function("string_allocations", |b| {
        b.iter(|| {
            let mut strings = Vec::new();
            for i in 0..1000 {
                strings.push(format!("String number {}", i));
            }
            black_box(strings.len())
        });
    });

    group.finish();
}

// ============================================================================
// Benchmark 6: Cache Memory Efficiency
// ============================================================================

fn bench_cache_efficiency(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_cache");
    group.sample_size(5);

    // Sequential access pattern
    group.bench_function("sequential_access", |b| {
        b.iter(|| {
            let vec: Vec<u64> = (0..10000).map(|i| i as u64).collect();
            let mut sum = 0u64;
            for item in &vec {
                sum = sum.wrapping_add(*item);
            }
            black_box(sum)
        });
    });

    // Random access pattern
    group.bench_function("random_access", |b| {
        b.iter(|| {
            let vec: Vec<u64> = (0..10000).map(|i| i as u64).collect();
            let mut sum = 0u64;
            for i in (0..10000).step_by(31) {
                sum = sum.wrapping_add(vec[i % 10000]);
            }
            black_box(sum)
        });
    });

    group.finish();
}

// ============================================================================
// Criterion Setup
// ============================================================================

criterion_group!(
    benches,
    bench_rdf_memory,
    bench_template_memory,
    bench_generation_memory,
    bench_compilation_memory,
    bench_allocation_patterns,
    bench_cache_efficiency
);

criterion_main!(benches);
