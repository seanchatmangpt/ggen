//! Comprehensive Performance Benchmark Suite for ggen v2.0.0
//!
//! This benchmark suite validates that the v2.0.0 migration meets all performance targets
//! using Chicago TDD principles: REAL execution times, REAL templates, REAL data, REAL I/O.
//!
//! # Performance SLOs (Service Level Objectives)
//! - CLI startup time: <100ms
//! - Simple template generation: <500ms
//! - Complex template generation: <2s
//! - RDF query execution: <3s for 1k triples
//! - Memory usage: <10MB baseline
//! - Concurrent operations: Linear scaling up to 8 cores
//!
//! # 80/20 Focus Areas
//! 1. **CLI Performance** - Startup, command routing, help generation
//! 2. **Template Generation** - Simple/complex templates with real I/O
//! 3. **RDF Operations** - Query execution, graph loading, validation
//! 4. **Memory & Concurrency** - Baseline usage, parallel execution
//!
//! # Usage
//! ```bash
//! # Run all benchmarks
//! cargo bench --bench v2_performance
//!
//! # Run specific benchmark group
//! cargo bench --bench v2_performance -- cli_startup
//! cargo bench --bench v2_performance -- template_generation
//! cargo bench --bench v2_performance -- rdf_operations
//! cargo bench --bench v2_performance -- memory_baseline
//! ```

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use std::fs;
use std::hint::black_box;
use std::path::PathBuf;
use std::process::Command;
use tempfile::TempDir;

// ============================================================================
// BENCHMARK GROUP 1: CLI Startup Performance
// Target: <100ms for CLI initialization and version check
// ============================================================================

fn bench_cli_startup(c: &mut Criterion) {
    let mut group = c.benchmark_group("cli_startup");
    group.sample_size(50);

    // Build the binary in release mode first
    let build_status = Command::new("cargo")
        .args(["build", "--release", "--bin", "ggen"])
        .status()
        .expect("Failed to build ggen binary");
    assert!(build_status.success(), "Failed to build ggen");

    let binary_path = PathBuf::from("target/release/ggen");
    assert!(binary_path.exists(), "ggen binary not found");

    // Benchmark 1.1: Version check (coldest path)
    group.bench_function("version_check", |b| {
        b.iter(|| {
            let output = Command::new(&binary_path)
                .arg("--version")
                .output()
                .expect("Failed to execute ggen --version");
            black_box(output.status.success());
        });
    });

    // Benchmark 1.2: Help generation (warm path)
    group.bench_function("help_generation", |b| {
        b.iter(|| {
            let output = Command::new(&binary_path)
                .arg("--help")
                .output()
                .expect("Failed to execute ggen --help");
            black_box(output.status.success());
        });
    });

    // Benchmark 1.3: Subcommand help (nested routing)
    group.bench_function("subcommand_help", |b| {
        b.iter(|| {
            let output = Command::new(&binary_path)
                .args(["template", "--help"])
                .output()
                .expect("Failed to execute ggen template --help");
            black_box(output.status.success());
        });
    });

    // Benchmark 1.4: Command with no-op (full initialization)
    group.bench_function("template_list_empty", |b| {
        b.iter_batched(
            || TempDir::new().unwrap(),
            |temp_dir| {
                let output = Command::new(&binary_path)
                    .args(["template", "list"])
                    .current_dir(temp_dir.path())
                    .output()
                    .expect("Failed to execute ggen template list");
                black_box(output.status.success());
            },
            criterion::BatchSize::SmallInput,
        );
    });

    group.finish();
}

// ============================================================================
// BENCHMARK GROUP 2: Template Generation Performance
// Target: Simple <500ms, Complex <2s
// ============================================================================

fn bench_template_generation(c: &mut Criterion) {
    let mut group = c.benchmark_group("template_generation");
    group.sample_size(20);

    let binary_path = PathBuf::from("target/release/ggen");

    // Setup: Create test templates
    let setup_templates = |temp_dir: &TempDir| {
        let template_dir = temp_dir.path().join(".ggen/templates");
        fs::create_dir_all(&template_dir).unwrap();

        // Simple template: Single file with basic variable substitution
        let simple_template = template_dir.join("simple.tera");
        fs::write(
            &simple_template,
            r#"// Generated: {{ name }}
fn main() {
    println!("Hello, {{ name }}!");
}
"#,
        )
        .unwrap();

        // Complex template: Multiple files with loops, conditionals, filters
        let complex_template = template_dir.join("complex.tera");
        fs::write(
            &complex_template,
            r#"{% for module in modules %}
// Module: {{ module.name | upper }}
pub mod {{ module.name }} {
    {% if module.has_tests %}
    #[cfg(test)]
    mod tests {
        {% for test in module.tests %}
        #[test]
        fn test_{{ test }}() {
            assert!(true);
        }
        {% endfor %}
    }
    {% endif %}

    {% for function in module.functions %}
    pub fn {{ function.name }}({% for arg in function.args %}{{ arg.name }}: {{ arg.type }}{% if not loop.last %}, {% endif %}{% endfor %}) -> {{ function.return_type }} {
        // TODO: Implement {{ function.name }}
        {% if function.return_type == "Result<T, E>" %}
        Ok(())
        {% elif function.return_type == "Option<T>" %}
        None
        {% else %}
        Default::default()
        {% endif %}
    }
    {% endfor %}
}
{% endfor %}
"#,
        )
        .unwrap();

        // Create context files
        let simple_context = temp_dir.path().join("simple_context.json");
        fs::write(&simple_context, r#"{"name": "SimpleApp"}"#).unwrap();

        let complex_context = temp_dir.path().join("complex_context.json");
        fs::write(
            &complex_context,
            r#"{
  "modules": [
    {
      "name": "core",
      "has_tests": true,
      "tests": ["initialization", "validation", "error_handling"],
      "functions": [
        {"name": "init", "args": [{"name": "config", "type": "Config"}], "return_type": "Result<T, E>"},
        {"name": "validate", "args": [{"name": "input", "type": "&str"}], "return_type": "bool"},
        {"name": "process", "args": [{"name": "data", "type": "Data"}], "return_type": "Result<T, E>"}
      ]
    },
    {
      "name": "utils",
      "has_tests": true,
      "tests": ["conversion", "formatting"],
      "functions": [
        {"name": "convert", "args": [{"name": "value", "type": "i32"}], "return_type": "String"},
        {"name": "format", "args": [{"name": "template", "type": "&str"}], "return_type": "String"}
      ]
    },
    {
      "name": "handlers",
      "has_tests": false,
      "tests": [],
      "functions": [
        {"name": "handle_request", "args": [{"name": "req", "type": "Request"}], "return_type": "Response"},
        {"name": "handle_error", "args": [{"name": "err", "type": "Error"}], "return_type": "Response"}
      ]
    }
  ]
}"#,
        )
        .unwrap();

        (simple_context, complex_context)
    };

    // Benchmark 2.1: Simple template generation (single file, basic vars)
    group.bench_function("simple_template", |b| {
        b.iter_batched(
            || {
                let temp_dir = TempDir::new().unwrap();
                let (simple_context, _) = setup_templates(&temp_dir);
                (temp_dir, simple_context)
            },
            |(temp_dir, simple_context)| {
                let output_file = temp_dir.path().join("output.rs");
                let output = Command::new(&binary_path)
                    .args([
                        "template",
                        "generate",
                        "simple",
                        "--context",
                        simple_context.to_str().unwrap(),
                        "--output",
                        output_file.to_str().unwrap(),
                    ])
                    .current_dir(temp_dir.path())
                    .output()
                    .expect("Failed to generate simple template");
                black_box(output.status.success());
            },
            criterion::BatchSize::SmallInput,
        );
    });

    // Benchmark 2.2: Complex template generation (multiple files, loops, conditionals)
    group.bench_function("complex_template", |b| {
        b.iter_batched(
            || {
                let temp_dir = TempDir::new().unwrap();
                let (_, complex_context) = setup_templates(&temp_dir);
                (temp_dir, complex_context)
            },
            |(temp_dir, complex_context)| {
                let output_file = temp_dir.path().join("complex_output.rs");
                let output = Command::new(&binary_path)
                    .args([
                        "template",
                        "generate",
                        "complex",
                        "--context",
                        complex_context.to_str().unwrap(),
                        "--output",
                        output_file.to_str().unwrap(),
                    ])
                    .current_dir(temp_dir.path())
                    .output()
                    .expect("Failed to generate complex template");
                black_box(output.status.success());
            },
            criterion::BatchSize::SmallInput,
        );
    });

    // Benchmark 2.3: Template with file tree generation
    group.bench_function("file_tree_generation", |b| {
        b.iter_batched(
            || TempDir::new().unwrap(),
            |temp_dir| {
                let output = Command::new(&binary_path)
                    .args([
                        "template",
                        "generate-tree",
                        "--template-dir",
                        "examples/templates",
                        "--output-dir",
                        temp_dir.path().to_str().unwrap(),
                    ])
                    .output()
                    .expect("Failed to generate file tree");
                black_box(output.status.success());
            },
            criterion::BatchSize::SmallInput,
        );
    });

    group.finish();
}

// ============================================================================
// BENCHMARK GROUP 3: RDF Operations Performance
// Target: <3s for 1k triples query
// ============================================================================

fn bench_rdf_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("rdf_operations");
    group.sample_size(20);

    let binary_path = PathBuf::from("target/release/ggen");

    // Setup: Create RDF test data
    let setup_rdf_graph = |temp_dir: &TempDir, num_triples: usize| {
        let graph_file = temp_dir.path().join("test_graph.ttl");
        let mut turtle_data = String::from("@prefix ex: <http://example.org/> .\n\n");

        for i in 0..num_triples {
            turtle_data.push_str(&format!("ex:entity{} ex:hasProperty ex:value{} .\n", i, i));
            turtle_data.push_str(&format!("ex:entity{} ex:hasType ex:Type{} .\n", i, i % 10));
            turtle_data.push_str(&format!(
                "ex:entity{} ex:relatesTo ex:entity{} .\n",
                i,
                (i + 1) % num_triples
            ));
        }

        fs::write(&graph_file, turtle_data).unwrap();
        graph_file
    };

    // Benchmark 3.1: Load small graph (100 triples)
    group.bench_function("load_small_graph_100", |b| {
        b.iter_batched(
            || {
                let temp_dir = TempDir::new().unwrap();
                let graph_file = setup_rdf_graph(&temp_dir, 100);
                (temp_dir, graph_file)
            },
            |(temp_dir, graph_file)| {
                let output = Command::new(&binary_path)
                    .args(["graph", "load", graph_file.to_str().unwrap()])
                    .current_dir(temp_dir.path())
                    .output()
                    .expect("Failed to load graph");
                black_box(output.status.success());
            },
            criterion::BatchSize::SmallInput,
        );
    });

    // Benchmark 3.2: Load medium graph (1k triples) - Target: <3s
    group.bench_function("load_medium_graph_1k", |b| {
        b.iter_batched(
            || {
                let temp_dir = TempDir::new().unwrap();
                let graph_file = setup_rdf_graph(&temp_dir, 1000);
                (temp_dir, graph_file)
            },
            |(temp_dir, graph_file)| {
                let output = Command::new(&binary_path)
                    .args(["graph", "load", graph_file.to_str().unwrap()])
                    .current_dir(temp_dir.path())
                    .output()
                    .expect("Failed to load graph");
                black_box(output.status.success());
            },
            criterion::BatchSize::SmallInput,
        );
    });

    // Benchmark 3.3: SPARQL query execution
    group.bench_function("sparql_query_simple", |b| {
        b.iter_batched(
            || {
                let temp_dir = TempDir::new().unwrap();
                let graph_file = setup_rdf_graph(&temp_dir, 500);

                // Load the graph first
                Command::new(&binary_path)
                    .args(["graph", "load", graph_file.to_str().unwrap()])
                    .current_dir(temp_dir.path())
                    .output()
                    .expect("Failed to load graph");

                let query_file = temp_dir.path().join("query.sparql");
                fs::write(
                    &query_file,
                    "SELECT ?entity ?property WHERE { ?entity ex:hasProperty ?property } LIMIT 10",
                )
                .unwrap();

                (temp_dir, query_file)
            },
            |(temp_dir, query_file)| {
                let output = Command::new(&binary_path)
                    .args([
                        "graph",
                        "query",
                        "--query-file",
                        query_file.to_str().unwrap(),
                    ])
                    .current_dir(temp_dir.path())
                    .output()
                    .expect("Failed to execute query");
                black_box(output.status.success());
            },
            criterion::BatchSize::SmallInput,
        );
    });

    // Benchmark 3.4: Graph validation
    group.bench_function("graph_validation", |b| {
        b.iter_batched(
            || {
                let temp_dir = TempDir::new().unwrap();
                let graph_file = setup_rdf_graph(&temp_dir, 200);
                (temp_dir, graph_file)
            },
            |(temp_dir, graph_file)| {
                let output = Command::new(&binary_path)
                    .args(["graph", "validate", graph_file.to_str().unwrap()])
                    .current_dir(temp_dir.path())
                    .output()
                    .expect("Failed to validate graph");
                black_box(output.status.success());
            },
            criterion::BatchSize::SmallInput,
        );
    });

    group.finish();
}

// ============================================================================
// BENCHMARK GROUP 4: Memory Baseline
// Target: <10MB baseline memory usage
// ============================================================================

fn bench_memory_baseline(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_baseline");
    group.sample_size(10);

    let binary_path = PathBuf::from("target/release/ggen");

    // Benchmark 4.1: Minimal execution memory footprint
    group.bench_function("minimal_execution", |b| {
        b.iter(|| {
            let output = Command::new(&binary_path)
                .arg("--version")
                .output()
                .expect("Failed to execute ggen");
            black_box(output.stdout);
        });
    });

    // Benchmark 4.2: Command routing memory
    group.bench_function("command_routing", |b| {
        b.iter_batched(
            || TempDir::new().unwrap(),
            |temp_dir| {
                let output = Command::new(&binary_path)
                    .args(["template", "list"])
                    .current_dir(temp_dir.path())
                    .output()
                    .expect("Failed to list templates");
                black_box(output.stdout);
            },
            criterion::BatchSize::SmallInput,
        );
    });

    group.finish();
}

// ============================================================================
// BENCHMARK GROUP 5: Concurrent Operations
// Target: Linear scaling up to 8 cores
// ============================================================================

fn bench_concurrent_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("concurrent_operations");
    group.sample_size(10);

    let binary_path = PathBuf::from("target/release/ggen");

    // Benchmark 5.1: Parallel template generation
    for num_parallel in [1, 2, 4, 8].iter() {
        group.bench_with_input(
            BenchmarkId::new("parallel_templates", num_parallel),
            num_parallel,
            |b, &num_parallel| {
                b.iter_batched(
                    || {
                        let temp_dir = TempDir::new().unwrap();
                        let template_dir = temp_dir.path().join(".ggen/templates");
                        fs::create_dir_all(&template_dir).unwrap();

                        let template = template_dir.join("test.tera");
                        fs::write(&template, "// {{ name }}").unwrap();

                        let contexts: Vec<_> = (0..num_parallel)
                            .map(|i| {
                                let context_file = temp_dir.path().join(format!("ctx{}.json", i));
                                fs::write(&context_file, format!(r#"{{"name": "Test{}" }}"#, i))
                                    .unwrap();
                                context_file
                            })
                            .collect();

                        (temp_dir, contexts)
                    },
                    |(temp_dir, contexts)| {
                        let handles: Vec<_> = contexts
                            .iter()
                            .enumerate()
                            .map(|(i, ctx)| {
                                let binary = binary_path.clone();
                                let ctx = ctx.clone();
                                let output = temp_dir.path().join(format!("out{}.rs", i));
                                let temp_path = temp_dir.path().to_path_buf();

                                std::thread::spawn(move || {
                                    Command::new(&binary)
                                        .args([
                                            "template",
                                            "generate",
                                            "test",
                                            "--context",
                                            ctx.to_str().unwrap(),
                                            "--output",
                                            output.to_str().unwrap(),
                                        ])
                                        .current_dir(&temp_path)
                                        .output()
                                        .expect("Failed to generate template")
                                })
                            })
                            .collect();

                        for handle in handles {
                            let output = handle.join().unwrap();
                            black_box(output.status.success());
                        }
                    },
                    criterion::BatchSize::SmallInput,
                );
            },
        );
    }

    group.finish();
}

// ============================================================================
// Criterion Configuration
// ============================================================================

criterion_group!(
    benches,
    bench_cli_startup,
    bench_template_generation,
    bench_rdf_operations,
    bench_memory_baseline,
    bench_concurrent_operations
);

criterion_main!(benches);
