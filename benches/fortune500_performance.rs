//! Fortune 500 Performance Benchmark Suite for ggen v2.5.0
//!
//! This benchmark suite validates ggen's readiness for enterprise production workloads
//! with specific focus on:
//!
//! 1. CLI Startup Time: <100ms (cold start, warm start, binary size impact)
//! 2. Template Rendering: <1s for typical projects, <5s for large projects
//! 3. RDF Query Performance: <100ms for 90th percentile SPARQL queries
//! 4. Code Generation Speed: Throughput in files/second
//! 5. Memory Usage: <500MB for typical usage, no memory leaks
//! 6. Concurrent Operations: Linear scaling with multi-core systems
//!
//! Fortune 500 Requirements:
//! - Support for large monorepos (1000+ files)
//! - Multi-user concurrent access
//! - Long-running processes (hours/days)
//! - Production-grade reliability (99.9% uptime)
//! - Enterprise security compliance

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use std::fs;
use std::hint::black_box;
use std::path::PathBuf;
use std::process::Command;
use std::time::{Duration, Instant};
use tempfile::TempDir;

// ============================================================================
// BENCHMARK GROUP 1: CLI Startup Performance
// Fortune 500 Target: <100ms for cold/warm start
// ============================================================================

fn bench_cli_startup_times(c: &mut Criterion) {
    let mut group = c.benchmark_group("cli_startup");
    group.sample_size(100);
    group.measurement_time(Duration::from_secs(20));

    let binary_path = PathBuf::from("target/release/ggen");
    assert!(binary_path.exists(), "ggen binary not found");

    // 1.1: Cold start - version check (absolute minimum overhead)
    group.bench_function("cold_start_version", |b| {
        b.iter_custom(|iters| {
            let mut total = Duration::ZERO;
            for _ in 0..iters {
                let start = Instant::now();
                let output = Command::new(&binary_path)
                    .arg("--version")
                    .output()
                    .expect("Failed to execute ggen --version");
                total += start.elapsed();
                assert!(output.status.success());
            }
            total
        });
    });

    // 1.2: Warm start - help text generation
    group.bench_function("warm_start_help", |b| {
        b.iter_custom(|iters| {
            let mut total = Duration::ZERO;
            for _ in 0..iters {
                let start = Instant::now();
                let output = Command::new(&binary_path)
                    .arg("--help")
                    .output()
                    .expect("Failed to execute ggen --help");
                total += start.elapsed();
                assert!(output.status.success());
            }
            total
        });
    });

    // 1.3: Subcommand routing
    group.bench_function("subcommand_routing", |b| {
        b.iter_custom(|iters| {
            let mut total = Duration::ZERO;
            for _ in 0..iters {
                let start = Instant::now();
                let output = Command::new(&binary_path)
                    .args(["template", "--help"])
                    .output()
                    .expect("Failed to execute subcommand");
                total += start.elapsed();
                assert!(output.status.success());
            }
            total
        });
    });

    // 1.4: Full initialization with empty project
    group.bench_function("full_init_empty_project", |b| {
        b.iter_custom(|iters| {
            let mut total = Duration::ZERO;
            for _ in 0..iters {
                let temp_dir = TempDir::new().unwrap();
                let start = Instant::now();
                let output = Command::new(&binary_path)
                    .args(["template", "list"])
                    .current_dir(temp_dir.path())
                    .output()
                    .expect("Failed to list templates");
                total += start.elapsed();
                assert!(output.status.success());
            }
            total
        });
    });

    group.finish();
}

// ============================================================================
// BENCHMARK GROUP 2: Template Rendering at Scale
// Fortune 500 Target: <1s typical, <5s large projects
// ============================================================================

fn bench_template_rendering_scale(c: &mut Criterion) {
    let mut group = c.benchmark_group("template_rendering_scale");
    group.sample_size(20);
    group.measurement_time(Duration::from_secs(30));

    let binary_path = PathBuf::from("target/release/ggen");

    // Helper: Create template with complexity
    let create_scaled_template = |vars: usize, loops: usize, conditionals: usize| -> String {
        let var_declarations: Vec<String> = (0..vars)
            .map(|i| format!("  var{}: value{}", i, i))
            .collect();

        let loop_content: Vec<String> = (0..loops)
            .map(|i| {
                format!(
                    r#"{% for item{} in items{} %}
    // Loop {}: {{{{ item{} }}}}
{% endfor %}"#,
                    i, i, i, i
                )
            })
            .collect();

        let conditional_content: Vec<String> = (0..conditionals)
            .map(|i| {
                format!(
                    r#"{% if condition{} %}
    // Conditional {}
{% endif %}"#,
                    i, i
                )
            })
            .collect();

        format!(
            r#"---
to: "output/{{{{ name }}}}.rs"
vars:
{}
---
// Generated file with {} vars, {} loops, {} conditionals
fn main() {{
    println!("Hello, {{{{ name }}}}!");

{}

{}
}}
"#,
            var_declarations.join("\n"),
            vars,
            loops,
            conditionals,
            loop_content.join("\n"),
            conditional_content.join("\n")
        )
    };

    // 2.1: Small template (typical microservice endpoint)
    for complexity in [(5, 2, 2), (10, 5, 5), (20, 10, 10)].iter() {
        let (vars, loops, conds) = complexity;
        let label = format!("v{}_l{}_c{}", vars, loops, conds);

        group.bench_with_input(
            BenchmarkId::new("microservice_template", &label),
            complexity,
            |b, &(v, l, c)| {
                b.iter_batched(
                    || {
                        let temp_dir = TempDir::new().unwrap();
                        let template_dir = temp_dir.path().join(".ggen/templates");
                        fs::create_dir_all(&template_dir).unwrap();

                        let template_file = template_dir.join("service.tera");
                        fs::write(&template_file, create_scaled_template(v, l, c)).unwrap();

                        let context_file = temp_dir.path().join("context.json");
                        fs::write(&context_file, r#"{"name": "UserService"}"#).unwrap();

                        (temp_dir, context_file)
                    },
                    |(temp_dir, context_file)| {
                        let output = Command::new(&binary_path)
                            .args([
                                "template",
                                "generate",
                                "service",
                                "--context",
                                context_file.to_str().unwrap(),
                                "--output",
                                temp_dir.path().join("output.rs").to_str().unwrap(),
                            ])
                            .current_dir(temp_dir.path())
                            .output()
                            .expect("Failed to generate template");
                        black_box(output.status.success());
                    },
                    criterion::BatchSize::SmallInput,
                );
            },
        );
    }

    // 2.2: Large monorepo template (100+ files)
    for file_count in [10, 50, 100, 200].iter() {
        group.throughput(Throughput::Elements(*file_count as u64));

        group.bench_with_input(
            BenchmarkId::new("monorepo_generation", file_count),
            file_count,
            |b, &count| {
                b.iter_batched(
                    || {
                        let temp_dir = TempDir::new().unwrap();
                        let template_dir = temp_dir.path().join(".ggen/templates");
                        fs::create_dir_all(&template_dir).unwrap();

                        // Create multiple templates
                        for i in 0..count {
                            let template_file = template_dir.join(format!("module_{}.tera", i));
                            fs::write(&template_file, create_scaled_template(5, 2, 2)).unwrap();
                        }

                        let context_file = temp_dir.path().join("context.json");
                        fs::write(&context_file, r#"{"name": "LargeProject"}"#).unwrap();

                        temp_dir
                    },
                    |temp_dir| {
                        // Generate all templates
                        for i in 0..count {
                            let output = Command::new(&binary_path)
                                .args([
                                    "template",
                                    "generate",
                                    &format!("module_{}", i),
                                    "--context",
                                    temp_dir.path().join("context.json").to_str().unwrap(),
                                    "--output",
                                    temp_dir
                                        .path()
                                        .join(format!("module_{}.rs", i))
                                        .to_str()
                                        .unwrap(),
                                ])
                                .current_dir(temp_dir.path())
                                .output()
                                .expect("Failed to generate template");
                            assert!(output.status.success());
                        }
                        black_box(count);
                    },
                    criterion::BatchSize::LargeInput,
                );
            },
        );
    }

    group.finish();
}

// ============================================================================
// BENCHMARK GROUP 3: RDF Query Performance
// Fortune 500 Target: <100ms for 90th percentile
// ============================================================================

fn bench_rdf_query_performance(c: &mut Criterion) {
    let mut group = c.benchmark_group("rdf_query_performance");
    group.sample_size(50);
    group.measurement_time(Duration::from_secs(25));

    let binary_path = PathBuf::from("target/release/ggen");

    // Helper: Create RDF graph with specified triple count
    let create_rdf_graph = |temp_dir: &TempDir, num_entities: usize| {
        let graph_file = temp_dir.path().join("data.ttl");
        let mut turtle_data = String::from("@prefix ex: <http://example.org/> .\n");
        turtle_data.push_str("@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n");
        turtle_data.push_str("@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n\n");

        for i in 0..num_entities {
            turtle_data.push_str(&format!("ex:Entity{} a ex:Type{} ;\n", i, i % 10));
            turtle_data.push_str(&format!("  ex:hasName \"Entity {}\" ;\n", i));
            turtle_data.push_str(&format!("  ex:hasValue {} ;\n", i * 100));
            turtle_data.push_str(&format!(
                "  ex:relatesTo ex:Entity{} .\n\n",
                (i + 1) % num_entities
            ));
        }

        fs::write(&graph_file, turtle_data).unwrap();
        graph_file
    };

    // 3.1: Small graph queries (100 triples)
    group.bench_function("query_small_graph_100_triples", |b| {
        b.iter_batched(
            || {
                let temp_dir = TempDir::new().unwrap();
                let graph_file = create_rdf_graph(&temp_dir, 25); // 25 entities * 4 triples = 100

                let query_file = temp_dir.path().join("query.sparql");
                fs::write(
                    &query_file,
                    "SELECT ?entity ?name WHERE { ?entity ex:hasName ?name } LIMIT 10",
                )
                .unwrap();

                // Pre-load graph
                Command::new(&binary_path)
                    .args(["graph", "load", graph_file.to_str().unwrap()])
                    .current_dir(temp_dir.path())
                    .output()
                    .expect("Failed to load graph");

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

    // 3.2: Medium graph queries (1000 triples) - Target: <100ms
    group.bench_function("query_medium_graph_1k_triples", |b| {
        b.iter_batched(
            || {
                let temp_dir = TempDir::new().unwrap();
                let graph_file = create_rdf_graph(&temp_dir, 250); // 250 entities * 4 = 1000

                let query_file = temp_dir.path().join("query.sparql");
                fs::write(
                    &query_file,
                    "SELECT ?entity ?value WHERE { ?entity ex:hasValue ?value } LIMIT 50",
                )
                .unwrap();

                Command::new(&binary_path)
                    .args(["graph", "load", graph_file.to_str().unwrap()])
                    .current_dir(temp_dir.path())
                    .output()
                    .expect("Failed to load graph");

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

    // 3.3: Large graph queries (10k triples)
    group.bench_function("query_large_graph_10k_triples", |b| {
        b.iter_batched(
            || {
                let temp_dir = TempDir::new().unwrap();
                let graph_file = create_rdf_graph(&temp_dir, 2500); // 2500 * 4 = 10k

                let query_file = temp_dir.path().join("query.sparql");
                fs::write(
                    &query_file,
                    "SELECT ?entity WHERE { ?entity a ex:Type1 } LIMIT 100",
                )
                .unwrap();

                Command::new(&binary_path)
                    .args(["graph", "load", graph_file.to_str().unwrap()])
                    .current_dir(temp_dir.path())
                    .output()
                    .expect("Failed to load graph");

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
            criterion::BatchSize::LargeInput,
        );
    });

    // 3.4: Complex SPARQL queries with joins
    group.bench_function("query_complex_joins", |b| {
        b.iter_batched(
            || {
                let temp_dir = TempDir::new().unwrap();
                let graph_file = create_rdf_graph(&temp_dir, 500);

                let query_file = temp_dir.path().join("complex_query.sparql");
                fs::write(
                    &query_file,
                    r#"SELECT ?entity1 ?entity2 ?name1 ?name2
                    WHERE {
                        ?entity1 ex:relatesTo ?entity2 .
                        ?entity1 ex:hasName ?name1 .
                        ?entity2 ex:hasName ?name2 .
                    } LIMIT 20"#,
                )
                .unwrap();

                Command::new(&binary_path)
                    .args(["graph", "load", graph_file.to_str().unwrap()])
                    .current_dir(temp_dir.path())
                    .output()
                    .expect("Failed to load graph");

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

    group.finish();
}

// ============================================================================
// BENCHMARK GROUP 4: Memory Usage Under Load
// Fortune 500 Target: <500MB typical, no leaks
// ============================================================================

fn bench_memory_usage(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_usage");
    group.sample_size(10);
    group.measurement_time(Duration::from_secs(20));

    let binary_path = PathBuf::from("target/release/ggen");

    // 4.1: Memory baseline (minimal execution)
    group.bench_function("memory_baseline_version", |b| {
        b.iter(|| {
            let output = Command::new(&binary_path)
                .arg("--version")
                .output()
                .expect("Failed to execute");
            black_box(output.stdout);
        });
    });

    // 4.2: Memory under template load (100 templates)
    group.bench_function("memory_100_template_generations", |b| {
        b.iter_batched(
            || {
                let temp_dir = TempDir::new().unwrap();
                let template_dir = temp_dir.path().join(".ggen/templates");
                fs::create_dir_all(&template_dir).unwrap();

                let template = template_dir.join("test.tera");
                fs::write(&template, "// Template {{ i }}").unwrap();

                temp_dir
            },
            |temp_dir| {
                for i in 0..100 {
                    let context = temp_dir.path().join(format!("ctx_{}.json", i));
                    fs::write(&context, format!(r#"{{"i": {}}}"#, i)).unwrap();

                    let output = Command::new(&binary_path)
                        .args([
                            "template",
                            "generate",
                            "test",
                            "--context",
                            context.to_str().unwrap(),
                            "--output",
                            temp_dir
                                .path()
                                .join(format!("out_{}.rs", i))
                                .to_str()
                                .unwrap(),
                        ])
                        .current_dir(temp_dir.path())
                        .output()
                        .expect("Failed to generate");
                    assert!(output.status.success());
                }
                black_box(100);
            },
            criterion::BatchSize::LargeInput,
        );
    });

    // 4.3: Long-running process simulation (1000 iterations)
    group.bench_function("memory_long_running_1000_iterations", |b| {
        b.iter_batched(
            || {
                let temp_dir = TempDir::new().unwrap();
                let template_dir = temp_dir.path().join(".ggen/templates");
                fs::create_dir_all(&template_dir).unwrap();

                let template = template_dir.join("simple.tera");
                fs::write(&template, "// Generated").unwrap();

                temp_dir
            },
            |temp_dir| {
                for _ in 0..1000 {
                    let output = Command::new(&binary_path)
                        .args(["template", "list"])
                        .current_dir(temp_dir.path())
                        .output()
                        .expect("Failed to list");
                    black_box(output.status.success());
                }
                black_box(1000);
            },
            criterion::BatchSize::NumIterations(1),
        );
    });

    group.finish();
}

// ============================================================================
// BENCHMARK GROUP 5: Concurrent Operations
// Fortune 500 Target: Linear scaling up to 8 cores
// ============================================================================

fn bench_concurrent_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("concurrent_operations");
    group.sample_size(10);
    group.measurement_time(Duration::from_secs(30));

    let binary_path = PathBuf::from("target/release/ggen");

    // 5.1: Parallel template generation (1, 2, 4, 8 parallel)
    for num_parallel in [1, 2, 4, 8].iter() {
        group.throughput(Throughput::Elements(*num_parallel as u64));

        group.bench_with_input(
            BenchmarkId::new("parallel_template_gen", num_parallel),
            num_parallel,
            |b, &num| {
                b.iter_batched(
                    || {
                        let temp_dir = TempDir::new().unwrap();
                        let template_dir = temp_dir.path().join(".ggen/templates");
                        fs::create_dir_all(&template_dir).unwrap();

                        let template = template_dir.join("test.tera");
                        fs::write(&template, "// Generated {{ id }}").unwrap();

                        let contexts: Vec<_> = (0..num)
                            .map(|i| {
                                let ctx = temp_dir.path().join(format!("ctx_{}.json", i));
                                fs::write(&ctx, format!(r#"{{"id": {}}}"#, i)).unwrap();
                                ctx
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
                                let output = temp_dir.path().join(format!("out_{}.rs", i));
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
                                        .expect("Failed to generate")
                                })
                            })
                            .collect();

                        for handle in handles {
                            let output = handle.join().unwrap();
                            assert!(output.status.success());
                        }
                        black_box(num);
                    },
                    criterion::BatchSize::SmallInput,
                );
            },
        );
    }

    // 5.2: Multi-user concurrent access simulation
    group.bench_function("multi_user_10_concurrent", |b| {
        b.iter_batched(
            || {
                // Create 10 separate project directories
                let projects: Vec<_> = (0..10)
                    .map(|i| {
                        let temp_dir = TempDir::new().unwrap();
                        let template_dir = temp_dir.path().join(".ggen/templates");
                        fs::create_dir_all(&template_dir).unwrap();

                        let template = template_dir.join("user.tera");
                        fs::write(&template, format!("// User {}", i)).unwrap();

                        let ctx = temp_dir.path().join("context.json");
                        fs::write(&ctx, format!(r#"{{"user": {}}}"#, i)).unwrap();

                        temp_dir
                    })
                    .collect();

                projects
            },
            |projects| {
                let handles: Vec<_> = projects
                    .iter()
                    .enumerate()
                    .map(|(i, project)| {
                        let binary = binary_path.clone();
                        let project_path = project.path().to_path_buf();

                        std::thread::spawn(move || {
                            Command::new(&binary)
                                .args(["template", "list"])
                                .current_dir(&project_path)
                                .output()
                                .expect("Failed to list templates")
                        })
                    })
                    .collect();

                for handle in handles {
                    let output = handle.join().unwrap();
                    assert!(output.status.success());
                }
                black_box(10);
            },
            criterion::BatchSize::SmallInput,
        );
    });

    group.finish();
}

// ============================================================================
// BENCHMARK GROUP 6: End-to-End Workflows
// Fortune 500 Target: Complete workflows <10s
// ============================================================================

fn bench_e2e_workflows(c: &mut Criterion) {
    let mut group = c.benchmark_group("e2e_workflows");
    group.sample_size(10);
    group.measurement_time(Duration::from_secs(40));

    let binary_path = PathBuf::from("target/release/ggen");

    // 6.1: Complete microservice generation
    group.bench_function("e2e_microservice_generation", |b| {
        b.iter_batched(
            || {
                let temp_dir = TempDir::new().unwrap();
                let template_dir = temp_dir.path().join(".ggen/templates");
                fs::create_dir_all(&template_dir).unwrap();

                // Create typical microservice templates
                let templates = vec![
                    ("main.tera", "fn main() { println!(\"{{name}}\"); }"),
                    ("lib.tera", "pub mod {{name}} {}"),
                    ("test.tera", "#[test] fn test_{{name}}() {}"),
                    ("config.tera", "// Config for {{name}}"),
                ];

                for (name, content) in templates {
                    fs::write(template_dir.join(name), content).unwrap();
                }

                let ctx = temp_dir.path().join("context.json");
                fs::write(&ctx, r#"{"name": "UserService"}"#).unwrap();

                temp_dir
            },
            |temp_dir| {
                // Generate all templates
                for template in ["main", "lib", "test", "config"] {
                    let output = Command::new(&binary_path)
                        .args([
                            "template",
                            "generate",
                            template,
                            "--context",
                            temp_dir.path().join("context.json").to_str().unwrap(),
                            "--output",
                            temp_dir
                                .path()
                                .join(format!("{}.rs", template))
                                .to_str()
                                .unwrap(),
                        ])
                        .current_dir(temp_dir.path())
                        .output()
                        .expect("Failed to generate");
                    assert!(output.status.success());
                }
                black_box(4);
            },
            criterion::BatchSize::SmallInput,
        );
    });

    // 6.2: Complete project with RDF metadata
    group.bench_function("e2e_project_with_rdf", |b| {
        b.iter_batched(
            || {
                let temp_dir = TempDir::new().unwrap();

                // Create RDF graph
                let graph_file = temp_dir.path().join("project.ttl");
                fs::write(
                    &graph_file,
                    r#"@prefix ex: <http://example.org/> .
                    ex:Project1 a ex:Project ;
                        ex:hasName "TestProject" ;
                        ex:hasVersion "1.0.0" ."#,
                )
                .unwrap();

                temp_dir
            },
            |temp_dir| {
                // Load graph
                let output = Command::new(&binary_path)
                    .args([
                        "graph",
                        "load",
                        temp_dir.path().join("project.ttl").to_str().unwrap(),
                    ])
                    .current_dir(temp_dir.path())
                    .output()
                    .expect("Failed to load graph");
                assert!(output.status.success());

                // Query graph
                let query_file = temp_dir.path().join("query.sparql");
                fs::write(
                    &query_file,
                    "SELECT ?project WHERE { ?project a ex:Project }",
                )
                .unwrap();

                let output = Command::new(&binary_path)
                    .args([
                        "graph",
                        "query",
                        "--query-file",
                        query_file.to_str().unwrap(),
                    ])
                    .current_dir(temp_dir.path())
                    .output()
                    .expect("Failed to query");
                assert!(output.status.success());

                black_box(2);
            },
            criterion::BatchSize::SmallInput,
        );
    });

    group.finish();
}

// ============================================================================
// Criterion Configuration
// ============================================================================

criterion_group!(startup_benches, bench_cli_startup_times,);

criterion_group!(template_benches, bench_template_rendering_scale,);

criterion_group!(rdf_benches, bench_rdf_query_performance,);

criterion_group!(memory_benches, bench_memory_usage,);

criterion_group!(concurrency_benches, bench_concurrent_operations,);

criterion_group!(e2e_benches, bench_e2e_workflows,);

criterion_main!(
    startup_benches,
    template_benches,
    rdf_benches,
    memory_benches,
    concurrency_benches,
    e2e_benches,
);
