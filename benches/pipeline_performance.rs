use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use ggen_core::graph::Graph;
use ggen_core::pipeline::Pipeline;
use ggen_core::template::Template;
use std::fs;
use std::io::Write;
use std::path::PathBuf;
use tempfile::{NamedTempFile, TempDir};
use tera::Context;

// ============================================================================
// BENCHMARK 1: Ontology Parsing Performance (100+ Class Ontologies)
// ============================================================================

fn bench_ontology_parsing(c: &mut Criterion) {
    let mut group = c.benchmark_group("ontology_parsing");

    let ontology_sizes = vec![
        (
            "small",
            "benches/fixtures/ontologies/small_ontology.ttl",
            10,
        ),
        (
            "medium",
            "benches/fixtures/ontologies/medium_ontology.ttl",
            50,
        ),
        (
            "large",
            "benches/fixtures/ontologies/large_ontology.ttl",
            150,
        ),
        (
            "very_large",
            "benches/fixtures/ontologies/very_large_ontology.ttl",
            500,
        ),
    ];

    for (name, path, class_count) in ontology_sizes {
        let ontology_path = PathBuf::from(path);

        if !ontology_path.exists() {
            eprintln!("Warning: Ontology file not found: {}", path);
            continue;
        }

        group.throughput(Throughput::Elements(class_count));

        group.bench_with_input(
            BenchmarkId::new("parse", name),
            &ontology_path,
            |b, path| {
                b.iter(|| {
                    let mut graph = Graph::new().unwrap();
                    let ontology_content = fs::read_to_string(path).unwrap();
                    let result = graph.load_turtle(black_box(&ontology_content));
                    black_box(result)
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// BENCHMARK 2: SPARQL Query Execution Time
// ============================================================================

fn create_sparql_test_data(
    graph: &mut Graph, entity_count: usize,
) -> Result<(), Box<dyn std::error::Error>> {
    let mut rdf_data = String::from(
        "@prefix ex: <http://example.org/> .\n\
         @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n\n",
    );

    for i in 0..entity_count {
        rdf_data.push_str(&format!(
            "ex:Entity{} a ex:TestClass ;\n  ex:hasName \"Entity {}\" ;\n  ex:hasId {} .\n\n",
            i, i, i
        ));
    }

    graph.load_turtle(&rdf_data)?;
    Ok(())
}

fn bench_sparql_query_execution(c: &mut Criterion) {
    let mut group = c.benchmark_group("sparql_query_execution");

    let entity_counts = vec![10, 50, 100, 500, 1000];

    for count in entity_counts {
        group.throughput(Throughput::Elements(count));

        // Simple SELECT query
        group.bench_with_input(BenchmarkId::new("select_all", count), &count, |b, &cnt| {
            b.iter(|| {
                let mut graph = Graph::new().unwrap();
                create_sparql_test_data(&mut graph, cnt).unwrap();

                let query = "PREFIX ex: <http://example.org/>\n\
                                SELECT ?s ?name WHERE { ?s ex:hasName ?name }";
                let result = graph.query(black_box(query));
                black_box(result)
            });
        });

        // Filtered query
        group.bench_with_input(
            BenchmarkId::new("select_filtered", count),
            &count,
            |b, &cnt| {
                b.iter(|| {
                    let mut graph = Graph::new().unwrap();
                    create_sparql_test_data(&mut graph, cnt).unwrap();

                    let query = "PREFIX ex: <http://example.org/>\n\
                                SELECT ?s ?id WHERE { ?s ex:hasId ?id . FILTER(?id < 50) }";
                    let result = graph.query(black_box(query));
                    black_box(result)
                });
            },
        );

        // Complex query with joins
        group.bench_with_input(
            BenchmarkId::new("select_complex", count),
            &count,
            |b, &cnt| {
                b.iter(|| {
                    let mut graph = Graph::new().unwrap();
                    create_sparql_test_data(&mut graph, cnt).unwrap();

                    let query = "PREFIX ex: <http://example.org/>\n\
                                PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n\
                                SELECT ?s ?name ?id WHERE {\n\
                                  ?s a ex:TestClass .\n\
                                  ?s ex:hasName ?name .\n\
                                  ?s ex:hasId ?id .\n\
                                }";
                    let result = graph.query(black_box(query));
                    black_box(result)
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// BENCHMARK 3: Template Rendering Performance
// ============================================================================

fn create_rendering_template(var_count: usize, loop_iterations: usize) -> String {
    let vars: Vec<String> = (0..var_count)
        .map(|i| format!("  var{}: value{}", i, i))
        .collect();

    let items: Vec<String> = (0..loop_iterations)
        .map(|i| format!("    - item{}", i))
        .collect();

    format!(
        r#"---
to: "output/{{{{ name }}}}.rs"
vars:
{}
items:
{}
---
// Generated file with {{{{ var_count }}}} variables
fn main() {{
    let name = "{{{{ name }}}}";
    println!("Hello, {{}}!", name);

    // Variable substitutions
{{% for i in range(end={}) %}}
    let var{{{{ i }}}} = "{{{{ vars[i] }}}}";
{{% endfor %}}

    // Loop rendering
{{% for item in items %}}
    println!("Item: {{{{ item }}}}");
{{% endfor %}}
}}
"#,
        vars.join("\n"),
        items.join("\n"),
        var_count
    )
}

fn bench_template_rendering(c: &mut Criterion) {
    let mut group = c.benchmark_group("template_rendering");

    let pipeline = Pipeline::new().unwrap();

    let configs = vec![(10, 10), (50, 50), (100, 100), (200, 200), (500, 100)];

    for (var_count, loop_count) in configs {
        group.throughput(Throughput::Elements((var_count + loop_count) as u64));

        let template_str = create_rendering_template(var_count, loop_count);

        group.bench_with_input(
            BenchmarkId::new("render", format!("v{}_l{}", var_count, loop_count)),
            &template_str,
            |b, tmpl| {
                b.iter(|| {
                    let mut template = Template::parse(tmpl).unwrap();
                    let mut ctx = Context::new();
                    ctx.insert("name", "test_app");
                    ctx.insert("var_count", &var_count);

                    let mut tera = pipeline.tera.clone();
                    template.render_frontmatter(&mut tera, &ctx).unwrap();
                    let result = template.render(&mut tera, &ctx);
                    black_box(result)
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// BENCHMARK 4: File I/O Operations
// ============================================================================

fn bench_file_io_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("file_io_operations");

    let file_sizes = vec![
        ("1kb", 1024),
        ("10kb", 10 * 1024),
        ("100kb", 100 * 1024),
        ("1mb", 1024 * 1024),
    ];

    for (name, size) in file_sizes {
        let content: String = (0..size).map(|_| 'x').collect();

        group.throughput(Throughput::Bytes(size as u64));

        // Write benchmark
        group.bench_with_input(BenchmarkId::new("write", name), &content, |b, data| {
            b.iter(|| {
                let temp_dir = TempDir::new().unwrap();
                let file_path = temp_dir.path().join("test.txt");
                let result = fs::write(black_box(&file_path), black_box(data));
                black_box(result)
            });
        });

        // Read benchmark
        group.bench_with_input(BenchmarkId::new("read", name), &content, |b, data| {
            let temp_dir = TempDir::new().unwrap();
            let file_path = temp_dir.path().join("test.txt");
            fs::write(&file_path, data).unwrap();

            b.iter(|| {
                let result = fs::read_to_string(black_box(&file_path));
                black_box(result)
            });
        });

        // Append benchmark
        group.bench_with_input(BenchmarkId::new("append", name), &content, |b, data| {
            b.iter(|| {
                let temp_dir = TempDir::new().unwrap();
                let file_path = temp_dir.path().join("test.txt");

                let mut file = fs::OpenOptions::new()
                    .create(true)
                    .append(true)
                    .open(black_box(&file_path))
                    .unwrap();

                let result = file.write_all(black_box(data.as_bytes()));
                black_box(result)
            });
        });
    }

    group.finish();
}

// ============================================================================
// BENCHMARK 5: Memory Usage During Large Generations
// ============================================================================

fn bench_memory_large_generations(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_large_generations");
    group.sample_size(10); // Reduce sample size for memory-intensive benchmarks

    let generation_counts = vec![10, 100, 500, 1000];

    for count in generation_counts {
        group.throughput(Throughput::Elements(count));

        // Sequential generation (memory-efficient)
        group.bench_with_input(BenchmarkId::new("sequential", count), &count, |b, &cnt| {
            let template_str = create_rendering_template(10, 10);
            let pipeline = Pipeline::new().unwrap();

            b.iter(|| {
                for i in 0..cnt {
                    let mut template = Template::parse(&template_str).unwrap();
                    let mut ctx = Context::new();
                    ctx.insert("name", &format!("app_{}", i));

                    let mut tera = pipeline.tera.clone();
                    template.render_frontmatter(&mut tera, &ctx).unwrap();
                    let rendered = template.render(&mut tera, &ctx).unwrap();
                    black_box(rendered);
                    // Template and rendered content dropped here
                }
            });
        });

        // Batch generation (holds all in memory)
        group.bench_with_input(BenchmarkId::new("batch", count), &count, |b, &cnt| {
            let template_str = create_rendering_template(10, 10);
            let pipeline = Pipeline::new().unwrap();

            b.iter(|| {
                let mut results = Vec::new();
                for i in 0..cnt {
                    let mut template = Template::parse(&template_str).unwrap();
                    let mut ctx = Context::new();
                    ctx.insert("name", &format!("app_{}", i));

                    let mut tera = pipeline.tera.clone();
                    template.render_frontmatter(&mut tera, &ctx).unwrap();
                    let rendered = template.render(&mut tera, &ctx).unwrap();
                    results.push(rendered);
                }
                black_box(results);
            });
        });
    }

    group.finish();
}

// ============================================================================
// BENCHMARK 6: End-to-End Generation Time
// ============================================================================

fn create_e2e_template_with_rdf(complexity: &str) -> String {
    match complexity {
        "simple" => {
            r#"---
to: "output/{{ name }}.rs"
vars:
  author: "Test Author"
  version: "1.0.0"
---
// Generated by {{ author }}
// Version {{ version }}

fn main() {
    println!("Hello, {{ name }}!");
}
"#.to_string()
        },
        "medium" => {
            r#"---
to: "output/{{ name }}.rs"
prefixes:
  ex: "http://example.org/"
vars:
  author: "Test Author"
  version: "1.0.0"
rdf_inline:
  - "@prefix ex: <http://example.org/> . ex:App a ex:Application ."
sparql:
  apps: "PREFIX ex: <http://example.org/> SELECT ?s WHERE { ?s a ex:Application }"
---
// Generated by {{ author }}
// Version {{ version }}

{% for row in apps %}
fn process_{{ row.s }}() {
    println!("Processing application");
}
{% endfor %}

fn main() {
    println!("Hello, {{ name }}!");
}
"#.to_string()
        },
        "complex" => {
            r#"---
to: "output/{{ name }}/mod.rs"
prefixes:
  ex: "http://example.org/"
  rdf: "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
vars:
  author: "Test Author"
  version: "1.0.0"
  license: "MIT"
rdf_inline:
  - "@prefix ex: <http://example.org/> ."
  - "ex:App a ex:Application ; ex:hasName 'TestApp' ; ex:hasVersion '1.0.0' ."
  - "ex:Module1 a ex:Module ; ex:partOf ex:App ."
  - "ex:Module2 a ex:Module ; ex:partOf ex:App ."
sparql:
  modules: "PREFIX ex: <http://example.org/> SELECT ?module WHERE { ?module a ex:Module }"
  app_info: "PREFIX ex: <http://example.org/> SELECT ?name ?version WHERE { ex:App ex:hasName ?name ; ex:hasVersion ?version }"
---
// Generated by {{ author }}
// License: {{ license }}
// Version {{ version }}

{% for row in app_info %}
/// Application: {{ row.name }}
/// Version: {{ row.version }}
{% endfor %}

pub mod modules {
{% for row in modules %}
    pub mod {{ row.module | basename }} {
        pub fn init() {
            println!("Initializing {{ row.module }}");
        }
    }
{% endfor %}
}

fn main() {
    println!("Hello, {{ name }}!");
    modules::init_all();
}
"#.to_string()
        },
        _ => panic!("Unknown complexity level"),
    }
}

fn bench_e2e_generation(c: &mut Criterion) {
    let mut group = c.benchmark_group("e2e_generation");

    let pipeline = Pipeline::new().unwrap();
    let temp_dir = TempDir::new().unwrap();

    for complexity in &["simple", "medium", "complex"] {
        let template_str = create_e2e_template_with_rdf(complexity);
        let template_path = temp_dir.path().join(format!("{}.tmpl", complexity));

        group.bench_with_input(
            BenchmarkId::new("generation", complexity),
            &template_str,
            |b, tmpl| {
                b.iter(|| {
                    let mut template = Template::parse(tmpl).unwrap();
                    let mut ctx = Context::new();
                    ctx.insert("name", "test_app");

                    let mut graph = Graph::new().unwrap();
                    let mut tera = pipeline.tera.clone();

                    // Full pipeline: parse -> render frontmatter -> process graph -> render
                    template.render_frontmatter(&mut tera, &ctx).unwrap();
                    template
                        .process_graph(&mut graph, &mut tera, &ctx, &template_path)
                        .ok();
                    let result = template.render(&mut tera, &ctx);

                    black_box(result)
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// BENCHMARK 7: Multiple File Generation (Realistic Workload)
// ============================================================================

fn bench_multi_file_generation(c: &mut Criterion) {
    let mut group = c.benchmark_group("multi_file_generation");
    group.sample_size(10);

    let pipeline = Pipeline::new().unwrap();
    let file_counts = vec![10, 50, 100];

    for count in file_counts {
        group.throughput(Throughput::Elements(count));

        group.bench_with_input(BenchmarkId::new("files", count), &count, |b, &cnt| {
            let template_str = create_rendering_template(10, 10);

            b.iter(|| {
                let temp_dir = TempDir::new().unwrap();

                for i in 0..cnt {
                    let mut template = Template::parse(&template_str).unwrap();
                    let mut ctx = Context::new();
                    ctx.insert("name", &format!("file_{}", i));

                    let mut tera = pipeline.tera.clone();
                    template.render_frontmatter(&mut tera, &ctx).unwrap();
                    let rendered = template.render(&mut tera, &ctx).unwrap();

                    let output_path = temp_dir.path().join(format!("file_{}.rs", i));
                    fs::write(output_path, rendered).unwrap();
                }

                black_box(temp_dir);
            });
        });
    }

    group.finish();
}

// ============================================================================
// Benchmark Groups
// ============================================================================

criterion_group!(
    ontology_benches,
    bench_ontology_parsing,
    bench_sparql_query_execution
);

criterion_group!(
    rendering_benches,
    bench_template_rendering,
    bench_file_io_operations
);

criterion_group!(memory_benches, bench_memory_large_generations);

criterion_group!(
    e2e_benches,
    bench_e2e_generation,
    bench_multi_file_generation
);

criterion_main!(
    ontology_benches,
    rendering_benches,
    memory_benches,
    e2e_benches
);
