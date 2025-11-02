/// Template Generation Performance Benchmark Suite
///
/// This benchmark suite measures the performance of template generation
/// with RDF files across various scenarios and configurations.
///
/// ## Benchmark Scenarios
/// 1. Single template + single RDF file - Target: <100ms
/// 2. Single template + 10 RDF files - Target: <500ms
/// 3. 10 templates + shared RDF - Target: <1s
/// 4. SPARQL query execution - Target: <50ms
/// 5. Full project generation - Target: <2s
///
/// ## Metrics Measured
/// - Cold start time (first run)
/// - Warm cache time (subsequent runs)
/// - Memory usage during generation
/// - Disk I/O patterns
///
/// ## Results Format
/// - Performance vs SLO targets
/// - Regression analysis
/// - Optimization recommendations

use criterion::{
    black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput,
};
use ggen_core::{graph::Graph, pipeline::Pipeline, template::Template};
use std::collections::HashMap;
use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::time::Instant;
use tempfile::{NamedTempFile, TempDir};
use tera::Context;

// ============================================================================
// Test Data Generators
// ============================================================================

/// Create a simple template with variables
fn create_simple_template(var_count: usize) -> String {
    let vars: Vec<String> = (0..var_count)
        .map(|i| format!("  var{}: value{}", i, i))
        .collect();

    format!(
        r#"---
to: "output/{{{{ name }}}}.rs"
prefixes:
  ex: "http://example.org/"
---
// Generated file with {{{{ name }}}}
fn main() {{
    println!("Template variables: {var_count}");
}}
"#
    )
}

/// Create a template with RDF inline and SPARQL queries
fn create_rdf_template(rdf_triple_count: usize, sparql_query_count: usize) -> String {
    let rdf_triples: Vec<String> = (0..rdf_triple_count)
        .map(|i| {
            format!(
                "  - \"@prefix ex: <http://example.org/> . ex:Entity{} a ex:Type{} ; ex:name 'Entity {}' .\"",
                i, i % 5, i
            )
        })
        .collect();

    let sparql_queries: Vec<String> = (0..sparql_query_count)
        .map(|i| {
            format!(
                "  query{}: \"SELECT ?s ?name WHERE {{ ?s a ex:Type{} ; ex:name ?name }}\"",
                i, i % 5
            )
        })
        .collect();

    format!(
        r#"---
to: "output/{{{{ name }}}}.rs"
prefixes:
  ex: "http://example.org/"
rdf_inline:
{}
sparql:
{}
---
// Generated from RDF data
fn main() {{
    // Count: {{{{ sparql_count(results=sparql_results.query0) }}}}
    println!("RDF-based generation");
}}
"#,
        rdf_triples.join("\n"),
        sparql_queries.join("\n")
    )
}

/// Create RDF file with specified number of triples
fn create_rdf_file(triple_count: usize) -> NamedTempFile {
    let mut file = NamedTempFile::new().unwrap();

    writeln!(file, "@prefix ex: <http://example.org/> .").unwrap();
    writeln!(file, "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .").unwrap();
    writeln!(file, "").unwrap();

    for i in 0..triple_count {
        writeln!(
            file,
            "ex:Entity{} a ex:Type{} ; ex:name \"Entity {}\" ; ex:value {} .",
            i,
            i % 10,
            i,
            i * 100
        )
        .unwrap();
    }

    file.flush().unwrap();
    file
}

/// Create multiple RDF files
fn create_rdf_files(file_count: usize, triples_per_file: usize) -> Vec<NamedTempFile> {
    (0..file_count)
        .map(|_| create_rdf_file(triples_per_file))
        .collect()
}

// ============================================================================
// Memory Profiling Utilities
// ============================================================================

#[cfg(target_os = "linux")]
fn get_memory_usage() -> Option<usize> {
    use std::fs::read_to_string;

    let status = read_to_string("/proc/self/status").ok()?;
    for line in status.lines() {
        if line.starts_with("VmRSS:") {
            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.len() >= 2 {
                return parts[1].parse().ok();
            }
        }
    }
    None
}

#[cfg(not(target_os = "linux"))]
fn get_memory_usage() -> Option<usize> {
    // Fallback for non-Linux systems
    None
}

struct MemorySnapshot {
    before: Option<usize>,
    after: Option<usize>,
    peak: Option<usize>,
}

impl MemorySnapshot {
    fn new() -> Self {
        Self {
            before: get_memory_usage(),
            after: None,
            peak: None,
        }
    }

    fn update_peak(&mut self) {
        if let Some(current) = get_memory_usage() {
            self.peak = Some(self.peak.unwrap_or(current).max(current));
        }
    }

    fn finish(&mut self) {
        self.after = get_memory_usage();
    }

    fn delta_kb(&self) -> Option<i64> {
        match (self.before, self.after) {
            (Some(before), Some(after)) => Some(after as i64 - before as i64),
            _ => None,
        }
    }

    fn peak_kb(&self) -> Option<usize> {
        self.peak
    }
}

// ============================================================================
// BENCHMARK 1: Single Template + Single RDF File
// Target: <100ms
// ============================================================================

fn bench_single_template_single_rdf(c: &mut Criterion) {
    let mut group = c.benchmark_group("single_template_single_rdf");
    group.significance_level(0.1).sample_size(50);

    // Test with varying RDF file sizes
    for triple_count in [10, 100, 1000, 10000].iter() {
        let rdf_file = create_rdf_file(*triple_count);
        let rdf_path = rdf_file.path().to_path_buf();

        let template_str = create_rdf_template(10, 1);

        group.bench_with_input(
            BenchmarkId::new("cold_start", triple_count),
            triple_count,
            |b, _| {
                b.iter_custom(|iters| {
                    let mut total = std::time::Duration::ZERO;

                    for _ in 0..iters {
                        let start = Instant::now();

                        // Fresh parse and render (cold start)
                        let mut template = Template::parse(black_box(&template_str)).unwrap();
                        let mut graph = Graph::new().unwrap();
                        let mut pipeline = Pipeline::new().unwrap();
                        let mut ctx = Context::new();
                        ctx.insert("name", "test_app");

                        // Load RDF file
                        let rdf_content = fs::read_to_string(&rdf_path).unwrap();
                        graph.insert_turtle(&rdf_content).unwrap();

                        // Process and render
                        template.render_frontmatter(pipeline.tera_mut(), &ctx).unwrap();
                        template.process_graph(&mut graph, pipeline.tera_mut(), &ctx, &rdf_path).unwrap();
                        let _output = template.render(pipeline.tera_mut(), &ctx).unwrap();

                        total += start.elapsed();
                        black_box(_output);
                    }

                    total
                });
            },
        );

        // Warm cache benchmark (reuse graph and pipeline)
        group.bench_with_input(
            BenchmarkId::new("warm_cache", triple_count),
            triple_count,
            |b, _| {
                let mut graph = Graph::new().unwrap();
                let rdf_content = fs::read_to_string(&rdf_path).unwrap();
                graph.insert_turtle(&rdf_content).unwrap();
                let pipeline = Pipeline::new().unwrap();

                b.iter(|| {
                    let mut template = Template::parse(black_box(&template_str)).unwrap();
                    let mut ctx = Context::new();
                    ctx.insert("name", "test_app");
                    let mut pipeline_clone = Pipeline::new().unwrap();

                    template.render_frontmatter(pipeline_clone.tera_mut(), &ctx).unwrap();
                    template.process_graph(&mut graph, pipeline_clone.tera_mut(), &ctx, &rdf_path).unwrap();
                    let output = template.render(pipeline_clone.tera_mut(), &ctx).unwrap();
                    black_box(output)
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// BENCHMARK 2: Single Template + 10 RDF Files
// Target: <500ms
// ============================================================================

fn bench_single_template_multiple_rdf(c: &mut Criterion) {
    let mut group = c.benchmark_group("single_template_multiple_rdf");
    group.significance_level(0.1).sample_size(20);

    let rdf_files = create_rdf_files(10, 100);
    let rdf_paths: Vec<PathBuf> = rdf_files.iter().map(|f| f.path().to_path_buf()).collect();

    let template_str = create_rdf_template(0, 3);

    // Cold start - load all RDF files
    group.bench_function("cold_start_10_files", |b| {
        b.iter_custom(|iters| {
            let mut total = std::time::Duration::ZERO;

            for _ in 0..iters {
                let start = Instant::now();

                let mut template = Template::parse(black_box(&template_str)).unwrap();
                let mut graph = Graph::new().unwrap();
                let mut pipeline = Pipeline::new().unwrap();
                let mut ctx = Context::new();
                ctx.insert("name", "multi_rdf_app");

                // Load all RDF files
                for rdf_path in &rdf_paths {
                    let rdf_content = fs::read_to_string(rdf_path).unwrap();
                    graph.insert_turtle(&rdf_content).unwrap();
                }

                template.render_frontmatter(pipeline.tera_mut(), &ctx).unwrap();
                template.process_graph(&mut graph, pipeline.tera_mut(), &ctx, &rdf_paths[0]).unwrap();
                let _output = template.render(pipeline.tera_mut(), &ctx).unwrap();

                total += start.elapsed();
                black_box(_output);
            }

            total
        });
    });

    // Warm cache - graph already loaded
    group.bench_function("warm_cache_10_files", |b| {
        let mut graph = Graph::new().unwrap();
        for rdf_path in &rdf_paths {
            let rdf_content = fs::read_to_string(rdf_path).unwrap();
            graph.insert_turtle(&rdf_content).unwrap();
        }
        let pipeline = Pipeline::new().unwrap();

        b.iter(|| {
            let mut template = Template::parse(black_box(&template_str)).unwrap();
            let mut ctx = Context::new();
            ctx.insert("name", "multi_rdf_app");
            let mut pipeline_clone = Pipeline::new().unwrap();

            template.render_frontmatter(pipeline_clone.tera_mut(), &ctx).unwrap();
            template.process_graph(&mut graph, pipeline_clone.tera_mut(), &ctx, &rdf_paths[0]).unwrap();
            let output = template.render(pipeline_clone.tera_mut(), &ctx).unwrap();
            black_box(output)
        });
    });

    // Memory profiling
    group.bench_function("memory_profile_10_files", |b| {
        b.iter_custom(|iters| {
            let mut total = std::time::Duration::ZERO;

            for _ in 0..iters {
                let mut mem_snapshot = MemorySnapshot::new();
                let start = Instant::now();

                let mut template = Template::parse(black_box(&template_str)).unwrap();
                let mut graph = Graph::new().unwrap();
                let mut pipeline = Pipeline::new().unwrap();
                let mut ctx = Context::new();
                ctx.insert("name", "multi_rdf_app");

                mem_snapshot.update_peak();

                for rdf_path in &rdf_paths {
                    let rdf_content = fs::read_to_string(rdf_path).unwrap();
                    graph.insert_turtle(&rdf_content).unwrap();
                    mem_snapshot.update_peak();
                }

                template.render_frontmatter(pipeline.tera_mut(), &ctx).unwrap();
                template.process_graph(&mut graph, pipeline.tera_mut(), &ctx, &rdf_paths[0]).unwrap();
                let _output = template.render(pipeline.tera_mut(), &ctx).unwrap();

                mem_snapshot.finish();
                total += start.elapsed();

                if let Some(delta) = mem_snapshot.delta_kb() {
                    eprintln!("Memory delta: {} KB", delta);
                }
                if let Some(peak) = mem_snapshot.peak_kb() {
                    eprintln!("Peak memory: {} KB", peak);
                }

                black_box(_output);
            }

            total
        });
    });

    group.finish();
}

// ============================================================================
// BENCHMARK 3: 10 Templates + Shared RDF
// Target: <1s
// ============================================================================

fn bench_multiple_templates_shared_rdf(c: &mut Criterion) {
    let mut group = c.benchmark_group("multiple_templates_shared_rdf");
    group.significance_level(0.1).sample_size(20);

    let rdf_file = create_rdf_file(1000);
    let rdf_path = rdf_file.path().to_path_buf();

    // Create 10 different templates
    let templates: Vec<String> = (0..10)
        .map(|i| {
            format!(
                r#"---
to: "output/file_{}.rs"
prefixes:
  ex: "http://example.org/"
sparql:
  entities: "SELECT ?s ?name WHERE {{ ?s a ex:Type{} ; ex:name ?name }}"
---
// Generated file {}
fn main() {{
    println!("Count: {{{{ sparql_count(results=sparql_results.entities) }}}}");
}}
"#,
                i,
                i % 10,
                i
            )
        })
        .collect();

    // Cold start - parse all templates and load RDF
    group.bench_function("cold_start_10_templates", |b| {
        b.iter_custom(|iters| {
            let mut total = std::time::Duration::ZERO;

            for _ in 0..iters {
                let start = Instant::now();

                let mut graph = Graph::new().unwrap();
                let rdf_content = fs::read_to_string(&rdf_path).unwrap();
                graph.insert_turtle(&rdf_content).unwrap();

                let mut outputs = Vec::new();

                for template_str in &templates {
                    let mut pipeline = Pipeline::new().unwrap();
                    let mut template = Template::parse(black_box(template_str)).unwrap();
                    let mut ctx = Context::new();
                    ctx.insert("name", "shared_rdf_app");

                    template.render_frontmatter(pipeline.tera_mut(), &ctx).unwrap();
                    template.process_graph(&mut graph, pipeline.tera_mut(), &ctx, &rdf_path).unwrap();
                    let output = template.render(pipeline.tera_mut(), &ctx).unwrap();
                    outputs.push(output);
                }

                total += start.elapsed();
                black_box(outputs);
            }

            total
        });
    });

    // Warm cache - shared graph
    group.bench_function("warm_cache_10_templates", |b| {
        let mut graph = Graph::new().unwrap();
        let rdf_content = fs::read_to_string(&rdf_path).unwrap();
        graph.insert_turtle(&rdf_content).unwrap();

        b.iter(|| {
            let mut outputs = Vec::new();

            for template_str in &templates {
                let mut template = Template::parse(black_box(template_str)).unwrap();
                let mut ctx = Context::new();
                ctx.insert("name", "shared_rdf_app");
                let mut pipeline_clone = Pipeline::new().unwrap();

                template.render_frontmatter(pipeline_clone.tera_mut(), &ctx).unwrap();
                template.process_graph(&mut graph, pipeline_clone.tera_mut(), &ctx, &rdf_path).unwrap();
                let output = template.render(pipeline_clone.tera_mut(), &ctx).unwrap();
                outputs.push(output);
            }

            black_box(outputs)
        });
    });

    // Parallel processing with rayon
    group.bench_function("parallel_10_templates", |b| {
        use rayon::prelude::*;

        let rdf_content = fs::read_to_string(&rdf_path).unwrap();

        b.iter(|| {
            let outputs: Vec<_> = templates
                .par_iter()
                .map(|template_str| {
                    let mut graph = Graph::new().unwrap();
                    graph.insert_turtle(&rdf_content).unwrap();
                    let mut pipeline = Pipeline::new().unwrap();
                    let mut template = Template::parse(template_str).unwrap();
                    let mut ctx = Context::new();
                    ctx.insert("name", "shared_rdf_app");

                    template.render_frontmatter(pipeline.tera_mut(), &ctx).unwrap();
                    template.process_graph(&mut graph, pipeline.tera_mut(), &ctx, &rdf_path).unwrap();
                    template.render(pipeline.tera_mut(), &ctx).unwrap()
                })
                .collect();

            black_box(outputs)
        });
    });

    group.finish();
}

// ============================================================================
// BENCHMARK 4: SPARQL Query Execution
// Target: <50ms per query
// ============================================================================

fn bench_sparql_execution(c: &mut Criterion) {
    let mut group = c.benchmark_group("sparql_execution");
    group.significance_level(0.1).sample_size(100);

    // Prepare graph with test data
    let rdf_file = create_rdf_file(10000);
    let rdf_path = rdf_file.path().to_path_buf();
    let mut graph = Graph::new().unwrap();
    let rdf_content = fs::read_to_string(&rdf_path).unwrap();
    graph.insert_turtle(&rdf_content).unwrap();

    // Simple SELECT query
    group.bench_function("simple_select", |b| {
        b.iter(|| {
            let query = "PREFIX ex: <http://example.org/>\nSELECT ?s WHERE { ?s a ex:Type0 }";
            let results = graph.query(black_box(query)).unwrap();
            black_box(results)
        });
    });

    // Complex SELECT with filters
    group.bench_function("complex_select_with_filter", |b| {
        b.iter(|| {
            let query = r#"
                PREFIX ex: <http://example.org/>
                SELECT ?s ?name ?value WHERE {
                    ?s a ex:Type0 ;
                       ex:name ?name ;
                       ex:value ?value .
                    FILTER(?value > 500)
                }
                ORDER BY ?value
            "#;
            let results = graph.query(black_box(query)).unwrap();
            black_box(results)
        });
    });

    // ASK query
    group.bench_function("ask_query", |b| {
        b.iter(|| {
            let query = "PREFIX ex: <http://example.org/>\nASK { ?s a ex:Type0 }";
            let results = graph.query(black_box(query)).unwrap();
            black_box(results)
        });
    });

    // Multiple queries in sequence
    group.bench_function("5_queries_sequential", |b| {
        b.iter(|| {
            for i in 0..5 {
                let query = format!(
                    "PREFIX ex: <http://example.org/>\nSELECT ?s WHERE {{ ?s a ex:Type{} }}",
                    i % 10
                );
                let results = graph.query(&query).unwrap();
                black_box(results);
            }
        });
    });

    // Query with aggregation
    group.bench_function("aggregation_query", |b| {
        b.iter(|| {
            let query = r#"
                PREFIX ex: <http://example.org/>
                SELECT (COUNT(?s) as ?count) WHERE {
                    ?s a ex:Type0 .
                }
            "#;
            let results = graph.query(black_box(query)).unwrap();
            black_box(results)
        });
    });

    group.finish();
}

// ============================================================================
// BENCHMARK 5: Full Project Generation
// Target: <2s
// ============================================================================

fn bench_full_project_generation(c: &mut Criterion) {
    let mut group = c.benchmark_group("full_project_generation");
    group.significance_level(0.1).sample_size(10);
    group.measurement_time(std::time::Duration::from_secs(20));

    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    fs::create_dir_all(&output_dir).unwrap();

    // Simulate a realistic project with multiple templates and RDF files
    let project_templates: Vec<(String, String)> = vec![
        // Main application
        (
            "src/main.rs".to_string(),
            r#"---
to: "src/main.rs"
prefixes:
  ex: "http://example.org/"
sparql:
  modules: "SELECT ?name WHERE { ?s a ex:Module ; ex:name ?name }"
---
// Auto-generated main.rs
{% for row in sparql_results.modules %}
mod {{ row.name }};
{% endfor %}

fn main() {
    println!("Application started");
}
"#
            .to_string(),
        ),
        // Configuration
        (
            "src/config.rs".to_string(),
            r#"---
to: "src/config.rs"
prefixes:
  ex: "http://example.org/"
sparql:
  settings: "SELECT ?key ?value WHERE { ?s a ex:Setting ; ex:key ?key ; ex:value ?value }"
---
// Auto-generated config.rs
use std::collections::HashMap;

pub fn get_config() -> HashMap<String, String> {
    let mut config = HashMap::new();
    {% for row in sparql_results.settings %}
    config.insert("{{ row.key }}".to_string(), "{{ row.value }}".to_string());
    {% endfor %}
    config
}
"#
            .to_string(),
        ),
        // Tests
        (
            "tests/integration.rs".to_string(),
            r#"---
to: "tests/integration.rs"
prefixes:
  ex: "http://example.org/"
sparql:
  test_cases: "SELECT ?name WHERE { ?s a ex:TestCase ; ex:name ?name }"
---
// Auto-generated integration tests
#[cfg(test)]
mod tests {
    {% for row in sparql_results.test_cases %}
    #[test]
    fn test_{{ row.name }}() {
        // Test {{ row.name }}
        assert!(true);
    }
    {% endfor %}
}
"#
            .to_string(),
        ),
    ];

    // Create RDF data files
    let rdf_files = vec![
        // Modules
        r#"@prefix ex: <http://example.org/> .
ex:Module1 a ex:Module ; ex:name "auth" .
ex:Module2 a ex:Module ; ex:name "database" .
ex:Module3 a ex:Module ; ex:name "api" ."#,
        // Settings
        r#"@prefix ex: <http://example.org/> .
ex:Setting1 a ex:Setting ; ex:key "app_name" ; ex:value "MyApp" .
ex:Setting2 a ex:Setting ; ex:key "version" ; ex:value "1.0.0" ."#,
        // Test cases
        r#"@prefix ex: <http://example.org/> .
ex:Test1 a ex:TestCase ; ex:name "auth_login" .
ex:Test2 a ex:TestCase ; ex:name "auth_logout" .
ex:Test3 a ex:TestCase ; ex:name "db_connection" ."#,
    ];

    // End-to-end project generation
    group.bench_function("e2e_project_generation", |b| {
        b.iter_custom(|iters| {
            let mut total = std::time::Duration::ZERO;

            for _ in 0..iters {
                let start = Instant::now();

                // Load all RDF data
                let mut graph = Graph::new().unwrap();
                for rdf_data in &rdf_files {
                    graph.insert_turtle(rdf_data).unwrap();
                }

                // Generate all files
                let mut generated_files = Vec::new();

                for (file_path, template_str) in &project_templates {
                    let mut pipeline = Pipeline::new().unwrap();
                    let mut template = Template::parse(template_str).unwrap();
                    let mut ctx = Context::new();
                    ctx.insert("project_name", "TestProject");

                    template.render_frontmatter(pipeline.tera_mut(), &ctx).unwrap();
                    template.process_graph(&mut graph, pipeline.tera_mut(), &ctx, temp_dir.path()).unwrap();
                    let output = template.render(pipeline.tera_mut(), &ctx).unwrap();

                    // Write to disk
                    let output_path = output_dir.join(file_path);
                    if let Some(parent) = output_path.parent() {
                        fs::create_dir_all(parent).ok();
                    }
                    fs::write(&output_path, &output).unwrap();

                    generated_files.push(output);
                }

                total += start.elapsed();
                black_box(generated_files);
            }

            total
        });
    });

    // Parallel project generation
    group.bench_function("parallel_project_generation", |b| {
        use rayon::prelude::*;

        let rdf_data_combined: String = rdf_files.join("\n");

        b.iter(|| {
            // Each parallel task needs its own graph to avoid mutable borrow conflicts
            let outputs: Vec<_> = project_templates
                .par_iter()
                .map(|(file_path, template_str)| {
                    // Create separate graph for each parallel task
                    let mut graph = Graph::new().unwrap();
                    graph.insert_turtle(&rdf_data_combined).unwrap();

                    let mut pipeline = Pipeline::new().unwrap();
                    let mut template = Template::parse(template_str).unwrap();
                    let mut ctx = Context::new();
                    ctx.insert("project_name", "TestProject");

                    template.render_frontmatter(pipeline.tera_mut(), &ctx).unwrap();
                    template.process_graph(&mut graph, pipeline.tera_mut(), &ctx, temp_dir.path()).unwrap();
                    let output = template.render(pipeline.tera_mut(), &ctx).unwrap();

                    (file_path.clone(), output)
                })
                .collect();

            black_box(outputs)
        });
    });

    group.finish();
}

// ============================================================================
// BENCHMARK 6: Cold Start vs Warm Cache Analysis
// ============================================================================

fn bench_cache_effectiveness(c: &mut Criterion) {
    let mut group = c.benchmark_group("cache_effectiveness");

    let template_str = create_rdf_template(100, 5);
    let rdf_file = create_rdf_file(1000);
    let rdf_path = rdf_file.path().to_path_buf();

    // Measure first run (cold)
    group.bench_function("first_run_cold", |b| {
        b.iter(|| {
            let mut template = Template::parse(black_box(&template_str)).unwrap();
            let mut graph = Graph::new().unwrap();
            let mut pipeline = Pipeline::new().unwrap();
            let mut ctx = Context::new();
            ctx.insert("name", "cache_test");

            let rdf_content = fs::read_to_string(&rdf_path).unwrap();
            graph.insert_turtle(&rdf_content).unwrap();

            template.render_frontmatter(pipeline.tera_mut(), &ctx).unwrap();
            template.process_graph(&mut graph, pipeline.tera_mut(), &ctx, &rdf_path).unwrap();
            let output = template.render(pipeline.tera_mut(), &ctx).unwrap();
            black_box(output)
        });
    });

    // Measure subsequent runs (warm)
    group.bench_function("subsequent_run_warm", |b| {
        let mut graph = Graph::new().unwrap();
        let rdf_content = fs::read_to_string(&rdf_path).unwrap();
        graph.insert_turtle(&rdf_content).unwrap();

        b.iter(|| {
            let mut template = Template::parse(black_box(&template_str)).unwrap();
            let mut ctx = Context::new();
            ctx.insert("name", "cache_test");
            let mut pipeline_clone = Pipeline::new().unwrap();

            template.render_frontmatter(pipeline_clone.tera_mut(), &ctx).unwrap();
            template.process_graph(&mut graph, pipeline_clone.tera_mut(), &ctx, &rdf_path).unwrap();
            let output = template.render(pipeline_clone.tera_mut(), &ctx).unwrap();
            black_box(output)
        });
    });

    group.finish();
}

// ============================================================================
// Criterion Configuration
// ============================================================================

criterion_group!(
    name = core_benchmarks;
    config = Criterion::default()
        .warm_up_time(std::time::Duration::from_secs(3))
        .measurement_time(std::time::Duration::from_secs(10));
    targets =
        bench_single_template_single_rdf,
        bench_sparql_execution,
        bench_cache_effectiveness
);

criterion_group!(
    name = advanced_benchmarks;
    config = Criterion::default()
        .warm_up_time(std::time::Duration::from_secs(5))
        .measurement_time(std::time::Duration::from_secs(15));
    targets =
        bench_single_template_multiple_rdf,
        bench_multiple_templates_shared_rdf,
        bench_full_project_generation
);

criterion_main!(core_benchmarks, advanced_benchmarks);
