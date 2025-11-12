use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use ggen_core::graph::Graph;
use ggen_core::pipeline::Pipeline;
use ggen_core::template::Template;
use std::collections::BTreeMap;
use std::fs;
use std::io::Write;
use tempfile::{NamedTempFile, TempDir};
use tera::Context;

// ============================================================================
// BENCHMARK 1: Template Parsing Performance
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
// Generated file
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
// Complex generated file
fn main() {{
    println!("Hello, {{{{ name }}}}!");
}}
"#,
        vars.join("\n"),
        rdf.join("\n"),
        sparql.join("\n")
    )
}

fn bench_template_parsing(c: &mut Criterion) {
    let mut group = c.benchmark_group("template_parsing");

    // Simple template parsing
    for var_count in [1, 10, 50].iter() {
        let template_str = create_simple_template(*var_count);

        group.bench_with_input(BenchmarkId::new("simple", var_count), var_count, |b, _| {
            b.iter(|| {
                let result = Template::parse(black_box(&template_str));
                black_box(result)
            });
        });
    }

    // Complex template parsing (with RDF and SPARQL)
    for complexity in [(5, 5, 5), (10, 10, 10), (20, 20, 20)].iter() {
        let (vars, rdf, sparql) = complexity;
        let template_str = create_complex_template(*vars, *rdf, *sparql);

        group.bench_with_input(
            BenchmarkId::new("complex", format!("v{}_r{}_s{}", vars, rdf, sparql)),
            &template_str,
            |b, tmpl_str| {
                b.iter(|| {
                    let result = Template::parse(black_box(tmpl_str));
                    black_box(result)
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// BENCHMARK 2: Frontmatter Rendering Performance
// ============================================================================

fn bench_frontmatter_rendering(c: &mut Criterion) {
    let mut group = c.benchmark_group("frontmatter_rendering");

    let pipeline = Pipeline::new().unwrap();

    for var_count in [1, 10, 50, 100].iter() {
        let template_str = create_simple_template(*var_count);
        let mut template = Template::parse(&template_str).unwrap();

        let mut ctx = Context::new();
        ctx.insert("name", "test_app");

        group.bench_with_input(BenchmarkId::new("render", var_count), var_count, |b, _| {
            b.iter(|| {
                let mut tera = pipeline.tera.clone();
                let result = template.render_frontmatter(&mut tera, &ctx);
                black_box(result)
            });
        });
    }

    group.finish();
}

// ============================================================================
// BENCHMARK 3: RDF Metadata Processing
// ============================================================================

fn bench_rdf_processing(c: &mut Criterion) {
    let mut group = c.benchmark_group("rdf_processing");

    let pipeline = Pipeline::new().unwrap();
    let temp_dir = TempDir::new().unwrap();
    let template_path = temp_dir.path().join("test.tmpl");

    for rdf_count in [1, 10, 50, 100].iter() {
        let template_str = create_complex_template(5, *rdf_count, 0);
        let mut template = Template::parse(&template_str).unwrap();

        let mut ctx = Context::new();
        ctx.insert("name", "test_app");

        group.bench_with_input(
            BenchmarkId::new("rdf_insert", rdf_count),
            rdf_count,
            |b, _| {
                b.iter(|| {
                    let mut graph = Graph::new().unwrap();
                    let mut tera = pipeline.tera.clone();
                    let result =
                        template.process_graph(&mut graph, &mut tera, &ctx, &template_path);
                    black_box(result)
                });
            },
        );
    }

    group.finish();
}

fn bench_sparql_execution(c: &mut Criterion) {
    let mut group = c.benchmark_group("sparql_execution");

    let pipeline = Pipeline::new().unwrap();
    let temp_dir = TempDir::new().unwrap();
    let template_path = temp_dir.path().join("test.tmpl");

    for query_count in [1, 5, 10, 20].iter() {
        let template_str = create_complex_template(5, 10, *query_count);
        let mut template = Template::parse(&template_str).unwrap();

        let mut ctx = Context::new();
        ctx.insert("name", "test_app");

        group.bench_with_input(
            BenchmarkId::new("sparql_query", query_count),
            query_count,
            |b, _| {
                b.iter(|| {
                    let mut graph = Graph::new().unwrap();
                    let mut tera = pipeline.tera.clone();
                    let result =
                        template.process_graph(&mut graph, &mut tera, &ctx, &template_path);
                    black_box(result)
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// BENCHMARK 4: Variable Substitution Performance
// ============================================================================

fn create_template_with_substitutions(sub_count: usize) -> String {
    let body_lines: Vec<String> = (0..sub_count)
        .map(|i| format!("// Line {}: {{{{ var{} }}}}", i, i % 10))
        .collect();

    format!(
        r#"---
to: "output/{{{{ name }}}}.rs"
vars:
  var0: value0
  var1: value1
  var2: value2
  var3: value3
  var4: value4
  var5: value5
  var6: value6
  var7: value7
  var8: value8
  var9: value9
---
{}
"#,
        body_lines.join("\n")
    )
}

fn bench_variable_substitution(c: &mut Criterion) {
    let mut group = c.benchmark_group("variable_substitution");

    let pipeline = Pipeline::new().unwrap();

    for sub_count in [10, 50, 100, 500].iter() {
        let template_str = create_template_with_substitutions(*sub_count);
        let mut template = Template::parse(&template_str).unwrap();

        let mut ctx = Context::new();
        ctx.insert("name", "test_app");

        group.bench_with_input(
            BenchmarkId::new("substitute", sub_count),
            sub_count,
            |b, _| {
                b.iter(|| {
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
// BENCHMARK 5: Large File Tree Generation
// ============================================================================

fn create_file_tree_template(file_count: usize) -> Vec<String> {
    (0..file_count)
        .map(|i| {
            format!(
                r#"---
to: "output/file_{}.rs"
---
// Generated file {}
fn main() {{
    println!("File {}: {{{{ name }}}}");
}}
"#,
                i, i, i
            )
        })
        .collect()
}

fn bench_file_tree_generation(c: &mut Criterion) {
    let mut group = c.benchmark_group("file_tree_generation");
    group.sample_size(10); // Reduce sample size for large benchmarks

    let pipeline = Pipeline::new().unwrap();
    let temp_dir = TempDir::new().unwrap();

    for file_count in [10, 100, 1000].iter() {
        let templates = create_file_tree_template(*file_count);

        group.throughput(Throughput::Elements(*file_count as u64));

        group.bench_with_input(
            BenchmarkId::new("sequential", file_count),
            file_count,
            |b, _| {
                b.iter(|| {
                    let mut ctx = Context::new();
                    ctx.insert("name", "test_app");

                    for template_str in &templates {
                        let mut template = Template::parse(template_str).unwrap();
                        let mut tera = pipeline.tera.clone();
                        template.render_frontmatter(&mut tera, &ctx).unwrap();
                        let _rendered = template.render(&mut tera, &ctx).unwrap();
                        black_box(_rendered);
                    }
                });
            },
        );

        // Parallel file generation benchmark
        group.bench_with_input(
            BenchmarkId::new("parallel", file_count),
            file_count,
            |b, _| {
                b.iter(|| {
                    use rayon::prelude::*;

                    let results: Vec<_> = templates
                        .par_iter()
                        .map(|template_str| {
                            let mut template = Template::parse(template_str).unwrap();
                            let mut ctx = Context::new();
                            ctx.insert("name", "test_app");
                            let mut tera = pipeline.tera.clone();
                            template.render_frontmatter(&mut tera, &ctx).unwrap();
                            template.render(&mut tera, &ctx).unwrap()
                        })
                        .collect();

                    black_box(results);
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// BENCHMARK 6: Template Compilation and Caching
// ============================================================================

fn bench_template_caching(c: &mut Criterion) {
    let mut group = c.benchmark_group("template_caching");

    let template_str = create_simple_template(10);

    // Without caching - parse every time
    group.bench_function("no_cache", |b| {
        b.iter(|| {
            let template = Template::parse(black_box(&template_str));
            black_box(template)
        });
    });

    // With caching - parse once, reuse
    group.bench_function("with_cache", |b| {
        let template = Template::parse(&template_str).unwrap();

        b.iter(|| {
            let cloned = template.clone();
            black_box(cloned)
        });
    });

    group.finish();
}

// ============================================================================
// BENCHMARK 7: Memory Usage Benchmarks
// ============================================================================

fn bench_memory_usage(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_usage");
    group.throughput(Throughput::Elements(1000));

    // Test memory usage with 1000 templates in memory
    group.bench_function("1000_templates_in_memory", |b| {
        let template_str = create_simple_template(10);

        b.iter(|| {
            let templates: Vec<Template> = (0..1000)
                .map(|_| Template::parse(&template_str).unwrap())
                .collect();
            black_box(templates);
        });
    });

    // Test memory usage with streaming approach (parse + process + discard)
    group.bench_function("1000_templates_streaming", |b| {
        let template_str = create_simple_template(10);
        let pipeline = Pipeline::new().unwrap();

        b.iter(|| {
            for _ in 0..1000 {
                let mut template = Template::parse(&template_str).unwrap();
                let mut ctx = Context::new();
                ctx.insert("name", "test");
                let mut tera = pipeline.tera.clone();
                template.render_frontmatter(&mut tera, &ctx).unwrap();
                let rendered = template.render(&mut tera, &ctx).unwrap();
                black_box(rendered);
                // Template is dropped here, memory released
            }
        });
    });

    group.finish();
}

// ============================================================================
// BENCHMARK 8: End-to-End Template Processing
// ============================================================================

fn bench_e2e_template_processing(c: &mut Criterion) {
    let mut group = c.benchmark_group("e2e_processing");

    let pipeline = Pipeline::new().unwrap();
    let temp_dir = TempDir::new().unwrap();
    let template_path = temp_dir.path().join("test.tmpl");

    // Simple template (baseline)
    group.bench_function("simple_template", |b| {
        let template_str = create_simple_template(5);

        b.iter(|| {
            let mut template = Template::parse(&template_str).unwrap();
            let mut ctx = Context::new();
            ctx.insert("name", "test_app");
            let mut tera = pipeline.tera.clone();
            template.render_frontmatter(&mut tera, &ctx).unwrap();
            let rendered = template.render(&mut tera, &ctx).unwrap();
            black_box(rendered);
        });
    });

    // Complex template (with RDF and SPARQL)
    group.bench_function("complex_template", |b| {
        let template_str = create_complex_template(10, 10, 5);

        b.iter(|| {
            let mut template = Template::parse(&template_str).unwrap();
            let mut ctx = Context::new();
            ctx.insert("name", "test_app");
            let mut graph = Graph::new().unwrap();
            let mut tera = pipeline.tera.clone();
            template.render_frontmatter(&mut tera, &ctx).unwrap();
            template
                .process_graph(&mut graph, &mut tera, &ctx, &template_path)
                .unwrap();
            let rendered = template.render(&mut tera, &ctx).unwrap();
            black_box(rendered);
        });
    });

    group.finish();
}

// ============================================================================
// BENCHMARK 9: Preprocessor Performance
// ============================================================================

fn bench_preprocessor_integration(c: &mut Criterion) {
    let mut group = c.benchmark_group("preprocessor");

    let temp_dir = TempDir::new().unwrap();
    let template_path = temp_dir.path().join("test.tmpl");
    let out_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&out_dir).unwrap();

    let template_with_freeze = r#"---
to: "output.rs"
---
fn main() {
    println!("Hello");
    {% startfreeze id="block1" %}
    println!("Frozen content");
    {% endfreeze %}
    println!("World");
}
"#;

    // Benchmark without preprocessor
    group.bench_function("without_preprocessor", |b| {
        b.iter(|| {
            let template = Template::parse(black_box(&template_with_freeze));
            black_box(template)
        });
    });

    // Benchmark with preprocessor
    group.bench_function("with_preprocessor", |b| {
        b.iter(|| {
            let template = Template::parse_with_preprocessor(
                black_box(&template_with_freeze),
                &template_path,
                &out_dir,
            );
            black_box(template)
        });
    });

    group.finish();
}

// ============================================================================
// Benchmark Groups
// ============================================================================

criterion_group!(
    parsing_benches,
    bench_template_parsing,
    bench_frontmatter_rendering
);

criterion_group!(rdf_benches, bench_rdf_processing, bench_sparql_execution);

criterion_group!(
    rendering_benches,
    bench_variable_substitution,
    bench_file_tree_generation
);

criterion_group!(
    optimization_benches,
    bench_template_caching,
    bench_memory_usage
);

criterion_group!(
    e2e_benches,
    bench_e2e_template_processing,
    bench_preprocessor_integration
);

criterion_main!(
    parsing_benches,
    rdf_benches,
    rendering_benches,
    optimization_benches,
    e2e_benches
);
