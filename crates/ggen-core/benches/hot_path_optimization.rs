//! Hot path optimization benchmarks - Before/After comparison
//!
//! This benchmark targets the 20% hot paths identified in the codebase:
//! 1. Template parsing (most frequent operation)
//! 2. Frontmatter rendering
//! 3. Variable substitution
//! 4. RDF/SPARQL processing

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use ggen_core::graph::Graph;
use ggen_core::pipeline::Pipeline;
use ggen_core::template::Template;
use std::fs;
use std::io::Write;
use tempfile::TempDir;
use tera::Context;

// ============================================================================
// OPTIMIZATION TARGET 1: Template Parsing (String allocations)
// ============================================================================

fn create_template_string_owned(var_count: usize) -> String {
    // BEFORE: Many intermediate String allocations
    let mut result = String::from("---\nto: \"output/{{ name }}.rs\"\nvars:\n");

    for i in 0..var_count {
        result.push_str(&format!("  var{}: value{}\n", i, i));
    }

    result.push_str("---\nfn main() { println!(\"Hello\"); }");
    result
}

fn create_template_string_cow(var_count: usize) -> String {
    // AFTER: Reduced allocations (potential optimization)
    let capacity = 100 + (var_count * 20); // Pre-calculate capacity
    let mut result = String::with_capacity(capacity);

    result.push_str("---\nto: \"output/{{ name }}.rs\"\nvars:\n");

    for i in 0..var_count {
        // Use push_str with static strings where possible
        result.push_str("  var");
        result.push_str(&i.to_string());
        result.push_str(": value");
        result.push_str(&i.to_string());
        result.push_str("\n");
    }

    result.push_str("---\nfn main() { println!(\"Hello\"); }");
    result
}

fn bench_template_parsing_allocation_optimization(c: &mut Criterion) {
    let mut group = c.benchmark_group("template_parsing_allocations");

    for var_count in [10, 50, 100].iter() {
        // BEFORE: String allocations in template creation
        let template_before = create_template_string_owned(*var_count);
        group.bench_with_input(BenchmarkId::new("before_owned", var_count), var_count, |b, _| {
            b.iter(|| {
                let result = Template::parse(black_box(&template_before));
                black_box(result)
            });
        });

        // AFTER: Pre-allocated capacity
        let template_after = create_template_string_cow(*var_count);
        group.bench_with_input(BenchmarkId::new("after_capacity", var_count), var_count, |b, _| {
            b.iter(|| {
                let result = Template::parse(black_box(&template_after));
                black_box(result)
            });
        });
    }

    group.finish();
}

// ============================================================================
// OPTIMIZATION TARGET 2: Frontmatter Rendering (Context insertion)
// ============================================================================

fn bench_context_insertion_str_vs_string(c: &mut Criterion) {
    let mut group = c.benchmark_group("context_insertion");

    let template_str = create_template_string_owned(50);

    for var_count in [10, 50, 100].iter() {
        // BEFORE: Inserting &str (no allocation)
        group.bench_with_input(BenchmarkId::new("str_refs", var_count), var_count, |b, &var_count| {
            b.iter(|| {
                let mut ctx = Context::new();
                ctx.insert("name", "test_app");
                for i in 0..var_count {
                    ctx.insert(&format!("var{}", i), &format!("value{}", i));
                }
                black_box(ctx)
            });
        });

        // AFTER: Using String (with allocation - baseline)
        group.bench_with_input(BenchmarkId::new("strings", var_count), var_count, |b, &var_count| {
            b.iter(|| {
                let mut ctx = Context::new();
                ctx.insert("name", "test_app");
                for i in 0..var_count {
                    let key = format!("var{}", i);
                    let value = format!("value{}", i);
                    ctx.insert(&key, &value);
                }
                black_box(ctx)
            });
        });
    }

    group.finish();
}

// ============================================================================
// OPTIMIZATION TARGET 3: Variable Substitution (Caching)
// ============================================================================

fn bench_template_caching(c: &mut Criterion) {
    let mut group = c.benchmark_group("template_caching");

    let template_str = create_template_string_owned(10);

    // BEFORE: Parse every time
    group.bench_function("parse_every_time", |b| {
        b.iter(|| {
            let template = Template::parse(black_box(&template_str)).unwrap();
            black_box(template)
        });
    });

    // AFTER: Parse once, clone (cheap operation)
    group.bench_function("parse_once_clone", |b| {
        let template = Template::parse(&template_str).unwrap();
        b.iter(|| {
            let cloned = template.clone();
            black_box(cloned)
        });
    });

    group.finish();
}

// ============================================================================
// OPTIMIZATION TARGET 4: RDF Processing (Batch operations)
// ============================================================================

fn create_rdf_template(triple_count: usize) -> String {
    let mut result = String::from("---\nrdf_inline:\n");

    for i in 0..triple_count {
        result.push_str(&format!(
            "  - \"@prefix ex: <http://example.org/> . ex:entity{} a ex:Type{} .\"\n",
            i, i
        ));
    }

    result.push_str("---\nfn main() { println!(\"Hello\"); }");
    result
}

fn bench_rdf_processing(c: &mut Criterion) {
    let mut group = c.benchmark_group("rdf_processing");

    let temp_dir = TempDir::new().unwrap();
    let template_path = temp_dir.path().join("test.tmpl");

    for triple_count in [10, 50, 100].iter() {
        let template_str = create_rdf_template(*triple_count);

        group.bench_with_input(
            BenchmarkId::new("rdf_insert", triple_count),
            triple_count,
            |b, _| {
                b.iter(|| {
                    let mut pipeline = Pipeline::new().unwrap();
                    let mut template = Template::parse(&template_str).unwrap();
                    let mut graph = Graph::new().unwrap();
                    let mut ctx = Context::new();
                    ctx.insert("name", "test_app");
                    let result = template.process_graph(
                        &mut graph,
                        pipeline.tera_mut(),
                        &ctx,
                        &template_path,
                    );
                    black_box(result)
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// OPTIMIZATION TARGET 5: File Tree Generation (Parallel vs Sequential)
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

fn bench_file_tree_generation_parallel_vs_sequential(c: &mut Criterion) {
    let mut group = c.benchmark_group("file_tree_generation");
    group.sample_size(10); // Reduce sample size for large benchmarks

    let _temp_dir = TempDir::new().unwrap();

    for file_count in [10, 100].iter() {
        let templates = create_file_tree_template(*file_count);

        group.throughput(Throughput::Elements(*file_count as u64));

        // Sequential processing
        group.bench_with_input(
            BenchmarkId::new("sequential", file_count),
            file_count,
            |b, _| {
                b.iter(|| {
                    let mut pipeline = Pipeline::new().unwrap();
                    let mut ctx = Context::new();
                    ctx.insert("name", "test_app");

                    for template_str in &templates {
                        let mut template = Template::parse(template_str).unwrap();
                        template
                            .render_frontmatter(pipeline.tera_mut(), &ctx)
                            .unwrap();
                        let _rendered = template.render(pipeline.tera_mut(), &ctx).unwrap();
                        black_box(_rendered);
                    }
                });
            },
        );

        // Parallel processing (rayon)
        group.bench_with_input(
            BenchmarkId::new("parallel", file_count),
            file_count,
            |b, _| {
                b.iter(|| {
                    use rayon::prelude::*;

                    let results: Vec<_> = templates
                        .par_iter()
                        .map(|template_str| {
                            let mut pipeline = Pipeline::new().unwrap();
                            let mut template = Template::parse(template_str).unwrap();
                            let mut ctx = Context::new();
                            ctx.insert("name", "test_app");
                            template
                                .render_frontmatter(pipeline.tera_mut(), &ctx)
                                .unwrap();
                            template.render(pipeline.tera_mut(), &ctx).unwrap()
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
// COMPREHENSIVE BENCHMARK: End-to-End Template Processing
// ============================================================================

fn bench_e2e_hot_path(c: &mut Criterion) {
    let mut group = c.benchmark_group("e2e_hot_path");

    let temp_dir = TempDir::new().unwrap();
    let template_path = temp_dir.path().join("test.tmpl");

    // Simple template (baseline)
    let simple_template = create_template_string_owned(10);
    group.bench_function("simple_template", |b| {
        b.iter(|| {
            let mut pipeline = Pipeline::new().unwrap();
            let mut template = Template::parse(&simple_template).unwrap();
            let mut ctx = Context::new();
            ctx.insert("name", "test_app");
            template
                .render_frontmatter(pipeline.tera_mut(), &ctx)
                .unwrap();
            let rendered = template.render(pipeline.tera_mut(), &ctx).unwrap();
            black_box(rendered);
        });
    });

    // Complex template (with RDF)
    let complex_template = create_rdf_template(20);
    group.bench_function("complex_template_rdf", |b| {
        b.iter(|| {
            let mut pipeline = Pipeline::new().unwrap();
            let mut template = Template::parse(&complex_template).unwrap();
            let mut ctx = Context::new();
            ctx.insert("name", "test_app");
            let mut graph = Graph::new().unwrap();
            template
                .render_frontmatter(pipeline.tera_mut(), &ctx)
                .unwrap();
            template
                .process_graph(&mut graph, pipeline.tera_mut(), &ctx, &template_path)
                .unwrap();
            let rendered = template.render(pipeline.tera_mut(), &ctx).unwrap();
            black_box(rendered);
        });
    });

    group.finish();
}

// ============================================================================
// Benchmark Groups
// ============================================================================

criterion_group!(
    allocation_benches,
    bench_template_parsing_allocation_optimization
);

criterion_group!(
    context_benches,
    bench_context_insertion_str_vs_string
);

criterion_group!(
    caching_benches,
    bench_template_caching
);

criterion_group!(
    rdf_benches,
    bench_rdf_processing
);

criterion_group!(
    parallel_benches,
    bench_file_tree_generation_parallel_vs_sequential
);

criterion_group!(
    e2e_benches,
    bench_e2e_hot_path
);

criterion_main!(
    allocation_benches,
    context_benches,
    caching_benches,
    rdf_benches,
    parallel_benches,
    e2e_benches
);
