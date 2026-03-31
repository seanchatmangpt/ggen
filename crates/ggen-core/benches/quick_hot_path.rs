//! Quick hot path benchmark - Focus on template parsing optimization
//!
//! This is a simplified version of the optimization benchmark that focuses
//! on the most frequent operation: template parsing.

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use ggen_core::template::Template;

// ============================================================================
// OPTIMIZATION 1: String Capacity Pre-allocation
// ============================================================================

fn create_template_owned(var_count: usize) -> String {
    // BEFORE: Many reallocations as string grows
    let mut result = String::from("---\nto: \"output/{{ name }}.rs\"\nvars:\n");

    for i in 0..var_count {
        result.push_str(&format!("  var{}: value{}\n", i, i));
    }

    result.push_str("---\nfn main() { println!(\"Hello\"); }");
    result
}

fn create_template_with_capacity(var_count: usize) -> String {
    // AFTER: Pre-allocate capacity to avoid reallocations
    let capacity = 100 + (var_count * 20); // Estimate: 100 chars overhead + 20 chars per var
    let mut result = String::with_capacity(capacity);

    result.push_str("---\nto: \"output/{{ name }}.rs\"\nvars:\n");

    for i in 0..var_count {
        result.push_str("  var");
        result.push_str(&i.to_string());
        result.push_str(": value");
        result.push_str(&i.to_string());
        result.push_str("\n");
    }

    result.push_str("---\nfn main() { println!(\"Hello\"); }");
    result
}

// ============================================================================
// OPTIMIZATION 2: Template Caching
// ============================================================================

fn bench_template_parsing_with_capacity(c: &mut Criterion) {
    let mut group = c.benchmark_group("template_parsing_capacity");

    for var_count in [10, 50, 100].iter() {
        // BEFORE: Default String growth (multiple reallocations)
        let template_before = create_template_owned(*var_count);
        group.bench_with_input(BenchmarkId::new("before", var_count), var_count, |b, _| {
            b.iter(|| {
                let result = Template::parse(black_box(&template_before));
                black_box(result)
            });
        });

        // AFTER: Pre-allocated capacity (single allocation)
        let template_after = create_template_with_capacity(*var_count);
        group.bench_with_input(BenchmarkId::new("after", var_count), var_count, |b, _| {
            b.iter(|| {
                let result = Template::parse(black_box(&template_after));
                black_box(result)
            });
        });
    }

    group.finish();
}

fn bench_template_caching_benefit(c: &mut Criterion) {
    let mut group = c.benchmark_group("template_caching");

    let template_str = create_template_owned(10);

    // BEFORE: Parse template on every use
    group.bench_function("parse_every_time", |b| {
        b.iter(|| {
            let template = Template::parse(black_box(&template_str)).unwrap();
            black_box(template)
        });
    });

    // AFTER: Parse once, clone (cheap reference count bump)
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
// OPTIMIZATION 3: Reduce String Allocations in Loop
// ============================================================================

fn bench_string_formatting(c: &mut Criterion) {
    let mut group = c.benchmark_group("string_formatting");

    for count in [10, 50, 100].iter() {
        // BEFORE: format! macro (allocates new String each iteration)
        group.bench_with_input(
            BenchmarkId::new("format_macro", count),
            count,
            |b, &count| {
                b.iter(|| {
                    let mut result = String::new();
                    for i in 0..count {
                        result.push_str(&format!("var{} = value{}\n", i, i));
                    }
                    black_box(result)
                });
            },
        );

        // AFTER: Push individual string pieces (fewer allocations)
        group.bench_with_input(BenchmarkId::new("push_str", count), count, |b, &count| {
            b.iter(|| {
                let mut result = String::with_capacity(count * 20);
                for i in 0..count {
                    result.push_str("var");
                    result.push_str(&i.to_string());
                    result.push_str(" = value");
                    result.push_str(&i.to_string());
                    result.push_str("\n");
                }
                black_box(result)
            });
        });
    }

    group.finish();
}

criterion_group!(parsing_benches, bench_template_parsing_with_capacity);

criterion_group!(caching_benches, bench_template_caching_benefit);

criterion_group!(string_benches, bench_string_formatting);

criterion_main!(parsing_benches, caching_benches, string_benches);
