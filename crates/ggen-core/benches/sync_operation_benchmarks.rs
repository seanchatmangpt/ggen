//! Test fixture setup benchmarks - NOT ggen sync operation benchmarks
//!
//! IMPORTANT DISCLAIMER:
//! These benchmarks measure test infrastructure creation (temporary directories,
//! template files, configuration files) - NOT actual ggen sync operations.
//!
//! They do NOT call the real sync implementation. This is a SIMULATION benchmark,
//! not a measurement of actual ggen performance.
//!
//! To benchmark real ggen sync, you would need to:
//! - Call Generator::generate() or equivalent
//! - Call Pipeline::execute()
//! - Measure actual code generation operations
//!
//! Current status: These benchmarks are INVALID for measuring ggen sync.
//!
//! Run with: cargo bench --bench sync_operation_benchmarks

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use std::collections::BTreeMap;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

fn create_test_project(
    template_count: usize,
    vars_per_template: usize,
) -> (TempDir, PathBuf, PathBuf, Vec<PathBuf>) {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let templates_dir = temp_dir.path().join("templates");
    let output_dir = temp_dir.path().join("output");
    let manifest_path = temp_dir.path().join("ggen.toml");

    fs::create_dir_all(&templates_dir).expect("Failed to create templates dir");
    fs::create_dir_all(&output_dir).expect("Failed to create output dir");

    // Create manifest
    let manifest = r#"[project]
name = "test-project"
version = "0.1.0"

[generation]
output_dir = "output"
templates_dir = "templates"
ontology = "spec.ttl"
"#;

    fs::write(&manifest_path, manifest).expect("Failed to write manifest");

    // Create templates
    let mut template_paths = Vec::new();
    for i in 0..template_count {
        let template_path = templates_dir.join(format!("template_{:03}.tmpl", i));

        let mut variables = String::new();
        for j in 0..vars_per_template {
            variables.push_str(&format!("// {{{{ var_{} }}}}\n", j));
        }

        let template_content = format!(
            r#"---
to: "output/generated_{}.rs"
---
// Template {}
{}
pub fn main_{}() {{
    println!("Generated from template {}")
}}
"#,
            i, i, variables, i, i
        );

        fs::write(&template_path, template_content).expect("Failed to write template");
        template_paths.push(template_path);
    }

    (temp_dir, manifest_path, output_dir, template_paths)
}

fn bench_simple_sync(c: &mut Criterion) {
    let mut group = c.benchmark_group("simple_sync");
    group.sample_size(20);

    group.bench_function("sync_1_template_simple", |b| {
        b.iter(|| {
            let (_project, manifest, _output, _templates) = create_test_project(1, 1);
            // In real scenario, would call sync with manifest
            let _ = black_box((&manifest));
        });
    });

    group.finish();
}

fn bench_sync_varying_templates(c: &mut Criterion) {
    let mut group = c.benchmark_group("sync_varying_templates");
    group.sample_size(10);

    let template_counts = vec![1, 5, 10, 25, 50];

    for count in template_counts {
        group.throughput(Throughput::Elements(count as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(format!("{}templates", count)),
            &count,
            |b, &count| {
                b.iter(|| {
                    let (_project, _manifest, _output, _templates) =
                        create_test_project(count, 3);
                    // Would sync all templates
                });
            },
        );
    }

    group.finish();
}

fn bench_sync_varying_variables(c: &mut Criterion) {
    let mut group = c.benchmark_group("sync_varying_variables");
    group.sample_size(10);

    let var_counts = vec![1, 5, 10, 25, 50];

    for count in var_counts {
        group.bench_with_input(
            BenchmarkId::from_parameter(format!("{}vars", count)),
            &count,
            |b, &count| {
                b.iter(|| {
                    let (_project, _manifest, _output, _templates) =
                        create_test_project(10, count);
                    // Would sync with varying number of variables
                });
            },
        );
    }

    group.finish();
}

fn bench_sync_modes(c: &mut Criterion) {
    let mut group = c.benchmark_group("sync_modes");
    group.sample_size(15);

    group.bench_function("sync_default_mode", |b| {
        b.iter(|| {
            let (_project, _manifest, _output, _templates) = create_test_project(10, 5);
            // Normal sync mode
        });
    });

    group.bench_function("sync_dry_run_mode", |b| {
        b.iter(|| {
            let (_project, _manifest, _output, _templates) = create_test_project(10, 5);
            // Dry-run mode (no file writes)
        });
    });

    group.bench_function("sync_validate_only_mode", |b| {
        b.iter(|| {
            let (_project, _manifest, _output, _templates) = create_test_project(10, 5);
            // Validation only (parse, don't generate)
        });
    });

    group.finish();
}

fn bench_sync_with_force_flag(c: &mut Criterion) {
    let mut group = c.benchmark_group("sync_with_force_flag");
    group.sample_size(15);

    group.bench_function("sync_without_force_new_files", |b| {
        b.iter(|| {
            let (_project, _manifest, _output, _templates) = create_test_project(10, 5);
            // Sync new files (no overwrite)
        });
    });

    group.bench_function("sync_with_force_overwrite", |b| {
        b.iter(|| {
            let (_project, _manifest, _output, _templates) = create_test_project(10, 5);
            // Sync with force flag (overwrites existing)
        });
    });

    group.finish();
}

fn bench_variable_substitution(c: &mut Criterion) {
    let mut group = c.benchmark_group("variable_substitution");

    let vars_data = vec![
        ("small", vec![("name", "test"), ("version", "1.0.0")]),
        (
            "medium",
            vec![
                ("name", "test"),
                ("version", "1.0.0"),
                ("author", "Alice"),
                ("description", "A test project"),
                ("license", "MIT"),
            ],
        ),
        (
            "large",
            vec![
                ("name", "test"),
                ("version", "1.0.0"),
                ("author", "Alice"),
                ("description", "A test project"),
                ("license", "MIT"),
                ("repository", "https://github.com/example"),
                ("bugs", "https://github.com/example/issues"),
                ("homepage", "https://example.com"),
                ("keywords", "rust,generator,codegen"),
                ("category", "development-tools"),
            ],
        ),
    ];

    for (name, _vars) in vars_data {
        group.bench_with_input(BenchmarkId::from_parameter(name), &name, |b, _name| {
            b.iter(|| {
                // Simulate variable substitution
                let mut vars = BTreeMap::new();
                vars.insert("name".to_string(), "test-project".to_string());
                vars.insert("version".to_string(), "1.0.0".to_string());
                let _vars = black_box(vars);
            });
        });
    }

    group.finish();
}

fn bench_conflict_detection(c: &mut Criterion) {
    let mut group = c.benchmark_group("conflict_detection");
    group.sample_size(15);

    group.bench_function("detect_conflicts_no_conflicts", |b| {
        b.iter(|| {
            let (_project, _manifest, _output, _templates) = create_test_project(10, 3);
            // Check for conflicts when none exist
        });
    });

    group.bench_function("detect_conflicts_with_modifications", |b| {
        b.iter(|| {
            let (_project, _manifest, output_dir, _templates) = create_test_project(5, 3);
            // Create some existing files to test conflict detection
            fs::write(output_dir.join("generated_0.rs"), "modified content")
                .expect("Failed to write test file");
            // Check for conflicts when modifications exist
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_simple_sync,
    bench_sync_varying_templates,
    bench_sync_varying_variables,
    bench_sync_modes,
    bench_sync_with_force_flag,
    bench_variable_substitution,
    bench_conflict_detection,
);
criterion_main!(benches);
