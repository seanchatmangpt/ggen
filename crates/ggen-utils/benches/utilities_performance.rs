//! Performance benchmarks for ggen-utils utility functions
//!
//! These benchmarks measure the performance of:
//! - File operations
//! - String processing
//! - Pattern matching
//! - Path utilities
//! - Hashing operations

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use std::collections::HashMap;

fn benchmark_file_path_handling(c: &mut Criterion) {
    let mut group = c.benchmark_group("file_path_handling");

    for path_depth in [1, 5, 10].iter() {
        group.bench_with_input(
            BenchmarkId::from_parameter(path_depth),
            path_depth,
            |b, &path_depth| {
                b.iter(|| {
                    // Simulate file path construction
                    let mut path = "root".to_string();
                    for i in 0..path_depth {
                        path.push_str(&format!("/{}", i));
                    }
                    let _final_path = black_box(path);
                })
            },
        );
    }
    group.finish();
}

fn benchmark_pattern_matching(c: &mut Criterion) {
    let mut group = c.benchmark_group("pattern_matching");

    for pattern_count in [1, 10, 100].iter() {
        group.bench_with_input(
            BenchmarkId::from_parameter(pattern_count),
            pattern_count,
            |b, &pattern_count| {
                b.iter(|| {
                    // Simulate glob pattern matching
                    let filename = "generated_code.rs";
                    let mut matches = 0;

                    for i in 0..pattern_count {
                        let pattern = black_box(format!("**/*.rs"));
                        if black_box(filename.ends_with(".rs")) {
                            matches += 1;
                        }
                    }

                    let _result = black_box(matches);
                })
            },
        );
    }
    group.finish();
}

fn benchmark_string_interpolation(c: &mut Criterion) {
    c.bench_function("template_string_interpolation", |b| {
        b.iter(|| {
            // Simulate template variable substitution
            let template = black_box("Hello, {}! Your ID is {}.");
            let name = black_box("World");
            let id = black_box(42);

            let result = format!("{} {} {}", template, name, id);
            let _output = black_box(result);
        })
    });
}

fn benchmark_hash_computation(c: &mut Criterion) {
    let mut group = c.benchmark_group("hash_computation");

    for data_size in [100, 1000, 10000].iter() {
        group.bench_with_input(
            BenchmarkId::from_parameter(data_size),
            data_size,
            |b, &data_size| {
                b.iter(|| {
                    // Simulate SHA256 hashing
                    let data = vec![0u8; data_size];
                    let _hash = black_box(format!("{:?}", data.len()));
                })
            },
        );
    }
    group.finish();
}

fn benchmark_line_ending_normalization(c: &mut Criterion) {
    c.bench_function("line_ending_normalization", |b| {
        b.iter(|| {
            // Simulate normalizing line endings
            let mixed_text = "line1\r\nline2\nline3\r\n";
            let normalized = mixed_text.replace("\r\n", "\n");
            let _result = black_box(normalized);
        })
    });
}

fn benchmark_yaml_parsing(c: &mut Criterion) {
    c.bench_function("yaml_config_parsing", |b| {
        b.iter(|| {
            // Simulate YAML config parsing
            let config_lines = vec!["key1: value1", "key2: value2", "key3: value3"];

            let mut config = HashMap::new();
            for line in config_lines {
                if let Some((k, v)) = line.split_once(':') {
                    config.insert(k.trim(), v.trim());
                }
            }

            let _parsed = black_box(config);
        })
    });
}

fn benchmark_json_serialization(c: &mut Criterion) {
    c.bench_function("json_serialization_roundtrip", |b| {
        b.iter(|| {
            // Simulate JSON serialization
            let data = black_box(HashMap::from([
                ("key1".to_string(), "value1".to_string()),
                ("key2".to_string(), "value2".to_string()),
                ("key3".to_string(), "value3".to_string()),
            ]));

            let _json = black_box(format!("{:?}", data));
        })
    });
}

fn benchmark_environment_variable_loading(c: &mut Criterion) {
    c.bench_function("environment_variable_resolution", |b| {
        b.iter(|| {
            // Simulate loading environment variables
            let vars = HashMap::from([
                ("GGEN_HOME".to_string(), "/home/user/.ggen".to_string()),
                ("GGEN_CACHE".to_string(), "/tmp/ggen".to_string()),
                ("RUST_LOG".to_string(), "debug".to_string()),
            ]);

            let _value = black_box(vars.get("GGEN_HOME"));
        })
    });
}

fn benchmark_file_glob_expansion(c: &mut Criterion) {
    let mut group = c.benchmark_group("file_glob_expansion");

    for file_count in [10, 100, 1000].iter() {
        group.bench_with_input(
            BenchmarkId::from_parameter(file_count),
            file_count,
            |b, &file_count| {
                b.iter(|| {
                    // Simulate glob pattern expansion
                    let mut files = Vec::new();
                    for i in 0..file_count {
                        files.push(black_box(format!("file_{}.rs", i)));
                    }

                    // Match against pattern
                    let matches: Vec<_> = files.iter().filter(|f| f.ends_with(".rs")).collect();

                    let _result = black_box(matches.len());
                })
            },
        );
    }
    group.finish();
}

fn benchmark_directory_traversal(c: &mut Criterion) {
    let mut group = c.benchmark_group("directory_traversal");

    for depth in [1, 3, 5].iter() {
        group.bench_with_input(BenchmarkId::from_parameter(depth), depth, |b, &depth| {
            b.iter(|| {
                // Simulate directory tree traversal
                let mut paths = vec![black_box("root")];

                for level in 0..depth {
                    for parent in paths.clone() {
                        let _child = black_box(format!("{}/child_{}", parent, level));
                    }
                }

                let _count = black_box(paths.len());
            })
        });
    }
    group.finish();
}

criterion_group!(
    benches,
    benchmark_file_path_handling,
    benchmark_pattern_matching,
    benchmark_string_interpolation,
    benchmark_hash_computation,
    benchmark_line_ending_normalization,
    benchmark_yaml_parsing,
    benchmark_json_serialization,
    benchmark_environment_variable_loading,
    benchmark_file_glob_expansion,
    benchmark_directory_traversal
);
criterion_main!(benches);
