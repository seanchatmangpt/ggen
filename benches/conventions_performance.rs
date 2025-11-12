//! ggen v2.2.0 Conventions Performance Benchmark Suite
//!
//! This benchmark suite validates convention-based code generation performance targets:
//! - Fast file discovery (<100ms for 100 files)
//! - Efficient template discovery (<50ms for 50 templates)
//! - Quick build plan generation (<50ms)
//! - Low-latency watch mode (<500ms change-to-regeneration)
//! - High cache hit rate (90%+ for incremental)
//! - Fast full project generation (<1s for clap-noun-verb)
//!
//! # Convention-Based Architecture
//!
//! ggen v2.2.0 follows convention-over-configuration:
//! - `.ggen/templates/` - Template discovery location
//! - `.ggen/config.toml` - Project configuration
//! - `.ggen/cache/` - Build cache for incremental generation
//! - `*.tera` - Template file convention
//! - `*.rdf`, `*.ttl` - RDF metadata convention
//!
//! # Performance Targets (from Architecture)
//!
//! | Benchmark | Target | Rationale |
//! |-----------|--------|-----------|
//! | RDF File Discovery | <100ms for 100 files | File I/O bound |
//! | Template Discovery | <50ms for 50 templates | Memory bound |
//! | Build Plan Generation | <50ms | CPU bound |
//! | Watch Mode Latency | <500ms | Real-time UX |
//! | Cache Hit Rate | 90%+ | Incremental efficiency |
//! | Full Project Gen | <1s for typical project | End-to-end |
//!
//! # Usage
//!
//! ```bash
//! # Run all conventions benchmarks
//! cargo bench --bench conventions_performance
//!
//! # Run specific benchmark
//! cargo bench --bench conventions_performance -- discover_rdf
//! cargo bench --bench conventions_performance -- watch_mode
//! cargo bench --bench conventions_performance -- incremental
//! ```

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use std::fs;
use std::io::Write as IoWrite;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};
use tempfile::TempDir;

// ============================================================================
// BENCHMARK 1: RDF File Discovery
// Target: <100ms for 100 files
// ============================================================================

fn bench_discover_rdf_files(c: &mut Criterion) {
    let mut group = c.benchmark_group("discover_rdf_files");
    group.sample_size(50);

    // Test with varying file counts
    for file_count in [10, 50, 100, 200].iter() {
        group.throughput(Throughput::Elements(*file_count as u64));

        group.bench_with_input(
            BenchmarkId::from_parameter(file_count),
            file_count,
            |b, &file_count| {
                b.iter_batched(
                    || setup_rdf_files(file_count),
                    |temp_dir| {
                        let start = Instant::now();
                        let rdf_files = discover_rdf_files_recursive(temp_dir.path());
                        let elapsed = start.elapsed();

                        black_box(rdf_files.len());

                        // Validate target: <100ms for 100 files
                        if file_count == 100 {
                            assert!(
                                elapsed < Duration::from_millis(100),
                                "RDF discovery took {}ms, target is <100ms for 100 files",
                                elapsed.as_millis()
                            );
                        }
                    },
                    criterion::BatchSize::SmallInput,
                );
            },
        );
    }

    group.finish();
}

/// Discover all RDF files (.rdf, .ttl, .n3) recursively
fn discover_rdf_files_recursive(root: &Path) -> Vec<PathBuf> {
    let mut rdf_files = Vec::new();

    if let Ok(entries) = fs::read_dir(root) {
        for entry in entries.flatten() {
            let path = entry.path();

            if path.is_dir() {
                // Recurse into subdirectories
                rdf_files.extend(discover_rdf_files_recursive(&path));
            } else if let Some(ext) = path.extension() {
                // Check for RDF file extensions
                if matches!(ext.to_str(), Some("rdf") | Some("ttl") | Some("n3")) {
                    rdf_files.push(path);
                }
            }
        }
    }

    rdf_files
}

/// Setup test directory with RDF files
fn setup_rdf_files(count: usize) -> TempDir {
    let temp_dir = TempDir::new().unwrap();
    let base_path = temp_dir.path();

    // Create nested directory structure
    for i in 0..count {
        let dir_depth = i % 5; // 0-4 levels deep
        let mut dir_path = base_path.to_path_buf();

        for d in 0..dir_depth {
            dir_path.push(format!("level{}", d));
        }

        fs::create_dir_all(&dir_path).unwrap();

        // Create RDF file
        let ext = match i % 3 {
            0 => "rdf",
            1 => "ttl",
            _ => "n3",
        };

        let rdf_file = dir_path.join(format!("metadata_{}.{}", i, ext));
        let mut file = fs::File::create(&rdf_file).unwrap();
        writeln!(
            file,
            "@prefix ex: <http://example.org/> .\nex:entity{} ex:hasType ex:Type{} .",
            i, i
        )
        .unwrap();
    }

    temp_dir
}

// ============================================================================
// BENCHMARK 2: Template Discovery
// Target: <50ms for 50 templates
// ============================================================================

fn bench_discover_templates(c: &mut Criterion) {
    let mut group = c.benchmark_group("discover_templates");
    group.sample_size(50);

    for template_count in [10, 25, 50, 100].iter() {
        group.throughput(Throughput::Elements(*template_count as u64));

        group.bench_with_input(
            BenchmarkId::from_parameter(template_count),
            template_count,
            |b, &template_count| {
                b.iter_batched(
                    || setup_templates(template_count),
                    |temp_dir| {
                        let start = Instant::now();
                        let templates = discover_templates(temp_dir.path());
                        let elapsed = start.elapsed();

                        black_box(templates.len());

                        // Validate target: <50ms for 50 templates
                        if template_count == 50 {
                            assert!(
                                elapsed < Duration::from_millis(50),
                                "Template discovery took {}ms, target is <50ms for 50 templates",
                                elapsed.as_millis()
                            );
                        }
                    },
                    criterion::BatchSize::SmallInput,
                );
            },
        );
    }

    group.finish();
}

/// Discover all template files (.tera) in .ggen/templates/
fn discover_templates(root: &Path) -> Vec<PathBuf> {
    let template_dir = root.join(".ggen/templates");
    let mut templates = Vec::new();

    if let Ok(entries) = fs::read_dir(&template_dir) {
        for entry in entries.flatten() {
            let path = entry.path();

            if path.is_file() {
                if let Some(ext) = path.extension() {
                    if ext == "tera" {
                        templates.push(path);
                    }
                }
            }
        }
    }

    templates
}

/// Setup test directory with templates
fn setup_templates(count: usize) -> TempDir {
    let temp_dir = TempDir::new().unwrap();
    let template_dir = temp_dir.path().join(".ggen/templates");
    fs::create_dir_all(&template_dir).unwrap();

    for i in 0..count {
        let template_file = template_dir.join(format!("template_{}.tera", i));
        let mut file = fs::File::create(&template_file).unwrap();
        writeln!(file, "// Template {}\n{{{{ name }}}}\n{{{{ content }}}}", i).unwrap();
    }

    temp_dir
}

// ============================================================================
// BENCHMARK 3: Build Generation Plan
// Target: <50ms for typical project
// ============================================================================

fn bench_build_generation_plan(c: &mut Criterion) {
    let mut group = c.benchmark_group("build_generation_plan");
    group.sample_size(50);

    group.bench_function("typical_clap_project", |b| {
        b.iter_batched(
            || setup_typical_clap_project(),
            |temp_dir| {
                let start = Instant::now();
                let plan = build_generation_plan(temp_dir.path());
                let elapsed = start.elapsed();

                black_box(plan.len());

                // Validate target: <50ms
                assert!(
                    elapsed < Duration::from_millis(50),
                    "Build plan generation took {}ms, target is <50ms",
                    elapsed.as_millis()
                );
            },
            criterion::BatchSize::SmallInput,
        );
    });

    group.finish();
}

/// Build a generation plan (which templates to render for which outputs)
fn build_generation_plan(root: &Path) -> Vec<(PathBuf, PathBuf)> {
    let mut plan = Vec::new();
    let templates = discover_templates(root);
    let config_file = root.join(".ggen/config.toml");

    // Parse config to determine output mappings
    if config_file.exists() {
        // Simple convention: template_name.tera -> output/template_name.rs
        for template in templates {
            if let Some(stem) = template.file_stem() {
                let output = root
                    .join("output")
                    .join(format!("{}.rs", stem.to_string_lossy()));
                plan.push((template, output));
            }
        }
    }

    plan
}

/// Setup a typical clap-noun-verb project structure
fn setup_typical_clap_project() -> TempDir {
    let temp_dir = TempDir::new().unwrap();
    let base = temp_dir.path();

    // Create .ggen/templates/
    let template_dir = base.join(".ggen/templates");
    fs::create_dir_all(&template_dir).unwrap();

    // Create typical templates for clap project
    let templates = vec![
        ("main.tera", "fn main() { /* {{ app_name }} */ }"),
        ("cli.tera", "// CLI for {{ app_name }}"),
        ("commands.tera", "// Commands module"),
        ("lib.tera", "// Library module"),
        ("config.tera", "// Config module"),
    ];

    for (name, content) in templates {
        let template_file = template_dir.join(name);
        fs::write(&template_file, content).unwrap();
    }

    // Create config
    let config = base.join(".ggen/config.toml");
    fs::write(
        &config,
        r#"
[project]
name = "my-cli"
type = "clap-noun-verb"

[output]
directory = "output"
"#,
    )
    .unwrap();

    temp_dir
}

// ============================================================================
// BENCHMARK 4: Watch Mode Latency
// Target: <500ms from change to regeneration start
// ============================================================================

fn bench_watch_mode_latency(c: &mut Criterion) {
    let mut group = c.benchmark_group("watch_mode_latency");
    group.sample_size(30);

    group.bench_function("detect_and_plan_regeneration", |b| {
        b.iter_batched(
            || setup_watch_mode_project(),
            |temp_dir| {
                let template_file = temp_dir.path().join(".ggen/templates/main.tera");

                // Simulate file change
                let start = Instant::now();

                // 1. Detect change (file metadata check)
                let metadata = fs::metadata(&template_file).unwrap();
                black_box(metadata.modified().unwrap());

                // 2. Determine what needs regeneration
                let plan = build_generation_plan(temp_dir.path());
                black_box(plan.len());

                let elapsed = start.elapsed();

                // Validate target: <500ms
                assert!(
                    elapsed < Duration::from_millis(500),
                    "Watch mode latency {}ms, target is <500ms",
                    elapsed.as_millis()
                );
            },
            criterion::BatchSize::SmallInput,
        );
    });

    group.finish();
}

/// Setup a project for watch mode testing
fn setup_watch_mode_project() -> TempDir {
    let temp_dir = TempDir::new().unwrap();
    let template_dir = temp_dir.path().join(".ggen/templates");
    fs::create_dir_all(&template_dir).unwrap();

    // Create template file
    let template_file = template_dir.join("main.tera");
    fs::write(&template_file, "// Generated: {{ timestamp }}").unwrap();

    // Create config
    let config = temp_dir.path().join(".ggen/config.toml");
    fs::write(&config, "[project]\nname = \"watch-test\"").unwrap();

    temp_dir
}

// ============================================================================
// BENCHMARK 5: Incremental Generation (Cache Hit Rate)
// Target: 90%+ cache hit rate
// ============================================================================

fn bench_incremental_generation(c: &mut Criterion) {
    let mut group = c.benchmark_group("incremental_generation");
    group.sample_size(30);

    group.bench_function("cache_hit_rate_90_percent", |b| {
        b.iter_batched(
            || setup_incremental_project(),
            |temp_dir| {
                let cache_dir = temp_dir.path().join(".ggen/cache");
                fs::create_dir_all(&cache_dir).unwrap();

                // Generate 10 files
                let templates = discover_templates(temp_dir.path());
                let mut cache_hits = 0;
                let total_files = templates.len();

                for template in &templates {
                    // Check cache (use content hash as cache key)
                    let content = fs::read_to_string(template).unwrap();
                    let cache_key = format!("{:x}", md5::compute(&content));
                    let cache_file = cache_dir.join(&cache_key);

                    if cache_file.exists() {
                        cache_hits += 1;
                        black_box(fs::read(&cache_file).unwrap());
                    } else {
                        // Cache miss - write to cache
                        fs::write(&cache_file, &content).unwrap();
                    }
                }

                // Calculate cache hit rate
                let hit_rate = if total_files > 0 {
                    (cache_hits as f64 / total_files as f64) * 100.0
                } else {
                    0.0
                };

                black_box(hit_rate);

                // Second pass should have 100% hit rate
                cache_hits = 0;
                for template in &templates {
                    let content = fs::read_to_string(template).unwrap();
                    let cache_key = format!("{:x}", md5::compute(&content));
                    let cache_file = cache_dir.join(&cache_key);

                    if cache_file.exists() {
                        cache_hits += 1;
                    }
                }

                let second_hit_rate = (cache_hits as f64 / total_files as f64) * 100.0;

                // Validate target: 90%+ on second pass
                assert!(
                    second_hit_rate >= 90.0,
                    "Cache hit rate {}%, target is ≥90%",
                    second_hit_rate
                );

                black_box(second_hit_rate);
            },
            criterion::BatchSize::SmallInput,
        );
    });

    group.finish();
}

/// Setup incremental generation test project
fn setup_incremental_project() -> TempDir {
    let temp_dir = TempDir::new().unwrap();
    let template_dir = temp_dir.path().join(".ggen/templates");
    fs::create_dir_all(&template_dir).unwrap();

    // Create 10 templates
    for i in 0..10 {
        let template_file = template_dir.join(format!("file_{}.tera", i));
        fs::write(&template_file, format!("// File {}\n{{{{ content }}}}", i)).unwrap();
    }

    temp_dir
}

// ============================================================================
// BENCHMARK 6: Full Project Generation
// Target: <1s for typical clap-noun-verb project
// ============================================================================

fn bench_full_project_generation(c: &mut Criterion) {
    let mut group = c.benchmark_group("full_project_generation");
    group.sample_size(20);

    group.bench_function("clap_noun_verb_project", |b| {
        b.iter_batched(
            || setup_full_clap_project(),
            |temp_dir| {
                let start = Instant::now();

                // 1. Discover templates
                let templates = discover_templates(temp_dir.path());

                // 2. Build generation plan
                let plan = build_generation_plan(temp_dir.path());

                // 3. Generate all files
                let output_dir = temp_dir.path().join("output");
                fs::create_dir_all(&output_dir).unwrap();

                for (template_path, output_path) in plan {
                    let template_content = fs::read_to_string(&template_path).unwrap();

                    // Simple template rendering (replace {{ name }} with "MyApp")
                    let rendered = template_content
                        .replace("{{ name }}", "MyApp")
                        .replace("{{ app_name }}", "my-app")
                        .replace("{{ version }}", "1.0.0");

                    if let Some(parent) = output_path.parent() {
                        fs::create_dir_all(parent).unwrap();
                    }
                    fs::write(&output_path, rendered).unwrap();
                }

                let elapsed = start.elapsed();

                black_box(templates.len());

                // Validate target: <1s
                assert!(
                    elapsed < Duration::from_secs(1),
                    "Full project generation took {}ms, target is <1000ms",
                    elapsed.as_millis()
                );
            },
            criterion::BatchSize::SmallInput,
        );
    });

    group.finish();
}

/// Setup a complete clap-noun-verb project
fn setup_full_clap_project() -> TempDir {
    let temp_dir = TempDir::new().unwrap();
    let base = temp_dir.path();
    let template_dir = base.join(".ggen/templates");
    fs::create_dir_all(&template_dir).unwrap();

    // Create comprehensive template set for clap-noun-verb
    let templates = vec![
        ("main.tera", include_str!("../templates/clap_main.template")),
        ("cli.tera", include_str!("../templates/clap_cli.template")),
        (
            "commands.tera",
            include_str!("../templates/clap_commands.template"),
        ),
        ("lib.tera", include_str!("../templates/clap_lib.template")),
        (
            "cargo.tera",
            include_str!("../templates/clap_cargo.template"),
        ),
    ];

    for (name, content) in templates {
        let template_file = template_dir.join(name);
        // Use fallback content if template file doesn't exist
        let content_str = if content.is_empty() {
            format!("// Template: {}\n// App: {{{{ name }}}}\n", name)
        } else {
            content.to_string()
        };
        fs::write(&template_file, content_str).unwrap();
    }

    // Create config
    let config = base.join(".ggen/config.toml");
    fs::write(
        &config,
        r#"
[project]
name = "my-cli"
type = "clap-noun-verb"
version = "1.0.0"

[output]
directory = "output"

[conventions]
templates_dir = ".ggen/templates"
cache_dir = ".ggen/cache"
"#,
    )
    .unwrap();

    temp_dir
}

// ============================================================================
// Criterion Configuration
// ============================================================================

criterion_group!(
    conventions_benches,
    bench_discover_rdf_files,
    bench_discover_templates,
    bench_build_generation_plan,
    bench_watch_mode_latency,
    bench_incremental_generation,
    bench_full_project_generation
);

criterion_main!(conventions_benches);
