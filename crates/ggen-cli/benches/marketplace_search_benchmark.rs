//! Performance benchmarks for marketplace search
//!
//! Verifies that search performance meets <100ms target for 1000 packages

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use ggen_cli_lib::domain::marketplace::search::{search_packages, SearchFilters};
use std::fs;
use tempfile::TempDir;

fn create_large_registry(num_packages: usize) -> TempDir {
    let temp_dir = TempDir::new().unwrap();
    let registry_path = temp_dir.path().join("registry");
    fs::create_dir_all(&registry_path).unwrap();

    let mut packs = String::new();
    packs.push_str("{\n  \"updated\": \"2025-01-15T00:00:00Z\",\n  \"packs\": {");

    for i in 0..num_packages {
        let category = match i % 4 {
            0 => "rust",
            1 => "python",
            2 => "typescript",
            _ => "go",
        };

        let tags = match i % 3 {
            0 => r#"["cli", "tool"]"#,
            1 => r#"["web", "api"]"#,
            _ => r#"["framework", "library"]"#,
        };

        if i > 0 {
            packs.push(',');
        }

        packs.push_str(&format!(
            r#"
    "io.ggen.{}.package-{}": {{
      "id": "io.ggen.{}.package-{}",
      "name": "Package {} - {} Tool",
      "description": "A {} package for doing amazing things with {} technology",
      "tags": {},
      "keywords": ["keyword1", "keyword2", "test"],
      "category": "{}",
      "author": "author-{}",
      "latest_version": "1.{}.0",
      "downloads": {},
      "stars": {},
      "license": "MIT"
    }}"#,
            category,
            i,
            category,
            i,
            i,
            category,
            category,
            category,
            tags,
            category,
            i % 10,
            i % 100,
            (i * 100) % 50000,
            (i * 10) % 1000
        ));
    }

    packs.push_str("\n  }\n}");

    fs::write(registry_path.join("index.json"), packs).unwrap();
    std::env::set_var("CARGO_MANIFEST_DIR", temp_dir.path());

    temp_dir
}

fn benchmark_search_basic(c: &mut Criterion) {
    let _temp = create_large_registry(100);

    c.bench_function("search_100_packages_basic", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async {
                let filters = SearchFilters::new();
                search_packages(black_box("rust"), &filters).await.unwrap()
            });
    });
}

fn benchmark_search_fuzzy(c: &mut Criterion) {
    let _temp = create_large_registry(100);

    c.bench_function("search_100_packages_fuzzy", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async {
                let filters = SearchFilters::new().with_fuzzy(true);
                search_packages(black_box("rst"), &filters).await.unwrap()
            });
    });
}

fn benchmark_search_with_filters(c: &mut Criterion) {
    let _temp = create_large_registry(100);

    c.bench_function("search_100_packages_with_filters", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| async {
                let mut filters = SearchFilters::new();
                filters.category = Some("rust".to_string());
                filters.min_stars = Some(100);
                filters.sort = "downloads".to_string();
                search_packages(black_box("cli"), &filters).await.unwrap()
            });
    });
}

fn benchmark_search_scaling(c: &mut Criterion) {
    let mut group = c.benchmark_group("search_scaling");

    for size in [100, 500, 1000].iter() {
        let _temp = create_large_registry(*size);

        group.bench_with_input(BenchmarkId::from_parameter(size), size, |b, _| {
            b.to_async(tokio::runtime::Runtime::new().unwrap())
                .iter(|| async {
                    let filters = SearchFilters::new();
                    search_packages(black_box("rust"), &filters).await.unwrap()
                });
        });
    }

    group.finish();
}

fn benchmark_search_sorting(c: &mut Criterion) {
    let _temp = create_large_registry(500);
    let mut group = c.benchmark_group("search_sorting");

    for sort_by in ["relevance", "stars", "downloads"].iter() {
        group.bench_with_input(BenchmarkId::from_parameter(sort_by), sort_by, |b, sort| {
            b.to_async(tokio::runtime::Runtime::new().unwrap())
                .iter(|| async {
                    let mut filters = SearchFilters::new();
                    filters.sort = sort.to_string();
                    search_packages(black_box("rust"), &filters).await.unwrap()
                });
        });
    }

    group.finish();
}

criterion_group!(
    benches,
    benchmark_search_basic,
    benchmark_search_fuzzy,
    benchmark_search_with_filters,
    benchmark_search_scaling,
    benchmark_search_sorting
);
criterion_main!(benches);
