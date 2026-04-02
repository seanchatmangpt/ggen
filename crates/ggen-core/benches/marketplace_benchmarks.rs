//! Benchmark tests for ggen-marketplace

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use ggen_core::registry::{
    PackMetadata, RegistryClient, RegistryIndex, SearchParams, VersionMetadata,
};
use std::collections::HashMap;
use tempfile::TempDir;
use url::Url;

fn create_mock_registry(pack_count: usize) -> RegistryIndex {
    let mut packs = HashMap::new();

    for i in 0..pack_count {
        let id = format!("package-{}", i);
        let mut versions = HashMap::new();

        for j in 0..5 {
            let version = format!("{}.0.0", j);
            versions.insert(
                version.clone(),
                VersionMetadata {
                    version: version.clone(),
                    git_url: format!("https://github.com/test/{}.git", id),
                    git_rev: format!("v{}", version),
                    manifest_url: None,
                    sha256: format!("hash{}", j),
                },
            );
        }

        packs.insert(
            id.clone(),
            PackMetadata {
                id: id.clone(),
                name: format!("Package {}", i),
                description: format!("Description for package {}", i),
                tags: vec![format!("tag{}", i % 10), "rust".to_string()],
                keywords: vec![format!("keyword{}", i % 20), "cli".to_string()],
                category: Some("development".to_string()),
                author: Some("Test Author".to_string()),
                latest_version: "4.0.0".to_string(),
                versions,
                downloads: Some(i as u64 * 100),
                updated: Some(chrono::Utc::now()),
                license: Some("MIT".to_string()),
                homepage: None,
                repository: None,
                documentation: None,
            },
        );
    }

    RegistryIndex {
        updated: chrono::Utc::now(),
        packs,
    }
}

fn benchmark_index_serialization(c: &mut Criterion) {
    let mut group = c.benchmark_group("index_serialization");

    for size in [10, 100, 1000].iter() {
        let index = create_mock_registry(*size);

        group.bench_with_input(BenchmarkId::from_parameter(size), size, |b, _| {
            b.iter(|| serde_json::to_string(black_box(&index)).unwrap());
        });
    }

    group.finish();
}

fn benchmark_index_deserialization(c: &mut Criterion) {
    let mut group = c.benchmark_group("index_deserialization");

    for size in [10, 100, 1000].iter() {
        let index = create_mock_registry(*size);
        let json = serde_json::to_string(&index).unwrap();

        group.bench_with_input(BenchmarkId::from_parameter(size), size, |b, _| {
            b.iter(|| serde_json::from_str::<RegistryIndex>(black_box(&json)).unwrap());
        });
    }

    group.finish();
}

fn benchmark_search_performance(c: &mut Criterion) {
    let mut group = c.benchmark_group("search_performance");

    for size in [100, 1000, 10000].iter() {
        let index = create_mock_registry(*size);

        group.bench_with_input(BenchmarkId::new("simple_search", size), size, |b, _| {
            b.iter(|| {
                let query = black_box("rust");
                let query_lower = query.to_lowercase();

                let results: Vec<_> = index
                    .packs
                    .iter()
                    .filter(|(_, pack)| {
                        pack.name.to_lowercase().contains(&query_lower)
                            || pack.description.to_lowercase().contains(&query_lower)
                            || pack
                                .tags
                                .iter()
                                .any(|t| t.to_lowercase().contains(&query_lower))
                    })
                    .collect();

                results.len()
            });
        });

        group.bench_with_input(BenchmarkId::new("filtered_search", size), size, |b, _| {
            b.iter(|| {
                let query = black_box("rust");
                let category = black_box("development");
                let query_lower = query.to_lowercase();

                let results: Vec<_> = index
                    .packs
                    .iter()
                    .filter(|(_, pack)| {
                        // Category filter
                        if pack.category.as_ref().map_or(true, |c| c != category) {
                            return false;
                        }

                        // Query filter
                        pack.name.to_lowercase().contains(&query_lower)
                            || pack.description.to_lowercase().contains(&query_lower)
                            || pack
                                .tags
                                .iter()
                                .any(|t| t.to_lowercase().contains(&query_lower))
                    })
                    .collect();

                results.len()
            });
        });
    }

    group.finish();
}

fn benchmark_version_comparison(c: &mut Criterion) {
    let mut group = c.benchmark_group("version_comparison");

    let v1 = semver::Version::parse("1.2.3").unwrap();
    let v2 = semver::Version::parse("1.2.4").unwrap();
    let v3 = semver::Version::parse("2.0.0").unwrap();

    group.bench_function("version_parse", |b| {
        b.iter(|| semver::Version::parse(black_box("1.2.3")).unwrap());
    });

    group.bench_function("version_compare", |b| {
        b.iter(|| black_box(&v1) < black_box(&v2));
    });

    group.bench_function("version_compare_major", |b| {
        b.iter(|| black_box(&v1) < black_box(&v3));
    });

    group.bench_function("version_to_string", |b| {
        b.iter(|| black_box(&v1).to_string());
    });

    group.finish();
}

fn benchmark_package_lookup(c: &mut Criterion) {
    let mut group = c.benchmark_group("package_lookup");

    for size in [100, 1000, 10000].iter() {
        let index = create_mock_registry(*size);

        group.bench_with_input(BenchmarkId::from_parameter(size), size, |b, _| {
            b.iter(|| {
                let id = format!("package-{}", *size / 2);
                index.packs.get(black_box(&id))
            });
        });
    }

    group.finish();
}

fn benchmark_category_aggregation(c: &mut Criterion) {
    let mut group = c.benchmark_group("category_aggregation");

    for size in [100, 1000, 10000].iter() {
        let index = create_mock_registry(*size);

        group.bench_with_input(BenchmarkId::from_parameter(size), size, |b, _| {
            b.iter(|| {
                let mut category_counts: HashMap<String, u64> = HashMap::new();

                for (_, pack) in &index.packs {
                    if let Some(category) = &pack.category {
                        *category_counts.entry(category.clone()).or_insert(0) += 1;
                    }
                }

                category_counts
            });
        });
    }

    group.finish();
}

fn benchmark_keyword_extraction(c: &mut Criterion) {
    let mut group = c.benchmark_group("keyword_extraction");

    for size in [100, 1000, 10000].iter() {
        let index = create_mock_registry(*size);

        group.bench_with_input(BenchmarkId::from_parameter(size), size, |b, _| {
            b.iter(|| {
                let mut keyword_counts: HashMap<String, u64> = HashMap::new();

                for (_, pack) in &index.packs {
                    for keyword in &pack.keywords {
                        *keyword_counts.entry(keyword.clone()).or_insert(0) += 1;
                    }
                }

                keyword_counts
            });
        });
    }

    group.finish();
}

fn benchmark_sorting_by_downloads(c: &mut Criterion) {
    let mut group = c.benchmark_group("sorting_by_downloads");

    for size in [100, 1000].iter() {
        let index = create_mock_registry(*size);

        group.bench_with_input(BenchmarkId::from_parameter(size), size, |b, _| {
            b.iter(|| {
                let mut packages: Vec<_> = index.packs.values().collect();
                packages.sort_by(|a, b| match (a.downloads, b.downloads) {
                    (Some(a_dl), Some(b_dl)) => b_dl.cmp(&a_dl),
                    (Some(_), None) => std::cmp::Ordering::Less,
                    (None, Some(_)) => std::cmp::Ordering::Greater,
                    (None, None) => std::cmp::Ordering::Equal,
                });
                packages.len()
            });
        });
    }

    group.finish();
}

fn benchmark_memory_usage(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_usage");

    group.bench_function("create_1000_packages", |b| {
        b.iter(|| create_mock_registry(black_box(1000)));
    });

    group.bench_function("create_10000_packages", |b| {
        b.iter(|| create_mock_registry(black_box(10000)));
    });

    group.finish();
}

criterion_group!(
    benches,
    benchmark_index_serialization,
    benchmark_index_deserialization,
    benchmark_search_performance,
    benchmark_version_comparison,
    benchmark_package_lookup,
    benchmark_category_aggregation,
    benchmark_keyword_extraction,
    benchmark_sorting_by_downloads,
    benchmark_memory_usage,
);

criterion_main!(benches);
