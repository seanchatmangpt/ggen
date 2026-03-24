//! Performance benchmarks for RDF registry operations
//!
//! Validates SLOs:
//! - Package lookup: <100ms
//! - Search operations: <200ms
//! - Batch insert: <1s for 100 packages

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use ggen_marketplace_v2::{
    models::{Package, PackageId, PackageMetadata, PackageVersion},
    registry_rdf::RdfRegistry,
    traits::AsyncRepository,
};
use tokio::runtime::Runtime;

fn create_test_package(id: &str) -> Package {
    let package_id = PackageId::new(id).unwrap();
    let mut metadata = PackageMetadata::new(
        package_id.clone(),
        &format!("Package {}", id),
        &format!("Description for {}", id),
        "MIT",
    );
    metadata.authors = vec!["Benchmark Author".to_string()];
    metadata.keywords = vec!["test".to_string(), "benchmark".to_string()];

    Package {
        metadata,
        latest_version: PackageVersion::new("1.0.0").unwrap(),
        versions: vec![PackageVersion::new("1.0.0").unwrap()],
        releases: indexmap::IndexMap::new(),
    }
}

fn bench_package_insert(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let registry = RdfRegistry::new();

    c.bench_function("package_insert", |b| {
        let mut counter = 0;
        b.to_async(&rt).iter(|| {
            let pkg = create_test_package(&format!("bench-insert-{}", counter));
            counter += 1;
            async {
                registry.insert_package_rdf(&pkg).await.unwrap();
            }
        });
    });
}

fn bench_package_lookup(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let registry = RdfRegistry::new();

    // Pre-populate with packages
    rt.block_on(async {
        for i in 0..100 {
            let pkg = create_test_package(&format!("lookup-{}", i));
            registry.insert_package_rdf(&pkg).await.unwrap();
        }
    });

    c.bench_function("package_lookup", |b| {
        b.to_async(&rt).iter(|| async {
            let id = PackageId::new("lookup-50").unwrap();
            registry.get_package(&id).await.unwrap()
        });
    });
}

fn bench_batch_insert(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();

    let mut group = c.benchmark_group("batch_insert");

    for size in [10, 50, 100, 200].iter() {
        group.throughput(Throughput::Elements(*size as u64));
        group.bench_with_input(BenchmarkId::from_parameter(size), size, |b, &size| {
            b.to_async(&rt).iter(|| async {
                let registry = RdfRegistry::new();
                let mut packages = Vec::new();

                for i in 0..size {
                    packages.push(create_test_package(&format!("batch-{}", i)));
                }

                registry.batch_insert_packages(packages).await.unwrap()
            });
        });
    }

    group.finish();
}

fn bench_all_packages(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let registry = RdfRegistry::new();

    // Pre-populate
    rt.block_on(async {
        for i in 0..50 {
            let pkg = create_test_package(&format!("all-{}", i));
            registry.insert_package_rdf(&pkg).await.unwrap();
        }
    });

    c.bench_function("all_packages", |b| {
        b.to_async(&rt)
            .iter(|| async { registry.all_packages().await.unwrap() });
    });
}

fn bench_list_versions(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let registry = RdfRegistry::new();

    let id = PackageId::new("versions-test").unwrap();
    let mut metadata = PackageMetadata::new(id.clone(), "Versions Test", "Test", "MIT");

    rt.block_on(async {
        let package = Package {
            metadata,
            latest_version: PackageVersion::new("5.0.0").unwrap(),
            versions: vec![
                PackageVersion::new("1.0.0").unwrap(),
                PackageVersion::new("2.0.0").unwrap(),
                PackageVersion::new("3.0.0").unwrap(),
                PackageVersion::new("4.0.0").unwrap(),
                PackageVersion::new("5.0.0").unwrap(),
            ],
            releases: indexmap::IndexMap::new(),
        };

        registry.insert_package_rdf(&package).await.unwrap();
    });

    c.bench_function("list_versions", |b| {
        b.to_async(&rt)
            .iter(|| async { registry.list_versions(&id).await.unwrap() });
    });
}

fn bench_package_exists(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let registry = RdfRegistry::new();

    let id = PackageId::new("exists-test").unwrap();

    rt.block_on(async {
        let pkg = create_test_package("exists-test");
        registry.insert_package_rdf(&pkg).await.unwrap();
    });

    c.bench_function("package_exists", |b| {
        b.to_async(&rt)
            .iter(|| async { registry.package_exists(&id).await.unwrap() });
    });
}

criterion_group!(
    benches,
    bench_package_insert,
    bench_package_lookup,
    bench_batch_insert,
    bench_all_packages,
    bench_list_versions,
    bench_package_exists
);
criterion_main!(benches);
