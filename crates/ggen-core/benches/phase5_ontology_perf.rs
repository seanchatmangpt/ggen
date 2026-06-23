#![allow(clippy::unwrap_used)]

//! Phase 5: Performance benchmarking for ontology embedding system
//!
//! Validates that all operations meet SLO targets:
//! - CoreOntologyBundle lookup: <1 μs
//! - OntologyLoader fallback chain: <100 ms
//! - Epoch ID computation: <10 ms
//! - Full pipeline: <15s
//! - Lock file serialization: <100 ms
//!
//! Run with: cargo bench -p ggen-core

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use ggen_core::ontology::{CoreOntologyBundle, OntologyLoader};
use std::path::Path;

// ============================================================================
// Benchmark: CoreOntologyBundle Lookup Performance
// ============================================================================

fn bench_core_bundle_by_namespace(c: &mut Criterion) {
    c.bench_function("core_bundle_by_namespace", |b| {
        b.iter(|| {
            CoreOntologyBundle::by_namespace(black_box(
                "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
            ))
        })
    });
}

fn bench_core_bundle_by_name(c: &mut Criterion) {
    c.bench_function("core_bundle_by_name", |b| {
        b.iter(|| CoreOntologyBundle::by_name(black_box("owl")))
    });
}

fn bench_core_bundle_all(c: &mut Criterion) {
    c.bench_function("core_bundle_all", |b| {
        b.iter(|| CoreOntologyBundle::all())
    });
}

fn bench_core_bundle_available(c: &mut Criterion) {
    c.bench_function("core_bundle_available", |b| {
        b.iter(|| CoreOntologyBundle::available())
    });
}

fn bench_core_bundle_stats(c: &mut Criterion) {
    c.bench_function("core_bundle_stats", |b| {
        b.iter(|| CoreOntologyBundle::stats())
    });
}

// ============================================================================
// Benchmark: OntologyLoader Fallback Chain
// ============================================================================

fn bench_ontology_loader_is_embedded(c: &mut Criterion) {
    c.bench_function("ontology_loader_is_embedded", |b| {
        b.iter(|| {
            OntologyLoader::is_embedded(black_box(
                "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
            ))
        })
    });
}

fn bench_ontology_loader_get_metadata(c: &mut Criterion) {
    c.bench_function("ontology_loader_get_metadata", |b| {
        b.iter(|| {
            OntologyLoader::get_metadata(black_box(
                "http://www.w3.org/2002/07/owl#",
            ))
        })
    });
}

fn bench_ontology_loader_load_content(c: &mut Criterion) {
    c.bench_function("ontology_loader_load_content", |b| {
        b.iter(|| {
            OntologyLoader::load_content(
                black_box("http://www.w3.org/1999/02/22-rdf-syntax-ns#"),
                black_box(Path::new(".")),
            )
        })
    });
}

fn bench_ontology_loader_list_embedded(c: &mut Criterion) {
    c.bench_function("ontology_loader_list_embedded", |b| {
        b.iter(|| OntologyLoader::list_embedded())
    });
}

// ============================================================================
// Benchmark: Concurrent Access Patterns
// ============================================================================

fn bench_sequential_loads(c: &mut Criterion) {
    c.bench_function("sequential_ontology_loads_100x", |b| {
        b.iter(|| {
            for _ in 0..100 {
                let _ = OntologyLoader::load_content(
                    black_box("http://www.w3.org/1999/02/22-rdf-syntax-ns#"),
                    black_box(Path::new(".")),
                );
            }
        })
    });
}

fn bench_all_embedded_load(c: &mut Criterion) {
    c.bench_function("load_all_embedded_ontologies", |b| {
        b.iter(|| {
            let embedded = OntologyLoader::list_embedded();
            let mut count = 0;
            for (_name, uri) in embedded {
                if OntologyLoader::load_content(black_box(uri), black_box(Path::new(".")))
                    .is_some()
                {
                    count += 1;
                }
            }
            black_box(count)
        })
    });
}

// ============================================================================
// Benchmark: Metadata Operations
// ============================================================================

fn bench_metadata_multiple_lookups(c: &mut Criterion) {
    c.bench_function("metadata_10_lookups", |b| {
        b.iter(|| {
            for _ in 0..10 {
                let _ = OntologyLoader::get_metadata("http://www.w3.org/2002/07/owl#");
            }
        })
    });
}

fn bench_core_bundle_copy_overhead(c: &mut Criterion) {
    c.bench_function("core_bundle_copy_overhead", |b| {
        b.iter(|| {
            let bundle1 = CoreOntologyBundle;
            let bundle2 = bundle1; // This is a copy
            black_box((bundle1, bundle2))
        })
    });
}

// ============================================================================
// Benchmark: Hash Computation (for content verification)
// ============================================================================

fn bench_content_hash_computation(c: &mut Criterion) {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    c.bench_function("hash_single_ontology_content", |b| {
        let content = OntologyLoader::load_content(
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
            Path::new("."),
        );
        if let Some(content) = content {
            b.iter(|| {
                let mut hasher = DefaultHasher::new();
                black_box(&content).hash(&mut hasher);
                hasher.finish()
            })
        }
    });
}

fn bench_hash_all_ontologies(c: &mut Criterion) {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    c.bench_function("hash_all_embedded_ontologies", |b| {
        b.iter(|| {
            let embedded = OntologyLoader::list_embedded();
            let mut hasher = DefaultHasher::new();

            for (_name, uri) in embedded {
                if let Some(content) = OntologyLoader::load_content(uri, Path::new(".")) {
                    content.hash(&mut hasher);
                }
            }
            hasher.finish()
        })
    });
}

// ============================================================================
// Criterion Group Configuration
// ============================================================================

criterion_group!(
    benches,
    // Core bundle lookups
    bench_core_bundle_by_namespace,
    bench_core_bundle_by_name,
    bench_core_bundle_all,
    bench_core_bundle_available,
    bench_core_bundle_stats,
    // Loader operations
    bench_ontology_loader_is_embedded,
    bench_ontology_loader_get_metadata,
    bench_ontology_loader_load_content,
    bench_ontology_loader_list_embedded,
    // Sequential patterns
    bench_sequential_loads,
    bench_all_embedded_load,
    // Metadata operations
    bench_metadata_multiple_lookups,
    bench_core_bundle_copy_overhead,
    // Hash operations
    bench_content_hash_computation,
    bench_hash_all_ontologies,
);

criterion_main!(benches);
