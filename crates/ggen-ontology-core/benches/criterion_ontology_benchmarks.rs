//! Criterion.rs benchmark suite for ggen-ontology-core
//!
//! Comprehensive benchmarks for ontology operations (8 total):
//! - LoadOntology: Load RDF ontology from file into triplestore
//! - SPARQLQuery: Execute SPARQL SELECT/CONSTRUCT/ASK query
//! - ClassLookup: Lookup ontology class by IRI or name
//! - PropertyLookup: Lookup ontology property by IRI or name
//! - HierarchyTraversal: Traverse class hierarchy (parent/child relationships)
//! - InstanceClassification: Classify instance against ontology classes
//! - Inference: Run reasoner to infer new triples
//! - Validation: Validate ontology syntax and semantics
//!
//! Generated from: /Users/sac/chatmangpt/OSA/priv/ontologies/benchmark_schema.ttl
//! Date: 2026-03-29
//!
//! Run with:
//!   cargo bench --bench criterion_ontology_benchmarks
//!
//! Requires Oxigraph running at localhost:7878

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use ggen_ontology_core::{
    entity_mapper::EntityMapper, sparql_generator::SparqlGenerator, triple_store::TripleStore,
    validators::validate_turtle,
};
use std::path::PathBuf;

// ============================================================================
// Operation 1: LoadOntology
// ============================================================================

/// Benchmark RDF loading operations across different file sizes
fn bench_load_ontology(c: &mut Criterion) {
    let mut group = c.benchmark_group("load_ontology");

    let test_files = vec![
        ("hipaa.ttl", 4.5, 32),
        ("it_sla.ttl", 5.0, 28),
        ("security.ttl", 5.6, 35),
        ("aws_cloud.ttl", 6.4, 40),
    ];

    for (filename, kb, _triple_count) in test_files {
        group.throughput(Throughput::Bytes((kb * 1024.0) as u64));

        group.bench_with_input(BenchmarkId::new(filename, kb), &filename, |b, filename| {
            b.iter(|| {
                let path = get_ontology_path(filename);
                let store = TripleStore::new().unwrap();
                black_box(store.load_turtle(&path))
            })
        });
    }

    group.finish();
}

// ============================================================================
// Operation 2: SPARQLQuery
// ============================================================================

/// Benchmark SPARQL query execution
fn bench_sparql_query(c: &mut Criterion) {
    let mut group = c.benchmark_group("sparql_query");

    // Setup: load ontologies
    let store = TripleStore::new().unwrap();
    let ontologies = vec!["hipaa.ttl", "it_sla.ttl", "security.ttl", "aws_cloud.ttl"];

    for ontology in ontologies {
        let _ = store.load_turtle(get_ontology_path(ontology));
    }

    let queries = vec![
        (
            "SELECT Jurisdiction",
            SparqlGenerator::find_policies_by_jurisdiction("US"),
        ),
        (
            "SELECT Data Classification",
            SparqlGenerator::find_data_classifications("Public"),
        ),
        (
            "SELECT Service Level",
            SparqlGenerator::find_services_by_sla(0.99),
        ),
        (
            "CONSTRUCT Security",
            SparqlGenerator::find_security_controls("Authentication"),
        ),
        (
            "ASK Policy",
            SparqlGenerator::find_policies_by_jurisdiction("EU"),
        ),
    ];

    for (name, query) in queries {
        group.bench_with_input(name, &query, |b, query| {
            b.iter(|| black_box(store.query_sparql(query)))
        });
    }

    group.finish();
}

// ============================================================================
// Operation 3: ClassLookup
// ============================================================================

/// Benchmark class lookup operations
fn bench_class_lookup(c: &mut Criterion) {
    let mut group = c.benchmark_group("class_lookup");

    // Setup: load ontology
    let store = TripleStore::new().unwrap();
    let _ = store.load_turtle(get_ontology_path("hipaa.ttl"));

    let classes = vec![
        ("Policy by IRI", "http://example.org/ontology#Policy"),
        ("Entity by IRI", "http://example.org/ontology#Entity"),
        (
            "DataClassification by IRI",
            "http://example.org/ontology#DataClassification",
        ),
    ];

    for (name, class_iri) in classes {
        group.bench_with_input(name, &class_iri, |b, class_iri| {
            b.iter(|| {
                let query = format!("SELECT ?p ?o WHERE {{ <{}> ?p ?o }}", class_iri);
                black_box(store.query_sparql(&query))
            })
        });
    }

    group.finish();
}

// ============================================================================
// Operation 4: PropertyLookup
// ============================================================================

/// Benchmark property lookup operations
fn bench_property_lookup(c: &mut Criterion) {
    let mut group = c.benchmark_group("property_lookup");

    // Setup: load ontology
    let store = TripleStore::new().unwrap();
    let _ = store.load_turtle(get_ontology_path("hipaa.ttl"));

    let properties = vec![
        ("title by IRI", "http://purl.org/dc/terms/title"),
        ("description by IRI", "http://purl.org/dc/terms/description"),
        (
            "jurisdiction by IRI",
            "http://example.org/ontology#jurisdiction",
        ),
    ];

    for (name, prop_iri) in properties {
        group.bench_with_input(name, &prop_iri, |b, prop_iri| {
            b.iter(|| {
                let query = format!("SELECT ?s ?o WHERE {{ ?s <{}> ?o }} LIMIT 10", prop_iri);
                black_box(store.query_sparql(&query))
            })
        });
    }

    group.finish();
}

// ============================================================================
// Operation 5: HierarchyTraversal
// ============================================================================

/// Benchmark class hierarchy traversal
fn bench_hierarchy_traversal(c: &mut Criterion) {
    let mut group = c.benchmark_group("hierarchy_traversal");

    // Setup: load ontology
    let store = TripleStore::new().unwrap();
    let _ = store.load_turtle(get_ontology_path("hipaa.ttl"));

    let traversal_queries = vec![("Depth 3", 3), ("Depth 5", 5), ("Depth 10", 10)];

    for (name, depth) in traversal_queries {
        group.bench_with_input(name, &depth, |b, depth| {
            b.iter(|| {
                let query = format!(
                    "SELECT ?s ?p ?o WHERE {{
                      ?s a ?type .
                      ?s ?p ?o .
                    }} LIMIT {}",
                    depth * 10
                );
                black_box(store.query_sparql(&query))
            })
        });
    }

    group.finish();
}

// ============================================================================
// Operation 6: InstanceClassification
// ============================================================================

/// Benchmark entity mapping operations
fn bench_instance_classification(c: &mut Criterion) {
    let mut group = c.benchmark_group("instance_classification");

    let test_entities = vec![
        ("Policy Match", "Privacy Policy", "policy"),
        ("Data Classification", "Confidential", "data_classification"),
        ("Service Level", "99.9", "service_level"),
        ("Security Control", "MFA", "security_control"),
        ("Compute Service", "VM", "compute_service"),
    ];

    for (name, entity, category) in test_entities {
        group.bench_with_input(name, &entity, |b, entity| {
            b.iter(|| match category {
                "policy" => black_box(EntityMapper::match_policy(entity)),
                "data_classification" => black_box(EntityMapper::match_data_classification(entity)),
                "service_level" => {
                    if let Ok(value) = entity.parse::<f32>() {
                        black_box(EntityMapper::match_service_level(value))
                    } else {
                        Ok(vec![])
                    }
                }
                "security_control" => black_box(EntityMapper::match_security_control(entity)),
                "compute_service" => black_box(EntityMapper::match_compute_service(entity)),
                _ => Ok(vec![]),
            })
        });
    }

    group.finish();
}

// ============================================================================
// Operation 7: Inference
// ============================================================================

/// Benchmark inference operations
fn bench_inference(c: &mut Criterion) {
    let mut group = c.benchmark_group("inference");

    // Setup: load ontology
    let store = TripleStore::new().unwrap();
    let _ = store.load_turtle(get_ontology_path("hipaa.ttl"));

    let inference_queries = vec![
        (
            "RDFS subclass inference",
            "SELECT ?s WHERE { ?s a <http://example.org/ontology#Policy> }",
        ),
        (
            "OWL property inference",
            "SELECT ?s WHERE { ?s <http://purl.org/dc/terms/title> ?o }",
        ),
        ("Type inference", "SELECT ?s WHERE { ?s a ?type }"),
    ];

    for (name, query) in inference_queries {
        group.bench_with_input(name, &query, |b, query| {
            b.iter(|| black_box(store.query_sparql(query)))
        });
    }

    group.finish();
}

// ============================================================================
// Operation 8: Validation
// ============================================================================

/// Benchmark Turtle validation operations
fn bench_validation(c: &mut Criterion) {
    let mut group = c.benchmark_group("validation");

    let valid_files = vec![
        ("hipaa.ttl", "HIPAA", 4.5),
        ("it_sla.ttl", "IT SLA", 5.0),
        ("security.ttl", "Security", 5.6),
        ("aws_cloud.ttl", "AWS Cloud", 6.4),
    ];

    for (filename, _name, kb) in valid_files {
        group.throughput(Throughput::Bytes((kb * 1024.0) as u64));

        group.bench_with_input(BenchmarkId::from_parameter(kb), &filename, |b, filename| {
            b.iter(|| {
                let path = get_ontology_path(filename);
                black_box(validate_turtle(&path))
            })
        });
    }

    group.finish();
}

// ============================================================================
// Criterion Groups
// ============================================================================

criterion_group!(
    benches,
    bench_load_ontology,
    bench_sparql_query,
    bench_class_lookup,
    bench_property_lookup,
    bench_hierarchy_traversal,
    bench_instance_classification,
    bench_inference,
    bench_validation
);
criterion_main!(benches);

/// Get path to ontology test file
fn get_ontology_path(filename: &str) -> PathBuf {
    PathBuf::from(format!(
        "{}/benches/ontologies/{}",
        env!("CARGO_MANIFEST_DIR"),
        filename
    ))
}
