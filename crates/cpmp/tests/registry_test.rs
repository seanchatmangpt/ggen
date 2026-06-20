use cpmp::registry::OntologyRegistry;
use cpmp::tier::OntologyTier;

#[test]
fn global_singleton_initialises_without_panic() {
    let registry = OntologyRegistry::global();
    // Calling twice must return the same pointer (OnceLock guarantee).
    let registry2 = OntologyRegistry::global();
    assert!(std::ptr::eq(registry, registry2));
}

#[test]
fn tier0_returns_exactly_eight_iris() {
    let registry = OntologyRegistry::global();
    let iris = registry.tier0_iris();
    assert_eq!(
        iris.len(),
        8,
        "expected 8 Tier-0 (Core) ontologies, got {}: {iris:?}",
        iris.len()
    );
}

#[test]
fn tier0_iris_include_all_w3c_foundations() {
    let iris = OntologyRegistry::global().tier0_iris();
    let required = [
        "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
        "http://www.w3.org/2000/01/rdf-schema#",
        "http://www.w3.org/2002/07/owl#",
        "http://www.w3.org/2001/XMLSchema#",
        "http://www.w3.org/ns/shacl#",
        "http://www.w3.org/ns/prov#",
        "http://purl.org/dc/terms/",
        "https://www.ocel-standard.org/2.0/",
    ];
    for expected in &required {
        assert!(
            iris.contains(expected),
            "tier0_iris() is missing {expected}"
        );
    }
}

#[test]
fn catalog_store_contains_triples_after_load() {
    let registry = OntologyRegistry::global();
    let triple_count = registry.catalog_triple_count();
    assert!(
        triple_count > 50,
        "catalog store should have >50 triples from catalog.ttl, got {triple_count}"
    );
}

#[test]
fn tier0_entries_have_embedded_content() {
    let registry = OntologyRegistry::global();
    let core_entries: Vec<_> = registry
        .entries()
        .filter(|e| e.tier == OntologyTier::Core)
        .collect();

    assert_eq!(core_entries.len(), 8);

    for entry in &core_entries {
        let content = entry
            .embedded_content()
            .unwrap_or_else(|| panic!("Tier-0 entry '{}' must have Embedded content", entry.iri));
        assert!(
            !content.is_empty(),
            "embedded content for {} must be non-empty",
            entry.iri
        );
        assert!(
            content.contains("owl:Ontology")
                || content.contains("owl#Ontology")
                || content.len() > 100,
            "embedded TTL for {} looks truncated: {} bytes",
            entry.iri,
            content.len()
        );
    }
}

#[test]
fn load_tier0_into_store_populates_owl_classes() {
    use oxigraph::store::Store;

    let store = Store::new().expect("oxigraph store creation failed");
    OntologyRegistry::global()
        .load_tier0_into_store(&store)
        .expect("load_tier0_into_store failed");

    let quad_count = store.quads_for_pattern(None, None, None, None).count();
    assert!(
        quad_count > 200,
        "store should contain >200 quads after loading 7 Tier-0 ontologies, got {quad_count}"
    );
}

#[test]
fn catalog_covers_all_three_tiers() {
    let registry = OntologyRegistry::global();
    let has_core = registry.entries().any(|e| e.tier == OntologyTier::Core);
    let has_cached = registry.entries().any(|e| e.tier == OntologyTier::Cached);
    let has_referenced = registry
        .entries()
        .any(|e| e.tier == OntologyTier::Referenced);

    assert!(has_core, "catalog must include Tier 0 Core entries");
    assert!(has_cached, "catalog must include Tier 1 Cached entries");
    assert!(
        has_referenced,
        "catalog must include Tier 2 Referenced entries"
    );
}

#[test]
fn total_catalog_entry_count_meets_vision_2030_minimum() {
    let registry = OntologyRegistry::global();
    let total: usize = registry.entries().count();
    assert!(
        total >= 54,
        "Vision 2030 requires ≥54 catalog entries, got {total}"
    );
}

#[test]
fn get_entry_by_iri_returns_correct_tier() {
    let registry = OntologyRegistry::global();

    let rdf = registry
        .get("http://www.w3.org/1999/02/22-rdf-syntax-ns#")
        .expect("RDF 1.1 must be in the catalog");
    assert_eq!(rdf.tier, OntologyTier::Core);
    assert_eq!(rdf.prefix, "rdf");

    let skos = registry
        .get("http://www.w3.org/2004/02/skos/core#")
        .expect("SKOS must be in the catalog");
    assert_eq!(skos.tier, OntologyTier::Cached);

    let fibo = registry
        .get("https://spec.edmcouncil.org/fibo/ontology/FND/")
        .expect("FIBO FND must be in the catalog");
    assert_eq!(fibo.tier, OntologyTier::Referenced);
}
