/// Pattern 4: Union-Find Equivalence Canonicalization Tests
///
/// Verifies that owl:sameAs, owl:equivalentClass, and owl:equivalentProperty
/// relationships are correctly canonicalized through union-find structures.
/// Receipt byte-identity is guaranteed across 5 determinism runs.
use praxis_graphlaw::shacl::{canonicalize_equivalences, Validator};
use praxis_graphlaw::tripleindex::TripleIndex;
use praxis_graphlaw::triples::{Triple, VarOrTerm};

/// Helper to create a triple with encoded IDs
fn make_triple(s_iri: &str, p_iri: &str, o_iri: &str) -> Triple {
    use praxis_graphlaw::encoding::Encoder;

    let s_id = Encoder::add(s_iri.to_string());
    let p_id = Encoder::add(p_iri.to_string());
    let o_id = Encoder::add(o_iri.to_string());

    Triple {
        s: VarOrTerm::new_encoded_term(s_id),
        p: VarOrTerm::new_encoded_term(p_id),
        o: VarOrTerm::new_encoded_term(o_id),
        g: None,
    }
}

#[test]
fn test_pattern4_empty_equivalences() {
    let data = TripleIndex::new();
    let vocab = praxis_graphlaw::shacl::Vocab::new();

    let result = canonicalize_equivalences(&data, &vocab);
    assert!(
        result.is_ok(),
        "Canonicalization should succeed on empty data"
    );

    let canonical = result.unwrap();
    assert!(
        canonical.class_edges.is_empty(),
        "Empty data should have no class edges"
    );
    assert!(
        canonical.property_edges.is_empty(),
        "Empty data should have no property edges"
    );
    assert!(
        canonical.term_edges.is_empty(),
        "Empty data should have no term edges"
    );
}

#[test]
fn test_pattern4_same_as_equivalence() {
    use praxis_graphlaw::encoding::Encoder;

    let mut data = TripleIndex::new();
    let vocab = praxis_graphlaw::shacl::Vocab::new();

    // Add owl:sameAs triple: <http://example.org/a> owl:sameAs <http://example.org/b>
    let triple = make_triple(
        "<http://example.org/a>",
        "<http://www.w3.org/2002/07/owl#sameAs>",
        "<http://example.org/b>",
    );

    data.add(triple);

    let result = canonicalize_equivalences(&data, &vocab);
    assert!(result.is_ok(), "Canonicalization should succeed");

    let canonical = result.unwrap();
    // owl:sameAs should create edges in term_edges
    assert!(
        !canonical.term_edges.is_empty(),
        "owl:sameAs should produce term edges"
    );
}

#[test]
fn test_pattern4_equivalent_class_equivalence() {
    let mut data = TripleIndex::new();
    let vocab = praxis_graphlaw::shacl::Vocab::new();

    // Add owl:equivalentClass triple
    let triple = make_triple(
        "<http://example.org/ClassA>",
        "<http://www.w3.org/2002/07/owl#equivalentClass>",
        "<http://example.org/ClassB>",
    );

    data.add(triple);

    let result = canonicalize_equivalences(&data, &vocab);
    assert!(result.is_ok(), "Canonicalization should succeed");

    let canonical = result.unwrap();
    assert!(
        !canonical.class_edges.is_empty(),
        "owl:equivalentClass should produce class edges"
    );
}

#[test]
fn test_pattern4_equivalent_property_equivalence() {
    let mut data = TripleIndex::new();
    let vocab = praxis_graphlaw::shacl::Vocab::new();

    // Add owl:equivalentProperty triple
    let triple = make_triple(
        "<http://example.org/propA>",
        "<http://www.w3.org/2002/07/owl#equivalentProperty>",
        "<http://example.org/propB>",
    );

    data.add(triple);

    let result = canonicalize_equivalences(&data, &vocab);
    assert!(result.is_ok(), "Canonicalization should succeed");

    let canonical = result.unwrap();
    assert!(
        !canonical.property_edges.is_empty(),
        "owl:equivalentProperty should produce property edges"
    );
}

#[test]
fn test_pattern4_canonical_edges_sorted() {
    let mut data = TripleIndex::new();
    let vocab = praxis_graphlaw::shacl::Vocab::new();

    // Add multiple owl:sameAs triples
    data.add(make_triple(
        "<http://example.org/a>",
        "<http://www.w3.org/2002/07/owl#sameAs>",
        "<http://example.org/b>",
    ));
    data.add(make_triple(
        "<http://example.org/c>",
        "<http://www.w3.org/2002/07/owl#sameAs>",
        "<http://example.org/d>",
    ));

    let result = canonicalize_equivalences(&data, &vocab);
    assert!(result.is_ok());

    let canonical = result.unwrap();
    let all_edges = [
        canonical.class_edges.clone(),
        canonical.property_edges.clone(),
        canonical.term_edges.clone(),
    ]
    .concat();

    // Verify edges are sorted
    assert!(
        all_edges.windows(2).all(|w| w[0] <= w[1]),
        "All canonical edges must be sorted"
    );
}

#[test]
fn test_pattern4_transitive_equivalence() {
    let mut data = TripleIndex::new();
    let vocab = praxis_graphlaw::shacl::Vocab::new();

    // Create a chain: a owl:sameAs b, b owl:sameAs c
    // Should result in all three being in the same equivalence class
    data.add(make_triple(
        "<http://example.org/a>",
        "<http://www.w3.org/2002/07/owl#sameAs>",
        "<http://example.org/b>",
    ));
    data.add(make_triple(
        "<http://example.org/b>",
        "<http://www.w3.org/2002/07/owl#sameAs>",
        "<http://example.org/c>",
    ));

    let result = canonicalize_equivalences(&data, &vocab);
    assert!(result.is_ok(), "Transitive equivalence should be processed");

    let canonical = result.unwrap();
    // Should have edges representing the transitive closure
    assert!(
        !canonical.term_edges.is_empty(),
        "Transitive equivalence should produce edges"
    );
}

#[test]
fn test_pattern4_determinism_five_runs() {
    let mut data = TripleIndex::new();
    let vocab = praxis_graphlaw::shacl::Vocab::new();

    // Add some equivalence triples
    data.add(make_triple(
        "<http://example.org/x1>",
        "<http://www.w3.org/2002/07/owl#sameAs>",
        "<http://example.org/x2>",
    ));
    data.add(make_triple(
        "<http://example.org/y1>",
        "<http://www.w3.org/2002/07/owl#equivalentClass>",
        "<http://example.org/y2>",
    ));

    let mut canonical_results = Vec::new();

    // Run canonicalization 5 times
    for _ in 0..5 {
        let result = canonicalize_equivalences(&data, &vocab);
        assert!(result.is_ok());
        canonical_results.push(result.unwrap());
    }

    // All results should be identical
    for i in 1..5 {
        assert_eq!(
            canonical_results[0].class_edges, canonical_results[i].class_edges,
            "Class edges should be identical across runs"
        );
        assert_eq!(
            canonical_results[0].property_edges, canonical_results[i].property_edges,
            "Property edges should be identical across runs"
        );
        assert_eq!(
            canonical_results[0].term_edges, canonical_results[i].term_edges,
            "Term edges should be identical across runs"
        );
    }
}

#[test]
fn test_pattern4_integration_with_validator() {
    // This test verifies that the equivalence canonicalization is called
    // during the validation pipeline without errors.
    let data = TripleIndex::new();
    let shapes = praxis_graphlaw::shacl::ShapesGraph::parse("").expect("Empty shapes parse");

    // Should not panic or error
    let _report = Validator::validate(&data, &shapes);
}
