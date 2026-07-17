// Integration tests for the `oxrdf` adapter layer.
//
// These tests verify the conversion between `roxi`'s native `TripleIndex` / `VarOrTerm`
// and the corresponding `oxrdf` types (`oxrdf::Graph`, `oxrdf::Term`, etc.).
//
// NOTE: These tests are commented out because the adapter layer and extended term model
// (TICKET-001 and TICKET-007) are not yet implemented. Uncomment these tests and the imports
// once the adapter is implemented.

use oxrdf::{Literal, NamedNode, NamedOrBlankNode, NamedOrBlankNodeRef, Term, TermRef};
use praxis_graphlaw::oxrdf_adapter::{oxrdf_term_to_roxi_term, triple_index_to_oxrdf_graph};
use praxis_graphlaw::tripleindex::TripleIndex;
use praxis_graphlaw::triples::{Triple, VarOrTerm};

#[test]
fn test_triple_index_to_oxrdf_graph_roundtrip() {
    let mut index = TripleIndex::new();

    // Create native triples
    let s = VarOrTerm::convert("http://example.org/s".to_string());
    let p = VarOrTerm::convert("http://example.org/p".to_string());
    let o = VarOrTerm::convert("http://example.org/o".to_string());
    let triple = Triple {
        s: s.clone(),
        p: p.clone(),
        o: o.clone(),
        g: None,
    };

    index.add(triple.clone());

    // Convert to oxrdf::Graph
    let graph = triple_index_to_oxrdf_graph(&index);
    assert_eq!(graph.len(), 1);

    // Verify graph contains the converted triple
    let expected_s = NamedOrBlankNode::NamedNode(NamedNode::new("http://example.org/s").unwrap());
    let expected_p = NamedNode::new("http://example.org/p").unwrap();
    let expected_o = Term::NamedNode(NamedNode::new("http://example.org/o").unwrap());

    assert!(graph.contains(&oxrdf::Triple::new(expected_s, expected_p, expected_o)));
}

#[test]
fn test_literal_datatype_langtag_preserved_across_adapter() {
    let mut index = TripleIndex::new();

    // Construct triples with simple literal, typed literal, and language tagged literal
    // (assuming new constructors/variants from TICKET-001)
    let s = VarOrTerm::convert("http://example.org/s".to_string());
    let p_typed = VarOrTerm::convert("http://example.org/p1".to_string());
    let o_typed = VarOrTerm::new_literal(
        "42".to_string(),
        Some("http://www.w3.org/2001/XMLSchema#integer".to_string()),
        None,
    );

    let p_lang = VarOrTerm::convert("http://example.org/p2".to_string());
    let o_lang = VarOrTerm::new_literal("hello".to_string(), None, Some("en".to_string()));

    index.add(Triple {
        s: s.clone(),
        p: p_typed.clone(),
        o: o_typed.clone(),
        g: None,
    });
    index.add(Triple {
        s: s.clone(),
        p: p_lang.clone(),
        o: o_lang.clone(),
        g: None,
    });

    let graph = triple_index_to_oxrdf_graph(&index);
    assert_eq!(graph.len(), 2);

    // Verify typed literal translation
    let expected_p_typed = NamedNode::new("http://example.org/p1").unwrap();
    let expected_o_typed = Term::Literal(Literal::new_typed_literal(
        "42",
        NamedNode::new("http://www.w3.org/2001/XMLSchema#integer").unwrap(),
    ));
    assert!(graph.contains(&oxrdf::Triple::new(
        NamedOrBlankNode::NamedNode(NamedNode::new("http://example.org/s").unwrap()),
        expected_p_typed,
        expected_o_typed
    )));

    // Verify language-tagged literal translation
    let expected_p_lang = NamedNode::new("http://example.org/p2").unwrap();
    let expected_o_lang =
        Term::Literal(Literal::new_language_tagged_literal("hello", "en").unwrap());
    assert!(graph.contains(&oxrdf::Triple::new(
        NamedOrBlankNode::NamedNode(NamedNode::new("http://example.org/s").unwrap()),
        expected_p_lang,
        expected_o_lang
    )));
}

#[test]
fn test_blank_node_identity_preserved() {
    let mut index = TripleIndex::new();

    let s_bnode = VarOrTerm::new_blank_node("b0".to_string());
    let p = VarOrTerm::convert("http://example.org/p".to_string());
    let o_bnode = VarOrTerm::new_blank_node("b1".to_string());

    index.add(Triple {
        s: s_bnode.clone(),
        p: p.clone(),
        o: o_bnode.clone(),
        g: None,
    });

    let graph = triple_index_to_oxrdf_graph(&index);
    assert_eq!(graph.len(), 1);

    // Retrieve the triple from the graph and verify blank node identity/labels
    let oxrdf_triple = graph.iter().next().unwrap();

    if let NamedOrBlankNodeRef::BlankNode(ref sb) = oxrdf_triple.subject {
        assert_eq!(sb.as_str(), "b0");
    } else {
        panic!("Expected blank node subject");
    }

    if let TermRef::BlankNode(ref ob) = oxrdf_triple.object {
        assert_eq!(ob.as_str(), "b1");
    } else {
        panic!("Expected blank node object");
    }
}

#[test]
fn test_oxrdf_adapter_robustness() {
    use praxis_graphlaw::oxrdf_adapter::oxrdf_term_to_roxi_term;
    use praxis_graphlaw::triples::Term as RoxiTerm;

    // 1. Empty Literals
    // - Simple empty literal
    let ox_simple_empty = Term::Literal(Literal::new_simple_literal(""));
    let roxi_simple_empty = oxrdf_term_to_roxi_term(&ox_simple_empty);
    assert!(matches!(roxi_simple_empty, RoxiTerm::Literal(_)));
    // Roundtrip back to oxrdf
    let mut index = TripleIndex::new();
    let s = VarOrTerm::convert("http://example.org/s".to_string());
    let p = VarOrTerm::convert("http://example.org/p".to_string());
    let o = VarOrTerm::Term(roxi_simple_empty);
    index.add(Triple {
        s: s.clone(),
        p: p.clone(),
        o: o.clone(),
        g: None,
    });
    let graph = triple_index_to_oxrdf_graph(&index);
    assert_eq!(graph.len(), 1);
    let back_o = graph.iter().next().unwrap().object;
    assert_eq!(Term::from(back_o), ox_simple_empty);

    // - Language tagged empty literal
    let ox_lang_empty = Term::Literal(Literal::new_language_tagged_literal("", "en").unwrap());
    let roxi_lang_empty = oxrdf_term_to_roxi_term(&ox_lang_empty);
    let mut index = TripleIndex::new();
    index.add(Triple {
        s: s.clone(),
        p: p.clone(),
        o: VarOrTerm::Term(roxi_lang_empty),
        g: None,
    });
    let graph = triple_index_to_oxrdf_graph(&index);
    let back_o = graph.iter().next().unwrap().object;
    assert_eq!(Term::from(back_o), ox_lang_empty);

    // - Typed empty literal
    let ox_typed_empty = Term::Literal(Literal::new_typed_literal(
        "",
        NamedNode::new("http://example.org/dt").unwrap(),
    ));
    let roxi_typed_empty = oxrdf_term_to_roxi_term(&ox_typed_empty);
    let mut index = TripleIndex::new();
    index.add(Triple {
        s: s.clone(),
        p: p.clone(),
        o: VarOrTerm::Term(roxi_typed_empty),
        g: None,
    });
    let graph = triple_index_to_oxrdf_graph(&index);
    let back_o = graph.iter().next().unwrap().object;
    assert_eq!(Term::from(back_o), ox_typed_empty);

    // 2. Language tags (lowercase/uppercase/subtags)
    for tag in &["en", "EN", "en-US", "zh-Hant"] {
        let ox_lit = Term::Literal(Literal::new_language_tagged_literal("hello", *tag).unwrap());
        let roxi_lit = oxrdf_term_to_roxi_term(&ox_lit);
        let mut index = TripleIndex::new();
        index.add(Triple {
            s: s.clone(),
            p: p.clone(),
            o: VarOrTerm::Term(roxi_lit),
            g: None,
        });
        let graph = triple_index_to_oxrdf_graph(&index);
        let back_o = graph.iter().next().unwrap().object;
        assert_eq!(Term::from(back_o), ox_lit);
    }

    // 3. Custom Datatypes (with/without brackets, special chars)
    let custom_dts = &[
        "http://example.org/custom",
        "http://example.org/custom#type",
        "urn:uuid:f81d4fae-7dec-11d0-a765-00a0c91e6bf6",
    ];
    for dt in custom_dts {
        let ox_lit = Term::Literal(Literal::new_typed_literal(
            "val",
            NamedNode::new(*dt).unwrap(),
        ));
        let roxi_lit = oxrdf_term_to_roxi_term(&ox_lit);
        let mut index = TripleIndex::new();
        index.add(Triple {
            s: s.clone(),
            p: p.clone(),
            o: VarOrTerm::Term(roxi_lit),
            g: None,
        });
        let graph = triple_index_to_oxrdf_graph(&index);
        let back_o = graph.iter().next().unwrap().object;
        assert_eq!(Term::from(back_o), ox_lit);
    }

    // 4. Blank Node Prefixes and Roundtrips
    let b1 = VarOrTerm::new_blank_node("my-bnode-1".to_string());
    let b2 = VarOrTerm::new_blank_node("_:my-bnode-2".to_string());

    let mut index = TripleIndex::new();
    index.add(Triple {
        s: b1.clone(),
        p: p.clone(),
        o: b2.clone(),
        g: None,
    });
    let graph = triple_index_to_oxrdf_graph(&index);
    assert_eq!(graph.len(), 1);
    let ox_triple = graph.iter().next().unwrap();

    if let NamedOrBlankNodeRef::BlankNode(sb) = ox_triple.subject {
        assert_eq!(sb.as_str(), "my-bnode-1");
    } else {
        panic!("Expected BlankNode subject");
    }

    if let TermRef::BlankNode(ob) = ox_triple.object {
        assert_eq!(ob.as_str(), "_:my-bnode-2");
    } else {
        panic!("Expected BlankNode object");
    }
}

#[test]
fn test_literal_lang_roundtrip_equality() {
    use praxis_graphlaw::oxrdf_adapter::oxrdf_term_to_roxi_term;
    let original = VarOrTerm::new_literal("hello".to_string(), None, Some("en".to_string()));
    let original_term = match &original {
        VarOrTerm::Term(t) => t,
        _ => panic!("Expected term"),
    };

    let mut index = TripleIndex::new();
    let s = VarOrTerm::convert("http://example.org/s".to_string());
    let p = VarOrTerm::convert("http://example.org/p".to_string());
    index.add(Triple {
        s,
        p,
        o: original.clone(),
        g: None,
    });

    let graph = triple_index_to_oxrdf_graph(&index);
    let oxrdf_triple = graph.iter().next().unwrap();
    let ox_term = oxrdf_triple.object.into_owned();

    let roundtripped_term = oxrdf_term_to_roxi_term(&ox_term);
    assert_eq!(original_term, &roundtripped_term);
}

#[test]
fn test_simple_literal_roundtrip_equality() {
    let mut index = TripleIndex::new();
    let s = VarOrTerm::convert("http://example.org/s".to_string());
    let p = VarOrTerm::convert("http://example.org/p".to_string());
    let o = VarOrTerm::convert("\"hello\"".to_string());
    index.add(Triple {
        s: s.clone(),
        p: p.clone(),
        o: o.clone(),
        g: None,
    });

    let graph = triple_index_to_oxrdf_graph(&index);
    assert_eq!(graph.len(), 1);

    let oxrdf_triple = graph.iter().next().unwrap();
    let ox_term = oxrdf::Term::from(oxrdf_triple.object);
    let roundtripped_o = oxrdf_term_to_roxi_term(&ox_term);
    let original_o = match o {
        VarOrTerm::Term(t) => t,
        _ => panic!("Expected term"),
    };

    assert_eq!(roundtripped_o, original_o);
}

/// Round-trip identity for `xsd:decimal`, `xsd:boolean`, and `xsd:date`
/// literals specifically -- prior adapter coverage only exercised
/// `xsd:integer` and opaque custom datatype IRIs, leaving these three real,
/// commonly-used XSD datatypes unverified. A lossy adapter that silently
/// mangled a decimal's precision, a boolean's lexical form, or a date's
/// format on the way through `oxrdf::Graph` would be a subtle, hard-to-spot
/// data-corruption bug -- exactly the failure class this test targets.
#[test]
fn test_xsd_decimal_boolean_date_roundtrip_equality() {
    use praxis_graphlaw::oxrdf_adapter::oxrdf_term_to_roxi_term;

    let cases: &[(&str, &str)] = &[
        ("3.14159", "http://www.w3.org/2001/XMLSchema#decimal"),
        ("-0.001", "http://www.w3.org/2001/XMLSchema#decimal"),
        ("true", "http://www.w3.org/2001/XMLSchema#boolean"),
        ("false", "http://www.w3.org/2001/XMLSchema#boolean"),
        ("2026-07-05", "http://www.w3.org/2001/XMLSchema#date"),
    ];

    for (lexical, datatype) in cases {
        // Datatype IRIs passed to `new_literal` follow this codebase's
        // established bracketed-IRI convention (see e.g.
        // `queryengine.rs`'s `intern_string`/numeric-literal construction),
        // matching what `oxrdf_term_to_roxi_term` itself produces.
        let bracketed_datatype = format!("<{}>", datatype);
        let original = VarOrTerm::new_literal(lexical.to_string(), Some(bracketed_datatype), None);
        let original_term = match &original {
            VarOrTerm::Term(t) => t.clone(),
            _ => panic!("Expected term"),
        };

        let mut index = TripleIndex::new();
        let s = VarOrTerm::convert("http://example.org/s".to_string());
        let p = VarOrTerm::convert("http://example.org/p".to_string());
        index.add(Triple {
            s,
            p,
            o: original.clone(),
            g: None,
        });

        let graph = triple_index_to_oxrdf_graph(&index);
        assert_eq!(
            graph.len(),
            1,
            "expected exactly one triple for {} ({})",
            lexical,
            datatype
        );

        let oxrdf_triple = graph.iter().next().unwrap();
        let ox_term = Term::from(oxrdf_triple.object);

        // The oxrdf side must carry the datatype through untouched.
        if let Term::Literal(lit) = &ox_term {
            assert_eq!(
                lit.value(),
                *lexical,
                "lexical form must survive the roxi->oxrdf conversion for {}",
                datatype
            );
            assert_eq!(
                lit.datatype().as_str(),
                *datatype,
                "datatype IRI must survive the roxi->oxrdf conversion for {}",
                lexical
            );
        } else {
            panic!(
                "expected a Literal term for {} ({}), got {:?}",
                lexical, datatype, ox_term
            );
        }

        // Full round-trip: oxrdf -> roxi must reproduce the original term exactly.
        let roundtripped_term = oxrdf_term_to_roxi_term(&ox_term);
        assert_eq!(
            original_term, roundtripped_term,
            "round-trip (roxi->oxrdf->roxi) must preserve {} ({}) exactly",
            lexical, datatype
        );
    }
}
