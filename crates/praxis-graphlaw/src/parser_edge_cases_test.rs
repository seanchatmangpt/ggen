#![cfg(test)]

use crate::TripleStore;

mod turtle_edge_cases {
    use super::*;

    /// Blank nodes: various label formats
    #[test]
    fn test_blank_node_labels() {
        let ttl = r#"
@prefix ex: <http://example.org/> .

_:b1 ex:prop "simple" .
_:b_underscore ex:prop "underscore" .
_:b-hyphen ex:prop "hyphen" .
_:b.dot ex:prop "dot" .
_:b123 ex:prop "numeric" .
_:BCapital ex:prop "capital" .
        "#;

        let store = TripleStore::from(ttl);
        assert!(store.len() >= 5, "Should parse all blank node variations");
    }

    /// Typed literals with various XSD types
    #[test]
    fn test_typed_literals_xsd() {
        let ttl = r#"
@prefix ex: <http://example.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:int "42"^^xsd:integer .
ex:dec "3.14"^^xsd:decimal .
ex:float "2.71"^^xsd:float .
ex:double "1.618"^^xsd:double .
ex:bool "true"^^xsd:boolean .
ex:string "text"^^xsd:string .
ex:date "2026-07-08"^^xsd:date .
ex:time "12:34:56"^^xsd:time .
ex:datetime "2026-07-08T12:34:56Z"^^xsd:dateTime .
ex:hex "DEADBEEF"^^xsd:hexBinary .
ex:custom "value"^^<http://example.org/customType> .
        "#;

        let store = TripleStore::from(ttl);
        assert!(store.len() >= 11, "Should parse all XSD type variations");
    }

    /// Language-tagged literals
    #[test]
    fn test_language_tagged_literals() {
        let ttl = r#"
@prefix ex: <http://example.org/> .

ex:title "Hello"@en .
ex:title "Hola"@es .
ex:title "Bonjour"@fr .
ex:title "こんにちは"@ja .
ex:title "Hello"@en-US .
ex:title "Hello"@en-GB .
ex:title "Café"@fr-CA .
        "#;

        let store = TripleStore::from(ttl);
        assert!(store.len() >= 7, "Should parse language tags");
    }

    /// RDF lists/collections
    #[test]
    fn test_rdf_collections() {
        let ttl = r#"
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:list1 rdf:value () .
ex:list2 rdf:value (1) .
ex:list3 rdf:value (1 2 3) .
ex:list4 rdf:value ("a" "b" "c") .
ex:list5 rdf:value (ex:a ex:b ex:c) .
        "#;

        let store = TripleStore::from(ttl);
        assert!(
            store.len() > 0,
            "collection statements must produce triples, got empty store"
        );
    }

    /// Nested collections
    #[test]
    fn test_nested_collections() {
        let ttl = r#"
@prefix ex: <http://example.org/> .

ex:nested1 ex:value ((1 2) (3 4)) .
ex:nested2 ex:value (ex:a (ex:b ex:c) ex:d) .
ex:nested3 ex:value ((("x"))) .
        "#;

        let store = TripleStore::from(ttl);
        assert!(
            store.len() > 0,
            "nested collection statements must produce triples, got empty store"
        );
    }

    /// Blank node property lists
    #[test]
    fn test_blank_node_property_lists() {
        let ttl = r#"
@prefix ex: <http://example.org/> .

ex:person [ ex:name "Alice" ; ex:age 30 ] .
ex:company [ ex:name "Acme" ; ex:employees [ ex:name "Bob" ] ] .
[ ex:orphan "no parent" ] .
        "#;

        let store = TripleStore::from(ttl);
        assert!(
            store.len() >= 3,
            "each blank-node property list must produce at least its own triples, got {}",
            store.len()
        );
    }

    /// Multiple objects (comma operator)
    #[test]
    fn test_multiple_objects() {
        let ttl = r#"
@prefix ex: <http://example.org/> .

ex:alice ex:knows ex:bob, ex:charlie, ex:diana .
ex:person ex:name "Alice", "Alicia"@es, "Arisya"@ja .
        "#;

        let store = TripleStore::from(ttl);
        // Should expand to multiple triples
        assert!(store.len() >= 5, "Should expand comma-separated objects");
    }

    /// Multiple predicates (semicolon operator)
    #[test]
    fn test_multiple_predicates() {
        let ttl = r#"
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:alice foaf:name "Alice" ;
         foaf:age 30 ;
         foaf:knows ex:bob ;
         ex:email "alice@example.org" .
        "#;

        let store = TripleStore::from(ttl);
        assert!(
            store.len() >= 4,
            "Should expand semicolon-separated predicates"
        );
    }

    /// Mixed comma and semicolon
    #[test]
    fn test_mixed_comma_semicolon() {
        let ttl = r#"
@prefix ex: <http://example.org/> .

ex:alice ex:knows ex:bob, ex:charlie ;
         ex:likes "music", "coding" ;
         ex:lives ex:chicago .
        "#;

        let store = TripleStore::from(ttl);
        // 2 + 2 + 1 objects across the mixed comma/semicolon statement.
        assert!(
            store.len() >= 5,
            "mixed comma/semicolon should expand to >= 5 triples, got {}",
            store.len()
        );
    }

    /// Bare "a" (rdf:type shorthand)
    #[test]
    fn test_rdf_type_shorthand() {
        let ttl = r#"
@prefix ex: <http://example.org/> .

ex:alice a ex:Person .
ex:bob a ex:Person, ex:Developer .
[ a ex:Organization ] .
        "#;

        let store = TripleStore::from(ttl);
        assert!(store.len() >= 3, "Should expand 'a' to rdf:type");
    }

    /// String literals (various quote styles)
    #[test]
    fn test_string_literal_styles() {
        let ttl = r#"
@prefix ex: <http://example.org/> .

ex:single 'single quoted' .
ex:double "double quoted" .
ex:triple '''triple single''' .
ex:triple2 """triple double""" .
ex:with_newline """line 1
line 2""" .
ex:with_escape "escaped \"quotes\"" .
ex:with_backslash "path\\to\\file" .
        "#;

        let store = TripleStore::from(ttl);
        assert!(store.len() >= 7, "Should handle all string literal styles");
    }

    /// Prefixed names and abbreviation
    #[test]
    fn test_prefixed_names() {
        let ttl = r#"
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix : <http://default.org/> .

ex:alice foaf:knows :bob .
:charlie ex:knows foaf:Person .
        "#;

        let store = TripleStore::from(ttl);
        assert!(store.len() >= 2, "Should expand prefixed names");
    }

    /// Empty prefix (default namespace)
    #[test]
    fn test_default_namespace() {
        let ttl = r#"
@prefix : <http://example.org/> .

:alice :knows :bob ;
       :age 30 .
        "#;

        let store = TripleStore::from(ttl);
        assert!(store.len() >= 2, "Should handle empty prefix");
    }

    /// Numeric literals
    #[test]
    fn test_numeric_literals() {
        let ttl = r#"
@prefix ex: <http://example.org/> .

ex:int 42 .
ex:negative -99 .
ex:decimal 3.14 .
ex:scientific 1.23e10 .
ex:scientific_neg 5.5e-3 .
ex:float 2.71f .
ex:double 1.618d .
        "#;

        let store = TripleStore::from(ttl);
        assert!(store.len() >= 7, "Should parse numeric literals");
    }

    /// Boolean literals
    #[test]
    fn test_boolean_literals() {
        let ttl = r#"
@prefix ex: <http://example.org/> .

ex:flag1 true .
ex:flag2 false .
ex:explicitly_true "true"^^<http://www.w3.org/2001/XMLSchema#boolean> .
        "#;

        let store = TripleStore::from(ttl);
        assert!(store.len() >= 3, "Should parse boolean literals");
    }

    /// Inverse predicates (is/of)
    #[test]
    fn test_inverse_predicates() {
        let ttl = r#"
@prefix ex: <http://example.org/> .

ex:alice ex:knows ex:bob .
ex:bob is ex:knows of ex:alice .
ex:charlie is ex:parent of ex:diana .
        "#;

        let store = TripleStore::from(ttl);
        // Should have equivalent triples from forward and inverse forms
        assert!(store.len() >= 3, "Should handle inverse predicates");
    }

    /// Comments
    #[test]
    fn test_comments() {
        let ttl = r#"
@prefix ex: <http://example.org/> .

# This is a comment
ex:alice ex:knows ex:bob . # end-of-line comment

# Another comment before triple
ex:charlie a ex:Person .
        "#;

        let store = TripleStore::from(ttl);
        assert!(store.len() >= 2, "Should ignore comments");
    }

    /// @base directive
    #[test]
    fn test_base_directive() {
        let ttl = r#"
@base <http://example.org/> .
@prefix ex: <http://example.org/> .

<alice> ex:knows <bob> .
<#fragment> ex:prop "value" .
        "#;

        let store = TripleStore::from(ttl);
        assert!(store.len() >= 2, "Should handle @base directive");
    }

    /// SPARQL-style PREFIX
    #[test]
    fn test_sparql_prefix() {
        let ttl = r#"
PREFIX ex: <http://example.org/>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>

ex:alice foaf:knows ex:bob .
        "#;

        let store = TripleStore::from(ttl);
        assert!(store.len() >= 1, "Should accept SPARQL-style PREFIX");
    }

    /// SPARQL-style BASE
    #[test]
    fn test_sparql_base() {
        let ttl = r#"
BASE <http://example.org/>
PREFIX ex: <http://example.org/>

<alice> ex:knows <bob> .
        "#;

        let store = TripleStore::from(ttl);
        assert!(store.len() >= 1, "Should accept SPARQL-style BASE");
    }

    /// Path expressions (forward)
    #[test]
    fn test_path_forward() {
        let ttl = r#"
@prefix ex: <http://example.org/> .

ex:company !ex:employee ex:alice .
ex:alice !ex:department !ex:manager ex:bob .
        "#;

        let store = TripleStore::from(ttl);
        // Paths desugar to intermediate blank nodes
        assert!(store.len() > 1, "Should desugar forward path expressions");
    }

    /// Path expressions (inverse)
    #[test]
    fn test_path_inverse() {
        let ttl = r#"
@prefix ex: <http://example.org/> .

ex:alice ^ex:knows ex:bob .
ex:bob ^ex:manages ^ex:works_at ex:company .
        "#;

        let store = TripleStore::from(ttl);
        assert!(store.len() > 1, "Should desugar inverse path expressions");
    }

    /// Complex real-world scenario
    #[test]
    fn test_complex_scenario() {
        let ttl = r#"
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:alice a foaf:Person ;
    foaf:name "Alice Johnson"@en, "Alice Johnson"@es ;
    foaf:age 30 ;
    foaf:knows ex:bob, ex:charlie ;
    foaf:mbox <mailto:alice@example.org> ;
    ex:preferences [
        rdf:value ("music" "coding" "reading") ;
        ex:updated "2026-07-08"^^xsd:date
    ] ;
    ex:metadata [
        ex:created "2020-01-15T10:30:00Z"^^xsd:dateTime ;
        ex:version "1.0.0"^^xsd:string ;
        ex:active true
    ] .

ex:bob foaf:knows ex:alice, ex:diana ;
       foaf:name "Bob Smith" .

ex:charlie a foaf:Person ;
    foaf:givenName "Charlie" ;
    foaf:familyName "Brown" .
        "#;

        let store = TripleStore::from(ttl);
        assert!(store.len() > 10, "Should parse complex real-world ontology");
    }

    /// Empty document
    #[test]
    fn test_empty_document() {
        let ttl = "";
        let store = TripleStore::from(ttl);
        assert_eq!(
            store.len(),
            0,
            "Empty document should result in empty store"
        );
    }

    /// Only comments
    #[test]
    fn test_only_comments() {
        let ttl = r#"
# Comment 1
# Comment 2
# Comment 3
        "#;

        let store = TripleStore::from(ttl);
        assert_eq!(
            store.len(),
            0,
            "Document with only comments should be empty"
        );
    }

    /// Only directives
    #[test]
    fn test_only_directives() {
        let ttl = r#"
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@base <http://example.org/> .
        "#;

        let store = TripleStore::from(ttl);
        assert_eq!(
            store.len(),
            0,
            "Document with only directives should be empty"
        );
    }

    /// Mixed blank nodes and IRIs
    #[test]
    fn test_mixed_blank_and_iri() {
        let ttl = r#"
@prefix ex: <http://example.org/> .

ex:named [ ex:anon "value1" ] ;
         ex:other _:orphan .

_:orphan ex:prop "orphan_value" .

[ ex:deeply [ ex:nested [ ex:structure "yes" ] ] ] .
        "#;

        let store = TripleStore::from(ttl);
        assert!(store.len() > 3, "Should mix named and blank nodes");
    }

    /// Unicode in various positions
    #[test]
    fn test_unicode_support() {
        let ttl = r#"
@prefix ex: <http://example.org/> .

ex:unicode "English and 中文 and العربية and Ελληνικά" .
ex:name "José García"@es .
ex:emoji "Hello 🚀 World" .
        "#;

        let store = TripleStore::from(ttl);
        assert!(store.len() >= 3, "Should handle Unicode characters");
    }
}
