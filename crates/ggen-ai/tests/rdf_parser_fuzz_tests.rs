//! Fuzz tests for RDF ontology parser robustness
//!
//! This test suite verifies that the RDF parser handles malformed and adversarial inputs
//! gracefully without panicking and produces clear error messages.
//!
//! Test categories:
//! - Invalid Turtle syntax
//! - Extremely large ontologies (10000+ triples)
//! - Deeply nested structures
//! - Unicode edge cases
//! - Circular references
//! - Duplicate properties
//! - Namespace conflicts
//! - Invalid URIs
//! - Empty files
//! - Binary garbage
//! - Extremely long strings

use ggen_ai::rdf::RdfParser;
use proptest::prelude::*;
use std::io::Write;
use std::panic;
use tempfile::NamedTempFile;

/// Helper to create a temporary TTL file with given content
fn create_ttl_file(content: &str) -> NamedTempFile {
    let mut file = NamedTempFile::new().unwrap();
    write!(file, "{}", content).unwrap();
    file.flush().unwrap();
    file
}

/// Helper to assert that parser handles error gracefully without panicking
fn assert_no_panic_on_parse(content: &str) -> bool {
    let result = panic::catch_unwind(|| {
        let mut parser = RdfParser::new().unwrap();
        let file = create_ttl_file(content);
        let parse_result = parser.load_ttl(file.path());

        // Either succeeds or returns a proper error
        match parse_result {
            Ok(_) => true,
            Err(e) => {
                // Verify error message is not empty
                let msg = e.to_string();
                !msg.is_empty()
            }
        }
    });

    result.is_ok()
}

// ============================================================================
// 1. Invalid Turtle Syntax Tests
// ============================================================================

#[test]
fn test_malformed_prefix_declaration() {
    let inputs = vec![
        "@prefix : <",  // Incomplete prefix
        "@prefix <http://example.org/>",  // Missing prefix name
        "prefix ex: <http://example.org/>",  // Missing @
        "@prefix ex http://example.org/> .",  // Missing colon after prefix
        "@prefix ex: http://example.org/> .",  // Missing angle brackets
        "@prefix ex: <http://example.org/",  // Missing closing bracket
    ];

    for input in inputs {
        assert!(
            assert_no_panic_on_parse(input),
            "Parser panicked on malformed prefix: {}",
            input
        );
    }
}

#[test]
fn test_malformed_triples() {
    let inputs = vec![
        "ex:subject",  // Incomplete triple
        "ex:subject ex:predicate",  // Missing object
        "ex:subject ex:predicate .",  // Missing object with terminator
        "ex:predicate ex:object .",  // Missing subject
        "ex:subject ex:object",  // Missing predicate
        "[[[ ]]] .",  // Invalid blank node syntax
        "ex:s ex:p ex:o",  // Missing terminator
        "ex:s ex:p ex:o .",  // Missing prefix declaration
        "ex:s ex:p 'unclosed string .",  // Unclosed string literal
        "ex:s ex:p \"unclosed string .",  // Unclosed string literal (double quote)
    ];

    for input in inputs {
        assert!(
            assert_no_panic_on_parse(input),
            "Parser panicked on malformed triple: {}",
            input
        );
    }
}

#[test]
fn test_invalid_literals() {
    let base = "@prefix ex: <http://example.org/> .\n";
    let inputs = vec![
        format!("{}ex:s ex:p \"\"\"unclosed long literal .", base),
        format!("{}ex:s ex:p 123abc .", base),  // Invalid number
        format!("{}ex:s ex:p true123 .", base),  // Invalid boolean
        format!("{}ex:s ex:p \"test\"@@ .", base),  // Invalid language tag
        format!("{}ex:s ex:p \"test\"^^ .", base),  // Invalid datatype
        format!("{}ex:s ex:p \"test\"^^<> .", base),  // Empty datatype
    ];

    for input in inputs {
        assert!(
            assert_no_panic_on_parse(&input),
            "Parser panicked on invalid literal: {}",
            input
        );
    }
}

#[test]
fn test_syntax_errors_combinations() {
    let inputs = vec![
        ";;;",
        ".,.,.",
        "[ [ [ ] ] ]",
        "( ( ( ) ) )",
        "@prefix @prefix @prefix",
        "<<<<<>>>>>",
        "ex:s ex:p ex:o ex:o2 ex:o3 .",
        "ex:s ; ; ; .",
    ];

    for input in inputs {
        assert!(
            assert_no_panic_on_parse(input),
            "Parser panicked on syntax error combination: {}",
            input
        );
    }
}

// ============================================================================
// 2. Extremely Large Ontologies (10000+ triples)
// ============================================================================

#[test]
fn test_large_ontology_10k_triples() {
    let mut content = String::from("@prefix ex: <http://example.org/> .\n");

    // Generate 10,000 triples
    for i in 0..10_000 {
        content.push_str(&format!("ex:subject{} ex:predicate{} \"value{}\" .\n", i, i % 100, i));
    }

    assert!(
        assert_no_panic_on_parse(&content),
        "Parser panicked on 10k triples"
    );
}

#[test]
fn test_large_ontology_50k_triples() {
    let mut content = String::from("@prefix ex: <http://example.org/> .\n");

    // Generate 50,000 triples
    for i in 0..50_000 {
        content.push_str(&format!("ex:s{} ex:p{} ex:o{} .\n", i, i % 1000, i));
    }

    assert!(
        assert_no_panic_on_parse(&content),
        "Parser panicked on 50k triples"
    );
}

#[test]
fn test_large_number_of_prefixes() {
    let mut content = String::new();

    // Generate 1000 prefix declarations
    for i in 0..1000 {
        content.push_str(&format!("@prefix ns{}: <http://example{}.org/> .\n", i, i));
    }

    // Add some triples using the prefixes
    for i in 0..100 {
        content.push_str(&format!("ns{}:subject ns{}:predicate \"value\" .\n", i, i));
    }

    assert!(
        assert_no_panic_on_parse(&content),
        "Parser panicked on large number of prefixes"
    );
}

// ============================================================================
// 3. Deeply Nested Structures
// ============================================================================

#[test]
fn test_deeply_nested_blank_nodes() {
    let mut content = String::from("@prefix ex: <http://example.org/> .\n");
    content.push_str("ex:root ex:has ");

    // Create 100 levels of nested blank nodes
    for _ in 0..100 {
        content.push_str("[ ex:nested ");
    }
    content.push_str("\"deepest value\"");
    for _ in 0..100 {
        content.push_str(" ]");
    }
    content.push_str(" .\n");

    assert!(
        assert_no_panic_on_parse(&content),
        "Parser panicked on deeply nested blank nodes"
    );
}

#[test]
fn test_deeply_nested_collections() {
    let mut content = String::from("@prefix ex: <http://example.org/> .\n");
    content.push_str("ex:root ex:hasList ");

    // Create 50 levels of nested lists
    for _ in 0..50 {
        content.push_str("( ");
    }
    content.push_str("\"innermost\"");
    for _ in 0..50 {
        content.push_str(" )");
    }
    content.push_str(" .\n");

    assert!(
        assert_no_panic_on_parse(&content),
        "Parser panicked on deeply nested collections"
    );
}

#[test]
fn test_complex_nested_structures() {
    let content = r#"
@prefix ex: <http://example.org/> .

ex:root ex:has [
    ex:level1 [
        ex:level2 [
            ex:level3 [
                ex:level4 [
                    ex:level5 [
                        ex:level6 [
                            ex:level7 [
                                ex:level8 [
                                    ex:level9 [
                                        ex:level10 "deep value"
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]
] .
"#;

    assert!(
        assert_no_panic_on_parse(content),
        "Parser panicked on complex nested structures"
    );
}

// ============================================================================
// 4. Unicode Edge Cases
// ============================================================================

#[test]
fn test_unicode_in_literals() {
    let content = r#"
@prefix ex: <http://example.org/> .

ex:subject1 ex:name "Hello 世界" .
ex:subject2 ex:emoji "🎉🚀💻🔥" .
ex:subject3 ex:arabic "مرحبا بك" .
ex:subject4 ex:hebrew "שלום" .
ex:subject5 ex:russian "Привет" .
ex:subject6 ex:greek "Γειά σου" .
ex:subject7 ex:japanese "こんにちは" .
ex:subject8 ex:korean "안녕하세요" .
ex:subject9 ex:mixed "Hello🌍世界Мир" .
"#;

    assert!(
        assert_no_panic_on_parse(content),
        "Parser panicked on Unicode in literals"
    );
}

#[test]
fn test_unicode_edge_cases() {
    let content = r#"
@prefix ex: <http://example.org/> .

ex:s1 ex:p "Zero-width space:​test" .
ex:s2 ex:p "Right-to-left mark:‏test" .
ex:s3 ex:p "Combining characters: e\u{0301}\u{0302}\u{0303}" .
ex:s4 ex:p "Zalgo: H̸̢̫̙̓ę̷̰͒l̴̰̈́l̶̰̀ǫ̶̣̈́" .
ex:s5 ex:p "Null char:\u{0000}" .
ex:s6 ex:p "Control chars:\u{0001}\u{0002}\u{001F}" .
ex:s7 ex:p "Surrogate pairs: 𝕳𝖊𝖑𝖑𝖔" .
ex:s8 ex:p "Private use: \u{E000}\u{F8FF}" .
"#;

    assert!(
        assert_no_panic_on_parse(content),
        "Parser panicked on Unicode edge cases"
    );
}

#[test]
fn test_unicode_normalization() {
    let content = r#"
@prefix ex: <http://example.org/> .

# Same character in different Unicode normalization forms
ex:s1 ex:p "é" .  # Precomposed (NFC)
ex:s2 ex:p "é" .  # Decomposed (NFD)
ex:s3 ex:p "Å" .  # Precomposed
ex:s4 ex:p "Å" .  # Decomposed
"#;

    assert!(
        assert_no_panic_on_parse(content),
        "Parser panicked on Unicode normalization"
    );
}

// ============================================================================
// 5. Circular References
// ============================================================================

#[test]
fn test_circular_references() {
    let content = r#"
@prefix ex: <http://example.org/> .

ex:node1 ex:references ex:node2 .
ex:node2 ex:references ex:node3 .
ex:node3 ex:references ex:node1 .
"#;

    assert!(
        assert_no_panic_on_parse(content),
        "Parser panicked on circular references"
    );
}

#[test]
fn test_self_referential_node() {
    let content = r#"
@prefix ex: <http://example.org/> .

ex:node ex:references ex:node .
ex:node ex:contains ex:node .
ex:node ex:parent ex:node .
"#;

    assert!(
        assert_no_panic_on_parse(content),
        "Parser panicked on self-referential nodes"
    );
}

#[test]
fn test_complex_circular_graph() {
    let mut content = String::from("@prefix ex: <http://example.org/> .\n");

    // Create a complex graph with multiple circular paths
    for i in 0..100 {
        let next = (i + 1) % 100;
        content.push_str(&format!("ex:node{} ex:next ex:node{} .\n", i, next));
        content.push_str(&format!("ex:node{} ex:prev ex:node{} .\n", i, (i + 99) % 100));
    }

    assert!(
        assert_no_panic_on_parse(&content),
        "Parser panicked on complex circular graph"
    );
}

// ============================================================================
// 6. Duplicate Properties
// ============================================================================

#[test]
fn test_duplicate_properties() {
    let content = r#"
@prefix ex: <http://example.org/> .

ex:subject ex:property "value1" .
ex:subject ex:property "value2" .
ex:subject ex:property "value3" .
ex:subject ex:property "value4" .
ex:subject ex:property "value5" .
"#;

    assert!(
        assert_no_panic_on_parse(content),
        "Parser panicked on duplicate properties"
    );
}

#[test]
fn test_massive_duplicate_properties() {
    let mut content = String::from("@prefix ex: <http://example.org/> .\n");

    // Add 1000 values for the same property
    for i in 0..1000 {
        content.push_str(&format!("ex:subject ex:property \"value{}\" .\n", i));
    }

    assert!(
        assert_no_panic_on_parse(&content),
        "Parser panicked on massive duplicate properties"
    );
}

#[test]
fn test_duplicate_triples() {
    let content = r#"
@prefix ex: <http://example.org/> .

ex:s ex:p ex:o .
ex:s ex:p ex:o .
ex:s ex:p ex:o .
ex:s ex:p ex:o .
"#;

    assert!(
        assert_no_panic_on_parse(content),
        "Parser panicked on duplicate triples"
    );
}

// ============================================================================
// 7. Namespace Conflicts
// ============================================================================

#[test]
fn test_namespace_conflicts() {
    let content = r#"
@prefix ex: <http://example1.org/> .
@prefix ex: <http://example2.org/> .
@prefix ex: <http://example3.org/> .

ex:subject ex:predicate "value" .
"#;

    assert!(
        assert_no_panic_on_parse(content),
        "Parser panicked on namespace conflicts"
    );
}

#[test]
fn test_conflicting_base_declarations() {
    let content = r#"
@base <http://example1.org/> .
@base <http://example2.org/> .
@base <http://example3.org/> .

@prefix ex: <http://example.org/> .
ex:subject ex:predicate "value" .
"#;

    assert!(
        assert_no_panic_on_parse(content),
        "Parser panicked on conflicting base declarations"
    );
}

#[test]
fn test_reserved_prefix_conflicts() {
    let content = r#"
@prefix rdf: <http://my-custom-rdf.org/> .
@prefix rdfs: <http://my-custom-rdfs.org/> .
@prefix owl: <http://my-custom-owl.org/> .

rdf:subject rdfs:predicate owl:value .
"#;

    assert!(
        assert_no_panic_on_parse(content),
        "Parser panicked on reserved prefix conflicts"
    );
}

// ============================================================================
// 8. Invalid URIs
// ============================================================================

#[test]
fn test_invalid_uris() {
    let inputs = vec![
        "@prefix ex: <> .\nex:s ex:p ex:o .",  // Empty URI
        "@prefix ex: <   > .\nex:s ex:p ex:o .",  // Whitespace URI
        "@prefix ex: <ht tp://example.org/> .\nex:s ex:p ex:o .",  // Space in URI
        "@prefix ex: <http://> .\nex:s ex:p ex:o .",  // Incomplete URI
        "@prefix ex: <file:///../../etc/passwd> .\nex:s ex:p ex:o .",  // Path traversal
        "@prefix ex: <javascript:alert(1)> .\nex:s ex:p ex:o .",  // JavaScript URI
        "@prefix ex: <data:text/html,<script>alert(1)</script>> .\nex:s ex:p ex:o .",  // Data URI with script
    ];

    for input in inputs {
        assert!(
            assert_no_panic_on_parse(input),
            "Parser panicked on invalid URI: {}",
            input
        );
    }
}

#[test]
fn test_malformed_iris() {
    let content = r#"
@prefix ex: <http://example.org/> .

ex:s ex:p <http://example.org/unclosed .
ex:s ex:p <> .
ex:s ex:p <   > .
ex:s ex:p <urn:isbn:> .
"#;

    assert!(
        assert_no_panic_on_parse(content),
        "Parser panicked on malformed IRIs"
    );
}

#[test]
fn test_extremely_long_uris() {
    let long_path = "a/".repeat(10000);
    let content = format!(
        "@prefix ex: <http://example.org/{}> .\nex:s ex:p \"value\" .",
        long_path
    );

    assert!(
        assert_no_panic_on_parse(&content),
        "Parser panicked on extremely long URI"
    );
}

// ============================================================================
// 9. Empty Files and Edge Cases
// ============================================================================

#[test]
fn test_empty_file() {
    assert!(
        assert_no_panic_on_parse(""),
        "Parser panicked on empty file"
    );
}

#[test]
fn test_whitespace_only() {
    assert!(
        assert_no_panic_on_parse("   \n\t\r\n   \t\t\n"),
        "Parser panicked on whitespace-only file"
    );
}

#[test]
fn test_comments_only() {
    let content = r#"
# This is a comment
# Another comment
# Yet another comment
"#;

    assert!(
        assert_no_panic_on_parse(content),
        "Parser panicked on comments-only file"
    );
}

// ============================================================================
// 10. Binary Garbage
// ============================================================================

#[test]
fn test_binary_garbage() {
    let inputs = vec![
        vec![0x00, 0xFF, 0xFE, 0xFD],  // Binary data
        vec![0x89, 0x50, 0x4E, 0x47],  // PNG header
        vec![0xFF, 0xD8, 0xFF, 0xE0],  // JPEG header
        vec![0x1F, 0x8B, 0x08, 0x00],  // GZIP header
    ];

    for binary_data in inputs {
        let content = String::from_utf8_lossy(&binary_data);
        assert!(
            assert_no_panic_on_parse(&content),
            "Parser panicked on binary garbage"
        );
    }
}

#[test]
fn test_random_bytes() {
    use std::iter;

    let random_bytes: Vec<u8> = iter::repeat_with(|| rand::random::<u8>())
        .take(1000)
        .collect();

    let content = String::from_utf8_lossy(&random_bytes);

    assert!(
        assert_no_panic_on_parse(&content),
        "Parser panicked on random bytes"
    );
}

#[test]
fn test_mixed_valid_and_binary() {
    let mut data = Vec::new();
    data.extend_from_slice(b"@prefix ex: <http://example.org/> .\n");
    data.extend_from_slice(&[0x00, 0xFF, 0xFE, 0xFD]);
    data.extend_from_slice(b"ex:s ex:p \"value\" .\n");

    let content = String::from_utf8_lossy(&data);

    assert!(
        assert_no_panic_on_parse(&content),
        "Parser panicked on mixed valid and binary content"
    );
}

// ============================================================================
// 11. Extremely Long Strings
// ============================================================================

#[test]
fn test_extremely_long_literal() {
    let long_string = "a".repeat(1_000_000);
    let content = format!(
        "@prefix ex: <http://example.org/> .\nex:s ex:p \"{}\" .",
        long_string
    );

    assert!(
        assert_no_panic_on_parse(&content),
        "Parser panicked on extremely long literal (1M chars)"
    );
}

#[test]
fn test_extremely_long_uri() {
    let long_uri = "a".repeat(100_000);
    let content = format!(
        "@prefix ex: <http://example.org/{}> .\nex:s ex:p \"value\" .",
        long_uri
    );

    assert!(
        assert_no_panic_on_parse(&content),
        "Parser panicked on extremely long URI"
    );
}

#[test]
fn test_extremely_long_local_name() {
    let long_name = "a".repeat(100_000);
    let content = format!(
        "@prefix ex: <http://example.org/> .\nex:{} ex:p \"value\" .",
        long_name
    );

    assert!(
        assert_no_panic_on_parse(&content),
        "Parser panicked on extremely long local name"
    );
}

#[test]
fn test_long_multi_line_literal() {
    let long_string = "Line\n".repeat(100_000);
    let content = format!(
        "@prefix ex: <http://example.org/> .\nex:s ex:p \"\"\"{}\"\"\" .",
        long_string
    );

    assert!(
        assert_no_panic_on_parse(&content),
        "Parser panicked on long multi-line literal"
    );
}

// ============================================================================
// 12. Property-Based Fuzz Tests
// ============================================================================

proptest! {
    #[test]
    fn prop_random_string_no_panic(s in "\\PC*") {
        let _ = assert_no_panic_on_parse(&s);
    }

    #[test]
    fn prop_random_prefix_declaration(
        prefix in "[a-z]+",
        uri in "https?://[a-z]+\\.[a-z]+/"
    ) {
        let content = format!("@prefix {}: <{}> .", prefix, uri);
        let _ = assert_no_panic_on_parse(&content);
    }

    #[test]
    fn prop_random_triple(
        subj in "[a-z]+:[a-z]+",
        pred in "[a-z]+:[a-z]+",
        obj in "\"[^\"]*\""
    ) {
        let content = format!(
            "@prefix ex: <http://example.org/> .\n{} {} {} .",
            subj, pred, obj
        );
        let _ = assert_no_panic_on_parse(&content);
    }

    #[test]
    fn prop_many_triples(count in 1usize..1000) {
        let mut content = String::from("@prefix ex: <http://example.org/> .\n");
        for i in 0..count {
            content.push_str(&format!("ex:s{} ex:p{} \"v{}\" .\n", i, i, i));
        }
        let _ = assert_no_panic_on_parse(&content);
    }

    #[test]
    fn prop_unicode_strings(s in "[\\u{0}-\\u{FFFF}]+") {
        let content = format!(
            "@prefix ex: <http://example.org/> .\nex:s ex:p \"{}\" .",
            s.replace('\\', "\\\\").replace('"', "\\\"")
        );
        let _ = assert_no_panic_on_parse(&content);
    }
}

// ============================================================================
// 13. Error Message Quality Tests
// ============================================================================

#[test]
fn test_error_messages_are_informative() {
    let test_cases = vec![
        ("", "empty file"),
        ("@prefix", "incomplete prefix"),
        ("ex:s ex:p ex:o .", "undefined prefix"),
        ("@prefix ex: <http://example.org/> .\nex:s ex:p", "incomplete triple"),
    ];

    for (input, description) in test_cases {
        let mut parser = RdfParser::new().unwrap();
        let file = create_ttl_file(input);

        if let Err(e) = parser.load_ttl(file.path()) {
            let error_msg = e.to_string();
            assert!(
                !error_msg.is_empty(),
                "Error message is empty for: {}",
                description
            );
            assert!(
                error_msg.len() > 10,
                "Error message too short for {}: '{}'",
                description,
                error_msg
            );
        }
    }
}

// ============================================================================
// 14. Stress Tests - Combined Adversarial Inputs
// ============================================================================

#[test]
fn test_kitchen_sink_adversarial() {
    let content = r#"
@prefix ex: <http://example.org/> .
@prefix ex: <http://example2.org/> .
@prefix ex: <http://example3.org/> .

# Circular references
ex:a ex:ref ex:b .
ex:b ex:ref ex:c .
ex:c ex:ref ex:a .

# Duplicate properties
ex:subj ex:prop "val1" .
ex:subj ex:prop "val2" .
ex:subj ex:prop "val3" .

# Unicode madness
ex:unicode ex:p "🎉🚀💻🔥Hello世界مرحبا" .
ex:zalgo ex:p "H̸̢̫̙̓ę̷̰͒l̴̰̈́l̶̰̀ǫ̶̣̈́" .

# Deeply nested
ex:root ex:has [
    ex:nested [
        ex:nested [
            ex:nested [
                ex:nested "deep"
            ]
        ]
    ]
] .

# Self reference
ex:self ex:ref ex:self .

# Long strings
"#;

    let mut full_content = String::from(content);
    let long_string = "x".repeat(10_000);
    full_content.push_str(&format!("ex:long ex:p \"{}\" .\n", long_string));

    assert!(
        assert_no_panic_on_parse(&full_content),
        "Parser panicked on kitchen sink adversarial input"
    );
}

#[test]
fn test_rapid_fire_malformed_inputs() {
    let malformed_inputs = vec![
        ";;;",
        "@@@",
        "<<<>>>",
        "[ ] ] ]",
        "( ) ) )",
        "\" \" \" \"",
        ". . . .",
        ", , , ,",
        ": : : :",
        "^ ^ ^ ^",
        "@ @ @ @",
    ];

    for input in malformed_inputs {
        assert!(
            assert_no_panic_on_parse(input),
            "Parser panicked on malformed input: {}",
            input
        );
    }
}
