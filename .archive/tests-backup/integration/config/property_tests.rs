//! Property-based tests for GgenConfig
//!
//! Uses proptest to verify deterministic behavior and invariants
//! Chicago TDD: Property tests verify behavior holds across random inputs

use proptest::prelude::*;
use ggen_utils::project_config::GgenConfig;
use std::collections::BTreeMap;

proptest! {
    #[test]
    fn test_toml_parsing_is_deterministic(
        output_dir in "[a-z0-9/_-]+",
        prefix_key in "[a-z]{2,10}",
        prefix_uri in "http://[a-z]+\\.org/"
    ) {
        // Arrange: Create TOML string
        let toml_str = format!(
            r#"
[project]
output_dir = "{}"

[prefixes]
{} = "{}"

[rdf]
"#,
            output_dir, prefix_key, prefix_uri
        );

        // Act: Parse twice
        let config1: Result<GgenConfig, _> = toml::from_str(&toml_str);
        let config2: Result<GgenConfig, _> = toml::from_str(&toml_str);

        // Assert: Verify deterministic parsing
        match (config1, config2) {
            (Ok(c1), Ok(c2)) => {
                assert_eq!(c1.project.output_dir, c2.project.output_dir);
                assert_eq!(c1.prefixes.len(), c2.prefixes.len());
            }
            (Err(_), Err(_)) => {
                // Both failed consistently - acceptable for invalid inputs
            }
            _ => panic!("Parsing should be deterministic (both succeed or both fail)"),
        }
    }

    #[test]
    fn test_empty_collections_are_default(
        output_dir in "[a-z0-9/_-]+"
    ) {
        // Arrange: Create minimal TOML
        let toml_str = format!(
            r#"
[project]
output_dir = "{}"

[rdf]
"#,
            output_dir
        );

        // Act: Parse config
        let config: Result<GgenConfig, _> = toml::from_str(&toml_str);

        // Assert: Verify defaults are empty collections
        if let Ok(cfg) = config {
            assert!(cfg.prefixes.is_empty());
            assert!(cfg.vars.is_empty());
            assert!(cfg.rdf.files.is_empty());
            assert!(cfg.rdf.inline.is_empty());
        }
    }

    #[test]
    fn test_btreemap_maintains_order(
        keys in prop::collection::vec("[a-z]{3}", 1..10)
    ) {
        // Arrange: Create unique sorted keys
        let mut unique_keys: Vec<String> = keys.into_iter().collect();
        unique_keys.sort();
        unique_keys.dedup();

        if unique_keys.is_empty() {
            return Ok(());
        }

        // Arrange: Build TOML with multiple prefixes
        let mut toml_str = String::from("[project]\noutput_dir = \"out\"\n\n[prefixes]\n");
        for key in &unique_keys {
            toml_str.push_str(&format!("{} = \"http://example.org/{}/\"\n", key, key));
        }
        toml_str.push_str("\n[rdf]\n");

        // Act: Parse config
        let config: Result<GgenConfig, _> = toml::from_str(&toml_str);

        // Assert: Verify BTreeMap maintains sorted order
        if let Ok(cfg) = config {
            let parsed_keys: Vec<&String> = cfg.prefixes.keys().collect();
            let mut sorted_unique = unique_keys.clone();
            sorted_unique.sort();

            assert_eq!(
                parsed_keys.len(),
                sorted_unique.len(),
                "All prefixes should be present"
            );

            for (parsed, expected) in parsed_keys.iter().zip(sorted_unique.iter()) {
                assert_eq!(*parsed, expected, "Keys should be in sorted order");
            }
        }
    }

    #[test]
    fn test_rdf_inline_preserves_line_order(
        line_count in 1usize..10,
        line_prefix in "[a-z]{2,5}"
    ) {
        // Arrange: Create TOML with multiple inline RDF lines
        let mut toml_str = String::from("[project]\noutput_dir = \"out\"\n\n[rdf]\ninline = [\n");

        let expected_lines: Vec<String> = (0..line_count)
            .map(|i| format!("{}_{}", line_prefix, i))
            .collect();

        for line in &expected_lines {
            toml_str.push_str(&format!("  \"{}\",\n", line));
        }
        toml_str.push_str("]\n");

        // Act: Parse config
        let config: Result<GgenConfig, _> = toml::from_str(&toml_str);

        // Assert: Verify line order is preserved
        if let Ok(cfg) = config {
            assert_eq!(cfg.rdf.inline.len(), expected_lines.len());
            for (parsed, expected) in cfg.rdf.inline.iter().zip(expected_lines.iter()) {
                assert_eq!(parsed, expected, "Inline RDF lines should preserve order");
            }
        }
    }
}

#[test]
fn test_config_serialization_roundtrip_invariant() {
    // Arrange: Create config with known values
    let original_toml = r#"
[project]
output_dir = "generated"

[prefixes]
ex = "http://example.org/"
schema = "http://schema.org/"

[rdf]
files = ["data.ttl"]
inline = ["@prefix ex: <http://example.org/> ."]

[vars]
project_name = "test"
"#;

    // Act: Parse, serialize, parse again
    let config1: GgenConfig = toml::from_str(original_toml).unwrap();
    let serialized = toml::to_string(&config1).unwrap();
    let config2: GgenConfig = toml::from_str(&serialized).unwrap();

    // Assert: Verify roundtrip preserves structure
    assert_eq!(config1.project.output_dir, config2.project.output_dir);
    assert_eq!(config1.prefixes.len(), config2.prefixes.len());
    assert_eq!(config1.rdf.files.len(), config2.rdf.files.len());
    assert_eq!(config1.rdf.inline.len(), config2.rdf.inline.len());
    assert_eq!(config1.vars.len(), config2.vars.len());
}
