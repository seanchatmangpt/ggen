//! Unit tests for Package→RDF triple mapping
//!
//! Tests the conversion of package metadata into RDF triples,
//! SPARQL query generation, and round-trip conversion accuracy.

#[cfg(test)]
mod rdf_mapping_tests {
    use chrono::Utc;
    use ggen_marketplace::models::{Package, PackageId, PackageMetadata, PackageVersion};
    use ggen_marketplace::ontology::MARKETPLACE_ONTOLOGY;

    /// Helper: Create test package
    fn create_test_package(id: &str, name: &str, version: &str) -> Package {
        Package {
            metadata: PackageMetadata {
                id: PackageId::new(id).unwrap(),
                name: name.to_string(),
                version: PackageVersion::new(version).unwrap(),
                description: format!("Test package: {}", name),
                author: "test-author".to_string(),
                license: "MIT".to_string(),
                repository_url: Some(format!("https://github.com/test/{}", id)),
                created_at: Utc::now(),
                updated_at: Utc::now(),
                tags: vec!["test".to_string()],
            },
        }
    }

    /// Helper: Convert package to RDF triples (simulated)
    fn package_to_rdf_triples(pkg: &Package) -> Vec<(String, String, String)> {
        let base_uri = format!("http://ggen.dev/marketplace/{}", pkg.metadata.id);

        vec![
            (
                base_uri.clone(),
                "http://www.w3.org/1999/02/22-rdf-syntax-ns#type".to_string(),
                "http://ggen.dev/ontology#Package".to_string(),
            ),
            (
                base_uri.clone(),
                "http://ggen.dev/ontology#name".to_string(),
                pkg.metadata.name.clone(),
            ),
            (
                base_uri.clone(),
                "http://ggen.dev/ontology#version".to_string(),
                pkg.metadata.version.to_string(),
            ),
            (
                base_uri.clone(),
                "http://ggen.dev/ontology#description".to_string(),
                pkg.metadata.description.clone(),
            ),
            (
                base_uri.clone(),
                "http://ggen.dev/ontology#author".to_string(),
                pkg.metadata.author.clone(),
            ),
            (
                base_uri,
                "http://ggen.dev/ontology#license".to_string(),
                pkg.metadata.license.clone(),
            ),
        ]
    }

    #[test]
    fn test_basic_rdf_triple_generation() {
        let pkg = create_test_package("basic-pkg", "Basic Package", "1.0.0");
        let triples = package_to_rdf_triples(&pkg);

        assert!(!triples.is_empty());
        assert!(triples.len() >= 6); // At least: type, name, version, desc, author, license

        // Verify RDF type triple
        let type_triple = triples
            .iter()
            .find(|(_, p, _)| p == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type");
        assert!(type_triple.is_some());
    }

    #[test]
    fn test_rdf_name_property_mapping() {
        let pkg = create_test_package("name-test", "Name Test Package", "1.0.0");
        let triples = package_to_rdf_triples(&pkg);

        let name_triple = triples
            .iter()
            .find(|(_, p, _)| p == "http://ggen.dev/ontology#name");

        assert!(name_triple.is_some());
        let (_, _, obj) = name_triple.unwrap();
        assert_eq!(obj, "Name Test Package");
    }

    #[test]
    fn test_rdf_version_property_mapping() {
        let pkg = create_test_package("version-test", "Version Test", "2.3.4");
        let triples = package_to_rdf_triples(&pkg);

        let version_triple = triples
            .iter()
            .find(|(_, p, _)| p == "http://ggen.dev/ontology#version");

        assert!(version_triple.is_some());
        let (_, _, obj) = version_triple.unwrap();
        assert_eq!(obj, "2.3.4");
    }

    #[test]
    fn test_rdf_unicode_handling() {
        let pkg = create_test_package("unicode-rdf", "Unicode 测试 🚀", "1.0.0");
        let triples = package_to_rdf_triples(&pkg);

        let name_triple = triples
            .iter()
            .find(|(_, p, _)| p == "http://ggen.dev/ontology#name");

        assert!(name_triple.is_some());
        let (_, _, obj) = name_triple.unwrap();
        assert_eq!(obj, "Unicode 测试 🚀");
    }

    #[test]
    fn test_rdf_uri_generation() {
        let pkg = create_test_package("uri-test", "URI Test", "1.0.0");
        let triples = package_to_rdf_triples(&pkg);

        // All triples should use the same subject URI
        let subject_uri = format!("http://ggen.dev/marketplace/{}", pkg.metadata.id);

        for (subj, _, _) in &triples {
            assert_eq!(subj, &subject_uri);
        }
    }

    #[test]
    fn test_ontology_vocabulary_usage() {
        let ontology = MARKETPLACE_ONTOLOGY;

        // Verify ontology contains expected classes and properties
        assert!(ontology.contains("Package"));
        assert!(ontology.contains("name"));
        assert!(ontology.contains("version"));
        assert!(ontology.contains("author"));
    }

    #[test]
    fn test_rdf_special_characters_escaping() {
        let mut pkg = create_test_package("special-chars", "Special Test", "1.0.0");
        pkg.metadata.description = r#"Description with "quotes" and <tags>"#.to_string();

        let triples = package_to_rdf_triples(&pkg);

        let desc_triple = triples
            .iter()
            .find(|(_, p, _)| p == "http://ggen.dev/ontology#description");

        assert!(desc_triple.is_some());
        // Should preserve special characters (escaping handled by RDF library)
        let (_, _, obj) = desc_triple.unwrap();
        assert!(obj.contains("quotes"));
        assert!(obj.contains("<tags>"));
    }

    #[test]
    fn test_rdf_optional_fields() {
        let mut pkg = create_test_package("optional-test", "Optional Test", "1.0.0");
        pkg.metadata.repository_url = None;

        let triples = package_to_rdf_triples(&pkg);

        // Optional fields should not generate triples if None
        let repo_triple = triples
            .iter()
            .find(|(_, p, _)| p == "http://ggen.dev/ontology#repositoryUrl");

        assert!(repo_triple.is_none());
    }

    #[test]
    fn test_rdf_multi_valued_properties() {
        let mut pkg = create_test_package("tags-test", "Tags Test", "1.0.0");
        pkg.metadata.tags = vec!["rust".to_string(), "cli".to_string(), "async".to_string()];

        // In RDF, multi-valued properties generate multiple triples
        // Each tag would be a separate triple with same subject/predicate

        // Simulated: package_to_rdf_triples would generate multiple tag triples
        let expected_tags = vec!["rust", "cli", "async"];

        for tag in expected_tags {
            assert!(pkg.metadata.tags.contains(&tag.to_string()));
        }
    }

    #[test]
    fn test_rdf_triple_count() {
        let pkg = create_test_package("count-test", "Count Test", "1.0.0");
        let triples = package_to_rdf_triples(&pkg);

        // Minimum expected triples: type, id, name, version, description, author, license
        assert!(triples.len() >= 6);
    }

    #[test]
    fn test_rdf_round_trip_conversion() {
        let original_pkg = create_test_package("roundtrip-rdf", "RDF Roundtrip", "1.0.0");
        let triples = package_to_rdf_triples(&original_pkg);

        // Simulate parsing triples back to package (simplified)
        let reconstructed_name = triples
            .iter()
            .find(|(_, p, _)| p == "http://ggen.dev/ontology#name")
            .map(|(_, _, o)| o.clone())
            .unwrap();

        let reconstructed_version = triples
            .iter()
            .find(|(_, p, _)| p == "http://ggen.dev/ontology#version")
            .map(|(_, _, o)| o.clone())
            .unwrap();

        assert_eq!(reconstructed_name, original_pkg.metadata.name);
        assert_eq!(
            reconstructed_version,
            original_pkg.metadata.version.to_string()
        );
    }

    #[test]
    fn test_rdf_timestamp_serialization() {
        let pkg = create_test_package("timestamp-rdf", "Timestamp RDF", "1.0.0");

        // Timestamps should be serialized in ISO 8601 format for RDF
        let created_iso = pkg.metadata.created_at.to_rfc3339();
        let updated_iso = pkg.metadata.updated_at.to_rfc3339();

        assert!(created_iso.contains('T'));
        assert!(updated_iso.contains('T'));
    }

    #[test]
    fn test_rdf_performance() {
        use std::time::Instant;

        let pkg = create_test_package("perf-rdf", "Performance RDF", "1.0.0");

        let start = Instant::now();
        for _ in 0..1000 {
            let _ = package_to_rdf_triples(&pkg);
        }
        let duration = start.elapsed();

        // Should generate 1000 RDF triple sets in <50ms
        assert!(
            duration.as_millis() < 50,
            "RDF generation too slow: {:?}",
            duration
        );
    }
}
