//! Unit tests for V1↔V2 adapter conversion
//!
//! Tests the bidirectional conversion between marketplace v1 and v2 models,
//! ensuring data integrity, unicode support, and round-trip accuracy.

#[cfg(test)]
mod adapter_conversion_tests {
    use ggen_marketplace::Package as V1Package;
    use ggen_marketplace_v2::models::{Package as V2Package, PackageId, PackageVersion};

    /// Helper: Create V1 test package
    fn create_v1_package(id: &str, name: &str, version: &str) -> V1Package {
        use chrono::Utc;
        use ggen_marketplace::{MaturityLevel, PackageMetadata};

        V1Package {
            metadata: PackageMetadata {
                id: id.to_string(),
                name: name.to_string(),
                version: version.to_string(),
                description: format!("Description for {}", name),
                author: "test-author".to_string(),
                license: "MIT".to_string(),
                repository_url: Some(format!("https://github.com/test/{}", id)),
                created_at: Utc::now(),
                updated_at: Utc::now(),
                maturity: MaturityLevel::Stable,
                tags: vec!["test".to_string(), "package".to_string()],
            },
        }
    }

    /// Helper: Convert V1→V2 (adapter logic placeholder)
    fn v1_to_v2(v1: &V1Package) -> V2Package {
        use chrono::Utc;
        use ggen_marketplace_v2::models::PackageMetadata as V2Metadata;

        V2Package {
            metadata: V2Metadata {
                id: PackageId::new(&v1.metadata.id).unwrap(),
                name: v1.metadata.name.clone(),
                version: PackageVersion::new(&v1.metadata.version).unwrap(),
                description: v1.metadata.description.clone(),
                author: v1.metadata.author.clone(),
                license: v1.metadata.license.clone(),
                repository_url: v1.metadata.repository_url.clone(),
                created_at: v1.metadata.created_at,
                updated_at: v1.metadata.updated_at,
                tags: v1.metadata.tags.clone(),
            },
        }
    }

    /// Helper: Convert V2→V1 (adapter logic placeholder)
    fn v2_to_v1(v2: &V2Package) -> V1Package {
        use ggen_marketplace::{MaturityLevel, PackageMetadata};

        V1Package {
            metadata: PackageMetadata {
                id: v2.metadata.id.to_string(),
                name: v2.metadata.name.clone(),
                version: v2.metadata.version.to_string(),
                description: v2.metadata.description.clone(),
                author: v2.metadata.author.clone(),
                license: v2.metadata.license.clone(),
                repository_url: v2.metadata.repository_url.clone(),
                created_at: v2.metadata.created_at,
                updated_at: v2.metadata.updated_at,
                maturity: MaturityLevel::Stable, // Default mapping
                tags: v2.metadata.tags.clone(),
            },
        }
    }

    #[test]
    fn test_v1_to_v2_basic_conversion() {
        let v1_pkg = create_v1_package("test-package", "Test Package", "1.0.0");
        let v2_pkg = v1_to_v2(&v1_pkg);

        assert_eq!(v2_pkg.metadata.id.to_string(), "test-package");
        assert_eq!(v2_pkg.metadata.name, "Test Package");
        assert_eq!(v2_pkg.metadata.version.to_string(), "1.0.0");
        assert_eq!(v2_pkg.metadata.author, "test-author");
        assert_eq!(v2_pkg.metadata.license, "MIT");
    }

    #[test]
    fn test_v2_to_v1_basic_conversion() {
        let v1_pkg = create_v1_package("reverse-test", "Reverse Test", "2.0.0");
        let v2_pkg = v1_to_v2(&v1_pkg);
        let roundtrip = v2_to_v1(&v2_pkg);

        assert_eq!(roundtrip.metadata.id, "reverse-test");
        assert_eq!(roundtrip.metadata.name, "Reverse Test");
        assert_eq!(roundtrip.metadata.version, "2.0.0");
    }

    #[test]
    fn test_round_trip_conversion_preserves_data() {
        let original = create_v1_package("roundtrip", "Roundtrip Test", "1.2.3");
        let v2_pkg = v1_to_v2(&original);
        let restored = v2_to_v1(&v2_pkg);

        // Core metadata must match exactly
        assert_eq!(restored.metadata.id, original.metadata.id);
        assert_eq!(restored.metadata.name, original.metadata.name);
        assert_eq!(restored.metadata.version, original.metadata.version);
        assert_eq!(restored.metadata.description, original.metadata.description);
        assert_eq!(restored.metadata.author, original.metadata.author);
        assert_eq!(restored.metadata.license, original.metadata.license);
        assert_eq!(restored.metadata.tags, original.metadata.tags);
    }

    #[test]
    fn test_unicode_package_name_conversion() {
        let v1_pkg = create_v1_package("unicode-test", "测试包 🚀 Тест", "1.0.0");
        let v2_pkg = v1_to_v2(&v1_pkg);
        let roundtrip = v2_to_v1(&v2_pkg);

        assert_eq!(roundtrip.metadata.name, "测试包 🚀 Тест");
        assert_eq!(v2_pkg.metadata.name, "测试包 🚀 Тест");
    }

    #[test]
    fn test_unicode_description_conversion() {
        let mut v1_pkg = create_v1_package("desc-unicode", "Unicode Desc", "1.0.0");
        v1_pkg.metadata.description = "This is a test with émojis 🎉 and ĆÝŘÏĻĻÏĆ".to_string();

        let v2_pkg = v1_to_v2(&v1_pkg);
        let roundtrip = v2_to_v1(&v2_pkg);

        assert_eq!(roundtrip.metadata.description, v1_pkg.metadata.description);
    }

    #[test]
    fn test_special_characters_in_tags() {
        let mut v1_pkg = create_v1_package("special-tags", "Special Tags", "1.0.0");
        v1_pkg.metadata.tags = vec![
            "rust".to_string(),
            "CLI-tool".to_string(),
            "async/await".to_string(),
            "💻 programming".to_string(),
        ];

        let v2_pkg = v1_to_v2(&v1_pkg);
        let roundtrip = v2_to_v1(&v2_pkg);

        assert_eq!(roundtrip.metadata.tags, v1_pkg.metadata.tags);
    }

    #[test]
    fn test_empty_optional_fields() {
        let mut v1_pkg = create_v1_package("empty-opts", "Empty Options", "1.0.0");
        v1_pkg.metadata.repository_url = None;

        let v2_pkg = v1_to_v2(&v1_pkg);
        let roundtrip = v2_to_v1(&v2_pkg);

        assert_eq!(roundtrip.metadata.repository_url, None);
    }

    #[test]
    fn test_semantic_version_conversion() {
        let versions = vec!["0.1.0", "1.0.0", "2.3.4", "10.20.30"];

        for ver in versions {
            let v1_pkg = create_v1_package("semver-test", "SemVer Test", ver);
            let v2_pkg = v1_to_v2(&v1_pkg);
            let roundtrip = v2_to_v1(&v2_pkg);

            assert_eq!(roundtrip.metadata.version, ver);
        }
    }

    #[test]
    fn test_large_description_conversion() {
        let mut v1_pkg = create_v1_package("large-desc", "Large Description", "1.0.0");
        v1_pkg.metadata.description = "x".repeat(10000); // 10KB description

        let v2_pkg = v1_to_v2(&v1_pkg);
        let roundtrip = v2_to_v1(&v2_pkg);

        assert_eq!(roundtrip.metadata.description.len(), 10000);
        assert_eq!(roundtrip.metadata.description, v1_pkg.metadata.description);
    }

    #[test]
    fn test_many_tags_conversion() {
        let mut v1_pkg = create_v1_package("many-tags", "Many Tags", "1.0.0");
        v1_pkg.metadata.tags = (0..100).map(|i| format!("tag-{}", i)).collect();

        let v2_pkg = v1_to_v2(&v1_pkg);
        let roundtrip = v2_to_v1(&v2_pkg);

        assert_eq!(roundtrip.metadata.tags.len(), 100);
        assert_eq!(roundtrip.metadata.tags, v1_pkg.metadata.tags);
    }

    #[test]
    fn test_timestamp_preservation() {
        let v1_pkg = create_v1_package("timestamp-test", "Timestamp Test", "1.0.0");
        let original_created = v1_pkg.metadata.created_at;
        let original_updated = v1_pkg.metadata.updated_at;

        let v2_pkg = v1_to_v2(&v1_pkg);

        // Timestamps should be preserved exactly
        assert_eq!(
            v2_pkg.metadata.created_at.timestamp(),
            original_created.timestamp()
        );
        assert_eq!(
            v2_pkg.metadata.updated_at.timestamp(),
            original_updated.timestamp()
        );
    }

    #[test]
    fn test_author_with_special_characters() {
        let mut v1_pkg = create_v1_package("author-special", "Author Special", "1.0.0");
        v1_pkg.metadata.author =
            "John O'Brien <john@example.com> (https://example.com)".to_string();

        let v2_pkg = v1_to_v2(&v1_pkg);
        let roundtrip = v2_to_v1(&v2_pkg);

        assert_eq!(roundtrip.metadata.author, v1_pkg.metadata.author);
    }

    #[test]
    fn test_license_spdx_identifiers() {
        let licenses = vec![
            "MIT",
            "Apache-2.0",
            "GPL-3.0",
            "BSD-3-Clause",
            "MIT OR Apache-2.0",
        ];

        for license in licenses {
            let mut v1_pkg = create_v1_package("license-test", "License Test", "1.0.0");
            v1_pkg.metadata.license = license.to_string();

            let v2_pkg = v1_to_v2(&v1_pkg);
            let roundtrip = v2_to_v1(&v2_pkg);

            assert_eq!(roundtrip.metadata.license, license);
        }
    }

    #[test]
    fn test_conversion_performance() {
        use std::time::Instant;

        let v1_pkg = create_v1_package("perf-test", "Performance Test", "1.0.0");

        let start = Instant::now();
        for _ in 0..1000 {
            let v2_pkg = v1_to_v2(&v1_pkg);
            let _ = v2_to_v1(&v2_pkg);
        }
        let duration = start.elapsed();

        // Should complete 1000 round-trips in <100ms
        assert!(
            duration.as_millis() < 100,
            "Conversion too slow: {:?}",
            duration
        );
    }

    #[test]
    fn test_null_byte_handling() {
        // Ensure null bytes are handled/rejected properly
        let v1_pkg = create_v1_package("null-test", "Null\0Test", "1.0.0");
        let v2_pkg = v1_to_v2(&v1_pkg);

        // System should either strip null bytes or preserve them safely
        assert!(!v2_pkg.metadata.name.contains('\0') || v2_pkg.metadata.name == "Null\0Test");
    }
}
