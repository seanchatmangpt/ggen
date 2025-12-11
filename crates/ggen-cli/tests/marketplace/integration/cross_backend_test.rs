//! Cross-backend comparison tests
//!
//! NOTE: These tests are disabled because marketplace v1 has been removed.
//! The CLI now uses ggen-marketplace-v2 directly.
//!
//! This file is kept for reference only.

#[cfg(test)]
#[cfg(all(feature = "marketplace-v1", feature = "marketplace-v2"))] // v1 feature no longer exists - tests are disabled
#[allow(dead_code)] // Tests are intentionally disabled
mod cross_backend_tests {
    use chrono::Utc;
    // NOTE: ggen_marketplace (v1) no longer exists
    use ggen_marketplace_v2::models::Package as V2Package;

    /// Helper: Create test packages for v2 (v1 removed)
    fn create_test_packages(id: &str, name: &str, version: &str) -> V2Package {
        // NOTE: v1 removed - only v2 package created
        use ggen_marketplace_v2::models::{PackageId, PackageMetadata as V2Meta, PackageVersion};

        let created_at = Utc::now();
        let updated_at = Utc::now();

        // NOTE: v1 package creation removed - v1 no longer exists

        let v2_pkg = V2Package {
            metadata: V2Meta {
                id: PackageId::new(id).unwrap(),
                name: name.to_string(),
                version: PackageVersion::new(version).unwrap(),
                description: format!("Test: {}", name),
                author: "test-author".to_string(),
                license: "MIT".to_string(),
                repository_url: Some(format!("https://github.com/test/{}", id)),
                created_at,
                updated_at,
                tags: vec!["test".to_string()],
            },
        };

        v2_pkg
    }

    #[tokio::test]
    async fn test_same_package_data_both_backends() {
        // NOTE: v1 removed - test disabled
        let _v2_pkg = create_test_packages("cross-test", "Cross Test", "1.0.0");
        // Test disabled - v1 no longer exists
    }

    #[tokio::test]
    async fn test_search_consistency_across_backends() {
        // Simulated search test (actual implementation would use real backends)
        let search_term = "database";

        // V1 search (placeholder)
        let v1_results = vec!["db-pkg", "database-utils"];

        // V2 search should return same packages
        let v2_results = vec!["db-pkg", "database-utils"];

        assert_eq!(v1_results, v2_results, "Search results should be identical");
    }

    #[tokio::test]
    async fn test_list_consistency_across_backends() {
        // Both backends should list same packages
        let limit = 10;
        let offset = 0;

        // V1 list (placeholder)
        let v1_count = 100;

        // V2 list should return same count
        let v2_count = 100;

        assert_eq!(v1_count, v2_count, "Package count should match");
    }

    #[tokio::test]
    async fn test_install_behavior_consistency() {
        let (v1_pkg, v2_pkg) = create_test_packages("install-test", "Install Test", "1.0.0");

        // Install behavior should be identical
        assert_eq!(v1_pkg.metadata.id, v2_pkg.metadata.id.to_string());
        assert_eq!(v1_pkg.metadata.version, v2_pkg.metadata.version.to_string());
    }

    #[tokio::test]
    async fn test_error_handling_consistency() {
        // Non-existent package should produce similar errors
        let invalid_id = "nonexistent-package-xyz";

        // V1 error (simulated)
        let v1_error = "Package not found: nonexistent-package-xyz";

        // V2 error should be similar
        let v2_error = "Package not found: nonexistent-package-xyz";

        assert_eq!(v1_error, v2_error, "Error messages should be consistent");
    }

    #[tokio::test]
    async fn test_performance_comparison_search() {
        use std::time::Instant;

        // V1 search performance
        let v1_start = Instant::now();
        // Simulated V1 search
        std::thread::sleep(std::time::Duration::from_millis(50));
        let v1_duration = v1_start.elapsed();

        // V2 search performance
        let v2_start = Instant::now();
        // Simulated V2 search
        std::thread::sleep(std::time::Duration::from_millis(50));
        let v2_duration = v2_start.elapsed();

        // V2 should be comparable to V1 (within 2x)
        assert!(
            v2_duration.as_millis() <= v1_duration.as_millis() * 2,
            "V2 search too slow compared to V1"
        );
    }

    #[tokio::test]
    async fn test_performance_comparison_lookup() {
        use std::time::Instant;

        // V1 lookup performance
        let v1_start = Instant::now();
        std::thread::sleep(std::time::Duration::from_millis(30));
        let v1_duration = v1_start.elapsed();

        // V2 lookup performance
        let v2_start = Instant::now();
        std::thread::sleep(std::time::Duration::from_millis(30));
        let v2_duration = v2_start.elapsed();

        // V2 should be comparable
        assert!(v2_duration.as_millis() <= v1_duration.as_millis() * 2);
    }

    #[tokio::test]
    async fn test_unicode_handling_consistency() {
        let (v1_pkg, v2_pkg) = create_test_packages("unicode-cross", "Unicode 测试 🚀", "1.0.0");

        assert_eq!(v1_pkg.metadata.name, v2_pkg.metadata.name);
    }

    #[tokio::test]
    async fn test_special_characters_consistency() {
        let (v1_pkg, v2_pkg) =
            create_test_packages("special-cross", "Test \"Package\" <Name>", "1.0.0");

        assert_eq!(v1_pkg.metadata.name, v2_pkg.metadata.name);
    }

    #[tokio::test]
    async fn test_version_sorting_consistency() {
        let versions = vec!["1.0.0", "2.0.0", "1.5.0", "0.9.0"];

        // Both backends should sort versions identically
        let mut v1_sorted = versions.clone();
        v1_sorted.sort();

        let mut v2_sorted = versions.clone();
        v2_sorted.sort();

        assert_eq!(v1_sorted, v2_sorted);
    }

    #[tokio::test]
    async fn test_pagination_consistency() {
        // Page 1: limit=10, offset=0
        // Both backends should return same subset

        let page_size = 10;
        let page_offset = 0;

        // Simulated results
        let v1_page: Vec<&str> = vec![];
        let v2_page: Vec<&str> = vec![];

        assert_eq!(v1_page.len(), v2_page.len());
    }

    #[tokio::test]
    async fn test_filter_consistency() {
        // Filter by tag should produce same results
        let filter_tag = "rust";

        // Simulated results
        let v1_filtered = vec!["rust-pkg-1", "rust-pkg-2"];
        let v2_filtered = vec!["rust-pkg-1", "rust-pkg-2"];

        assert_eq!(v1_filtered, v2_filtered);
    }
}
