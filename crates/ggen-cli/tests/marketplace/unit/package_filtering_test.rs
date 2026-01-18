//! Unit tests for marketplace package filtering
//!
//! Tests verify filtering by:
//! - Maturity level (experimental, beta, production, enterprise)
//! - Score range (min/max thresholds)
//! - Dimension scores (documentation, testing, security, etc.)
//! - Use case matching

use ggen_marketplace::prelude::*;

#[test]
fn test_filter_by_maturity_level_production() {
    // Arrange: Generate sample packages
    let all_packages = generate_all_assessments();

    // Act: Filter for production-ready packages
    let production_packages = filter_by_level(&all_packages, MaturityLevel::Production);

    // Assert: All returned packages should be production or higher
    for pkg in &production_packages {
        assert!(
            matches!(
                pkg.level(),
                MaturityLevel::Production | MaturityLevel::Enterprise
            ),
            "Package {} has level {:?}, expected Production or Enterprise",
            pkg.package_id,
            pkg.level()
        );
        assert!(
            pkg.total_score() >= 61,
            "Production packages should score >= 61"
        );
    }
}

#[test]
fn test_filter_by_maturity_level_beta() {
    let all_packages = generate_all_assessments();
    let beta_packages = filter_by_level(&all_packages, MaturityLevel::Beta);

    for pkg in &beta_packages {
        assert!(
            pkg.level() >= MaturityLevel::Beta,
            "Package {} should be Beta or higher",
            pkg.package_id
        );
        assert!(pkg.total_score() >= 41);
    }
}

#[test]
fn test_filter_by_maturity_level_experimental() {
    let all_packages = generate_all_assessments();
    let experimental_packages = filter_by_level(&all_packages, MaturityLevel::Experimental);

    // All packages should be included (experimental is minimum)
    assert_eq!(experimental_packages.len(), all_packages.len());
}

#[test]
fn test_filter_by_score_range() {
    // Arrange
    let all_packages = generate_all_assessments();

    // Act: Filter for scores 70-90
    let filtered = filter_by_score_range(&all_packages, 70, 90);

    // Assert: All packages should be in range
    for pkg in &filtered {
        let score = pkg.total_score();
        assert!(
            score >= 70 && score <= 90,
            "Package {} score {} should be in [70,90]",
            pkg.package_id,
            score
        );
    }
}

#[test]
fn test_filter_by_score_minimum_only() {
    let all_packages = generate_all_assessments();

    // Filter for minimum score of 80
    let filtered = filter_by_score_range(&all_packages, 80, 100);

    for pkg in &filtered {
        assert!(pkg.total_score() >= 80);
    }
}

#[test]
fn test_filter_by_score_exact_range() {
    let all_packages = generate_all_assessments();

    // Very narrow range
    let filtered = filter_by_score_range(&all_packages, 85, 87);

    for pkg in &filtered {
        let score = pkg.total_score();
        assert!(score >= 85 && score <= 87);
    }
}

#[test]
fn test_filter_by_dimension_documentation() {
    // Arrange
    let all_packages = generate_all_assessments();

    // Act: Filter for packages with excellent documentation (>= 18/20)
    let criteria = vec![("documentation", 18u32)];
    let filtered = filter_by_dimensions(&all_packages, &criteria);

    // Assert
    for pkg in &filtered {
        assert!(
            pkg.documentation.total() >= 18,
            "Package {} documentation score {} should be >= 18",
            pkg.package_id,
            pkg.documentation.total()
        );
    }
}

#[test]
fn test_filter_by_dimension_testing() {
    let all_packages = generate_all_assessments();

    let criteria = vec![("testing", 15u32)];
    let filtered = filter_by_dimensions(&all_packages, &criteria);

    for pkg in &filtered {
        assert!(pkg.testing.total() >= 15);
    }
}

#[test]
fn test_filter_by_dimension_security() {
    let all_packages = generate_all_assessments();

    let criteria = vec![("security", 18u32)];
    let filtered = filter_by_dimensions(&all_packages, &criteria);

    for pkg in &filtered {
        assert!(pkg.security.total() >= 18);
    }
}

#[test]
fn test_filter_by_multiple_dimensions() {
    // Arrange
    let all_packages = generate_all_assessments();

    // Act: Filter for high documentation AND testing
    let criteria = vec![("documentation", 15u32), ("testing", 15u32)];
    let filtered = filter_by_dimensions(&all_packages, &criteria);

    // Assert: All must meet both criteria
    for pkg in &filtered {
        assert!(pkg.documentation.total() >= 15);
        assert!(pkg.testing.total() >= 15);
    }
}

#[test]
fn test_filter_by_all_dimensions_high_bar() {
    let all_packages = generate_all_assessments();

    // Require high scores across all dimensions
    let criteria = vec![
        ("documentation", 15u32),
        ("testing", 15u32),
        ("security", 15u32),
        ("performance", 12u32),
        ("adoption", 10u32),
        ("maintenance", 8u32),
    ];
    let filtered = filter_by_dimensions(&all_packages, &criteria);

    for pkg in &filtered {
        assert!(pkg.documentation.total() >= 15);
        assert!(pkg.testing.total() >= 15);
        assert!(pkg.security.total() >= 15);
        assert!(pkg.performance.total() >= 12);
        assert!(pkg.adoption.total() >= 10);
        assert!(pkg.maintenance.total() >= 8);
    }
}

#[test]
fn test_filter_empty_result_high_threshold() {
    let all_packages = generate_all_assessments();

    // Impossible criteria
    let filtered = filter_by_score_range(&all_packages, 101, 110);

    assert!(filtered.is_empty(), "No packages should score > 100");
}

#[test]
fn test_filter_by_use_case_production() {
    // Arrange
    let all_packages = generate_all_assessments();

    // Act: Find packages for production use
    let production_packages = find_for_use_case(&all_packages, "production");

    // Assert: Should be production-ready
    for pkg in &production_packages {
        assert!(
            pkg.total_score() >= 65,
            "Production use case should require score >= 65"
        );
    }
}

#[test]
fn test_filter_by_use_case_research() {
    let all_packages = generate_all_assessments();

    let research_packages = find_for_use_case(&all_packages, "research");

    // Research can accept lower maturity
    for pkg in &research_packages {
        assert!(
            pkg.total_score() >= 40,
            "Research use case should accept score >= 40"
        );
    }
}

#[test]
fn test_filter_by_use_case_enterprise() {
    let all_packages = generate_all_assessments();

    let enterprise_packages = find_for_use_case(&all_packages, "enterprise");

    // Enterprise requires highest maturity
    for pkg in &enterprise_packages {
        assert!(
            pkg.total_score() >= 85,
            "Enterprise use case should require score >= 85"
        );
    }
}

#[test]
fn test_filter_by_use_case_startup() {
    let all_packages = generate_all_assessments();

    let startup_packages = find_for_use_case(&all_packages, "startup");

    // Startup needs minimum viable
    for pkg in &startup_packages {
        assert!(
            pkg.total_score() >= 50,
            "Startup use case should require score >= 50"
        );
    }
}

#[test]
fn test_filter_combination_level_and_dimension() {
    // Arrange
    let all_packages = generate_all_assessments();

    // Act: Production level WITH high security
    let production = filter_by_level(&all_packages, MaturityLevel::Production);
    let criteria = vec![("security", 18u32)];
    let secure_production = filter_by_dimensions(&production, &criteria);

    // Assert
    for pkg in &secure_production {
        assert!(pkg.level() >= MaturityLevel::Production);
        assert!(pkg.security.total() >= 18);
    }
}

#[test]
fn test_filter_preserves_order() {
    // Arrange: Get packages in specific order
    let all_packages = generate_all_assessments();
    let original_ids: Vec<_> = all_packages.iter().map(|p| &p.package_id).collect();

    // Act: Filter with loose criteria (should keep most/all)
    let filtered = filter_by_score_range(&all_packages, 0, 100);
    let filtered_ids: Vec<_> = filtered.iter().map(|p| &p.package_id).collect();

    // Assert: Relative order should be preserved
    for id in &original_ids {
        if filtered_ids.contains(id) {
            let original_idx = original_ids.iter().position(|x| x == id).unwrap();
            let filtered_idx = filtered_ids.iter().position(|x| x == id).unwrap();

            // Check that earlier items in original are earlier in filtered
            for other_id in &filtered_ids[0..filtered_idx] {
                let other_original_idx = original_ids.iter().position(|x| x == other_id).unwrap();
                assert!(
                    other_original_idx < original_idx,
                    "Order should be preserved"
                );
            }
        }
    }
}

#[test]
fn test_filter_no_mutation() {
    // Arrange
    let all_packages = generate_all_assessments();
    let original_len = all_packages.len();
    let original_first = all_packages.first().unwrap().clone();

    // Act: Apply filters
    let _ = filter_by_level(&all_packages, MaturityLevel::Production);
    let _ = filter_by_score_range(&all_packages, 70, 90);

    // Assert: Original should be unchanged
    assert_eq!(all_packages.len(), original_len);
    assert_eq!(
        all_packages.first().unwrap().package_id,
        original_first.package_id
    );
}
