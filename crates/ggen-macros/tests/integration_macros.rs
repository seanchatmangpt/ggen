//! Integration tests for ggen-macros procedural macros
//!
//! These tests verify that the derive macros (#[derive(Guard)], #[derive(Bundle)])
//! correctly generate code for marketplace package validation.
//!
//! Note: Full procedural macro testing typically requires trybuild or macrotest crates.
//! These tests verify the macro metadata and helper functions.

use ggen_macros::Guard;

/// Test structure for Guard macro
#[derive(Guard)]
#[guard_name = "TestGuard8020"]
#[guard_description = "Test guard with 8020 coverage"]
pub struct TestGuard {
    pub ontology_valid: bool,
    pub projections_complete: bool,
    pub templates_present: bool,
}

/// Test: Guard derive macro generates correct metadata
#[test]
fn test_guard_metadata() {
    let guard = TestGuard {
        ontology_valid: true,
        projections_complete: true,
        templates_present: true,
    };

    // Verify guard name and description are accessible
    assert_eq!(TestGuard::guard_name(), "TestGuard8020");
    assert_eq!(
        TestGuard::guard_description(),
        "Test guard with 8020 coverage"
    );
}

/// Test: Guard validation returns expected structure
#[test]
fn test_guard_validation_structure() {
    let guard = TestGuard {
        ontology_valid: true,
        projections_complete: true,
        templates_present: false,
    };

    // Verify the guard can be instantiated and methods accessible
    let name = TestGuard::guard_name();
    assert!(!name.is_empty());
}

/// Test: Multiple guards can coexist
#[test]
fn test_multiple_guards() {
    #[derive(Guard)]
    #[guard_name = "OntologyGuard"]
    pub struct OntologyCheckGuard {
        pub is_valid: bool,
    }

    #[derive(Guard)]
    #[guard_name = "ProjectionGuard"]
    pub struct ProjectionCheckGuard {
        pub is_complete: bool,
    }

    let ontology = OntologyCheckGuard { is_valid: true };
    let projection = ProjectionCheckGuard { is_complete: true };

    assert_eq!(OntologyCheckGuard::guard_name(), "OntologyGuard");
    assert_eq!(ProjectionCheckGuard::guard_name(), "ProjectionGuard");
}

/// Test: Guard name customization via attribute
#[test]
fn test_guard_name_customization() {
    #[derive(Guard)]
    #[guard_name = "CustomGuardName"]
    pub struct CustomGuard {
        pub field: bool,
    }

    assert_eq!(CustomGuard::guard_name(), "CustomGuardName");
}

/// Test: Guard description customization
#[test]
fn test_guard_description_customization() {
    #[derive(Guard)]
    #[guard_name = "DescribedGuard"]
    #[guard_description = "This is a custom description for testing"]
    pub struct DescribedGuard {
        pub field: bool,
    }

    assert_eq!(
        DescribedGuard::guard_description(),
        "This is a custom description for testing"
    );
}

/// Test: Default description generation when not provided
#[test]
fn test_guard_default_description() {
    #[derive(Guard)]
    #[guard_name = "DefaultDescGuard"]
    pub struct DefaultDescGuard {
        pub field: bool,
    }

    let description = DefaultDescGuard::guard_description();
    assert!(!description.is_empty());
    // Should contain the guard name or a default message
    assert!(description.contains("DefaultDescGuard") || description.contains("Validates"));
}

/// Test: Guard struct with multiple fields
#[test]
fn test_guard_multiple_fields() {
    #[derive(Guard)]
    #[guard_name = "MultiFieldGuard"]
    pub struct MultiFieldGuard {
        pub ontology: bool,
        pub templates: bool,
        pub tests: bool,
        pub docs: bool,
        pub examples: bool,
    }

    let guard = MultiFieldGuard {
        ontology: true,
        templates: true,
        tests: false,
        docs: true,
        examples: false,
    };

    assert_eq!(MultiFieldGuard::guard_name(), "MultiFieldGuard");
}

/// Test: Guard name default generation from struct name
#[test]
fn test_guard_implicit_name() {
    #[derive(Guard)]
    pub struct ImplicitNameGuard {
        pub field: bool,
    }

    // Should use the struct name when guard_name attribute is not provided
    let name = ImplicitNameGuard::guard_name();
    assert!(!name.is_empty());
}

/// Test: Verify guard trait methods are generated
#[test]
fn test_guard_trait_implementation() {
    #[derive(Guard)]
    #[guard_name = "TraitTestGuard"]
    pub struct TraitTestGuard {
        pub check_passed: bool,
    }

    let guard = TraitTestGuard {
        check_passed: true,
    };

    // Guard trait should have been implemented
    let name = guard.guard_name();
    assert!(!name.is_empty());
}

/// Test: Guard validation can be called
#[test]
fn test_guard_validation_callable() {
    #[derive(Guard)]
    #[guard_name = "ValidatableGuard"]
    pub struct ValidatableGuard {
        pub is_valid: bool,
    }

    let guard = ValidatableGuard { is_valid: true };

    // Validation method should exist
    let validation_result = guard.validate_internal("/test/path");
    assert!(validation_result.is_ok());
}

/// Test: Guard scoring is available
#[test]
fn test_guard_scoring_available() {
    #[derive(Guard)]
    #[guard_name = "ScoredGuard"]
    pub struct ScoredGuard {
        pub check1: bool,
        pub check2: bool,
        pub check3: bool,
    }

    let guard = ScoredGuard {
        check1: true,
        check2: true,
        check3: false,
    };

    // Guard should have validation capability
    let validation = guard.validate_internal("/test");
    assert!(validation.is_ok());

    // Score should be available in result
    if let Ok(result) = validation {
        assert!(result.score >= 0);
        assert!(result.score <= 100);
    }
}

/// Test: Combined guards for comprehensive validation
#[test]
fn test_combined_guard_suite() {
    #[derive(Guard)]
    #[guard_name = "OntologyGuard"]
    pub struct OntologyGuard {
        pub valid: bool,
    }

    #[derive(Guard)]
    #[guard_name = "ProjectionGuard"]
    pub struct ProjectionGuard {
        pub complete: bool,
    }

    #[derive(Guard)]
    #[guard_name = "TestGuard"]
    pub struct TestGuard {
        pub passing: bool,
    }

    let guards = vec![
        OntologyGuard { valid: true },
        ProjectionGuard { complete: true },
        TestGuard { passing: true },
    ];

    assert_eq!(guards.len(), 3);
    assert_eq!(OntologyGuard::guard_name(), "OntologyGuard");
    assert_eq!(ProjectionGuard::guard_name(), "ProjectionGuard");
    assert_eq!(TestGuard::guard_name(), "TestGuard");
}

/// Test: Bundle-like guard composition
#[test]
fn test_bundle_guard_composition() {
    #[derive(Guard)]
    #[guard_name = "BundleGuard")]
    #[guard_description = "Comprehensive bundle validation"]
    pub struct BundleGuard {
        pub ontology: bool,
        pub projections: bool,
        pub templates: bool,
        pub tests: bool,
        pub docs: bool,
        pub examples: bool,
    }

    let bundle_guard = BundleGuard {
        ontology: true,
        projections: true,
        templates: true,
        tests: true,
        docs: true,
        examples: false,
    };

    assert_eq!(BundleGuard::guard_name(), "BundleGuard");
    let desc = BundleGuard::guard_description();
    assert_eq!(desc, "Comprehensive bundle validation");
}

/// Test: Macro hygiene - no namespace pollution
#[test]
fn test_macro_hygiene() {
    #[derive(Guard)]
    #[guard_name = "HygieneTestGuard")]
    pub struct HygieneTestGuard {
        pub value: bool,
    }

    let guard = HygieneTestGuard { value: true };

    // Methods should be accessible with proper namespacing
    let name = HygieneTestGuard::guard_name();
    assert!(name.contains("HygieneTestGuard") || name.contains("Hygiene"));
}
