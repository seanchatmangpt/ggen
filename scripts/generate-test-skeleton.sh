#!/bin/bash
# Generate test skeleton for a module
#
# Usage: ./scripts/generate-test-skeleton.sh <module> <crate>
# Example: ./scripts/generate-test-skeleton.sh cache ggen-core

set -e

MODULE=$1
CRATE=$2

if [ -z "$MODULE" ] || [ -z "$CRATE" ]; then
    echo "❌ Missing required arguments"
    echo ""
    echo "Usage: $0 <module> <crate>"
    echo ""
    echo "Examples:"
    echo "  $0 cache ggen-core"
    echo "  $0 install ggen-domain"
    echo "  $0 marketplace ggen-cli"
    exit 1
fi

# Validate crate exists
if [ ! -d "crates/$CRATE" ]; then
    echo "❌ Crate not found: crates/$CRATE"
    exit 1
fi

# Validate module exists
MODULE_FILE="crates/$CRATE/src/${MODULE}.rs"
MODULE_DIR="crates/$CRATE/src/${MODULE}"

if [ ! -f "$MODULE_FILE" ] && [ ! -d "$MODULE_DIR" ]; then
    echo "❌ Module not found: $MODULE"
    echo "   Expected: $MODULE_FILE or $MODULE_DIR"
    exit 1
fi

echo "📝 Generating test skeleton for module: $MODULE"
echo "   Crate: $CRATE"
echo ""

# Create test directories
mkdir -p "crates/$CRATE/tests/unit"
mkdir -p "crates/$CRATE/tests/integration"
mkdir -p "crates/$CRATE/tests/property"
mkdir -p "crates/$CRATE/tests/security"
mkdir -p "crates/$CRATE/tests/common"

# Convert crate name to Rust module name (replace hyphens with underscores)
CRATE_MODULE="${CRATE//-/_}"

# Generate unit test skeleton
UNIT_TEST_FILE="crates/$CRATE/tests/unit/${MODULE}_tests.rs"
if [ -f "$UNIT_TEST_FILE" ]; then
    echo "⚠️  Unit test file already exists: $UNIT_TEST_FILE"
    echo "   Skipping unit test generation"
else
    cat > "$UNIT_TEST_FILE" <<EOF
//! Unit tests for $MODULE module
//!
//! Chicago TDD: Real objects, state verification, AAA pattern
//!
//! Coverage Target: ≥85% line coverage

use chicago_tdd_tools::prelude::*;

// Test config helpers for timeouts
#[path = "../common/mod.rs"]
mod test_config;
use test_config::{integration_timeout, unit_timeout};

// Import module under test
use ${CRATE_MODULE}::${MODULE}::*;

// Test helper utilities
mod helpers {
    use super::*;

    /// Create test input for ${MODULE}
    pub fn create_test_input() -> TestInput {
        TestInput::new("test_value")
    }

    /// Create invalid test input for error testing
    pub fn create_invalid_input() -> TestInput {
        TestInput::new("")
    }
}

// ============================================================================
// Unit Tests: Constructor and Creation
// ============================================================================

test!(test_${MODULE}_creation_with_valid_input_succeeds, {
    // Arrange
    let input = helpers::create_test_input();

    // Act
    let result = create(input);

    // Assert
    assert_ok!(result, "${MODULE} creation should succeed with valid input");
});

test!(test_${MODULE}_creation_with_invalid_input_fails, {
    // Arrange
    let input = helpers::create_invalid_input();

    // Act
    let result = create(input);

    // Assert
    assert_err!(result, "${MODULE} creation should fail with invalid input");
});

// ============================================================================
// Unit Tests: Basic Operations
// ============================================================================

test!(test_${MODULE}_operation_returns_expected_value, {
    // Arrange
    let instance = create(helpers::create_test_input()).unwrap();

    // Act
    let result = instance.operation();

    // Assert
    assert_eq!(result, expected_value(), "Operation should return expected value");
});

// ============================================================================
// Unit Tests: Async Operations
// ============================================================================

async_test!(test_${MODULE}_async_operation_completes, async {
    // Arrange
    let instance = create(helpers::create_test_input()).unwrap();

    // Act
    let result = instance.async_operation().await.unwrap();

    // Assert
    assert!(!result.is_empty(), "Async operation should return non-empty result");
});

// ============================================================================
// Unit Tests: Error Handling
// ============================================================================

test!(test_${MODULE}_error_propagation_is_correct, {
    // Arrange
    let instance = create(helpers::create_test_input()).unwrap();
    let invalid_param = "";

    // Act
    let result = instance.operation_with_param(invalid_param);

    // Assert
    assert_err!(result, "Should propagate error for invalid parameter");
});

// ============================================================================
// Unit Tests: Edge Cases
// ============================================================================

test!(test_${MODULE}_handles_empty_input, {
    // Arrange
    let input = TestInput::new("");

    // Act
    let result = create(input);

    // Assert
    // TODO: Define expected behavior for empty input
    assert_err!(result, "Should handle empty input gracefully");
});

test!(test_${MODULE}_handles_large_input, {
    // Arrange
    let large_input = "x".repeat(10000);
    let input = TestInput::new(&large_input);

    // Act
    let result = create(input);

    // Assert
    // TODO: Define expected behavior for large input
    assert_ok!(result, "Should handle large input without panic");
});

// ============================================================================
// TODO: Add More Tests
// ============================================================================

// TODO: Add tests for:
// - [ ] Concurrent operations (if applicable)
// - [ ] Resource cleanup (Drop implementation)
// - [ ] Cloning and copying (if applicable)
// - [ ] Serialization/deserialization (if applicable)
// - [ ] Custom traits implementation
// - [ ] Error recovery scenarios
// - [ ] Performance-critical paths
EOF
    echo "✅ Generated unit tests: $UNIT_TEST_FILE"
fi

# Generate integration test skeleton
INTEGRATION_TEST_FILE="crates/$CRATE/tests/integration/${MODULE}_integration.rs"
if [ -f "$INTEGRATION_TEST_FILE" ]; then
    echo "⚠️  Integration test file already exists: $INTEGRATION_TEST_FILE"
    echo "   Skipping integration test generation"
else
    cat > "$INTEGRATION_TEST_FILE" <<EOF
//! Integration tests for $MODULE module
//!
//! Chicago TDD: Real objects, full workflow, no mocks
//!
//! Coverage Target: ≥75% integration coverage

use chicago_tdd_tools::prelude::*;
use tempfile::TempDir;

// Test config helpers
#[path = "../common/mod.rs"]
mod test_config;
use test_config::integration_timeout;

// Import module under test
use ${CRATE_MODULE}::${MODULE}::*;

// Test helper utilities
mod helpers {
    use super::*;

    /// Create isolated test environment
    pub fn create_test_env() -> TempDir {
        TempDir::new().expect("Failed to create temp dir")
    }
}

// ============================================================================
// Integration Tests: Full Workflow
// ============================================================================

async_test!(test_${MODULE}_full_workflow_succeeds, async {
    // Arrange: Set up isolated test environment
    let temp_dir = helpers::create_test_env();
    let instance = create_with_env(&temp_dir).unwrap();

    // Act: Execute full workflow
    let result = instance.execute_workflow().await.unwrap();

    // Assert: Verify final state
    assert!(result.is_success(), "Full workflow should succeed");
    assert!(temp_dir.path().exists(), "Test environment should exist");
});

// ============================================================================
// Integration Tests: Cross-Module Interaction
// ============================================================================

async_test!(test_${MODULE}_integrates_with_dependent_modules, async {
    // Arrange: Set up modules
    let dependency = DependentModule::new().unwrap();
    let instance = create_with_dependency(dependency).unwrap();

    // Act: Execute cross-module operation
    let result = instance.cross_module_operation().await.unwrap();

    // Assert: Verify integration
    assert!(result.is_valid(), "Cross-module operation should succeed");
});

// ============================================================================
// Integration Tests: API Contract Validation
// ============================================================================

async_test!(test_${MODULE}_api_contract_is_stable, async {
    // Arrange: Create instance
    let instance = create_default().unwrap();

    // Act: Call all public API methods
    let _result1 = instance.public_method_1().await.unwrap();
    let _result2 = instance.public_method_2().await.unwrap();

    // Assert: API contract is stable (compiles)
    // TODO: Add specific assertions for API contract requirements
});

// ============================================================================
// Integration Tests: Concurrency Safety
// ============================================================================

async_test!(test_${MODULE}_concurrent_operations_are_safe, async {
    // Arrange: Create shared instance
    let instance = std::sync::Arc::new(create_default().unwrap());

    // Act: Execute concurrent operations
    let tasks: Vec<_> = (0..10)
        .map(|i| {
            let instance = instance.clone();
            tokio::spawn(async move {
                instance.concurrent_operation(i).await
            })
        })
        .collect();

    // Wait for all tasks
    let results = futures::future::join_all(tasks).await;

    // Assert: All operations succeeded
    for result in results {
        assert_ok!(result.unwrap(), "Concurrent operation should succeed");
    }
});

// ============================================================================
// TODO: Add More Integration Tests
// ============================================================================

// TODO: Add tests for:
// - [ ] Error recovery across modules
// - [ ] Resource sharing
// - [ ] Event propagation
// - [ ] State synchronization
// - [ ] Performance under load
// - [ ] Graceful degradation
EOF
    echo "✅ Generated integration tests: $INTEGRATION_TEST_FILE"
fi

# Generate property test skeleton
PROPERTY_TEST_FILE="crates/$CRATE/tests/property/${MODULE}_properties.rs"
if [ -f "$PROPERTY_TEST_FILE" ]; then
    echo "⚠️  Property test file already exists: $PROPERTY_TEST_FILE"
    echo "   Skipping property test generation"
else
    cat > "$PROPERTY_TEST_FILE" <<EOF
//! Property-based tests for $MODULE module
//!
//! Chicago TDD: Property testing for invariants
//!
//! Coverage Target: Critical properties verified

use proptest::prelude::*;

// Test config for property test cases
#[path = "../common/mod.rs"]
mod test_config;

// Import module under test
use ${CRATE_MODULE}::${MODULE}::*;

// ============================================================================
// Property Tests: Round-Trip Consistency
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(test_config::property_test_cases()))]

    #[test]
    fn property_${MODULE}_round_trip_consistency(
        input in "[a-zA-Z0-9_]+",
    ) {
        // Property: deserialize(serialize(x)) = x
        let original = create_from_string(&input).unwrap();
        let serialized = serialize(&original).unwrap();
        let deserialized = deserialize(&serialized).unwrap();

        prop_assert_eq!(original, deserialized);
    }
}

// ============================================================================
// Property Tests: Idempotency
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(test_config::property_test_cases()))]

    #[test]
    fn property_${MODULE}_operation_is_idempotent(
        input in "[a-zA-Z0-9_]+",
    ) {
        // Property: f(f(x)) = f(x)
        let instance = create_from_string(&input).unwrap();
        let once = instance.transform().unwrap();
        let twice = once.transform().unwrap();

        prop_assert_eq!(once, twice);
    }
}

// ============================================================================
// Property Tests: Invariants
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(test_config::property_test_cases()))]

    #[test]
    fn property_${MODULE}_invariant_holds(
        input in "[a-zA-Z0-9_]+",
    ) {
        // Property: Module invariant always holds
        let instance = create_from_string(&input).unwrap();

        // Define invariant
        prop_assert!(instance.invariant_check());
    }
}

// ============================================================================
// TODO: Add More Property Tests
// ============================================================================

// TODO: Add property tests for:
// - [ ] Commutativity (if applicable)
// - [ ] Associativity (if applicable)
// - [ ] Distributivity (if applicable)
// - [ ] Identity elements
// - [ ] Inverse operations
// - [ ] Monotonicity
EOF
    echo "✅ Generated property tests: $PROPERTY_TEST_FILE"
fi

# Generate common test config (if it doesn't exist)
COMMON_FILE="crates/$CRATE/tests/common/mod.rs"
if [ ! -f "$COMMON_FILE" ]; then
    cat > "$COMMON_FILE" <<'EOF'
//! Test configuration helpers
//!
//! Provides centralized test configuration from chicago-tdd-tools.toml

use std::time::Duration;

/// Get unit test timeout from config
pub fn unit_timeout() -> Duration {
    Duration::from_secs(chicago_tdd_tools::config::unit_timeout_seconds())
}

/// Get integration test timeout from config
pub fn integration_timeout() -> Duration {
    Duration::from_secs(chicago_tdd_tools::config::integration_timeout_seconds())
}

/// Get property test cases from config
pub fn property_test_cases() -> u32 {
    chicago_tdd_tools::config::property_test_cases()
}

/// Get HTTP connection timeout from config
pub fn http_connection_timeout() -> Duration {
    Duration::from_secs(chicago_tdd_tools::config::http_connection_timeout_seconds())
}

/// Get container wait timeout from config
pub fn container_wait_timeout() -> Duration {
    Duration::from_secs(chicago_tdd_tools::config::container_wait_timeout_seconds())
}
EOF
    echo "✅ Generated common test config: $COMMON_FILE"
fi

# Generate module test checklist
CHECKLIST_FILE="crates/$CRATE/tests/MODULE_TEST_CHECKLIST_${MODULE}.md"
if [ -f "$CHECKLIST_FILE" ]; then
    echo "⚠️  Test checklist already exists: $CHECKLIST_FILE"
    echo "   Skipping checklist generation"
else
    cp "docs/architecture/MODULE_TEST_CHECKLIST_TEMPLATE.md" "$CHECKLIST_FILE"

    # Replace placeholders
    sed -i '' "s/\[MODULE_NAME\]/${MODULE}/g" "$CHECKLIST_FILE"
    sed -i '' "s/\[crate\]/${CRATE}/g" "$CHECKLIST_FILE"
    sed -i '' "s/\[module\]/${MODULE}/g" "$CHECKLIST_FILE"
    sed -i '' "s/\[Date\]/$(date +%Y-%m-%d)/g" "$CHECKLIST_FILE"

    echo "✅ Generated test checklist: $CHECKLIST_FILE"
fi

echo ""
echo "🎉 Test skeleton generation complete!"
echo ""
echo "Generated files:"
echo "  1. Unit tests:        $UNIT_TEST_FILE"
echo "  2. Integration tests: $INTEGRATION_TEST_FILE"
echo "  3. Property tests:    $PROPERTY_TEST_FILE"
echo "  4. Test checklist:    $CHECKLIST_FILE"
echo ""
echo "Next steps:"
echo "  1. Implement test cases following Chicago TDD patterns"
echo "  2. Run tests: cargo test --package $CRATE --test ${MODULE}_tests"
echo "  3. Check coverage: cargo tarpaulin --package $CRATE"
echo "  4. Update checklist: $CHECKLIST_FILE"
echo ""
echo "Chicago TDD Reminders:"
echo "  ✅ Use test!() for synchronous tests"
echo "  ✅ Use async_test!() for async tests (auto-timeout from config)"
echo "  ✅ Follow AAA pattern (Arrange, Act, Assert)"
echo "  ✅ Use real objects (minimal mocking)"
echo "  ✅ Verify state, not behavior"
echo ""
EOF

    chmod +x "/Users/sac/ggen/scripts/generate-test-skeleton.sh"
    echo "✅ Created test skeleton generator script"
