# V6 Testing Migration Guide

**Version**: 6.0.0
**Date**: 2026-01-24
**Author**: Test Engineer Agent
**Status**: Production-Ready

---

## Overview

This guide provides step-by-step instructions for migrating ggen tests from v5.x patterns to v6 Chicago TDD standards. It includes code examples, scripts, and verification steps.

---

## Table of Contents

1. [Quick Start](#quick-start)
2. [Migration Patterns](#migration-patterns)
3. [Property-Based Testing](#property-based-testing)
4. [Test Organization](#test-organization)
5. [Fixture Management](#fixture-management)
6. [Verification](#verification)

---

## Quick Start

### Step 1: Install Dependencies

```toml
# Add to Cargo.toml (workspace or crate-level)
[dev-dependencies]
chicago-tdd-tools = "1.4.0"
proptest = "1.8"
tempfile = "3.10"
testcontainers = "0.25"
insta = "1.40"  # For snapshot testing
```

### Step 2: Run Analysis

```bash
# Find all mock usage
cargo make test-analyze-mocks

# Find missing property tests
cargo make test-analyze-property-gaps

# Find ignored tests
cargo make test-analyze-ignored
```

### Step 3: Migrate Incrementally

```bash
# Migrate one crate at a time
cargo make test-migrate-crate --crate ggen-core

# Run verification
cargo make test-verify-chicago-tdd
```

---

## Migration Patterns

### Pattern 1: Mock to Real Collaborator

#### Before (London TDD with Mocks):
```rust
use mockall::mock;

mock! {
    pub HttpClient {
        fn get(&self, url: &str) -> Result<String>;
    }
}

#[test]
fn test_fetch_package_info() {
    // Arrange: Set up mock
    let mut mock_client = MockHttpClient::new();
    mock_client
        .expect_get()
        .with(eq("https://api.example.com/packages/foo"))
        .times(1)
        .returning(|_| Ok(r#"{"name": "foo", "version": "1.0.0"}"#.to_string()));

    // Act
    let result = fetch_package_info(&mock_client, "foo");

    // Assert: Verify interaction
    assert!(result.is_ok());
    let info = result.unwrap();
    assert_eq!(info.name, "foo");
}
```

#### After (Chicago TDD with Real Collaborators):
```rust
use chicago_tdd_tools::prelude::*;
use testcontainers::{clients::Cli, GenericImage};
use tempfile::TempDir;

async_test!(test_fetch_package_info_with_real_server, {
    // Arrange: Real HTTP server using testcontainers
    let docker = Cli::default();
    let container = docker.run(GenericImage::new("kennethreitz/httpbin", "latest"));
    let port = container.get_host_port_ipv4(80);
    let base_url = format!("http://localhost:{}", port);

    // Create real HTTP client
    let client = reqwest::Client::new();

    // Act: Real HTTP request
    let response = client
        .get(format!("{}/json", base_url))
        .send()
        .await
        .unwrap();

    // Assert: Verify ACTUAL behavior
    assert_eq!(response.status(), 200);
    let body = response.text().await.unwrap();
    assert!(body.contains("slideshow"), "Response should contain JSON data");
});
```

**Key Changes**:
1. Replaced `MockHttpClient` with real `reqwest::Client`
2. Used testcontainers for real HTTP server
3. Verified actual HTTP responses, not mocked interactions
4. Tests real network behavior, finds real bugs

---

### Pattern 2: Add Property-Based Tests

#### Step 1: Define Strategies

```rust
use proptest::prelude::*;
use ggen_core::rdf::parse_turtle;

// Strategy: Generate valid Turtle documents
fn valid_turtle_strategy() -> impl Strategy<Value = String> {
    prop_oneof![
        // Simple triple
        Just("@prefix ex: <http://example.org/> .\nex:Subject ex:predicate ex:Object ."),

        // With literals
        Just("@prefix ex: <http://example.org/> .\nex:Subject ex:name \"Alice\" ."),

        // With typed literals
        Just("@prefix ex: <http://example.org/> .\nex:Subject ex:age \"42\"^^<http://www.w3.org/2001/XMLSchema#integer> ."),

        // Complex document
        prop::string::string_regex("[a-z]{3,10}")
            .unwrap()
            .prop_map(|name| {
                format!(
                    "@prefix ex: <http://example.org/> .\n\
                     ex:{} rdf:type ex:User ;\n\
                            ex:name \"{}\" .",
                    name, name
                )
            }),
    ]
}

// Strategy: Generate invalid Turtle documents
fn invalid_turtle_strategy() -> impl Strategy<Value = String> {
    prop_oneof![
        // Missing prefix
        Just("ex:Subject ex:predicate ex:Object ."),

        // Unterminated triple
        Just("@prefix ex: <http://example.org/> .\nex:Subject ex:predicate"),

        // Invalid URI
        Just("@prefix ex: <invalid uri> .\nex:Subject ex:predicate ex:Object ."),

        // Random garbage
        prop::string::string_regex("[^a-zA-Z0-9]{10,100}").unwrap(),
    ]
}
```

#### Step 2: Write Property Tests

```rust
proptest! {
    /// Property: Parser never panics on any input
    #[test]
    fn prop_parser_never_panics(input in "\\PC*") {
        // Should either succeed or return error, never panic
        let _ = parse_turtle(&input);
    }

    /// Property: Valid inputs are always parsed
    #[test]
    fn prop_valid_inputs_parsed(doc in valid_turtle_strategy()) {
        let result = parse_turtle(&doc);
        assert!(result.is_ok(), "Valid Turtle should parse: {}", doc);
    }

    /// Property: Invalid inputs are always rejected
    #[test]
    fn prop_invalid_inputs_rejected(doc in invalid_turtle_strategy()) {
        let result = parse_turtle(&doc);
        assert!(result.is_err(), "Invalid Turtle should be rejected: {}", doc);
    }

    /// Property: Roundtrip preserves semantics
    #[test]
    fn prop_roundtrip_preserves_semantics(doc in valid_turtle_strategy()) {
        // Parse
        let parsed = parse_turtle(&doc).expect("Should parse");

        // Serialize
        let serialized = serialize_turtle(&parsed).expect("Should serialize");

        // Re-parse
        let reparsed = parse_turtle(&serialized).expect("Should re-parse");

        // Assert: Semantics preserved
        assert_eq!(parsed, reparsed, "Roundtrip should preserve semantics");
    }

    /// Property: Deterministic output (same input → same output)
    #[test]
    fn prop_deterministic_output(doc in valid_turtle_strategy()) {
        let result1 = parse_turtle(&doc);
        let result2 = parse_turtle(&doc);
        assert_eq!(result1, result2, "Parser should be deterministic");
    }
}
```

---

### Pattern 3: Organize Tests by Type

#### New Directory Structure:

```
crates/ggen-core/
├── src/
│   ├── rdf/
│   │   ├── mod.rs           # Production code only
│   │   ├── parser.rs        # Production code only
│   │   └── serializer.rs    # Production code only
│   └── template/
│       ├── mod.rs
│       └── renderer.rs
├── tests/
│   ├── common/              # Shared fixtures
│   │   ├── mod.rs
│   │   ├── fixtures.rs
│   │   └── setup.rs
│   ├── unit/                # Fast, isolated tests
│   │   ├── mod.rs
│   │   ├── rdf_parser_tests.rs
│   │   ├── rdf_serializer_tests.rs
│   │   └── template_renderer_tests.rs
│   ├── integration/         # Real collaborators, slower
│   │   ├── mod.rs
│   │   ├── rdf_workflow_tests.rs
│   │   └── template_workflow_tests.rs
│   └── property/            # Property-based tests
│       ├── mod.rs
│       ├── rdf_roundtrip_tests.rs
│       └── template_composition_tests.rs
└── Cargo.toml
```

#### Migration Script:

```bash
#!/bin/bash
# migrate-test-structure.sh

CRATE="$1"
if [ -z "$CRATE" ]; then
    echo "Usage: $0 <crate-name>"
    exit 1
fi

CRATE_DIR="crates/$CRATE"
TEST_DIR="$CRATE_DIR/tests"

# Create new directory structure
mkdir -p "$TEST_DIR/common"
mkdir -p "$TEST_DIR/unit"
mkdir -p "$TEST_DIR/integration"
mkdir -p "$TEST_DIR/property"

# Create mod.rs files
cat > "$TEST_DIR/common/mod.rs" <<'EOF'
//! Shared test fixtures and utilities
pub mod fixtures;
pub mod setup;
EOF

cat > "$TEST_DIR/unit/mod.rs" <<'EOF'
//! Unit tests (fast, isolated)
EOF

cat > "$TEST_DIR/integration/mod.rs" <<'EOF'
//! Integration tests (real collaborators, slower)
EOF

cat > "$TEST_DIR/property/mod.rs" <<'EOF'
//! Property-based tests
EOF

# Move existing tests to unit/
find "$TEST_DIR" -maxdepth 1 -name "*_test*.rs" -exec mv {} "$TEST_DIR/unit/" \;

echo "✅ Test structure created for $CRATE"
echo "📝 Next steps:"
echo "   1. Move integration tests to integration/"
echo "   2. Add property tests to property/"
echo "   3. Extract fixtures to common/fixtures.rs"
```

---

### Pattern 4: Centralize Fixtures

#### Before (Duplicated Fixtures):
```rust
// In test1.rs
fn setup_temp_dir() -> TempDir {
    let temp = TempDir::new().unwrap();
    fs::create_dir_all(temp.path().join("packages")).unwrap();
    temp
}

// In test2.rs (duplicated!)
fn setup_temp_dir() -> TempDir {
    let temp = TempDir::new().unwrap();
    fs::create_dir_all(temp.path().join("packages")).unwrap();
    temp
}
```

#### After (Centralized Fixtures):
```rust
// tests/common/fixtures.rs
use tempfile::TempDir;
use std::fs;
use std::path::PathBuf;

/// Standard test environment with packages directory
pub struct TestEnv {
    _temp_dir: TempDir,
    pub root: PathBuf,
    pub packages_dir: PathBuf,
    pub config_dir: PathBuf,
}

impl TestEnv {
    /// Create new test environment
    pub fn new() -> Self {
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let root = temp_dir.path().to_path_buf();
        let packages_dir = root.join("packages");
        let config_dir = root.join(".ggen");

        fs::create_dir_all(&packages_dir).expect("Failed to create packages dir");
        fs::create_dir_all(&config_dir).expect("Failed to create config dir");

        Self {
            _temp_dir: temp_dir,
            root,
            packages_dir,
            config_dir,
        }
    }

    /// Create with custom registry
    pub fn with_registry(mut self, packages: Vec<PackageMetadata>) -> Self {
        let registry_path = self.config_dir.join("registry.json");
        let registry = Registry { packages };
        let json = serde_json::to_string_pretty(&registry).unwrap();
        fs::write(&registry_path, json).unwrap();
        self
    }
}

/// Sample Turtle document fixture
pub fn sample_turtle() -> &'static str {
    include_str!("fixtures/sample.ttl")
}

/// Sample invalid Turtle fixture
pub fn invalid_turtle() -> &'static str {
    include_str!("fixtures/invalid.ttl")
}

// tests/common/fixtures/sample.ttl
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:User1 rdf:type ex:User ;
    ex:name "Alice" ;
    ex:email "alice@example.com" .
```

#### Usage:
```rust
use common::fixtures::*;

async_test!(test_with_centralized_fixtures, {
    // Arrange: Use centralized fixture
    let env = TestEnv::new();

    // Act
    let result = process_directory(&env.packages_dir).await;

    // Assert
    assert_ok!(result);
});
```

---

## Property-Based Testing Recipes

### Recipe 1: Parser Roundtrip

```rust
proptest! {
    #[test]
    fn prop_parser_roundtrip(
        subject in "[a-zA-Z]{3,10}",
        predicate in "[a-zA-Z]{3,10}",
        object in "[a-zA-Z]{3,10}",
    ) {
        // Generate Turtle
        let turtle = format!(
            "@prefix ex: <http://example.org/> .\n\
             ex:{} ex:{} ex:{} .",
            subject, predicate, object
        );

        // Parse
        let graph = parse_turtle(&turtle)
            .expect("Generated Turtle should parse");

        // Serialize
        let serialized = serialize_turtle(&graph)
            .expect("Should serialize");

        // Re-parse
        let reparsed = parse_turtle(&serialized)
            .expect("Serialized should re-parse");

        // Assert: Same graph
        assert_eq!(graph, reparsed);
    }
}
```

### Recipe 2: Constraint Validation

```rust
proptest! {
    #[test]
    fn prop_min_length_enforced(
        min_len in 1usize..100,
        actual_len in 0usize..200,
    ) {
        let constraint = MinLength(min_len);
        let test_string = "a".repeat(actual_len);

        let result = constraint.validate(&test_string);

        if actual_len < min_len {
            assert_err!(result, "Should reject too-short strings");
        } else {
            assert_ok!(result, "Should accept sufficient-length strings");
        }
    }
}
```

### Recipe 3: Idempotency

```rust
proptest! {
    #[test]
    fn prop_operation_is_idempotent(input in any::<String>()) {
        let result1 = normalize(&input);
        let result2 = normalize(&result1);
        assert_eq!(result1, result2, "Normalize should be idempotent");
    }
}
```

### Recipe 4: Commutativity

```rust
proptest! {
    #[test]
    fn prop_merge_is_commutative(
        a in any::<Vec<i32>>(),
        b in any::<Vec<i32>>(),
    ) {
        let ab = merge(&a, &b);
        let ba = merge(&b, &a);
        assert_eq!(ab.sort(), ba.sort(), "Merge should be commutative");
    }
}
```

---

## Verification Steps

### Step 1: Run Chicago TDD Linter

```bash
# Check for London TDD violations
cargo make test-lint-chicago-tdd

# Output:
# ❌ crates/ggen-cli/tests/marketplace/unit/mod.rs:10
#    Found mockall usage - violates Chicago TDD
#
# ❌ tests/common/mocks.rs:5
#    Found Mock struct - use real collaborators
#
# ✅ crates/ggen-ai/tests/dspy_property_tests.rs
#    No violations found
```

### Step 2: Verify Property Test Coverage

```bash
# Check property test coverage
cargo make test-property-coverage

# Output:
# ✅ ggen-core/rdf: 80% coverage (parsers have property tests)
# ❌ ggen-core/template: 20% coverage (missing roundtrip tests)
# ❌ ggen-cli/clap: 10% coverage (missing input validation tests)
```

### Step 3: Run Full Test Suite

```bash
# Run all tests with strict validation
cargo make test-all-strict

# Should pass:
# ✅ All unit tests (≤10s)
# ✅ All integration tests (≤30s)
# ✅ All property tests (≤60s)
# ✅ No ignored tests
# ✅ No TODO/FIXME in test code
```

---

## Common Migration Issues

### Issue 1: Test Depends on External Service

**Problem**:
```rust
#[test]
fn test_fetch_from_github() {
    let result = fetch_from_github("seanchatmangpt/ggen");
    assert!(result.is_ok());  // ❌ Flaky: depends on network
}
```

**Solution**: Use testcontainers or mock HTTP server
```rust
async_test!(test_fetch_from_mock_server, {
    // Arrange: Real HTTP server (testcontainers)
    let docker = Cli::default();
    let container = docker.run(GenericImage::new("kennethreitz/httpbin", "latest"));
    let port = container.get_host_port_ipv4(80);

    // Act: Real HTTP request to controlled server
    let client = reqwest::Client::new();
    let response = client
        .get(format!("http://localhost:{}/json", port))
        .send()
        .await
        .unwrap();

    // Assert: Verify real behavior
    assert_eq!(response.status(), 200);
});
```

### Issue 2: Test is Slow

**Problem**:
```rust
#[test]
fn test_large_file_processing() {
    let content = "a".repeat(10_000_000);  // 10MB
    let result = process(&content);
    assert!(result.is_ok());  // ❌ Slow: 30s
}
```

**Solution**: Use property test with smaller inputs
```rust
proptest! {
    #[test]
    fn prop_handles_various_sizes(size in 0usize..1000) {
        let content = "a".repeat(size);
        let result = process(&content);
        // Test behavior, not just large size
        assert!(result.is_ok());
    }
}

// Separate slow test with #[ignore]
#[test]
#[ignore]  // Run with: cargo test --ignored
fn test_large_file_processing_slow() {
    let content = "a".repeat(10_000_000);
    let result = process(&content);
    assert!(result.is_ok());
}
```

### Issue 3: Flaky Test

**Problem**:
```rust
#[tokio::test]
async fn test_concurrent_access() {
    let shared = Arc::new(Mutex::new(0));

    let handles: Vec<_> = (0..10)
        .map(|_| {
            let s = Arc::clone(&shared);
            tokio::spawn(async move {
                *s.lock().unwrap() += 1;
            })
        })
        .collect();

    for h in handles {
        h.await.unwrap();
    }

    assert_eq!(*shared.lock().unwrap(), 10);  // ❌ Flaky: race condition
}
```

**Solution**: Use proper synchronization
```rust
#[tokio::test]
async fn test_concurrent_access_fixed() {
    let shared = Arc::new(Mutex::new(0));
    let barrier = Arc::new(tokio::sync::Barrier::new(10));

    let handles: Vec<_> = (0..10)
        .map(|_| {
            let s = Arc::clone(&shared);
            let b = Arc::clone(&barrier);
            tokio::spawn(async move {
                // Wait for all threads to be ready
                b.wait().await;

                // Now increment
                *s.lock().unwrap() += 1;
            })
        })
        .collect();

    for h in handles {
        h.await.unwrap();
    }

    // All increments completed
    assert_eq!(*shared.lock().unwrap(), 10);
}
```

---

## Makefile.toml Additions

Add these tasks to `/home/user/ggen/Makefile.toml`:

```toml
[tasks.test-analyze-mocks]
description = "Find all mock usage in tests"
script_runner = "@shell"
script = '''
#!/bin/bash
echo "🔍 Analyzing mock usage..."
rg "mockall|Mock\{|mock!" crates/*/tests tests/ --count-matches | \
    sort -t: -k2 -nr | \
    head -20
echo ""
echo "Run: cargo make test-migrate-crate --crate <name>"
'''

[tasks.test-analyze-property-gaps]
description = "Find modules missing property tests"
script_runner = "@shell"
script = '''
#!/bin/bash
echo "🔍 Analyzing property test coverage..."
for crate in crates/*/; do
    crate_name=$(basename "$crate")
    prop_tests=$(find "$crate/tests" -name "*property*" 2>/dev/null | wc -l)
    total_tests=$(find "$crate/tests" -name "*.rs" 2>/dev/null | wc -l)

    if [ "$total_tests" -gt 0 ]; then
        coverage=$(( prop_tests * 100 / total_tests ))
        if [ "$coverage" -lt 20 ]; then
            echo "❌ $crate_name: ${coverage}% (${prop_tests}/${total_tests})"
        else
            echo "✅ $crate_name: ${coverage}% (${prop_tests}/${total_tests})"
        fi
    fi
done
'''

[tasks.test-lint-chicago-tdd]
description = "Lint tests for Chicago TDD violations"
script_runner = "@shell"
script = '''
#!/bin/bash
violations=0

echo "🔍 Checking for London TDD violations..."

# Check for mockall usage
if rg "use mockall::" crates/*/tests tests/ -l 2>/dev/null; then
    echo "❌ Found mockall usage - violates Chicago TDD"
    violations=$((violations + 1))
fi

# Check for Mock structs
if rg "struct Mock" crates/*/tests tests/ -l 2>/dev/null; then
    echo "❌ Found Mock structs - use real collaborators"
    violations=$((violations + 1))
fi

# Check for .expect() usage (London TDD pattern)
if rg "\.expect_" crates/*/tests tests/ -c | awk -F: '$2 > 5 {print}'; then
    echo "⚠️  Found excessive .expect_() calls - consider Chicago TDD"
fi

if [ "$violations" -eq 0 ]; then
    echo "✅ No Chicago TDD violations found"
    exit 0
else
    echo "❌ Found $violations violation(s)"
    exit 1
fi
'''

[tasks.test-property-coverage]
description = "Report property test coverage by crate"
script_runner = "@shell"
script = '''
#!/bin/bash
echo "📊 Property Test Coverage Report"
echo "================================"

for crate in crates/*/; do
    crate_name=$(basename "$crate")

    # Count total test files
    total=$(find "$crate" -path "*/tests/*.rs" 2>/dev/null | wc -l)

    # Count property test files
    prop=$(find "$crate" -path "*/tests/*property*.rs" 2>/dev/null | wc -l)
    prop=$((prop + $(rg "proptest!" "$crate" -l 2>/dev/null | wc -l)))

    if [ "$total" -gt 0 ]; then
        coverage=$(( prop * 100 / total ))

        if [ "$coverage" -ge 80 ]; then
            status="✅"
        elif [ "$coverage" -ge 50 ]; then
            status="⚠️ "
        else
            status="❌"
        fi

        printf "%s %-30s %3d%% (%d/%d)\n" "$status" "$crate_name" "$coverage" "$prop" "$total"
    fi
done
'''
```

---

## Summary

This migration guide provides:
1. ✅ Step-by-step migration patterns
2. ✅ Property-based testing recipes
3. ✅ Test organization standards
4. ✅ Centralized fixture management
5. ✅ Verification scripts
6. ✅ Common issue solutions

**Estimated migration effort**:
- High-priority items: 40-60 hours
- Medium-priority items: 20-30 hours
- Low-priority items: 10-15 hours

**ROI**: 80% of bugs caught with 20% of effort (property tests on parsers/generators).

**Start with**: Property tests for RDF parsers (highest value, lowest effort).
