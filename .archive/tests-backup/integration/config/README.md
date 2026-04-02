# ggen.toml Configuration Test Suite

This directory contains comprehensive tests for the `ggen.toml` configuration system.

## Test Structure (80/20 Principle)

### Integration Tests (Current Directory)
- `config_integration_test.rs` - End-to-end config loading and validation

### Unit Tests (In crates/ggen-config/tests/ - to be created by CODER)
- `parser_tests.rs` - TOML parsing, error handling, edge cases
- `validator_tests.rs` - Schema validation, required fields, constraints
- `resolver_tests.rs` - Workspace dependency resolution
- `workspace_tests.rs` - Multi-package workspace features

### Test Fixtures (tests/fixtures/config/)
- `simple.ggen.toml` - Minimal valid configuration
- `workspace.ggen.toml` - Multi-package workspace
- `advanced.ggen.toml` - All features enabled
- `member_package.ggen.toml` - Workspace member config
- `invalid_*.ggen.toml` - Error case fixtures

## Coverage Focus (80% of Value)

### ✅ Critical Paths Tested
1. Basic config file loading and parsing
2. Workspace resolution and member inheritance
3. Schema validation and error messages
4. All feature sections (AI, marketplace, templates, graph, etc.)
5. Environment variable resolution
6. Version constraint parsing
7. Serialization roundtrip

### ⏭️ Low Priority (Skipped for 80/20)
- Obscure TOML edge cases
- Performance optimization edge cases
- Rare error combinations
- Legacy format migrations

## Running Tests

```bash
# Run all config tests
cargo test --test config_integration_test

# Run specific test
cargo test test_simple_config_loading

# Run with output
cargo test -- --nocapture
```

## Test Quality Metrics

- Target: 100% pass rate
- Fast execution: <2 seconds total
- Clear test names describing behavior
- Comprehensive error message validation
- Focus on user-facing functionality

## Coordination with Other Agents

- **CODER**: Implements `ggen-config` crate with the schema
- **ANALYST**: Validates test coverage metrics
- **REVIEWER**: Ensures test quality and maintainability

## Implementation Notes

Tests are currently stubbed with TODO comments. Once CODER implements:
1. `crates/ggen-config/src/lib.rs` - Main config structures
2. `crates/ggen-config/src/parser.rs` - TOML parsing
3. `crates/ggen-config/src/validator.rs` - Schema validation
4. `crates/ggen-config/src/resolver.rs` - Workspace resolution

Then uncomment test assertions and run full test suite.
