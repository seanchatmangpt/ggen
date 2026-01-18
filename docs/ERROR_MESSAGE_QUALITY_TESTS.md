# Error Message Quality Tests Documentation

## Overview

This document describes the comprehensive error message quality test suite created to ensure all error paths in ggen produce actionable, user-friendly messages.

## Test Location

- **File**: `tests/london_tdd/cli_commands/error_message_quality_comprehensive_test.rs`
- **Feature Flag**: `london-tdd`
- **Test Framework**: Rust unit tests with custom quality assertions
- **Total Tests**: 25 comprehensive error message quality tests

## Test Coverage

### 1. CLI Argument Parsing Errors (4 tests)

#### Test: `test_cli_missing_required_argument_error_quality`
- **Purpose**: Verify errors when required CLI arguments are missing
- **Validates**:
  - Contains "required" context
  - Suggests "try" as fix guidance
  - Clearly indicates user error
  - Has consistent formatting

#### Test: `test_cli_invalid_argument_value_error_quality`
- **Purpose**: Verify errors for invalid argument values
- **Validates**:
  - Contains "invalid" context
  - Suggests valid alternatives
  - Clearly indicates user error
  - Provides examples
  - Has consistent formatting

#### Test: `test_cli_unknown_command_error_quality`
- **Purpose**: Verify errors for unknown commands with suggestions
- **Validates**:
  - Contains "unknown" context
  - Suggests "did you mean" alternatives
  - Clearly indicates user error
  - Provides examples
  - Has consistent formatting

#### Test: `test_cli_conflicting_arguments_error_quality`
- **Purpose**: Verify errors when arguments conflict
- **Validates**:
  - Contains "conflict" context
  - Suggests which arguments to use
  - Clearly indicates user error
  - Has consistent formatting

### 2. File I/O Errors (5 tests)

#### Test: `test_file_not_found_error_quality`
- **Purpose**: Verify file not found errors
- **Validates**:
  - Contains "not found" context
  - Includes file path
  - Suggests to check path
  - Clearly indicates user error
  - Has consistent formatting

#### Test: `test_file_permission_denied_error_quality`
- **Purpose**: Verify permission denied errors
- **Validates**:
  - Contains "permission" context
  - Includes file path
  - Suggests chmod command
  - Clearly indicates system error
  - Has consistent formatting

#### Test: `test_file_invalid_format_error_quality`
- **Purpose**: Verify invalid file format errors
- **Validates**:
  - Contains "invalid" context
  - Includes file path
  - Clearly indicates user error
  - Has consistent formatting
  - Includes line number when available

#### Test: `test_directory_not_empty_error_quality`
- **Purpose**: Verify directory not empty errors
- **Validates**:
  - Contains "not empty" context
  - Includes file path
  - Suggests to remove or choose different location
  - Clearly indicates user error
  - Has consistent formatting

#### Test: `test_disk_full_error_quality`
- **Purpose**: Verify disk full errors
- **Validates**:
  - Contains "space" context
  - Suggests to free up disk space
  - Clearly indicates system error
  - Has consistent formatting

### 3. Ontology Validation Errors (4 tests)

#### Test: `test_ontology_invalid_syntax_error_quality`
- **Purpose**: Verify RDF syntax errors
- **Validates**:
  - Contains "syntax" context
  - Includes file path
  - Clearly indicates user error
  - Has consistent formatting
  - Includes line number

#### Test: `test_ontology_invalid_prefix_error_quality`
- **Purpose**: Verify missing/invalid prefix errors
- **Validates**:
  - Contains "prefix" context
  - Includes file path
  - Suggests @prefix syntax
  - Clearly indicates user error
  - Provides examples
  - Has consistent formatting

#### Test: `test_ontology_shacl_validation_error_quality`
- **Purpose**: Verify SHACL validation failures
- **Validates**:
  - Contains "validation" context
  - Suggests to add missing properties
  - Clearly indicates user error
  - Has consistent formatting

#### Test: `test_ontology_constraint_violation_error_quality`
- **Purpose**: Verify constraint violations
- **Validates**:
  - Contains "constraint" context
  - Suggests expected type
  - Clearly indicates user error
  - Provides examples
  - Has consistent formatting

### 4. SPARQL Errors (4 tests)

#### Test: `test_sparql_syntax_error_quality`
- **Purpose**: Verify SPARQL syntax errors
- **Validates**:
  - Contains "syntax" context
  - Suggests to check syntax
  - Clearly indicates user error
  - Provides examples
  - Has consistent formatting

#### Test: `test_sparql_undefined_prefix_error_quality`
- **Purpose**: Verify undefined prefix in SPARQL
- **Validates**:
  - Contains "prefix" context
  - Suggests PREFIX declaration
  - Clearly indicates user error
  - Provides examples
  - Has consistent formatting

#### Test: `test_sparql_query_timeout_error_quality`
- **Purpose**: Verify query timeout errors
- **Validates**:
  - Contains "timeout" context
  - Suggests to simplify query
  - Clearly indicates system error
  - Has consistent formatting

#### Test: `test_sparql_no_results_warning_quality`
- **Purpose**: Verify empty result set warnings
- **Validates**:
  - Indicates no results clearly
  - Provides informative guidance

### 5. Template Rendering Errors (6 tests)

#### Test: `test_template_missing_variable_error_quality`
- **Purpose**: Verify missing required variable errors
- **Validates**:
  - Contains "variable" context
  - Includes file path
  - Suggests how to provide variable
  - Clearly indicates user error
  - Provides examples
  - Has consistent formatting

#### Test: `test_template_syntax_error_quality`
- **Purpose**: Verify template syntax errors
- **Validates**:
  - Contains "syntax" context
  - Includes file path
  - Suggests to close tags
  - Clearly indicates user error
  - Provides examples
  - Has consistent formatting
  - Includes line number

#### Test: `test_template_invalid_filter_error_quality`
- **Purpose**: Verify invalid filter usage
- **Validates**:
  - Contains "filter" context
  - Suggests available filters
  - Clearly indicates user error
  - Provides examples
  - Has consistent formatting

#### Test: `test_template_type_mismatch_error_quality`
- **Purpose**: Verify type mismatch errors
- **Validates**:
  - Contains "type" context
  - Suggests expected type
  - Clearly indicates user error
  - Has consistent formatting

#### Test: `test_template_not_found_with_suggestions_error_quality`
- **Purpose**: Verify template not found with did-you-mean
- **Validates**:
  - Contains "not found" context
  - Includes file path
  - Suggests "did you mean" alternatives
  - Clearly indicates user error
  - Provides examples
  - Has consistent formatting

#### Test: `test_template_frontmatter_parse_error_quality`
- **Purpose**: Verify frontmatter parsing errors
- **Validates**:
  - Contains "frontmatter" context
  - Includes file path
  - Clearly indicates user error
  - Has consistent formatting
  - Includes line number

### 6. Cross-Cutting Quality Tests (3 tests)

#### Test: `test_error_chaining_preserves_context`
- **Purpose**: Verify error chains preserve all context
- **Validates**:
  - All error levels present in chain
  - File paths preserved
  - Consistent formatting

#### Test: `test_error_includes_actionable_next_steps`
- **Purpose**: Verify all errors provide actionable guidance
- **Validates**:
  - Contains actionable keywords (try, use, run, check, etc.)
  - Applies to all error types

#### Test: `test_error_formatting_consistency`
- **Purpose**: Verify consistent formatting across all errors
- **Validates**:
  - All errors follow same format rules
  - Consistent capitalization
  - Proper structure

#### Test: `test_user_vs_system_error_distinction`
- **Purpose**: Verify clear distinction between error types
- **Validates**:
  - User errors clearly identifiable
  - System errors clearly identifiable
  - Appropriate indicators for each type

## Error Quality Assertion Framework

The test suite includes a custom `ErrorQualityAssertion` helper that validates:

### 1. Context Validation
- **Method**: `has_context(expected_context: &str)`
- **Purpose**: Ensures error explains what failed
- **Example**: Error must contain "not found", "invalid", etc.

### 2. Fix Suggestions
- **Method**: `has_fix_suggestion(expected_fix: &str)`
- **Purpose**: Ensures error provides actionable fix
- **Keywords**: try, use, run, check, install, provide, etc.

### 3. File Path Inclusion
- **Method**: `has_file_path()`
- **Purpose**: Ensures error references specific files
- **Detects**: /, \\, .ttl, .toml, .yaml, .json, .tmpl

### 4. User Error Classification
- **Method**: `is_user_error()`
- **Purpose**: Clearly identifies fixable user errors
- **Indicators**: not found, invalid, missing, required, expected

### 5. System Error Classification
- **Method**: `is_system_error()`
- **Purpose**: Clearly identifies system-level errors
- **Indicators**: permission denied, network, timeout, unavailable

### 6. Formatting Consistency
- **Method**: `has_consistent_formatting()`
- **Purpose**: Ensures professional error formatting
- **Rules**: Starts with uppercase, proper structure

### 7. Example Inclusion
- **Method**: `has_example()`
- **Purpose**: Ensures error provides helpful examples
- **Detects**: "example", "e.g.", code snippets in quotes/backticks

## Error Simulation Helpers

The test suite includes comprehensive error simulation functions:

### CLI Errors
- `simulate_cli_missing_arg(command, subcommand)` - Missing required arguments
- `simulate_cli_invalid_value(arg, value, valid)` - Invalid argument values
- `simulate_cli_unknown_command(cmd, suggestions)` - Unknown commands with suggestions
- `simulate_cli_conflicting_args(arg1, arg2)` - Conflicting arguments

### File I/O Errors
- `simulate_file_not_found(path)` - File not found
- `simulate_permission_denied(path)` - Permission denied
- `simulate_invalid_file_format(file, details)` - Invalid format
- `simulate_directory_not_empty(path)` - Directory not empty
- `simulate_disk_full(path)` - Disk full

### Ontology Errors
- `simulate_rdf_syntax_error(file, line, details)` - RDF syntax errors
- `simulate_invalid_prefix(prefix, file)` - Invalid prefixes
- `simulate_shacl_validation_error(shape, constraint, details)` - SHACL validation
- `simulate_constraint_violation(property, expected, actual, class)` - Constraint violations

### SPARQL Errors
- `simulate_sparql_syntax_error(query, details)` - SPARQL syntax errors
- `simulate_sparql_undefined_prefix(prefix, query)` - Undefined prefixes
- `simulate_sparql_timeout(query_desc, timeout_secs)` - Query timeouts
- `simulate_sparql_empty_results(query)` - Empty result sets

### Template Errors
- `simulate_template_missing_variable(var, template)` - Missing variables
- `simulate_template_syntax_error(file, line, details)` - Template syntax errors
- `simulate_template_invalid_filter(filter, usage)` - Invalid filters
- `simulate_template_type_mismatch(var, expected, actual, details)` - Type mismatches
- `simulate_template_not_found(template, suggestions)` - Template not found with suggestions
- `simulate_frontmatter_parse_error(file, details, line)` - Frontmatter parsing errors

## Running the Tests

```bash
# Run all error message quality tests
cargo test --test london_tdd_main --features london-tdd error_message_quality_comprehensive_test

# Run specific test
cargo test --test london_tdd_main --features london-tdd test_cli_missing_required_argument_error_quality

# Run all error quality tests with output
cargo test --test london_tdd_main --features london-tdd error_message_quality_comprehensive_test -- --nocapture
```

## Quality Standards Enforced

All error messages must meet these standards:

1. **Context**: Clear explanation of what failed
2. **Fix Suggestions**: Actionable guidance on how to fix
3. **File Paths/Line Numbers**: Specific location references when applicable
4. **Error Type Distinction**: Clear user vs system error classification
5. **Consistent Formatting**: Professional, consistent structure
6. **Helpful Examples**: Concrete examples when beneficial

## Integration with Existing Tests

This test suite complements existing error tests:

- **enhanced_errors_test.rs**: Tests template-not-found with suggestions
- **error_handling.rs**: E2E error scenarios with real CLI
- **error_scenarios.rs**: Marketplace-specific errors
- **error_handling_tests.rs**: Security and validation errors

## Benefits

1. **Comprehensive Coverage**: Tests all major error paths
2. **Quality Assurance**: Ensures error messages help users
3. **Regression Prevention**: Detects error message degradation
4. **Documentation**: Serves as examples of good error messages
5. **Maintainability**: Centralized error quality standards

## Future Enhancements

Potential improvements:

1. Integration with actual error types from codebase
2. Automated error message documentation generation
3. Error message localization testing
4. Error message accessibility testing
5. Performance benchmarking for error generation
