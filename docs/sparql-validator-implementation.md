# SPARQL Validator Agent - Implementation Summary

## Overview

The `SPARQLValidatorAgent` is an autonomous A2A (Agent-to-Agent) component that validates and fixes SPARQL syntax errors using Oxigraph's parser. It automatically detects and corrects common syntax mistakes in SPARQL queries.

## Location

- **File**: `/Users/sac/ggen/crates/ggen-ai/src/swarm/agents/sparql_validator.rs`
- **Module**: `ggen_ai::swarm::agents::sparql_validator`
- **Tests**: `/Users/sac/ggen/crates/ggen-ai/tests/sparql_validator_agent_tests.rs`

## Architecture

### Core Components

#### 1. `SPARQLValidatorAgent` Struct

```rust
pub struct SPARQLValidatorAgent {
    dry_run: bool,      // Don't apply fixes when true
    verbose: bool,      // Enable detailed logging
}
```

**Key Methods:**

- `validate_and_fix(&self, query: &str) -> Result<ValidationReport>`
  - Main entry point for validation and fixing
  - Returns detailed report with errors and fixes applied

- `validate(&self, query: &str) -> Result<bool>`
  - Simple validation without fixing
  - Returns `Ok(true)` if query is valid

- `fix_syntax(&self, query: &str) -> String`
  - Fixes common syntax errors (periods, brackets, quotes)

- `fix_variables(&self, query: &str) -> String`
  - Fixes variable reference issues

- `restructure(&self, query: &str) -> String`
  - Restructures malformed queries (missing WHERE, etc.)

#### 2. `ValidationReport` Struct

```rust
pub struct ValidationReport {
    pub original_query: String,      // Input query
    pub fixed_query: String,         // Fixed version
    pub is_valid: bool,              // Validation status
    pub errors: Vec<SparqlError>,    // Errors found
    pub fixes_applied: Vec<SparqlFix>, // Fixes applied
    pub line: usize,                 // Error line number
    pub column: usize,               // Error column number
}
```

#### 3. `SparqlError` Struct

```rust
pub struct SparqlError {
    pub error_type: SparqlErrorType, // Type of error
    pub message: String,             // Error message
    pub line: usize,                 // Line number
    pub column: usize,               // Column number
    pub suggestion: Option<String>,  // Suggested fix
}
```

#### 4. Error Types

```rust
pub enum SparqlErrorType {
    SyntaxError,      // Syntax errors (periods, brackets, etc.)
    VariableError,    // Variable reference errors
    StructureError,   // Query structure errors
    PrefixError,      // PREFIX declaration errors
    Unknown,          // Unclassified errors
}
```

## Supported Fixes

### 1. Syntax Fixes (`fix_syntax`)

- **Missing periods**: Adds periods after triple patterns
  ```sparql
  # Before
  SELECT * WHERE { ?s ?p ?o }

  # After
  SELECT * WHERE { ?s ?p ?o . }
  ```

- **Unbalanced brackets**: Adds/removes brackets to balance
  ```sparql
  # Before
  SELECT * WHERE { ?s ?p [ ?o1 }

  # After
  SELECT * WHERE { ?s ?p [ ?o1 ] }
  ```

- **Unbalanced parentheses**: Balances parentheses in expressions
  ```sparql
  # Before
  FILTER(?o > 42

  # After
  FILTER(?o > 42)
  ```

- **PREFIX declarations**: Adds angle brackets to PREFIX URIs
  ```sparql
  # Before
  PREFIX rdf: http://www.w3.org/1999/02/22-rdf-syntax-ns#

  # After
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  ```

- **Missing quotes**: Adds quotes to string literals
  ```sparql
  # Before
  ?s rdfs:label hello

  # After
  ?s rdfs:label "hello"
  ```

### 2. Variable Fixes (`fix_variables`)

- **Orphan variables**: Warns about variables in SELECT not used in WHERE
- **Invalid variable names**: Ensures variables start with `?` or `$`

### 3. Structure Fixes (`restructure`)

- **Missing WHERE**: Adds WHERE clause to SELECT queries
  ```sparql
  # Before
  SELECT * { ?s ?p ?o }

  # After
  SELECT * WHERE { ?s ?p ?o }
  ```

- **FILTER expressions**: Adds parentheses around FILTER expressions
  ```sparql
  # Before
  FILTER ?o > 42

  # After
  FILTER (?o > 42)
  ```

- **OPTIONAL patterns**: Adds braces around OPTIONAL patterns
  ```sparql
  # Before
  OPTIONAL ?s ?q ?r

  # After
  OPTIONAL { ?s ?q ?r }
  ```

## Swarm Integration

The `SPARQLValidatorAgent` implements the `SwarmAgent` trait for integration with the Ultrathink swarm:

```rust
#[async_trait]
impl SwarmAgent for SPARQLValidatorAgent {
    fn name(&self) -> &str {
        "sparql_validator"
    }

    fn capabilities(&self) -> Vec<String> {
        vec![
            "sparql_validation".to_string(),
            "sparql_fixing".to_string(),
            "syntax_correction".to_string(),
            "variable_validation".to_string(),
            "query_restructuring".to_string(),
        ]
    }

    async fn execute(&self, context: &SwarmContext, input: AgentInput) -> Result<AgentOutput> {
        // Extract query from input and validate
    }

    async fn validate(&self) -> Result<bool> {
        // Validate Oxigraph parser is working
    }

    async fn health_check(&self) -> AgentHealth {
        // Return health status
    }
}
```

## Usage Examples

### Basic Validation

```rust
use ggen_ai::swarm::agents::sparql_validator::SPARQLValidatorAgent;

let agent = SPARQLValidatorAgent::new(false);
let query = "SELECT * WHERE { ?s ?p ?o }";

let report = agent.validate_and_fix(query)?;
if report.is_valid {
    println!("Query is valid!");
} else {
    println!("Errors: {:?}", report.errors);
}
```

### Fix Syntax Errors

```rust
let agent = SPARQLValidatorAgent::new(false);
let broken_query = "SELECT * WHERE { ?s ?p ?o }"; // Missing period

let fixed = agent.fix_syntax(broken_query);
println!("Fixed: {}", fixed);
// Output: SELECT * WHERE { ?s ?p ?o . }
```

### Dry Run Mode

```rust
let agent = SPARQLValidatorAgent::new(true); // Don't modify
let query = "SELECT * WHERE { ?s ?p ?o }";

let report = agent.validate_and_fix(query)?;
// report.fixed_query will show what would be fixed
// but original query won't be modified
```

### Verbose Logging

```rust
let agent = SPARQLValidatorAgent::new(false)
    .with_verbose(true);

let report = agent.validate_and_fix(query)?;
// Will log detailed information about fixes
```

## Test Coverage

The implementation includes **15 comprehensive tests** covering:

1. ✅ Valid query validation
2. ✅ Missing period fixes
3. ✅ Unbalanced bracket fixes
4. ✅ Unbalanced parenthesis fixes
5. ✅ PREFIX declaration fixes
6. ✅ Missing WHERE clause fixes
7. ✅ Validation without fixing
8. ✅ Error classification (syntax, variable, structure)
9. ✅ Dry run mode
10. ✅ Verbose mode
11. ✅ Complex query validation
12. ✅ Error line/column extraction
13. ✅ FILTER expression fixes
14. ✅ OPTIONAL pattern fixes
15. ✅ Orphan variable detection

## Integration with A2A Workflow

The agent fits into the A2A autonomous workflow as follows:

```
┌─────────────────┐
│  Event Trigger  │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ SPARQL Validator│ ◄── Validates SPARQL queries
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Code Generator │ ◄── Generates code from fixed queries
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│    Output       │
└─────────────────┘
```

## Performance Characteristics

- **Parsing**: Uses Oxigraph's native SPARQL parser (Rust-based, fast)
- **Fixing**: Regex-based pattern matching (efficient for common errors)
- **Memory**: Minimal allocation (works with string slices)
- **Concurrency**: Thread-safe (can run multiple agents in parallel)

## Error Handling

The agent provides detailed error information:

```rust
pub struct SparqlError {
    pub error_type: SparqlErrorType,  // Categorized error
    pub message: String,              // Human-readable message
    pub line: usize,                  // Exact line number
    pub column: usize,                // Exact column number
    pub suggestion: Option<String>,   // Fix suggestion
}
```

## Future Enhancements

Potential improvements for future versions:

1. **More sophisticated fixes**: Use LLM to suggest complex fixes
2. **Query optimization**: Restructure queries for better performance
3. **Style normalization**: Enforce consistent SPARQL style
4. **Validation rules**: Custom validation rules for specific ontologies
5. **Batch processing**: Validate multiple queries in parallel

## Dependencies

- `oxigraph` v0.5.6: SPARQL parser and query engine
- `regex`: Pattern matching for fixes
- `serde`/`serde_json`: Serialization for swarm communication
- `async-trait`: Async trait implementation
- `tracing`: Structured logging

## Conclusion

The `SPARQLValidatorAgent` provides autonomous SPARQL validation and fixing capabilities, essential for maintaining query quality in the ggen codebase. It integrates seamlessly with the Ultrathink swarm and follows Chicago TDD principles with comprehensive test coverage.

---

**Implementation Date**: 2026-03-30
**Status**: ✅ Complete and tested
**Priority**: P1 (Gap #5)
**File**: `/Users/sac/ggen/crates/ggen-ai/src/swarm/agents/sparql_validator.rs`
