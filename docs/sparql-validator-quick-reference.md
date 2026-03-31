# SPARQL Validator Agent - Quick Reference

## Import

```rust
use ggen_ai::swarm::agents::sparql_validator::SPARQLValidatorAgent;
```

## Basic Usage

### Create Agent

```rust
// Normal mode (will apply fixes)
let agent = SPARQLValidatorAgent::new(false);

// Dry run mode (won't modify)
let agent = SPARQLValidatorAgent::new(true);

// With verbose logging
let agent = SPARQLValidatorAgent::new(false).with_verbose(true);
```

### Validate and Fix

```rust
let query = "SELECT * WHERE { ?s ?p ?o }";
let report = agent.validate_and_fix(query)?;

if report.is_valid {
    println!("✓ Valid query");
    println!("Fixed: {}", report.fixed_query);
} else {
    println!("✗ Invalid query");
    for error in report.errors {
        println!("  - Line {}: {}", error.line, error.message);
    }
}
```

### Quick Validation

```rust
let query = "SELECT * WHERE { ?s ?p ?o }";
if agent.validate(query)? {
    println!("Query is valid!");
}
```

### Apply Specific Fixes

```rust
// Fix syntax errors (periods, brackets, quotes)
let fixed = agent.fix_syntax(query);

// Fix variable references
let fixed = agent.fix_variables(query);

// Restructure malformed queries
let fixed = agent.restructure(query);
```

## Common Fixes

| Fix Type | Example |
|----------|---------|
| Missing periods | `?s ?p ?o` → `?s ?p ?o .` |
| Unbalanced brackets | `[ ?o1` → `[ ?o1 ]` |
| Unbalanced parens | `FILTER(?o > 42` → `FILTER(?o > 42)` |
| PREFIX URIs | `PREFIX x: http://...` → `PREFIX x: <http://...>` |
| Missing WHERE | `SELECT * { }` → `SELECT * WHERE { }` |
| FILTER parens | `FILTER ?x > 0` → `FILTER (?x > 0)` |
| OPTIONAL braces | `OPTIONAL ?s ?q ?r` → `OPTIONAL { ?s ?q ?r }` |

## Error Types

| Type | Description |
|------|-------------|
| `SyntaxError` | Missing periods, brackets, quotes |
| `VariableError` | Undefined variables, invalid names |
| `StructureError` | Missing WHERE, malformed FILTER/OPTIONAL |
| `PrefixError` | Invalid PREFIX declarations |
| `Unknown` | Unclassified errors |

## Swarm Integration

### Capabilities

- `sparql_validation` - Validate SPARQL syntax
- `sparql_fixing` - Fix common errors
- `syntax_correction` - Correct syntax mistakes
- `variable_validation` - Check variable references
- `query_restructuring` - Restructure malformed queries

### Input Format

```json
{
  "query": "SELECT * WHERE { ?s ?p ?o }"
}
```

### Output Format

```json
{
  "is_valid": true,
  "original_query": "...",
  "fixed_query": "...",
  "errors": [],
  "fixes_applied": [],
  "line": 0,
  "column": 0
}
```

## Testing

Run tests:

```bash
cargo test --manifest-path=crates/ggen-ai/Cargo.toml --lib sparql_validator
```

Run integration tests:

```bash
cargo test --manifest-path=crates/ggen-ai/Cargo.toml --test sparql_validator_agent_tests
```

## Files

- **Implementation**: `crates/ggen-ai/src/swarm/agents/sparql_validator.rs`
- **Tests**: `crates/ggen-ai/tests/sparql_validator_agent_tests.rs`
- **Documentation**: `docs/sparql-validator-implementation.md`

## Example Queries

### Valid Query

```sparql
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT ?s ?p ?o
WHERE {
  ?s a rdf:Resource .
  ?s ?p ?o .
  FILTER(?o > 42)
}
LIMIT 10
```

### Query with Errors

```sparql
# Missing periods, unbalanced brackets
PREFIX rdf: http://www.w3.org/1999/02/22-rdf-syntax-ns#
SELECT ?s ?p ?o WHERE {
  ?s a rdf:Resource
  ?s ?p [ ?o1
}
```

### After Fixing

```sparql
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT ?s ?p ?o WHERE {
  ?s a rdf:Resource .
  ?s ?p [ ?o1 ] .
}
```

## Tips

1. **Use dry run first**: Test with `dry_run=true` to see what would be fixed
2. **Enable verbose logging**: Set `verbose=true` to see detailed fix information
3. **Check error types**: Use `error_type` to categorize and prioritize fixes
4. **Validate files**: Read `.rq` files and pass content to validator
5. **Batch processing**: Process multiple queries in parallel for efficiency

## Limitations

- Only fixes common syntax errors (not semantic issues)
- May not fix all complex malformed queries
- Doesn't optimize query performance
- Requires valid SPARQL grammar structure

## Future Work

- LLM-assisted complex fixes
- Query optimization suggestions
- Custom validation rules
- Performance analysis
- Style normalization
