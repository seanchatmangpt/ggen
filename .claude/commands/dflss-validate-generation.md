# Validate Template Generation

Validate that template generation produces correct and deterministic outputs.

## Commands

### Validate Templates
```bash
cargo make validate-templates
```

This command:
- Validates template frontmatter (YAML)
- Checks template syntax
- Verifies template security (no arbitrary code execution)
- Ensures templates follow ggen standards

### Generate and Validate Output
```bash
# Generate code from template
cargo run --bin ggen -- template generate <template-path> --output <output-dir>

# Validate generated output
cargo make validate-templates
```

### Check Template Frontmatter

Templates must include:
- `to` - Output file path
- `vars` - Template variables
- `rdf` - RDF graph source (optional)
- `sparql` - SPARQL query (optional)
- `determinism` - Determinism guarantees

### Verify Generated Outputs

Generated code should:
- Match expected structure
- Be deterministic (same inputs → same output)
- Pass snapshot tests (if using `insta`)
- Compile successfully
- Follow project conventions

## Testing

### Snapshot Testing
```rust
#[test]
fn test_template_generation() {
    let template = Template::load("templates/example.tmpl")?;
    let vars = extract_vars(&graph)?;
    let output = template.render(&vars)?;
    insta::assert_snapshot!(output);
}
```

### Deterministic Validation
```bash
# Run deterministic tests
cargo make deterministic

# Run single-threaded tests
cargo make test-single-threaded
```

## Checklist

- [ ] Template frontmatter is valid YAML
- [ ] Template variables are properly extracted
- [ ] Generated output matches expectations
- [ ] Output is deterministic (same inputs → same output)
- [ ] Generated code compiles
- [ ] Snapshot tests pass (if applicable)
- [ ] Template security validated (no arbitrary code execution)

