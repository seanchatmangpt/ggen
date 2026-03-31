# validate Tool

Validate a Turtle (.ttl) ontology file for syntax correctness.

## Parameters

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `ttl` | string | Yes | Turtle ontology content as string |

## Validation Checks

### Syntax Validation
- RDF/Turtle syntax correctness
- Prefix declarations
- Triple structure
- Literal encoding

### Schema Validation
- SHACL shapes (if present)
- OWL constraints
- Custom validation rules

### Statistics
- Triple count
- Unique subjects
- Unique predicates
- Unique objects

## Example Usage

### Validate File Content

```json
{
  "name": "validate",
  "arguments": {
    "ttl": "@prefix ex: <http://example.org/> .\n\nex:Subject ex:predicate ex:Object ."
  }
}
```

### Reading from File

```bash
# Read file and pass to validate
ttl_content=$(cat examples/hello-world/ontology.ttl)
# Pass $ttl_content to the tool
```

## Output

### Success Response

```json
{
  "valid": true,
  "triples": 234,
  "subjects": 45,
  "predicates": 12,
  "objects": 178,
  "namespaces": [
    "rdf:",
    "rdfs:",
    "xsd:",
    "ex:"
  ],
  "warnings": [
    {
      "line": 42,
      "message": "Unused prefix detected",
      "code": "UNUSED_PREFIX"
    }
  ]
}
```

### Error Response

```json
{
  "valid": false,
  "errors": [
    {
      "line": 15,
      "column": 8,
      "code": "INVALID_PREFIX",
      "message": "Prefix 'xyz:' not declared"
    }
  ]
}
```

## Error Codes

| Code | Description |
|------|-------------|
| `INVALID_PREFIX` | Prefix not declared |
| `MALFORMED_URI` | Invalid URI syntax |
| `INVALID_LITERAL` | Literal encoding error |
| `DUPLICATE_TRIPLE` | Same triple declared twice |
| `CYCLIC_IMPORT` | Circular dependency in imports |

## Best Practices

1. **Validate before generating**: Always validate ontologies before using `generate` tool
2. **Fix warnings**: Warnings don't block generation but may indicate issues
3. **Check imports**: Ensure all imported ontologies exist
4. **Use prefixes**: Declare prefixes at top of file

## Related Tools

- [`generate`](./generate.md) - Generate code after validation
- [`validate_sparql`](./validate_sparql.md) - Validate SPARQL queries
- [`query_ontology`](./query_ontology.md) - Query validated ontology
