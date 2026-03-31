# generate Tool

Generate code from an RDF ontology file via the μ₁-μ₅ pipeline.

## Parameters

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `ontology_path` | string | Yes | Path to .ttl ontology file |
| `queries_dir` | string | No | Path to SPARQL queries directory |
| `output_dir` | string | No | Output directory (default: ./generated) |
| `language` | string | No | Target language (default: rust) |

## Pipeline Stages

The generate tool executes the 5-stage μ pipeline:

### μ₁: Load
- Parse Turtle (.ttl) ontology
- Load into Oxigraph triplestore
- Validate RDF syntax

### μ₂: Extract
- Identify skill definitions
- Extract templates and patterns
- Build generation context

### μ₃: Generate
- Apply code generation rules
- Generate source files
- Run LLM-assisted completion

### μ₄: Validate
- Compile generated code
- Run tests
- Check quality gates

### μ₅: Emit
- Write files to output directory
- Generate receipt
- Log metrics

## Example Usage

### Basic Generation

```json
{
  "name": "generate",
  "arguments": {
    "ontology_path": "examples/hello-world/ontology.ttl",
    "output_dir": "./generated/hello"
  }
}
```

### With SPARQL Queries

```json
{
  "name": "generate",
  "arguments": {
    "ontology_path": "examples/hello-world/ontology.ttl",
    "queries_dir": "examples/hello-world/queries/",
    "output_dir": "./generated/hello"
  }
}
```

### Target Language

```json
{
  "name": "generate",
  "arguments": {
    "ontology_path": "examples/hello-world/ontology.ttl",
    "language": "python",
    "output_dir": "./generated/hello-python"
  }
}
```

## Supported Languages

| Language | Generator | Status |
|----------|-----------|--------|
| rust | rust | Stable |
| go | go | Stable |
| python | python | Stable |
| typescript | typescript | Stable |
| elixir | elixir | Experimental |
| terraform | terraform | Experimental |
| docker-kubernetes | k8s | Experimental |

## Output

### Success Response

```json
{
  "files_generated": 12,
  "triples_processed": 234,
  "duration_ms": 1523,
  "receipt": {
    "input_hash": "abc123...",
    "output_hash": "def456...",
    "signature": "sig789..."
  },
  "files": [
    "generated/hello/src/main.rs",
    "generated/hello/src/lib.rs",
    "generated/hello/Cargo.toml"
  ]
}
```

### Error Response

```json
{
  "error": {
    "code": "ONTOLOGY_PARSE_ERROR",
    "message": "Failed to parse TTL at line 42",
    "details": {
      "file": "examples/hello-world/ontology.ttl",
      "line": 42,
      "column": 15
    }
  }
}
```

## Common Errors

| Error | Cause | Solution |
|-------|-------|----------|
| `ONTOLOGY_NOT_FOUND` | File path incorrect | Check ontology_path exists |
| `INVALID_TTL_SYNTAX` | Malformed Turtle | Validate with `validate` tool first |
| `TEMPLATE_NOT_FOUND` | Missing template | Check queries_dir path |
| `COMPILATION_FAILED` | Generated code invalid | Check language generator compatibility |

## Related Tools

- [`validate`](./validate.md) - Validate ontology before generation
- [`sync`](./sync.md) - Full sync with audit trail
- [`validate_pipeline`](./validate_pipeline.md) - Run quality gates

## See Also

- [Ontology Design Guide](../../03-tutorials/02-ontology-design.md)
- [Code Generation Architecture](../../05-architecture/components/mcp-server.md)
