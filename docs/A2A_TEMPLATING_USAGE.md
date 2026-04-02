# A2A Templating Usage Guide

## Overview

ggen provides instant code generation for A2A (agent-to-agent) skills across 5 programming languages: Rust, Go, Elixir, Java, and TypeScript. This is achieved through a powerful pipeline:

```
RDF Ontology (.ttl) → SPARQL-star CONSTRUCT (.rq) → Tera Template (.tera) → Generated Code
```

## Quick Start

### 1. Define Your Schema

Use the compact schema syntax in your Turtle ontology:

```turtle
:FileReadSkill a a2a:Skill ;
    a2a:skillName "file.read" ;
    a2a:description "Read file contents" ;
    a2a:inputType "FileReadRequest { path: string, offset?: integer, limit?: integer }"^^xsd:string ;
    a2a:outputType "FileReadResponse { contents: string, size: integer }"^^xsd:string ;
    .
```

### 2. Use in Tera Templates

```jinja2
// Rust
{% for skill in skills %}
{{ skill.input_type | schema_to_rust }}
{% endfor %}

// Go
{% for skill in skills %}
{{ skill.input_type | schema_to_go }}
{% endfor %}

// Elixir
{% for skill in skills %}
{{ skill.input_type | schema_to_elixir }}
{% endfor %}

// Java
{% for skill in skills %}
{{ skill.input_type | schema_to_java }}
{% endfor %}

// TypeScript
{% for skill in skills %}
{{ skill.input_type | schema_to_typescript }}
{% endfor %}
```

### 3. Generated Code Examples

#### Input Schema:
```
FileReadRequest { path: string, offset?: integer, limit?: integer }
```

#### Generated Rust:
```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileReadRequest {
    pub path: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub offset: Option<i64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub limit: Option<i64>,
}
```

#### Generated Go:
```go
type FileReadRequest struct {
    Path   string `json:"path"`
    Offset *int64 `json:"offset,omitempty"`
    Limit  *int64 `json:"limit,omitempty"`
}
```

#### Generated Elixir:
```elixir
defmodule FileReadRequest do
  @moduledoc false
  defstruct [:path, :offset, :limit]
end
```

#### Generated Java:
```java
import com.fasterxml.jackson.annotation.*;

public class FileReadRequest {
    private String path;
    
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Long offset;
    
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Long limit;
}
```

#### Generated TypeScript:
```typescript
export interface FileReadRequest {
    path: string;
    offset?: number;
    limit?: number;
}
```

## Schema Syntax

### Basic Types

- `string` - String/text values
- `integer` - Integer numbers (i64, int64, Long, number)
- `float` - Floating-point numbers (f64, float64, Double, number)
- `boolean` - Boolean values (bool, bool, Boolean, boolean)

### Optional Fields

Suffix field names with `?` to make them optional:

```
Request { required: string, optional?: integer }
```

### Array Types

Suffix types with `[]` to create arrays:

```
Request { items: string[], count?: integer[] }
```

### Complete Example

```
ProcessMap {
    nodes: Node[],
    edges: Edge[],
    name: string,
    timeout?: integer,
    debug?: boolean
}
```

## Tera Filters Reference

### Available Filters

All filters take a schema string and return generated code:

- `schema_to_rust` - Generate Rust struct
- `schema_to_go` - Generate Go struct
- `schema_to_elixir` - Generate Elixir module
- `schema_to_java` - Generate Java class
- `schema_to_typescript` - Generate TypeScript interface

### Filter Chaining

You can chain filters with other Tera transformations:

```jinja2
{{ skill.name | pascal }}  // "file.read" → "FileRead"
{{ skill.input_type | schema_to_rust }}
```

## Migration Guide: From Untyped to Typed

### Before (Untyped):
```turtle
:FileReadSkill a a2a:Skill ;
    a2a:input "JSON object with path field" ;
    a2a:output "JSON object with contents field" ;
    .
```

### After (Typed):
```turtle
:FileReadSkill a a2a:Skill ;
    a2a:inputType "FileReadRequest { path: string }"^^xsd:string ;
    a2a:outputType "FileReadResponse { contents: string }"^^xsd:string ;
    .
```

## Best Practices

1. **Use PascalCase for type names**: `FileReadRequest`, not `fileReadRequest`
2. **Make large fields optional**: Binary data, large arrays should use `?`
3. **Use snake_case for field names**: `file_path`, not `filePath` (converts automatically in Go/Rust)
4. **Prefer specific types**: Use `integer` instead of `float` for counts
5. **Document complex schemas**: Add `@moduledoc` or Javadoc comments in post-processing

## Error Handling

### Invalid Schema Syntax:
```
Error: Failed to parse schema: Missing closing brace '}'
```

### Unknown Type:
```
Error: Failed to parse schema: Unknown type: 'bigint'
```

### Solution:
Check schema syntax matches: `TypeName { field: type, optional?: type[] }`

## Testing

Test your schema generation:

```bash
# Run schema parser tests
cargo test schema_parser -p ggen-core

# Run full integration test
cargo test schema -p ggen-core
```

## Advanced Usage

### Nested Types (Future):
Currently, object types are represented as `Any` or generic map types. Future versions will support nested schema definitions.

### Custom Type Mapping:
The generators can be extended to support custom type mappings for your specific domain types.

## Behavior Predicates for LLM-Driven Implementation

### Overview

ggen supports behavior predicates that enable LLMs to automatically generate skill implementations from RDF ontologies. These predicates provide rich context for code generation while maintaining semantic rigor.

### Core Behavior Predicates

#### a2a:hasSystemPrompt
Natural language description of skill behavior for LLM generation.

```turtle
:FileReadSkill a a2a:Skill ;
    a2a:hasSystemPrompt """
Read file contents from the filesystem at the specified path.
Supports optional offset (start position) and limit (max bytes) for partial reads.
Returns file contents as string along with file size and original path.
Handle errors for missing files, permission denied, and invalid paths.
""" ;
```

#### a2a:hasImplementationHint
Code snippets or algorithm descriptions for implementation guidance.

```turtle
:FileReadSkill a a2a:Skill ;
    a2a:hasImplementationHint """
Use std::fs::read_to_string for full file reads.
For offset/limit reading, use std::fs::File with std::io::Seek and std::io::Read.
Return proper error types for filesystem errors.
""" ;
```

#### a2a:hasTestExample
Example input/output pairs for validation testing.

```turtle
:FileReadSkill a a2a:Skill ;
    a2a:hasTestExample """
Input: { path: "/tmp/test.txt", offset: 0, limit: 100 }
Output: { contents: "Hello World", size: 11, path: "/tmp/test.txt" }

Input: { path: "/nonexistent.txt" }
Output: Error: File not found
""" ;
```

### MCP-Specific Predicates

#### mcp:hasAutoImplementation
Flag indicating LLM should generate implementation automatically.

```turtle
:DatabaseQueryTool a mcp:Tool ;
    mcp:hasAutoImplementation true ;
    mcp:hasImplementationLanguage "rust" ;
    mcp:hasToolCategory "database" .
```

### Complete Example

```turtle
@prefix a2a: <https://a2a.dev/ontology#> .
@prefix mcp: <https://ggen.io/ontology/mcp#> .

:FileReadSkill a a2a:Skill ;
    a2a:hasName "file_read" ;
    a2a:hasDescription "Read file contents at given path" ;
    a2a:hasInputType "FileReadRequest { path: string, offset?: integer, limit?: integer }"^^xsd:string ;
    a2a:hasOutputType "FileReadResponse { contents: string, size: integer }"^^xsd:string ;

    # Behavior Predicates
    a2a:hasSystemPrompt """
Read file contents from the filesystem at the specified path.
Supports optional offset (start position) and limit (max bytes) for partial reads.
""" ;

    a2a:hasImplementationHint """
Use std::fs::read_to_string for full file reads.
For offset/limit reading, use std::fs::File with Seek and Read traits.
""" ;

    a2a:hasTestExample """
Input: { path: "/tmp/test.txt", offset: 0, limit: 100 }
Output: { contents: "Hello World", size: 11 }
""" ;

    a2a:hasDependency """
std >= 1.0.0
""" ;

    a2a:hasValidationRule """
- Path must be non-empty string
- Offset must be >= 0 if specified
- Limit must be > 0 if specified
""" .
```

### Advanced Predicates

#### Error Handling
```turtle
a2a:hasErrorHandling """
- FileNotFoundError: Path does not exist
- PermissionError: Insufficient permissions
- InvalidPathError: Path contains invalid characters
""" .
```

#### Performance Hints
```turtle
a2a:hasPerformanceHint """
For large files (>1MB), use buffered reading with BufReader.
Consider memory constraints when limit is not specified.
""" .
```

#### Dependencies
```turtle
a2a:hasDependency """
sqlx = { version = "0.7", features = ["postgres", "json"] }
tokio = { version = "1", features = ["full"] }
serde_json = "1.0"
""" .
```

#### Validation Rules
```turtle
a2a:hasValidationRule """
- URL must be valid HTTP/HTTPS URL
- Method must be one of: GET, POST, PUT, DELETE, PATCH
- Headers must be object if provided
""" .
```

### Composite Skills

Skills can be composed of multiple sub-skills:

```turtle
:FileProcessingSkill a a2a:Skill ;
    a2a:hasName "file_processing" ;
    a2a:composedOf :FileReadSkill, :DataTransformSkill, :FileWriteSkill ;

    a2a:hasSystemPrompt """
Process files by reading, transforming, and writing.
Chain sub-skills in order: read → transform → write.
""" .
```

### Best Practices

1. **Be Specific in System Prompts**: Provide clear context about behavior, inputs, outputs, and edge cases
2. **Include Concrete Examples**: Test examples should cover success cases and error scenarios
3. **Specify Dependencies**: List all required crates/packages with version constraints
4. **Add Validation Rules**: Explicitly state input validation requirements
5. **Document Error Handling**: Describe error scenarios and recovery strategies
6. **Provide Performance Guidance**: Include hints for optimization and resource management

### Files Reference

- **Predicate Definitions**: `.specify/specs/014-a2a-integration/behavior-predicates.ttl`
- **Usage Examples**: `.specify/specs/014-a2a-integration/behavior-example.ttl`
- **A2A Ontology**: `.specify/specs/014-a2a-integration/a2a-ontology.ttl`

## See Also

- [Tera Template Documentation](https://tera.netlify.app/)
- [A2A Specification](https://github.com/your-org/a2a-spec)
- [ggen README](../README.md)
- [Behavior Predicates Spec](../.specify/specs/014-a2a-integration/behavior-predicates.ttl)

