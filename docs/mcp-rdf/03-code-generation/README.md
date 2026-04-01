# Code Generation Guide

How the ggen MCP server generation pipeline works internally.

## Pipeline Overview

The generation pipeline follows the formula **A = μ(O)** — Code precipitates from RDF through five stages:

```
RDF Ontology (.ttl) → μ₁ → μ₂ → μ₃ → μ₄ → μ₅ → Generated Rust Code
```

| Stage | Name | Input | Output | Description |
|-------|------|-------|--------|-------------|
| μ₁ | Load | `.ttl` file | Oxigraph `Store` | Parse RDF into triplestore |
| μ₂ | Extract | Triplestore | JSON context | SPARQL CONSTRUCT query |
| μ₃ | Render | JSON context | Rust code | Tera templates |
| μ₄ | Validate | Rust code | Compilation result | `cargo check` |
| μ₅ | Emit | Files | Receipt + files | Write + cryptographic hash |

---

## μ₁: Load Ontology

**Input:** RDF/Turtle file (`.ttl`)
**Output:** In-memory Oxigraph triplestore
**Duration:** ~10-50ms depending on ontology size

```bash
μ₁: Loading ontology... 156 triples loaded in 12ms
```

**What happens:**
1. File is parsed using Oxigraph's Turtle parser
2. Triples are loaded into an in-memory store
3. Base URI resolution and prefix expansion occur
4. Validation ensures well-formed RDF

**Error handling:**
- **Syntax errors** - Parser reports line/column
- **Invalid URIs** - Fails with IRI error
- **Duplicate triples** - Idempotent (no error)

---

## μ₂: Extract Context

**Input:** Oxigraph triplestore
**Output:** Normalized JSON context
**Duration:** ~20-100ms

```bash
μ₂: Extracting context... 9 tools, 3 resources, 2 prompts in 34ms
```

**What happens:**

The SPARQL CONSTRUCT query (`extract-mcp-context.rq`) transforms raw RDF into a normalized graph with predictable URI structure:

```
ctx:server                    -- Singleton server metadata
ctx:server/capabilities       -- Boolean capability flags
ctx:server/transport          -- Transport type and config
ctx:server/logging            -- Logging policy
ctx:tool/{name}               -- One node per tool
ctx:tool/{name}/arg/{param}   -- One node per tool argument
ctx:resource/{uri}            -- One node per resource
ctx:resource-tpl/{name}       -- One node per resource template
ctx:prompt/{name}             -- One node per prompt
ctx:prompt/{name}/arg/{arg}   -- One node per prompt argument
ctx:completion/{type}/{name}/{arg}  -- One node per completion
```

**Example CONSTRUCT output:**

```sparql
CONSTRUCT {
  ctx:server a mcp:McpServer ;
    mcp:serverName ?server_name ;
    mcp:serverVersion ?server_version ;
    mcp:hasCapabilitySet ctx:server/capabilities .

  ctx:server/capabilities mcp:tools ?has_tools ;
    mcp:resources ?has_resources .

  ctx:tool/{tool_name} a mcp:Tool ;
    mcp:name ?tool_name ;
    mcp:description ?tool_desc .
}
WHERE {
  ?server a mcp:McpServer ;
    mcp:serverName ?server_name ;
    mcp:hasCapabilitySet/?mcp:tools ?has_tools .
  # ... more triples
}
```

**Normalization features:**
- **COALESCE** provides defaults for missing optional fields
- **BNODE labeling** ensures deterministic node IDs
- **URI encoding** handles special characters in names
- **Type coercion** ensures consistent literals

**See:** [SPARQL Guide](../05-sparql-guide/) for query patterns.

---

## μ₃: Render Templates

**Input:** JSON context
**Output:** Generated Rust code (multiple files)
**Duration:** ~50-200ms

```bash
μ₃: Rendering templates... 7 files generated in 87ms
```

**What happens:**

The JSON context is serialized and passed to 9 Tera templates:

| Template | Generates | Context Variables |
|----------|-----------|-------------------|
| `server.rs.tera` | Server struct, lifecycle | `server_name`, `version`, `transport` |
| `get_info.rs.tera` | `ServerHandler::get_info()` | `server_name`, `capabilities` |
| `tools.rs.tera` | Tool router + handlers | `tools` array |
| `resources.rs.tera` | Resource handlers | `resources` array |
| `resource_templates.rs.tera` | Template handlers | `resource_templates` array |
| `prompts.rs.tera` | Prompt handlers | `prompts` array |
| `completions.rs.tera` | Completion handler | `completions` array |
| `logging.rs.tera` | Logging handlers | `logging` object |
| `main.rs.tera` | Tokio bootstrap | `server_name`, `transport` |

**Template features:**

**Filters:**
- `| pascal` - Convert to PascalCase (`my_tool` → `MyTool`)
- `| snake` - Convert to snake_case (`MyTool` → `my_tool`)
- `| upper` - Uppercase
- `| lower` - Lowercase

**Conditionals:**
```tera
{% if tools %}
// Tool code here
{% else %}
// Empty stub
{% endif %}
```

**Loops:**
```tera
{% for tool in tools %}
#[tool(description = "{{ tool.description }}")]
async fn {{ tool.name|snake }}(
    &self,
    {% for arg in tool.arguments %}
    {{ arg.name }}: {% if arg.required %}{{ arg.type }}{% else %}Option<{{ arg.type }}>{% endif %},
    {% endfor %}
) -> Result<CallToolResult, McpError> {
    // Handler implementation
}
{% endfor %}
```

**Generated code quality:**
- ✅ OTEL spans with semantic conventions
- ✅ JSON Schema validation via `schemars`
- ✅ Proper `Result<T, E>` error handling
- ✅ Serde serializable parameter structs
- ✅ Async/await with Tokio

**See:** [Template Customization Guide](../04-template-customization/) for modifying templates.

---

## μ₄: Validate (Compile Gate)

**Input:** Generated Rust code
**Output:** Compilation result
**Duration:** ~1-5s

```bash
μ₄: Compile gate... ✅ PASSED (cargo check in 2.3s)
```

**What happens:**

1. Generated files are written to a temporary directory
2. `cargo check --quiet -p ggen-a2a-mcp` is executed
3. Compilation errors are captured and reported
4. On success, the gate passes

**Skip the compile gate:**
```bash
ggen mcp generate --ontology server.ttl --output ./src --skip-compile-gate
```

**Why the compile gate?**
- Catches template syntax errors early
- Ensures generated code is valid Rust
- Prevents broken code from being emitted
- Provides fast feedback loop

**Error handling:**
```
❌ COMPILE FAILED

error[E0433]: failed to resolve: use of undeclared crate `rmcp`
  --> src/tools.rs:12:5
   |
12 |     rmcp::tool
   |     ^^^^^ use of undeclared crate `rmcp`

Fix: Add rmcp = "1.3.0" to Cargo.toml dependencies
```

---

## μ₅: Emit Files and Receipt

**Input:** Generated code, validation result
**Output:** Files written + cryptographic receipt
**Duration:** ~10-50ms

```bash
μ₅: Emitting files... receipt: a3f7c9e2b8d1

Generated files:
  - src/server.rs (187 lines)
  - src/tools.rs (453 lines)
  - src/resources.rs (128 lines)
  - src/prompts.rs (89 lines)
  - src/completions.rs (67 lines)
  - src/logging.rs (134 lines)
  - src/main.rs (45 lines)
```

**What happens:**

1. Files are written to the output directory
2. Receipt is generated:
   ```
   Receipt {
     hash: "a3f7c9e2b8d1",
     timestamp: 2026-03-31T23:20:00Z,
     source: "server.ttl",
     triple_count: 156,
     files_generated: 7
   }
   ```
3. Receipt is signed with Ed25519 private key
4. `.receipt.json` file is written alongside generated code

**Receipt verification:**

```rust
use ggen_receipt::verify_receipt;

let receipt = verify_receipt("server.ttl", ".receipt.json")?;
assert_eq!(receipt.hash, "a3f7c9e2b8d1");
```

**Receipt purposes:**
- **Provenance** - Trace code to source ontology
- **Reproducibility** - Re-generate identical code
- **Audit** - Track when code was generated
- **Dependency tracking** - Ensure ontology hasn't changed

---

## Pipeline Internals

### Context Structure

The JSON context passed to templates:

```json
{
  "server": {
    "name": "ggen-mcp",
    "version": "6.0.1",
    "description": "ggen MCP server",
    "protocol_version": "2025-11-25"
  },
  "capabilities": {
    "tools": true,
    "resources": true,
    "prompts": true
  },
  "transport": {
    "type": "stdio",
    "host": null,
    "port": null
  },
  "tools": [
    {
      "name": "sync",
      "description": "Execute full pipeline",
      "arguments": [
        {
          "name": "ontology_path",
          "type": "string",
          "required": true
        }
      ]
    }
  ],
  "resources": [],
  "prompts": [],
  "completions": [],
  "logging": {
    "default_level": "info"
  }
}
```

### Template Rendering

```rust
use tera::{Tera, Context};
use serde_json::Value;

fn render_template(template_name: &str, context: &Value) -> Result<String> {
    let tera = Tera::new("templates/**/*.tera")?;
    let mut ctx = Context::new();
    ctx.extend(context.as_object().unwrap());
    Ok(tera.render(template_name, &ctx)?)
}
```

### Error Handling

Each stage returns `Result<T, PipelineError>`:

```rust
pub enum PipelineError {
    LoadError(String),
    ExtractionError(String),
    RenderError(String),
    CompileError(String),
    EmitError(String),
}
```

---

## Performance

| Ontology Size | Triples | μ₁ | μ₂ | μ₃ | μ₄ | μ₅ | Total |
|---------------|---------|----|----|----|----|----|-------|
| Minimal | 10 | 5ms | 8ms | 12ms | 1.2s | 3ms | **1.2s** |
| Small | 50 | 8ms | 15ms | 34ms | 1.8s | 5ms | **1.9s** |
| Medium | 200 | 15ms | 42ms | 87ms | 2.3s | 8ms | **2.5s** |
| Large | 500 | 28ms | 98ms | 180ms | 3.1s | 12ms | **3.4s** |

*Bottleneck is μ₄ (compile gate). Skip with `--skip-compile-gate` for faster iteration.*

---

## Debugging

### Enable trace logging

```bash
RUST_LOG=trace,ggen_core=trace ggen mcp generate \
  --ontology server.ttl \
  --output ./src
```

### Inspect extracted context

The `--dry-run` flag shows JSON context without generating files:

```bash
ggen mcp generate \
  --ontology server.ttl \
  --output ./src \
  --dry-run
```

### View SPARQL results

```bash
# Run CONSTRUCT query manually
robin query \
  --file crates/ggen-core/queries/mcp/extract-mcp-context.rq \
  server.ttl
```

---

## See Also

- [RDF Schema Reference](../02-rdf-schema/) - Ontology classes
- [Template Customization](../04-template-customization/) - Modify templates
- [SPARQL Guide](../05-sparql-guide/) - Query extraction
