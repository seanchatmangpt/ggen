# mcp-server-definition

A ggen example that defines an MCP (Model Context Protocol) server with tools in RDF/Turtle,
then generates a complete [rmcp](https://crates.io/crates/rmcp) 1.3.0 Rust server implementation.

## What this example demonstrates

- **RDF as server contract** — tools, parameters, and descriptions are declared in `ontology/mcp-server.ttl`
- **SPARQL → Tera → Rust** — a SELECT query extracts tool/parameter rows; a Tera template renders them into idiomatic rmcp source
- **Grouped generation** — the template uses `set_global` to track the current struct/tool and emit one block per unique name across SPARQL result rows
- **Two output files** — `generated/server.rs` (full Rust server) and `generated/Cargo.toml` (dependency fragment)

## Directory layout

```
mcp-server-definition/
  ontology/mcp-server.ttl          # Source of truth — servers, tools, params
  templates/server.rs.tera         # Tera template → rmcp Rust source
  templates/cargo-fragment.toml.tera  # Tera template → Cargo.toml fragment
  ggen.toml                        # ggen manifest with SPARQL queries
  generated/                       # Written by ggen sync (do not edit)
    server.rs
    Cargo.toml
```

## Quick start

```bash
cd examples/mcp-server-definition
ggen sync
```

That's it. `generated/server.rs` is ready to drop into a Rust binary crate.

## Generated MCP tools

| Tool | Struct | Description |
|------|--------|-------------|
| `sync_project` | `SyncProjectParams` | Run ggen sync pipeline on a project directory |
| `validate_ontology` | `ValidateOntologyParams` | Validate a Turtle (.ttl) ontology file |
| `list_examples` | `ListExamplesParams` | List available ggen example projects |
| `generate_preview` | `GeneratePreviewParams` | Preview generated output for a rule without writing files |

## The key pattern

```
ontology/mcp-server.ttl          (RDF — source of truth)
        │
        │  mcp:Server / mcp:Tool / mcp:Parameter triples
        ▼
ggen.toml SPARQL query           (μ₃ — query phase)
        │
        │  rows: serverStruct, toolName, structName, paramName, paramType …
        ▼
templates/server.rs.tera         (μ₄ — render phase)
        │
        │  set_global cur_struct / cur_tool for grouping
        ▼
generated/server.rs              (rmcp 1.3.0 Rust server)
```

To add a new tool, add `mcp:Tool` and `mcp:Parameter` triples to the TTL and re-run `ggen sync`.
No template changes needed.
