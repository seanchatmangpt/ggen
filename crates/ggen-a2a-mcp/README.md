# ggen-a2a-mcp

MCP (Model Context Protocol) server for ggen, built on the official [rmcp 1.3.0](https://github.com/modelcontextprotocol/rust-sdk) Rust SDK.

Exposes ggen's code-generation capabilities as first-class MCP primitives: **Tools**, **Resources**, **Prompts**, and **Completions**.

---

## Quick Start

```bash
# Start as stdio MCP server (Claude Desktop / any MCP client)
ggen mcp start-server --transport stdio

# Start as HTTP server
ggen mcp start-server --transport http
```

**Claude Desktop config** (`~/.claude/claude_desktop_config.json`):
```json
{
  "mcpServers": {
    "ggen": {
      "command": "ggen",
      "args": ["mcp", "start-server", "--transport", "stdio"]
    }
  }
}
```

---

## Tools (9)

| Tool | Description | Key Parameters |
|------|-------------|----------------|
| `generate` | Run the μ₁-μ₅ pipeline on a .ttl file, write generated code | `ontology_path`, `queries_dir?`, `output_dir?`, `language?` |
| `sync` | Same as generate with `dry_run` support | `ontology_path`, `dry_run?`, `language?` |
| `validate` | Parse Turtle content, report triple count or errors | `ttl` |
| `list_generators` | List supported code generators | — |
| `list_examples` | Browse bundled example projects | `category?`, `limit?` |
| `get_example` | Get ggen.toml + TTL + README for an example | `name` |
| `search` | Search marketplace packages by keyword | `query`, `category?`, `limit?` |
| `scaffold_from_example` | Copy an example to a target directory | `example_name`, `target_dir` |
| `query_ontology` | Run SPARQL SELECT on inline TTL, return JSON rows | `ttl`, `sparql` |

### Supported generators

`go` · `rust` · `python` · `typescript` · `elixir` · `terraform` · `docker-kubernetes`

### Examples directory

`list_examples`, `get_example`, `scaffold_from_example` scan the `examples/` directory adjacent to the working directory, or the path set in `GGEN_EXAMPLES_DIR`.

---

## Resources

Resources expose bundled examples as browsable MCP resources with cursor-based pagination (20 per page).

| URI pattern | Content |
|-------------|---------|
| `ggen://example/{name}` | JSON summary: name, description, category, raw ggen.toml |
| `ggen://example/{name}/ttl` | Raw Turtle ontology content |
| `ggen://example/{name}/readme` | README.md content |
| `ggen://example/{name}/config` | Raw ggen.toml content |

---

## Prompts

Three domain-specific prompt templates for LLM-assisted ggen workflows:

### `explain-rdf-schema`
Explains a Turtle schema in plain English.

**Arguments:** `ttl_content` (required)

### `generate-from-example`
Adapts a bundled example to a new domain. Loads the real TTL + ggen.toml from the example.

**Arguments:** `example_name` (required), `target_domain` (required)

### `scaffold-project`
Designs a new ggen project (TTL + ggen.toml) from scratch.

**Arguments:** `domain` (required), `language` (optional, default: `auto`)

---

## Completions

Argument autocomplete for MCP clients that support it:

- `example_name` — returns matching example directory names from `examples/`
- `generator` / `language` — returns matching generator names

---

## Architecture

```
GgenMcpServer (rmcp 1.3.0)
├── #[tool_router] impl GgenMcpServer   — 9 tools
│   ├── generate / sync → ggen_core::sync::sync() via spawn_blocking
│   ├── validate       → oxigraph::io::RdfParser
│   ├── list_examples  → std::fs::read_dir(examples/)
│   ├── search         → ggen_domain::marketplace::search_packages()
│   └── query_ontology → oxigraph::sparql::SparqlEvaluator
└── #[tool_handler] impl ServerHandler
    ├── list_resources / read_resource → examples/ scan
    ├── list_prompts / get_prompt      → 3 prompt templates
    └── complete                       → example + generator completions
```

---

## Testing

```bash
cargo test -p ggen-a2a-mcp
```

15 Chicago TDD tests (AAA pattern, in-process `tokio::io::duplex` transport):
- Server name and capabilities
- All 9 tools (success + error paths)
- Resources, Prompts, Completions

---

## Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `GGEN_EXAMPLES_DIR` | `./examples` | Path to examples directory |

---

## See Also

- [`crates/ggen-core/src/sync/mod.rs`](../ggen-core/src/sync/mod.rs) — μ₁-μ₅ pipeline
- [`crates/ggen-domain/src/marketplace/search.rs`](../ggen-domain/src/marketplace/search.rs) — search backend
- [`docs/RMCP_NOTES.md`](../../docs/RMCP_NOTES.md) — rmcp 1.3.0 API facts and gotchas
- [Official Rust MCP SDK](https://github.com/modelcontextprotocol/rust-sdk)
