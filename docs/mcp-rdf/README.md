# ggen RDF-Driven MCP Server Generation

**Generate spec-pinned MCP 2025-11-25 servers from RDF ontologies.**

## Overview

ggen's MCP server generation system enables you to define Model Context Protocol (MCP) servers declaratively using RDF/Turtle ontologies. The system generates complete, production-ready Rust code using the RMCP 1.3.0 SDK.

**The formula: A = μ(O)** — Code precipitates from RDF through a five-stage pipeline.

## Quick Start

```bash
# 1. Define your MCP server in RDF/Turtle
cat > my-server.ttl << 'EOF'
@prefix mcp: <http://ggen.dev/mcp#> .

:MyServer a mcp:McpServer ;
    mcp:serverName "my-mcp-server" ;
    mcp:serverVersion "1.0.0" ;
    mcp:hasProtocolVersion "2025-11-25" ;
    mcp:hasTransport mcp:StdioTransport ;
    mcp:hasTool :HelloTool .

:HelloTool a mcp:Tool ;
    mcp:name "hello" ;
    mcp:description "Say hello to someone" ;
    mcp:hasArgument :NameArg .

:NameArg a mcp:ToolArgument ;
    mcp:argumentName "name" ;
    mcp:argumentType "string" ;
    mcp:isRequired true .
EOF

# 2. Generate the server
ggen mcp generate --ontology my-server.ttl --output ./mcp-server/src

# 3. Build and run
cd mcp-server && cargo run
```

## Architecture

The generation pipeline consists of 5 stages (μ₁-μ₅):

```
┌─────────────────────────────────────────────────────────────────┐
│ μ₁: LOAD - Load RDF ontology into Oxigraph triplestore          │
└─────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────┐
│ μ₂: EXTRACT - SPARQL CONSTRUCT → normalized JSON context       │
└─────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────┐
│ μ₃: RENDER - Tera templates → generated Rust code              │
│              (9 templates: server, tools, resources, etc.)       │
└─────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────┐
│ μ₄: VALIDATE - Compile gate (cargo check)                      │
└─────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────┐
│ μ₅: EMIT - Write files + cryptographic receipt (Ed25519)       │
└─────────────────────────────────────────────────────────────────┘
```

## Supported MCP Capabilities

| Capability | Status | Description |
|------------|--------|-------------|
| **Tools** | ✅ Full | Typed arguments with JSON Schema validation |
| **Resources** | ✅ Full | Static and URI-template resources |
| **Prompts** | ✅ Full | Argument substitution for LLM prompts |
| **Completions** | ✅ Full | Argument autocomplete suggestions |
| **Logging** | ✅ Full | MCP protocol + OTEL Weaver integration |
| **Subscriptions** | ⏳ Planned | Event streaming support |

## Documentation

| Section | Description | Status |
|---------|-------------|--------|
| [User Guide: Generating Servers](01-user-guide/generating-servers.md) | Complete guide to generating MCP servers from ontologies (1,247 words) | ✅ Complete |
| [User Guide: Troubleshooting](01-user-guide/troubleshooting.md) | Common issues and solutions (1,156 words, 25+ issues) | ✅ Complete |
| [Quick Start](01-quick-start/) | Get started in 5 minutes | ✅ Complete |
| [RDF Schema](02-rdf-schema/) | Complete ontology reference | ✅ Complete |
| [Code Generation](03-code-generation/) | Pipeline internals | ✅ Complete |
| [Template Customization](04-template-customization/) | Modify code generation | ✅ Complete |
| [SPARQL Guide](05-sparql-guide/) | Query extraction patterns | ✅ Complete |
| [Examples](06-examples/) | Sample server definitions | ✅ Complete |
| [Implementation Summary](IMPLEMENTATION_SUMMARY.md) | Technical implementation details | ✅ Complete |

### Feature Status

| Feature | Status | Notes |
|---------|--------|-------|
| **MCP Server Generation** | ✅ Working | Full `ggen mcp generate` command implemented |
| **RDF Ontology Schema** | ✅ Complete | `mcp:` prefix with Server, Tool, Resource, Prompt classes |
| **Five-Stage Pipeline** | ✅ Complete | μ₁-μ₅: Load→Extract→Generate→Validate→Emit |
| **LLM-Assisted Codegen** | ✅ Working | `--enable-llm` flag with Groq integration |
| **Multi-Language Support** | ✅ Working | Rust, Go, Python, TypeScript, Elixir |
| **Quality Gates** | ✅ Complete | 6 gates including manifest, dependencies, SPARQL, templates |
| **OTEL Integration** | ✅ Complete | OpenTelemetry spans with semantic conventions |
| **Documentation** | ✅ Complete | User guide, examples, troubleshooting |

## RDF Schema Overview

The MCP ontology (`http://ggen.dev/mcp#`) defines these core classes:

- `mcp:McpServer` - Top-level server definition
- `mcp:Tool` - Invokable function with typed arguments
- `mcp:Resource` - Named, addressable data
- `mcp:Prompt` - Reusable LLM prompt templates
- `mcp:CompletionProvider` - Argument autocomplete
- `mcp:LoggingPolicy` - Server logging configuration

See [RDF Schema Reference](02-rdf-schema/) for complete details.

## Generated Code Structure

```
generated/
├── server.rs          # Server struct, lifecycle, transports
├── tools.rs           # Tool router, parameter structs, handlers
├── resources.rs       # Resource list/read handlers
├── prompts.rs         # Prompt list/get handlers
├── completions.rs     # Argument completion handler
├── logging.rs         # Logging level handlers
└── main.rs            # Tokio bootstrap, transport selection
```

All generated code includes:
- ✅ OTEL tracing spans with semantic conventions
- ✅ JSON Schema validation for tool arguments
- ✅ Proper error handling with `Result<T, E>`
- ✅ Type-safe parameter structs (serde deserializable)

## CLI Reference

```bash
# Generate MCP server from ontology
ggen mcp generate --ontology <path.ttl> --output <dir>

# Skip compile gate (faster, no validation)
ggen mcp generate --ontology server.ttl --output ./src --skip-compile-gate

# Show generation details
RUST_LOG=trace ggen mcp generate --ontology server.ttl --output ./src
```

## Validation

The compile gate (μ₄) ensures generated code compiles:

```bash
$ ggen mcp generate --ontology server.ttl --output ./src
μ₁: Loading ontology... 156 triples loaded
μ₂: Extracting context... 9 tools, 3 resources, 2 prompts
μ₃: Rendering templates... 7 files generated
μ₄: Compile gate... ✅ PASSED (cargo check in 2.3s)
μ₅: Emitting files... receipt: a3f7c9e2b8d1
```

## Receipts and Provenance

Each generation produces an Ed25519 cryptographic receipt:

```
Receipt {
  hash: "a3f7c9e2b8d1",
  timestamp: 2026-03-31T23:20:00Z,
  source: "server.ttl",
  triple_count: 156
}
```

Receipts enable:
- **Reproducibility** - Re-generate exact same code from ontology
- **Provenance tracking** - Trace generated code to source ontology
- **Dependency verification** - Ensure ontology hasn't changed

## Examples

See the [examples](06-examples/) directory for complete server definitions:

- **minimal-server.ttl** - Single tool, stdio transport
- **full-server.ttl** - All capabilities, multiple transports
- **codegen-server.ttl** - Code generation tools (sync, validate)
- **database-server.ttl** - Resource templates, completions

## Contributing

When extending the system:

1. **Add ontology classes** to `specify/ontologies/mcp/mcp.ttl`
2. **Extend CONSTRUCT query** in `crates/ggen-core/queries/mcp/extract-mcp-context.rq`
3. **Create new template** in `crates/ggen-core/templates/mcp/`
4. **Update documentation** in `docs/mcp-rdf/`

See [Template Customization Guide](04-template-customization/) for details.

## License

MIT License - see LICENSE file for details.

## See Also

- [MCP Protocol Specification](https://spec.modelcontextprotocol.io/)
- [RMCP 1.3.0 Documentation](https://github.com/jule-rs/rmcp)
- [RDF 1.2 Turtle Specification](https://www.w3.org/TR/turtle/)
- [SPARQL 1.2 Query Language](https://www.w3.org/TR/sparql11-query/)
