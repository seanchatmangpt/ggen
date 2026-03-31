# ggen MCP Documentation

Complete documentation for the ggen Model Context Protocol (MCP) server implementation.

## Quick Links

- [What is MCP?](./01-overview/what-is-mcp.md) - Introduction to MCP
- [Quick Start](./01-overview/quick-start.md) - Get up and running in 5 minutes
- [Tool Reference](./02-user-guide/tools/) - All 16 MCP tools documented
- [Architecture](./05-architecture/system-overview.md) - System design and components
- [API Reference](./07-integration/api-reference/) - Complete API documentation

## Overview

ggen MCP is a Model Context Protocol server that bridges LLM clients (Claude, ChatGPT) with ggen's code generation capabilities. It exposes 16 tools for code generation, validation, SPARQL querying, and project management.

## Key Features

| Feature | Description |
|----------|-------------|
| **16 MCP Tools** | Complete code generation workflow |
| **Dual Transport** | Stdio (local) and HTTP (remote) |
| **A2A Protocol** | Agent-to-Agent communication |
| **Streaming** | Long-running operation support |
| **Observable** | OpenTelemetry tracing built-in |
| **Secure** | Origin validation, rate limiting |

## Documentation Structure

```
docs/mcp/
├── 01-overview/          # Introduction and quick start
├── 02-user-guide/        # Tool reference and client setup
├── 03-tutorials/         # Step-by-step tutorials
├── 04-how-to/            # Task-specific guides
├── 05-architecture/      # System architecture
├── 06-development/       # Contributor guide
├── 07-integration/       # API reference
├── 08-operations/        # Deployment and operations
├── 09-reference/         # Tool specs and error codes
├── 10-appendix/          # FAQ and glossary
└── diagrams/             # Mermaid diagrams
```

## MCP Tools

### Pipeline
- [`generate`](./02-user-guide/tools/generate.md) - Generate code from RDF ontology
- [`sync`](./02-user-guide/tools/sync.md) - Full sync with audit trail
- [`validate`](./02-user-guide/tools/validate.md) - Validate Turtle syntax

### Query
- [`query_ontology`](./02-user-guide/tools/query_ontology.md) - Execute SPARQL queries
- [`list_generators`](./02-user-guide/tools/list_generators.md) - List code generators

### Examples
- [`list_examples`](./02-user-guide/tools/list_examples.md) - List bundled examples
- [`get_example`](./02-user-guide/tools/get_example.md) - Get example details
- [`scaffold_from_example`](./02-user-guide/tools/scaffold_from_example.md) - Copy example

### Validation
- [`validate_pipeline`](./02-user-guide/tools/validate_pipeline.md) - Run all quality gates
- [`validate_sparql`](./02-user-guide/tools/validate_sparql.md) - Validate SPARQL syntax
- [`validate_templates`](./02-user-guide/tools/validate_templates.md) - Validate templates
- [`validate_project`](./02-user-guide/tools/validate_project.md) - Full project validation
- [`validate_incremental`](./02-user-guide/tools/validate_incremental.md) - Validate changed files
- [`validate_dependency_graph`](./02-user-guide/tools/validate_dependency_graph.md) - Dependency analysis

### Orchestration
- [`fix_cycles`](./02-user-guide/tools/fix_cycles.md) - Fix circular dependencies

## Getting Started

### 1. Install

```bash
cargo make build
```

### 2. Configure Claude Desktop

```json
{
  "mcpServers": {
    "ggen": {
      "command": "/path/to/ggen",
      "args": ["mcp", "start-server", "--transport", "stdio"]
    }
  }
}
```

### 3. Start Using

```
@ggen use the generate tool with ontology_path: examples/hello-world/ontology.ttl
```

## Architecture

```
┌─────────────┐     JSON-RPC 2.0      ┌──────────────────┐
│   LLM       │ ◄────────────────────► │  GgenMcpServer   │
│  (Claude)   │                        │   (16 tools)     │
└─────────────┘                        └────────┬─────────┘
                                                 │
                                                 ▼
                                        ┌─────────────────┐
                                        │  Code Gen Core  │
                                        │   (μ₁-μ₅)       │
                                        └─────────────────┘
```

See [System Architecture](./05-architecture/system-overview.md) for details.

## Support

- **Issues**: https://github.com/seanchatmangpt/ggen/issues
- **Documentation**: https://github.com/seanchatmangpt/ggen/tree/main/docs
- **Examples**: https://github.com/seanchatmangpt/ggen/tree/main/examples

## License

MIT License - see LICENSE file for details.
