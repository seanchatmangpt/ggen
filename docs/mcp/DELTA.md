# MCP Implementation Delta: Plan vs Reality

**Date**: 2026-03-31
**Status**: Documentation updated to reflect actual implementation

## Overview

The plan in `docs/superpowers/plans/2026-03-31-mcp-server-generation.md` describes a **fully RDF-driven code generation system** where MCP servers are generated from RDF ontologies. The **current implementation** is different: a **hybrid approach** with a hand-written MCP server plus RDF extraction for parameters and metadata.

## Planned vs Actual

### Planned Architecture (from plan)

```
RDF Ontology (.ttl) → SPARQL CONSTRUCT (.rq) → Tera Template (.tera) → Generated Rust Code
```

**Key Features of Planned System:**
1. Pure MCP ontology (`specify/ontologies/mcp/mcp.ttl`) defining all MCP classes
2. Example server definition (`mcp-server.ttl`) describing a specific server
3. SPARQL CONSTRUCT query (`extract-mcp-context.rq`) extracting normalized context
4. Tera templates (`server.rs.tera`, `tools.rs.tera`, etc.) generating code
5. CLI command: `ggen mcp generate --ontology server.ttl --output ./src`
6. **Fully generated server code** - no hand-written implementation

### Actual Architecture (current implementation)

```
Hand-written GgenMcpServer + RDF extraction for tool parameters
```

**Key Features of Actual System:**
1. Hand-written `GgenMcpServer` in `ggen_server.rs` with 16 tools directly coded
2. MCP+A2A protocol ontology exists (`specify/mcp-a2a-protocol.ttl`) but used for documentation/metadata
3. SPARQL queries extract tool parameters for validation/documentation
4. Tera template exists (`server.rs.tera`) but generates stub scaffolding, not full implementation
5. RMCP 1.3.0 SDK used directly with `#[tool]` macros
6. **Hybrid**: Server is hand-written, RDF provides parameter validation and some scaffolding

## Critical Differences

| Aspect | Planned (Plan) | Actual (Current) |
|--------|---------------|-----------------|
| **Server Implementation** | Generated from RDF | Hand-written `GgenMcpServer` |
| **Tool Definitions** | From RDF ontology | Directly coded with `#[tool]` macro |
| **Parameter Structs** | Generated from RDF | Hand-written, validated via RDF |
| **Code Location** | `crates/ggen-a2a-mcp/src/` (generated) | `crates/ggen-a2a-mcp/src/ggen_server.rs` (hand-written) |
| **CLI Command** | `ggen mcp generate` | `ggen mcp start-server` (run server) |
| **RDF Role** | Source of truth for server code | Metadata/documentation for tools |
| **Generation** | Full server generated | Partial (scaffolding, validation) |

## Files That Exist

### ✅ Exists (Actual Implementation)

| File | Purpose |
|------|---------|
| `crates/ggen-a2a-mcp/src/ggen_server.rs` | **Hand-written** MCP server with 16 tools |
| `crates/ggen-a2a-mcp/src/server.rs` | Transport entry points (stdio, HTTP) |
| `crates/ggen-a2a-mcp/src/handlers.rs` | Message routing, 5 handlers |
| `crates/ggen-a2a-mcp/src/adapter.rs` | A2A↔LLM bidirectional adapters |
| `specify/mcp-a2a-protocol.ttl` | MCP+A2A protocol ontology (metadata) |
| `examples/mcp-server-definition/ontology/mcp-server.ttl` | Example server definition |
| `crates/ggen-core/queries/mcp/extract-mcp-server.rq` | Extract server metadata |
| `crates/ggen-core/templates/mcp/server.rs.tera` | Generate server scaffolding (not used for main server) |

### ❌ Does Not Exist (Planned but Not Implemented)

| File | Purpose |
|------|---------|
| `specify/ontologies/mcp/mcp.ttl` | Pure MCP ontology (replaced by `mcp-a2a-protocol.ttl`) |
| `crates/ggen-core/templates/mcp/tools.rs.tera` | Generate tool implementations (not used) |
| `crates/ggen-core/templates/mcp/resources.rs.tera` | Generate resource handlers (not used) |
| `crates/ggen-core/templates/mcp/prompts.rs.tera` | Generate prompt handlers (not used) |
| `crates/ggen-core/templates/mcp/completions.rs.tera` | Generate completion handlers (not used) |
| `crates/ggen-core/templates/mcp/logging.rs.tera` | Generate logging handlers (not used) |
| `crates/ggen-a2a-mcp/src/tools.rs` | Generated tools (hand-written instead) |
| `crates/ggen-a2a-mcp/src/resources.rs` | Generated resources (hand-written in server.rs) |

## Why the Difference?

### Technical Reasons

1. **RMCP SDK Convenience**: RMCP 1.3.0's `#[tool]` macro is simpler than full code generation
2. **Performance**: Hand-written code avoids template overhead and is easier to debug
3. **Type Safety**: Direct Rust implementation provides better IDE support than generated code
4. **Iterative Development**: Hand-written code allows faster iteration during development

### Architectural Reasons

1. **Hybrid Approach**: The project uses RDF for **metadata and validation** while hand-writing core logic
2. **Separation of Concerns**: RDF describes WHAT (capabilities), Rust code implements HOW (execution)
3. **Best of Both Worlds**: Type-safe hand-written code + RDF-driven parameter validation

## Updated Documentation Strategy

The documentation has been updated to reflect:

1. **What IS implemented**: Hand-written `GgenMcpServer` with 16 tools
2. **What EXISTS for RDF**: Protocol ontology for metadata, parameter extraction for validation
3. **What the templates DO**: Generate scaffolding and documentation, not main server code
4. **How to use the current system**: `ggen mcp start-server` (not `ggen mcp generate`)

## Migration Path (If Implementing Full Plan)

To move from current → planned (fully RDF-driven):

1. **Extract all tool logic** into separate modules (decouple from `GgenMcpServer`)
2. **Create tool ontology** mapping each tool to its implementation module
3. **Generate tool wrappers** from templates that call implementation modules
4. **Gradual migration**: One tool at a time, validate with tests
5. **Keep hand-written logic** in implementation modules (RDF drives wiring, not algorithms)

## Current Architecture Summary

```
┌─────────────────────────────────────────────────────────────────┐
│                    Hand-Written MCP Server                      │
│                   (GgenMcpServer in ggen_server.rs)            │
│                                                                   │
│  • 16 tools directly implemented with #[tool] macro         │
│  • A2A bridge adapters for agent communication               │
│  • Message router with 5 handlers (Text, File, Data, etc.)    │
│  • YAWL bridge for workflow integration                      │
│  • OTEL tracing for observability                            │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                 RDF Metadata Layer (Supplementary)              │
│                                                                   │
│  • specify/mcp-a2a-protocol.ttl - Protocol ontology           │
│  • examples/mcp-server-definition/ontology/mcp-server.ttl      │
│  • crates/ggen-core/queries/mcp/ - Parameter extraction       │
│  • crates/ggen-core/templates/mcp/server.rs.tera - Scaffolding  │
└─────────────────────────────────────────────────────────────────┘
```

## Conclusion

The current implementation is a **pragmatic hybrid**: hand-written server code for control and performance, with RDF providing metadata, validation, and some scaffolding. The planned "fully RDF-generated" approach would be more automated but less practical for hand-coded tool logic.

**The documentation now reflects the actual implementation**, not the theoretical plan.
