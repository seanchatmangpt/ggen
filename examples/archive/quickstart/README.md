# ggen Quickstart Examples

Three simple, self-contained examples demonstrating ggen's key features.

## Examples

### 1. MCP Tool (mcp-tool/)
Simple MCP server with one tool that validates a project path.

**Run:**
```bash
cd mcp-tool
cargo run --bin mcp_tool_example
```

**What it demonstrates:**
- Creating an MCP server with rmcp
- Defining a simple tool (`validate_project`)
- Handling tool parameters
- Returning structured results

**Key concepts:**
- MCP (Model Context Protocol) server implementation
- Tool registration and invocation
- Error handling and result formatting

---

### 2. A2A Agent (a2a-agent/)
Basic agent-to-agent handshake and state management.

**Run:**
```bash
cd a2a-agent
cargo run --bin a2a_agent_example
```

**What it demonstrates:**
- Agent state machine (Initializing → Ready → Processing → Idle)
- State transitions with validation
- Message queue management
- Error recovery

**Key concepts:**
- Agent lifecycle management
- State machine patterns
- Message passing
- Dead letter queue for failed messages

---

### 3. Template Generation (template-generation/)
Generate code from a simple RDF ontology.

**Run:**
```bash
cd template-generation
cargo run --bin template_example
```

**What it demonstrates:**
- Loading RDF ontology from TTL file
- Extracting template definitions
- Variable substitution
- File tree generation

**Key concepts:**
- RDF-based code generation
- Template variable interpolation
- File tree generation
- Ontology-driven development

---

## Common Requirements

All examples require:
- Rust 1.91.1 or later
- `cargo make` for building

## Testing Each Example

Each example can be tested independently:

```bash
# MCP Tool
cd examples/quickstart/mcp-tool
cargo make test

# A2A Agent
cd examples/quickstart/a2a-agent
cargo make test

# Template Generation
cd examples/quickstart/template-generation
cargo make test
```

## Next Steps

After mastering these quickstarts, explore:
- Full example applications in `examples/`
- Integration tests in `crates/*/tests/`
- Documentation in `docs/`
