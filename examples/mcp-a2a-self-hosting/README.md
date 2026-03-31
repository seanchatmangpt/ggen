# MCP/A2A Self-Hosting Example

**Complete working example** demonstrating ggen's zero-touch workflow from RDF ontology to running MCP/A2A agent with LLM-powered code generation.

## What This Example Demonstrates

This example showcases **5 key features** of ggen v6:

1. **Behavior Predicates** - Use `a2a:hasSystemPrompt` and `a2a:hasImplementationHint` in ontologies
2. **LLM Generation** - Auto-generate skill implementations via GPT-4/Claude
3. **Schema Parser** - Type-safe parameter generation from compact schema syntax
4. **Quality Gates** - Validation prevents broken code from being generated
5. **Zero-Touch Workflow** - From ontology to running agent without manual coding

## Project Structure

```
mcp-a2a-self-hosting/
├── ontology/
│   └── agent.ttl              # RDF ontology with behavior predicates
├── templates/
│   └── skills.tera             # Template using {{ generated_impl }}
├── src/
│   └── generated/              # Auto-generated code (DO NOT EDIT)
├── ggen.toml                    # ggen config with LLM generation enabled
├── .mcp.json                    # MCP server config
├── a2a.toml                     # A2A agent config
├── Cargo.toml                   # Rust project config
├── setup.sh                     # Setup and validation script
└── README.md                    # This file
```

## Quick Start (5 minutes)

### Prerequisites

- Rust 1.91.1+
- ggen CLI installed (`cargo install ggen-cli`)
- OpenAI API key (or Anthropic for Claude)

### Step 1: Configure LLM Provider

```bash
# Set your API key
export OPENAI_API_KEY="sk-..."
# or
export ANTHROPIC_API_KEY="sk-ant-..."
```

### Step 2: Generate Code from Ontology

```bash
# From this directory
ggen sync

# Output:
# [Receipt] Specification validation: PASSED
# [Receipt] Schema parsing: PASSED (3 skills)
# [Receipt] LLM generation: PASSED (3 implementations)
# [Receipt] Quality gates: PASSED
# ✓ Generated: src/generated/agent.rs
# ✓ Generated: src/generated/skills.rs
# ✓ Generated: src/generated/agent_card.json
```

### Step 3: Run the Agent

```bash
# Run A2A HTTP server
cargo run --bin a2a-server

# Or run MCP server
cargo run --bin mcp-server

# Server starts on http://localhost:8080
# Agent card available at /.well-known/agent.json
```

### Step 4: Test the Agent

```bash
# Discover agent capabilities
curl http://localhost:8080/.well-known/agent.json

# Send a task via JSON-RPC
curl -X POST http://localhost:8080/rpc \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer dev-token" \
  -d '{
    "jsonrpc": "2.0",
    "method": "tasks/send",
    "id": 1,
    "params": {
      "id": "task-001",
      "message": {
        "role": "user",
        "messageId": "msg-001",
        "parts": [{
          "kind": "text",
          "text": "generate_code ontology/agent.ttl rust"
        }]
      }
    }
  }'
```

## Behavior Predicates

The ontology uses **behavior predicates** to guide LLM generation:

### a2a:hasSystemPrompt

Defines the agent's behavior and expertise:

```turtle
agent:CodeGeneratorAgent a2a:hasSystemPrompt """
You are an expert code generation agent specializing in RDF ontologies...
"""^^xsd:string ;
```

**Usage**: Sets context for all skill implementations, ensuring consistent behavior.

### a2a:hasImplementationHint

Provides specific guidance for each skill:

```turtle
agent:GenerateCodeSkill a2a:hasImplementationHint """
Use ggen sync command with the provided ontology path.
Support target_language: rust, go, elixir, java, typescript.
"""^^xsd:string ;
```

**Usage**: LLM uses this hint to generate accurate, context-aware implementations.

## LLM Generation

How auto-generation works:

1. **Extract** skill definitions from ontology (SPARQL)
2. **Parse** implementation hints and system prompts
3. **Generate** code via LLM (GPT-4 or Claude)
4. **Validate** generated code against schema (quality gate)
5. **Render** final code with Tera template

### Example Generated Code

**Input (Ontology):**
```turtle
agent:GenerateCodeSkill a2a:hasImplementationHint """
Use ggen sync command with the provided ontology path.
"""^^xsd:string ;
```

**Output (Generated Rust):**
```rust
pub async fn generate_code(
    &self,
    request: GenerateCodeRequest,
) -> Result<GenerateCodeResponse, anyhow::Error> {
    // Auto-generated from implementation hint
    let status = std::process::Command::new("ggen")
        .args(["sync", "--ontology", &request.ontology_path])
        .status()?;

    if status.success() {
        Ok(GenerateCodeResponse {
            generated_files: vec![...],
            status: "success".to_string(),
            message: "Code generated successfully".to_string(),
        })
    } else {
        Ok(GenerateCodeResponse {
            generated_files: vec![],
            status: "failed".to_string(),
            message: "Code generation failed".to_string(),
        })
    }
}
```

## Schema Parser

Compact schema syntax → Type-safe code:

### Input Schema
```
GenerateCodeRequest { ontology_path: string, target_language: string, output_dir?: string }
```

### Generated Rust
```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerateCodeRequest {
    pub ontology_path: String,
    pub target_language: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub output_dir: Option<String>,
}
```

### Supported Types

- **Primitives**: `string`, `integer`, `float`, `boolean`
- **Optional**: `field?: type` suffix
- **Arrays**: `type[]` suffix
- **Nested**: `TypeName { field: type }`

## Quality Gates

Four validation gates prevent broken code:

### 1. Specification Validation
```bash
[Receipt] Specification validation: PASSED
✓ 15 triples, 3 skills, 0 errors
```

### 2. Schema Parsing
```bash
[Receipt] Schema parsing: PASSED
✓ 6 schemas parsed (3 input, 3 output)
✓ All types valid
```

### 3. LLM Generation
```bash
[Receipt] LLM generation: PASSED
✓ 3 skill implementations generated
✓ All implementations compile
```

### 4. Compilation Check
```bash
[Receipt] Compilation check: PASSED
✓ 0 errors, 0 warnings
```

If any gate fails, generation stops and reports errors.

## Zero-Touch Workflow

From ontology to agent without manual coding:

```
1. Edit ontology/agent.ttl
   ↓
2. Run: ggen sync
   ↓
3. Auto-generated: src/generated/*
   ↓
4. Run: cargo run --bin a2a-server
   ↓
5. Running agent on http://localhost:8080
```

**No manual coding required** - the ontology is the single source of truth.

## Configuration Files

### ggen.toml
```toml
[llm]
enabled = true
provider = "openai"  # or "anthropic"
model = "gpt-4"

[quality]
gates = ["specification", "compilation", "tests"]
require_closure = true
```

### a2a.toml
```toml
[a2a]
agent_name = "CodeGeneratorAgent"
agent_url = "http://localhost:8080"

[a2a.auth]
enabled = true
```

### .mcp.json
```json
{
  "mcpServers": {
    "ggen-codegen": {
      "command": "cargo",
      "args": ["run", "--bin", "mcp-server"]
    }
  }
}
```

## Modifying the Agent

1. **Edit ontology** (`ontology/agent.ttl`)
   - Add/remove skills
   - Change behavior predicates
   - Update schema definitions

2. **Regenerate code**
   ```bash
   ggen sync
   ```

3. **Run agent**
   ```bash
   cargo run --bin a2a-server
   ```

**Never edit `src/generated/` directly** - always modify the ontology and regenerate.

## Troubleshooting

### LLM Generation Fails

**Error**: `LLM generation failed: API key not found`

**Solution**: Set `OPENAI_API_KEY` or `ANTHROPIC_API_KEY` environment variable.

### Schema Parse Error

**Error**: `Failed to parse schema: Missing closing brace`

**Solution**: Check schema syntax matches `TypeName { field: type }`

### Quality Gate Failure

**Error**: `Quality gate failed: Compilation errors`

**Solution**: Check generated code for syntax errors. Report issue if generated code is invalid.

## Advanced Usage

### Custom LLM Prompts

Override system prompts per skill:

```turtle
agent:CustomSkill a2a:hasSystemPrompt """
Custom prompt for this specific skill...
"""^^xsd:string ;
```

### Schema Validation

Enable strict SHACL validation:

```toml
[quality]
gates = ["specification", "shacl", "compilation", "tests"]
shacl_rules = "validation/rules.shacl"
```

### Multiple Agents

Define multiple agents in one ontology:

```turtle
agent:Agent1 a a2a:Agent ; ...
agent:Agent2 a a2a:Agent ; ...
```

## See Also

- [A2A Templating Usage](../../docs/A2A_TEMPLATING_USAGE.md) - Complete schema syntax reference
- [Schema Parser Reference](../../docs/A2A_TEMPLATING_COMPLETION_PLAN.md) - Implementation details
- [MCP Server Guide](../../crates/ggen-a2a-mcp/README.md) - MCP integration
- [ggen README](../../README.md) - Main project documentation

## License

Apache 2.0 OR MIT
