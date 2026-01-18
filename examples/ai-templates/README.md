# ggen-ai Templates with Ollama qwen3-coder:30b

This directory contains AI-generated templates and examples for ggen-ai using the Ollama qwen3-coder:30b model.

## Prerequisites

1. **Install Ollama**: Download from [https://ollama.ai/](https://ollama.ai/)
2. **Pull qwen3-coder:30b model**:
   ```bash
   ollama pull qwen3-coder:30b
   ```
3. **Start Ollama service**:
   ```bash
   ollama serve
   ```

## Usage

### CLI Commands

Generate templates using the AI CLI:

```bash
# Generate a basic template
cargo run -- ai generate --description "A REST API controller for user management" --language "Rust" --framework "Actix Web"

# Generate with examples and save to file
cargo run -- ai generate --description "User authentication system" --examples "Include JWT validation" --examples "Add password hashing" --output user-auth.tmpl

# Run the demo
cargo run -- ai demo
```

### MCP Server

Start the MCP server for integration with other tools:

```bash
cargo run --bin ggen-ai-mcp
```

The server will automatically use Ollama with qwen3-coder:30b model.

### Programmatic Usage

```rust
use ggen_ai::mcp::tools::AiMcpTools;
use serde_json::json;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize with Ollama client (uses qwen3-coder:30b by default)
    let tools = AiMcpTools::new().with_ollama();
    
    // Generate a template
    let params = json!({
        "description": "A user management system",
        "examples": ["Include CRUD operations"],
        "language": "Rust"
    });
    
    let result = tools.ai_generate_template(params).await?;
    println!("Generated template: {}", serde_json::to_string_pretty(&result)?);
    
    Ok(())
}
```

## Available Templates

### 1. User Management Template (`user-management.tmpl`)
- Generates Rust structs for user management
- Includes validation, metadata handling, and role-based access
- Uses RDF ontology for semantic modeling

### 2. API Controller Template (`api-controller.tmpl`)
- Generates REST API controllers with CRUD operations
- Supports Actix Web framework
- Includes error handling and validation

### 3. SPARQL Queries Template (`sparql-queries.tmpl`)
- Generates SPARQL queries for RDF data
- Includes alternative query patterns
- Supports domain-specific filtering

## AI Tools Available

The ggen-ai system provides several AI-powered tools:

1. **Template Generation**: Generate ggen templates from natural language descriptions
2. **SPARQL Generation**: Create SPARQL queries from intent descriptions
3. **Ontology Generation**: Generate RDF/OWL ontologies from domain descriptions
4. **Code Refactoring**: Suggest improvements for existing code
5. **Graph Explanation**: Explain RDF graph content in natural language
6. **Delta Suggestions**: Recommend merge strategies for code changes

## Configuration

The system is configured to use qwen3-coder:30b with optimized settings:

- **Model**: qwen3-coder:30b
- **Temperature**: 0.1 (for deterministic code generation)
- **Max Tokens**: 4096
- **Top-p**: 0.9
- **Stop Sequences**: ["```", "---"]

## Examples

### Generate a User Struct

```bash
cargo run -- ai generate \
  --description "A User struct for web application with authentication" \
  --examples "Include email validation" \
  --examples "Add password hashing" \
  --examples "Support role-based access" \
  --language "Rust" \
  --framework "Actix Web" \
  --output user-struct.tmpl
```

### Generate SPARQL Query

```bash
cargo run -- ai generate-sparql \
  --intent "Find all users who have admin role and are active" \
  --graph "@prefix ex: <http://example.org/> . ex:user1 a ex:User ; ex:role 'admin' ; ex:status 'active' ."
```

### Generate Ontology

```bash
cargo run -- ai generate-ontology \
  --domain "E-commerce platform with user management" \
  --requirements "Include User, Product, and Order classes" \
  --requirements "Add authentication and authorization properties"
```

## Troubleshooting

### Ollama Connection Issues

1. Ensure Ollama is running: `ollama serve`
2. Check if qwen3-coder:30b is installed: `ollama list`
3. Test the model: `ollama run qwen3-coder:30b "Hello, world!"`

### Model Not Found

If you get model not found errors:

```bash
# Pull the model
ollama pull qwen3-coder:30b

# Verify installation
ollama list
```

### Performance Issues

For better performance with large models:

1. Ensure sufficient RAM (qwen3-coder:30b requires ~20GB)
2. Use GPU acceleration if available
3. Consider using smaller models for development

## Integration with ggen

Generated templates can be used with the main ggen system:

```bash
# Generate code from AI-created template
cargo run -- project gen user-struct.tmpl --var name=User --out src/
```

## Contributing

To add new AI tools or improve existing ones:

1. Add new methods to `AiMcpTools` in `ggen-ai/src/mcp/tools.rs`
2. Update the MCP server in `ggen-ai/src/mcp/server.rs`
3. Add corresponding CLI commands in `cli/src/cmds/ai.rs`
4. Update tests and documentation

## License

This project follows the same license as the main ggen project.

