# Frontmatter Generation: JSON to YAML Workflow

This document demonstrates how to generate frontmatter as JSON and convert it to YAML format for ggen templates.

## Overview

The frontmatter generation workflow involves:
1. **Generate frontmatter as JSON** - Programmatically create frontmatter structure
2. **Convert JSON to YAML** - Transform for ggen template format
3. **Use with AI models** - Leverage Ollama qwen3-coder:30b for intelligent generation

## Examples

### 1. Basic JSON to YAML Conversion

```rust
use serde_json::{json, Value};
use serde_yaml;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Generate frontmatter as JSON
    let frontmatter_json = json!({
        "to": "src/{{name}}.rs",
        "vars": [
            {"name": "string"},
            {"email": "string"},
            {"role": "string"}
        ],
        "rdf": "@prefix ex: <http://example.org/> .\nex:User a owl:Class .",
        "sparql": "SELECT ?name ?email WHERE { ?user ex:name ?name ; ex:email ?email }",
        "determinism": true
    });

    // Convert to YAML
    let frontmatter_yaml = serde_yaml::to_string(&frontmatter_json)?;
    
    println!("{}", frontmatter_yaml);
    Ok(())
}
```

### 2. AI-Powered Frontmatter Generation

```rust
use ggen_ai::mcp::tools::AiMcpTools;
use serde_json::json;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize AI tools with Ollama qwen3-coder:30b
    let tools = AiMcpTools::new().with_ollama();
    
    // Generate frontmatter using AI
    let template_params = json!({
        "description": "A comprehensive user management system",
        "examples": ["Include email validation", "Add password hashing"],
        "language": "Rust",
        "framework": "Actix Web"
    });
    
    let result = tools.ai_generate_template(template_params).await?;
    println!("Generated: {}", serde_json::to_string_pretty(&result)?);
    
    Ok(())
}
```

### 3. CLI Tool for Frontmatter Generation

```bash
# Generate frontmatter with RDF and SPARQL
cargo run --example frontmatter-cli -- generate \
  --description "User authentication system" \
  --yaml --rdf --sparql \
  --output user-auth-frontmatter.yaml

# Show example frontmatter
cargo run --example frontmatter-cli -- example --template-type api

# Convert JSON to YAML
cargo run --example frontmatter-cli -- convert \
  --input frontmatter.json \
  --output frontmatter.yaml
```

## Frontmatter Structure

### JSON Format
```json
{
  "to": "src/{{name}}.rs",
  "vars": [
    {"name": "string"},
    {"email": "string"},
    {"role": "string"}
  ],
  "rdf": "@prefix ex: <http://example.org/> .\nex:User a owl:Class .",
  "sparql": "SELECT ?name ?email WHERE { ?user ex:name ?name ; ex:email ?email }",
  "determinism": true
}
```

### YAML Format
```yaml
to: src/{{name}}.rs
vars:
- name: string
- email: string
- role: string
rdf: |-
  @prefix ex: <http://example.org/> .
  ex:User a owl:Class .
sparql: SELECT ?name ?email WHERE { ?user ex:name ?name ; ex:email ?email }
determinism: true
```

## Template Types

### User Management Template
```yaml
to: src/models/user.rs
vars:
- name: string
- email: string
- role: string
rdf: |-
  @prefix ex: <http://example.org/> .
  ex:User a owl:Class .
sparql: SELECT ?name ?email WHERE { ?user ex:name ?name ; ex:email ?email }
determinism: true
```

### API Controller Template
```yaml
to: src/controllers/{{resource}}_controller.rs
vars:
- resource: string
- actions: array
rdf: |-
  @prefix ex: <http://example.org/> .
  ex:Controller a owl:Class .
sparql: SELECT ?resource ?action WHERE { ?controller ex:manages ?resource ; ex:hasAction ?action }
determinism: true
```

### SPARQL Query Template
```yaml
to: queries/{{query_name}}.sparql
vars:
- query_name: string
- domain: string
rdf: |-
  @prefix ex: <http://example.org/> .
  ex:Query a owl:Class .
sparql: SELECT ?query ?domain WHERE { ?query ex:belongsTo ?domain }
determinism: true
```

## AI Integration

### Using Ollama qwen3-coder:30b

1. **Install Ollama**: https://ollama.ai/
2. **Pull model**: `ollama pull qwen3-coder:30b`
3. **Start service**: `ollama serve`
4. **Use in code**:
   ```rust
   let tools = AiMcpTools::new().with_ollama();
   ```

### AI-Generated Frontmatter Features

- **Intelligent variable detection** - AI identifies required template variables
- **RDF ontology generation** - Creates semantic models for data
- **SPARQL query generation** - Produces queries based on intent
- **Framework-specific templates** - Adapts to target frameworks (Actix Web, etc.)

## Usage Patterns

### 1. Programmatic Generation
```rust
// Generate frontmatter programmatically
let frontmatter = generate_frontmatter(
    "User management system",
    true, // include RDF
    true, // include SPARQL
)?;
```

### 2. AI-Assisted Generation
```rust
// Use AI to generate intelligent frontmatter
let ai_frontmatter = tools.ai_generate_template(params).await?;
```

### 3. Template Composition
```rust
// Combine multiple frontmatter sources
let base_frontmatter = load_base_template();
let ai_enhancements = generate_ai_enhancements();
let final_frontmatter = merge_frontmatter(base_frontmatter, ai_enhancements);
```

## Best Practices

### 1. Frontmatter Design
- **Use descriptive variable names** - `user_name` instead of `name`
- **Include type information** - Specify data types for variables
- **Add validation rules** - Include constraints in RDF
- **Enable determinism** - Set `determinism: true` for reproducible outputs

### 2. RDF Integration
- **Define clear ontologies** - Use proper prefixes and namespaces
- **Include domain knowledge** - Add semantic relationships
- **Validate against SHACL** - Use shapes for data validation

### 3. SPARQL Queries
- **Use descriptive names** - `get_users_by_role` instead of `query1`
- **Include prefixes** - Always define namespace prefixes
- **Test queries** - Validate against sample data

### 4. AI Usage
- **Provide clear descriptions** - Be specific about requirements
- **Include examples** - Show desired patterns and structures
- **Specify constraints** - Mention performance, security, or other requirements

## Integration with ggen

Generated frontmatter can be used directly with ggen:

```bash
# Use generated frontmatter
cargo run -- project gen user-auth-frontmatter.yaml --var name=User --out src/
```

## Troubleshooting

### Common Issues

1. **YAML parsing errors** - Check indentation and syntax
2. **Variable resolution** - Ensure all template variables are defined
3. **RDF validation** - Verify ontology syntax and prefixes
4. **SPARQL syntax** - Test queries against sample data

### Debug Tips

1. **Use JSON first** - Generate JSON frontmatter for easier debugging
2. **Validate incrementally** - Test each component separately
3. **Check AI output** - Verify AI-generated content makes sense
4. **Test with sample data** - Use realistic test cases

## Examples Directory

- `json-to-yaml-frontmatter.rs` - Basic conversion example
- `ai-frontmatter-generator.rs` - AI-powered generation
- `frontmatter-cli.rs` - Command-line tool
- `ollama-qwen3-demo.rs` - Ollama integration demo

## Dependencies

```toml
[dependencies]
serde_json = "1.0"
serde_yaml = "0.9"
clap = { version = "4.0", features = ["derive"] }
ggen-ai = { path = "ggen-ai" }
```

## License

This project follows the same license as the main ggen project.
