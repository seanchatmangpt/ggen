# MCP Zero-to-Hero: Using ggen in Your Model Context Protocol Server

**Complete guide from "What is ggen?" to building production MCP tools that leverage ggen's code generation, RDF reasoning, and ontology capabilities**

---

## Table of Contents

1. [What You'll Learn](#what-youll-learn)
2. [Prerequisites](#prerequisites)
3. [Part 1: Understanding ggen's Capabilities](#part-1-understanding-ggens-capabilities)
4. [Part 2: Discovering What ggen Can Do](#part-2-discovering-what-ggen-can-do)
5. [Part 3: Your First ggen Tool](#part-3-your-first-ggen-tool)
6. [Part 4: Building Complete MCP Tools](#part-4-building-complete-mcp-tools)
7. [Part 5: Real-World Examples](#part-5-real-world-examples)
8. [Common Patterns](#common-patterns)
9. [Troubleshooting](#troubleshooting)

---

## What You'll Learn

By the end of this guide, you will:
- Understand what ggen does and why MCPs want it
- Know how to discover ggen capabilities from any MCP
- Write MCP tools that call ggen for code generation
- Build sophisticated workflows using ggen's RDF and template engines
- Understand production patterns from ggen's own MCP packages

**Estimated Time:** 2-3 hours
**Difficulty:** Intermediate
**Prerequisites:** Basic understanding of MCPs, Rust or JavaScript, shell commands

---

## Prerequisites

You should have:
- Basic knowledge of [Model Context Protocol](https://modelcontextprotocol.io/)
- Understanding of ggen (read [Code Generation Working Loop](docs/diataxis/tutorials/05-code-generation-working-loop.md) if unsure)
- Ability to run shell commands
- Either Rust or JavaScript/TypeScript environment

---

## Part 1: Understanding ggen's Capabilities

### Why MCPs Want ggen

ggen provides three powerful capabilities that MCPs need:

#### 1. **Code Generation from Specifications**
```
Ontology (RDF) → SPARQL Queries → Templates → Generated Code
```

MCPs can:
- Generate OpenAPI specs from domain models
- Create Zod validation schemas
- Generate TypeScript type definitions
- Build CLI tools from specifications

#### 2. **Semantic Reasoning (RDF/SPARQL)**
```
Knowledge Graph (RDF) → SPARQL Queries → Insights
```

MCPs can:
- Reason about domain relationships
- Find patterns in specifications
- Validate consistency across models
- Answer complex questions about domains

#### 3. **Ontology Management**
```
Domain Model → RDF Ontology → Reproducible Outputs
```

MCPs can:
- Extract domain models from code
- Build knowledge graphs
- Generate documentation
- Validate domain constraints

### The 44 Capabilities: Grouped by Use Case

**For Code Generation:**
- `template::generate` - Transform data into code
- `project::generate` - Create full projects
- `ai::generate-schema` - Generate validation schemas
- `project::plan` - Plan implementation

**For Semantic Reasoning:**
- `graph::query` - Query RDF with SPARQL
- `graph::export` - Export reasoning results
- `ai::generate-ontology` - Create domain models

**For Validation:**
- `template::validate` - Validate template syntax
- `graph::validate` - Validate RDF integrity
- `project::validate` - Validate project structure

---

## Part 2: Discovering What ggen Can Do

### The Two-Phase Discovery Pattern

All MCPs follow this pattern to use ggen:

**Phase 1: List Available Capabilities**
```bash
ggen --graph
```

Returns JSON with 44 capabilities (verbs) across 9 modules.

**Phase 2: Get Details for Specific Capability**
```bash
ggen --capabilities ai generate-ontology
```

Returns:
```json
{
  "noun": "ai",
  "verb": "generate-ontology",
  "description": "Generate RDF ontology from specification",
  "arguments": [
    {
      "name": "spec",
      "type": "string",
      "optional": false,
      "description": "Domain specification"
    },
    {
      "name": "output",
      "type": "path",
      "optional": true,
      "description": "Output file path"
    }
  ],
  "return_type": "ontology",
  "supports_json_output": true
}
```

### Discovering Capabilities from Your MCP

Here's how to implement capability discovery in your MCP:

**Rust (using ggen crate):**
```rust
use ggen::cli::introspection;

fn discover_capabilities() -> Result<CommandGraph> {
    let graph = introspection::load_command_graph()?;

    for capability in graph.verbs {
        println!("{}: {}", capability.name, capability.description);
    }

    Ok(graph)
}

fn get_capability_details(noun: &str, verb: &str) -> Result<VerbMetadata> {
    let metadata = introspection::get_verb_metadata(noun, verb)?;
    Ok(metadata)
}
```

**JavaScript (calling ggen CLI):**
```javascript
const { execSync } = require('child_process');

function discoverCapabilities() {
    const output = execSync('ggen --graph').toString();
    return JSON.parse(output);
}

function getCapabilityDetails(noun, verb) {
    const output = execSync(`ggen --capabilities ${noun} ${verb}`).toString();
    return JSON.parse(output);
}
```

### The 44 Capabilities Organized by Domain

**AI Module** (6 capabilities):
- Generate ontologies from specs
- Analyze domain models
- Generate validation schemas
- Validate models
- Export types
- Infer relationships

**Template Module** (7 capabilities):
- Generate code from templates
- Validate template syntax
- List available templates
- Preview template output
- Export/import templates
- Generate RDF from templates

**Graph Module** (5 capabilities):
- Load and query RDF graphs
- Export graph data
- Validate RDF integrity
- Merge multiple graphs

**Ontology Module** (4 capabilities):
- Extract ontologies from code
- Generate ontologies from specs
- Validate ontology structure
- Initialize new ontologies

**Project Module** (4 capabilities):
- Create new projects
- Generate from ontologies
- Plan implementations
- Validate project structure

**CI Module** (4 capabilities):
- Generate CI workflows
- Create CI actions
- Build pipelines
- Validate CI configs

**Workflow Module** (2 capabilities):
- Generate workflows
- Execute workflow plans

**FMEA Module** (5 capabilities):
- Generate risk reports
- Create Pareto analysis
- List failure modes
- Show detailed failures
- Export FMEA data

**Utils Module** (2 capabilities):
- System diagnostics (doctor)
- Environment information

---

## Part 3: Your First ggen Tool

### Step 1: Create a Basic MCP Tool

Let's create a simple tool that generates a type definitions from a spec.

**The Tool Definition** (in your MCP config):
```json
{
  "tools": [
    {
      "name": "generate_types",
      "description": "Generate TypeScript types from a domain specification",
      "inputSchema": {
        "type": "object",
        "properties": {
          "spec": {
            "type": "string",
            "description": "Domain specification (domain entities and properties)"
          },
          "format": {
            "type": "string",
            "enum": ["typescript", "zod", "openapi"],
            "description": "Output format"
          }
        },
        "required": ["spec", "format"]
      }
    }
  ]
}
```

**The Implementation** (Rust):
```rust
use ggen::cli::introspection;
use ggen::modules::ai;

fn handle_generate_types(spec: String, format: String) -> Result<String> {
    // Step 1: Check if ggen supports this
    let metadata = introspection::get_verb_metadata("ai", "generate-schema")?;

    // Step 2: Call ggen with the spec
    let ontology = ai::generate_ontology(&spec)?;

    // Step 3: Generate in desired format
    let result = match format.as_str() {
        "typescript" => ai::export_types(&ontology, "typescript")?,
        "zod" => ai::export_types(&ontology, "zod")?,
        "openapi" => ai::export_types(&ontology, "openapi")?,
        _ => return Err("Unknown format".into()),
    };

    Ok(result)
}
```

**The Implementation** (JavaScript):
```javascript
const { execSync } = require('child_process');

function handleGenerateTypes(spec, format) {
    // Step 1: Create a temporary ontology file
    const tempFile = `/tmp/spec-${Date.now()}.ttl`;
    require('fs').writeFileSync(tempFile, spec);

    // Step 2: Call ggen to generate schema
    try {
        const output = execSync(
            `ggen ai generate-schema --file ${tempFile} --format ${format}`
        ).toString();
        return output;
    } finally {
        require('fs').unlinkSync(tempFile);
    }
}
```

### Step 2: Test Your Tool

You now have an MCP tool that:
1. ✅ Takes a spec as input
2. ✅ Calls ggen's AI module
3. ✅ Returns generated types

**Test it:**
```bash
curl http://localhost:3000/tools/generate_types \
  -d '{
    "spec": "User: id (string), email (string), name (string)",
    "format": "typescript"
  }'
```

---

## Part 4: Building Complete MCP Tools

### Pattern 1: Multi-Step Workflows

Real-world MCPs need more than single calls. Here's a complete workflow:

```
User asks: "Create a complete API from this spec"
    ↓
Step 1: Parse specification → Generate ontology (ai::generate-ontology)
    ↓
Step 2: Validate ontology → Ensure consistency (graph::validate)
    ↓
Step 3: Generate artifacts:
    - OpenAPI spec (template::generate)
    - Zod schemas (template::generate)
    - TypeScript types (ai::export-types)
    ↓
Step 4: Return all artifacts
```

**Implementation:**
```rust
fn complete_api_workflow(spec: String) -> Result<ApiArtifacts> {
    // Step 1: Generate ontology
    let ontology = ai::generate_ontology(&spec)?;

    // Step 2: Validate
    if !graph::validate(&ontology)? {
        return Err("Ontology validation failed".into());
    }

    // Step 3: Generate artifacts in parallel
    let openapi = template::generate(&ontology, "openapi-spec")?;
    let zod = template::generate(&ontology, "zod-schemas")?;
    let types = ai::export_types(&ontology, "typescript")?;

    Ok(ApiArtifacts {
        spec: openapi,
        schemas: zod,
        types,
    })
}
```

### Pattern 2: Error Recovery

MCPs need to handle failures gracefully:

```rust
fn robust_generation(spec: String) -> Result<Output> {
    // Try the happy path
    match ai::generate_ontology(&spec) {
        Ok(ontology) => {
            // Validate before returning
            if graph::validate(&ontology)? {
                Ok(Output::Success(ontology))
            } else {
                // Recovery: Get detailed validation errors
                let errors = graph::validate_detailed(&ontology)?;
                Ok(Output::PartialSuccess {
                    ontology,
                    warnings: errors,
                })
            }
        }
        Err(e) => {
            // Recovery: Try with default values
            eprintln!("Generation failed: {}, trying with defaults", e);
            let default_ontology = ai::generate_ontology_with_defaults(&spec)?;
            Ok(Output::WithWarnings(default_ontology))
        }
    }
}
```

### Pattern 3: Streaming Long Operations

For MCPs that stream results:

```rust
async fn streaming_generation(
    spec: String,
    tx: mpsc::Sender<ProgressUpdate>,
) -> Result<()> {
    // Step 1
    tx.send(ProgressUpdate::Started).await?;
    let ontology = ai::generate_ontology(&spec)?;

    // Step 2
    tx.send(ProgressUpdate::OntologyReady).await?;
    let validation = graph::validate(&ontology)?;

    // Step 3
    tx.send(ProgressUpdate::GeneratingArtifacts).await?;
    let openapi = template::generate(&ontology, "openapi")?;
    tx.send(ProgressUpdate::OpenAPIReady(openapi.clone())).await?;

    let zod = template::generate(&ontology, "zod")?;
    tx.send(ProgressUpdate::ZodReady(zod.clone())).await?;

    // Complete
    tx.send(ProgressUpdate::Complete).await?;
    Ok(())
}
```

---

## Part 5: Real-World Examples

### Example 1: The Agent-Reasoning-MCP Package

ggen's own `agent-reasoning-mcp` package shows production patterns:

**What it does:**
- Agents discover ggen's 44 capabilities
- Agents learn verb metadata (types, arguments, constraints)
- Agents build workflows from capabilities
- Agents execute with proper error handling

**Key Pattern:**
```rust
// Phase 1: Discovery
let graph = introspection::load_command_graph()?;
let verbs = graph.extract_verbs();

// Phase 2: Learning
for verb in verbs {
    let metadata = introspection::get_verb_metadata(&verb.noun, &verb.verb)?;
    agent.learn(metadata);  // Store for planning
}

// Phase 3: Planning
let workflow = agent.plan_workflow("Generate API from spec")?;
// workflow: [ai::generate-ontology, template::generate, ai::export-types]

// Phase 4: Execution
for step in workflow {
    let result = step.execute()?;
    agent.adapt(result);  // Learn from execution
}
```

### Example 2: The Rig-MCP Package

ggen's `rig-mcp` shows how to integrate with multiple LLM providers:

**Architecture:**
```rust
pub struct RigMcpClient {
    // Multiple LLM providers
    providers: HashMap<String, Box<dyn CompletionModel>>,

    // Embeddings for intelligent tool selection
    embeddings: Option<Box<dyn EmbeddingModel>>,

    // Connected MCP servers (including ggen)
    mcp_servers: Vec<Server>,
}

impl RigMcpClient {
    fn discover_ggen_tools(&self) -> Result<Vec<Tool>> {
        // Load ggen's 44 tools
        let server = self.get_mcp_server("ggen")?;
        let tools = server.list_tools()?;
        Ok(tools)
    }

    fn select_best_tool(&self, task: &str) -> Result<Tool> {
        // Use embeddings to find best ggen tool for task
        let available = self.discover_ggen_tools()?;
        let task_embedding = self.embeddings.embed(task)?;

        let best = available
            .iter()
            .max_by_key(|tool| {
                self.similarity(&task_embedding, &tool.embedding())
            })?;

        Ok(best.clone())
    }
}
```

### Example 3: Building a Code Generator MCP Tool

Complete tool that generates full codebases:

```rust
pub struct CodeGeneratorTool;

impl CodeGeneratorTool {
    pub fn generate_project(
        &self,
        spec: ProjectSpec,
    ) -> Result<GeneratedProject> {
        // Step 1: Create ontology
        let ontology = ai::generate_ontology(&spec.domain)?;

        // Step 2: Validate
        graph::validate(&ontology)
            .ok()
            .ok_or_else(|| "Invalid ontology".into())?;

        // Step 3: Generate all artifacts
        let artifacts = tokio::join!(
            // Code
            template::generate(&ontology, "rust-code"),
            template::generate(&ontology, "typescript-code"),

            // Documentation
            template::generate(&ontology, "openapi-spec"),
            template::generate(&ontology, "markdown-docs"),

            // Tests
            template::generate(&ontology, "rust-tests"),
            template::generate(&ontology, "typescript-tests"),

            // CI/CD
            ci::workflow(&ontology, "github-actions"),
        );

        Ok(GeneratedProject {
            rust_code: artifacts.0?,
            typescript_code: artifacts.1?,
            openapi: artifacts.2?,
            docs: artifacts.3?,
            rust_tests: artifacts.4?,
            ts_tests: artifacts.5?,
            ci: artifacts.6?,
        })
    }
}
```

---

## Common Patterns

### Pattern 1: Tool Registration and Discovery

**Your MCP needs to:**
1. Register tools in MCP config
2. Let users discover what tools you have
3. Help users understand tool arguments

**Implementation:**
```rust
fn register_ggen_tools(server: &mut MCPServer) {
    let capabilities = discover_ggen_capabilities()?;

    for capability in capabilities {
        server.register_tool(Tool {
            name: format!("{}__{}", capability.noun, capability.verb),
            description: capability.description,
            input_schema: capability.to_json_schema(),
        })?;
    }
}
```

### Pattern 2: Argument Validation

Before calling ggen, validate arguments:

```rust
fn validate_arguments(
    capability: &VerbMetadata,
    args: &JsonValue,
) -> Result<()> {
    for param in &capability.arguments {
        if !param.optional && args.get(&param.name).is_none() {
            return Err(format!("Missing required argument: {}", param.name));
        }

        // Type validation
        match param.param_type.as_str() {
            "string" => {
                if !args[&param.name].is_string() {
                    return Err(format!("{} must be string", param.name));
                }
            }
            "path" => {
                let path = args[&param.name].as_str().ok_or("Invalid path")?;
                if !std::path::Path::new(path).exists() {
                    return Err(format!("Path not found: {}", path));
                }
            }
            _ => {}
        }
    }
    Ok(())
}
```

### Pattern 3: Caching and Performance

Cache capability metadata to avoid repeated calls:

```rust
lazy_static::lazy_static! {
    static ref CAPABILITY_CACHE: Mutex<HashMap<String, VerbMetadata>> =
        Mutex::new(HashMap::new());
}

fn get_metadata_cached(noun: &str, verb: &str) -> Result<VerbMetadata> {
    let key = format!("{}:{}", noun, verb);

    let mut cache = CAPABILITY_CACHE.lock().unwrap();
    if let Some(metadata) = cache.get(&key) {
        return Ok(metadata.clone());
    }

    let metadata = introspection::get_verb_metadata(noun, verb)?;
    cache.insert(key, metadata.clone());
    Ok(metadata)
}
```

### Pattern 4: Result Formatting

Different MCPs want different output formats:

```rust
fn format_result(
    result: GeneratedOutput,
    format: OutputFormat,
) -> Result<String> {
    match format {
        OutputFormat::Json => serde_json::to_string_pretty(&result),
        OutputFormat::Yaml => serde_yaml::to_string(&result),
        OutputFormat::Raw => Ok(result.content),
        OutputFormat::Markdown => {
            Ok(format!("```\n{}\n```", result.content))
        }
    }
}
```

---

## Troubleshooting

### Issue: ggen command not found

**Symptom:**
```
Error: ggen command not found
```

**Solution:**
1. Ensure ggen is in PATH: `which ggen`
2. If not, add to PATH: `export PATH="$PATH:$(ggen --path)"`
3. Or call with absolute path: `/usr/local/bin/ggen`

### Issue: Capability metadata returns unexpected format

**Symptom:**
```
Error parsing capability metadata: unknown field
```

**Solution:**
1. Check ggen version: `ggen --version`
2. Update to latest: `cargo install ggen@latest`
3. Regenerate capability cache: `rm ~/.ggen_cache.json`

### Issue: Template generation fails silently

**Symptom:**
```
Template rendered but output is empty
```

**Diagnosis:**
1. Validate SPARQL query: `ggen graph query --query "YOUR_QUERY"`
2. Check template syntax: `ggen template validate --file template.tera`
3. Debug with `--verbose`: `ggen template::generate --verbose`

### Issue: Ontology validation fails

**Symptom:**
```
Graph validation failed: [list of errors]
```

**Solution:**
1. Check ontology syntax: `ggen graph --file ontology.ttl --validate`
2. Use detailed errors: `ggen graph --file ontology.ttl --validate --details`
3. Export for inspection: `ggen graph export --file ontology.ttl --format json`

### Issue: Performance degradation with large specs

**Symptom:**
```
Generation takes 30+ seconds for large ontology
```

**Solutions:**
1. Cache capability metadata (see Pattern 3)
2. Process in parallel: `async/await` for independent operations
3. Stream results: Use `tx.send()` pattern instead of buffering
4. Validate early: Fail fast on invalid input

---

## Next Steps

1. **Choose your use case:**
   - Code generation? → Start with [Part 3](#part-3-your-first-ggen-tool)
   - Semantic reasoning? → Study `agent-reasoning-mcp` package
   - Multi-provider LLM? → Study `rig-mcp` package

2. **Read production implementations:**
   - `/home/user/ggen/marketplace/packages/agent-reasoning-mcp/`
   - `/home/user/ggen/marketplace/packages/rig-mcp/`
   - `/home/user/ggen/.archive/crate-tests-backup/mcp_agent_workflows.rs`

3. **Implement your first tool:**
   - Follow the pattern from Part 3
   - Test with `ggen --graph` and `ggen --capabilities`
   - Deploy to production with error handling from Part 4

4. **Join the ecosystem:**
   - Publish your MCP to the ggen marketplace
   - Share patterns with the community
   - Contribute improvements back to ggen

---

## Summary

**MCPs using ggen follow this pattern:**

```
1. Discover: ggen --graph → List 44 capabilities
2. Learn: ggen --capabilities noun verb → Get metadata
3. Plan: Choose which capabilities to use
4. Execute: Call chosen capabilities with validated arguments
5. Adapt: Learn from results for future optimization
6. Scale: Cache, parallel, stream as needed
```

**Key principles:**
- ✅ Always validate arguments before calling ggen
- ✅ Cache capability metadata to avoid repeated discovery
- ✅ Implement error recovery patterns
- ✅ Stream long-running operations
- ✅ Format output for the user's needs

**Your first MCP tool:**
```rust
fn my_ggen_tool(spec: String) -> Result<Output> {
    // 1. Discover & validate
    let metadata = introspection::get_verb_metadata("ai", "generate-ontology")?;

    // 2. Execute
    let ontology = ai::generate_ontology(&spec)?;

    // 3. Return
    Ok(Output::Success(ontology))
}
```

Start with this pattern, add error handling, then scale to multi-step workflows.
