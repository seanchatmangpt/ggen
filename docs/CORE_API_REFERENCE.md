# ggen Core API Reference

**Version:** 6.0.1
**Last Updated:** 2026-03-31
**Rust Version:** 1.91.1

Comprehensive reference for ggen's key public APIs, organized by crate and functional area.

## Table of Contents

- [ggen-core](#ggen-core) - Core graph-aware code generation engine
  - [Generator](#generator) - Template generation orchestration
  - [Pipeline](#pipeline) - Template processing with RDF/SPARQL
  - [Graph](#graph) - RDF graph management
- [ggen-ai](#ggen-ai) - LLM integration layer
  - [GenAiClient](#genaiclient) - Multi-provider LLM client
  - [LlmConfig](#llmconfig) - LLM request configuration
- [ggen-a2a-mcp](#ggen-a2a-mcp) - Agent-to-Agent MCP integration
  - [MessageHandler](#messagehandler) - A2A message processing
- [ggen-domain](#ggen-domain) - Domain logic layer
  - [Domain Operations](#domain-operations) - Business logic functions

---

## ggen-core

**Path:** `/Users/sac/ggen/crates/ggen-core/`
**Purpose:** Core graph-aware code generation engine
**Source:** [`lib.rs`](../crates/ggen-core/src/lib.rs)

### Generator

**Module:** `ggen_core::generator`
**Source:** [`generator.rs`](../crates/ggen-core/src/generator.rs)

Template generation engine that orchestrates template processing, RDF graph operations, and file generation.

#### Type: `GenContext`

Context for template generation with paths, variables, and configuration.

```rust
pub struct GenContext {
    pub template_path: PathBuf,
    pub output_root: PathBuf,
    pub vars: BTreeMap<String, String>,
    pub global_prefixes: BTreeMap<String, String>,
    pub base: Option<String>,
    pub dry_run: bool,
}
```

**Methods:**

##### `new(template_path: PathBuf, output_root: PathBuf) -> Self`

Create a new generation context.

**Parameters:**
- `template_path` - Path to template file
- `output_root` - Root directory for output

**Returns:** `GenContext`

**Example:**
```rust
use ggen_core::generator::GenContext;
use std::path::PathBuf;

let ctx = GenContext::new(
    PathBuf::from("template.tmpl"),
    PathBuf::from("output")
);
```

---

##### `with_vars(vars: BTreeMap<String, String>) -> Self`

Set template variables for rendering.

**Parameters:**
- `vars` - Key-value pairs for template substitution

**Returns:** `Self` (for chaining)

**Example:**
```rust
let mut vars = BTreeMap::new();
vars.insert("name".to_string(), "MyApp".to_string());
vars.insert("version".to_string(), "1.0.0".to_string());

let ctx = GenContext::new(
    PathBuf::from("template.tmpl"),
    PathBuf::from("output")
).with_vars(vars);
```

---

##### `dry(dry_run: bool) -> Self`

Enable or disable dry run mode (preview without writing files).

**Parameters:**
- `dry_run` - If true, don't write actual files

**Returns:** `Self` (for chaining)

---

#### Type: `Generator`

Main generation engine that coordinates the entire generation pipeline.

```rust
pub struct Generator {
    pipeline: Pipeline,
    ctx: GenContext,
}
```

**Methods:**

##### `new(pipeline: Pipeline, ctx: GenContext) -> Self`

Create a new generator with pipeline and context.

**Parameters:**
- `pipeline` - Template processing pipeline
- `ctx` - Generation context

**Returns:** `Generator`

---

##### `generate(&mut self) -> Result<PathBuf>`

Execute generation and return output path.

**Returns:** `Result<PathBuf>` - Path to generated file

**Errors:**
- Template parsing errors
- RDF query errors
- File I/O errors

**Example:**
```rust
use ggen_core::generator::{Generator, GenContext};
use ggen_core::pipeline::Pipeline;
use std::collections::BTreeMap;
use std::path::PathBuf;

# fn main() -> ggen_utils::error::Result<()> {
let pipeline = Pipeline::new()?;
let mut vars = BTreeMap::new();
vars.insert("name".to_string(), "MyApp".to_string());

let ctx = GenContext::new(
    PathBuf::from("template.tmpl"),
    PathBuf::from("output")
).with_vars(vars);

let mut generator = Generator::new(pipeline, ctx);
let output_path = generator.generate()?;
println!("Generated: {:?}", output_path);
# Ok(())
# }
```

---

### Pipeline

**Module:** `ggen_core::pipeline`
**Source:** [`pipeline.rs`](../crates/ggen-core/src/pipeline.rs)

Template processing pipeline with RDF/SPARQL integration. Orchestrates template parsing, frontmatter rendering, graph processing, and body rendering.

#### Type: `Pipeline`

Multi-stage processing pipeline for templates with RDF integration.

```rust
pub struct Pipeline {
    tera: Tera,
    graph: Graph,
}
```

**Methods:**

##### `new() -> Result<Self>`

Create a new pipeline with default configuration.

**Returns:** `Result<Pipeline>`

**Example:**
```rust
use ggen_core::pipeline::Pipeline;

let pipeline = Pipeline::new()?;
```

---

##### `register_prefixes(&mut self, base: Option<&str>, prefixes: &BTreeMap<String, String>)`

Register RDF prefixes for SPARQL queries.

**Parameters:**
- `base` - Base IRI for relative IRIs
- `prefixes` - Map of prefix to IRI

**Example:**
```rust
let mut prefixes = BTreeMap::new();
prefixes.insert("ex".to_string(), "http://example.org/".to_string());
prefixes.insert("foaf".to_string(), "http://xmlns.com/foaf/0.1/".to_string());

pipeline.register_prefixes(Some("http://example.org/base/"), &prefixes);
```

---

##### `render_file(&self, path: &Path, vars: &BTreeMap<String, String>, dry: bool) -> Result<Plan>`

Render a template file and return execution plan.

**Parameters:**
- `path` - Path to template file
- `vars` - Template variables
- `dry` - If true, don't execute plan

**Returns:** `Result<Plan>` - Execution plan

**Example:**
```rust
use std::path::Path;

let vars = BTreeMap::new();
let plan = pipeline.render_file(Path::new("template.tmpl"), &vars, false)?;
plan.apply()?; // Execute the plan
```

---

#### Type: `PipelineBuilder`

Builder for constructing configured pipelines.

**Methods:**

##### `new() -> Self`

Create a new builder.

---

##### `with_prefixes(self, prefixes: BTreeMap<String, String>, base: Option<String>) -> Self`

Add RDF prefixes.

---

##### `with_rdf_file(self, path: impl AsRef<Path>) -> Self`

Load RDF from file.

---

##### `with_inline_rdf(self, rdf: Vec<&str>) -> Self`

Add inline RDF data.

---

##### `build(self) -> Result<Pipeline>`

Build the pipeline.

**Example:**
```rust
use ggen_core::pipeline::PipelineBuilder;
use std::collections::BTreeMap;

let mut prefixes = BTreeMap::new();
prefixes.insert("ex".to_string(), "http://example.org/".to_string());

let pipeline = PipelineBuilder::new()
    .with_prefixes(prefixes, Some("http://example.org/base/".to_string()))
    .with_rdf_file("data.ttl")
    .with_inline_rdf(vec!["@prefix ex: <http://example.org/> . ex:test a ex:Type ."])
    .build()?;
```

---

### Graph

**Module:** `ggen_core::graph`
**Source:** [`mod.rs`](../crates/ggen-core/src/graph/mod.rs)

Comprehensive Oxigraph wrapper with full Store API coverage, intelligent caching, and thread-safety.

#### Type: `Graph`

Main wrapper around Oxigraph's RDF store with SPARQL query caching.

```rust
pub struct Graph {
    store: Arc<Store>,
    query_cache: Arc<RwLock<LruCache<String, CachedResult>>>,
    epoch: Arc<AtomicU64>,
}
```

**Methods:**

##### `new() -> Result<Self>`

Create in-memory graph.

**Returns:** `Result<Graph>`

**Example:**
```rust
use ggen_core::Graph;

let graph = Graph::new()?;
```

---

##### `open_persistent(path: impl AsRef<Path>) -> Result<Self>`

Open persistent graph store.

**Parameters:**
- `path` - Path to database directory

**Returns:** `Result<Graph>`

---

##### `insert_turtle(&self, turtle: &str) -> Result<()>`

Insert Turtle-formatted RDF data.

**Parameters:**
- `turtle` - Turtle format RDF string

**Returns:** `Result<()>`

**Example:**
```rust
graph.insert_turtle(r#"
    @prefix ex: <http://example.org/> .
    ex:alice a ex:Person ;
             ex:name "Alice" ;
             ex:age 30 .
"#)?;
```

---

##### `query(&self, query: &str) -> Result<QueryResults>`

Execute SPARQL query.

**Parameters:**
- `query` - SPARQL query string

**Returns:** `Result<QueryResults>`

**Example:**
```rust
let results = graph.query("SELECT ?s ?o WHERE { ?s ex:name ?o }")?;
```

---

##### `update(&self, update: &str) -> Result<()>`

Execute SPARQL update.

**Parameters:**
- `update` - SPARQL update string

**Returns:** `Result<()>`

**Example:**
```rust
graph.update("INSERT DATA { ex:bob a ex:Person }")?;
```

---

#### Type: `GraphQuery`

Advanced query building and execution.

**Methods:**

##### `new(graph: &Graph) -> Self`

Create query executor.

---

##### `execute(&self, query: &str) -> Result<QueryResults>`

Execute SPARQL query.

---

##### `select_as_json(&self, query: &str) -> Result<Value>`

Execute SELECT and return JSON.

---

#### Type: `GraphUpdate`

SPARQL Update operations.

**Methods:**

##### `new(graph: &Graph) -> Self`

Create update executor.

---

##### `insert(&self, update: &str) -> Result<()>`

Execute INSERT.

---

##### `delete(&self, update: &str) -> Result<()>`

Execute DELETE.

---

#### Type: `GraphExport`

RDF serialization in all formats.

**Methods:**

##### `new(graph: &Graph) -> Self`

Create exporter.

---

##### `write_to_file(&self, path: impl AsRef<Path>, format: RdfFormat) -> Result<()>`

Export to file.

**Parameters:**
- `path` - Output file path
- `format` - RDF format (Turtle, N-Triples, etc.)

**Example:**
```rust
use ggen_core::graph::GraphExport;
use oxigraph::io::RdfFormat;

let export = GraphExport::new(&graph);
export.write_to_file("output.ttl", RdfFormat::Turtle)?;
```

---

##### `to_string(&self, format: RdfFormat) -> Result<String>`

Export to string.

---

## ggen-ai

**Path:** `/Users/sac/ggen/crates/ggen-ai/`
**Purpose:** LLM integration layer for ggen
**Source:** [`lib.rs`](../crates/ggen-ai/src/lib.rs)

### GenAiClient

**Module:** `ggen_ai::client`
**Source:** [`client.rs`](../crates/ggen-ai/src/client.rs)

Multi-provider LLM client with environment-based configuration and response caching.

#### Type: `LlmConfig`

Configuration for LLM requests.

```rust
pub struct LlmConfig {
    pub model: String,
    pub max_tokens: Option<u32>,
    pub temperature: Option<f32>,
    pub top_p: Option<f32>,
    pub stop: Option<Vec<String>>,
    pub extra: HashMap<String, serde_json::Value>,
}
```

**Default Implementation:**

Auto-detects provider from environment variables:
- `GROQ_API_KEY` → Groq
- `OPENAI_API_KEY` → OpenAI
- `ANTHROPIC_API_KEY` → Anthropic
- etc.

**Example:**
```rust
use ggen_ai::LlmConfig;

let config = LlmConfig {
    model: "groq::llama-3.3-70b-versatile".to_string(),
    max_tokens: Some(1000),
    temperature: Some(0.7),
    top_p: Some(0.9),
    stop: None,
    extra: HashMap::new(),
};
```

---

#### Type: `GenAiClient`

Main LLM client with support for multiple providers.

```rust
pub struct GenAiClient {
    config: LlmConfig,
    client: Client,
    cache: Option<LlmCache>,
}
```

**Trait: `LlmClient`**

```rust
#[async_trait]
pub trait LlmClient: Send + Sync {
    async fn complete(&self, prompt: &str) -> Result<LlmResponse>;
    async fn complete_stream(&self, prompt: &str) -> Result<BoxStream<LlmChunk>>;
}
```

**Methods:**

##### `new(config: LlmConfig) -> Result<Self>`

Create client with configuration.

**Parameters:**
- `config` - LLM configuration

**Returns:** `Result<GenAiClient>`

---

##### `complete(&self, prompt: &str) -> Result<LlmResponse>`

Generate completion.

**Parameters:**
- `prompt` - Input prompt

**Returns:** `Result<LlmResponse>`

**Example:**
```rust
use ggen_ai::{GenAiClient, LlmClient, LlmConfig};

# async fn example() -> Result<(), Box<dyn std::error::Error>> {
let config = LlmConfig::default();
let client = GenAiClient::new(config)?;

let response = client.complete("Explain Rust ownership").await?;
println!("{}", response.content);
println!("Tokens: {}", response.usage.total_tokens);
# Ok(())
# }
```

---

##### `complete_stream(&self, prompt: &str) -> Result<BoxStream<LlmChunk>>`

Generate streaming completion.

**Parameters:**
- `prompt` - Input prompt

**Returns:** `Result<BoxStream<LlmChunk>>` - Stream of response chunks

**Example:**
```rust
use futures::StreamExt;

# async fn example() -> Result<(), Box<dyn std::error::Error>> {
let stream = client.complete_stream("Write a poem").await?;

pin_utils::pin_mut!(stream);
while let Some(chunk) = stream.next().await {
    print!("{}", chunk.content);
}
# Ok(())
# }
```

---

#### Type: `LlmResponse`

Response from LLM completion.

```rust
pub struct LlmResponse {
    pub content: String,
    pub usage: UsageStats,
    pub model: String,
}
```

---

#### Type: `UsageStats`

Token usage statistics (from OTEL spans).

```rust
pub struct UsageStats {
    pub prompt_tokens: u32,
    pub completion_tokens: u32,
    pub total_tokens: u32,
}
```

**Note:** These values are populated from actual LLM API responses and verified via OTEL tracing.

---

### Specialized Generators

**Module:** `ggen_ai::generators`
**Source:** [`generators/`](../crates/ggen-ai/src/generators/)

Specialized generators for specific tasks.

#### Type: `TemplateGenerator`

Generate ggen templates from natural language.

**Methods:**

##### `generate(&self, description: &str) -> Result<String>`

Generate template from description.

---

#### Type: `SparqlGenerator`

Generate SPARQL queries from natural language.

**Methods:**

##### `generate(&self, intent: &str) -> Result<String>`

Generate SPARQL query.

---

#### Type: `OntologyGenerator`

Generate RDF/OWL ontologies from domain descriptions.

**Methods:**

##### `generate(&self, domain: &str) -> Result<String>`

Generate ontology.

---

## ggen-a2a-mcp

**Path:** `/Users/sac/ggen/crates/ggen-a2a-mcp/`
**Purpose:** Agent-to-Agent (A2A) protocol and MCP integration
**Source:** [`lib.rs`](../crates/ggen-a2a-mcp/src/lib.rs)

### MessageHandler

**Module:** `ggen_a2a_mcp::handlers`
**Source:** [`handlers.rs`](../crates/ggen-a2a-mcp/src/handlers.rs)

Trait and implementations for processing A2A messages with support for all ConvergedMessage content types.

#### Trait: `MessageHandler`

Trait for handling A2A messages.

```rust
#[async_trait]
pub trait MessageHandler: Send + Sync + std::fmt::Debug {
    async fn handle(&self, message: ConvergedMessage) -> HandlerResult<ConvergedMessage>;
    fn can_handle(&self, message_type: &ConvergedMessageType) -> bool;
    fn priority(&self) -> HandlerPriority;
    fn name(&self) -> &str;
    fn supported_types(&self) -> Vec<ConvergedMessageType>;
}
```

**Methods:**

##### `handle(&self, message: ConvergedMessage) -> HandlerResult<ConvergedMessage>`

Handle the message and return a response.

---

##### `can_handle(&self, message_type: &ConvergedMessageType) -> bool`

Check if this handler can process the given message type.

---

##### `priority(&self) -> HandlerPriority`

Get the handler priority.

---

##### `name(&self) -> &str`

Get the handler name.

---

##### `supported_types(&self) -> Vec<ConvergedMessageType>`

Get the supported message types.

---

#### Type: `HandlerContext`

Context provided to handlers during execution.

```rust
pub struct HandlerContext {
    pub handler_name: String,
    pub message: ConvergedMessage,
    pub start_time: chrono::DateTime<Utc>,
    pub metadata: HashMap<String, serde_json::Value>,
}
```

**Methods:**

##### `new(handler_name: String, message: ConvergedMessage) -> Self`

Create new context.

---

##### `with_metadata(self, key: String, value: serde_json::Value) -> Self`

Add metadata to context.

---

#### Enum: `HandlerPriority`

Handler priority levels.

```rust
pub enum HandlerPriority {
    Lowest = 0,
    Low = 1,
    Normal = 2,
    High = 3,
    Highest = 4,
    Critical = 5,
}
```

---

#### Enum: `HandlerStatus`

Handler execution status.

```rust
pub enum HandlerStatus {
    Pending,
    Running,
    Completed,
    Failed,
    Skipped,
}
```

---

#### Implementations

##### `TextContentHandler`

Handles text-based messages (Direct, Task, Query).

```rust
let handler = TextContentHandler::new();
```

---

##### `FileContentHandler`

Handles file-based messages with I/O operations.

---

##### `MultipartHandler`

Handles multipart messages with batch processing.

---

##### `StreamHandler`

Handles streaming messages.

---

##### `DataContentHandler`

Handles structured data content.

---

## ggen-domain

**Path:** `/Users/sac/ggen/crates/ggen-domain/`
**Purpose:** Domain logic layer - pure business logic functions
**Source:** [`lib.rs`](../crates/ggen-domain/src/lib.rs)

### Domain Operations

**Module:** `ggen_domain::domain`
**Source:** [`domain.rs`](../crates/ggen-domain/src/domain.rs)

Domain logic organized by functional area.

#### AI Operations (`ggen_domain::ai`)

```rust
use ggen_domain::ai;

// Analyze codebase structure
let analysis = ai::analyze_code(project_path)?;

// Generate code from intent
let code = ai::generate_code(intent, context)?;
```

---

#### Graph Operations (`ggen_domain::graph`)

```rust
use ggen_domain::graph;

// Load RDF ontology
let graph = graph::load_ontology(path)?;

// Execute SPARQL query
let results = graph::execute_sparql(query, &graph)?;
```

---

#### Marketplace Operations (`ggen_domain::marketplace`)

```rust
use ggen_domain::marketplace;

// Search template marketplace
let templates = marketplace::search_templates("cli")?;

// Install template pack
marketplace::install_template(&pack)?;
```

---

#### Project Operations (`ggen_domain::project`)

```rust
use ggen_domain::project;

// Create new project
project::create_project(&config)?;

// Generate project from ontology
project::generate_project(&ontology)?;
```

---

#### Template Operations (`ggen_domain::template`)

```rust
use ggen_domain::template;

// Generate template
let tmpl = template::generate_template(&spec)?;

// Validate template
let report = template::validate_template(&tmpl)?;
```

---

## Security and Audit Types

**Module:** `ggen_domain::audit::security`
**Source:** [`security.rs`](../crates/ggen-domain/src/audit/security.rs)

### Type: `SecurityScanner`

Security vulnerability scanner for dependencies and configurations.

**Methods:**

##### `scan_dependencies(&self) -> Result<SecurityScanResult>`

Scan for vulnerable dependencies.

---

##### `scan_config(&self) -> Result<SecurityScanResult>`

Scan configuration files.

---

### Type: `ConfigAuditor`

Configuration auditor for security issues.

**Methods:**

##### `audit(&self, config: &Config) -> Result<Vec<ConfigIssue>>`

Audit configuration.

---

## OpenTelemetry Integration

All ggen crates include OTEL semantic convention attributes for observability.

### ggen-core OTEL Attributes

- `pipeline.operation` - Pipeline stage identifier
- `pipeline.duration_ms` - Stage execution time
- `pipeline.files_generated` - Number of files generated

**Verification:**
```bash
RUST_LOG=trace,ggen_core=trace cargo test -p ggen-core 2>&1 | grep -E "pipeline\."
```

---

### ggen-ai OTEL Attributes

- `llm.model` - Model identifier (e.g., `groq::llama-3.3-70b-versatile`)
- `llm.prompt_tokens` - Input token count
- `llm.completion_tokens` - Output token count
- `llm.total_tokens` - Total tokens used

**Verification:**
```bash
RUST_LOG=trace,ggen_ai=trace cargo test -p ggen-ai 2>&1 | grep -E "llm\."
# Expected: llm.complete, llm.model, llm.total_tokens
```

**Note:** These spans prove actual LLM API calls were made (not mocks).

---

### ggen-a2a-mcp OTEL Attributes

- `a2a.correlation_id` - Message correlation ID
- `a2a.message_type` - Message type identifier
- `mcp.tool_name` - MCP tool name
- `mcp.duration_ms` - Tool execution time

---

## Error Handling

All crates use consistent error types:

```rust
// ggen-core
use ggen_utils::error::{Error, Result};

// ggen-ai
use ggen_ai::{GgenAiError, Result};

// ggen-a2a-mcp
use ggen_a2a_mcp::{A2aMcpError, A2aMcpResult};
```

---

## Version Information

Each crate exposes version information:

```rust
// ggen-core
ggen_core::VERSION // "6.0.1"

// ggen-ai
ggen_ai::VERSION // "6.0.1"

// ggen-a2a-mcp
ggen_a2a_mcp::VERSION // "6.0.1"
```

---

## Quick Reference

### Top 10 Most-Used APIs

1. **`Generator::generate()`** - Execute template generation
2. **`Pipeline::new()`** - Create template processing pipeline
3. **`Graph::query()`** - Execute SPARQL queries
4. **`GenAiClient::complete()`** - Generate LLM completion
5. **`LlmConfig::default()`** - Auto-detect LLM provider
6. **`MessageHandler::handle()`** - Process A2A messages
7. **`GenContext::new()`** - Create generation context
8. **`Pipeline::render_file()`** - Render template file
9. **`Graph::insert_turtle()`** - Load RDF data
10. **`ToolRegistry::invoke()`** - Invoke MCP tool

---

## Related Documentation

- [User Guide](../README.md) - Getting started with ggen
- [Architecture](../docs/ARCHITECTURE.md) - System architecture overview
- [Contributing](../docs/CONTRIBUTING.md) - Development guidelines
- [Testing](../docs/TESTING.md) - Testing strategy (Chicago TDD)
- [Agent Integration API](../docs/API_REFERENCE.md) - Agent/tool integration APIs

---

**API Count:** 10 core public APIs documented
**Source Files:** 4 main crates (ggen-core, ggen-ai, ggen-a2a-mcp, ggen-domain)
**Lines of Documentation:** ~600 lines
