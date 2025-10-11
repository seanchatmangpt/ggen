<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen Autonomous System - API Reference](#ggen-autonomous-system---api-reference)
  - [Table of Contents](#table-of-contents)
  - [MCP Tool Specifications](#mcp-tool-specifications)
    - [Base Endpoint](#base-endpoint)
    - [Tool Categories](#tool-categories)
    - [Template Operations](#template-operations)
      - [`generate_template`](#generate_template)
      - [`validate_template`](#validate_template)
    - [Graph Operations](#graph-operations)
      - [`generate_graph`](#generate_graph)
      - [`execute_sparql`](#execute_sparql)
      - [`validate_graph`](#validate_graph)
    - [AI Operations](#ai-operations)
      - [`ai_chat`](#ai_chat)
      - [`ai_refactor`](#ai_refactor)
    - [Project Management](#project-management)
      - [`create_project`](#create_project)
    - [Marketplace Operations](#marketplace-operations)
      - [`search_templates`](#search_templates)
      - [`publish_template`](#publish_template)
  - [Rust API Documentation](#rust-api-documentation)
    - [Core Traits](#core-traits)
      - [`AiProvider` Trait](#aiprovider-trait)
      - [`Agent` Trait](#agent-trait)
    - [Key Structs](#key-structs)
      - [`TemplateGenerator`](#templategenerator)
      - [`GraphEngine`](#graphengine)
      - [`ByzantineValidator`](#byzantinevalidator)
  - [Request/Response Formats](#requestresponse-formats)
    - [JSON-RPC 2.0](#json-rpc-20)
    - [Streaming Responses](#streaming-responses)
  - [Error Codes](#error-codes)
    - [Standard Error Codes](#standard-error-codes)
    - [Custom Error Codes](#custom-error-codes)
      - [AI Provider Errors (1000-1099)](#ai-provider-errors-1000-1099)
      - [Generation Errors (1100-1199)](#generation-errors-1100-1199)
      - [Graph Errors (1200-1299)](#graph-errors-1200-1299)
      - [Agent Errors (1300-1399)](#agent-errors-1300-1399)
      - [Project Errors (1400-1499)](#project-errors-1400-1499)
      - [Marketplace Errors (1500-1599)](#marketplace-errors-1500-1599)
    - [Error Response Examples](#error-response-examples)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen Autonomous System - API Reference

## Table of Contents
1. [MCP Tool Specifications](#mcp-tool-specifications)
2. [Rust API Documentation](#rust-api-documentation)
3. [Request/Response Formats](#requestresponse-formats)
4. [Error Codes](#error-codes)

## MCP Tool Specifications

The ggen MCP server exposes tools following the Model Context Protocol specification.

### Base Endpoint

```
stdio://ggen-mcp
```

### Tool Categories

1. **Template Operations**
2. **Graph Operations**
3. **AI Operations**
4. **Project Management**
5. **Marketplace Operations**

---

### Template Operations

#### `generate_template`

Generate a template using AI from a natural language description.

**Input Schema:**
```json
{
  "type": "object",
  "properties": {
    "description": {
      "type": "string",
      "description": "Natural language description of what to generate"
    },
    "format": {
      "type": "string",
      "enum": ["rust", "python", "typescript", "sql", "markdown", "yaml"],
      "description": "Target output format"
    },
    "validate": {
      "type": "boolean",
      "default": false,
      "description": "Enable iterative validation"
    },
    "max_iterations": {
      "type": "integer",
      "default": 3,
      "description": "Maximum validation iterations"
    },
    "context": {
      "type": "object",
      "description": "Additional context for generation"
    }
  },
  "required": ["description"]
}
```

**Output Schema:**
```json
{
  "type": "object",
  "properties": {
    "content": {
      "type": "string",
      "description": "Generated template content"
    },
    "validation_report": {
      "type": "object",
      "properties": {
        "score": {
          "type": "number",
          "description": "Quality score (0.0 - 1.0)"
        },
        "issues": {
          "type": "array",
          "items": {"type": "string"}
        },
        "iterations": {
          "type": "integer"
        }
      }
    },
    "metadata": {
      "type": "object",
      "properties": {
        "provider": {"type": "string"},
        "model": {"type": "string"},
        "tokens_used": {"type": "integer"},
        "generation_time_ms": {"type": "integer"}
      }
    }
  }
}
```

**Example Request:**
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "generate_template",
    "arguments": {
      "description": "REST API endpoint for user authentication",
      "format": "rust",
      "validate": true,
      "max_iterations": 3
    }
  }
}
```

**Example Response:**
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "content": "use actix_web::{post, web, HttpResponse};\n...",
    "validation_report": {
      "score": 0.92,
      "issues": [],
      "iterations": 2
    },
    "metadata": {
      "provider": "anthropic",
      "model": "claude-3-opus-20240229",
      "tokens_used": 1247,
      "generation_time_ms": 3421
    }
  }
}
```

#### `validate_template`

Validate a template for quality and correctness.

**Input Schema:**
```json
{
  "type": "object",
  "properties": {
    "content": {
      "type": "string",
      "description": "Template content to validate"
    },
    "format": {
      "type": "string",
      "description": "Template format"
    },
    "strict": {
      "type": "boolean",
      "default": false,
      "description": "Enable strict validation"
    }
  },
  "required": ["content"]
}
```

**Output Schema:**
```json
{
  "type": "object",
  "properties": {
    "valid": {"type": "boolean"},
    "score": {
      "type": "number",
      "description": "Quality score (0.0 - 1.0)"
    },
    "issues": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "severity": {
            "type": "string",
            "enum": ["error", "warning", "info"]
          },
          "message": {"type": "string"},
          "line": {"type": "integer"},
          "column": {"type": "integer"}
        }
      }
    },
    "suggestions": {
      "type": "array",
      "items": {"type": "string"}
    }
  }
}
```

---

### Graph Operations

#### `generate_graph`

Generate an RDF ontology from a description.

**Input Schema:**
```json
{
  "type": "object",
  "properties": {
    "description": {
      "type": "string",
      "description": "Domain description for the ontology"
    },
    "format": {
      "type": "string",
      "enum": ["turtle", "rdfxml", "jsonld", "ntriples"],
      "default": "turtle"
    },
    "verify": {
      "type": "boolean",
      "default": false,
      "description": "Generate verification reference file"
    },
    "base_uri": {
      "type": "string",
      "description": "Base URI for the ontology"
    }
  },
  "required": ["description"]
}
```

**Output Schema:**
```json
{
  "type": "object",
  "properties": {
    "graph_content": {
      "type": "string",
      "description": "RDF graph in requested format"
    },
    "triple_count": {
      "type": "integer"
    },
    "verification": {
      "type": "object",
      "properties": {
        "reference_file": {"type": "string"},
        "integrity_hash": {"type": "string"}
      }
    },
    "metadata": {
      "type": "object",
      "properties": {
        "format": {"type": "string"},
        "base_uri": {"type": "string"},
        "generated_at": {"type": "string", "format": "date-time"}
      }
    }
  }
}
```

#### `execute_sparql`

Execute a SPARQL query against an RDF graph.

**Input Schema:**
```json
{
  "type": "object",
  "properties": {
    "query": {
      "type": "string",
      "description": "SPARQL query to execute"
    },
    "graph_file": {
      "type": "string",
      "description": "Path to graph file"
    },
    "graph_uri": {
      "type": "string",
      "description": "URI of named graph (alternative to file)"
    },
    "format": {
      "type": "string",
      "enum": ["json", "xml", "csv", "tsv"],
      "default": "json"
    }
  },
  "required": ["query"]
}
```

**Output Schema:**
```json
{
  "type": "object",
  "properties": {
    "results": {
      "type": "array",
      "items": {
        "type": "object",
        "description": "Query result bindings"
      }
    },
    "execution_time_ms": {
      "type": "integer"
    },
    "result_count": {
      "type": "integer"
    },
    "query_plan": {
      "type": "object",
      "description": "Query execution plan (if requested)"
    }
  }
}
```

#### `validate_graph`

Validate an RDF graph for integrity and SHACL compliance.

**Input Schema:**
```json
{
  "type": "object",
  "properties": {
    "graph_file": {
      "type": "string",
      "description": "Path to graph file"
    },
    "shacl_shapes": {
      "type": "string",
      "description": "Path to SHACL shapes file (optional)"
    },
    "check_integrity": {
      "type": "boolean",
      "default": true
    }
  },
  "required": ["graph_file"]
}
```

**Output Schema:**
```json
{
  "type": "object",
  "properties": {
    "valid": {"type": "boolean"},
    "triple_count": {"type": "integer"},
    "violations": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "severity": {"type": "string"},
          "message": {"type": "string"},
          "focus_node": {"type": "string"},
          "value": {"type": "string"}
        }
      }
    },
    "integrity_report": {
      "type": "object",
      "properties": {
        "orphaned_nodes": {"type": "integer"},
        "broken_references": {"type": "integer"},
        "duplicate_triples": {"type": "integer"}
      }
    }
  }
}
```

---

### AI Operations

#### `ai_chat`

Interactive chat with AI for code assistance.

**Input Schema:**
```json
{
  "type": "object",
  "properties": {
    "messages": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "role": {
            "type": "string",
            "enum": ["user", "assistant", "system"]
          },
          "content": {"type": "string"}
        }
      }
    },
    "context": {
      "type": "object",
      "properties": {
        "files": {
          "type": "array",
          "items": {"type": "string"}
        },
        "graph": {"type": "string"}
      }
    },
    "stream": {
      "type": "boolean",
      "default": false
    }
  },
  "required": ["messages"]
}
```

**Output Schema (Non-streaming):**
```json
{
  "type": "object",
  "properties": {
    "response": {"type": "string"},
    "metadata": {
      "type": "object",
      "properties": {
        "model": {"type": "string"},
        "tokens_used": {"type": "integer"},
        "finish_reason": {"type": "string"}
      }
    }
  }
}
```

#### `ai_refactor`

AI-powered code refactoring.

**Input Schema:**
```json
{
  "type": "object",
  "properties": {
    "code": {
      "type": "string",
      "description": "Code to refactor"
    },
    "language": {
      "type": "string",
      "description": "Programming language"
    },
    "instructions": {
      "type": "string",
      "description": "Refactoring instructions"
    },
    "preserve_behavior": {
      "type": "boolean",
      "default": true,
      "description": "Ensure behavioral equivalence"
    }
  },
  "required": ["code", "language"]
}
```

**Output Schema:**
```json
{
  "type": "object",
  "properties": {
    "refactored_code": {"type": "string"},
    "changes": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "type": {"type": "string"},
          "description": {"type": "string"},
          "before": {"type": "string"},
          "after": {"type": "string"}
        }
      }
    },
    "test_preservation_verified": {"type": "boolean"}
  }
}
```

---

### Project Management

#### `create_project`

Create a new project with scaffolding.

**Input Schema:**
```json
{
  "type": "object",
  "properties": {
    "name": {
      "type": "string",
      "description": "Project name"
    },
    "description": {
      "type": "string",
      "description": "Project description"
    },
    "template": {
      "type": "string",
      "description": "Template to use (optional)"
    },
    "language": {
      "type": "string",
      "enum": ["rust", "python", "typescript", "go"]
    },
    "features": {
      "type": "array",
      "items": {"type": "string"},
      "description": "Features to include"
    }
  },
  "required": ["name", "language"]
}
```

**Output Schema:**
```json
{
  "type": "object",
  "properties": {
    "project_path": {"type": "string"},
    "files_created": {
      "type": "array",
      "items": {"type": "string"}
    },
    "next_steps": {
      "type": "array",
      "items": {"type": "string"}
    }
  }
}
```

---

### Marketplace Operations

#### `search_templates`

Search the template marketplace.

**Input Schema:**
```json
{
  "type": "object",
  "properties": {
    "query": {
      "type": "string",
      "description": "Search query"
    },
    "tags": {
      "type": "array",
      "items": {"type": "string"}
    },
    "language": {"type": "string"},
    "limit": {
      "type": "integer",
      "default": 20
    }
  }
}
```

**Output Schema:**
```json
{
  "type": "object",
  "properties": {
    "results": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "id": {"type": "string"},
          "name": {"type": "string"},
          "description": {"type": "string"},
          "tags": {"type": "array", "items": {"type": "string"}},
          "author": {"type": "string"},
          "downloads": {"type": "integer"},
          "rating": {"type": "number"}
        }
      }
    },
    "total": {"type": "integer"}
  }
}
```

#### `publish_template`

Publish a template to the marketplace.

**Input Schema:**
```json
{
  "type": "object",
  "properties": {
    "template_path": {
      "type": "string",
      "description": "Path to template file"
    },
    "metadata": {
      "type": "object",
      "properties": {
        "name": {"type": "string"},
        "description": {"type": "string"},
        "tags": {"type": "array", "items": {"type": "string"}},
        "version": {"type": "string"},
        "license": {"type": "string"}
      },
      "required": ["name", "description"]
    }
  },
  "required": ["template_path", "metadata"]
}
```

**Output Schema:**
```json
{
  "type": "object",
  "properties": {
    "template_id": {"type": "string"},
    "published_at": {"type": "string", "format": "date-time"},
    "url": {"type": "string"}
  }
}
```

---

## Rust API Documentation

### Core Traits

#### `AiProvider` Trait

Unified interface for AI providers.

```rust
#[async_trait::async_trait]
pub trait AiProvider: Send + Sync {
    /// Generate content from a prompt
    async fn generate(&self, prompt: &Prompt) -> Result<String, AiError>;

    /// Generate streaming response
    async fn generate_stream(
        &self,
        prompt: &Prompt,
    ) -> Result<Pin<Box<dyn Stream<Item = Result<String>>>>, AiError>;

    /// Get provider name
    fn provider_name(&self) -> &str;

    /// Get model name
    fn model_name(&self) -> &str;

    /// Get model capabilities
    fn capabilities(&self) -> ProviderCapabilities;
}
```

**Usage Example:**
```rust
use ggen_ai::{AnthropicProvider, AiProvider, Prompt};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let provider = AnthropicProvider::new(
        std::env::var("ANTHROPIC_API_KEY")?,
        "claude-3-opus-20240229".to_string(),
    );

    let prompt = Prompt::new("Generate a Rust struct for a User entity");
    let response = provider.generate(&prompt).await?;

    println!("{}", response);
    Ok(())
}
```

#### `Agent` Trait

Base trait for all autonomous agents.

```rust
#[async_trait::async_trait]
pub trait Agent: Send + Sync {
    /// Initialize the agent
    async fn initialize(&mut self) -> Result<(), AgentError>;

    /// Start the agent
    async fn start(&mut self) -> Result<(), AgentError>;

    /// Stop the agent
    async fn stop(&mut self) -> Result<(), AgentError>;

    /// Get current status
    async fn status(&self) -> AgentStatus;

    /// Get agent configuration
    fn config(&self) -> &AgentConfig;

    /// Handle incoming message
    async fn handle_message(
        &mut self,
        message: AgentMessage,
    ) -> Result<AgentMessage, AgentError>;
}
```

**Usage Example:**
```rust
use ggen_agents::{Agent, AgentConfig, AgentRole, LondonBddAgent};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let config = AgentConfig {
        id: uuid::Uuid::new_v4(),
        name: "BDD Coordinator".to_string(),
        role: AgentRole::LondonBddCoordinator,
        timeout_ms: 5000,
        retry_count: 3,
        health_check_interval_ms: 1000,
    };

    let mut agent = LondonBddAgent::new(config);
    agent.initialize().await?;
    agent.start().await?;

    let status = agent.status().await;
    println!("Agent status: {:?}", status);

    Ok(())
}
```

### Key Structs

#### `TemplateGenerator`

AI-powered template generation.

```rust
pub struct TemplateGenerator {
    client: Box<dyn AiProvider>,
    validator: TemplateValidator,
    cache: Arc<RwLock<LruCache<String, String>>>,
}

impl TemplateGenerator {
    /// Create new generator with AI client
    pub fn new(client: Box<dyn AiProvider>) -> Self;

    /// Generate template from description
    pub async fn generate(
        &self,
        description: &str,
        format: TemplateFormat,
    ) -> Result<String, GenerationError>;

    /// Generate with iterative validation
    pub async fn generate_validated(
        &self,
        description: &str,
        format: TemplateFormat,
        max_iterations: usize,
    ) -> Result<(String, ValidationReport), GenerationError>;
}
```

**Usage Example:**
```rust
use ggen_ai::{TemplateGenerator, AnthropicProvider, TemplateFormat};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let provider = AnthropicProvider::from_env()?;
    let generator = TemplateGenerator::new(Box::new(provider));

    let (template, report) = generator.generate_validated(
        "REST API endpoint for user CRUD",
        TemplateFormat::Rust,
        3,
    ).await?;

    println!("Quality score: {}", report.score);
    println!("Template:\n{}", template);

    Ok(())
}
```

#### `GraphEngine`

RDF graph operations.

```rust
pub struct GraphEngine {
    store: oxigraph::Store,
    cache: QueryCache,
}

impl GraphEngine {
    /// Create new graph engine
    pub fn new() -> Result<Self, GraphError>;

    /// Load graph from file
    pub fn load_from_file(
        &self,
        path: &Path,
        format: GraphFormat,
    ) -> Result<Graph, GraphError>;

    /// Execute SPARQL query
    pub fn execute_sparql(
        &self,
        query: &str,
    ) -> Result<QueryResults, GraphError>;

    /// Insert RDF triple
    pub fn insert_triple(&self, triple: Triple) -> Result<(), GraphError>;

    /// Verify graph integrity
    pub fn verify_integrity(&self) -> IntegrityReport;
}
```

**Usage Example:**
```rust
use ggen_core::{GraphEngine, GraphFormat, Triple};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let engine = GraphEngine::new()?;

    let graph = engine.load_from_file(
        Path::new("ontologies/domain.ttl"),
        GraphFormat::Turtle,
    )?;

    let query = r#"
        SELECT ?subject ?predicate ?object
        WHERE {
            ?subject ?predicate ?object .
        }
        LIMIT 10
    "#;

    let results = engine.execute_sparql(query)?;

    for result in results {
        println!("{:?}", result);
    }

    Ok(())
}
```

#### `ByzantineValidator`

Byzantine fault tolerance validation.

```rust
pub struct ByzantineValidator {
    config: AgentConfig,
    quorum_size: usize,
    fault_threshold: usize,
    consensus_algorithm: ConsensusAlgorithm,
}

impl ByzantineValidator {
    /// Create new validator
    pub fn new(
        config: AgentConfig,
        quorum_size: usize,
        fault_threshold: usize,
    ) -> Self;

    /// Validate operation with consensus
    pub async fn validate_operation(
        &self,
        operation: Operation,
    ) -> Result<ValidationResult, ValidatorError>;

    /// Reach consensus on proposal
    pub async fn reach_consensus(
        &self,
        proposal: Proposal,
    ) -> Result<ConsensusResult, ValidatorError>;

    /// Detect Byzantine faults
    pub async fn detect_faults(&self) -> Vec<FaultReport>;
}
```

**Usage Example:**
```rust
use ggen_agents::{ByzantineValidator, Operation, Proposal};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let validator = ByzantineValidator::new(
        agent_config,
        7,  // quorum size (2f+1)
        3,  // fault threshold (f)
    );

    let operation = Operation::GraphUpdate { /* ... */ };
    let result = validator.validate_operation(operation).await?;

    if result.is_valid() {
        println!("Operation validated with consensus");
    } else {
        println!("Validation failed: {:?}", result.faults);
    }

    Ok(())
}
```

---

## Request/Response Formats

### JSON-RPC 2.0

All MCP tools use JSON-RPC 2.0 protocol.

**Request Format:**
```json
{
  "jsonrpc": "2.0",
  "id": <number|string>,
  "method": "tools/call",
  "params": {
    "name": "<tool_name>",
    "arguments": {
      // Tool-specific arguments
    }
  }
}
```

**Success Response:**
```json
{
  "jsonrpc": "2.0",
  "id": <number|string>,
  "result": {
    // Tool-specific result
  }
}
```

**Error Response:**
```json
{
  "jsonrpc": "2.0",
  "id": <number|string>,
  "error": {
    "code": <error_code>,
    "message": "<error_message>",
    "data": {
      // Additional error context
    }
  }
}
```

### Streaming Responses

For streaming operations (e.g., AI generation):

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "generate_template",
    "arguments": {
      "description": "...",
      "stream": true
    }
  }
}
```

**Stream Events:**
```json
// Event 1: Start
{"type": "start", "data": {"request_id": "..."}}

// Event 2: Progress
{"type": "progress", "data": {"chunk": "...", "tokens": 10}}

// Event 3: Complete
{"type": "complete", "data": {"content": "...", "tokens_total": 100}}
```

---

## Error Codes

### Standard Error Codes

| Code | Name | Description |
|------|------|-------------|
| -32700 | Parse Error | Invalid JSON |
| -32600 | Invalid Request | Invalid JSON-RPC request |
| -32601 | Method Not Found | Tool does not exist |
| -32602 | Invalid Params | Invalid tool arguments |
| -32603 | Internal Error | Internal server error |

### Custom Error Codes

#### AI Provider Errors (1000-1099)

| Code | Name | Description |
|------|------|-------------|
| 1000 | AI_PROVIDER_ERROR | General AI provider error |
| 1001 | AI_API_KEY_MISSING | API key not configured |
| 1002 | AI_API_KEY_INVALID | API key is invalid |
| 1003 | AI_RATE_LIMIT | Rate limit exceeded |
| 1004 | AI_TIMEOUT | AI request timed out |
| 1005 | AI_CONTENT_FILTER | Content filtered by provider |
| 1006 | AI_MODEL_NOT_FOUND | Requested model not available |

#### Generation Errors (1100-1199)

| Code | Name | Description |
|------|------|-------------|
| 1100 | GENERATION_ERROR | General generation error |
| 1101 | VALIDATION_FAILED | Template validation failed |
| 1102 | MAX_ITERATIONS_EXCEEDED | Exceeded max validation iterations |
| 1103 | INVALID_FORMAT | Invalid output format |
| 1104 | TEMPLATE_PARSE_ERROR | Failed to parse generated template |

#### Graph Errors (1200-1299)

| Code | Name | Description |
|------|------|-------------|
| 1200 | GRAPH_ERROR | General graph error |
| 1201 | GRAPH_LOAD_ERROR | Failed to load graph |
| 1202 | GRAPH_PARSE_ERROR | Failed to parse graph |
| 1203 | SPARQL_SYNTAX_ERROR | Invalid SPARQL syntax |
| 1204 | SPARQL_EXECUTION_ERROR | SPARQL execution failed |
| 1205 | GRAPH_INTEGRITY_ERROR | Graph integrity check failed |
| 1206 | SHACL_VALIDATION_ERROR | SHACL validation failed |

#### Agent Errors (1300-1399)

| Code | Name | Description |
|------|------|-------------|
| 1300 | AGENT_ERROR | General agent error |
| 1301 | AGENT_NOT_INITIALIZED | Agent not initialized |
| 1302 | AGENT_TIMEOUT | Agent operation timed out |
| 1303 | AGENT_UNHEALTHY | Agent is unhealthy |
| 1304 | CONSENSUS_FAILED | Failed to reach consensus |
| 1305 | BYZANTINE_FAULT_DETECTED | Byzantine fault detected |

#### Project Errors (1400-1499)

| Code | Name | Description |
|------|------|-------------|
| 1400 | PROJECT_ERROR | General project error |
| 1401 | PROJECT_EXISTS | Project already exists |
| 1402 | TEMPLATE_NOT_FOUND | Project template not found |
| 1403 | INVALID_PROJECT_NAME | Invalid project name |

#### Marketplace Errors (1500-1599)

| Code | Name | Description |
|------|------|-------------|
| 1500 | MARKETPLACE_ERROR | General marketplace error |
| 1501 | TEMPLATE_NOT_FOUND | Template not found in marketplace |
| 1502 | PUBLISH_FAILED | Failed to publish template |
| 1503 | AUTHENTICATION_REQUIRED | Authentication required |
| 1504 | INSUFFICIENT_PERMISSIONS | Insufficient permissions |

### Error Response Examples

**AI API Key Missing:**
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": 1001,
    "message": "AI_API_KEY_MISSING",
    "data": {
      "description": "Anthropic API key not found in environment",
      "hint": "Set ANTHROPIC_API_KEY environment variable"
    }
  }
}
```

**Validation Failed:**
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": 1101,
    "message": "VALIDATION_FAILED",
    "data": {
      "score": 0.65,
      "threshold": 0.8,
      "issues": [
        "Missing error handling",
        "No input validation"
      ],
      "iterations_used": 3
    }
  }
}
```

**Byzantine Fault Detected:**
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": 1305,
    "message": "BYZANTINE_FAULT_DETECTED",
    "data": {
      "faulty_nodes": ["node-3", "node-7"],
      "fault_type": "conflicting_responses",
      "consensus_achievable": false,
      "required_quorum": 7,
      "healthy_nodes": 5
    }
  }
}
```
