<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen MCP Usage Guide](#ggen-mcp-usage-guide)
  - [📋 Table of Contents](#-table-of-contents)
  - [Introduction](#introduction)
    - [How It Works](#how-it-works)
  - [Transport Options Deep Dive](#transport-options-deep-dive)
    - [Stdio Transport](#stdio-transport)
    - [SSE Transport](#sse-transport)
    - [HTTP Transport](#http-transport)
  - [Tool Categories and Workflows](#tool-categories-and-workflows)
    - [Workflow 1: Template Discovery → Generation](#workflow-1-template-discovery-%E2%86%92-generation)
    - [Workflow 2: RDF-Driven Multi-File Generation](#workflow-2-rdf-driven-multi-file-generation)
    - [Workflow 3: Idempotent File Injection](#workflow-3-idempotent-file-injection)
    - [Workflow 4: Deterministic Multi-Language Generation](#workflow-4-deterministic-multi-language-generation)
  - [Real-World Conversation Examples](#real-world-conversation-examples)
    - [Example 1: Building a REST API from Scratch](#example-1-building-a-rest-api-from-scratch)
    - [Example 2: Migrating Legacy Code](#example-2-migrating-legacy-code)
    - [Example 3: Generating Documentation](#example-3-generating-documentation)
  - [Error Handling](#error-handling)
    - [Client-Side Error Handling](#client-side-error-handling)
      - [Rust Client](#rust-client)
      - [Python Client](#python-client)
    - [Server-Side Error Handling](#server-side-error-handling)
  - [Performance Tuning](#performance-tuning)
    - [Benchmarking](#benchmarking)
    - [Optimization Strategies](#optimization-strategies)
      - [1. Template Compilation](#1-template-compilation)
      - [2. SPARQL Query Caching](#2-sparql-query-caching)
      - [3. Parallel Batch Generation](#3-parallel-batch-generation)
      - [4. Graph Indexing](#4-graph-indexing)
      - [5. Streaming Large Outputs](#5-streaming-large-outputs)
  - [Advanced Usage Patterns](#advanced-usage-patterns)
    - [Pattern 1: Multi-Stage Generation Pipeline](#pattern-1-multi-stage-generation-pipeline)
    - [Pattern 2: Conditional Template Selection](#pattern-2-conditional-template-selection)
    - [Pattern 3: Incremental Graph Building](#pattern-3-incremental-graph-building)
    - [Pattern 4: Template Hot-Reloading](#pattern-4-template-hot-reloading)
  - [Security Best Practices](#security-best-practices)
    - [1. Input Validation](#1-input-validation)
    - [2. Template Sandboxing](#2-template-sandboxing)
    - [3. Graph Access Control](#3-graph-access-control)
    - [4. Rate Limiting](#4-rate-limiting)
    - [5. Audit Logging](#5-audit-logging)
  - [Troubleshooting Guide](#troubleshooting-guide)
    - [Issue: "Template rendering is slow"](#issue-template-rendering-is-slow)
    - [Issue: "SPARQL queries return empty results"](#issue-sparql-queries-return-empty-results)
    - [Issue: "Marketplace connection fails"](#issue-marketplace-connection-fails)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen MCP Usage Guide

A comprehensive guide to using the ggen MCP server for AI-assisted code generation with practical workflows, examples, and best practices.

## 📋 Table of Contents

- [Introduction](#introduction)
- [Transport Options Deep Dive](#transport-options-deep-dive)
- [Tool Categories and Workflows](#tool-categories-and-workflows)
- [Real-World Conversation Examples](#real-world-conversation-examples)
- [Error Handling](#error-handling)
- [Performance Tuning](#performance-tuning)
- [Advanced Usage Patterns](#advanced-usage-patterns)
- [Security Best Practices](#security-best-practices)

---

## Introduction

The ggen MCP server transforms how AI assistants generate code by providing:

1. **Deterministic Generation** - Reproducible outputs with RDF backing
2. **Semantic Context** - SPARQL queries for intelligent code generation
3. **Template Marketplace** - Reusable, versioned code templates
4. **Multi-Language Support** - Generate any language from one ontology

### How It Works

```
┌─────────────────┐
│   AI Assistant  │
│    (Claude)     │
└────────┬────────┘
         │ MCP Protocol
         ▼
┌─────────────────┐      ┌──────────────┐
│  ggen MCP       │◄────►│ RDF Graph    │
│  Server         │      │ (SPARQL)     │
└────────┬────────┘      └──────────────┘
         │
         ▼
┌─────────────────┐      ┌──────────────┐
│  Templates      │◄────►│ Marketplace  │
│  (.tmpl files)  │      │ (gpacks)     │
└─────────────────┘      └──────────────┘
         │
         ▼
   Generated Code
```

---

## Transport Options Deep Dive

### Stdio Transport

**Use Case:** Local development, IDE plugins, desktop apps

**Lifecycle:**
1. MCP client spawns `ggen mcp start` as child process
2. Communication via stdin/stdout
3. Process auto-terminates when client disconnects

**Configuration Example (Claude Desktop):**
```json
{
  "mcpServers": {
    "ggen": {
      "command": "ggen",
      "args": ["mcp", "start"],
      "type": "stdio",
      "env": {
        "GGEN_HOME": "${HOME}/.ggen/templates",
        "RUST_LOG": "ggen=info"
      }
    }
  }
}
```

**Advantages:**
- ✅ Zero configuration networking
- ✅ Process isolation per client
- ✅ Automatic cleanup
- ✅ Works offline

**Limitations:**
- ❌ One client per server instance
- ❌ Cannot share state across clients

### SSE Transport

**Use Case:** Web apps, remote teams, multi-user environments

**Architecture:**
```
┌──────────┐     HTTP GET /sse      ┌─────────────────┐
│ Client 1 │◄────────────────────────┤                 │
└──────────┘                         │  ggen MCP       │
                                     │  SSE Server     │
┌──────────┐     HTTP GET /sse      │  (port 3000)    │
│ Client 2 │◄────────────────────────┤                 │
└──────────┘                         └─────────────────┘
```

**Setup:**
```bash
# Start SSE server
ggen mcp start \
  --transport sse \
  --port 3000 \
  --host 0.0.0.0 \
  --max-clients 10

# Client configuration
{
  "url": "http://localhost:3000/sse",
  "type": "sse",
  "reconnect": true,
  "reconnect_delay": 5000
}
```

**Event Stream Example:**
```
GET http://localhost:3000/sse

event: message
data: {"id":"1","method":"ggen_template_list","result":[...]}

event: message
data: {"id":"2","method":"ggen_gen","result":{"file":"src/main.rs"}}

event: heartbeat
data: {"timestamp":1699999999}
```

**Advantages:**
- ✅ Multiple concurrent clients
- ✅ Real-time updates
- ✅ Auto-reconnection
- ✅ Browser-compatible

**Limitations:**
- ❌ Requires server deployment
- ❌ Firewall considerations
- ❌ Potential network latency

### HTTP Transport

**Use Case:** API integrations, serverless, stateless operations

**REST-like Endpoints:**
```
POST /tools/call           - Call a tool
GET  /tools/list           - List available tools
POST /tools/batch          - Batch tool calls
GET  /health               - Health check
GET  /metrics              - Prometheus metrics
```

**Authentication:**
```bash
# Bearer token
ggen mcp start \
  --transport http \
  --port 8080 \
  --auth-token "secret-token-here"

# API key header
ggen mcp start \
  --transport http \
  --port 8080 \
  --auth-header "X-API-Key"
```

**Example Request:**
```bash
curl -X POST http://localhost:8080/tools/call \
  -H "Authorization: Bearer secret-token-here" \
  -H "Content-Type: application/json" \
  -d '{
    "name": "ggen_gen_with_vars",
    "arguments": {
      "template": "templates/rust-cli.tmpl",
      "vars": {
        "command": "my_command"
      }
    }
  }'
```

**Batch Operations:**
```bash
curl -X POST http://localhost:8080/tools/batch \
  -H "Content-Type: application/json" \
  -d '{
    "calls": [
      {
        "name": "ggen_graph_load",
        "arguments": {"file": "data.ttl"}
      },
      {
        "name": "ggen_graph_query",
        "arguments": {"query": "SELECT * WHERE { ?s ?p ?o } LIMIT 10"}
      },
      {
        "name": "ggen_gen",
        "arguments": {"template": "output.tmpl"}
      }
    ]
  }'
```

**Advantages:**
- ✅ Stateless (horizontally scalable)
- ✅ Standard HTTP tooling
- ✅ Easy load balancing
- ✅ Cache-friendly

**Limitations:**
- ❌ Higher latency per request
- ❌ No streaming support
- ❌ State management complexity

---

## Tool Categories and Workflows

### Workflow 1: Template Discovery → Generation

**Goal:** Find and use a template from the marketplace

```javascript
// Step 1: Browse categories
ggen_market_categories({})
// Returns: ["rust", "python", "web", "cli", ...]

// Step 2: Search for templates
ggen_market_search({
  query: "rust cli",
  category: "rust"
})
// Returns: [
//   {
//     id: "io.ggen.rust.cli-subcommand",
//     description: "Generate Clap subcommands",
//     downloads: 1523,
//     rating: 4.8
//   }
// ]

// Step 3: Get template info
ggen_market_info({
  gpack: "io.ggen.rust.cli-subcommand"
})
// Returns: {
//   templates: ["subcommand.tmpl", "integration-test.tmpl"],
//   vars: ["command_name", "description"],
//   examples: [...]
// }

// Step 4: Install template
ggen_market_add({
  gpack: "io.ggen.rust.cli-subcommand"
})
// Returns: {
//   installed: true,
//   location: "~/.ggen/gpacks/io.ggen.rust.cli-subcommand"
// }

// Step 5: Generate code
ggen_gen_with_vars({
  template: "~/.ggen/gpacks/io.ggen.rust.cli-subcommand/subcommand.tmpl",
  vars: {
    command_name: "deploy",
    description: "Deploy the application"
  }
})
// Returns: {
//   files: ["src/cmds/deploy.rs", "tests/deploy_test.rs"],
//   lines_generated: 127
// }
```

### Workflow 2: RDF-Driven Multi-File Generation

**Goal:** Generate multiple related files from an RDF ontology

```javascript
// Step 1: Load domain model
ggen_graph_load({
  file: "models/ecommerce.ttl",
  format: "turtle"
})
// RDF content:
// @prefix shop: <http://shop.example.org/> .
// shop:Product a owl:Class ;
//   rdfs:label "Product" ;
//   shop:hasField shop:id, shop:name, shop:price .
// shop:Order a owl:Class ;
//   rdfs:label "Order" ;
//   shop:hasField shop:orderId, shop:products .

// Step 2: Query all entities
ggen_graph_query({
  query: `
    PREFIX shop: <http://shop.example.org/>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    SELECT ?entity ?label ?field
    WHERE {
      ?entity a owl:Class ;
              rdfs:label ?label .
      OPTIONAL { ?entity shop:hasField ?field }
    }
  `
})
// Returns: [
//   { entity: "shop:Product", label: "Product", field: "shop:id" },
//   { entity: "shop:Product", label: "Product", field: "shop:name" },
//   { entity: "shop:Product", label: "Product", field: "shop:price" },
//   { entity: "shop:Order", label: "Order", field: "shop:orderId" },
//   ...
// ]

// Step 3: Generate database schema
ggen_gen({
  template: "templates/postgres-schema.tmpl",
  output: "migrations/001_create_tables.sql"
})
// Uses SPARQL results from graph

// Step 4: Generate Rust structs
ggen_gen({
  template: "templates/rust-struct.tmpl",
  output: "src/models.rs"
})

// Step 5: Generate API endpoints
ggen_gen({
  template: "templates/rest-api.tmpl",
  output: "src/api.rs"
})

// Step 6: Generate tests
ggen_gen({
  template: "templates/integration-test.tmpl",
  output: "tests/api_test.rs"
})
```

### Workflow 3: Idempotent File Injection

**Goal:** Add new code to existing files without duplicates

```javascript
// Scenario: Add a new route to existing router file

// Step 1: Preview injection
ggen_gen_dry_run({
  template: "templates/route-injection.tmpl",
  output: "src/router.rs"
})
// Template content:
// ---
// inject:
//   mode: "after"
//   pattern: "// Routes start"
//   skip_if: "{{route_path}}"
// ---
// router.route("{{route_path}}", {{handler}});

// Returns: {
//   would_inject: true,
//   position: 42,
//   preview: "router.route(\"/api/users\", get_users);\n"
// }

// Step 2: Inject for real
ggen_inject_idempotent({
  template: "templates/route-injection.tmpl",
  target: "src/router.rs",
  vars: {
    route_path: "/api/users",
    handler: "get_users"
  }
})
// Returns: {
//   injected: true,
//   line: 43,
//   skipped: false
// }

// Step 3: Try duplicate injection (should skip)
ggen_inject_idempotent({
  template: "templates/route-injection.tmpl",
  target: "src/router.rs",
  vars: {
    route_path: "/api/users",  // Same route
    handler: "get_users"
  }
})
// Returns: {
//   injected: false,
//   skipped: true,
//   reason: "skip_if pattern matched"
// }
```

### Workflow 4: Deterministic Multi-Language Generation

**Goal:** Generate identical code in multiple languages from one ontology

```javascript
// Step 1: Define domain model
ggen_graph_add_triple({
  subject: "http://api.example.org/User",
  predicate: "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
  object: "http://www.w3.org/2002/07/owl#Class"
})

ggen_graph_add_triple({
  subject: "http://api.example.org/User",
  predicate: "http://api.example.org/hasProperty",
  object: "http://api.example.org/email"
})

// Step 2: Generate TypeScript interface
ggen_gen_with_vars({
  template: "templates/typescript-interface.tmpl",
  vars: { determinism: 42 },
  output: "src/types.ts"
})
// Result: interface User { email: string; }

// Step 3: Generate Python dataclass
ggen_gen_with_vars({
  template: "templates/python-dataclass.tmpl",
  vars: { determinism: 42 },
  output: "src/models.py"
})
// Result: @dataclass\nclass User:\n    email: str

// Step 4: Generate Rust struct
ggen_gen_with_vars({
  template: "templates/rust-struct.tmpl",
  vars: { determinism: 42 },
  output: "src/models.rs"
})
// Result: pub struct User { pub email: String, }

// Step 5: Verify determinism (same seed = same output)
ggen_gen_with_vars({
  template: "templates/rust-struct.tmpl",
  vars: { determinism: 42 },
  output: "src/models_verify.rs"
})
// Hash matches previous generation
```

---

## Real-World Conversation Examples

### Example 1: Building a REST API from Scratch

**User:**
> "I need to build a REST API for a blog with posts, comments, and users. Use Rust with Axum. Make it production-ready with tests."

**Claude (using ggen MCP):**

```
I'll help you build a production-ready blog API. Let me start by searching for relevant templates.

[Tool: ggen_market_search]
{
  "query": "rust axum rest api",
  "category": "rust"
}

Found: io.ggen.rust.axum-crud-api

[Tool: ggen_market_add]
{
  "gpack": "io.ggen.rust.axum-crud-api"
}

Now I'll create the RDF domain model for your blog:

[Tool: ggen_graph_load]
{
  "content": "@prefix blog: <http://blog.example.org/> .\nblog:Post a owl:Class ;\n  blog:hasField blog:id, blog:title, blog:content, blog:author .\nblog:Comment a owl:Class ;\n  blog:hasField blog:id, blog:postId, blog:author, blog:text .\nblog:User a owl:Class ;\n  blog:hasField blog:id, blog:username, blog:email .",
  "format": "turtle"
}

Generating the project structure:

[Tool: ggen_gen_batch]
{
  "templates": [
    "~/.ggen/gpacks/io.ggen.rust.axum-crud-api/project-structure.tmpl",
    "~/.ggen/gpacks/io.ggen.rust.axum-crud-api/models.tmpl",
    "~/.ggen/gpacks/io.ggen.rust.axum-crud-api/routes.tmpl",
    "~/.ggen/gpacks/io.ggen.rust.axum-crud-api/database.tmpl",
    "~/.ggen/gpacks/io.ggen.rust.axum-crud-api/tests.tmpl"
  ],
  "vars": {
    "project_name": "blog-api",
    "determinism": 42
  }
}

Generated:
- src/main.rs (API server entry point)
- src/models.rs (Post, Comment, User structs)
- src/routes/ (CRUD endpoints for each entity)
- src/db.rs (Database connection pool)
- tests/ (Integration tests with 85% coverage)
- Cargo.toml (Dependencies: axum, tokio, sqlx)

Your blog API is ready! Run `cargo test` to verify everything works.
```

### Example 2: Migrating Legacy Code

**User:**
> "I have a Python Flask app with 20 routes. Help me migrate to FastAPI while keeping the same functionality."

**Claude (using ggen MCP):**

```
I'll help migrate your Flask app to FastAPI. First, let me analyze your existing routes:

[Tool: ggen_graph_load]
{
  "file": "app.py",
  "format": "python"
}

[Tool: ggen_graph_query]
{
  "query": "SELECT ?route ?method ?handler WHERE { ?route a :FlaskRoute ; :method ?method ; :handler ?handler }"
}

Found 20 routes. Now I'll generate FastAPI equivalents:

[Tool: ggen_market_search]
{
  "query": "python fastapi migration"
}

[Tool: ggen_gen_batch]
{
  "templates": [
    "templates/fastapi-route.tmpl"
  ],
  "items": [
    {"route": "/users", "method": "GET", "handler": "get_users"},
    {"route": "/users", "method": "POST", "handler": "create_user"},
    // ... 18 more routes
  ]
}

Generated:
- app/main.py (FastAPI app with all 20 routes)
- app/models.py (Pydantic models)
- app/routers/ (Organized by domain)
- tests/ (Pytest fixtures and tests)

Migration complete! Your Flask routes are now FastAPI-compatible.
```

### Example 3: Generating Documentation

**User:**
> "Generate OpenAPI docs for my API based on the code"

**Claude (using ggen MCP):**

```
I'll analyze your API and generate OpenAPI documentation.

[Tool: ggen_graph_load]
{
  "file": "src/api.rs",
  "format": "rust"
}

[Tool: ggen_graph_query]
{
  "query": "SELECT ?endpoint ?method ?params ?response WHERE { ?endpoint a :APIEndpoint ; :method ?method ; :params ?params ; :response ?response }"
}

[Tool: ggen_gen]
{
  "template": "templates/openapi-spec.tmpl",
  "output": "docs/openapi.yaml"
}

Generated:
- docs/openapi.yaml (Complete OpenAPI 3.0 spec)
- docs/README.md (Human-readable API docs)
- docs/postman-collection.json (Postman import)

Your API is now fully documented! View at http://localhost:8080/docs
```

---

## Error Handling

### Client-Side Error Handling

#### Rust Client
```rust
use rmcp::{Client, Error};

async fn generate_code(client: &mut Client) -> Result<(), Error> {
    match client.call_tool(
        "ggen_gen",
        json!({"template": "missing.tmpl"})
    ).await {
        Ok(response) => {
            println!("Success: {:?}", response);
        }
        Err(Error::ToolNotFound(name)) => {
            eprintln!("Tool '{}' does not exist", name);
        }
        Err(Error::InvalidArguments(msg)) => {
            eprintln!("Invalid arguments: {}", msg);
        }
        Err(Error::ToolExecutionFailed { tool, message }) => {
            eprintln!("Tool '{}' failed: {}", tool, message);
            // Retry with fallback template
            client.call_tool(
                "ggen_gen",
                json!({"template": "default.tmpl"})
            ).await?;
        }
        Err(e) => {
            eprintln!("Unexpected error: {:?}", e);
        }
    }

    Ok(())
}
```

#### Python Client
```python
from mcp.client import ClientSession
from mcp.types import McpError

async def generate_with_retry(session: ClientSession, template: str, max_retries=3):
    for attempt in range(max_retries):
        try:
            result = await session.call_tool(
                "ggen_gen",
                {"template": template}
            )
            return result
        except McpError as e:
            if e.code == "TOOL_NOT_FOUND":
                # Try alternative template
                template = f"templates/fallback-{template}"
            elif e.code == "INVALID_ARGUMENTS":
                print(f"Fix arguments: {e.message}")
                raise
            elif e.code == "TIMEOUT":
                print(f"Retry {attempt + 1}/{max_retries}")
                await asyncio.sleep(2 ** attempt)  # Exponential backoff
            else:
                raise

    raise Exception("Max retries exceeded")
```

### Server-Side Error Handling

The ggen MCP server returns structured errors:

```json
{
  "jsonrpc": "2.0",
  "id": "123",
  "error": {
    "code": "TOOL_EXECUTION_FAILED",
    "message": "Template rendering failed",
    "data": {
      "tool": "ggen_gen",
      "template": "templates/invalid.tmpl",
      "error_type": "SyntaxError",
      "line": 42,
      "details": "Unclosed template tag at line 42"
    }
  }
}
```

**Common Error Codes:**

| Code | Meaning | Recovery |
|------|---------|----------|
| `TOOL_NOT_FOUND` | Tool doesn't exist | Check `ggen_tools_list` |
| `INVALID_ARGUMENTS` | Missing/wrong params | Validate with schema |
| `TEMPLATE_NOT_FOUND` | Template file missing | Use `ggen_template_list` |
| `SPARQL_SYNTAX_ERROR` | Invalid SPARQL query | Validate query syntax |
| `GRAPH_LOAD_FAILED` | RDF parsing error | Check RDF file format |
| `TIMEOUT` | Operation exceeded limit | Increase timeout or simplify |
| `RESOURCE_EXHAUSTED` | Out of memory/disk | Reduce batch size |

---

## Performance Tuning

### Benchmarking

```bash
# Run performance benchmarks
ggen mcp benchmark \
  --templates 1000 \
  --graph-triples 100000 \
  --duration 60s \
  --output benchmark.json

# Results:
# {
#   "avg_generation_time_ms": 12.4,
#   "p50_ms": 8.2,
#   "p95_ms": 24.6,
#   "p99_ms": 45.1,
#   "throughput_per_sec": 80.6,
#   "memory_mb": 87.3
# }
```

### Optimization Strategies

#### 1. Template Compilation

Pre-compile templates for faster rendering:

```bash
ggen template compile \
  --input templates/ \
  --output ~/.ggen/compiled/ \
  --format bincode

ggen mcp start --compiled-templates ~/.ggen/compiled/
```

**Performance Gain:** 3-5x faster rendering

#### 2. SPARQL Query Caching

Enable query result caching:

```bash
ggen mcp start \
  --sparql-cache-enabled \
  --sparql-cache-size 500mb \
  --sparql-cache-ttl 3600
```

**Performance Gain:** 10-100x for repeated queries

#### 3. Parallel Batch Generation

Process templates in parallel:

```javascript
ggen_gen_batch({
  templates: [...100 templates...],
  parallel: true,
  max_workers: 8  // Match CPU cores
})
```

**Performance Gain:** Near-linear scaling up to core count

#### 4. Graph Indexing

Build SPARQL indexes for frequent queries:

```bash
ggen graph index \
  --file ontology.ttl \
  --patterns "?s rdf:type ?type" "?s rdfs:label ?label" \
  --output ontology.idx

ggen mcp start --graph-index ontology.idx
```

**Performance Gain:** 5-20x for indexed predicates

#### 5. Streaming Large Outputs

Stream file generation to avoid memory spikes:

```javascript
ggen_gen({
  template: "large-template.tmpl",
  output: "large-file.rs",
  stream: true,
  buffer_size: 8192
})
```

**Memory Savings:** 90%+ for files >10MB

---

## Advanced Usage Patterns

### Pattern 1: Multi-Stage Generation Pipeline

```javascript
// Stage 1: Load multiple ontologies
await ggen_graph_load({ file: "domain.ttl" });
await ggen_graph_load({ file: "ui.ttl" });
await ggen_graph_load({ file: "api.ttl" });

// Stage 2: Query and validate
const entities = await ggen_graph_query({
  query: "SELECT ?entity WHERE { ?entity a owl:Class }"
});

// Stage 3: Generate backend
await ggen_gen_batch({
  templates: ["models.tmpl", "database.tmpl", "api.tmpl"],
  vars: { entities }
});

// Stage 4: Generate frontend
await ggen_gen_batch({
  templates: ["components.tmpl", "services.tmpl"],
  vars: { entities }
});

// Stage 5: Generate tests
await ggen_gen_batch({
  templates: ["unit-tests.tmpl", "integration-tests.tmpl"],
  vars: { entities }
});
```

### Pattern 2: Conditional Template Selection

```javascript
// Choose template based on context
const template = await (async () => {
  const graphStats = await ggen_graph_stats({});

  if (graphStats.triple_count > 10000) {
    return "templates/optimized-large.tmpl";
  } else if (graphStats.has_inference) {
    return "templates/reasoning-enabled.tmpl";
  } else {
    return "templates/standard.tmpl";
  }
})();

await ggen_gen({ template });
```

### Pattern 3: Incremental Graph Building

```javascript
// Start with base ontology
await ggen_graph_load({ file: "base.ttl" });

// Add domain-specific triples
for (const entity of userEntities) {
  await ggen_graph_add_triple({
    subject: entity.uri,
    predicate: "rdf:type",
    object: entity.type
  });
}

// Infer additional triples
await ggen_graph_infer({ rules: "rdfs" });

// Export combined graph
await ggen_graph_export({
  file: "complete.ttl",
  format: "turtle"
});
```

### Pattern 4: Template Hot-Reloading

```bash
# Start server in watch mode
ggen mcp start \
  --watch-templates \
  --template-dir ./templates \
  --reload-on-change
```

Now template edits are instantly available without restart.

---

## Security Best Practices

### 1. Input Validation

Always validate user-provided template variables:

```yaml
---
to: "src/{{name | safe | regex_replace('[^a-zA-Z0-9_]', '')}}.rs"
vars:
  name: "user_input"  # Sanitized in template
---
```

### 2. Template Sandboxing

Restrict template capabilities:

```bash
ggen mcp start \
  --sandbox-templates \
  --disallow-shell-exec \
  --disallow-file-include \
  --max-iterations 1000
```

### 3. Graph Access Control

Limit which graphs can be queried:

```bash
ggen mcp start \
  --allowed-graphs "http://safe.example.org/*" \
  --deny-graphs "http://private.example.org/*"
```

### 4. Rate Limiting

Prevent abuse with rate limits:

```bash
ggen mcp start \
  --rate-limit 100/min \
  --rate-limit-burst 10 \
  --rate-limit-key client-ip
```

### 5. Audit Logging

Log all tool calls for security review:

```bash
ggen mcp start \
  --audit-log /var/log/ggen-audit.json \
  --audit-level full \
  --audit-include-vars true
```

**Example Audit Log:**
```json
{
  "timestamp": "2024-01-15T10:30:00Z",
  "client_id": "claude-desktop",
  "tool": "ggen_gen",
  "arguments": {
    "template": "templates/admin-user.tmpl",
    "vars": {"role": "admin"}
  },
  "result": "success",
  "files_written": ["src/admin.rs"],
  "duration_ms": 45
}
```

---

## Troubleshooting Guide

### Issue: "Template rendering is slow"

**Diagnosis:**
```bash
ggen mcp benchmark --template slow-template.tmpl --profile
```

**Solutions:**
1. Enable template compilation
2. Reduce SPARQL query complexity
3. Use `{% cache %}` tags for expensive computations
4. Profile with `--debug-timing`

### Issue: "SPARQL queries return empty results"

**Diagnosis:**
```javascript
// Check graph contents
await ggen_graph_stats({});

// Validate SPARQL syntax
await ggen_graph_validate_query({
  query: "SELECT * WHERE { ?s ?p ?o }"
});

// Test simplified query
await ggen_graph_query({
  query: "SELECT * WHERE { ?s ?p ?o } LIMIT 1"
});
```

**Solutions:**
1. Verify RDF data loaded correctly
2. Check namespace prefixes match
3. Use `OPTIONAL` for missing predicates
4. Enable query logging: `RUST_LOG=ggen::sparql=debug`

### Issue: "Marketplace connection fails"

**Diagnosis:**
```bash
curl -v https://registry.ggen.io/health
ggen market search --debug
```

**Solutions:**
1. Check network connectivity
2. Verify TLS certificates
3. Use mirror: `GGEN_REGISTRY_URL=https://mirror.ggen.io`
4. Enable offline mode: `ggen mcp start --offline`

---

**For more help, see:**
- [MCP Server Documentation](./MCP_SERVER.md)
- [ggen Cookbook](./COOKBOOK-CONVO.md)
- [GitHub Discussions](https://github.com/seanchatmangpt/ggen/discussions)
