<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen MCP Server](#ggen-mcp-server)
  - [📋 Table of Contents](#-table-of-contents)
  - [What is the ggen MCP Server?](#what-is-the-ggen-mcp-server)
    - [Why Use ggen with MCP?](#why-use-ggen-with-mcp)
  - [Quick Start](#quick-start)
    - [Installation](#installation)
  - [Available Tools](#available-tools)
    - [📝 Template Management (8 tools)](#-template-management-8-tools)
    - [🚀 Project Generation (7 tools)](#-project-generation-7-tools)
    - [🌐 RDF Graph Operations (9 tools)](#-rdf-graph-operations-9-tools)
    - [📦 Marketplace (gpacks) (8 tools)](#-marketplace-gpacks-8-tools)
    - [🔧 Project Scaffolding (4 tools)](#-project-scaffolding-4-tools)
    - [🐙 GitHub Integration (3 tools)](#-github-integration-3-tools)
    - [🔍 Utility Tools (3 tools)](#-utility-tools-3-tools)
  - [Configuration](#configuration)
    - [Stdio Transport (Recommended)](#stdio-transport-recommended)
    - [SSE Transport (Server-Sent Events)](#sse-transport-server-sent-events)
    - [HTTP Transport](#http-transport)
    - [Environment Variables](#environment-variables)
  - [Integration Examples](#integration-examples)
    - [Example 1: Claude Desktop - Generate a Rust Module](#example-1-claude-desktop---generate-a-rust-module)
    - [Example 2: Cline VSCode - RDF-Driven API Generation](#example-2-cline-vscode---rdf-driven-api-generation)
    - [Example 3: Custom Rust Client](#example-3-custom-rust-client)
    - [Example 4: Python MCP SDK](#example-4-python-mcp-sdk)
    - [Example 5: HTTP API with curl](#example-5-http-api-with-curl)
  - [Transport Options](#transport-options)
    - [Stdio (Standard I/O)](#stdio-standard-io)
    - [SSE (Server-Sent Events)](#sse-server-sent-events)
    - [HTTP](#http)
  - [Advanced Usage](#advanced-usage)
    - [Custom Graph Context](#custom-graph-context)
    - [Template Auto-Discovery](#template-auto-discovery)
    - [Authentication & Security](#authentication--security)
      - [Token-Based Auth (HTTP/SSE)](#token-based-auth-httpsse)
      - [mTLS (Mutual TLS)](#mtls-mutual-tls)
    - [Resource Limits](#resource-limits)
    - [Logging & Monitoring](#logging--monitoring)
    - [Caching](#caching)
  - [Troubleshooting](#troubleshooting)
    - [Problem: MCP Server Not Starting](#problem-mcp-server-not-starting)
    - [Problem: Templates Not Found](#problem-templates-not-found)
    - [Problem: SPARQL Queries Failing](#problem-sparql-queries-failing)
    - [Problem: Generation Not Deterministic](#problem-generation-not-deterministic)
    - [Problem: High Memory Usage](#problem-high-memory-usage)
    - [Problem: Marketplace Access Errors](#problem-marketplace-access-errors)
  - [Performance Tuning](#performance-tuning)
    - [Benchmarking](#benchmarking)
    - [Optimization Tips](#optimization-tips)
  - [Additional Resources](#additional-resources)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen MCP Server

[![MCP](https://img.shields.io/badge/MCP-Compatible-blue)](https://modelcontextprotocol.io)
[![Rust](https://img.shields.io/badge/rust-1.70%2B-orange.svg)](https://www.rust-lang.org/)

The **ggen MCP Server** exposes ggen's graph-aware code generation capabilities through the Model Context Protocol (MCP), enabling AI assistants like Claude to generate deterministic, RDF-backed code directly within their workflows.

## 📋 Table of Contents

- [What is the ggen MCP Server?](#what-is-the-ggen-mcp-server)
- [Quick Start](#quick-start)
- [Available Tools](#available-tools)
- [Configuration](#configuration)
- [Integration Examples](#integration-examples)
- [Transport Options](#transport-options)
- [Advanced Usage](#advanced-usage)
- [Troubleshooting](#troubleshooting)

---

## What is the ggen MCP Server?

The ggen MCP Server is a **Model Context Protocol** implementation that provides AI assistants with direct access to ggen's powerful code generation capabilities:

- 🎯 **40+ MCP Tools** for template generation, RDF operations, and marketplace access
- 🌐 **Language-Agnostic** code generation from RDF knowledge graphs
- 🔗 **SPARQL Integration** for semantic queries
- 📦 **Marketplace Access** to search and install template packages (gpacks)
- 🔄 **Injection Support** for modifying existing files
- 🎲 **Deterministic Output** with reproducible generation

### Why Use ggen with MCP?

**Traditional Workflow:**
```
User → AI Assistant → Code Suggestions → User copies/pastes → Manual edits
```

**MCP-Enhanced Workflow:**
```
User → AI Assistant → ggen MCP Tools → Deterministic Code Generation → RDF-Backed Artifacts
```

---

## Quick Start

### Installation

**Step 1: Install ggen**
```bash
# Via Homebrew (macOS/Linux)
brew tap seanchatmangpt/tap
brew install ggen

# Or from source
git clone https://github.com/seanchatmangpt/ggen
cd ggen
cargo make build-release
```

**Step 2: Configure MCP Server**

Add to your MCP client configuration (e.g., Claude Desktop):

```json
{
  "mcpServers": {
    "ggen": {
      "command": "ggen",
      "args": ["mcp", "start"],
      "type": "stdio"
    }
  }
}
```

**Step 3: Verify Installation**

In Claude Desktop or your MCP client:
```
"Can you list the available ggen templates?"
```

The assistant will use the `ggen_template_list` tool to show available templates.

---

## Available Tools

The ggen MCP Server provides **40+ tools** organized into 7 categories:

### 📝 Template Management (8 tools)

| Tool | Description |
|------|-------------|
| `ggen_template_list` | List all available templates |
| `ggen_template_show` | Show template metadata and frontmatter |
| `ggen_template_lint` | Validate template syntax and schema |
| `ggen_template_create` | Create a new template file |
| `ggen_template_validate` | Validate template against JSON schema |
| `ggen_template_render_preview` | Preview template output without saving |
| `ggen_template_edit` | Edit template frontmatter or body |
| `ggen_template_delete` | Remove a template file |

### 🚀 Project Generation (7 tools)

| Tool | Description |
|------|-------------|
| `ggen_gen` | Generate code from a template |
| `ggen_gen_with_vars` | Generate with custom variables |
| `ggen_gen_multi` | Generate multiple files from one template |
| `ggen_gen_dry_run` | Preview generation without writing files |
| `ggen_inject` | Inject content into existing files |
| `ggen_inject_idempotent` | Inject with skip-if guards |
| `ggen_gen_batch` | Generate from multiple templates at once |

### 🌐 RDF Graph Operations (9 tools)

| Tool | Description |
|------|-------------|
| `ggen_graph_query` | Execute SPARQL query on RDF graph |
| `ggen_graph_load` | Load RDF data from Turtle/N-Triples/RDF/XML |
| `ggen_graph_add_triple` | Add a single RDF triple |
| `ggen_graph_export` | Export graph to file |
| `ggen_graph_clear` | Clear all triples |
| `ggen_graph_validate` | Validate RDF syntax |
| `ggen_graph_stats` | Get graph statistics (triple count, prefixes) |
| `ggen_graph_describe` | Describe a resource (all related triples) |
| `ggen_graph_construct` | Execute SPARQL CONSTRUCT query |

### 📦 Marketplace (gpacks) (8 tools)

| Tool | Description |
|------|-------------|
| `ggen_market_search` | Search for template packages |
| `ggen_market_add` | Install a gpack |
| `ggen_market_remove` | Uninstall a gpack |
| `ggen_market_list` | List installed gpacks |
| `ggen_market_update` | Update gpacks to latest versions |
| `ggen_market_categories` | Browse categories and keywords |
| `ggen_market_info` | Get detailed gpack information |
| `ggen_market_verify` | Verify gpack integrity (post-quantum signatures) |

### 🔧 Project Scaffolding (4 tools)

| Tool | Description |
|------|-------------|
| `ggen_project_init` | Initialize a new ggen project |
| `ggen_project_scaffold` | Create project structure from template |
| `ggen_project_config` | View/edit ggen.toml configuration |
| `ggen_project_status` | Show project generation status |

### 🐙 GitHub Integration (3 tools)

| Tool | Description |
|------|-------------|
| `ggen_github_pages_status` | Check GitHub Pages deployment status |
| `ggen_github_workflow_status` | View GitHub Actions workflow runs |
| `ggen_github_trigger_workflow` | Trigger a workflow dispatch event |

### 🔍 Utility Tools (3 tools)

| Tool | Description |
|------|-------------|
| `ggen_validate_config` | Validate ggen.toml configuration |
| `ggen_completion_generate` | Generate shell completion scripts |
| `ggen_hazard_report` | Generate hazard analysis report |

---

## Configuration

### Stdio Transport (Recommended)

**Claude Desktop** (`~/Library/Application Support/Claude/claude_desktop_config.json`):
```json
{
  "mcpServers": {
    "ggen": {
      "command": "ggen",
      "args": ["mcp", "start"],
      "type": "stdio",
      "env": {
        "GGEN_HOME": "/path/to/your/templates",
        "GGEN_REGISTRY_URL": "https://registry.ggen.io",
        "RUST_LOG": "info"
      }
    }
  }
}
```

**Cline (VSCode)** (`.vscode/settings.json`):
```json
{
  "mcp.servers": {
    "ggen": {
      "command": "ggen",
      "args": ["mcp", "start"],
      "type": "stdio"
    }
  }
}
```

### SSE Transport (Server-Sent Events)

For remote deployments:

```bash
ggen mcp start --transport sse --port 3000
```

Client configuration:
```json
{
  "mcpServers": {
    "ggen": {
      "url": "http://localhost:3000/sse",
      "type": "sse"
    }
  }
}
```

### HTTP Transport

For stateless request/response:

```bash
ggen mcp start --transport http --port 8080
```

Client configuration:
```json
{
  "mcpServers": {
    "ggen": {
      "url": "http://localhost:8080",
      "type": "http",
      "headers": {
        "Authorization": "Bearer YOUR_API_KEY"
      }
    }
  }
}
```

### Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `GGEN_HOME` | Template directory | `~/.ggen/templates` |
| `GGEN_REGISTRY_URL` | Marketplace registry URL | `https://registry.ggen.io` |
| `GGEN_CACHE_DIR` | Template cache directory | `~/.ggen/cache` |
| `RUST_LOG` | Logging level | `warn` |
| `GGEN_MAX_GRAPH_SIZE` | Max RDF graph size (MB) | `100` |
| `GGEN_SPARQL_TIMEOUT` | SPARQL query timeout (s) | `30` |

---

## Integration Examples

### Example 1: Claude Desktop - Generate a Rust Module

**User Prompt:**
```
"Create a Rust module called 'auth' with login and logout functions"
```

**Claude's Tool Usage:**
```javascript
// Step 1: Search for Rust templates
ggen_market_search({ query: "rust module" })

// Step 2: Generate from template
ggen_gen_with_vars({
  template: "templates/rust-module.tmpl",
  vars: {
    name: "auth",
    functions: ["login", "logout"]
  }
})
```

**Result:**
```rust
// src/auth.rs (generated)
pub struct Auth;

impl Auth {
    pub fn login(&self) -> Result<(), AuthError> {
        // Implementation
    }

    pub fn logout(&self) -> Result<(), AuthError> {
        // Implementation
    }
}
```

### Example 2: Cline VSCode - RDF-Driven API Generation

**User Prompt:**
```
"Generate REST API endpoints from my OpenAPI spec using RDF"
```

**Claude's Tool Usage:**
```javascript
// Step 1: Load OpenAPI as RDF
ggen_graph_load({
  file: "openapi.yaml",
  format: "yaml"
})

// Step 2: Query endpoints
ggen_graph_query({
  query: `
    PREFIX api: <http://api.example.org/>
    SELECT ?path ?method ?summary
    WHERE {
      ?endpoint api:path ?path ;
                api:method ?method ;
                api:summary ?summary .
    }
  `
})

// Step 3: Generate for each endpoint
ggen_gen_batch({
  template: "templates/rest-endpoint.tmpl",
  items: [
    { path: "/users", method: "GET" },
    { path: "/users", method: "POST" }
  ]
})
```

### Example 3: Custom Rust Client

```rust
use rmcp::{Client, StdioTransport};
use serde_json::json;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Connect to ggen MCP server
    let transport = StdioTransport::new("ggen", vec!["mcp", "start"]);
    let mut client = Client::new(transport).await?;

    // List available templates
    let response = client.call_tool(
        "ggen_template_list",
        json!({})
    ).await?;

    println!("Available templates: {:?}", response);

    // Generate code
    let gen_response = client.call_tool(
        "ggen_gen_with_vars",
        json!({
            "template": "templates/rust-cli.tmpl",
            "vars": {
                "command_name": "my_command",
                "description": "My CLI command"
            }
        })
    ).await?;

    println!("Generated: {:?}", gen_response);

    Ok(())
}
```

### Example 4: Python MCP SDK

```python
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client
import asyncio

async def main():
    # Connect to ggen MCP server
    server_params = StdioServerParameters(
        command="ggen",
        args=["mcp", "start"]
    )

    async with stdio_client(server_params) as (read, write):
        async with ClientSession(read, write) as session:
            # Initialize the session
            await session.initialize()

            # Search marketplace
            result = await session.call_tool(
                "ggen_market_search",
                {"query": "python flask"}
            )
            print(f"Search results: {result}")

            # Generate from template
            gen_result = await session.call_tool(
                "ggen_gen",
                {
                    "template": "templates/python-flask-route.tmpl",
                    "output": "app/routes.py"
                }
            )
            print(f"Generated: {gen_result}")

asyncio.run(main())
```

### Example 5: HTTP API with curl

```bash
# Call ggen MCP server via HTTP transport

# List templates
curl -X POST http://localhost:8080/tools/call \
  -H "Content-Type: application/json" \
  -d '{
    "name": "ggen_template_list",
    "arguments": {}
  }'

# Generate code
curl -X POST http://localhost:8080/tools/call \
  -H "Content-Type: application/json" \
  -d '{
    "name": "ggen_gen_with_vars",
    "arguments": {
      "template": "templates/typescript-interface.tmpl",
      "vars": {
        "name": "User",
        "fields": ["id", "email", "name"]
      }
    }
  }'

# SPARQL query
curl -X POST http://localhost:8080/tools/call \
  -H "Content-Type: application/json" \
  -d '{
    "name": "ggen_graph_query",
    "arguments": {
      "query": "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10"
    }
  }'
```

---

## Transport Options

### Stdio (Standard I/O)

**Best for:** Local development, IDE integrations, desktop applications

**Pros:**
- ✅ Simple configuration
- ✅ Low latency
- ✅ No network setup required
- ✅ Automatic process management

**Cons:**
- ❌ Single client per server instance
- ❌ Cannot share across multiple applications

**Example:**
```json
{
  "command": "ggen",
  "args": ["mcp", "start"],
  "type": "stdio"
}
```

### SSE (Server-Sent Events)

**Best for:** Web applications, remote access, multiple clients

**Pros:**
- ✅ Real-time updates
- ✅ Multiple concurrent clients
- ✅ Long-running connections
- ✅ Browser-compatible

**Cons:**
- ❌ Requires server deployment
- ❌ More complex setup
- ❌ Network latency

**Example:**
```bash
# Start SSE server
ggen mcp start --transport sse --port 3000 --host 0.0.0.0

# Configure client
{
  "url": "http://localhost:3000/sse",
  "type": "sse"
}
```

### HTTP

**Best for:** Stateless operations, API integrations, serverless

**Pros:**
- ✅ Stateless (easy to scale)
- ✅ Standard HTTP tools (curl, Postman)
- ✅ Load balancing friendly
- ✅ Firewall-friendly

**Cons:**
- ❌ Higher latency per request
- ❌ No streaming
- ❌ Requires authentication setup

**Example:**
```bash
# Start HTTP server
ggen mcp start --transport http --port 8080 \
  --auth-token "your-secret-token"

# Configure client
{
  "url": "http://localhost:8080",
  "type": "http",
  "headers": {
    "Authorization": "Bearer your-secret-token"
  }
}
```

---

## Advanced Usage

### Custom Graph Context

Pre-load RDF graphs for context:

```bash
ggen mcp start --graph-context ./ontology.ttl \
  --prefixes "foaf=http://xmlns.com/foaf/0.1/"
```

### Template Auto-Discovery

Auto-discover templates in directory:

```bash
ggen mcp start --template-dir ./my-templates \
  --watch
```

### Authentication & Security

#### Token-Based Auth (HTTP/SSE)

```bash
# Generate API token
ggen mcp generate-token --expires 30d > token.txt

# Start with auth
ggen mcp start --transport http \
  --auth-token $(cat token.txt)
```

#### mTLS (Mutual TLS)

```bash
ggen mcp start --transport https \
  --tls-cert server.crt \
  --tls-key server.key \
  --client-ca ca.crt
```

### Resource Limits

Prevent resource exhaustion:

```bash
ggen mcp start \
  --max-graph-size 500mb \
  --max-template-size 10mb \
  --max-concurrent-requests 10 \
  --request-timeout 60s
```

### Logging & Monitoring

```bash
# Enable structured JSON logging
RUST_LOG=ggen=debug ggen mcp start \
  --log-format json \
  --log-file /var/log/ggen-mcp.log

# Send metrics to StatsD
ggen mcp start \
  --metrics-endpoint statsd://localhost:8125 \
  --metrics-prefix ggen.mcp
```

### Caching

Enable template and SPARQL result caching:

```bash
ggen mcp start \
  --cache-enabled \
  --cache-dir /tmp/ggen-cache \
  --cache-ttl 3600
```

---

## Troubleshooting

### Problem: MCP Server Not Starting

**Symptoms:**
- Client shows "Connection failed"
- No response from ggen

**Solutions:**
1. **Verify installation:**
   ```bash
   ggen --version
   ggen mcp --help
   ```

2. **Check logs:**
   ```bash
   RUST_LOG=debug ggen mcp start 2>&1 | tee ggen-mcp.log
   ```

3. **Test manually:**
   ```bash
   echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}' | ggen mcp start
   ```

### Problem: Templates Not Found

**Symptoms:**
- `ggen_template_list` returns empty
- "Template not found" errors

**Solutions:**
1. **Set GGEN_HOME:**
   ```bash
   export GGEN_HOME=/path/to/templates
   ggen mcp start
   ```

2. **Verify template directory:**
   ```bash
   ls -la ~/.ggen/templates/
   ```

3. **Use explicit path:**
   ```json
   {
     "args": ["mcp", "start", "--template-dir", "/custom/path"]
   }
   ```

### Problem: SPARQL Queries Failing

**Symptoms:**
- Empty results from valid queries
- Timeout errors

**Solutions:**
1. **Check graph load:**
   ```javascript
   ggen_graph_stats({})
   ```

2. **Increase timeout:**
   ```bash
   GGEN_SPARQL_TIMEOUT=60 ggen mcp start
   ```

3. **Validate RDF syntax:**
   ```javascript
   ggen_graph_validate({ file: "data.ttl" })
   ```

### Problem: Generation Not Deterministic

**Symptoms:**
- Different output each run
- Hash mismatches

**Solutions:**
1. **Set determinism seed:**
   ```yaml
   ---
   determinism: 42  # Fixed seed
   ---
   ```

2. **Disable parallelism:**
   ```bash
   ggen mcp start --single-threaded
   ```

3. **Check for time-based variables:**
   Avoid `now()`, `random()` in templates

### Problem: High Memory Usage

**Symptoms:**
- OOM errors
- Slow performance

**Solutions:**
1. **Limit graph size:**
   ```bash
   GGEN_MAX_GRAPH_SIZE=50 ggen mcp start
   ```

2. **Clear cache:**
   ```bash
   rm -rf ~/.ggen/cache/*
   ```

3. **Stream large files:**
   Use `--stream` flag for large template batches

### Problem: Marketplace Access Errors

**Symptoms:**
- "Registry unreachable"
- Certificate errors

**Solutions:**
1. **Check network:**
   ```bash
   curl -v https://registry.ggen.io/health
   ```

2. **Use mirror:**
   ```bash
   GGEN_REGISTRY_URL=https://mirror.ggen.io ggen mcp start
   ```

3. **Disable TLS verification (dev only):**
   ```bash
   GGEN_INSECURE_REGISTRY=1 ggen mcp start
   ```

---

## Performance Tuning

### Benchmarking

```bash
# Run built-in benchmarks
ggen mcp benchmark \
  --templates 100 \
  --graph-size 10000 \
  --duration 60s
```

### Optimization Tips

1. **Enable SPARQL caching:**
   ```bash
   ggen mcp start --sparql-cache-enabled
   ```

2. **Use compiled templates:**
   ```bash
   ggen template compile ./templates/**/*.tmpl
   ```

3. **Parallel generation:**
   ```javascript
   ggen_gen_batch({
     templates: [...],
     parallel: true,
     max_workers: 4
   })
   ```

4. **Profile memory:**
   ```bash
   heaptrack ggen mcp start
   ```

---

## Additional Resources

- 📚 **[Full MCP Usage Guide](./MCP_USAGE_GUIDE.md)** - Detailed workflows and examples
- 🚀 **[MCP Examples](../examples/mcp/)** - Working code samples
- 🌐 **[Model Context Protocol Spec](https://modelcontextprotocol.io)** - MCP standard
- 🦀 **[rmcp Rust Library](https://github.com/modelcontextprotocol/rmcp)** - Rust MCP SDK
- 🐍 **[MCP Python SDK](https://github.com/modelcontextprotocol/python-sdk)** - Python MCP SDK
- 💬 **[ggen Discussions](https://github.com/seanchatmangpt/ggen/discussions)** - Community support

---

**Built with ❤️ using Rust, RDF, SPARQL, and the Model Context Protocol**
