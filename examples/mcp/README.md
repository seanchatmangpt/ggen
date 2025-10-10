# ggen MCP Examples

Practical, working examples of integrating ggen with various MCP clients.

## 📁 Directory Structure

```
examples/mcp/
├── README.md                          # This file
├── claude-desktop/                    # Claude Desktop integration
│   ├── config.json                   # MCP configuration
│   ├── example-conversation.md       # Sample conversations
│   └── troubleshooting.md            # Common issues
├── cline-vscode/                      # Cline VSCode extension
│   ├── settings.json                 # VSCode settings
│   ├── example-workflow.md           # Development workflows
│   └── keybindings.json              # Keyboard shortcuts
├── rust-client/                       # Rust rmcp client
│   ├── Cargo.toml
│   ├── src/
│   │   ├── main.rs                   # Basic usage
│   │   ├── batch.rs                  # Batch operations
│   │   └── streaming.rs              # Streaming responses
│   └── README.md
├── python-client/                     # Python MCP SDK
│   ├── requirements.txt
│   ├── basic_usage.py                # Simple examples
│   ├── advanced_workflows.py         # Complex patterns
│   └── README.md
├── http-api/                          # HTTP transport examples
│   ├── curl/                         # curl commands
│   │   ├── basic.sh
│   │   ├── batch.sh
│   │   └── authenticated.sh
│   ├── postman/                      # Postman collection
│   │   └── ggen-mcp.postman_collection.json
│   └── README.md
└── sse-client/                        # SSE transport examples
    ├── javascript/
    │   └── event-stream.js
    ├── python/
    │   └── sse_client.py
    └── README.md
```

---

## Quick Start Examples

### 1. Claude Desktop Integration

**File: `claude-desktop/config.json`**

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

**Location:**
- **macOS**: `~/Library/Application Support/Claude/claude_desktop_config.json`
- **Windows**: `%APPDATA%\Claude\claude_desktop_config.json`
- **Linux**: `~/.config/Claude/claude_desktop_config.json`

**Example Conversation:**

```
User: "Generate a Rust CLI command called 'deploy' with arg parsing"

Claude: I'll help you generate that CLI command using ggen.

[Uses: ggen_market_search { query: "rust cli" }]
Found template: io.ggen.rust.cli-subcommand

[Uses: ggen_gen_with_vars {
  template: "~/.ggen/gpacks/io.ggen.rust.cli-subcommand/subcommand.tmpl",
  vars: {
    command_name: "deploy",
    description: "Deploy the application",
    args: ["environment", "version"]
  }
}]

Generated:
- src/cmds/deploy.rs (CLI subcommand implementation)
- tests/deploy_test.rs (Unit tests)

Your 'deploy' command is ready with argument parsing for environment and version!
```

---

### 2. Cline (VSCode) Integration

**File: `cline-vscode/settings.json`**

```json
{
  "mcp.servers": {
    "ggen": {
      "command": "ggen",
      "args": ["mcp", "start"],
      "type": "stdio",
      "autoStart": true,
      "logFile": "${workspaceFolder}/.vscode/ggen-mcp.log"
    }
  },
  "cline.tools.ggen.enabled": true,
  "cline.tools.ggen.templateDir": "${workspaceFolder}/templates"
}
```

**Keyboard Shortcut:**

```json
{
  "key": "ctrl+shift+g",
  "command": "cline.tools.ggen.generate",
  "when": "editorTextFocus"
}
```

**Workflow Example:**

1. Open Cline sidebar
2. Type: "Generate REST endpoint for User CRUD"
3. Cline uses `ggen_market_search` → finds template
4. Cline uses `ggen_gen` → creates `routes/users.rs`
5. Code appears in editor, ready to use

---

### 3. Rust Client (rmcp)

**File: `rust-client/src/main.rs`**

```rust
use rmcp::{Client, StdioTransport};
use serde_json::json;
use anyhow::Result;

#[tokio::main]
async fn main() -> Result<()> {
    // Connect to ggen MCP server via stdio
    let transport = StdioTransport::new("ggen", vec!["mcp", "start"]);
    let mut client = Client::new(transport).await?;

    println!("Connected to ggen MCP server");

    // Example 1: List templates
    let templates = client.call_tool(
        "ggen_template_list",
        json!({})
    ).await?;
    println!("Available templates: {:#?}", templates);

    // Example 2: Search marketplace
    let search_results = client.call_tool(
        "ggen_market_search",
        json!({
            "query": "rust axum",
            "limit": 5
        })
    ).await?;
    println!("Search results: {:#?}", search_results);

    // Example 3: Generate code
    let gen_result = client.call_tool(
        "ggen_gen_with_vars",
        json!({
            "template": "templates/rust-struct.tmpl",
            "vars": {
                "name": "User",
                "fields": ["id", "email", "name"],
                "derive": ["Debug", "Clone", "Serialize"]
            },
            "output": "src/models/user.rs"
        })
    ).await?;
    println!("Generated: {:#?}", gen_result);

    // Example 4: RDF graph query
    client.call_tool(
        "ggen_graph_load",
        json!({
            "file": "ontology.ttl",
            "format": "turtle"
        })
    ).await?;

    let sparql_result = client.call_tool(
        "ggen_graph_query",
        json!({
            "query": "SELECT ?entity ?label WHERE { ?entity rdfs:label ?label } LIMIT 10"
        })
    ).await?;
    println!("SPARQL results: {:#?}", sparql_result);

    Ok(())
}
```

**Run:**
```bash
cd examples/mcp/rust-client
cargo run
```

---

### 4. Python Client (MCP SDK)

**File: `python-client/basic_usage.py`**

```python
import asyncio
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client

async def main():
    # Connect to ggen MCP server
    server_params = StdioServerParameters(
        command="ggen",
        args=["mcp", "start"],
        env={"RUST_LOG": "info"}
    )

    async with stdio_client(server_params) as (read, write):
        async with ClientSession(read, write) as session:
            # Initialize
            await session.initialize()

            print("Connected to ggen MCP server")

            # Example 1: List tools
            tools = await session.list_tools()
            print(f"Available tools: {len(tools)}")
            for tool in tools[:5]:
                print(f"  - {tool.name}: {tool.description}")

            # Example 2: Generate code
            result = await session.call_tool(
                "ggen_gen_with_vars",
                {
                    "template": "templates/python-dataclass.tmpl",
                    "vars": {
                        "class_name": "Product",
                        "fields": {
                            "id": "int",
                            "name": "str",
                            "price": "float"
                        }
                    },
                    "output": "models/product.py"
                }
            )
            print(f"Generated: {result}")

            # Example 3: RDF operations
            await session.call_tool(
                "ggen_graph_add_triple",
                {
                    "subject": "http://example.org/Product",
                    "predicate": "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
                    "object": "http://www.w3.org/2002/07/owl#Class"
                }
            )

            # Example 4: SPARQL query
            sparql_result = await session.call_tool(
                "ggen_graph_query",
                {
                    "query": "SELECT * WHERE { ?s ?p ?o } LIMIT 5"
                }
            )
            print(f"SPARQL results: {sparql_result}")

if __name__ == "__main__":
    asyncio.run(main())
```

**Run:**
```bash
cd examples/mcp/python-client
pip install -r requirements.txt
python basic_usage.py
```

---

### 5. HTTP API with curl

**File: `http-api/curl/basic.sh`**

```bash
#!/bin/bash

# Start ggen MCP HTTP server
ggen mcp start --transport http --port 8080 &
SERVER_PID=$!
sleep 2

BASE_URL="http://localhost:8080"

echo "=== List Templates ==="
curl -X POST "$BASE_URL/tools/call" \
  -H "Content-Type: application/json" \
  -d '{
    "name": "ggen_template_list",
    "arguments": {}
  }' | jq

echo -e "\n=== Search Marketplace ==="
curl -X POST "$BASE_URL/tools/call" \
  -H "Content-Type: application/json" \
  -d '{
    "name": "ggen_market_search",
    "arguments": {
      "query": "python flask",
      "limit": 3
    }
  }' | jq

echo -e "\n=== Generate Code ==="
curl -X POST "$BASE_URL/tools/call" \
  -H "Content-Type: application/json" \
  -d '{
    "name": "ggen_gen_with_vars",
    "arguments": {
      "template": "templates/python-function.tmpl",
      "vars": {
        "function_name": "calculate_discount",
        "params": ["price", "discount_rate"],
        "return_type": "float"
      },
      "output": "utils/discount.py"
    }
  }' | jq

echo -e "\n=== Batch Operations ==="
curl -X POST "$BASE_URL/tools/batch" \
  -H "Content-Type: application/json" \
  -d '{
    "calls": [
      {
        "name": "ggen_graph_load",
        "arguments": {"file": "schema.ttl"}
      },
      {
        "name": "ggen_graph_query",
        "arguments": {
          "query": "SELECT ?entity WHERE { ?entity a owl:Class }"
        }
      },
      {
        "name": "ggen_gen_batch",
        "arguments": {
          "template": "model.tmpl",
          "vars": {"entities": "{{sparql_results}}"}
        }
      }
    ]
  }' | jq

# Cleanup
kill $SERVER_PID
```

**Run:**
```bash
cd examples/mcp/http-api/curl
chmod +x basic.sh
./basic.sh
```

---

### 6. SSE (Server-Sent Events) Client

**File: `sse-client/javascript/event-stream.js`**

```javascript
const EventSource = require('eventsource');

// Connect to ggen MCP SSE server
const eventSource = new EventSource('http://localhost:3000/sse');

eventSource.addEventListener('message', (event) => {
  const data = JSON.parse(event.data);
  console.log('Received:', data);

  if (data.method === 'ggen_gen' && data.result) {
    console.log('Code generated:', data.result.file);
  }
});

eventSource.addEventListener('error', (error) => {
  console.error('SSE error:', error);
});

// Send tool call via POST
async function callTool(name, args) {
  const response = await fetch('http://localhost:3000/tools/call', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ name, arguments: args })
  });

  return response.json();
}

// Example: Generate code
callTool('ggen_gen_with_vars', {
  template: 'templates/react-component.tmpl',
  vars: {
    component_name: 'UserProfile',
    props: ['user', 'onEdit']
  }
}).then(result => {
  console.log('Generation started:', result);
});
```

**Run:**
```bash
# Terminal 1: Start SSE server
ggen mcp start --transport sse --port 3000

# Terminal 2: Run client
cd examples/mcp/sse-client/javascript
npm install eventsource
node event-stream.js
```

---

## Advanced Examples

### Batch Code Generation Pipeline

**File: `rust-client/src/batch.rs`**

```rust
use rmcp::{Client, StdioTransport};
use serde_json::json;

async fn generate_full_stack_app(client: &mut Client) -> Result<(), Box<dyn std::error::Error>> {
    println!("Starting full-stack generation...");

    // Step 1: Load domain model
    client.call_tool(
        "ggen_graph_load",
        json!({ "file": "models/ecommerce.ttl" })
    ).await?;

    // Step 2: Query entities
    let entities = client.call_tool(
        "ggen_graph_query",
        json!({
            "query": "SELECT ?entity ?label WHERE { ?entity a owl:Class ; rdfs:label ?label }"
        })
    ).await?;

    println!("Found {} entities", entities["results"].as_array().unwrap().len());

    // Step 3: Generate backend (parallel batch)
    client.call_tool(
        "ggen_gen_batch",
        json!({
            "templates": [
                "templates/rust-model.tmpl",
                "templates/rust-repository.tmpl",
                "templates/rust-service.tmpl",
                "templates/rust-api-routes.tmpl"
            ],
            "vars": { "entities": entities["results"] },
            "parallel": true,
            "max_workers": 4
        })
    ).await?;

    // Step 4: Generate frontend
    client.call_tool(
        "ggen_gen_batch",
        json!({
            "templates": [
                "templates/react-page.tmpl",
                "templates/react-form.tmpl",
                "templates/react-list.tmpl"
            ],
            "vars": { "entities": entities["results"] },
            "parallel": true
        })
    ).await?;

    // Step 5: Generate tests
    client.call_tool(
        "ggen_gen_batch",
        json!({
            "templates": [
                "templates/unit-test.tmpl",
                "templates/integration-test.tmpl"
            ],
            "vars": { "entities": entities["results"] }
        })
    ).await?;

    println!("Full-stack app generated successfully!");

    Ok(())
}
```

### Streaming Large File Generation

**File: `python-client/advanced_workflows.py`**

```python
async def stream_large_generation(session):
    """Stream generation for large files to avoid memory issues"""

    # Enable streaming mode
    result = await session.call_tool(
        "ggen_gen_stream",
        {
            "template": "templates/large-openapi-spec.tmpl",
            "output": "docs/openapi.yaml",
            "stream": True,
            "buffer_size": 8192
        }
    )

    # Process chunks as they arrive
    async for chunk in result.stream:
        print(f"Received chunk: {len(chunk['data'])} bytes")
        # Can validate/process incrementally

    print("Large file generated successfully!")
```

---

## Configuration Examples

### Production HTTP Server

```bash
ggen mcp start \
  --transport http \
  --port 8080 \
  --host 0.0.0.0 \
  --auth-token $(cat /run/secrets/ggen-token) \
  --tls-cert /etc/ggen/server.crt \
  --tls-key /etc/ggen/server.key \
  --max-concurrent-requests 100 \
  --request-timeout 120s \
  --cache-enabled \
  --cache-size 1gb \
  --log-format json \
  --log-file /var/log/ggen/mcp.log \
  --metrics-endpoint prometheus://localhost:9090
```

### Docker Deployment

```dockerfile
FROM rust:1.75-alpine AS builder
RUN apk add --no-cache musl-dev
WORKDIR /build
COPY . .
RUN cargo build --release --bin ggen

FROM alpine:latest
RUN apk add --no-cache ca-certificates
COPY --from=builder /build/target/release/ggen /usr/local/bin/
EXPOSE 8080
CMD ["ggen", "mcp", "start", "--transport", "http", "--port", "8080", "--host", "0.0.0.0"]
```

```bash
docker build -t ggen-mcp:latest .
docker run -p 8080:8080 -v ~/.ggen:/root/.ggen ggen-mcp:latest
```

---

## Testing Examples

**File: `rust-client/tests/integration_test.rs`**

```rust
#[tokio::test]
async fn test_code_generation_workflow() {
    let mut client = setup_client().await;

    // Test template listing
    let templates = client.call_tool("ggen_template_list", json!({})).await.unwrap();
    assert!(templates["templates"].as_array().unwrap().len() > 0);

    // Test generation
    let result = client.call_tool(
        "ggen_gen",
        json!({
            "template": "test.tmpl",
            "output": "/tmp/test.rs"
        })
    ).await.unwrap();

    assert_eq!(result["status"], "success");
    assert!(std::path::Path::new("/tmp/test.rs").exists());
}
```

---

## Troubleshooting

See individual subdirectory READMEs for specific troubleshooting:
- [Claude Desktop Troubleshooting](./claude-desktop/troubleshooting.md)
- [Rust Client Issues](./rust-client/README.md#troubleshooting)
- [Python Client Issues](./python-client/README.md#common-errors)
- [HTTP API Issues](./http-api/README.md#debugging)

---

## Contributing

Add your own examples! See [CONTRIBUTING.md](../../CONTRIBUTING.md) for guidelines.
