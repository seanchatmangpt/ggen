# Template Customization Guide

How to customize the code generation templates for your specific needs.

## Overview

ggen uses [Tera](https://tera.netlify.app/) templates to generate Rust code from RDF context. Templates are located in `crates/ggen-core/templates/mcp/`.

## Template Files

| Template | Purpose | Key Variables |
|----------|---------|---------------|
| `server.rs.tera` | Server struct, lifecycle | `server`, `transport` |
| `get_info.rs.tera` | ServerHandler::get_info() | `server`, `capabilities` |
| `tools.rs.tera` | Tool router + handlers | `tools` array |
| `resources.rs.tera` | Resource handlers | `resources` array |
| `resource_templates.rs.tera` | Template handlers | `resource_templates` array |
| `prompts.rs.tera` | Prompt handlers | `prompts` array |
| `completions.rs.tera` | Completion handler | `completions` array |
| `logging.rs.tera` | Logging handlers | `logging` object |
| `main.rs.tera` | Tokio bootstrap | `server`, `transport` |

## Template Context

The JSON context passed to all templates:

```json
{
  "server": {
    "name": "my-server",
    "version": "1.0.0",
    "description": "My MCP server",
    "protocol_version": "2025-11-25"
  },
  "capabilities": {
    "tools": true,
    "resources": false,
    "prompts": false
  },
  "transport": {
    "type": "stdio",
    "host": null,
    "port": null
  },
  "tools": [
    {
      "name": "my_tool",
      "description": "Does something",
      "arguments": [
        {
          "name": "input",
          "type": "string",
          "required": true,
          "description": "Input value"
        }
      ]
    }
  ],
  "resources": [],
  "prompts": [],
  "completions": [],
  "logging": {
    "default_level": "info"
  }
}
```

## Tera Syntax Reference

### Variables

```tera
{{ server.name }}
{{ server.version }}
```

### Filters

```tera
{{ tool.name | pascal }}    <!-- my_tool → MyTool -->
{{ tool.name | snake }}     <!-- MyTool → my_tool -->
{{ tool.name | upper }}     <!-- my_tool → MY_TOOL -->
```

### Conditionals

```tera
{% if capabilities.tools %}
// Tools are enabled
{% endif %}

{% if tool.arguments %}
// Tool has arguments
{% else %}
// Tool has no arguments
{% endif %}
```

### Loops

```tera
{% for tool in tools %}
#[tool(description = "{{ tool.description }}")]
async fn {{ tool.name | snake }}(&self) -> Result<CallToolResult, McpError> {
    // Handler for {{ tool.name }}
}
{% endfor %}
```

### Nested Loops

```tera
{% for tool in tools %}
pub struct {{ tool.name | pascal }}Args {
    {% for arg in tool.arguments %}
    pub {{ arg.name }}: {% if arg.required %}{{ arg.type }}{% else %}Option<{{ arg.type }}>{% endif %},
    {% endfor %}
}
{% endfor %}
```

## Customizing Tools Template

### Adding Custom Imports

Edit `templates/mcp/tools.rs.tera`:

```tera
// Add custom imports at the top
use crate::custom::helpers;
use my_crate::special_handler;

#[tool_router]
impl {{ server.name | pascal }} {
    {% for tool in tools %}
    #[tool(description = "{{ tool.description }}")]
    async fn {{ tool.name | snake }}(
        &self,
        {% for arg in tool.arguments %}
        {{ arg.name }}: {% if arg.required %}{{ arg.type }}{% else %}Option<{{ arg.type }}>{% endif %},
        {% endfor %}
    ) -> Result<CallToolResult, McpError> {
        // Custom handler logic
        helpers::validate_{{ tool.name }}({{ tool.arguments[0].name }})?;
        special_handler::execute()

        /* ... rest of handler ... */
    }
    {% endfor %}
}
```

### Adding OTEL Spans

```tera
{% for tool in tools %}
use opentelemetry::trace::{Tracer, TraceContextExt};
use tracing::{info_span, Instrument};

#[tool(description = "{{ tool.description }}")]
async fn {{ tool.name | snake }}(
    &self,
    {% for arg in tool.arguments %}
    {{ arg.name }}: {% if arg.required %}{{ arg.type }}{% else %}Option<{{ arg.type }}>{% endif %},
    {% endfor %}
) -> Result<CallToolResult, McpError> {
    let span = tracing::info_span!(
        "tool_invocation",
        tool.name = % "{{ tool.name }}",
        tool.arguments = ?({% for arg in tool.arguments %}{{ arg.name }}{% if not loop.last %}, {% endif %}{% endfor %})
    );

    async move {
        // Tool implementation here
        Ok(result)
    }
    .instrument(span)
    .await
}
{% endfor %}
```

## Customizing Server Template

### Adding Custom Fields to Server Struct

Edit `templates/mcp/server.rs.tera`:

```tera
use rmcp::{ServerHandler, ServerInfo};
use std::sync::Arc;

pub struct {{ server.name | pascal }} {
    // Custom fields
    pub config: Arc<MyConfig>,
    pub cache: Arc<dyn Cache>,
    pub metrics: Arc<Metrics>,
}

impl {{ server.name | pascal }} {
    pub fn new() -> Self {
        Self {
            config: Arc::new(MyConfig::default()),
            cache: Arc::new(InMemoryCache::new()),
            metrics: Arc::new(Metrics::new()),
        }
    }

    // Custom method
    pub fn with_config(mut self, config: MyConfig) -> Self {
        self.config = Arc::new(config);
        self
    }
}
```

### Adding Custom Transport Methods

```tera
{% if transport.type == "http" %}
impl {{ server.name | pascal }} {
    pub async fn serve_http_custom(self, host: &str, port: u16) -> Result<(), Box<dyn std::error::Error>> {
        // Custom HTTP server setup
        let listener = tokio::net::TcpListener::bind((host, port)).await?;
        tracing::info!("Server listening on {}:{}", host, port);

        // Custom middleware
        let app = axum::Router::new()
            .layer(tower_http::trace::TraceLayer::new_for_http())
            .layer(cors::CorsLayer::permissive());

        rmcp::transport::http().serve(listener, self).await?;
        Ok(())
    }
}
{% endif %}
```

## Customizing Main Template

### Adding Custom CLI Arguments

Edit `templates/mcp/main.rs.tera`:

```tera
use clap::Parser;

#[derive(Parser, Debug)]
struct Args {
    #[arg(long, default_value = "info")]
    log_level: String,

    #[arg(long)]
    config_file: Option<String>,

    // Custom argument
    #[arg(long, default_value = "100")]
    max_connections: usize,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    // Custom tracing setup
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::from_str(&args.log_level)?)
        .init();

    // Custom config loading
    let config = if let Some(path) = args.config_file {
        MyConfig::from_file(&path)?
    } else {
        MyConfig::default()
    };

    let server = {{ server.name | pascal }}::new()
        .with_config(config);

    {% if transport.type == "stdio" %}
    server.serve_stdio().await?;
    {% elif transport.type == "http" %}
    server.serve_http("{{ transport.host | default(value="127.0.0.1") }}", {{ transport.port | default(value=3000) }}).await?;
    {% endif %}

    Ok(())
}
```

## Creating Custom Templates

### Adding a New Template

1. Create the template file:
   ```bash
   touch crates/ggen-core/templates/mcp/middleware.rs.tera
   ```

2. Add to the template list in the pipeline:
   ```rust
   // crates/ggen-core/src/pipeline/mcp.rs
   let templates = vec![
       ("server.rs", "mcp/server.rs.tera"),
       ("middleware.rs", "mcp/middleware.rs.tera"),  // New
       ("tools.rs", "mcp/tools.rs.tera"),
       // ...
   ];
   ```

3. Write the template:
   ```tera
   // middleware.rs.tera
   use axum::{
       middleware::{self, Next},
       http::StatusCode,
       response::Response,
   };

   pub async fn auth_middleware(
       req: Request,
       next: Next,
   ) -> Result<Response, StatusCode> {
       // Custom auth logic
       tracing::info!("Authenticating request");
       Ok(next.run(req).await)
   }
   ```

## Template Inheritance

### Base Template

Create `templates/mcp/base.rs.tera`:

```tera
// Base code for all generated files
// {{ file_name }} - Generated by ggen mcp generate
// DO NOT EDIT - This file is generated from {{ ontology_source }}

{% block header %}
// Standard imports
use rmcp::{{ ServerHandler, tool, tool_router }};
{% endblock %}

{% block content %}
// Main content goes here
{% endblock %}

{% block footer %}
// Footer code
{% endblock %}
```

### Derived Template

```tera
{% extends "mcp/base.rs.tera" %}

{% block content %}
#[tool_router]
impl {{ server.name | pascal }} {
    // Tool implementations
}
{% endblock %}
```

## Testing Template Changes

### 1. Modify Template

```bash
vim crates/ggen-core/templates/mcp/tools.rs.tera
```

### 2. Test Generation

```bash
ggen mcp generate \
  --ontology examples/minimal-server.ttl \
  --output /tmp/test-gen \
  --skip-compile-gate
```

### 3. Inspect Generated Code

```bash
cat /tmp/test-gen/tools.rs
```

### 4. Compile Check

```bash
cd /tmp/test-gen && cargo check
```

### 5. Commit Template

```bash
git add crates/ggen-core/templates/mcp/tools.rs.tera
git commit -m "feat(templates): add custom OTEL spans to tool handlers"
```

## Best Practices

### DO ✅

- Keep templates focused on one concern
- Use Tera's built-in filters
- Add comments explaining complex logic
- Test templates with minimal ontologies first
- Use conditional compilation for optional features

### DON'T ❌

- Embed business logic in templates
- Generate overly complex code
- Skip the compile gate in production
- Create circular dependencies between templates
- Hardcode values that should come from context

## Common Patterns

### Optional Feature Flags

```tera
{% if capabilities.resources %}
// Resource handlers
{% endif %}
```

### Default Values

```tera
{% if transport.host %}
    let host = "{{ transport.host }}";
{% else %}
    let host = "127.0.0.1";
{% endif %}
```

### Array Iteration with Index

```tera
{% for tool in tools %}
    // Tool {{ loop.index }} of {{ loop.length }}
{% endfor %}
```

### String Joining

```tera
// Join argument names with comma
{% for arg in tool.arguments %}
    {{ arg.name }}{% if not loop.last %}, {% endif %}
{% endfor %}
```

## See Also

- [Code Generation Guide](../03-code-generation/) - Pipeline internals
- [Tera Documentation](https://tera.netlify.app/docs/) - Official Tera docs
- [Examples](../06-examples/) - Sample ontologies
