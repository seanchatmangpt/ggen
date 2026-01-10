# MCP Interface Specification

**Model Context Protocol Integration for ggen Agent Tooling**

## Table of Contents

1. [Overview](#overview)
2. [MCP Protocol Overview](#mcp-protocol-overview)
3. [Tool Definition Structure](#tool-definition-structure)
4. [JSON Schema Export](#json-schema-export)
5. [Error Response Format](#error-response-format)
6. [Timeout Handling](#timeout-handling)
7. [Authentication & Authorization](#authentication--authorization)
8. [Phase 4 Implementation Plan](#phase-4-implementation-plan)
9. [Integration Examples](#integration-examples)
10. [Rationale for Phase Deferral](#rationale-for-phase-deferral)

---

## Overview

This specification defines how ggen's **Tool Registry** and **Signature system** integrate with the **Model Context Protocol (MCP)**, enabling LLM-driven agents to discover, invoke, and compose tools dynamically.

### MCP Value Proposition

MCP provides a standardized bridge between:

- **LLMs** (Claude, GPT-4, etc.) and **Tools**
- **Multiple tool sources** (local, remote, custom)
- **Stateless request/response** with rich context

### ggen + MCP Integration Points

```
┌──────────────────────────────────────────────────┐
│           LLM (Claude, GPT-4)                     │
└────────────────────────┬─────────────────────────┘
                         │
                         ▼
                    ┌─────────────┐
                    │ MCP Client  │
                    │ (Prompting) │
                    └──────┬──────┘
                           │ MCP Protocol
                           │ (JSON-RPC)
                           ▼
                    ┌──────────────────────┐
                    │ ggen MCP Server      │
                    │ ┌────────────────┐   │
                    │ │ Tool Discovery │   │
                    │ │ Tool Invocation │   │
                    │ │ Error Handling  │   │
                    │ └────────────────┘   │
                    └─────────┬────────────┘
                              │
                    ┌─────────▼──────────┐
                    │ Tool Registry      │
                    │ ┌──────────────┐   │
                    │ │ Signatures   │   │
                    │ │ Validators   │   │
                    │ │ Constraints  │   │
                    │ └──────────────┘   │
                    └────────────────────┘
```

---

## MCP Protocol Overview

### Request/Response Model

MCP uses JSON-RPC 2.0 for all communication:

```json
// Request (LLM → MCP Server)
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/list",
  "params": {}
}

// Response (MCP Server → LLM)
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "tools": [
      {
        "name": "FinancialAnalyzer",
        "description": "Analyzes stock data",
        "inputSchema": { ... }
      }
    ]
  }
}
```

### Core Methods

| Method | Purpose | Phase |
|--------|---------|-------|
| `tools/list` | Discover all available tools | Phase 3 |
| `tools/call` | Invoke a tool with parameters | Phase 3 |
| `tools/validate` | Validate input against schema | Phase 4 |
| `resources/list` | List external data sources | Phase 4 |
| `resources/read` | Read resource content | Phase 4 |

---

## Tool Definition Structure

### MCP Tool Definition

```json
{
  "name": "FinancialAnalyzer",
  "description": "Analyzes stock data and returns investment recommendations",
  "inputSchema": {
    "type": "object",
    "properties": {
      "ticker_symbol": {
        "type": "string",
        "description": "Stock ticker symbol (e.g., AAPL)",
        "pattern": "^[A-Z]{1,5}$",
        "minLength": 1,
        "maxLength": 5
      },
      "period_days": {
        "type": "integer",
        "description": "Historical analysis period in days",
        "minimum": 1,
        "maximum": 365
      },
      "analysis_type": {
        "type": "string",
        "description": "Type of analysis",
        "enum": ["technical", "fundamental", "sentiment"],
        "default": "technical"
      }
    },
    "required": ["ticker_symbol", "period_days"]
  }
}
```

### Mapping Signature → MCP Tool Definition

```rust
impl Signature {
    /// Convert Signature to MCP tool definition
    pub fn to_mcp_tool(&self) -> McpToolDefinition {
        McpToolDefinition {
            name: self.name.clone(),
            description: self.description.clone(),
            input_schema: self.to_json_schema(),
        }
    }
}

pub struct McpToolDefinition {
    pub name: String,
    pub description: String,
    pub input_schema: JsonSchema,
}
```

### InputField → JSON Schema Property

| InputField | JSON Schema |
|-----------|------------|
| `String` type | `"type": "string"` |
| `Integer` type | `"type": "integer"` |
| `Float` type | `"type": "number"` |
| `Boolean` type | `"type": "boolean"` |
| `min_length` constraint | `"minLength": value` |
| `max_length` constraint | `"maxLength": value` |
| `pattern` constraint | `"pattern": "regex"` |
| `min_value` constraint | `"minimum": value` |
| `max_value` constraint | `"maximum": value` |
| required field | Add to `required: []` array |

### Example: Converting Signature to JSON Schema

```rust
pub fn signature_to_json_schema(signature: &Signature) -> serde_json::Value {
    let mut properties = serde_json::Map::new();
    let mut required = Vec::new();

    // Process input fields
    for input_field in &signature.inputs {
        let mut prop = serde_json::json!({
            "type": map_type(&input_field.type_annotation()),
            "description": &input_field.desc(),
        });

        // Add constraints
        if let Some(obj) = prop.as_object_mut() {
            for constraint in &input_field.constraints {
                match constraint {
                    FieldConstraint::MinLength(min) => {
                        obj.insert("minLength".to_string(), json!(min));
                    }
                    FieldConstraint::MaxLength(max) => {
                        obj.insert("maxLength".to_string(), json!(max));
                    }
                    FieldConstraint::Pattern(regex) => {
                        obj.insert("pattern".to_string(), json!(regex));
                    }
                    FieldConstraint::MinValue(min) => {
                        obj.insert("minimum".to_string(), json!(min));
                    }
                    FieldConstraint::MaxValue(max) => {
                        obj.insert("maximum".to_string(), json!(max));
                    }
                    _ => {}
                }
            }
        }

        properties.insert(input_field.name().to_string(), prop);

        if input_field.is_required() {
            required.push(input_field.name().to_string());
        }
    }

    serde_json::json!({
        "type": "object",
        "properties": properties,
        "required": required,
    })
}

fn map_type(rust_type: &str) -> &'static str {
    match rust_type {
        "String" => "string",
        "i32" | "i64" | "u32" | "u64" => "integer",
        "f32" | "f64" => "number",
        "bool" => "boolean",
        _ => "string", // Default
    }
}
```

---

## JSON Schema Export

### Full JSON Schema Generation

```rust
pub struct JsonSchemaExporter;

impl JsonSchemaExporter {
    pub fn export_tool_catalog(registry: &ToolRegistry) -> serde_json::Value {
        let tools = registry.list_all().await;
        let mut tools_schema = Vec::new();

        for tool_name in tools {
            if let Some(tool) = registry.get(&tool_name).await {
                tools_schema.push(serde_json::json!({
                    "name": tool.name(),
                    "description": tool.signature().description,
                    "inputSchema": signature_to_json_schema(tool.signature()),
                    "tags": ["financial", "analysis"], // Derived from domain
                }));
            }
        }

        serde_json::json!({
            "tools": tools_schema,
            "version": "1.0.0",
            "generated_at": chrono::Utc::now().to_rfc3339(),
        })
    }

    pub fn export_schema_for_tool(tool: &dyn Tool) -> JsonSchema {
        let sig = tool.signature();
        JsonSchema {
            schema: signature_to_json_schema(sig),
            examples: self.generate_examples(sig),
            metadata: self.extract_metadata(sig),
        }
    }

    fn generate_examples(&self, sig: &Signature) -> Vec<serde_json::Value> {
        // Generate valid example inputs from constraints
        vec![
            serde_json::json!({
                "ticker_symbol": "AAPL",
                "period_days": 90,
            }),
            serde_json::json!({
                "ticker_symbol": "MSFT",
                "period_days": 30,
            }),
        ]
    }

    fn extract_metadata(&self, sig: &Signature) -> Metadata {
        Metadata {
            category: "finance".to_string(),
            version: "1.0.0".to_string(),
            deprecated: false,
            author: "ggen-ai".to_string(),
        }
    }
}
```

### OpenAPI Compatibility

```json
{
  "openapi": "3.0.0",
  "info": {
    "title": "ggen Tool Registry",
    "version": "1.0.0"
  },
  "paths": {
    "/tools/{tool_name}": {
      "post": {
        "summary": "Invoke a tool",
        "parameters": [
          {
            "name": "tool_name",
            "in": "path",
            "required": true,
            "schema": { "type": "string" }
          }
        ],
        "requestBody": {
          "required": true,
          "content": {
            "application/json": {
              "schema": { "$ref": "#/components/schemas/ToolInput" }
            }
          }
        },
        "responses": {
          "200": {
            "description": "Tool execution successful",
            "content": {
              "application/json": {
                "schema": { "$ref": "#/components/schemas/ToolOutput" }
              }
            }
          },
          "400": { "$ref": "#/components/responses/ValidationError" },
          "404": { "$ref": "#/components/responses/NotFound" }
        }
      }
    }
  }
}
```

---

## Error Response Format

### MCP Standard Error Response

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": -32603,
    "message": "Internal error",
    "data": {
      "error_type": "ValidationError",
      "field": "ticker_symbol",
      "reason": "Pattern mismatch: expected ^[A-Z]{1,5}$, got 'invalid123'",
      "trace_id": "req-12345"
    }
  }
}
```

### Error Code Mapping

| Error Type | Code | Meaning | Retry |
|-----------|------|---------|-------|
| ParseError | -32700 | Invalid JSON sent | No |
| InvalidRequest | -32600 | Malformed request | No |
| MethodNotFound | -32601 | Unknown method | No |
| InvalidParams | -32602 | Invalid parameters | No |
| InternalError | -32603 | Server error | Yes |
| ValidationError | -32001 | Input validation failed | No |
| ToolNotFound | -32002 | Tool doesn't exist | No |
| ToolExecution | -32003 | Tool crashed/timeout | Yes |
| AuthenticationError | -32004 | Auth failed | No |
| AuthorizationError | -32005 | Permission denied | No |

### Detailed Error Response

```rust
#[derive(Serialize)]
pub struct McpErrorResponse {
    pub jsonrpc: String,
    pub id: serde_json::Value,
    pub error: McpError,
}

#[derive(Serialize)]
pub struct McpError {
    pub code: i32,
    pub message: String,
    pub data: Option<ErrorData>,
}

#[derive(Serialize)]
pub struct ErrorData {
    pub error_type: String,
    pub field: Option<String>,
    pub reason: String,
    pub trace_id: String,
    pub suggestions: Vec<String>,
    pub documentation_url: Option<String>,
}

// Example: Validation error
let error = McpErrorResponse {
    jsonrpc: "2.0".to_string(),
    id: json!(1),
    error: McpError {
        code: -32001,
        message: "Validation failed".to_string(),
        data: Some(ErrorData {
            error_type: "FieldValidation".to_string(),
            field: Some("ticker_symbol".to_string()),
            reason: "Pattern mismatch: expected ^[A-Z]{1,5}$, got 'invalid123'".to_string(),
            trace_id: "req-12345".to_string(),
            suggestions: vec![
                "Use uppercase letters only".to_string(),
                "Length must be 1-5 characters".to_string(),
            ],
            documentation_url: Some("https://docs.example.com/ticker-format".to_string()),
        }),
    },
};
```

---

## Timeout Handling

### Timeout Configuration

```rust
pub struct McpServerConfig {
    /// Maximum time to wait for tool execution
    pub tool_timeout: Duration,

    /// Maximum time to wait for validation
    pub validation_timeout: Duration,

    /// Maximum time to wait for tool discovery
    pub discovery_timeout: Duration,

    /// Maximum time to hold locks
    pub lock_timeout: Duration,
}

impl Default for McpServerConfig {
    fn default() -> Self {
        Self {
            tool_timeout: Duration::from_secs(30),
            validation_timeout: Duration::from_secs(5),
            discovery_timeout: Duration::from_secs(2),
            lock_timeout: Duration::from_millis(500),
        }
    }
}
```

### Timeout Enforcement

```rust
pub async fn invoke_with_timeout(
    tool: &dyn Tool,
    input: &InputData,
    timeout: Duration,
) -> Result<OutputData> {
    let result = tokio::time::timeout(timeout, async {
        tool.execute(input).await
    }).await;

    match result {
        Ok(Ok(output)) => Ok(output),
        Ok(Err(e)) => Err(e),
        Err(_) => Err(GgenAiError::ToolTimeout {
            tool_name: tool.name().to_string(),
            timeout_ms: timeout.as_millis() as u64,
        }),
    }
}
```

### Timeout Response

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": -32003,
    "message": "Tool execution timeout",
    "data": {
      "error_type": "ToolTimeout",
      "tool_name": "FinancialAnalyzer",
      "timeout_ms": 30000,
      "trace_id": "req-12345",
      "suggestions": [
        "Increase timeout for long-running analysis",
        "Check tool server health",
        "Break request into smaller chunks"
      ]
    }
  }
}
```

---

## Authentication & Authorization

### API Key Authentication

```rust
pub struct McpAuth {
    api_keys: Arc<RwLock<HashMap<String, ApiKeyInfo>>>,
    permissions: Arc<RwLock<HashMap<String, Vec<Permission>>>>,
}

pub struct ApiKeyInfo {
    pub key_hash: String,
    pub created_at: SystemTime,
    pub last_used: Option<SystemTime>,
    pub permissions: Vec<Permission>,
    pub rate_limit: Option<RateLimit>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Permission {
    ToolInvoke(String),  // Can invoke specific tool
    ToolRead(String),    // Can read tool schema
    ToolsListAll,        // Can list all tools
    AdminWrite,          // Can register new tools
    AdminRead,           // Can read all registrations
}

pub struct RateLimit {
    pub requests_per_minute: u32,
    pub requests_per_hour: u32,
}

impl McpAuth {
    pub async fn authenticate(&self, api_key: &str) -> Result<()> {
        let key_hash = sha256(api_key);
        let info = self.api_keys.read().await
            .values()
            .find(|k| k.key_hash == key_hash)
            .ok_or(GgenAiError::AuthenticationFailed)?;

        // Update last_used
        // Check rate limit
        Ok(())
    }

    pub async fn authorize(
        &self,
        api_key: &str,
        permission: &Permission,
    ) -> Result<()> {
        self.authenticate(api_key).await?;

        let key_hash = sha256(api_key);
        let info = self.api_keys.read().await
            .values()
            .find(|k| k.key_hash == key_hash)
            .ok_or(GgenAiError::AuthorizationFailed)?;

        if info.permissions.contains(permission) {
            Ok(())
        } else {
            Err(GgenAiError::AuthorizationFailed)
        }
    }
}
```

### Bearer Token in MCP Request

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "FinancialAnalyzer",
    "arguments": { "ticker_symbol": "AAPL", "period_days": 90 }
  },
  "auth": {
    "type": "bearer",
    "token": "sk-1234567890abcdef"
  }
}
```

---

## Phase 4 Implementation Plan

### Current Status: Phase 3

**Completed (Phase 3)**:
- Tool Registry core (register, get, list, invoke)
- Signature type system
- InputField/OutputField definitions
- Basic validation constraints
- Agent trait & lifecycle

**Deferred to Phase 4**:
- Full MCP server implementation
- Complex validation rules
- Resource management
- Advanced error recovery
- Performance optimization

### Phase 4 Deliverables

```
Phase 4: MCP Server & Advanced Integration (8-10 weeks)

1. MCP Server Implementation (3 weeks)
   - JSON-RPC 2.0 protocol handler
   - tools/list, tools/call methods
   - Error mapping & response formatting
   - Streaming support for large outputs

2. Advanced Validation (2 weeks)
   - Schema validation engine
   - Custom validator plugins
   - Constraint composition
   - Performance optimization

3. Resource Management (2 weeks)
   - Resource discovery (resources/list)
   - Resource reading (resources/read)
   - Cache management
   - TTL enforcement

4. Security Hardening (2 weeks)
   - Authentication system
   - Authorization policies
   - Rate limiting
   - Audit logging

5. Integration Testing (1 week)
   - End-to-end MCP workflows
   - Performance benchmarks
   - Security validation
   - Load testing
```

### Why Phase 4 Deferral

**Rationale**:

1. **Sufficient for Phase 3 Goals**
   - Tool Registry enables agent-to-tool binding
   - Signatures provide validation
   - Local tool invocation works end-to-end

2. **External Dependencies Ready**
   - MCP protocol stable (>1.0)
   - JSON-RPC libraries mature
   - Auth standards established

3. **Unblocks Critical Path**
   - Phase 3 delivery on schedule
   - Phase 4 can start immediately
   - No technical debt created

4. **Scalable Implementation**
   - MCP layer adds cleanly (trait-based)
   - No refactoring of core needed
   - Testing patterns established

---

## Integration Examples

### Example 1: MCP Server Discovery Request

```rust
// LLM asks: "What tools are available?"
// MCP Request
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/list",
  "params": {}
}

// ggen MCP Server Response
pub async fn handle_tools_list(&self) -> McpResponse {
    let tools = self.registry.list_all().await;
    let mut mcp_tools = Vec::new();

    for tool_name in tools {
        if let Some(tool) = self.registry.get(&tool_name).await {
            mcp_tools.push(serde_json::json!({
                "name": tool.name(),
                "description": tool.signature().description,
                "inputSchema": signature_to_json_schema(tool.signature()),
            }));
        }
    }

    McpResponse {
        jsonrpc: "2.0".to_string(),
        id: 1,
        result: serde_json::json!({
            "tools": mcp_tools
        }),
    }
}
```

### Example 2: MCP Tool Invocation

```rust
// LLM calls: FinancialAnalyzer with AAPL, 90 days
// MCP Request
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "tools/call",
  "params": {
    "name": "FinancialAnalyzer",
    "arguments": {
      "ticker_symbol": "AAPL",
      "period_days": 90
    }
  }
}

// ggen MCP Server Handler
pub async fn handle_tools_call(
    &self,
    name: &str,
    arguments: serde_json::Value,
) -> McpResponse {
    // Convert arguments to InputData
    let input = InputData::from_json(arguments)
        .map_err(|e| McpError::invalid_params(e.to_string()))?;

    // Invoke through registry (with validation)
    let output = self.registry.invoke(name, &input).await
        .map_err(|e| McpError::tool_execution(e.to_string()))?;

    McpResponse {
        jsonrpc: "2.0".to_string(),
        id: 2,
        result: serde_json::to_value(output).ok(),
    }
}
```

---

## Rationale for Phase Deferral

### Technical Justification

**MCP Server is an abstraction layer**:
- Sits atop Tool Registry
- Provides JSON-RPC transport
- Maps MCP methods to Registry operations
- Can be implemented without Registry changes

**Core functionality complete in Phase 3**:
- Registry implements all necessary operations
- Signature validation works
- Error types defined
- Agent-tool binding proven

**Phase 4 adds convenience, not capability**:
- HTTP/WebSocket transport (MCP)
- Standard JSON-RPC interface
- Auth/rate limiting (operational)
- These are additive features

### Business Impact

| Factor | Impact | Details |
|--------|--------|---------|
| **Delivery Risk** | ↓ Reduced | Smaller scope = better focus |
| **Code Quality** | ↑ Improved | Time for hardening tests |
| **Technical Debt** | → Neutral | Clean separation enforced |
| **Feature Parity** | ↑ Complete | Phase 3 unblocks MCP design |

### Implementation Continuity

Phase 4 implementation is **straightforward** because:

1. MCP Server becomes thin wrapper over Registry
2. No breaking changes to Phase 3 code
3. Testing doubles validate API contract
4. Performance proven in Phase 3

---

## Reference

- **MCP Specification**: https://modelcontextprotocol.io/
- **JSON Schema**: https://json-schema.org/
- **Tool Registry**: `crates/ggen-ai/src/` (registry module)
- **Signature System**: `crates/ggen-ai/src/dspy/`
- **Phase 3 Implementation**: `crates/ggen-ai/src/generators/validator/`
