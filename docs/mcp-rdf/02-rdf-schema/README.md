# RDF Schema Reference

Complete reference for the ggen MCP ontology (`http://ggen.dev/mcp#`).

## Overview

The MCP ontology defines all classes and properties needed to specify an MCP 2025-11-25 server in RDF/Turtle. The ontology uses OWL semantics and follows RDF 1.2 standards.

**Base URI:** `http://ggen.dev/mcp#`
**Prefix:** `mcp:`

## Core Classes

### McpServer

The top-level server definition.

**Properties:**
| Property | Range | Required | Description |
|----------|-------|----------|-------------|
| `mcp:serverName` | xsd:string | ✅ Yes | Server identifier |
| `mcp:serverVersion` | xsd:string | ✅ Yes | Semantic version |
| `mcp:serverDescription` | xsd:string | No | Human-readable description |
| `mcp:hasProtocolVersion` | xsd:string | ✅ Yes | MCP protocol version (typically "2025-11-25") |
| `mcp:hasCapabilitySet` | `mcp:CapabilitySet` | ✅ Yes | Feature flags |
| `mcp:hasTransport` | `mcp:Transport` | ✅ Yes | Communication mechanism |
| `mcp:hasTool` | `mcp:Tool` | No* | Tool definitions (0+) |
| `mcp:hasResource` | `mcp:Resource` | No* | Resource definitions (0+) |
| `mcp:hasPrompt` | `mcp:Prompt` | No* | Prompt definitions (0+) |
| `mcp:hasCompletionProvider` | `mcp:CompletionProvider` | No | Autocomplete (0+) |
| `mcp:hasLoggingPolicy` | `mcp:LoggingPolicy` | No | Logging config (0-1) |

**Example:**
```turtle
:MyServer a mcp:McpServer ;
    mcp:serverName "my-server" ;
    mcp:serverVersion "1.0.0" ;
    mcp:hasProtocolVersion "2025-11-25" ;
    mcp:hasCapabilitySet :MyCaps ;
    mcp:hasTransport mcp:StdioTransport ;
    mcp:hasTool :HelloTool .
```

---

### CapabilitySet

Boolean flags for MCP capabilities.

**Properties:**
| Property | Range | Required | Default |
|----------|-------|----------|---------|
| `mcp:tools` | xsd:boolean | No | false |
| `mcp:resources` | xsd:boolean | No | false |
| `mcp:prompts` | xsd:boolean | No | false |
| `mcp:completions` | xsd:boolean | No | false |
| `mcp:logging` | xsd:boolean | No | false |
| `mcp:subscriptions` | xsd:boolean | No | false |

**Example:**
```turtle
:MyCaps a mcp:CapabilitySet ;
    mcp:tools true ;
    mcp:resources true ;
    mcp:prompts false .
```

---

### Tool

An invokable function exposed by the server.

**Properties:**
| Property | Range | Required | Description |
|----------|-------|----------|-------------|
| `mcp:name` | xsd:string | ✅ Yes | Tool identifier (snake_case) |
| `mcp:description` | xsd:string | ✅ Yes | What the tool does |
| `mcp:hasArgument` | `mcp:ToolArgument` | No* | Parameters (0+) |
| `mcp:implementedBy` | xsd:string | No | Rust module path (e.g., `crate::tools::hello`) |

**Example:**
```turtle
:ValidateTool a mcp:Tool ;
    mcp:name "validate_pipeline" ;
    mcp:description "Validate the code generation pipeline" ;
    mcp:hasArgument :OntologyPathArg ;
    mcp:implementedBy "ggen_a2a_mcp::tools::validate" .
```

---

### ToolArgument

Named, typed parameter for a tool.

**Properties:**
| Property | Range | Required | Description |
|----------|-------|----------|-------------|
| `mcp:argumentName` | xsd:string | ✅ Yes | Parameter name (snake_case) |
| `mcp:argumentDescription` | xsd:string | No | What this parameter is for |
| `mcp:argumentType` | xsd:string | ✅ Yes | JSON Schema type |
| `mcp:isRequired` | xsd:boolean | No | Required flag (default: false) |
| `mcp:defaultValue` | xsd:string | No | Default value (if optional) |

**Supported Types:**
- `string` - Text value
- `number` - Integer or float
- `integer` - Integer only
- `boolean` - true/false
- `array` - List of values
- `object` - JSON object
- `null` - Null value

**Example:**
```turtle
:PathArg a mcp:ToolArgument ;
    mcp:argumentName "ontology_path" ;
    mcp:argumentDescription "Path to the RDF ontology file" ;
    mcp:argumentType "string" ;
    mcp:isRequired true .
```

---

### Resource

Named, addressable data exposed by the server.

**Properties:**
| Property | Range | Required | Description |
|----------|-------|----------|-------------|
| `mcp:uri` | xsd:string | ✅ Yes | Resource URI (e.g., `ggen://config`) |
| `mcp:resourceName` | xsd:string | ✅ Yes | Display name |
| `mcp:description` | xsd:string | No | What this resource is |
| `mcp:mimeType` | xsd:string | No | MIME type (default: `text/plain`) |
| `mcp:resourceImplementedBy` | xsd:string | No | Rust handler function |

**Example:**
```turtle
:ConfigResource a mcp:Resource ;
    mcp:uri "ggen://config" ;
    mcp:resourceName "Project Configuration" ;
    mcp:description "Current ggen.toml configuration" ;
    mcp:mimeType "application/json" .
```

---

### ResourceTemplate

URI-template pattern for parameterized resources.

**Properties:**
| Property | Range | Required | Description |
|----------|-------|----------|-------------|
| `mcp:uriPattern` | xsd:string | ✅ Yes | URI template (RFC 6570) |
| `mcp:name` | xsd:string | ✅ Yes | Template identifier |
| `mcp:description` | xsd:string | No | What this template matches |
| `mcp:mimeType` | xsd:string | No | MIME type |

**Example:**
```turtle
:ExampleTemplate a mcp:ResourceTemplate ;
    mcp:uriPattern "ggen://examples/{name}" ;
    mcp:name "examples_by_name" ;
    mcp:description "Get example by name" ;
    mcp:mimeType "application/json" .
```

---

### Prompt

Reusable prompt template for LLM interactions.

**Properties:**
| Property | Range | Required | Description |
|----------|-------|----------|-------------|
| `mcp:promptName` | xsd:string | ✅ Yes | Prompt identifier |
| `mcp:description` | xsd:string | No | What this prompt does |
| `mcp:hasPromptArgument` | `mcp:PromptArgument` | No* | Arguments (0+) |
| `mcp:promptImplementedBy` | xsd:string | No | Rust handler function |

**Example:**
```turtle
:ExplainSchemaPrompt a mcp:Prompt ;
    mcp:promptName "explain_rdf_schema" ;
    mcp:description "Explain an RDF schema in natural language" ;
    mcp:hasPromptArgument :FileArg .
```

---

### PromptArgument

Argument for prompt template substitution.

**Properties:**
| Property | Range | Required | Description |
|----------|-------|----------|-------------|
| `mcp:argumentName` | xsd:string | ✅ Yes | Parameter name |
| `mcp:argumentDescription` | xsd:string | No | What this argument is for |
| `mcp:argumentType` | xsd:string | ✅ Yes | JSON Schema type |
| `mcp:isRequired` | xsd:boolean | No | Required flag |

**Example:**
```turtle
:FileArg a mcp:PromptArgument ;
    mcp:argumentName "file_path" ;
    mcp:argumentDescription "Path to RDF schema file" ;
    mcp:argumentType "string" ;
    mcp:isRequired true .
```

---

### CompletionProvider

Autocomplete suggestions for tool arguments.

**Properties:**
| Property | Range | Required | Description |
|----------|-------|----------|-------------|
| `mcp:completionRefType` | xsd:string | ✅ Yes | Reference type ("tool" or "prompt") |
| `mcp:completionRefName` | xsd:string | ✅ Yes | Name of tool/prompt |
| `mcp:completionArgument` | xsd:string | ✅ Yes | Argument name to complete |
| `mcp:completionValues` | xsd:string | ✅ Yes | Comma-separated values |

**Example:**
```turtle
:SyncTargetCompletion a mcp:CompletionProvider ;
    mcp:completionRefType "tool" ;
    mcp:completionRefName "sync" ;
    mcp:completionArgument "target" ;
    mcp:completionValues "rust,elixir,full" .
```

---

### LoggingPolicy

Server logging configuration.

**Properties:**
| Property | Range | Required | Description |
|----------|-------|----------|-------------|
| `mcp:defaultLevel` | xsd:string | No | Default log level (default: "info") |
| `mcp:supportedLevels` | xsd:string | No | Comma-separated levels |

**Supported Levels:**
`debug`, `info`, `notice`, `warning`, `error`, `critical`, `alert`, `emergency`

**Example:**
```turtle
:MyLogging a mcp:LoggingPolicy ;
    mcp:defaultLevel "info" ;
    mcp:supportedLevels "debug,info,warning,error" .
```

---

### Transport

Communication mechanism for the server.

**Pre-defined Instances:**
| Instance | Description | Rust SDK Mapping |
|----------|-------------|------------------|
| `mcp:StdioTransport` | Standard input/output | `rmcp::transport::stdio` |
| `mcp:HttpTransport` | HTTP server | `rmcp::transport::http` |
| `mcp:SseTransport` | Server-Sent Events | `rmcp::transport::sse` |
| `mcp:WebSocketTransport` | WebSocket | `rmcp::transport::websocket` |

**Example:**
```turtle
:MyServer a mcp:McpServer ;
    mcp:hasTransport mcp:HttpTransport .
```

---

## Property Reference

### Domain-Specific Properties

| Property | Domain | Range | Description |
|----------|--------|-------|-------------|
| `mcp:serverName` | McpServer | xsd:string | Server name |
| `mcp:serverVersion` | McpServer | xsd:string | Semantic version |
| `mcp:hasProtocolVersion` | McpServer | xsd:string | MCP version |
| `mcp:hasCapabilitySet` | McpServer | CapabilitySet | Capabilities |
| `mcp:hasTransport` | McpServer | Transport | Transport |
| `mcp:hasTool` | McpServer | Tool | Tool |
| `mcp:hasResource` | McpServer | Resource | Resource |
| `mcp:hasPrompt` | McpServer | Prompt | Prompt |
| `mcp:name` | Tool, Prompt | xsd:string | Name |
| `mcp:description` | McpServer, Tool, Resource, Prompt, ResourceTemplate | xsd:string | Description |
| `mcp:hasArgument` | Tool | ToolArgument | Tool argument |
| `mcp:hasPromptArgument` | Prompt | PromptArgument | Prompt argument |
| `mcp:isRequired` | ToolArgument, PromptArgument | xsd:boolean | Required flag |
| `mcp:argumentType` | ToolArgument, PromptArgument | xsd:string | Type |
| `mcp:argumentName` | ToolArgument, PromptArgument | xsd:string | Argument name |

---

## Complete Example

```turtle
@prefix mcp: <http://ggen.dev/mcp#> .
@prefix ex:  <http://example.com/server#> .

ex:GgenServer a mcp:McpServer ;
    mcp:serverName "ggen-mcp" ;
    mcp:serverVersion "6.0.1" ;
    mcp:serverDescription "ggen code generation MCP server" ;
    mcp:hasProtocolVersion "2025-11-25" ;
    mcp:hasTransport mcp:StdioTransport ;
    mcp:hasCapabilitySet ex:FullCapabilities ;
    mcp:hasTool ex:SyncTool, ex:ValidateTool ;
    mcp:hasResource ex:ConfigResource ;
    mcp:hasPrompt ex:ExplainPrompt ;
    mcp:hasCompletionProvider ex:TargetCompletion ;
    mcp:hasLoggingPolicy ex:DefaultLogging .

ex:FullCapabilities a mcp:CapabilitySet ;
    mcp:tools true ;
    mcp:resources true ;
    mcp:prompts true ;
    mcp:completions true ;
    mcp:logging true .

ex:SyncTool a mcp:Tool ;
    mcp:name "sync" ;
    mcp:description "Execute the full code generation pipeline" ;
    mcp:hasArgument ex:OntologyArg .

ex:OntologyArg a mcp:ToolArgument ;
    mcp:argumentName "ontology_path" ;
    mcp:argumentDescription "Path to RDF ontology file" ;
    mcp:argumentType "string" ;
    mcp:isRequired true .

ex:ConfigResource a mcp:Resource ;
    mcp:uri "ggen://config" ;
    mcp:resourceName "Configuration" ;
    mcp:mimeType "application/json" .

ex:ExplainPrompt a mcp:Prompt ;
    mcp:promptName "explain_schema" ;
    mcp:description "Explain RDF schema" .

ex:TargetCompletion a mcp:CompletionProvider ;
    mcp:completionRefType "tool" ;
    mcp:completionRefName "sync" ;
    mcp:completionArgument "target" ;
    mcp:completionValues "rust,elixir,full" .

ex:DefaultLogging a mcp:LoggingPolicy ;
    mcp:defaultLevel "info" .
```

---

## See Also

- [Quick Start](../01-quick-start/) - Get started with examples
- [Code Generation Guide](../03-code-generation/) - How generation works
- [SPARQL Guide](../05-sparql-guide/) - Query extraction patterns
