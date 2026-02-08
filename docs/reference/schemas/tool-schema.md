# MCP Tool Schema Reference

Complete reference for MCP (Model Context Protocol) tool schemas used in ggen.

## Overview

MCP tools are defined using JSON Schema and describe the interface for interacting with agents and services. Tools specify their input parameters, return types, and behavior.

## Tool Schema Structure

### Basic Schema

```json
{
  "name": "tool-name",
  "description": "Tool description",
  "type": "object",
  "properties": {
    "param1": {
      "type": "string",
      "description": "Parameter description"
    }
  },
  "required": ["param1"]
}
```

### Schema Properties

| Property | Type | Required | Description |
|----------|------|----------|-------------|
| `name` | string | **Yes** | Tool identifier (kebab-case) |
| `description` | string | **Yes** | Human-readable tool description |
| `type` | string | **Yes** | Must be `"object"` |
| `properties` | object | No | Input parameter definitions |
| `required` | array | No | List of required parameter names |

---

## Core MCP Tools

### agent-list

List all registered A2A agents.

**Schema:**

```json
{
  "name": "agent-list",
  "description": "List all registered agents",
  "type": "object",
  "properties": {},
  "required": []
}
```

**Parameters:** None

**Returns:**

```json
{
  "agents": [
    {
      "id": "text-generator",
      "name": "Text Generator",
      "agent_type": "generation",
      "status": "running",
      "capabilities": ["text-generation", "summarization"]
    }
  ]
}
```

---

### agent-start

Start an A2A agent.

**Schema:**

```json
{
  "name": "agent-start",
  "description": "Start an agent",
  "type": "object",
  "properties": {
    "name": {
      "type": "string",
      "description": "Agent name to start"
    },
    "timeout": {
      "type": "integer",
      "description": "Startup timeout in seconds",
      "default": 30,
      "minimum": 1
    }
  },
  "required": ["name"]
}
```

**Parameters:**

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `name` | string | **Yes** | - | Agent name to start |
| `timeout` | integer | No | `30` | Startup timeout (min: 1) |

**Returns:**

```json
{
  "status": "started",
  "agent_id": "text-generator",
  "name": "Text Generator"
}
```

---

### agent-status

Get detailed status for an agent.

**Schema:**

```json
{
  "name": "agent-status",
  "description": "Show agent status",
  "type": "object",
  "properties": {
    "agent": {
      "type": "string",
      "description": "Agent name or ID"
    },
    "include_metrics": {
      "type": "boolean",
      "description": "Include performance metrics",
      "default": false
    }
  },
  "required": ["agent"]
}
```

**Parameters:**

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `agent` | string | **Yes** | - | Agent name or ID |
| `include_metrics` | boolean | No | `false` | Include performance metrics |

**Returns:**

```json
{
  "id": "text-generator",
  "name": "Text Generator",
  "status": "running",
  "agent_type": "generation",
  "capabilities": ["text-generation", "summarization"],
  "metrics": {
    "tasks_completed": 42,
    "tasks_failed": 3,
    "uptime_seconds": 86400
  }
}
```

---

### workflow-start

Start a workflow from YAWL specification.

**Schema:**

```json
{
  "name": "workflow-start",
  "description": "Start a workflow from YAWL specification",
  "type": "object",
  "properties": {
    "spec": {
      "type": "string",
      "description": "YAWL specification content or file path"
    },
    "input": {
      "type": "object",
      "description": "Workflow input parameters"
    }
  },
  "required": ["spec"]
}
```

**Parameters:**

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `spec` | string | **Yes** | - | YAWL specification content or file path |
| `input` | object | No | `{}` | Workflow input parameters |

**Returns:**

```json
{
  "status": "created",
  "case_id": "case-uuid-456",
  "workflow_id": "data-pipeline"
}
```

---

## Agent Tool Schemas

### Agent Tool Template

Tools created by bridging agents follow this template:

```json
{
  "name": "agent-<agent-name>",
  "description": "Bridge for agent <agent-name>",
  "type": "object",
  "properties": {
    "command": {
      "type": "string",
      "description": "Command to execute",
      "enum": ["generate", "analyze", "status", "configure"]
    },
    "input": {
      "type": "string",
      "description": "Command input data"
    },
    "options": {
      "type": "object",
      "description": "Additional command options"
    }
  },
  "required": ["command"]
}
```

**Example: agent-text-generator**

```json
{
  "name": "agent-text-generator",
  "description": "Bridge for agent text-generator",
  "type": "object",
  "properties": {
    "command": {
      "type": "string",
      "description": "Command to execute"
    },
    "prompt": {
      "type": "string",
      "description": "Generation prompt"
    },
    "max_tokens": {
      "type": "integer",
      "description": "Maximum tokens to generate",
      "default": 1000
    }
  },
  "required": ["command"]
}
```

---

## Parameter Type Definitions

### String

```json
{
  "parameter_name": {
    "type": "string",
    "description": "Parameter description",
    "minLength": 1,
    "maxLength": 1000,
    "pattern": "^[a-zA-Z0-9_-]+$"
  }
}
```

### Integer

```json
{
  "parameter_name": {
    "type": "integer",
    "description": "Parameter description",
    "minimum": 0,
    "maximum": 100,
    "default": 10
  }
}
```

### Boolean

```json
{
  "parameter_name": {
    "type": "boolean",
    "description": "Parameter description",
    "default": false
  }
}
```

### Array

```json
{
  "parameter_name": {
    "type": "array",
    "description": "Parameter description",
    "items": {
      "type": "string"
    },
    "minItems": 1,
    "maxItems": 10,
    "uniqueItems": true
  }
}
```

### Object

```json
{
  "parameter_name": {
    "type": "object",
    "description": "Parameter description",
    "properties": {
      "key": {
        "type": "string"
      }
    },
    "additionalProperties": false
  }
}
```

### Enum

```json
{
  "parameter_name": {
    "type": "string",
    "description": "Parameter description",
    "enum": ["value1", "value2", "value3"]
  }
}
```

---

## Return Value Schemas

### Success Response

```json
{
  "success": true,
  "data": { /* Response data */ }
}
```

### Error Response

```json
{
  "success": false,
  "error": {
    "code": "ERROR_CODE",
    "message": "Human-readable error message",
    "details": { /* Additional error details */ }
  }
}
```

### Agent List Response

```json
{
  "agents": [
    {
      "id": "string",
      "name": "string",
      "agent_type": "string",
      "status": "running|idle|stopped|error",
      "url": "string (optional)",
      "capabilities": ["string"]
    }
  ]
}
```

### Agent Metrics Response

```json
{
  "metrics": {
    "tasks_completed": "integer",
    "tasks_failed": "integer",
    "messages_sent": "integer",
    "messages_received": "integer",
    "uptime_seconds": "integer",
    "memory_usage_mb": "integer",
    "cpu_usage_percent": "float"
  }
}
```

---

## Tool Categories

### Core Tools

Built-in tools for agent and workflow management.

| Tool | Description |
|------|-------------|
| `agent-list` | List all registered agents |
| `agent-start` | Start an agent |
| `agent-status` | Get agent status |
| `workflow-start` | Start a workflow |

### Agent Tools

Tools created by bridging agents.

| Tool | Description |
|------|-------------|
| `agent-<name>` | Interface for bridged agent |
| `agent-text-generator` | Text generation agent |
| `agent-code-analyzer` | Code analysis agent |

### Custom Tools

User-defined tools for specific functionality.

---

## Schema Validation

### Validation Rules

1. **Required Parameters**: All parameters in `required` array must be provided
2. **Type Checking**: Parameter values must match their defined type
3. **Constraints**: Values must satisfy `minimum`, `maximum`, `minLength`, `maxLength`
4. **Pattern Matching**: String values must match `pattern` regex
5. **Enum Values**: Values must be in `enum` array

### Example Validation

```javascript
// Valid request
{
  "name": "text-generator",
  "timeout": 60
}

// Invalid request (missing required parameter)
{
  "timeout": 60
}

// Invalid request (type mismatch)
{
  "name": "text-generator",
  "timeout": "sixty"  // Should be integer
}
```

---

## See Also

- [MCP Command Reference](../commands/mcp.md)
- [A2A Command Reference](../commands/a2a.md)
- [MCP Configuration Reference](../configuration/mcp-config.md)
- [MCP A2A Integration Guide](../MCP_A2A_INTEGRATION.md)
