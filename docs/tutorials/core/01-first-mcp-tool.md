<!-- START doctoc generated TOC please keep comment here allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Your First MCP Tool](#your-first-mcp-tool)
  - [What You'll Build](#what-youll-build)
  - [Prerequisites](#prerequisites)
  - [Understanding MCP Tools](#understanding-mcp-tools)
  - [Step 1: Tool Discovery Workflow](#step-1-tool-discovery-workflow)
  - [Step 2: Core Tools Overview](#step-2-core-tools-overview)
  - [Step 3: Creating a Custom Tool](#step-3-creating-a-custom-tool)
  - [Step 4: Agent Bridging Example](#step-4-agent-bridging-example)
  - [Step 5: Tool Execution Demonstration](#step-5-tool-execution-demonstration)
  - [Step 6: Advanced Tool Configuration](#step-6-advanced-tool-configuration)
  - [Complete Example](#complete-example)
  - [Testing Your Tool](#testing-your-tool)
  - [Common Patterns](#common-patterns)
  - [Troubleshooting](#troubleshooting)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC -->

# Your First MCP Tool

**Goal**: Create, register, and execute a custom MCP tool with ggen agents.

**Time**: 15 minutes

---

## What You'll Build

You'll create a custom `file-analyzer` MCP tool that:
- Accepts file paths as input
- Analyzes file contents using A2A agents
- Returns structured analysis results
- Handles errors gracefully

**You'll learn**:
- How MCP tools are structured
- How to discover and use existing tools
- How to bridge agents as tools
- How to execute tools and handle responses

---

## Prerequisites

- Completed the [MCP & A2A Quick Start](../../getting-started/quick-start-mcp-a2a.md)
- ggen version 3.4.1 or newer
- Basic understanding of JSON
- Text editor for code editing

---

## Understanding MCP Tools

MCP (Model Context Protocol) tools are structured functions that agents can call. Each tool has:

```json
{
  "name": "tool-name",
  "description": "What the tool does",
  "inputSchema": {
    "type": "object",
    "properties": {
      "parameter": {
        "type": "string",
        "description": "Parameter description"
      }
    },
    "required": ["parameter"]
  }
}
```

**Tool Types**:
- **Core tools**: Built-in system tools (agent-list, agent-start, etc.)
- **Agent tools**: Bridged A2A agents exposed as tools
- **Custom tools**: User-defined tools for specific tasks

---

## Step 1: Tool Discovery Workflow

Discover all available tools in the system:

```bash
# List all tools
ggen mcp list

# List with detailed information
ggen mcp list --verbose
```

**Understanding the output**:
```
Listing 5 MCP tools (3 core, 2 agent)

  • agent-list (core)
    List all registered agents

  • agent-start (core)
    Start an agent
    Properties: name (string, required)

  • agent-status (core)
    Show agent status
    Properties: agent (string, required)

  • agent-text-generator (agent)
    Bridge for agent text-generator
    Bridged agent: text-generator
```

**Get tool schema**:
```bash
# Get schema for a specific tool
ggen mcp status "agent-start" --schema
```

**Expected output**:
```json
{
  "type": "object",
  "description": "Start an agent",
  "properties": {
    "name": {
      "type": "string",
      "description": "Agent name"
    },
    "capabilities": {
      "type": "array",
      "description": "Agent capabilities",
      "items": {
        "type": "string"
      }
    }
  },
  "required": ["name"]
}
```

---

## Step 2: Core Tools Overview

ggen provides several core MCP tools out of the box:

| Tool | Description | Parameters |
|------|-------------|------------|
| `agent-list` | List all registered agents | None |
| `agent-start` | Start an agent | `name` (required), `capabilities` (optional) |
| `agent-status` | Show agent status | `agent` (required) |
| `workflow-start` | Start a YAWL workflow | `spec` (required) |

**Test core tools**:
```bash
# List agents
ggen mcp test "agent-list"

# Start an agent
ggen mcp test "agent-start" --args '{"name": "test-agent"}'

# Check workflow tool
ggen mcp status "workflow-start" --schema
```

---

## Step 3: Creating a Custom Tool

Create a `file-analyzer` tool configuration:

```bash
# Create tool configuration directory
mkdir -p .ggen/tools
cd .ggen/tools

# Create file-analyzer tool definition
cat > file-analyzer.json << 'EOF'
{
  "name": "file-analyzer",
  "version": "1.0.0",
  "description": "Analyze file contents and extract metadata",
  "type": "custom",
  "handler": "a2a",
  "agent": "file-analyzer-agent",
  "inputSchema": {
    "type": "object",
    "description": "File analysis request",
    "properties": {
      "filePath": {
        "type": "string",
        "description": "Absolute path to the file to analyze"
      },
      "analysisType": {
        "type": "string",
        "enum": ["basic", "detailed", "security"],
        "description": "Type of analysis to perform"
      }
    },
    "required": ["filePath"]
  }
}
EOF
```

**What you created**:
- Tool definition with input validation
- Schema for parameter checking
- Agent handler mapping
- Multiple analysis types

**Register the tool**:
```bash
# Copy to project configuration
cp file-analyzer.json ../../.ggen/tools/

# Verify registration
ggen mcp list --verbose | grep file-analyzer
```

---

## Step 4: Agent Bridging Example

Bridge an existing agent as an MCP tool:

```bash
# Start a code-analyzer agent
ggen agent start "code-analyzer" --capabilities "code-analysis,syntax-check"

# Bridge the agent
ggen mcp bridge "code-analyzer" --tool-name "code-analyzer-tool"
```

**Expected output**:
```
Bridging agent 'code-analyzer' as MCP tool 'code-analyzer-tool'
Agent code-analyzer successfully bridged as MCP tool
Tool: code-analyzer-tool
Description: Bridge for agent code-analyzer
```

**Verify the bridge**:
```bash
# Check tool status
ggen mcp status "code-analyzer-tool" --schema
```

**Expected schema**:
```json
{
  "type": "object",
  "description": "Agent tool for code-analyzer-tool",
  "properties": {
    "command": {
      "type": "string",
      "description": "Command to execute"
    },
    "source": {
      "type": "string",
      "description": "Source code to analyze"
    }
  },
  "required": ["command"]
}
```

---

## Step 5: Tool Execution Demonstration

Execute your bridged tool with different parameters:

```bash
# Test with basic command
ggen mcp test "code-analyzer-tool" --args '{"command": "analyze"}'

# Test with source code
ggen mcp test "code-analyzer-tool" --args '{
  "command": "analyze",
  "source": "fn main() { println!(\"Hello\"); }"
}'
```

**Expected response**:
```json
{
  "status": "success",
  "analysis": {
    "language": "rust",
    "lines": 1,
    "functions": ["main"],
    "imports": [],
    "complexity": "low"
  }
}
```

**Execute with error handling**:
```bash
# Test with invalid input
ggen mcp test "code-analyzer-tool" --args '{"command": "invalid"}'
```

**Expected error response**:
```json
{
  "status": "error",
  "error": "Unknown command: invalid",
  "validCommands": ["analyze", "check", "format"]
}
```

---

## Step 6: Advanced Tool Configuration

Configure advanced tool options:

```bash
# Update MCP config with advanced settings
cat > .mcp.json << 'EOF'
{
  "version": "1.0.0",
  "description": "Advanced MCP configuration",
  "timeout_ms": 15000,
  "max_retries": 3,
  "mcp_servers": {
    "file-analyzer": {
      "command": "ggen",
      "args": ["agent", "start", "file-analyzer"],
      "enabled": true,
      "env": {
        "ANALYSIS_TIMEOUT": "30",
        "MAX_FILE_SIZE": "10485760"
      }
    },
    "code-analyzer": {
      "command": "ggen",
      "args": ["agent", "start", "code-analyzer"],
      "enabled": true,
      "env": {
        "RUST_BACKTRACE": "1"
      }
    }
  },
  "tools": {
    "file-analyzer": {
      "rate_limit": 100,
      "cache_ttl": 300
    }
  }
}
EOF
```

**Validate configuration**:
```bash
ggen mcp config validate --verbose
```

---

## Complete Example

Here's a complete workflow using all concepts:

```bash
# 1. Initialize configuration
ggen mcp config init
ggen mcp config init --a2a

# 2. Discover available tools
ggen mcp list --verbose

# 3. Start an agent
ggen agent start "text-processor" --capabilities "text-analysis,summarization"

# 4. Bridge the agent
ggen mcp bridge "text-processor" --tool-name "text-tool"

# 5. Test the tool
ggen mcp test "text-tool" --args '{
  "command": "summarize",
  "text": "Long text to summarize...",
  "max_length": 100
}'

# 6. Check tool schema
ggen mcp status "text-tool" --schema

# 7. List all schemas
ggen mcp schemas --detailed
```

---

## Testing Your Tool

Create a test script for your custom tool:

```bash
cat > test-tool.sh << 'EOF'
#!/bin/bash

echo "Testing MCP tool: file-analyzer"

# Test 1: Missing required parameter
echo "Test 1: Missing required parameter"
ggen mcp test "file-analyzer" --args '{}'
echo ""

# Test 2: Valid basic analysis
echo "Test 2: Valid basic analysis"
ggen mcp test "file-analyzer" --args '{
  "filePath": "/tmp/test.txt",
  "analysisType": "basic"
}'
echo ""

# Test 3: Invalid analysis type
echo "Test 3: Invalid analysis type"
ggen mcp test "file-analyzer" --args '{
  "filePath": "/tmp/test.txt",
  "analysisType": "invalid"
}'
echo ""

echo "Tests completed"
EOF

chmod +x test-tool.sh
./test-tool.sh
```

---

## Common Patterns

**Pattern 1: Chaining Tools**
```bash
# Execute multiple tools in sequence
ggen mcp test "file-analyzer" --args '{"filePath": "data.json"}' | \
  jq '.output' | \
  xargs -I {} ggen mcp test "text-processor" --args "{\"text\": {\"}\", \"action\": \"summarize\"}"
```

**Pattern 2: Batch Processing**
```bash
# Process multiple files
for file in data/*.json; do
  ggen mcp test "file-analyzer" --args "{\"filePath\": \"$file\"}"
done
```

**Pattern 3: Conditional Execution**
```bash
# Check tool availability before execution
if ggen mcp list | grep -q "file-analyzer"; then
  ggen mcp test "file-analyzer" --args '{"filePath": "data.json"}'
else
  echo "Tool not available"
fi
```

---

## Troubleshooting

| Issue | Solution |
|-------|----------|
| Tool not found after creation | Run `ggen mcp config validate` and restart MCP server |
| Schema validation errors | Verify JSON syntax with `jq` or online validator |
| Agent not responding | Check agent status: `ggen agent status "<name>"` |
| Bridge failures | Ensure agent is running before bridging |
| Timeout errors | Increase timeout in `.mcp.json` configuration |

**Debug tool execution**:
```bash
# Enable verbose logging
export RUST_LOG=debug

# Run with debug output
ggen mcp test "file-analyzer" --args '{"filePath": "test.txt"}'
```

**Check MCP server status**:
```bash
ggen mcp server status --verbose
```

---

## Next Steps

**Continue Your Journey**:

1. **[MCP & A2A Integration Guide](../../MCP_A2A_INTEGRATION.md)** - Complete integration documentation
2. **[Agent Workflow Examples](../../examples/agent_workflow_bridge.rs)** - Production patterns
3. **[Configuration Reference](../../reference/cli.md)** - All CLI options
4. **[Production Deployment](../../how-to-guides/deploy-production.md)** - Scaling and optimization

**Advanced Topics**:
- **[Transport Protocols](../../MCP_A2A_INTEGRATION.md#transport-protocols)** - HTTP, WebSocket, QUIC
- **[Security Best Practices](../../MCP_A2A_INTEGRATION.md#authentication-and-security)** - Authentication patterns
- **[Performance Optimization](../../how-to-guides/deploy-production.md)** - Connection pooling

**Join the community**:
- [GitHub Discussions](https://github.com/seanchatmangpt/ggen/discussions)
- [Examples Repository](https://github.com/seanchatmangpt/ggen/tree/master/examples)
