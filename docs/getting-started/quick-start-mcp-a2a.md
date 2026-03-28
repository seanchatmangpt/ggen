<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [MCP & A2A Quick Start Tutorial](#mcp--a2a-quick-start-tutorial)
  - [What You'll Learn](#what-youll-learn)
  - [Prerequisites](#prerequisites)
  - [Step 1: Initialize MCP Configuration (1 minute)](#step-1-initialize-mcp-configuration-1-minute)
  - [Step 2: Discover Available Tools (1 minute)](#step-2-discover-available-tools-1-minute)
  - [Step 3: Start an A2A Agent (2 minutes)](#step-3-start-an-a2a-agent-2-minutes)
  - [Step 4: Bridge Agent to MCP (2 minutes)](#step-4-bridge-agent-to-mcp-2-minutes)
  - [Step 5: Execute MCP Tool (3 minutes)](#step-5-execute-mcp-tool-3-minutes)
  - [Success Criteria](#success-criteria)
  - [Troubleshooting](#troubleshooting)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# MCP & A2A Quick Start Tutorial

**Goal**: Connect ggen agents to external MCP servers and execute tools in 10 minutes.

**Time**: 10 minutes

---

## What You'll Learn

- How to configure MCP and A2A settings
- How to discover available MCP tools
- How to start and manage A2A agents
- How to bridge agents as MCP tools
- How to execute tools and handle responses

---

## Prerequisites

Before starting, ensure you have:

- **ggen installed** (version 3.4.1 or newer)
  ```bash
  ggen --version
  ```

- **Basic terminal knowledge** - navigating directories, running commands

- **Text editor** (VS Code, vim, nano, etc.) - for viewing generated files

---

## Step 1: Initialize MCP Configuration (1 minute)

Initialize the MCP configuration file in your project:

```bash
# Create a new project directory
mkdir mcp-quickstart
cd mcp-quickstart

# Initialize MCP configuration
ggen mcp config init
```

**Expected output**:
```
Initializing MCP configuration at: .mcp.json
MCP configuration initialized successfully
  Servers configured: 0
  Config version: 1.0
```

**What was created**: A `.mcp.json` configuration file in your project directory with default settings.

**View your configuration**:
```bash
ggen mcp config show
```

---

## Step 2: Discover Available Tools (1 minute)

List all available MCP tools in the system:

```bash
ggen mcp list
```

**Expected output**:
```
Listing 4 MCP tools (3 core, 1 agent)
  • agent-list (core)
  • agent-start (core)
  • agent-status (core)
  • workflow-start (core)
```

**Get detailed tool information**:
```bash
ggen mcp list --verbose
```

**View tool schemas**:
```bash
ggen mcp schemas --detailed
```

**What you learned**: The MCP system provides core tools for agent management and workflow control.

---

## Step 3: Start an A2A Agent (2 minutes)

Start a text-generator agent using the A2A protocol:

```bash
# Initialize A2A configuration
ggen mcp config init --a2a

# View A2A configuration
ggen mcp config show --a2a
```

**Expected A2A configuration output**:
```
A2A Configuration:
  Host: localhost
  Port: 8080
  URL: http://localhost:8080
  TLS: disabled
  Timeout: 30s
  Max connections: 100
```

**Start a simple agent**:
```bash
# Start a text-generator agent
ggen agent start "text-generator"
```

**Check agent status**:
```bash
ggen agent status "text-generator" --metrics
```

**Expected output**:
```
Agent: text-generator
Status: running
Capabilities: text-generation
Started: 2025-01-15 10:30:00
Tasks Completed: 0
```

---

## Step 4: Bridge Agent to MCP (2 minutes)

Bridge your running agent as an MCP tool:

```bash
# Bridge the text-generator agent
ggen mcp bridge "text-generator"
```

**Expected output**:
```
Bridging agent 'text-generator' as MCP tool 'agent-text-generator'
Agent text-generator successfully bridged as MCP tool
Tool: agent-text-generator
Description: Bridge for agent text-generator
```

**Verify the bridge**:
```bash
# Check tool status
ggen mcp status "agent-text-generator" --schema
```

**List all tools again**:
```bash
ggen mcp list --verbose
```

You should now see your bridged agent listed:
```
  • agent-text-generator (agent)
    Bridge for agent text-generator
    Bridged agent: text-generator
```

---

## Step 5: Execute MCP Tool (3 minutes)

Test your bridged agent tool with MCP protocol:

```bash
# Test the tool with default parameters
ggen mcp test "agent-text-generator"
```

**Expected output**:
```
Testing MCP tool: agent-text-generator
Arguments: {}
Simulating agent list...
Test completed successfully
Result: {
  "status": "success",
  "response": "Hello from text-generator!"
}
```

**Test with custom arguments**:
```bash
# Test with specific parameters
ggen mcp test "agent-text-generator" --args '{"prompt": "Generate a haiku about coding"}'
```

**Expected output**:
```
Testing MCP tool: agent-text-generator
Arguments: {"prompt":"Generate a haiku about coding"}
Simulating agent start for text-generator...
Test completed successfully
Result: {
  "status": "success",
  "generation": "Code flows like water,\nBugs emerge from hidden depths,\nDebug brings the light."
}
```

**Test core MCP tools**:
```bash
# Test agent-list tool
ggen mcp test "agent-list"

# Test workflow-start tool
ggen mcp test "workflow-start" --args '{"spec": "test-workflow"}'
```

---

## Success Criteria

You've completed the tutorial if you:

- [ ] Created `.mcp.json` configuration file
- [ ] Created `.a2a.toml` configuration file
- [ ] Listed available MCP tools
- [ ] Started an A2A agent
- [ ] Bridged the agent as an MCP tool
- [ ] Successfully executed a tool call
- [ ] Received valid responses from the MCP system

---

## Troubleshooting

| Problem | Solution |
|---------|----------|
| `Configuration file not found` | Run `ggen mcp config init` to create default configuration |
| `Agent not found` | Verify agent name with `ggen agent list` |
| `Tool not found` | Check available tools with `ggen mcp list` |
| `Connection refused` | Ensure MCP server is running: `ggen mcp server start` |
| `Authentication failed` | Check API key in configuration: `ggen mcp config show` |
| `Bridge failed` | Start the agent first: `ggen agent start "<agent-name>"` |

**Enable debug mode** for detailed error information:
```bash
# Set environment variable for verbose logging
export RUST_LOG=debug

# Run command with debug output
ggen mcp list --debug
```

**Check server status**:
```bash
ggen mcp server status --verbose
```

**Validate configuration**:
```bash
ggen mcp config validate --verbose
```

---

## Next Steps

**Continue Learning**:

1. **[Your First MCP Tool](../tutorials/core/01-first-mcp-tool.md)** - Deep dive into tool creation
2. **[MCP & A2A Integration Guide](../MCP_A2A_INTEGRATION.md)** - Complete integration documentation
3. **[Agent Workflow Bridge](../../examples/agent_workflow_bridge.rs)** - Production examples
4. **[Configuration Reference](../reference/cli.md#mcp-commands)** - All CLI options

**Advanced Topics**:

- **[Transport Protocols](../MCP_A2A_INTEGRATION.md#transport-protocols)** - HTTP, WebSocket, QUIC
- **[Authentication & Security](../MCP_A2A_INTEGRATION.md#authentication-and-security)** - JWT, API keys, mTLS
- **[Performance Optimization](../how-to-guides/deploy-production.md)** - Connection pooling, message batching

**Join the community**:
- [GitHub Discussions](https://github.com/seanchatmangpt/ggen/discussions)
- [Examples Repository](https://github.com/seanchatmangpt/ggen/tree/master/examples)
