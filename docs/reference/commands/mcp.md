<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [`ggen mcp` Command Reference](#ggen-mcp-command-reference)
  - [Command Overview](#command-overview)
  - [Configuration Commands](#configuration-commands)
    - [`ggen mcp config init`](#ggen-mcp-config-init)
    - [`ggen mcp config validate`](#ggen-mcp-config-validate)
    - [`ggen mcp config show`](#ggen-mcp-config-show)
  - [Server Commands](#server-commands)
    - [`ggen mcp server start`](#ggen-mcp-server-start)
    - [`ggen mcp server stop`](#ggen-mcp-server-stop)
    - [`ggen mcp server status`](#ggen-mcp-server-status)
  - [Tool Commands](#tool-commands)
    - [`ggen mcp list`](#ggen-mcp-list)
    - [`ggen mcp bridge`](#ggen-mcp-bridge)
    - [`ggen mcp status`](#ggen-mcp-status)
    - [`ggen mcp schemas`](#ggen-mcp-schemas)
    - [`ggen mcp test`](#ggen-mcp-test)
  - [Environment Variables](#environment-variables)
  - [Configuration Priority](#configuration-priority)
  - [Common Workflows](#common-workflows)
    - [Initial Setup](#initial-setup)
    - [Agent Bridging Workflow](#agent-bridging-workflow)
    - [Troubleshooting Workflow](#troubleshooting-workflow)
  - [See Also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# `ggen mcp` Command Reference

Complete reference for all MCP (Model Context Protocol) commands in ggen.

## Command Overview

```
ggen mcp [SUBCOMMAND]
```

The `mcp` command provides management capabilities for MCP servers, tools, and configurations. It follows the noun-verb pattern for intuitive command composition.

## Configuration Commands

### `ggen mcp config init`

Initialize a new MCP configuration file.

**Syntax:**
```bash
ggen mcp config init [OPTIONS]
```

**Options:**

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `--file <path>` | string | `.mcp.json` | Path to the configuration file |
| `--examples` | flag | `false` | Include example server configurations |
| `--a2a` | flag | `false` | Initialize A2A configuration instead of MCP |
| `--project` | flag | `false` | Force project-level configuration |

**Examples:**

```bash
# Initialize MCP config in default location (.mcp.json)
ggen mcp config init

# Initialize at custom path
ggen mcp config init --file /path/to/config.json

# Initialize with example server configurations
ggen mcp config init --examples

# Initialize A2A configuration
ggen mcp config init --a2a
```

**Output:**

```
Initializing MCP configuration at: /project/.mcp.json
MCP configuration initialized successfully
  Servers configured: 3
  Config version: 1.0.0

Example servers added:
  • claude-code-guide (npx @anthropic-ai/claude-code-guide)
  • git (npx git mcp-server)
  • bash (npx bash --init-file .claude/helpers/bash-init.sh)
```

**Exit Codes:**

- `0` - Configuration initialized successfully
- `1` - Failed to create configuration file (permissions, invalid path)

---

### `ggen mcp config validate`

Validate an existing MCP configuration file.

**Syntax:**
```bash
ggen mcp config validate [OPTIONS]
```

**Options:**

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `--file <path>` | string | `.mcp.json` | Path to the configuration file |
| `--a2a` | flag | `false` | Validate A2A configuration instead of MCP |
| `--verbose` | flag | `false` | Show detailed validation results |

**Examples:**

```bash
# Validate default config
ggen mcp config validate

# Validate specific file
ggen mcp config validate --file /path/to/config.json

# Validate A2A config
ggen mcp config validate --a2a

# Show detailed validation results
ggen mcp config validate --verbose
```

**Output:**

```
Validating MCP configuration: /project/.mcp.json

Validation Results:
  Config version: 1.0.0
  ✓ Server: claude-code-guide
  ✓ Server: git
  ✗ Server: bash
    Error: Command not found: bash
    Warning: Timeout is very low (5s)

Configuration has errors that need to be fixed.
```

**Validation Checks:**

- JSON syntax correctness
- Command existence and validity
- Timeout value constraints (must be > 0)
- Dangerous command detection
- Server dependency validation

---

### `ggen mcp config show`

Display the effective configuration with source information.

**Syntax:**
```bash
ggen mcp config show [OPTIONS]
```

**Options:**

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `--effective` | flag | `false` | Show effective configuration only |
| `--format <fmt>` | string | `table` | Output format: `table`, `json` |
| `--a2a` | flag | `false` | Show A2A configuration instead of MCP |

**Examples:**

```bash
# Show effective configuration with source information
ggen mcp config show

# Show effective configuration only
ggen mcp config show --effective

# Show as JSON
ggen mcp config show --format json

# Show A2A configuration
ggen mcp config show --a2a
```

**Output:**

```
MCP Configuration:
  Version: 1.0.0
  Servers: 3
  Description: MCP servers for enhanced Claude Code capabilities
  Source: Project configuration (.mcp.json)

Configured Servers:
  • claude-code-guide (enabled)
    Command: npx @anthropic-ai/claude-code-guide
    Environment: 2 variables
  • git (enabled)
    Command: npx git mcp-server
  • bash (disabled)
    Command: bash --init-file .claude/helpers/bash-init.sh

Configuration priority:
  CLI args > Env vars > Project > User > System > Defaults
```

---

## Server Commands

### `ggen mcp server start`

Start the MCP server.

**Syntax:**
```bash
ggen mcp server start [OPTIONS]
```

**Options:**

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `--config <path>` | string | `.mcp.json` | Path to configuration file |
| `--port <port>` | integer | `3000` | Server port number |
| `--daemon` | flag | `false` | Run in background (daemon mode) |
| `--verbose` | flag | `false` | Enable verbose logging |

**Examples:**

```bash
# Start server with default configuration
ggen mcp server start

# Start with specific config file
ggen mcp server start --config /path/to/config.json

# Start on specific port
ggen mcp server start --port 3000

# Start in background (daemon mode)
ggen mcp server start --daemon

# Start with verbose logging
ggen mcp server start --verbose
```

**Output:**

```
Starting MCP server...
  Config: /project/.mcp.json
  Port: 3000
  Daemon: true
MCP server started successfully
  PID: 12345
  Port: 3000
```

**Errors:**

- Server already running (shows existing PID and address)
- Configuration file not found
- Port already in use
- Insufficient permissions

---

### `ggen mcp server stop`

Stop a running MCP server.

**Syntax:**
```bash
ggen mcp server stop [OPTIONS]
```

**Options:**

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `--force` | flag | `false` | Force stop (SIGKILL instead of SIGTERM) |

**Examples:**

```bash
# Stop server gracefully
ggen mcp server stop

# Force stop
ggen mcp server stop --force
```

**Output:**

```
Stopping MCP server...
Stopped successfully
```

**Exit Codes:**

- `0` - Server stopped successfully
- `1` - Server was not running or stop failed

---

### `ggen mcp server status`

Show the current status of the MCP server.

**Syntax:**
```bash
ggen mcp server status [OPTIONS]
```

**Options:**

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `--verbose` | flag | `false` | Show detailed status information |
| `--format <fmt>` | string | `table` | Output format: `table`, `json` |

**Examples:**

```bash
# Show server status
ggen mcp server status

# Show detailed status
ggen mcp server status --verbose

# Show status as JSON
ggen mcp server status --format json
```

**Output (running):**

```
MCP Server Status: Running
  PID: Some(12345)
  Uptime: 2h 15m
  Address: 127.0.0.1:3000
  Started: 2024-01-15T10:30:00Z
```

**Output (stopped):**

```
MCP Server Status: Stopped

Start the server with:
  ggen mcp server start
```

---

## Tool Commands

### `ggen mcp list`

List all available MCP tools.

**Syntax:**
```bash
ggen mcp list [OPTIONS]
```

**Options:**

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `--verbose` | flag | `false` | Show detailed tool information |

**Examples:**

```bash
# List all MCP tools
ggen mcp list

# List tools with detailed schema
ggen mcp list --verbose
```

**Output:**

```
Listing 6 MCP tools (4 core, 2 agent)

  • agent-list (core)
  • agent-start (core)
  • agent-status (core)
  • workflow-start (core)
  • agent-text-generator (agent)
  • agent-code-analyzer (agent)

Total: 6 agents (2 running)
```

**Verbose Output:**

```
  • agent-list (core)
    List all registered agents

  • agent-start (core)
    Start an agent
    Bridged agent: text-generator

  • workflow-start (core)
    Start a workflow from YAWL specification
```

---

### `ggen mcp bridge`

Bridge an agent as an MCP tool.

**Syntax:**
```bash
ggen mcp bridge <AGENT> [OPTIONS]
```

**Arguments:**

| Argument | Type | Description |
|----------|------|-------------|
| `<AGENT>` | string | Name of the agent to bridge |

**Options:**

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `--tool-name <name>` | string | `agent-<AGENT>` | Custom tool name |

**Examples:**

```bash
# Bridge an agent as MCP tool
ggen mcp bridge "text-generator"

# Bridge agent with custom tool name
ggen mcp bridge "text-generator" --tool-name "ai-assistant"

# Bridge and immediately test
ggen mcp bridge "text-generator" && ggen mcp test "agent-text-generator"
```

**Output:**

```
Bridging agent 'text-generator' as MCP tool 'agent-text-generator'
Agent text-generator successfully bridged as MCP tool
Tool: agent-text-generator
Description: Bridge for agent text-generator
```

---

### `ggen mcp status`

Show detailed status for a specific MCP tool.

**Syntax:**
```bash
ggen mcp status <TOOL> [OPTIONS]
```

**Arguments:**

| Argument | Type | Description |
|----------|------|-------------|
| `<TOOL>` | string | Name of the MCP tool |

**Options:**

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `--schema` | flag | `false` | Show full tool schema |

**Examples:**

```bash
# Show tool status
ggen mcp status "agent-list"

# Show tool with full schema
ggen mcp status "agent-start" --schema
```

**Output:**

```
Checking status for MCP tool: agent-list
Tool: agent-list
Type: core
Status: available
```

**With Schema:**

```
Checking status for MCP tool: agent-start
Tool: agent-start
Type: core
Status: available
Schema:
{
  "type": "object",
  "description": "Start an agent",
  "properties": {
    "name": {
      "type": "string",
      "description": "Agent name"
    }
  },
  "required": ["name"]
}
```

---

### `ggen mcp schemas`

List all MCP tool schemas.

**Syntax:**
```bash
ggen mcp schemas [OPTIONS]
```

**Options:**

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `--detailed` | flag | `false` | Show detailed schema information |

**Examples:**

```bash
# List schemas
ggen mcp schemas

# List schemas with detailed output
ggen mcp schemas --detailed
```

**Output:**

```
Listing 6 MCP tool schemas

  • agent-list
  • agent-start
  • agent-status
  • workflow-start
  • agent-text-generator
  • agent-code-analyzer
```

**Detailed Output:**

```
  • agent-list:
    Description: List all registered agents
    Type: object
    Properties: 0

  • agent-start:
    Description: Start an agent
    Type: object
    Properties: 1
```

---

### `ggen mcp test`

Test MCP tool functionality.

**Syntax:**
```bash
ggen mcp test <TOOL> [OPTIONS]
```

**Arguments:**

| Argument | Type | Description |
|----------|------|-------------|
| `<TOOL>` | string | Name of the MCP tool to test |

**Options:**

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `--args <json>` | string | `{}` | JSON arguments for the tool |

**Examples:**

```bash
# Test a tool
ggen mcp test "agent-list"

# Test with custom arguments
ggen mcp test "agent-start" --args '{"name": "my-agent"}'

# Test workflow tool
ggen mcp test "workflow-start" --args '{"spec": "workflow.yawl"}'
```

**Output:**

```
Testing MCP tool: agent-list
Arguments: {}
Simulating agent list...
Test completed successfully
Result:
{
  "agents": [
    {"name": "test-agent", "status": "ready"},
    {"name": "workflow-agent", "status": "busy"}
  ]
}
```

---

## Environment Variables

The following environment variables can override MCP configuration:

| Variable | Description | Example |
|----------|-------------|---------|
| `GGEN_MCP_CONFIG` | Path to MCP configuration file | `/path/to/config.json` |
| `GGEN_MCP_SERVER_<name>_COMMAND` | Command for a named server | `npx` |
| `GGEN_MCP_SERVER_<name>_ARGS` | Arguments for a named server | `@anthropic-ai/claude-code-guide` |
| `GGEN_MCP_SERVER_<name>_TIMEOUT` | Timeout in seconds | `30` |

**Examples:**

```bash
# Set configuration file path
export GGEN_MCP_CONFIG="/custom/path/config.json"

# Configure a server via environment
export GGEN_MCP_SERVER_MY_SERVER_COMMAND="npx"
export GGEN_MCP_SERVER_MY_SERVER_ARGS="@anthropic-ai/claude-code-guide"
export GGEN_MCP_SERVER_MY_SERVER_TIMEOUT="60"
```

---

## Configuration Priority

Configuration is resolved in the following order (highest to lowest priority):

1. **CLI arguments** - `--config` flag
2. **Environment variables** - `GGEN_MCP_*` variables
3. **Project configuration** - `.mcp.json` in current directory
4. **User configuration** - `~/.ggen/mcp/config.json`
5. **System configuration** - `/etc/ggen/mcp.json`
6. **Default values** - Built-in defaults

---

## Common Workflows

### Initial Setup

```bash
# Initialize configuration with examples
ggen mcp config init --examples

# Validate the configuration
ggen mcp config validate

# Start the server
ggen mcp server start --daemon

# Check server status
ggen mcp server status
```

### Agent Bridging Workflow

```bash
# List available agents
ggen a2a list

# Bridge an agent as MCP tool
ggen mcp bridge "text-generator" --tool-name "ai-assistant"

# Verify the tool is available
ggen mcp list --verbose

# Test the bridged tool
ggen mcp test "agent-text-generator" --args '{"command": "generate"}'
```

### Troubleshooting Workflow

```bash
# Validate configuration
ggen mcp config validate --verbose

# Check server status
ggen mcp server status --verbose

# List available tools
ggen mcp list --verbose

# Test a specific tool
ggen mcp test "agent-list"
```

---

## See Also

- [MCP Configuration Reference](../configuration/mcp-config.md)
- [A2A Command Reference](./a2a.md)
- [MCP A2A Integration Guide](../MCP_A2A_INTEGRATION.md)
