<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [`ggen a2a` Command Reference](#ggen-a2a-command-reference)
  - [Command Overview](#command-overview)
  - [Agent Commands](#agent-commands)
    - [`ggen a2a list`](#ggen-a2a-list)
    - [`ggen a2a start`](#ggen-a2a-start)
    - [`ggen a2a stop`](#ggen-a2a-stop)
    - [`ggen a2a status`](#ggen-a2a-status)
  - [Message Commands](#message-commands)
    - [`ggen a2a message send`](#ggen-a2a-message-send)
    - [`ggen a2a message list`](#ggen-a2a-message-list)
  - [Configuration](#configuration)
    - [Configuration Files](#configuration-files)
    - [Environment Variables](#environment-variables)
  - [Common Workflows](#common-workflows)
    - [Agent Lifecycle Management](#agent-lifecycle-management)
    - [Multi-Agent Coordination](#multi-agent-coordination)
    - [Troubleshooting](#troubleshooting)
    - [MCP Integration](#mcp-integration)
  - [Error Handling](#error-handling)
    - [Common Exit Codes](#common-exit-codes)
    - [Error Messages](#error-messages)
  - [Type Definitions](#type-definitions)
    - [AgentStatus](#agentstatus)
    - [MessageDeliveryStatus](#messagedeliverystatus)
    - [MessageDirection](#messagedirection)
    - [AgentMetrics](#agentmetrics)
  - [See Also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# `ggen a2a` Command Reference

Complete reference for all A2A (Agent-to-Agent) protocol commands in ggen.

## Command Overview

```
ggen a2a [SUBCOMMAND]
```

The `a2a` command provides management capabilities for A2A agents, tasks, and messaging. It enables communication between ggen agents using the Agent-to-Agent protocol.

## Agent Commands

### `ggen a2a list`

List all registered A2A agents.

**Syntax:**
```bash
ggen a2a list [OPTIONS]
```

**Options:**

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `--verbose` | flag | `false` | Show detailed agent information |

**Examples:**

```bash
# List all agents
ggen a2a list

# List with detailed information
ggen a2a list --verbose
```

**Output:**

```
A2A Agents
Server: http://127.0.0.1:8080

ID              Name                Type        Status
------------------------------------------------------------
text-generator  Text Generator      generation  Running
code-analyzer   Code Analyzer       analysis    Idle
workflow-agent  Workflow Agent      workflow    Stopped

Total: 3 agents (1 running)
```

**Verbose Output:**

```
A2A Agents
Server: http://127.0.0.1:8080

Text Generator
  ID: text-generator
  Type: generation
  Status: Running
  URL: http://127.0.0.1:8080/agents/text-generator
  Capabilities: text-generation, summarization

Code Analyzer
  ID: code-analyzer
  Type: analysis
  Status: Idle
  URL: http://127.0.0.1:8080/agents/code-analyzer
  Capabilities: code-review, security-scan
```

**Agent Status Values:**

| Status | Description |
|--------|-------------|
| `Running` | Agent is actively processing tasks |
| `Idle` | Agent is running but not processing |
| `Stopped` | Agent is not running |
| `Error` | Agent encountered an error |

---

### `ggen a2a start`

Start an A2A agent.

**Syntax:**
```bash
ggen a2a start <AGENT> [OPTIONS]
```

**Arguments:**

| Argument | Type | Description |
|----------|------|-------------|
| `<AGENT>` | string | Name of the agent to start |

**Options:**

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `--timeout <sec>` | integer | `30` | Startup timeout in seconds |

**Examples:**

```bash
# Start an agent
ggen a2a start text-generator

# Start with custom timeout
ggen a2a start text-generator --timeout 60

# Start workflow agent
ggen a2a start workflow-agent --timeout 120
```

**Output:**

```
Starting agent 'text-generator'
Timeout: 30s

✓ Agent 'text-generator' started successfully (timeout: 30s)
```

**Errors:**

- Agent not found (lists available agents)
- Agent already running (informational message)
- Timeout exceeded
- Insufficient permissions

---

### `ggen a2a stop`

Stop a running A2A agent.

**Syntax:**
```bash
ggen a2a stop <AGENT> [OPTIONS]
```

**Arguments:**

| Argument | Type | Description |
|----------|------|-------------|
| `<AGENT>` | string | Name of the agent to stop |

**Options:**

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `--timeout <sec>` | integer | `30` | Shutdown timeout in seconds |

**Examples:**

```bash
# Stop an agent
ggen a2a stop text-generator

# Stop with custom timeout
ggen a2a stop text-generator --timeout 60
```

**Output:**

```
Stopping agent 'text-generator'
Timeout: 30s

✓ Agent 'text-generator' stopped successfully (timeout: 30s)
```

---

### `ggen a2a status`

Get detailed status for a specific agent.

**Syntax:**
```bash
ggen a2a status <AGENT> [OPTIONS]
```

**Arguments:**

| Argument | Type | Description |
|----------|------|-------------|
| `<AGENT>` | string | Name of the agent |

**Options:**

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `--metrics` | flag | `false` | Include performance metrics |

**Examples:**

```bash
# Get basic status
ggen a2a status text-generator

# Get status with metrics
ggen a2a status text-generator --metrics
```

**Output:**

```
Agent Status
  ID: text-generator
  Name: Text Generator
  Type: generation
  Status: Running
  URL: http://127.0.0.1:8080/agents/text-generator
  Capabilities: text-generation, summarization
```

**With Metrics:**

```
Agent Status
  ID: text-generator
  Name: Text Generator
  Type: generation
  Status: Running
  URL: http://127.0.0.1:8080/agents/text-generator
  Capabilities: text-generation, summarization

Metrics
  Tasks Completed: 42
  Tasks Failed: 3
  Messages Sent: 156
  Messages Received: 148
  Uptime: 24h 0m
  Memory: 128 MB
  CPU: 5.2%
```

**Metric Definitions:**

| Metric | Type | Description |
|--------|------|-------------|
| `tasks_completed` | integer | Number of successfully completed tasks |
| `tasks_failed` | integer | Number of failed tasks |
| `messages_sent` | integer | Number of messages sent by the agent |
| `messages_received` | integer | Number of messages received |
| `uptime_seconds` | integer | Agent uptime in seconds |
| `memory_usage_mb` | integer | Memory consumption in MB |
| `cpu_usage_percent` | float | CPU utilization percentage |

---

## Message Commands

### `ggen a2a message send`

Send a message to an A2A agent.

**Syntax:**
```bash
ggen a2a message send <AGENT> <MESSAGE>
```

**Arguments:**

| Argument | Type | Description |
|----------|------|-------------|
| `<AGENT>` | string | Name of the target agent |
| `<MESSAGE>` | string | Message content to send |

**Examples:**

```bash
# Send a simple message
ggen a2a message send text-generator "Hello, agent!"

# Send a command message
ggen a2a message send workflow-agent "start workflow data-pipeline"

# Send JSON payload
ggen a2a message send code-analyzer '{"file": "src/main.rs", "action": "review"}'
```

**Output:**

```
Sending message to agent 'text-generator'
Message: Hello, agent!

✓ Message sent successfully
  Message ID: 550e8400-e29b-41d4-a716-446655440000
  Status: Sent
  Timestamp: 2024-01-15T10:30:00Z
```

**Message Delivery Status:**

| Status | Description |
|--------|-------------|
| `Pending` | Message is queued for delivery |
| `Sent` | Message has been sent |
| `Delivered` | Message has been delivered |
| `Failed` | Message delivery failed |

---

### `ggen a2a message list`

List message history for an agent.

**Syntax:**
```bash
ggen a2a message list <AGENT> [OPTIONS]
```

**Arguments:**

| Argument | Type | Description |
|----------|------|-------------|
| `<AGENT>` | string | Name of the agent |

**Options:**

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `--limit <n>` | integer | `20` | Maximum number of messages to return |

**Examples:**

```bash
# List recent messages
ggen a2a message list text-generator

# List with custom limit
ggen a2a message list text-generator --limit 50

# List all messages (use large limit)
ggen a2a message list text-generator --limit 1000
```

**Output:**

```
Message History for 'text-generator'
Showing 2 most recent messages

ID              Content                                             Direction  Status         Time
----------------------------------------------------------------------------------------------------
msg-001         Hello, agent!                                      → Out      Sent           2h ago
msg-002         Processing your request...                         ← In       Delivered      2h ago
```

**Message Direction Values:**

| Direction | Symbol | Description |
|-----------|--------|-------------|
| `Outgoing` | `→ Out` | Message sent to the agent |
| `Incoming` | `← In` | Message received from the agent |

---

## Configuration

### Configuration Files

A2A commands use configuration from the following locations (in priority order):

1. **Project-level** - `a2a.toml` in current directory
2. **User-level** - `~/.ggen/a2a.toml`
3. **System-level** - `/etc/ggen/a2a.toml`
4. **Environment variables** - `GGEN_A2A_*` variables
5. **Default values** - Built-in defaults

See [A2A Configuration Reference](../configuration/a2a-config.md) for detailed schema.

### Environment Variables

| Variable | Description | Example |
|----------|-------------|---------|
| `GGEN_A2A_CONFIG` | Path to A2A configuration file | `/path/to/a2a.toml` |
| `GGEN_A2A_HOST` | A2A server host | `127.0.0.1` |
| `GGEN_A2A_PORT` | A2A server port | `8080` |
| `GGEN_A2A_TIMEOUT` | Request timeout in seconds | `30` |
| `GGEN_A2A_TLS_ENABLED` | Enable TLS | `1` or `true` |
| `GGEN_A2A_TLS_CERT` | Path to TLS certificate | `/path/to/cert.pem` |
| `GGEN_A2A_TLS_KEY` | Path to TLS key | `/path/to/key.pem` |

**Examples:**

```bash
# Set server URL
export GGEN_A2A_HOST="192.168.1.100"
export GGEN_A2A_PORT="9000"

# Enable TLS
export GGEN_A2A_TLS_ENABLED="true"
export GGEN_A2A_TLS_CERT="/etc/ssl/certs/a2a-cert.pem"
export GGEN_A2A_TLS_KEY="/etc/ssl/private/a2a-key.pem"

# Set timeout
export GGEN_A2A_TIMEOUT="60"
```

---

## Common Workflows

### Agent Lifecycle Management

```bash
# List available agents
ggen a2a list --verbose

# Start an agent
ggen a2a start text-generator --timeout 60

# Check agent status with metrics
ggen a2a status text-generator --metrics

# Send a message to the agent
ggen a2a message send text-generator "Generate summary of document"

# Check message history
ggen a2a message list text-generator --limit 10

# Stop the agent when done
ggen a2a stop text-generator --timeout 30
```

### Multi-Agent Coordination

```bash
# Start multiple agents
ggen a2a start text-generator &
ggen a2a start code-analyzer &
ggen a2a start workflow-agent &

# Wait for agents to start
sleep 5

# Verify all agents are running
ggen a2a list

# Send coordinated messages
ggen a2a message send workflow-agent "start parallel-analysis"
ggen a2a message send code-analyzer '{"action": "scan", "path": "src/"}'
ggen a2a message send text-generator "Prepare status report"

# Check status of all agents
for agent in text-generator code-analyzer workflow-agent; do
    echo "=== Status for $agent ==="
    ggen a2a status "$agent" --metrics
done
```

### Troubleshooting

```bash
# Check agent status
ggen a2a status text-generator --metrics

# Verify connectivity
curl -f http://127.0.0.1:8080/health || echo "A2A server not accessible"

# Check message history for errors
ggen a2a message list text-generator --limit 50

# Restart an agent
ggen a2a stop text-generator --timeout 5
sleep 2
ggen a2a start text-generator --timeout 60
```

### MCP Integration

```bash
# Configure A2A for MCP integration
ggen mcp config init --a2a

# Validate A2A configuration
ggen mcp config validate --a2a

# Show effective A2A configuration
ggen mcp config show --a2a

# List A2A agents
ggen a2a list

# Bridge an A2A agent as MCP tool
ggen mcp bridge text-generator

# Test the bridged tool
ggen mcp test "agent-text-generator"
```

---

## Error Handling

### Common Exit Codes

| Code | Description |
|------|-------------|
| `0` | Success |
| `1` | General error |
| `2` | Agent not found |
| `3` | Timeout exceeded |
| `4` | Connection error |
| `5` | Authentication error |

### Error Messages

| Error | Cause | Solution |
|-------|-------|----------|
| `Agent 'X' not found` | Agent does not exist | Check available agents with `ggen a2a list` |
| `Connection refused` | A2A server not running | Start the A2A server |
| `Timeout exceeded` | Operation took too long | Increase timeout with `--timeout` |
| `Authentication failed` | Invalid credentials | Check authentication configuration |

---

## Type Definitions

### AgentStatus

```rust
pub enum AgentStatus {
    Running,   // Agent is actively processing tasks
    Idle,      // Agent is running but idle
    Stopped,   // Agent is not running
    Error,     // Agent encountered an error
}
```

### MessageDeliveryStatus

```rust
pub enum MessageDeliveryStatus {
    Pending,   // Message is queued for delivery
    Sent,      // Message has been sent
    Delivered, // Message has been delivered
    Failed,    // Message delivery failed
}
```

### MessageDirection

```rust
pub enum MessageDirection {
    Outgoing,  // Message sent to the agent
    Incoming,  // Message received from the agent
}
```

### AgentMetrics

```rust
pub struct AgentMetrics {
    pub tasks_completed: u64,      // Successfully completed tasks
    pub tasks_failed: u64,         // Failed tasks
    pub messages_sent: u64,        // Messages sent by agent
    pub messages_received: u64,    // Messages received
    pub uptime_seconds: u64,       // Uptime in seconds
    pub memory_usage_mb: u64,      // Memory usage in MB
    pub cpu_usage_percent: f64,    // CPU usage percentage
}
```

---

## See Also

- [A2A Configuration Reference](../configuration/a2a-config.md)
- [MCP Command Reference](./mcp.md)
- [MCP A2A Integration Guide](../MCP_A2A_INTEGRATION.md)
