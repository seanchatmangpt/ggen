<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [How to Bridge A2A Agents as MCP Tools](#how-to-bridge-a2a-agents-as-mcp-tools)
  - [Understanding Agent Bridging](#understanding-agent-bridging)
  - [Basic Agent Bridging](#basic-agent-bridging)
  - [Bridging with Custom Tool Name](#bridging-with-custom-tool-name)
  - [Bridging with Custom Schema](#bridging-with-custom-schema)
  - [Bridging Multiple Agents](#bridging-multiple-agents)
  - [Listing Bridged Agents](#listing-bridged-agents)
  - [Unbridging Agents](#unbridging-agents)
  - [Testing Bridged Tools](#testing-bridged-tools)
  - [Advanced Configuration](#advanced-configuration)
    - [Timeout Configuration](#timeout-configuration)
    - [Retry Configuration](#retry-configuration)
    - [Rate Limiting](#rate-limiting)
  - [Troubleshooting](#troubleshooting)
    - [Bridge Fails](#bridge-fails)
    - [Tool Not Accessible](#tool-not-accessible)
    - [Permission Denied](#permission-denied)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# How to Bridge A2A Agents as MCP Tools

Guide to exposing A2A (Agent-to-Agent) agents as MCP tools for external access.

## Understanding Agent Bridging

Agent bridging allows A2A agents to be accessible through the MCP protocol, enabling:

- External MCP clients to invoke ggen agents
- Standardized tool interfaces across different agent systems
- Centralized agent management through MCP

## Basic Agent Bridging

**Problem:** You need to expose an A2A agent as an MCP tool.

**Solution:** Use the `ggen mcp bridge` command.

```bash
# Bridge an agent with default settings
ggen mcp bridge "text-generator"

# The tool will be named "agent-text-generator"
```

**Expected Output:**
```bash
$ ggen mcp bridge "text-generator"

Bridging agent 'text-generator' as MCP tool 'agent-text-generator'
Agent 'text-generator' successfully bridged as MCP tool

Tool: agent-text-generator
Description: Bridge for agent text-generator
Status: available
```

## Bridging with Custom Tool Name

**Problem:** You want a custom name for the bridged tool.

**Solution:** Use the `--tool-name` option.

```bash
# Bridge with custom tool name
ggen mcp bridge "text-generator" --tool-name "ai-writer"

# The tool will be named "ai-writer"
```

**Expected Output:**
```bash
$ ggen mcp bridge "text-generator" --tool-name "ai-writer"

Bridging agent 'text-generator' as MCP tool 'ai-writer'
Agent 'text-generator' successfully bridged as MCP tool

Tool: ai-writer
Description: Custom bridge for agent text-generator
```

## Bridging with Custom Schema

**Problem:** You need to define a custom schema for the tool.

**Solution:** Use the `--schema` option.

```bash
# Bridge with custom schema
ggen mcp bridge "code-analyzer" \
  --tool-name "analyzer" \
  --schema '{
    "type": "object",
    "description": "Analyze code for quality and patterns",
    "properties": {
      "code": {"type": "string", "description": "Code to analyze"},
      "language": {"type": "string", "description": "Programming language"},
      "options": {
        "type": "object",
        "properties": {
          "deep": {"type": "boolean", "default": false},
          "metrics": {"type": "boolean", "default": true}
        }
      }
    },
    "required": ["code", "language"]
  }'
```

## Bridging Multiple Agents

**Problem:** You need to bridge multiple agents at once.

**Solution:** Use a script or loop.

```bash
# Bridge multiple agents
for agent in text-generator code-analyzer workflow-agent; do
  ggen mcp bridge "$agent"
done

# Or use a config file
ggen mcp bridge --config agents.yaml
```

**Config file example (agents.yaml):**
```yaml
bridges:
  - agent: text-generator
    tool_name: ai-writer
    enabled: true

  - agent: code-analyzer
    tool_name: analyzer
    enabled: true

  - agent: workflow-agent
    tool_name: workflow
    enabled: true
```

## Listing Bridged Agents

**Problem:** You need to see all currently bridged agents.

**Solution:** Use the `ggen mcp list` command with filtering.

```bash
# List all bridged agents
ggen mcp list --type agent

# List with details
ggen mcp list --type agent --verbose

# List as JSON
ggen mcp list --type agent --format json
```

**Expected Output:**
```bash
$ ggen mcp list --type agent

Agent-Bridged Tools (3):
  • agent-text-generator (agent)
    Agent: text-generator
    Status: available

  • agent-code-analyzer (agent)
    Agent: code-analyzer
    Status: available

  • agent-workflow-agent (agent)
    Agent: workflow-agent
    Status: available
```

## Unbridging Agents

**Problem:** You need to remove a bridge.

**Solution:** Use the `ggen mcp unbridge` command.

```bash
# Unbridge a specific tool
ggen mcp unbridge "agent-text-generator"

# Unbridge by agent name
ggen mcp unbridge --agent "text-generator"

# Unbridge all agents
ggen mcp unbridge --all
```

## Testing Bridged Tools

**Problem:** You need to verify a bridged tool works correctly.

**Solution:** Use the `ggen mcp test` command.

```bash
# Test a bridged tool
ggen mcp test "agent-text-generator"

# Test with arguments
ggen mcp test "agent-code-analyzer" \
  --args '{"code": "fn main() {}", "language": "rust"}'
```

**Expected Output:**
```bash
$ ggen mcp test "agent-text-generator" \
  --args '{"prompt": "Hello, world!"}'

Testing MCP tool: agent-text-generator
Arguments: {"prompt":"Hello, world!"}

Test completed successfully
Result:
{
  "status": "success",
  "output": "Hello! How can I help you today?",
  "agent": "text-generator",
  "latency_ms": 45
}
```

## Advanced Configuration

### Timeout Configuration

**Problem:** Bridged agents timeout on long operations.

**Solution:** Configure timeout per bridge.

```bash
# Set timeout in seconds
ggen mcp bridge "slow-agent" --timeout 120

# Set no timeout
ggen mcp bridge "agent" --timeout 0
```

### Retry Configuration

**Problem:** Transient failures cause bridge failures.

**Solution:** Configure retry behavior.

```bash
# Set retry count
ggen mcp bridge "flaky-agent" --retries 5

# Set retry delay
ggen mcp bridge "agent" --retry-delay 1000
```

### Rate Limiting

**Problem:** External clients overwhelm the agent.

**Solution:** Configure rate limits.

```bash
# Set rate limit
ggen mcp bridge "agent" \
  --rate-limit 10 \
  --rate-window 60
```

## Troubleshooting

### Bridge Fails

**Problem:** `ggen mcp bridge` returns an error.

**Solutions:**
```bash
# Check agent exists
ggen agent list | grep "agent-name"

# Check agent is running
ggen agent status "agent-name"

# Check MCP server is running
ggen mcp server status

# Try with verbose output
ggen mcp bridge "agent" --verbose
```

### Tool Not Accessible

**Problem:** Bridge succeeds but tool is not accessible.

**Solutions:**
```bash
# Check tool status
ggen mcp status "tool-name"

# Refresh MCP server
ggen mcp server restart

# Verify permissions
ggen mcp list --type agent --status unavailable
```

### Permission Denied

**Problem:** Bridge operation fails with permission error.

**Solutions:**
```bash
# Check authentication
ggen mcp config show

# Verify API key
echo $GGEN_MCP_API_KEY

# Re-authenticate
ggen mcp auth login
```

## Next Steps

- **List tools:** [How to List and Filter MCP Tools](list-tools.md)
- **Configure server:** [How to Configure MCP Server Settings](configure-server.md)
- **Setup authentication:** [How to Setup Authentication](setup-authentication.md)
- **Start agents:** [How to Start A2A Agents](../a2a/start-agent.md)
- **Integration guide:** [MCP A2A Integration Guide](../MCP_A2A_INTEGRATION.md)
