<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [How to Start and Manage A2A Agents](#how-to-start-and-manage-a2a-agents)
  - [Understanding A2A Agents](#understanding-a2a-agents)
  - [Listing Available Agents](#listing-available-agents)
  - [Starting an Agent](#starting-an-agent)
  - [Starting with Configuration](#starting-with-configuration)
  - [Starting Multiple Agents](#starting-multiple-agents)
  - [Managing Agent Lifecycle](#managing-agent-lifecycle)
    - [Agent States](#agent-states)
    - [Checking Agent State](#checking-agent-state)
  - [Monitoring Agent Status](#monitoring-agent-status)
  - [Stopping Agents](#stopping-agents)
  - [Agent Capabilities](#agent-capabilities)
  - [Troubleshooting](#troubleshooting)
    - [Agent Fails to Start](#agent-fails-to-start)
    - [Agent Not Responding](#agent-not-responding)
    - [Agent Crashes](#agent-crashes)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# How to Start and Manage A2A Agents

Guide to starting, monitoring, and managing A2A (Agent-to-Agent) agents.

## Understanding A2A Agents

A2A agents are autonomous entities that can:
- Communicate with other agents via the A2A protocol
- Expose capabilities as MCP tools
- Participate in multi-agent workflows
- Scale independently

## Listing Available Agents

**Problem:** You need to see what agents are available.

**Solution:** Use the `ggen agent list` command.

```bash
# List all available agents
ggen agent list

# List with detailed information
ggen agent list --verbose

# List as JSON
ggen agent list --format json

# List only running agents
ggen agent list --status running
```

**Expected Output:**
```bash
$ ggen agent list --verbose

Available Agents (12):
  text-generator
    Status: ready
    Type: generative
    Capabilities: text-generation, summarization
    Resources: 2 CPU, 4GB RAM

  code-analyzer
    Status: ready
    Type: analytical
    Capabilities: code-analysis, pattern-detection
    Resources: 1 CPU, 2GB RAM

  workflow-agent
    Status: running
    Type: orchestration
    Capabilities: workflow-execution, task-coordination
    Resources: 2 CPU, 4GB RAM
```

## Starting an Agent

**Problem:** You need to start an agent.

**Solution:** Use the `ggen agent start` command.

```bash
# Start an agent
ggen agent start "text-generator"

# Start with output
ggen agent start "text-generator" --verbose

# Start and wait for ready
ggen agent start "text-generator" --wait
```

**Expected Output:**
```bash
$ ggen agent start "text-generator"

Starting agent: text-generator
Agent ID: ag_abc123xyz789
Status: starting
...
Agent started successfully
  Agent ID: ag_abc123xyz789
  Status: ready
  Endpoint: http://localhost:8080/agents/ag_abc123xyz789
```

## Starting with Configuration

**Problem:** You need to start an agent with specific settings.

**Solution:** Use configuration options.

```bash
# Start with specific capabilities
ggen agent start "code-analyzer" \
  --capabilities "code-analysis,security-scan"

# Start with resource limits
ggen agent start "text-generator" \
  --cpu-limit 2 \
  --memory-limit 4G

# Start with environment variables
ggen agent start "agent" \
  --env "MODEL=gpt-4" \
  --env "TEMPERATURE=0.7"

# Start with timeout
ggen agent start "slow-agent" \
  --startup-timeout 120
```

**Configuration file example:**
```yaml
# agent-config.yaml
agents:
  text-generator:
    capabilities:
      - text-generation
      - summarization
    resources:
      cpu: 2
      memory: 4G
    env:
      MODEL: gpt-4
      TEMPERATURE: 0.7
    timeout: 60
```

```bash
# Start with config file
ggen agent start "text-generator" --config agent-config.yaml
```

## Starting Multiple Agents

**Problem:** You need to start multiple agents at once.

**Solution:** Use a batch start command.

```bash
# Start multiple agents
ggen agent start \
  "text-generator" \
  "code-analyzer" \
  "workflow-agent"

# Start from config file
ggen agent start --all --config agents.yaml

# Start all available agents
ggen agent start --all
```

**Parallel startup:**
```bash
# Start agents in parallel
for agent in text-generator code-analyzer workflow-agent; do
  ggen agent start "$agent" &
done
wait
```

## Managing Agent Lifecycle

### Agent States

Agents progress through these states:

1. **initializing** - Agent is being created
2. **starting** - Agent is loading
3. **ready** - Agent is available for requests
4. **busy** - Agent is processing
5. **stopping** - Agent is shutting down
6. **stopped** - Agent has terminated
7. **error** - Agent encountered an error

### Checking Agent State

```bash
# Check specific agent
ggen agent status "text-generator"

# Check all agents
ggen agent list --state

# Watch state changes
ggen agent status "text-generator" --watch
```

## Monitoring Agent Status

**Problem:** You need to monitor agent health and activity.

**Solution:** Use the status and metrics commands.

```bash
# Show agent status
ggen agent status "text-generator"

# Show with metrics
ggen agent status "text-generator" --metrics

# Show with recent activity
ggen agent status "text-generator" --activity

# Continuous monitoring
ggen agent monitor "text-generator"
```

**Expected Output:**
```bash
$ ggen agent status "text-generator" --metrics

Agent: text-generator
ID: ag_abc123xyz789
Status: ready

Metrics:
  Requests processed: 1,234
  Average latency: 45ms
  Error rate: 0.1%
  Uptime: 2h 34m

Resources:
  CPU: 15% (0.3 / 2 cores)
  Memory: 1.2GB / 4GB
  Network: 10KB/s in, 25KB/s out

Recent Activity:
  [10:23:45] Completed: text-generation (52ms)
  [10:23:30] Completed: summarization (38ms)
  [10:23:15] Started: text-generation
```

## Stopping Agents

**Problem:** You need to stop a running agent.

**Solution:** Use the `ggen agent stop` command.

```bash
# Stop an agent gracefully
ggen agent stop "text-generator"

# Stop with timeout
ggen agent stop "text-generator" --timeout 30

# Force stop
ggen agent stop "text-generator" --force

# Stop all agents
ggen agent stop --all
```

**Expected Output:**
```bash
$ ggen agent stop "text-generator"

Stopping agent: text-generator
Waiting for in-flight requests to complete...
Agent stopped successfully
  Final status: graceful-shutdown
  Requests processed: 1,234
```

## Agent Capabilities

**Problem:** You need to understand what an agent can do.

**Solution:** Use the capabilities command.

```bash
# Show agent capabilities
ggen agent capabilities "text-generator"

# Show detailed capabilities
ggen agent capabilities "text-generator" --detailed

# List agents by capability
ggen agent list --capability "text-generation"
```

**Expected Output:**
```bash
$ ggen agent capabilities "text-generator" --detailed

Agent: text-generator

Capabilities:
  text-generation
    Description: Generate text from prompts
    Parameters:
      - prompt (required): Text prompt
      - max_length (optional): Maximum length
      - temperature (optional): Creativity (0-1)

  summarization
    Description: Summarize long text
    Parameters:
      - text (required): Text to summarize
      - length (optional): Summary length
      - style (optional): Summary style

Resource Requirements:
  CPU: 1-2 cores
  Memory: 2-4GB
  Startup time: ~5s
```

## Troubleshooting

### Agent Fails to Start

**Problem:** `ggen agent start` returns an error.

**Solutions:**
```bash
# Check agent exists
ggen agent list | grep "agent-name"

# Check required resources
ggen agent status "agent-name" --resources

# Check for port conflicts
lsof -i :8080

# Try with verbose output
ggen agent start "agent-name" --verbose

# Check logs
ggen agent logs "agent-name" --tail 100
```

### Agent Not Responding

**Problem:** Agent is running but not responding to requests.

**Solutions:**
```bash
# Check agent health
ggen agent health "agent-name"

# Check network connectivity
curl http://localhost:8080/agents/agent-id/health

# Restart agent
ggen agent restart "agent-name"

# Check for deadlocks
ggen agent status "agent-name" --metrics
```

### Agent Crashes

**Problem:** Agent starts but crashes shortly after.

**Solutions:**
```bash
# Check crash logs
ggen agent logs "agent-name" --tail 100

# Check resource usage
ggen agent status "agent-name" --resources

# Increase resource limits
ggen agent start "agent-name" --memory-limit 8G

# Check for errors
ggen agent logs "agent-name" --grep "error" --tail 50
```

## Next Steps

- **Send messages:** [How to Send Messages Between Agents](send-messages.md)
- **Monitor agents:** [How to Monitor Agent Health and Metrics](monitor-agents.md)
- **Configure transport:** [How to Configure Transport Protocols](configure-transport.md)
- **Bridge to MCP:** [How to Bridge A2A Agents as MCP Tools](../mcp/bridge-agents.md)
- **Integration guide:** [MCP A2A Integration Guide](../MCP_A2A_INTEGRATION.md)
