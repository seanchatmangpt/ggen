<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [How to List and Filter MCP Tools](#how-to-list-and-filter-mcp-tools)
  - [Listing All Tools](#listing-all-tools)
  - [Filtering by Type](#filtering-by-type)
  - [Filtering by Status](#filtering-by-status)
  - [Filtering by Capability](#filtering-by-capability)
  - [Searching Tools](#searching-tools)
  - [Viewing Tool Details](#viewing-tool-details)
  - [Exporting Tool Lists](#exporting-tool-lists)
  - [Troubleshooting](#troubleshooting)
    - [No Tools Found](#no-tools-found)
    - [Outdated Tool List](#outdated-tool-list)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# How to List and Filter MCP Tools

Guide to discovering and filtering available MCP (Model Context Protocol) tools in ggen.

## Listing All Tools

**Problem:** You need to see all available MCP tools.

**Solution:** Use the `ggen mcp list` command.

```bash
# List all tools
ggen mcp list

# List with detailed information
ggen mcp list --verbose

# List as JSON for parsing
ggen mcp list --format json
```

**Expected Output:**
```
Listing 12 MCP tools (9 core, 3 agent)

  • agent-list (core)
  • agent-start (core)
  • agent-status (core)
  • workflow-start (core)
  • agent-text-generator (agent)
  • agent-code-analyzer (agent)
  • agent-workflow-agent (agent)
```

## Filtering by Type

**Problem:** You only want to see tools of a specific type.

**Solution:** Use the `--type` filter.

```bash
# List only core tools
ggen mcp list --type core

# List only agent-bridged tools
ggen mcp list --type agent

# List only custom tools
ggen mcp list --type custom
```

**Expected Output:**
```bash
$ ggen mcp list --type core

Core MCP Tools (9):
  • agent-list - List all registered agents
  • agent-start - Start an agent
  • agent-status - Show agent status
  • workflow-start - Start a workflow
```

## Filtering by Status

**Problem:** You want to see only available tools.

**Solution:** Use the `--status` filter.

```bash
# List only available tools
ggen mcp list --status available

# List only unavailable tools
ggen mcp list --status unavailable

# List all tools regardless of status
ggen mcp list --status all
```

**Expected Output:**
```bash
$ ggen mcp list --status available

Available Tools (11):
  ✓ agent-list
  ✓ agent-start
  ✓ workflow-start
  ✗ agent-legacy (unavailable)
```

## Filtering by Capability

**Problem:** You need tools with specific capabilities.

**Solution:** Use the `--capability` filter.

```bash
# List tools with text generation capability
ggen mcp list --capability text-generation

# List tools with code analysis capability
ggen mcp list --capability code-analysis

# List tools with workflow capability
ggen mcp list --capability workflow
```

**Expected Output:**
```bash
$ ggen mcp list --capability text-generation

Text Generation Tools (3):
  • text-generator - Generate text content
  • agent-ai-writer - AI writing assistant
  • content-creator - Create various content types
```

## Searching Tools

**Problem:** You need to find tools by name or description.

**Solution:** Use the `--search` filter.

```bash
# Search by tool name
ggen mcp list --search "agent"

# Search by description keyword
ggen mcp list --search "workflow"

# Case-insensitive search
ggen mcp list --search "START" --ignore-case
```

**Expected Output:**
```bash
$ ggen mcp list --search "agent"

Matching Tools (5):
  • agent-list - List all registered agents
  • agent-start - Start an agent
  • agent-status - Show agent status
  • agent-text-generator - Bridge for agent text-generator
  • code-agent - Bridge for agent code-agent
```

## Viewing Tool Details

**Problem:** You need detailed information about a specific tool.

**Solution:** Use the `ggen mcp status` command with `--schema`.

```bash
# View tool status
ggen mcp status "agent-list"

# View with full schema
ggen mcp status "agent-list" --schema

# View with examples
ggen mcp status "agent-start" --examples
```

**Expected Output:**
```bash
$ ggen mcp status "agent-start" --schema

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
    },
    "capabilities": {
      "type": "array",
      "items": {"type": "string"},
      "description": "Agent capabilities"
    }
  },
  "required": ["name"]
}

Example:
ggen mcp test "agent-start" --args '{"name": "text-generator"}'
```

## Exporting Tool Lists

**Problem:** You need to export the tool list for documentation or scripting.

**Solution:** Use the `--format` and `--output` options.

```bash
# Export as JSON
ggen mcp list --format json --output tools.json

# Export as CSV
ggen mcp list --format csv --output tools.csv

# Export as Markdown table
ggen mcp list --format markdown --output tools.md
```

**Expected Output (JSON):**
```json
{
  "tools": [
    {
      "name": "agent-list",
      "description": "List all registered agents",
      "type": "core",
      "status": "available"
    }
  ],
  "total_count": 12,
  "core_tools": 9,
  "agent_tools": 3
}
```

## Troubleshooting

### No Tools Found

**Problem:** `ggen mcp list` returns no tools.

**Solutions:**
```bash
# Check MCP server is running
ggen mcp server status

# Verify configuration
ggen mcp config validate

# Check connection
curl -f "$GGEN_MCP_SERVER_URL/health"

# Restart MCP server
ggen mcp server restart
```

### Outdated Tool List

**Problem:** Listed tools don't match available tools.

**Solutions:**
```bash
# Refresh tool list
ggen mcp list --refresh

# Clear cache
ggen mcp cache clear

# Reconnect to server
ggen mcp reconnect
```

## Next Steps

- **Bridge agents as tools:** [How to Bridge A2A Agents as MCP Tools](bridge-agents.md)
- **Configure MCP server:** [How to Configure MCP Server Settings](configure-server.md)
- **Setup authentication:** [How to Setup Authentication](setup-authentication.md)
- **CLI reference:** [CLI Reference](../reference/cli.md#mcp-commands)
- **Integration guide:** [MCP A2A Integration Guide](../MCP_A2A_INTEGRATION.md)
