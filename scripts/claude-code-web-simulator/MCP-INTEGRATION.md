# MCP Client Integration Guide

## Overview

The MCP Client Module provides production-ready real MCP server connectivity to replace simulated MCP proxy implementations. This document explains the integration architecture, usage patterns, and how to leverage MCP servers in your ggen projects.

## Architecture

### Component Relationships

```
┌─────────────────────────────────────────────────────────────┐
│                    Claude Code Web                           │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  ┌──────────────────────────────────────────────────────┐   │
│  │         Simulator Main Orchestrator                  │   │
│  │  (scripts/claude-code-web-simulator/main.sh)         │   │
│  └────────────────┬─────────────────────────────────────┘   │
│                   │                                          │
│       ┌───────────┼───────────┐                             │
│       │           │           │                             │
│  ┌────▼─────┐ ┌──▼──────┐ ┌─▼────────────┐               │
│  │   Agent  │ │  ggen   │ │   MCP       │               │
│  │ Orches.  │ │Pipeline │ │  Client ◄─── REAL SERVERS  │
│  └──────────┘ └─────────┘ └──────────────┘               │
│                                                              │
└─────────────────────────────────────────────────────────────┘
        ▲
        │
   MCP Server Connections
   (Git, Bash, RDF, etc.)
```

### Module Organization

```
scripts/claude-code-web-simulator/
├── main.sh                          # Main orchestrator
├── modules/
│   ├── mcp-client.sh               # ◄─ REAL MCP client (NEW)
│   ├── agent-orchestrator.sh       # Agent coordination
│   ├── ggen-pipeline.sh            # ggen integration
│   ├── hooks-engine.sh             # Pre/post hooks
│   ├── error-handler.sh            # Error handling
│   ├── memory-integrator.sh        # Memory coordination
│   ├── sandbox-simulator.sh        # Isolation
│   ├── receipt-generator.sh        # Receipt generation
│   └── invocation-patterns.sh      # Execution patterns
├── tests/
│   ├── mcp-client-tests.sh         # ◄─ MCP tests (NEW)
│   ├── sandbox-tests.sh
│   ├── agent-tests.sh
│   └── integration-tests.sh
├── examples/
│   ├── mcp-client-example.sh       # ◄─ MCP examples (NEW)
│   ├── agent-usage.sh
│   ├── ggen-integration.sh
│   └── error-recovery.sh
├── README-MCP.md                   # ◄─ MCP documentation (NEW)
├── MCP-INTEGRATION.md              # ◄─ This file (NEW)
└── config/
    └── mcp-servers.json            # Server configuration
```

## Integration Points

### 1. Configuration Management

The MCP client integrates with three configuration sources:

```bash
# Priority order (highest to lowest)
.mcp.json                  # Project-local config (fastest to update)
~/.mcp.json               # User home config (shared across projects)
~/.claude.json            # Claude Code config (fallback)
```

### 2. Main Orchestrator Integration

The simulator's main script can use MCP clients for agent execution:

```bash
# In scripts/claude-code-web-simulator/main.sh
source ./modules/mcp-client.sh

# Initialize on startup
mcp_init_client || log_warn "MCP servers unavailable"

# Use in agent execution
result=$(mcp_call_tool "git-status" 2>/dev/null || echo "{}")
```

### 3. Agent Orchestrator Integration

Agents can query MCP servers for tool availability:

```bash
# In modules/agent-orchestrator.sh
source ./modules/mcp-client.sh

# Discover available tools before task assignment
available_tools=$(mcp_list_tools 2>/dev/null)

# Route tasks to appropriate servers
if echo "$available_tools" | grep -q "git"; then
    # Assign git-related tasks
fi
```

### 4. Hook Engine Integration

Pre/post execution hooks can use MCP tools:

```bash
# Pre-execution: Check tool availability
if ! mcp_health_check; then
    log_warn "MCP servers not ready"
fi

# Post-execution: Store results via MCP
mcp_call_tool "audit-log" "$execution_result"
```

## Configuration Setup

### Quick Start

1. **Verify Configuration File**

```bash
# Check project configuration
cat .mcp.json

# Should contain:
# {
#   "mcpServers": {
#     "server-name": {
#       "command": "...",
#       "args": [...],
#       "disabled": false
#     }
#   }
# }
```

2. **Enable MCP Servers**

```bash
# Edit .mcp.json to ensure servers are not disabled
cat > .mcp.json <<'EOF'
{
  "mcpServers": {
    "git": {
      "command": "git",
      "args": ["mcp-server"],
      "env": {"GIT_REPO": "/home/user/ggen"},
      "disabled": false
    },
    "bash": {
      "command": "bash",
      "args": ["--init-file", ".claude/helpers/bash-init.sh"],
      "disabled": false
    }
  }
}
EOF
```

3. **Test Connectivity**

```bash
# Run initialization test
bash scripts/claude-code-web-simulator/tests/mcp-client-tests.sh

# Run example
bash scripts/claude-code-web-simulator/examples/mcp-client-example.sh initialize
```

## Usage Patterns

### Pattern 1: One-Time Tool Query

```bash
#!/bin/bash
source ./scripts/claude-code-web-simulator/modules/mcp-client.sh

# Initialize and query
mcp_init_client && mcp_list_tools | grep "git"
```

### Pattern 2: Session-Based Usage

```bash
#!/bin/bash
source ./scripts/claude-code-web-simulator/modules/mcp-client.sh

# Initialize once
mcp_init_client || exit 1

# Perform multiple operations
tools=$(mcp_list_tools)
git_tools=$(echo "$tools" | grep "git")

# Search for specific tools
mcp_search_tools "parse"

# Check health
mcp_health_check

# Clean up when done
mcp_clear_cache
```

### Pattern 3: Integration with Agent Workflow

```bash
#!/bin/bash
set -euo pipefail

source ./scripts/claude-code-web-simulator/modules/mcp-client.sh
source ./scripts/claude-code-web-simulator/modules/agent-orchestrator.sh

# Initialize MCP
mcp_init_client || {
    echo "WARNING: MCP unavailable, continuing with limited tools"
    false
} || true

# Run agent with tool access
run_agent "code-reviewer" \
    --available-tools "$(mcp_list_tools 2>/dev/null || echo '')" \
    --fallback-mode true
```

### Pattern 4: Error Recovery with Fallbacks

```bash
#!/bin/bash
source ./scripts/claude-code-web-simulator/modules/mcp-client.sh

# Initialize with retry
export MCP_RETRY_COUNT=5
export MCP_RETRY_DELAY=3

if ! mcp_init_client; then
    echo "MCP initialization failed, using fallback mode"
    # Run without MCP
    FALLBACK_MODE=1
else
    FALLBACK_MODE=0
    # Use MCP-enhanced execution
fi
```

## Performance Considerations

### Optimization Strategies

1. **Cache Tool Lists**

```bash
# Tool list cached for 5 minutes (default)
export MCP_CACHE_DURATION=600  # 10 minutes

# Tool queries reuse cache
mcp_list_tools  # ≤1s (cached)
mcp_list_tools  # ≤1s (cached)
sleep 600
mcp_list_tools  # 5-10s (network query)
```

2. **Batch Tool Execution**

```bash
# Initialize once
mcp_init_client

# Execute multiple tools efficiently
for tool in $(mcp_list_tools | cut -d: -f2 | head -5); do
    mcp_call_tool "$tool" 2>/dev/null || true
done
```

3. **Selective Server Usage**

```bash
# Disable unused servers
cat > .mcp.json <<'EOF'
{
  "mcpServers": {
    "git": {"command": "git", "disabled": false},
    "unused": {"command": "unused-tool", "disabled": true}
  }
}
EOF
```

### SLO Targets

| Operation | Target | Notes |
|-----------|--------|-------|
| Client initialization | ≤2s | With network, retries enabled |
| Tool list query (cached) | ≤1s | After first query |
| Tool list query (uncached) | ≤5s | For 3 servers |
| Tool execution | ≤10s | Default timeout |
| Health check | ≤2s | All servers |
| Search operation | ≤2s | Cached |

## Troubleshooting

### Issue: MCP servers not connecting

```bash
# 1. Check configuration
cat .mcp.json

# 2. Verify servers are enabled
jq '.mcpServers[] | select(.disabled == true)' .mcp.json

# 3. Check tool availability
bash scripts/claude-code-web-simulator/examples/mcp-client-example.sh health

# 4. Enable debug logging
export DEBUG_MCP=1
bash scripts/claude-code-web-simulator/examples/mcp-client-example.sh initialize
```

### Issue: Tool execution timeout

```bash
# Increase timeout
export MCP_TIMEOUT=30

# Then retry
mcp_call_tool "long-running-tool"
```

### Issue: Cache staleness

```bash
# Clear cache
mcp_clear_cache

# Or set shorter cache duration
export MCP_CACHE_DURATION=30  # 30 seconds
```

## Integration with Claude Code Infrastructure

### MCP + Receipt Generation

MCP operations generate deterministic receipts:

```bash
# Each MCP operation recorded
{
  "timestamp": "2026-01-29T18:00:00Z",
  "operation": "tool_execution",
  "tool": "git-status",
  "server": "git",
  "duration_ms": 1234,
  "status": "success",
  "output_hash": "sha256:abc123...",
  "cache_hit": false
}
```

## Testing and Validation

### Run Full Test Suite

```bash
# Unit tests
bash scripts/claude-code-web-simulator/tests/mcp-client-tests.sh

# Integration examples
bash scripts/claude-code-web-simulator/examples/mcp-client-example.sh all

# Performance baseline
time bash scripts/claude-code-web-simulator/examples/mcp-client-example.sh discover
```

### Manual Validation

```bash
# 1. Initialize
source scripts/claude-code-web-simulator/modules/mcp-client.sh
mcp_init_client

# 2. Status check
mcp_status

# 3. Health check
mcp_health_check

# 4. Tool discovery
mcp_list_tools | head -20

# 5. Tool search
mcp_search_tools "git"

# 6. Tool execution (example)
mcp_call_tool "git-status" 2>/dev/null || echo "Git not available"
```

## Future Roadmap

### Phase 1 (Current): Real MCP Integration ✓
- [x] MCP client initialization and connection
- [x] Configuration loading from multiple sources
- [x] Tool discovery and listing
- [x] Tool execution with timeout enforcement
- [x] Error handling and retry logic
- [x] Comprehensive testing
- [x] Full documentation

### Phase 2 (Planned): Advanced Features
- [ ] Connection pooling for performance
- [ ] Automatic server health monitoring
- [ ] Tool execution metrics and profiling
- [ ] Multi-threaded tool execution
- [ ] Advanced caching strategies (LRU)
- [ ] Tool dependency resolution
- [ ] Server fallback and failover

### Phase 3 (Future): Ecosystem Integration
- [ ] Marketplace integration for tool discovery
- [ ] Tool version management
- [ ] Distributed execution across agents
- [ ] GraphQL API for tool queries
- [ ] WebSocket real-time updates

## References

- **MCP Specification**: [Model Context Protocol](https://modelcontextprotocol.io)
- **ggen Documentation**: See `/docs` in project root
- **Module Documentation**: See `README-MCP.md` in this directory

## Support

For issues or questions:
1. Check `README-MCP.md` for API documentation
2. Run tests with `DEBUG_MCP=1` for verbose output
3. Review examples in `examples/mcp-client-example.sh`
4. Check module status with `mcp_status` and `mcp_health_check`

## License

This integration is part of the ggen project (v6.0.0) and is licensed under the same terms as the main project.
