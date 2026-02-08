# MCP Client Integration - Quick Start Guide

## 5-Minute Setup

### Step 1: Verify Configuration
```bash
cd /home/user/ggen
cat scripts/claude-code-web-simulator/.mcp.json
```

Expected output should show `mcpServers` with enabled servers.

### Step 2: Run Tests
```bash
cd /home/user/ggen/scripts/claude-code-web-simulator
bash tests/mcp-client-tests.sh
```

Expected: All 15 tests pass (100% pass rate).

### Step 3: Try the Examples
```bash
# Initialize client
bash examples/mcp-client-example.sh initialize

# Discover tools
bash examples/mcp-client-example.sh discover

# Search for tools
bash examples/mcp-client-example.sh search
```

## Basic Usage

### In Your Scripts

```bash
#!/bin/bash
set -euo pipefail

# Source the MCP client module
source ./scripts/claude-code-web-simulator/modules/mcp-client.sh

# Initialize
if ! mcp_init_client; then
    echo "Failed to initialize MCP client"
    exit 1
fi

# Use MCP functions
echo "Available tools:"
mcp_list_tools | head -10

echo "Git tools:"
mcp_search_tools "git"

# Check status
mcp_status
```

## Common Tasks

### Find a Specific Tool
```bash
mcp_search_tools "keyword"
```

### List All Available Tools
```bash
mcp_list_tools
```

### Execute a Tool
```bash
result=$(mcp_call_tool "tool-name" "parameter")
echo "$result"
```

### Check Server Health
```bash
mcp_health_check
mcp_status
```

### Clear Cache
```bash
mcp_clear_cache
```

## Environment Variables

```bash
# Set timeout to 20 seconds
export MCP_TIMEOUT=20

# Enable debug output
export DEBUG_MCP=1

# Customize cache location
export MCP_CACHE_DIR="/tmp/mcp-cache"

# Set cache duration to 10 minutes
export MCP_CACHE_DURATION=600
```

## Troubleshooting

### No servers connecting?
```bash
export DEBUG_MCP=1
bash scripts/claude-code-web-simulator/examples/mcp-client-example.sh initialize
```

### Tools not found?
```bash
mcp_list_tools
```

### Need to refresh cache?
```bash
mcp_clear_cache
mcp_list_tools
```

## File Locations

- **Module**: `/home/user/ggen/scripts/claude-code-web-simulator/modules/mcp-client.sh`
- **Tests**: `/home/user/ggen/scripts/claude-code-web-simulator/tests/mcp-client-tests.sh`
- **Examples**: `/home/user/ggen/scripts/claude-code-web-simulator/examples/mcp-client-example.sh`
- **Docs**: `/home/user/ggen/scripts/claude-code-web-simulator/README-MCP.md`

## Next Steps

1. **Read the full documentation**: `README-MCP.md`
2. **Review integration patterns**: `MCP-INTEGRATION.md`
3. **Explore examples**: `examples/mcp-client-example.sh`
4. **Run tests**: `tests/mcp-client-tests.sh`
5. **Integrate with your code**: Use in your scripts

## Support

For detailed information:
- API Reference: `modules/README-MCP.md`
- Integration Guide: `MCP-INTEGRATION.md`
- Examples: `examples/mcp-client-example.sh`
- Tests: `tests/mcp-client-tests.sh`

## Key Features

- ✅ Real MCP server connectivity
- ✅ Automatic configuration loading
- ✅ Tool discovery with caching
- ✅ Timeout enforcement
- ✅ Error handling and retry
- ✅ Health monitoring
- ✅ Production-ready code

That's it! You're ready to use MCP client functionality in ggen v6.0.0.
