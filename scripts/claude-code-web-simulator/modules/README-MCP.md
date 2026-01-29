# MCP Client Module - Production-Ready Integration

## Overview

The MCP Client Module (`mcp-client.sh`) provides production-grade Model Context Protocol (MCP) client functionality for real MCP server connectivity and tool access. It replaces simulated MCP proxy implementations with actual client SDK integration.

## Features

### Core Functionality
- **Real MCP Server Integration**: Connect to actual MCP servers via JSON-RPC protocol
- **Automatic Configuration Loading**: Loads configuration from multiple standard locations with intelligent precedence
- **Tool Discovery**: Query connected servers for available tools with descriptions
- **Tool Execution**: Execute tools with parameters, timeout enforcement, and error handling
- **Tool Search**: Search for tools across all connected servers
- **Health Monitoring**: Verify server connectivity and health status
- **Performance Caching**: Cache tool lists to minimize server queries
- **Retry Logic**: Automatic retry with exponential backoff for transient failures

### Configuration Support
- **Multiple Configuration Formats**: Supports both `mcpServers` and `mcp_servers` keys
- **Configuration Precedence**:
  1. `.mcp.json` (project-local config)
  2. `~/.mcp.json` (user home config)
  3. `~/.claude.json` (fallback)
- **Environment Variable Override**: All timeouts and limits can be configured via environment variables

### Error Handling
- **Graceful Degradation**: Continues operation even if some servers are unavailable
- **Clear Error Messages**: Informative error messages with actionable guidance
- **Timeout Prevention**: All operations have configurable timeout enforcement
- **Health Checks**: Automatic verification that servers are responsive

## Installation

### Prerequisites
- Bash 4.0 or later
- `jq` for JSON parsing (optional, but recommended)
- Access to configured MCP servers

### Setup
```bash
# Source the module in your script
source ./modules/mcp-client.sh

# Or directly call functions
./modules/mcp-client.sh
```

## Configuration

### Configuration File Format

Create a `.mcp.json` file in your project root or home directory:

```json
{
  "mcpServers": {
    "server-name": {
      "command": "command-to-execute",
      "args": ["arg1", "arg2"],
      "env": {
        "ENVIRONMENT_VAR": "value"
      },
      "disabled": false
    }
  }
}
```

### Example Configuration

```json
{
  "mcpServers": {
    "bash": {
      "command": "bash",
      "args": ["--init-file", ".claude/helpers/bash-init.sh"],
      "env": {
        "CARGO_TERM_COLOR": "always"
      },
      "disabled": false
    },
    "git": {
      "command": "git",
      "args": ["mcp-server"],
      "env": {
        "GIT_REPO": "/home/user/ggen"
      },
      "disabled": false
    },
    "rdf-tools": {
      "command": "oxigraph",
      "args": ["server"],
      "env": {
        "OXIGRAPH_PORT": "7878"
      },
      "disabled": false
    }
  }
}
```

### Environment Variables

Control MCP client behavior via environment variables:

```bash
# Connection and Execution
export MCP_TIMEOUT=10           # Tool execution timeout (seconds)
export MCP_RETRY_COUNT=3        # Number of connection retry attempts
export MCP_RETRY_DELAY=2        # Delay between retries (seconds)

# Caching
export MCP_CACHE_DIR=".mcp-cache"    # Cache directory location
export MCP_CACHE_DURATION=300        # Cache validity duration (seconds)

# Debugging
export DEBUG_MCP=1              # Enable debug logging
```

## Usage

### 1. Initialize MCP Client

```bash
#!/bin/bash
source ./modules/mcp-client.sh

# Initialize with default configuration
if ! mcp_init_client; then
    echo "Failed to initialize MCP client"
    exit 1
fi
```

### 2. Check Client Status

```bash
# View current status
mcp_status

# Run health check
mcp_health_check
```

### 3. List Available Tools

```bash
# Get all tools from all connected servers
tools=$(mcp_list_tools)
echo "$tools"
```

### 4. Search for Tools

```bash
# Search for tools matching a pattern
mcp_search_tools "git"      # Find git-related tools
mcp_search_tools "rdf"      # Find RDF-related tools
mcp_search_tools "bash"     # Find bash-related tools
```

### 5. Execute Tools

```bash
# Execute a tool with parameters
result=$(mcp_call_tool "git-clone" "https://github.com/user/repo.git")
echo "$result"

# Execute tools with complex parameters
mcp_call_tool "sparql-query" '{"query": "SELECT * WHERE { ?s ?p ?o }"}'
```

### 6. Manage Cache

```bash
# Clear tool cache to force refresh
mcp_clear_cache

# Cache is automatically managed with configurable duration
# Default cache validity: 300 seconds (5 minutes)
```

## API Reference

### Functions

#### `mcp_init_client()`
Initialize MCP client and connect to configured servers.

**Returns**: 0 on success, 1 on failure

**Example**:
```bash
if mcp_init_client; then
    echo "Connected to MCP servers"
fi
```

#### `mcp_list_tools()`
Query all connected servers for available tools.

**Returns**: 0 on success, 1 if no tools found

**Output**: Tool list formatted as "server: tool-name - description"

**Example**:
```bash
tools=$(mcp_list_tools)
echo "$tools" | grep "git"
```

#### `mcp_call_tool(tool_name, [parameters...])`
Execute a tool on the appropriate MCP server.

**Parameters**:
- `tool_name`: Name of the tool to execute
- `parameters`: Optional tool parameters

**Returns**: 0 on success, 1 on failure

**Output**: Tool execution result (JSON or raw output)

**Example**:
```bash
result=$(mcp_call_tool "git-status")
echo "$result"
```

#### `mcp_search_tools(query)`
Search for tools matching a query across all servers.

**Parameters**:
- `query`: Search pattern (case-insensitive)

**Returns**: 0 if matches found, 1 if no matches

**Output**: Matching tools with descriptions

**Example**:
```bash
mcp_search_tools "parse"  # Find parsing tools
```

#### `mcp_status()`
Display current MCP client status and configuration.

**Returns**: 0 if initialized, 1 if not initialized

**Output**: Formatted status information

**Example**:
```bash
mcp_status
```

#### `mcp_health_check()`
Verify MCP server connectivity and health.

**Returns**: 0 if all servers healthy, 1 if issues detected

**Output**: Health status for each server

**Example**:
```bash
mcp_health_check && echo "All servers healthy"
```

#### `mcp_clear_cache()`
Clear the MCP tool cache and force refresh on next query.

**Returns**: Always 0

**Example**:
```bash
mcp_clear_cache
```

## Integration Patterns

### Pattern 1: Simple Tool Execution

```bash
#!/bin/bash
set -euo pipefail
source ./modules/mcp-client.sh

# Initialize
mcp_init_client || exit 1

# Execute tool
result=$(mcp_call_tool "git-status")
echo "Result: $result"
```

### Pattern 2: Tool Discovery and Caching

```bash
#!/bin/bash
source ./modules/mcp-client.sh

# Initialize with custom cache location
export MCP_CACHE_DIR="/tmp/mcp-cache"
export MCP_CACHE_DURATION=600

mcp_init_client || exit 1

# List all tools (cached)
tools=$(mcp_list_tools)

# Search for specific tools
bash_tools=$(echo "$tools" | grep "bash" || true)
if [[ -n "$bash_tools" ]]; then
    echo "Found bash tools:"
    echo "$bash_tools"
fi
```

### Pattern 3: Error Handling and Retry

```bash
#!/bin/bash
source ./modules/mcp-client.sh

export MCP_RETRY_COUNT=5
export MCP_RETRY_DELAY=3

# Initialize with retry logic
if ! mcp_init_client; then
    echo "ERROR: Failed to connect to MCP servers after retries"
    exit 1
fi

# Execute tool with timeout handling
if ! result=$(mcp_call_tool "expensive-operation" 2>&1); then
    echo "Tool execution failed, retrying..."
    # Retry logic here
fi
```

### Pattern 4: Multi-Server Orchestration

```bash
#!/bin/bash
source ./modules/mcp-client.sh

mcp_init_client || exit 1

# Get status of all servers
echo "Server Status:"
mcp_status

# Execute tools on specific servers
echo -e "\nExecuting health checks:"
mcp_health_check

# Search for tools across all servers
echo -e "\nAvailable RDF tools:"
mcp_search_tools "rdf"
```

## Testing

Run the comprehensive test suite:

```bash
# Run all tests
bash tests/mcp-client-tests.sh

# Run with verbose output
DEBUG_MCP=1 bash tests/mcp-client-tests.sh

# Run specific test
# Edit the test file to run individual tests
```

Test Coverage:
- Configuration loading from multiple sources
- Server connection and health checks
- Tool discovery and caching
- Tool execution with timeout enforcement
- Error handling and recovery
- Function exports and API validation
- Bash compatibility checks

## Performance Characteristics

### SLO Targets
- **Client Initialization**: ≤2s with network connectivity
- **Tool Discovery**: ≤1s (cached), ≤5s (uncached with 3 servers)
- **Tool Execution**: ≤10s (default timeout)
- **Cache Refresh**: 300s (5 minutes) default duration
- **Memory Usage**: <5MB for typical configurations

### Optimization Tips
1. **Use Caching**: Tool lists are cached automatically; clear cache only when needed
2. **Batch Operations**: Execute multiple tools in a single script run
3. **Configure Timeouts**: Adjust `MCP_TIMEOUT` based on expected tool execution time
4. **Disable Unused Servers**: Set `"disabled": true` for servers you don't need

## Troubleshooting

### Problem: "MCP client not initialized"
**Solution**: Ensure `mcp_init_client()` was called successfully before using other functions.

```bash
if ! mcp_init_client; then
    echo "Failed to initialize"
    exit 1
fi
```

### Problem: "Tool not found in any connected server"
**Solution**: Use `mcp_list_tools()` to see available tools, or search with `mcp_search_tools()`.

```bash
mcp_list_tools | grep "tool-name"
```

### Problem: "Timeout querying tools from server"
**Solution**: Increase timeout or check if server is responsive.

```bash
export MCP_TIMEOUT=20
mcp_list_tools
```

### Problem: "No MCP configuration found"
**Solution**: Create `.mcp.json` in project root or `~/.mcp.json` in home directory.

```bash
cat > .mcp.json <<'EOF'
{
  "mcpServers": {
    "bash": {"command": "bash", "disabled": false}
  }
}
EOF
```

### Problem: Server appears unhealthy
**Solution**: Run health check and verify server configuration.

```bash
mcp_health_check
mcp_status
```

## Security Considerations

1. **Command Execution**: Only execute tools from trusted MCP servers
2. **Parameter Validation**: Validate tool parameters before execution
3. **Environment Variables**: Use environment variables for sensitive data (tokens, credentials)
4. **File Permissions**: Protect `.mcp.json` configuration files (should not be world-readable if containing secrets)
5. **Timeout Enforcement**: Always use timeouts to prevent hanging operations

## Logging

Enable debug logging for troubleshooting:

```bash
export DEBUG_MCP=1
bash your-script.sh
```

Debug output shows:
- Configuration file locations and loading
- Server connection attempts
- Tool queries and results
- Cache operations
- Timeout events
- Error details

## Future Enhancements

Planned features for future versions:
- [ ] Connection pooling for frequently used servers
- [ ] Automatic server health monitoring
- [ ] Tool execution metrics and profiling
- [ ] Multi-threaded tool execution
- [ ] Advanced caching strategies (LRU)
- [ ] Tool dependency resolution
- [ ] Server fallback and failover logic

## Support

For issues or questions:
1. Check troubleshooting section above
2. Enable debug logging with `DEBUG_MCP=1`
3. Review test suite for usage examples
4. Check server configuration validity with `mcp_status` and `mcp_health_check`

## License

This module is part of the ggen project and is available under the same license terms.

## Version History

### v1.0.0 (Production-Ready)
- Initial release with full MCP client functionality
- Configuration loading and management
- Tool discovery and execution
- Error handling and retry logic
- Comprehensive testing and documentation
- Performance optimization with caching
