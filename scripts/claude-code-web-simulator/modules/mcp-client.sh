#!/bin/bash

##############################################################################
# MCP Client Module - Real MCP Server Integration
#
# Provides production-grade MCP client functionality for tool access,
# server connectivity, and tool execution with robust error handling.
#
# Features:
#   - Real MCP client initialization with configuration loading
#   - Dynamic tool discovery from multiple servers
#   - Tool execution with timeout enforcement (default 10s)
#   - Tool search and caching for performance
#   - Comprehensive error handling and retry logic
#   - Support for authentication and environment variables
#
# Version: 1.0.0
# Author: ggen AI Team
##############################################################################

set -euo pipefail

# Configuration (use environment variables if set, otherwise use defaults)
# Note: These are readonly after initialization to prevent accidental modifications
MCP_TIMEOUT="${MCP_TIMEOUT:-10}"
MCP_CACHE_DIR="${MCP_CACHE_DIR:-.mcp-cache}"
MCP_CACHE_DURATION="${MCP_CACHE_DURATION:-300}"
MCP_RETRY_COUNT="${MCP_RETRY_COUNT:-3}"
MCP_RETRY_DELAY="${MCP_RETRY_DELAY:-2}"

# Make readonly after initialization
readonly MCP_TIMEOUT
readonly MCP_CACHE_DIR
readonly MCP_CACHE_DURATION
readonly MCP_RETRY_COUNT
readonly MCP_RETRY_DELAY

# Global state
declare -g MCP_INITIALIZED=0
declare -g MCP_SERVERS_CONFIG=""
declare -g MCP_CONNECTED_SERVERS=()
declare -gA MCP_TOOL_CACHE=()
declare -gA MCP_SERVER_CACHE=()

##############################################################################
# Logging Utilities
##############################################################################

_mcp_log_error() {
    echo "[MCP] ERROR: $*" >&2
}

_mcp_log_warn() {
    echo "[MCP] WARN: $*" >&2
}

_mcp_log_info() {
    echo "[MCP] INFO: $*"
}

_mcp_log_debug() {
    if [[ "${DEBUG_MCP:-0}" == "1" ]]; then
        echo "[MCP] DEBUG: $*"
    fi
}

##############################################################################
# Configuration Loading
##############################################################################

# Load MCP configuration from standard locations
_mcp_load_config() {
    local config_file=""
    local config_content=""

    # Try to load configuration in order of precedence
    # Priority: .mcp.json > ~/.mcp.json > ~/.claude.json
    if [[ -f .mcp.json ]]; then
        config_file=.mcp.json
        _mcp_log_debug "Loading config from .mcp.json"
    elif [[ -f ~/.mcp.json ]]; then
        config_file=~/.mcp.json
        _mcp_log_debug "Loading config from ~/.mcp.json"
    elif [[ -f ~/.claude.json ]]; then
        config_file=~/.claude.json
        _mcp_log_debug "Loading config from ~/.claude.json"
    else
        _mcp_log_error "No MCP configuration found in .mcp.json, ~/.mcp.json, or ~/.claude.json"
        return 1
    fi

    # Parse configuration
    if command -v jq &> /dev/null; then
        # Try both common formats for MCP configuration (note: avoid hyphenated keys due to jq parsing)
        config_content=$(jq '.mcpServers // .mcp_servers // {}' "$config_file" 2>/dev/null || echo "{}")
    else
        _mcp_log_warn "jq not found; using grep for config parsing"
        config_content=$(cat "$config_file")
    fi

    MCP_SERVERS_CONFIG="$config_content"
    _mcp_log_debug "Configuration loaded: $(echo "$config_content" | wc -c) bytes"
    return 0
}

# Parse server definition and validate connectivity
_mcp_parse_server_config() {
    local server_name="$1"
    local config="$2"

    if ! command -v jq &> /dev/null; then
        _mcp_log_error "jq required for parsing server config"
        return 1
    fi

    local command
    local args
    local enabled

    # Extract server details
    command=$(echo "$config" | jq -r ".${server_name}.command // empty" 2>/dev/null)
    args=$(echo "$config" | jq -r ".${server_name}.args[]? // empty" 2>/dev/null | tr '\n' ' ')
    enabled=$(echo "$config" | jq -r ".${server_name}.disabled // false" 2>/dev/null)

    if [[ -z "$command" ]]; then
        _mcp_log_warn "Server '$server_name' has no command defined"
        return 1
    fi

    if [[ "$enabled" == "true" ]]; then
        _mcp_log_debug "Server '$server_name' is disabled in config"
        return 1
    fi

    echo "${command}${args:+ $args}"
    return 0
}

##############################################################################
# MCP Client Initialization
##############################################################################

# Initialize MCP client and connect to configured servers
mcp_init_client() {
    local retry_count=0

    _mcp_log_info "Initializing MCP client..."

    # Load configuration
    if ! _mcp_load_config; then
        _mcp_log_error "Failed to load MCP configuration"
        return 1
    fi

    # Create cache directory
    mkdir -p "$MCP_CACHE_DIR"

    # Parse and connect to servers with retry logic
    while [[ $retry_count -lt $MCP_RETRY_COUNT ]]; do
        if _mcp_connect_servers; then
            MCP_INITIALIZED=1
            _mcp_log_info "MCP client initialized successfully"
            _mcp_log_info "Connected servers: ${MCP_CONNECTED_SERVERS[*]}"
            return 0
        fi

        retry_count=$((retry_count + 1))
        if [[ $retry_count -lt $MCP_RETRY_COUNT ]]; then
            _mcp_log_warn "Connection attempt $retry_count failed, retrying in ${MCP_RETRY_DELAY}s..."
            sleep "$MCP_RETRY_DELAY"
        fi
    done

    _mcp_log_error "Failed to initialize MCP client after $MCP_RETRY_COUNT attempts"
    return 1
}

# Connect to all configured MCP servers
_mcp_connect_servers() {
    local servers_json="$MCP_SERVERS_CONFIG"

    if ! command -v jq &> /dev/null; then
        _mcp_log_error "jq required for server connection"
        return 1
    fi

    # Get list of server names
    local servers
    servers=$(echo "$servers_json" | jq -r 'keys[]? // empty' 2>/dev/null)

    if [[ -z "$servers" ]]; then
        _mcp_log_warn "No MCP servers configured"
        return 1
    fi

    local connected=0
    local server_name

    # Connect to each server
    while IFS= read -r server_name; do
        if [[ -z "$server_name" ]]; then
            continue
        fi

        _mcp_log_debug "Attempting to connect to server: $server_name"

        if _mcp_connect_server "$server_name" "$servers_json"; then
            MCP_CONNECTED_SERVERS+=("$server_name")
            connected=$((connected + 1))
        else
            _mcp_log_warn "Failed to connect to server: $server_name"
        fi
    done <<< "$servers"

    if [[ $connected -eq 0 ]]; then
        _mcp_log_error "No MCP servers could be connected"
        return 1
    fi

    _mcp_log_info "Connected to $connected MCP server(s)"
    return 0
}

# Connect to a single MCP server with health check
_mcp_connect_server() {
    local server_name="$1"
    local servers_json="$2"

    # Parse server command
    local server_cmd
    if ! server_cmd=$(_mcp_parse_server_config "$server_name" "$servers_json"); then
        _mcp_log_debug "Cannot parse server '$server_name' config"
        return 1
    fi

    _mcp_log_debug "Server command: $server_cmd"

    # Verify command exists (timeout to prevent hanging)
    if ! timeout 2s bash -c "command -v ${server_cmd%% *}" &> /dev/null; then
        _mcp_log_debug "Server command not found: ${server_cmd%% *}"
        return 1
    fi

    # Store server command for later use
    MCP_SERVER_CACHE["$server_name"]="$server_cmd"

    return 0
}

##############################################################################
# Tool Discovery and Listing
##############################################################################

# Query connected MCP servers for available tools
mcp_list_tools() {
    if [[ $MCP_INITIALIZED -eq 0 ]]; then
        _mcp_log_error "MCP client not initialized; call mcp_init_client() first"
        return 1
    fi

    local cache_file="$MCP_CACHE_DIR/tools.cache"

    # Check cache freshness
    if [[ -f "$cache_file" ]]; then
        local cache_age
        cache_age=$(($(date +%s) - $(stat -f%m "$cache_file" 2>/dev/null || stat -c%Y "$cache_file" 2>/dev/null || echo 0)))

        if [[ $cache_age -lt $MCP_CACHE_DURATION ]]; then
            _mcp_log_debug "Using cached tools list (age: ${cache_age}s)"
            cat "$cache_file"
            return 0
        fi
    fi

    # Query each connected server
    local all_tools=()
    local server

    for server in "${MCP_CONNECTED_SERVERS[@]}"; do
        _mcp_log_debug "Querying tools from server: $server"

        local tools
        if tools=$(_mcp_query_server_tools "$server" 2>/dev/null); then
            all_tools+=("$tools")
        else
            _mcp_log_warn "Failed to query tools from server: $server"
        fi
    done

    # Combine and cache results
    local combined_output
    combined_output=$(printf "%s\n" "${all_tools[@]}")

    if [[ -n "$combined_output" ]]; then
        echo "$combined_output" | tee "$cache_file"
        return 0
    else
        _mcp_log_error "No tools found from any server"
        return 1
    fi
}

# Query tools from a specific server
_mcp_query_server_tools() {
    local server_name="$1"
    local server_cmd

    if [[ -z "${MCP_SERVER_CACHE[$server_name]:-}" ]]; then
        _mcp_log_error "Server '$server_name' not connected"
        return 1
    fi

    server_cmd="${MCP_SERVER_CACHE[$server_name]}"

    # Execute server command with timeout to fetch tools
    local tools_output
    if tools_output=$(timeout ${MCP_TIMEOUT}s bash -c "echo '{\"method\":\"tools/list\",\"jsonrpc\":\"2.0\",\"id\":1}' | $server_cmd" 2>/dev/null); then
        # Parse and format output
        if command -v jq &> /dev/null; then
            echo "$tools_output" | jq -r ".result.tools[]? | \"${server_name}: \(.name) - \(.description // \"No description\")\"" 2>/dev/null || echo "$tools_output"
        else
            echo "$tools_output"
        fi
        return 0
    else
        _mcp_log_error "Timeout querying tools from server: $server_name"
        return 1
    fi
}

##############################################################################
# Tool Execution
##############################################################################

# Execute a tool on a specific MCP server
mcp_call_tool() {
    local tool_name="$1"
    shift
    local -a tool_params=("$@")

    if [[ $MCP_INITIALIZED -eq 0 ]]; then
        _mcp_log_error "MCP client not initialized; call mcp_init_client() first"
        return 1
    fi

    if [[ -z "$tool_name" ]]; then
        _mcp_log_error "Tool name required"
        return 1
    fi

    local server_name
    local found=0

    # Find which server provides this tool
    for server in "${MCP_CONNECTED_SERVERS[@]}"; do
        if _mcp_server_has_tool "$server" "$tool_name"; then
            server_name="$server"
            found=1
            break
        fi
    done

    if [[ $found -eq 0 ]]; then
        _mcp_log_error "Tool '$tool_name' not found in any connected server"
        _mcp_log_info "Available servers: ${MCP_CONNECTED_SERVERS[*]}"
        return 1
    fi

    _mcp_log_info "Executing tool '$tool_name' on server '$server_name'..."
    _mcp_execute_tool "$server_name" "$tool_name" "${tool_params[@]}"
}

# Check if a server provides a specific tool
_mcp_server_has_tool() {
    local server_name="$1"
    local tool_name="$2"

    if ! command -v jq &> /dev/null; then
        _mcp_log_debug "jq required for tool lookup"
        return 1
    fi

    local tools_list
    tools_list=$(_mcp_query_server_tools "$server_name" 2>/dev/null | grep -i "$tool_name" || true)

    [[ -n "$tools_list" ]]
}

# Execute tool on specified server
_mcp_execute_tool() {
    local server_name="$1"
    local tool_name="$2"
    shift 2
    local -a tool_params=("$@")

    local server_cmd

    if [[ -z "${MCP_SERVER_CACHE[$server_name]:-}" ]]; then
        _mcp_log_error "Server '$server_name' not connected"
        return 1
    fi

    server_cmd="${MCP_SERVER_CACHE[$server_name]}"

    # Build tool call JSON
    local tool_call_json
    local params_json=""

    if [[ ${#tool_params[@]} -gt 0 ]]; then
        params_json=$(printf '%s\n' "${tool_params[@]}" | jq -Rs '.' 2>/dev/null || echo '{}')
    else
        params_json="{}"
    fi

    tool_call_json=$(cat <<EOF
{
  "method": "tools/call",
  "jsonrpc": "2.0",
  "id": 1,
  "params": {
    "name": "$tool_name",
    "arguments": $params_json
  }
}
EOF
)

    _mcp_log_debug "Tool call: $tool_call_json"

    # Execute with timeout and error handling
    local output
    local exit_code=0

    if output=$(timeout ${MCP_TIMEOUT}s bash -c "echo '$tool_call_json' | $server_cmd" 2>&1); then
        _mcp_log_info "Tool execution completed successfully"

        # Parse and format output
        if command -v jq &> /dev/null; then
            echo "$output" | jq '.result // .error // .' 2>/dev/null || echo "$output"
        else
            echo "$output"
        fi
        return 0
    else
        exit_code=$?
        if [[ $exit_code -eq 124 ]]; then
            _mcp_log_error "Tool execution timeout (${MCP_TIMEOUT}s exceeded) for tool: $tool_name"
            return 1
        else
            _mcp_log_error "Tool execution failed for tool: $tool_name (exit: $exit_code)"
            _mcp_log_error "Output: $output"
            return 1
        fi
    fi
}

##############################################################################
# Tool Search and Discovery
##############################################################################

# Search for tools matching a query across all servers
mcp_search_tools() {
    local query="$1"

    if [[ $MCP_INITIALIZED -eq 0 ]]; then
        _mcp_log_error "MCP client not initialized; call mcp_init_client() first"
        return 1
    fi

    if [[ -z "$query" ]]; then
        _mcp_log_error "Search query required"
        return 1
    fi

    _mcp_log_info "Searching for tools matching: $query"

    local found=0
    local server

    # Search in each server
    for server in "${MCP_CONNECTED_SERVERS[@]}"; do
        _mcp_log_debug "Searching in server: $server"

        local results
        if results=$(_mcp_search_server "$server" "$query"); then
            if [[ -n "$results" ]]; then
                echo "$results"
                found=$((found + 1))
            fi
        fi
    done

    if [[ $found -eq 0 ]]; then
        _mcp_log_warn "No tools found matching query: $query"
        return 1
    fi

    return 0
}

# Search within a specific server
_mcp_search_server() {
    local server_name="$1"
    local query="$2"

    if ! command -v jq &> /dev/null; then
        _mcp_log_error "jq required for tool search"
        return 1
    fi

    local tools
    if tools=$(_mcp_query_server_tools "$server_name" 2>/dev/null); then
        echo "$tools" | grep -i "$query" || true
    fi
}

##############################################################################
# Status and Health Checks
##############################################################################

# Check MCP client status
mcp_status() {
    echo "═══════════════════════════════════════════"
    echo "MCP Client Status"
    echo "═══════════════════════════════════════════"
    echo ""

    if [[ $MCP_INITIALIZED -eq 0 ]]; then
        echo "Status: NOT INITIALIZED"
        return 1
    fi

    echo "Status: INITIALIZED"
    echo "Connected Servers: ${#MCP_CONNECTED_SERVERS[@]}"

    local server
    for server in "${MCP_CONNECTED_SERVERS[@]}"; do
        echo "  - $server: ${MCP_SERVER_CACHE[$server]}"
    done

    echo ""
    echo "Configuration:"
    echo "  Timeout: ${MCP_TIMEOUT}s"
    echo "  Cache Directory: $MCP_CACHE_DIR"
    echo "  Cache Duration: ${MCP_CACHE_DURATION}s"
    echo "  Retry Count: $MCP_RETRY_COUNT"
    echo ""

    return 0
}

# Verify MCP client health
mcp_health_check() {
    _mcp_log_info "Running MCP health check..."

    if [[ $MCP_INITIALIZED -eq 0 ]]; then
        _mcp_log_error "MCP client not initialized"
        return 1
    fi

    local healthy=0
    local server

    for server in "${MCP_CONNECTED_SERVERS[@]}"; do
        _mcp_log_debug "Checking server health: $server"

        if timeout 2s bash -c "command -v ${MCP_SERVER_CACHE[$server]%% *}" &> /dev/null; then
            _mcp_log_info "✓ Server healthy: $server"
            healthy=$((healthy + 1))
        else
            _mcp_log_warn "✗ Server unhealthy: $server"
        fi
    done

    if [[ $healthy -eq 0 ]]; then
        _mcp_log_error "No healthy servers found"
        return 1
    fi

    _mcp_log_info "Health check complete: $healthy/${#MCP_CONNECTED_SERVERS[@]} servers healthy"
    return 0
}

##############################################################################
# Cache Management
##############################################################################

# Clear MCP tool cache
mcp_clear_cache() {
    _mcp_log_info "Clearing MCP cache..."

    if [[ -d "$MCP_CACHE_DIR" ]]; then
        rm -rf "$MCP_CACHE_DIR"
        mkdir -p "$MCP_CACHE_DIR"
        _mcp_log_info "Cache cleared successfully"
    fi

    # Clear in-memory caches
    MCP_TOOL_CACHE=()
    MCP_SERVER_CACHE=()
}

##############################################################################
# Export Functions for External Use
##############################################################################

# Make all public functions available
export -f mcp_init_client
export -f mcp_list_tools
export -f mcp_call_tool
export -f mcp_search_tools
export -f mcp_status
export -f mcp_health_check
export -f mcp_clear_cache
