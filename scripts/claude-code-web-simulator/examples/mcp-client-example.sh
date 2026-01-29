#!/bin/bash

##############################################################################
# MCP Client Integration Example
#
# Demonstrates production-ready MCP client usage for tool discovery,
# execution, and error handling with real MCP servers.
#
# Usage: ./mcp-client-example.sh [COMMAND]
# Commands:
#   initialize       - Initialize MCP client and show status
#   discover         - Discover and list available tools
#   execute          - Execute example tools
#   search           - Search for specific tools
#   health           - Run health checks
#   all              - Run all examples
#
# Version: 1.0.0
# Author: ggen AI Team
##############################################################################

set -euo pipefail

# Configuration
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly MODULES_DIR="$(cd "$SCRIPT_DIR/../modules" && pwd)"
readonly PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../../" && pwd)"

# Colors for output
readonly GREEN='\033[0;32m'
readonly RED='\033[0;31m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m' # No Color

##############################################################################
# Helper Functions
##############################################################################

print_header() {
    echo ""
    echo -e "${BLUE}═══════════════════════════════════════════${NC}"
    echo -e "${BLUE}$1${NC}"
    echo -e "${BLUE}═══════════════════════════════════════════${NC}"
    echo ""
}

print_success() {
    echo -e "${GREEN}✓${NC} $1"
}

print_error() {
    echo -e "${RED}✗${NC} $1" >&2
}

print_info() {
    echo -e "${YELLOW}ℹ${NC} $1"
}

##############################################################################
# MCP Client Integration
##############################################################################

# Load MCP client module
load_mcp_client() {
    if [[ ! -f "$MODULES_DIR/mcp-client.sh" ]]; then
        print_error "MCP client module not found: $MODULES_DIR/mcp-client.sh"
        return 1
    fi

    if ! source "$MODULES_DIR/mcp-client.sh"; then
        print_error "Failed to load MCP client module"
        return 1
    fi

    return 0
}

##############################################################################
# Example 1: Initialize MCP Client
##############################################################################

example_initialize() {
    print_header "Example 1: Initialize MCP Client"

    print_info "Loading MCP client module..."

    if ! load_mcp_client; then
        print_error "Failed to load MCP client"
        return 1
    fi

    print_info "Initializing MCP client..."

    if ! mcp_init_client; then
        print_error "Failed to initialize MCP client"
        return 1
    fi

    print_success "MCP client initialized successfully"

    # Display status
    print_info "Current MCP client status:"
    mcp_status

    return 0
}

##############################################################################
# Example 2: Tool Discovery
##############################################################################

example_discover() {
    print_header "Example 2: Tool Discovery"

    if ! load_mcp_client; then
        return 1
    fi

    print_info "Initializing MCP client..."
    if ! mcp_init_client; then
        print_error "Failed to initialize MCP client"
        return 1
    fi

    print_info "Discovering available tools from all servers..."
    echo ""

    if tools=$(mcp_list_tools 2>&1); then
        echo "$tools"
        print_success "Tool discovery completed"
    else
        print_error "Failed to discover tools"
        return 1
    fi

    return 0
}

##############################################################################
# Example 3: Execute Tools
##############################################################################

example_execute() {
    print_header "Example 3: Execute Example Tools"

    if ! load_mcp_client; then
        return 1
    fi

    print_info "Initializing MCP client..."
    if ! mcp_init_client; then
        print_error "Failed to initialize MCP client"
        return 1
    fi

    # Example 1: List tools
    print_info "Listing available tools..."
    if tools=$(mcp_list_tools 2>&1); then
        echo "$tools" | head -10
        print_success "Tools listed successfully"
    else
        print_warn "Could not list tools (server may not support this)"
    fi

    # Example 2: Search for specific tools
    print_info "Searching for 'bash' tools..."
    if bash_tools=$(mcp_search_tools "bash" 2>&1); then
        echo "$bash_tools"
        print_success "Search completed"
    else
        print_info "No bash tools found"
    fi

    # Example 3: Check server health
    print_info "Checking server health..."
    if mcp_health_check; then
        print_success "All servers are healthy"
    else
        print_warn "Some servers are not responding"
    fi

    return 0
}

##############################################################################
# Example 4: Tool Search
##############################################################################

example_search() {
    print_header "Example 4: Tool Search Examples"

    if ! load_mcp_client; then
        return 1
    fi

    print_info "Initializing MCP client..."
    if ! mcp_init_client; then
        print_error "Failed to initialize MCP client"
        return 1
    fi

    # Define search patterns
    local -a search_patterns=("git" "rdf" "bash" "parse" "query")

    for pattern in "${search_patterns[@]}"; do
        print_info "Searching for '$pattern' tools..."

        if results=$(mcp_search_tools "$pattern" 2>&1); then
            echo "$results"
            print_success "Found tools matching '$pattern'"
        else
            print_info "No tools found matching '$pattern'"
        fi

        echo ""
    done

    return 0
}

##############################################################################
# Example 5: Health Checks
##############################################################################

example_health() {
    print_header "Example 5: Health Checks and Status"

    if ! load_mcp_client; then
        return 1
    fi

    print_info "Initializing MCP client..."
    if ! mcp_init_client; then
        print_error "Failed to initialize MCP client"
        return 1
    fi

    print_info "Running health checks..."
    echo ""

    if mcp_health_check; then
        print_success "All servers are healthy"
    else
        print_error "Some servers are not responding"
    fi

    echo ""

    print_info "Current status:"
    mcp_status

    return 0
}

##############################################################################
# Example 6: Advanced Pattern - Multi-Server Orchestration
##############################################################################

example_advanced() {
    print_header "Example 6: Advanced Multi-Server Orchestration"

    if ! load_mcp_client; then
        return 1
    fi

    print_info "Advanced Example: Tool inventory and metrics"
    echo ""

    print_info "Step 1: Initialize client..."
    if ! mcp_init_client; then
        print_error "Failed to initialize"
        return 1
    fi
    print_success "Initialized"

    print_info "Step 2: Get tool inventory..."
    local tool_count=0
    if tools=$(mcp_list_tools 2>&1); then
        tool_count=$(echo "$tools" | wc -l)
        print_success "Found $tool_count tools"
    else
        print_warn "Could not enumerate tools"
    fi

    print_info "Step 3: Search for critical tools..."
    local critical_patterns=("git" "rdf" "query")
    local found=0

    for pattern in "${critical_patterns[@]}"; do
        if mcp_search_tools "$pattern" &>/dev/null; then
            found=$((found + 1))
        fi
    done

    print_success "Found $found/$((${#critical_patterns[@]})) critical tool categories"

    print_info "Step 4: Generate summary report..."
    echo ""
    echo "═══════════════════════════════════════════"
    echo "MCP Client Summary Report"
    echo "═══════════════════════════════════════════"
    echo ""
    mcp_status
    echo ""

    return 0
}

##############################################################################
# Main
##############################################################################

main() {
    local command="${1:-all}"

    case "$command" in
        initialize)
            example_initialize
            ;;
        discover)
            example_discover
            ;;
        execute)
            example_execute
            ;;
        search)
            example_search
            ;;
        health)
            example_health
            ;;
        advanced)
            example_advanced
            ;;
        all)
            example_initialize && echo "" && \
            example_discover && echo "" && \
            example_execute && echo "" && \
            example_search && echo "" && \
            example_health
            ;;
        *)
            echo "MCP Client Integration Examples"
            echo ""
            echo "Usage: $0 [COMMAND]"
            echo ""
            echo "Commands:"
            echo "  initialize       - Initialize MCP client and show status"
            echo "  discover         - Discover and list available tools"
            echo "  execute          - Execute example tools"
            echo "  search           - Search for specific tools"
            echo "  health           - Run health checks"
            echo "  advanced         - Advanced multi-server orchestration"
            echo "  all              - Run all examples (default)"
            echo ""
            return 1
            ;;
    esac
}

# Run main if script is executed directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
