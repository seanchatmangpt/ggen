#!/usr/bin/env bash

# GGen MCP Tool Testing Script
# Tests MCP protocol communication and tool invocations

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

info() { echo -e "${BLUE}[INFO]${NC} $1"; }
success() { echo -e "${GREEN}[✓]${NC} $1"; }
warn() { echo -e "${YELLOW}[!]${NC} $1"; }
error() { echo -e "${RED}[✗]${NC} $1"; }

# Find ggen-mcp binary
find_binary() {
    local script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    local ggen_root="$(cd "$script_dir/../.." && pwd)"
    local binary="$ggen_root/target/release/ggen-mcp"

    if [[ ! -f "$binary" ]]; then
        binary="$ggen_root/target/debug/ggen-mcp"
    fi

    if [[ ! -f "$binary" ]]; then
        error "ggen-mcp binary not found. Run: cargo build --release"
        exit 1
    fi

    echo "$binary"
}

# Send JSON-RPC request
send_request() {
    local binary=$1
    local method=$2
    local params=${3:-"{}"}
    local id=${4:-1}

    local request=$(cat <<EOF
{
  "jsonrpc": "2.0",
  "id": $id,
  "method": "$method",
  "params": $params
}
EOF
)

    echo "$request" | "$binary" 2>/dev/null
}

# Test 1: Protocol handshake
test_initialize() {
    local binary=$1
    info "Testing protocol initialization..."

    local params=$(cat <<'EOF'
{
  "protocolVersion": "2024-11-05",
  "capabilities": {},
  "clientInfo": {
    "name": "test-client",
    "version": "1.0.0"
  }
}
EOF
)

    local response=$(send_request "$binary" "initialize" "$params")

    if echo "$response" | grep -q '"protocolVersion"'; then
        success "Initialize handshake successful"
        return 0
    else
        error "Initialize failed: $response"
        return 1
    fi
}

# Test 2: List tools
test_list_tools() {
    local binary=$1
    info "Testing tools/list..."

    local response=$(send_request "$binary" "tools/list" "{}")

    if echo "$response" | grep -q '"tools"'; then
        local tool_count=$(echo "$response" | grep -o '"name"' | wc -l)
        success "Listed $tool_count tools"

        # Extract tool names
        echo "$response" | grep -o '"name":"[^"]*"' | cut -d'"' -f4 | while read -r tool; do
            echo "  - $tool"
        done
        return 0
    else
        error "tools/list failed: $response"
        return 1
    fi
}

# Test 3: Call specific tool
test_call_tool() {
    local binary=$1
    local tool_name=$2
    local tool_params=${3:-"{}"}

    info "Testing tools/call: $tool_name..."

    local params=$(cat <<EOF
{
  "name": "$tool_name",
  "arguments": $tool_params
}
EOF
)

    local response=$(send_request "$binary" "tools/call" "$params")

    if echo "$response" | grep -q '"content"'; then
        success "$tool_name executed successfully"
        echo "Response preview:"
        echo "$response" | head -c 500
        echo "..."
        return 0
    else
        error "$tool_name failed: $response"
        return 1
    fi
}

# Test 4: Market tools
test_market_tools() {
    local binary=$1

    info "Testing marketplace tools..."

    # Test market_list
    test_call_tool "$binary" "market_list" '{"category": "web"}' || true

    # Test market_search
    test_call_tool "$binary" "market_search" '{"query": "react", "limit": 5}' || true

    # Test market_cache_status
    test_call_tool "$binary" "market_cache_status" '{}' || true
}

# Test 5: Graph tools
test_graph_tools() {
    local binary=$1

    info "Testing graph tools..."

    # Test graph_query with simple SELECT query
    local sparql='SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 5'
    test_call_tool "$binary" "graph_query" "{\"sparql\": \"$sparql\"}" || true
}

# Test 6: AI tools
test_ai_tools() {
    local binary=$1

    info "Testing AI tools..."

    # Test ai_list_providers
    test_call_tool "$binary" "ai_list_providers" '{}' || true

    # Only test generation if API key is available
    if [[ -n "${ANTHROPIC_API_KEY:-}" || -n "${OPENAI_API_KEY:-}" ]]; then
        info "API key found, testing AI generation..."
        test_call_tool "$binary" "ai_generate_template" \
            '{"description": "A simple Python script", "template_type": "script"}' || true
    else
        warn "No API key found, skipping AI generation tests"
        echo "Set ANTHROPIC_API_KEY or OPENAI_API_KEY to test AI tools"
    fi
}

# Test 7: Project tools
test_project_tools() {
    local binary=$1

    info "Testing project tools..."

    # Test project_plan (dry run)
    test_call_tool "$binary" "project_plan" \
        '{"template": "simple-rust", "vars": {"project_name": "test"}}' || true
}

# Run all tests
run_all_tests() {
    local binary=$(find_binary)
    local passed=0
    local failed=0

    info "Starting MCP tool tests"
    info "Binary: $binary"
    echo ""

    # Run tests
    if test_initialize "$binary"; then ((passed++)); else ((failed++)); fi
    echo ""

    if test_list_tools "$binary"; then ((passed++)); else ((failed++)); fi
    echo ""

    test_market_tools "$binary"
    echo ""

    test_graph_tools "$binary"
    echo ""

    test_ai_tools "$binary"
    echo ""

    test_project_tools "$binary"
    echo ""

    # Summary
    echo "=========================================="
    echo "Test Summary"
    echo "=========================================="
    success "Passed: $passed"
    if [[ $failed -gt 0 ]]; then
        error "Failed: $failed"
    fi
    echo ""

    if [[ $failed -eq 0 ]]; then
        success "All critical tests passed! ✨"
        echo ""
        echo "Your ggen-mcp server is ready to use."
        echo "Add it to Claude Desktop or Cline to start using the tools."
        return 0
    else
        warn "Some tests failed, but the server may still work"
        echo "Check the errors above for details"
        return 1
    fi
}

# Interactive mode
interactive_mode() {
    local binary=$(find_binary)

    echo "GGen MCP Interactive Tester"
    echo "Binary: $binary"
    echo ""
    echo "Commands:"
    echo "  list              - List all tools"
    echo "  call <tool>       - Call a tool (will prompt for params)"
    echo "  test <tool>       - Test specific tool"
    echo "  json <file>       - Send custom JSON-RPC request"
    echo "  quit              - Exit"
    echo ""

    while true; do
        read -p "> " cmd args

        case "$cmd" in
            list)
                send_request "$binary" "tools/list" "{}" | jq -r '.result.tools[].name' 2>/dev/null || \
                    send_request "$binary" "tools/list" "{}"
                ;;
            call)
                echo "Enter parameters (JSON):"
                read -p "params> " params
                test_call_tool "$binary" "$args" "$params"
                ;;
            test)
                case "$args" in
                    market) test_market_tools "$binary" ;;
                    graph) test_graph_tools "$binary" ;;
                    ai) test_ai_tools "$binary" ;;
                    project) test_project_tools "$binary" ;;
                    *) test_call_tool "$binary" "$args" "{}" ;;
                esac
                ;;
            json)
                if [[ -f "$args" ]]; then
                    cat "$args" | "$binary" 2>/dev/null
                else
                    echo "File not found: $args"
                fi
                ;;
            quit|exit)
                echo "Goodbye!"
                break
                ;;
            *)
                echo "Unknown command: $cmd"
                ;;
        esac
        echo ""
    done
}

# Parse command line
case "${1:-all}" in
    all)
        run_all_tests
        ;;
    interactive|i)
        interactive_mode
        ;;
    list)
        binary=$(find_binary)
        test_list_tools "$binary"
        ;;
    market)
        binary=$(find_binary)
        test_market_tools "$binary"
        ;;
    graph)
        binary=$(find_binary)
        test_graph_tools "$binary"
        ;;
    ai)
        binary=$(find_binary)
        test_ai_tools "$binary"
        ;;
    project)
        binary=$(find_binary)
        test_project_tools "$binary"
        ;;
    *)
        echo "Usage: $0 [all|interactive|list|market|graph|ai|project]"
        echo ""
        echo "Examples:"
        echo "  $0              # Run all tests"
        echo "  $0 interactive  # Interactive mode"
        echo "  $0 market       # Test marketplace tools only"
        exit 1
        ;;
esac
