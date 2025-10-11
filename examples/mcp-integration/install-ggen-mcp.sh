#!/usr/bin/env bash

# GGen MCP Installation Script
# Builds ggen-mcp and configures it for Claude Desktop or Cline

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Detect MCP client
detect_client() {
    local client=""

    # Check for Claude Desktop
    if [[ "$OSTYPE" == "darwin"* ]]; then
        if [[ -d "/Applications/Claude.app" ]]; then
            client="claude-desktop"
        fi
    elif [[ "$OSTYPE" == "msys" || "$OSTYPE" == "win32" ]]; then
        if [[ -d "$LOCALAPPDATA/Programs/Claude" ]]; then
            client="claude-desktop"
        fi
    fi

    # Check for Cline (VS Code)
    if command -v code &> /dev/null; then
        if code --list-extensions | grep -q "saoudrizwan.claude-dev"; then
            client="cline"
        fi
    fi

    echo "$client"
}

# Get config file path
get_config_path() {
    local client=$1
    local config_path=""

    if [[ "$client" == "claude-desktop" ]]; then
        if [[ "$OSTYPE" == "darwin"* ]]; then
            config_path="$HOME/Library/Application Support/Claude/claude_desktop_config.json"
        elif [[ "$OSTYPE" == "msys" || "$OSTYPE" == "win32" ]]; then
            config_path="$APPDATA/Claude/claude_desktop_config.json"
        fi
    elif [[ "$client" == "cline" ]]; then
        if [[ "$OSTYPE" == "darwin"* ]]; then
            config_path="$HOME/Library/Application Support/Code/User/settings.json"
        elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
            config_path="$HOME/.config/Code/User/settings.json"
        elif [[ "$OSTYPE" == "msys" || "$OSTYPE" == "win32" ]]; then
            config_path="$APPDATA/Code/User/settings.json"
        fi
    fi

    echo "$config_path"
}

# Main installation
main() {
    info "GGen MCP Installation"
    echo ""

    # Find ggen repository root
    SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    GGEN_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
    MCP_DIR="$GGEN_ROOT/ggen-mcp"

    info "GGen root: $GGEN_ROOT"

    # Verify ggen-mcp exists
    if [[ ! -d "$MCP_DIR" ]]; then
        error "ggen-mcp directory not found at $MCP_DIR"
        exit 1
    fi

    # Step 1: Build ggen-mcp
    info "Building ggen-mcp in release mode..."
    cd "$MCP_DIR"

    if ! cargo build --release; then
        error "Failed to build ggen-mcp"
        exit 1
    fi

    BINARY_PATH="$GGEN_ROOT/target/release/ggen-mcp"

    if [[ ! -f "$BINARY_PATH" ]]; then
        error "Binary not found at $BINARY_PATH"
        exit 1
    fi

    success "Built ggen-mcp at $BINARY_PATH"

    # Step 2: Detect MCP client
    info "Detecting MCP client..."
    CLIENT=$(detect_client)

    if [[ -z "$CLIENT" ]]; then
        warn "No MCP client detected (Claude Desktop or Cline)"
        echo ""
        echo "Please install one of:"
        echo "  - Claude Desktop: https://claude.ai/download"
        echo "  - Cline (VS Code): https://marketplace.visualstudio.com/items?itemName=saoudrizwan.claude-dev"
        echo ""
        echo "Then run this script again, or manually configure:"
        echo "  Binary: $BINARY_PATH"
        exit 0
    fi

    success "Detected $CLIENT"

    # Step 3: Get or create config
    CONFIG_PATH=$(get_config_path "$CLIENT")

    if [[ -z "$CONFIG_PATH" ]]; then
        error "Could not determine config path for $CLIENT"
        exit 1
    fi

    info "Config path: $CONFIG_PATH"

    # Create config directory if needed
    CONFIG_DIR=$(dirname "$CONFIG_PATH")
    mkdir -p "$CONFIG_DIR"

    # Get API key (optional but recommended for AI tools)
    ANTHROPIC_KEY="${ANTHROPIC_API_KEY:-}"
    OPENAI_KEY="${OPENAI_API_KEY:-}"

    if [[ -z "$ANTHROPIC_KEY" && -z "$OPENAI_KEY" ]]; then
        warn "No AI API keys found in environment"
        echo "Set ANTHROPIC_API_KEY or OPENAI_API_KEY to use AI tools"
        echo ""
        read -p "Enter Anthropic API key (or press Enter to skip): " ANTHROPIC_KEY
    fi

    # Step 4: Configure client
    if [[ "$CLIENT" == "claude-desktop" ]]; then
        # Configure Claude Desktop
        if [[ -f "$CONFIG_PATH" ]]; then
            info "Updating existing Claude Desktop config..."
            # Backup existing config
            cp "$CONFIG_PATH" "$CONFIG_PATH.backup.$(date +%s)"
        else
            info "Creating new Claude Desktop config..."
            echo '{}' > "$CONFIG_PATH"
        fi

        # Use jq if available, otherwise manual JSON
        if command -v jq &> /dev/null; then
            # Build env object
            ENV_JSON='{"GGEN_MCP_LOG":"info"}'
            if [[ -n "$ANTHROPIC_KEY" ]]; then
                ENV_JSON=$(echo "$ENV_JSON" | jq --arg key "$ANTHROPIC_KEY" '.ANTHROPIC_API_KEY = $key')
            fi
            if [[ -n "$OPENAI_KEY" ]]; then
                ENV_JSON=$(echo "$ENV_JSON" | jq --arg key "$OPENAI_KEY" '.OPENAI_API_KEY = $key')
            fi

            # Update config
            jq --arg cmd "$BINARY_PATH" --argjson env "$ENV_JSON" \
                '.mcpServers.ggen = {command: $cmd, args: [], env: $env}' \
                "$CONFIG_PATH" > "$CONFIG_PATH.tmp"
            mv "$CONFIG_PATH.tmp" "$CONFIG_PATH"
        else
            # Manual JSON construction
            cat > "$CONFIG_PATH" <<EOF
{
  "mcpServers": {
    "ggen": {
      "command": "$BINARY_PATH",
      "args": [],
      "env": {
        "GGEN_MCP_LOG": "info"$(if [[ -n "$ANTHROPIC_KEY" ]]; then echo ",
        \"ANTHROPIC_API_KEY\": \"$ANTHROPIC_KEY\""; fi)$(if [[ -n "$OPENAI_KEY" ]]; then echo ",
        \"OPENAI_API_KEY\": \"$OPENAI_KEY\""; fi)
      }
    }
  }
}
EOF
        fi

        success "Claude Desktop configured"
        echo ""
        warn "Please restart Claude Desktop for changes to take effect"

    elif [[ "$CLIENT" == "cline" ]]; then
        # Configure Cline
        info "Configuring Cline..."

        # Build MCP server config
        MCP_CONFIG=$(cat <<EOF
{
  "ggen": {
    "command": "$BINARY_PATH",
    "args": [],
    "env": {
      "GGEN_MCP_LOG": "info"$(if [[ -n "$ANTHROPIC_KEY" ]]; then echo ",
      \"ANTHROPIC_API_KEY\": \"$ANTHROPIC_KEY\""; fi)$(if [[ -n "$OPENAI_KEY" ]]; then echo ",
      \"OPENAI_API_KEY\": \"$OPENAI_KEY\""; fi)
    }
  }
}
EOF
)

        echo ""
        echo "Add this to your VS Code settings (File > Preferences > Settings):"
        echo "Search for 'Cline: MCP Servers' or add to settings.json:"
        echo ""
        echo "$MCP_CONFIG"
        echo ""
        warn "Then reload VS Code (Ctrl/Cmd + Shift + P > Reload Window)"
    fi

    # Step 5: Test connection
    echo ""
    info "Testing MCP connection..."

    # Simple test: list tools
    if echo '{"jsonrpc":"2.0","id":1,"method":"tools/list"}' | "$BINARY_PATH" 2>/dev/null | grep -q "project_gen"; then
        success "MCP server responds to tool list request"
    else
        warn "Could not verify MCP server response (this may be normal)"
    fi

    # Step 6: Summary
    echo ""
    success "Installation complete!"
    echo ""
    echo "Next steps:"
    echo "  1. Restart your MCP client ($CLIENT)"
    echo "  2. Test by asking Claude: 'List available ggen MCP tools'"
    echo "  3. Try example conversations in: $SCRIPT_DIR/example-conversations/"
    echo ""
    echo "Available tools: 25+"
    echo "  - 4 project tools (gen, plan, apply, diff)"
    echo "  - 8 marketplace tools (search, install, info, etc.)"
    echo "  - 3 graph tools (query, load, export)"
    echo "  - 2 template tools (create, validate)"
    echo "  - 1 hook tool (register)"
    echo "  - 7 AI tools (generate_*, validate, list_providers)"
    echo ""
    echo "Documentation: $SCRIPT_DIR/README.md"
    echo "Test script: $SCRIPT_DIR/test-mcp-tools.sh"
    echo ""
    info "Happy generating! 🚀"
}

# Run main function
main "$@"
