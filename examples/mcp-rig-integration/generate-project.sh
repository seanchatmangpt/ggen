#!/bin/bash
# Generate MCP + Rig Integration Project
# This script uses ggen to generate a complete Rust project with MCP integration

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Print colored message
print_msg() {
    local color=$1
    local message=$2
    echo -e "${color}${message}${NC}"
}

# Print banner
print_banner() {
    print_msg "$BLUE" "╔════════════════════════════════════════════════════════════════╗"
    print_msg "$BLUE" "║       MCP + Rig Integration Project Generator                 ║"
    print_msg "$BLUE" "║       Generate production-ready AI agents with dynamic tools  ║"
    print_msg "$BLUE" "╚════════════════════════════════════════════════════════════════╝"
    echo
}

# Show usage
usage() {
    echo "Usage: $0 [OPTIONS] <project-name>"
    echo
    echo "Options:"
    echo "  -d, --deepseek        Enable DeepSeek provider (default: true)"
    echo "  -o, --openai          Enable OpenAI provider (default: false)"
    echo "  -c, --cohere          Enable Cohere provider (default: false)"
    echo "  -l, --log-dir DIR     Set log directory (default: logs)"
    echo "  -h, --help            Show this help message"
    echo
    echo "Example:"
    echo "  $0 my-mcp-agent"
    echo "  $0 --openai --cohere my-agent"
    exit 1
}

# Default values
PROJECT_NAME=""
DEEPSEEK_SUPPORT=true
OPENAI_SUPPORT=false
COHERE_SUPPORT=false
LOG_DIR="logs"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -d|--deepseek)
            DEEPSEEK_SUPPORT=true
            shift
            ;;
        -o|--openai)
            OPENAI_SUPPORT=true
            shift
            ;;
        -c|--cohere)
            COHERE_SUPPORT=true
            shift
            ;;
        -l|--log-dir)
            LOG_DIR="$2"
            shift 2
            ;;
        -h|--help)
            usage
            ;;
        *)
            if [ -z "$PROJECT_NAME" ]; then
                PROJECT_NAME="$1"
            else
                print_msg "$RED" "Error: Unknown option $1"
                usage
            fi
            shift
            ;;
    esac
done

# Check if project name is provided
if [ -z "$PROJECT_NAME" ]; then
    print_msg "$RED" "Error: Project name is required"
    usage
fi

# Print banner
print_banner

# Check if ggen is available
if ! command -v ggen &> /dev/null; then
    print_msg "$RED" "Error: ggen is not installed"
    print_msg "$YELLOW" "Please install ggen first: cargo install ggen"
    exit 1
fi

print_msg "$GREEN" "🚀 Generating project: $PROJECT_NAME"
echo

# Create project directory
print_msg "$BLUE" "📁 Creating project directory..."
mkdir -p "$PROJECT_NAME"/{src,config,tests}

# Generate source files
print_msg "$BLUE" "📝 Generating source files..."

# Create variables file for ggen
cat > "$PROJECT_NAME/.ggen-vars.toml" <<EOF
project_name = "$PROJECT_NAME"
deepseek_support = $DEEPSEEK_SUPPORT
openai_support = $OPENAI_SUPPORT
cohere_support = $COHERE_SUPPORT
log_dir = "$LOG_DIR"
EOF

# Generate all files
print_msg "$BLUE" "  ✓ Generating main.rs..."
ggen -t templates/main-rs.tmpl -o "$PROJECT_NAME/src/main.rs" -v "$PROJECT_NAME/.ggen-vars.toml"

print_msg "$BLUE" "  ✓ Generating chat.rs..."
ggen -t templates/chat-rs.tmpl -o "$PROJECT_NAME/src/chat.rs" -v "$PROJECT_NAME/.ggen-vars.toml"

print_msg "$BLUE" "  ✓ Generating config.rs..."
mkdir -p "$PROJECT_NAME/src/config"
ggen -t templates/config-rs.tmpl -o "$PROJECT_NAME/src/config.rs" -v "$PROJECT_NAME/.ggen-vars.toml"

print_msg "$BLUE" "  ✓ Generating config/mcp.rs..."
ggen -t templates/config-mcp-rs.tmpl -o "$PROJECT_NAME/src/config/mcp.rs" -v "$PROJECT_NAME/.ggen-vars.toml"

print_msg "$BLUE" "  ✓ Generating mcp_adaptor.rs..."
ggen -t templates/mcp-adaptor-rs.tmpl -o "$PROJECT_NAME/src/mcp_adaptor.rs" -v "$PROJECT_NAME/.ggen-vars.toml"

print_msg "$BLUE" "  ✓ Generating Cargo.toml..."
ggen -t templates/cargo-toml.tmpl -o "$PROJECT_NAME/Cargo.toml" -v "$PROJECT_NAME/.ggen-vars.toml"

print_msg "$BLUE" "  ✓ Generating config.toml..."
ggen -t templates/config-toml.tmpl -o "$PROJECT_NAME/config.toml" -v "$PROJECT_NAME/.ggen-vars.toml"

# Copy additional files
print_msg "$BLUE" "📋 Creating additional files..."

# Create README
cat > "$PROJECT_NAME/README.md" <<EOF
# $PROJECT_NAME

MCP + Rig Integration - AI Agent with Dynamic Tools

## Quick Start

1. Configure API keys in \`config.toml\`
2. Add MCP servers in \`config.toml\`
3. Run: \`cargo run\`

## Configuration

Edit \`config.toml\` to:
- Set API keys for AI providers
- Configure MCP servers (stdio, SSE, streamable)
- Customize application settings

## Available Providers

- DeepSeek: $([ "$DEEPSEEK_SUPPORT" = true ] && echo "✓ Enabled" || echo "✗ Disabled")
- OpenAI: $([ "$OPENAI_SUPPORT" = true ] && echo "✓ Enabled" || echo "✗ Disabled")
- Cohere: $([ "$COHERE_SUPPORT" = true ] && echo "✓ Enabled" || echo "✗ Disabled")

## MCP Servers

Configure MCP servers in \`config.toml\`. Examples:

\`\`\`toml
[[mcp_servers]]
name = "filesystem"
transport = "stdio"
command = "npx"
args = ["-y", "@modelcontextprotocol/server-filesystem", "/tmp"]
\`\`\`

## Usage

\`\`\`bash
cargo run
\`\`\`

Then interact with the AI agent through the CLI.

## Documentation

See the main project README for detailed documentation.
EOF

# Create .gitignore
cat > "$PROJECT_NAME/.gitignore" <<EOF
/target
Cargo.lock
*.swp
*.swo
*~
.DS_Store
$LOG_DIR/
*.log
config.toml.local
.env
EOF

# Create example environment file
cat > "$PROJECT_NAME/.env.example" <<EOF
# AI Provider API Keys
DEEPSEEK_API_KEY=your-api-key-here
$([ "$OPENAI_SUPPORT" = true ] && echo "OPENAI_API_KEY=your-api-key-here")
$([ "$COHERE_SUPPORT" = true ] && echo "COHERE_API_KEY=your-api-key-here")
EOF

# Clean up temporary files
rm "$PROJECT_NAME/.ggen-vars.toml"

print_msg "$GREEN" "✅ Project generated successfully!"
echo

# Print next steps
print_msg "$YELLOW" "📋 Next Steps:"
echo
echo "  1. cd $PROJECT_NAME"
echo "  2. Configure API keys in config.toml"
echo "  3. Install MCP servers (e.g., npm install -g @modelcontextprotocol/server-filesystem)"
echo "  4. cargo build"
echo "  5. cargo run"
echo

print_msg "$BLUE" "📚 Resources:"
echo "  - MCP Documentation: https://modelcontextprotocol.io/"
echo "  - Rig Framework: https://github.com/0xPlaygrounds/rig"
echo "  - MCP Servers: https://github.com/modelcontextprotocol/servers"
echo

print_msg "$GREEN" "Happy coding! 🚀"
