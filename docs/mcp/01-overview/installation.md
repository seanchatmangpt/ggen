<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [MCP Installation](#mcp-installation)
  - [Installation Methods](#installation-methods)
    - [Method 1: Build from Source (Recommended)](#method-1-build-from-source-recommended)
    - [Method 2: Install via Cargo](#method-2-install-via-cargo)
    - [Method 3: Download Pre-built Binary](#method-3-download-pre-built-binary)
  - [Configuration](#configuration)
    - [Claude Desktop (macOS)](#claude-desktop-macos)
    - [Cursor IDE](#cursor-ide)
    - [HTTP Server (Remote Access)](#http-server-remote-access)
  - [Environment Variables](#environment-variables)
  - [Verification](#verification)
    - [Check Server Start](#check-server-start)
    - [Test with MCP Inspector](#test-with-mcp-inspector)
    - [Verify Tools](#verify-tools)
  - [Dependencies](#dependencies)
    - [Required](#required)
    - [Optional](#optional)
  - [Troubleshooting](#troubleshooting)
    - [Build Errors](#build-errors)
    - [Runtime Errors](#runtime-errors)
    - [Connection Refused](#connection-refused)
  - [Uninstall](#uninstall)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# MCP Installation

Detailed installation instructions for ggen MCP server.

## Installation Methods

### Method 1: Build from Source (Recommended)

```bash
# Clone repository
git clone https://github.com/seanchatmangpt/ggen.git
cd ggen

# Build release binary
cargo make build

# Verify installation
./target/release/ggen --version
```

### Method 2: Install via Cargo

```bash
cargo install ggen-cli
```

### Method 3: Download Pre-built Binary

```bash
# Download latest release
curl -L https://github.com/seanchatmangpt/ggen/releases/latest/download/ggen-cli-darwin-aarch64.tar.gz -o ggen.tar.gz

# Extract
tar -xzf ggen.tar.gz

# Install
sudo mv ggen /usr/local/bin/
```

## Configuration

### Claude Desktop (macOS)

Edit `~/Library/Application Support/Claude/claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "ggen": {
      "command": "/usr/local/bin/ggen",
      "args": ["mcp", "start-server", "--transport", "stdio"],
      "env": {
        "GGEN_EXAMPLES_DIR": "./examples"
      }
    }
  }
}
```

### Cursor IDE

Add to `.cursorrules` or Cursor settings:

```json
{
  "mcp.servers": {
    "ggen": {
      "command": "/usr/local/bin/ggen",
      "args": ["mcp", "start-server", "--transport", "stdio"]
    }
  }
}
```

### HTTP Server (Remote Access)

```bash
ggen mcp start-server --transport http --host 0.0.0.0 --port 8080
```

Configure client with URL:
```
http://localhost:8080/mcp
```

## Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `GGEN_EXAMPLES_DIR` | Path to bundled examples | `./examples` |
| `GGEN_RUST_LOG` | Log level filter | `info` |
| `GGEN_OTEL_ENABLED` | Enable OpenTelemetry | `false` |
| `RUST_LOG` | Global Rust log filter | `ggen=info` |

## Verification

### Check Server Start

```bash
ggen mcp start-server --transport stdio
```

Expected output:
```
INFO ggen_a2a_mcp::server: Starting MCP server (stdio transport)
INFO ggen_a2a_mcp::ggen_server: Registered 16 tools
INFO ggen_a2a_mcp::ggen_server: Registered 4 resources
INFO ggen_a2a_mcp::ggen_server: Registered 3 prompts
```

### Test with MCP Inspector

```bash
npx @modelcontextprotocol/inspector ggen
```

### Verify Tools

List all available tools:

```bash
echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | ggen mcp start-server --transport stdio
```

## Dependencies

### Required

- Rust 1.91.1+
- Tokio runtime
- Serde (JSON serialization)

### Optional

- Oxigraph (RDF store)
- OpenTelemetry SDK (tracing)
- reqwest (HTTP client)

## Troubleshooting

### Build Errors

```bash
# Update Rust
rustup update stable

# Clean build
cargo clean
cargo make build
```

### Runtime Errors

Check logs:
```bash
RUST_LOG=debug ggen mcp start-server --transport stdio
```

### Connection Refused

1. Verify server is running
2. Check firewall settings
3. Confirm transport type matches client config

## Uninstall

```bash
# Remove binary
sudo rm /usr/local/bin/ggen

# Remove config
rm ~/Library/Application\ Support/Claude/claude_desktop_config.json

# Remove examples (optional)
rm -rf ~/.ggen
```

## Next Steps

- [Quick Start](./quick-start.md) - First steps
- [Tool Reference](../02-user-guide/tools/) - All tools documented
