<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [MCP Quick Start](#mcp-quick-start)
  - [Prerequisites](#prerequisites)
  - [Step 1: Build ggen](#step-1-build-ggen)
  - [Step 2: Configure Claude Desktop](#step-2-configure-claude-desktop)
  - [Step 3: Restart Claude Desktop](#step-3-restart-claude-desktop)
  - [Step 4: Verify Connection](#step-4-verify-connection)
  - [Step 5: First Tool Call](#step-5-first-tool-call)
  - [Troubleshooting](#troubleshooting)
    - [Server Not Starting](#server-not-starting)
    - [Tools Not Appearing](#tools-not-appearing)
    - [Permission Errors](#permission-errors)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# MCP Quick Start

Get up and running with ggen MCP in 5 minutes.

## Prerequisites

- Rust 1.91.1+
- Claude Desktop (or another MCP client)

## Step 1: Build ggen

```bash
cd .
cargo make build
```

## Step 2: Configure Claude Desktop

Add to `~/Library/Application Support/Claude/claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "ggen": {
      "command": "./target/release/ggen",
      "args": ["mcp", "start-server", "--transport", "stdio"]
    }
  }
}
```

## Step 3: Restart Claude Desktop

Quit and restart Claude Desktop. The ggen MCP server will start automatically.

## Step 4: Verify Connection

In Claude Desktop, ask:

```
@ggen what tools are available?
```

You should see a list of all 16 MCP tools.

## Step 5: First Tool Call

Generate code from an ontology:

```
@ggen use the generate tool with:
- ontology_path: examples/hello-world/ontology.ttl
- output_dir: /tmp/hello-gen
```

## Troubleshooting

### Server Not Starting

Check logs in `~/Library/Logs/Claude/` for errors.

### Tools Not Appearing

1. Verify the config JSON is valid
2. Check the binary path is correct
3. Run `ggen mcp start-server --transport stdio` manually to test

### Permission Errors

Ensure the binary has execute permissions:

```bash
chmod +x ./target/release/ggen
```

## Next Steps

- [Installation Guide](./installation.md) - Detailed setup
- [Tool Reference](../02-user-guide/tools/) - All 16 tools documented
- [Tutorials](../03-tutorials/) - Step-by-step guides
