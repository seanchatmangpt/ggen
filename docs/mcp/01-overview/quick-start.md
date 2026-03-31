# MCP Quick Start

Get up and running with ggen MCP in 5 minutes.

## Prerequisites

- Rust 1.91.1+
- Claude Desktop (or another MCP client)

## Step 1: Build ggen

```bash
cd /Users/sac/ggen
cargo make build
```

## Step 2: Configure Claude Desktop

Add to `~/Library/Application Support/Claude/claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "ggen": {
      "command": "/Users/sac/ggen/target/release/ggen",
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
chmod +x /Users/sac/ggen/target/release/ggen
```

## Next Steps

- [Installation Guide](./installation.md) - Detailed setup
- [Tool Reference](../02-user-guide/tools/) - All 16 tools documented
- [Tutorials](../03-tutorials/) - Step-by-step guides
