# ggen-lsp Installation & Setup

## Quick Start

1. **Ensure ggen is installed**:
   ```bash
   cargo install --path crates/ggen-cli
   ```

2. **One-command setup** (writes editor configs + Agent Admissibility Pack):
   ```bash
   ggen lsp init
   ```

3. **Enable in Claude Code**:
   ggen-lsp is auto-registered when you open any `.ttl`, `.tera`, or `ggen.toml` file.

3. **Verify it's running**:
   - Open a ggen project with `.ttl` or `ggen.toml` files
   - Look for syntax highlighting (colors) in the editor
   - Hover over a class name — should show documentation

## Configuration

Edit `~/.claude/settings.json`:

```json
{
  "ggen-lsp": {
    "enabled": true
  }
}
```

### Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `enabled` | bool | `true` | Enable/disable LSP server |

> The LSP server is stdio only. `auto_format_on_save`, `show_hints`, and
> `workspace_symbol_depth` are not enforced by ggen — auto-formatting, inlay
> hints, and workspace symbol are not delivered features.

## Troubleshooting

### LSP not starting

1. Verify `ggen` binary is in PATH:
   ```bash
   which ggen
   ```

2. Test LSP directly (stdio only):
   ```bash
   ggen lsp start
   ```

   Or run the MCP protocol server for non-LSP agents:
   ```bash
   ggen lsp serve --protocol mcp
   ```

3. Check Claude Code logs (View > Toggle Developer Tools)

### Completion/Hover not working

- Ensure file is saved (`.ttl`, `.tera`, or `ggen.toml`)
- Check file syntax (should parse without errors)
- Restart LSP: Cmd+Shift+P → `ggen-lsp: Restart Server`

### Performance issues

- Check available disk space (need 200MB+)
- Restart the LSP server in your editor

## Uninstall

Remove from `~/.claude/settings.json`:

```json
{
  "ggen-lsp": null
}
```

Or delete the entire `ggen-lsp` section.
