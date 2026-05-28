# ggen-lsp Installation & Setup

## Quick Start

1. **Ensure ggen is installed**:
   ```bash
   cargo install --path crates/ggen-cli
   ```

2. **Enable in Claude Code**:
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
    "enabled": true,
    "transport": "stdio",
    "auto_format_on_save": true,
    "show_hints": true,
    "workspace_symbol_depth": 5
  }
}
```

### Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `enabled` | bool | `true` | Enable/disable LSP server |
| `transport` | string | `"stdio"` | `"stdio"` for normal use, `"http"` for debugging |
| `auto_format_on_save` | bool | `true` | Auto-format documents on save |
| `show_hints` | bool | `true` | Show inline type hints and defaults |
| `workspace_symbol_depth` | number | `5` | Max depth for workspace symbol search |

## Troubleshooting

### LSP not starting

1. Verify `ggen` binary is in PATH:
   ```bash
   which ggen
   ```

2. Test LSP directly:
   ```bash
   ggen lsp start --transport stdio
   ```

3. Check Claude Code logs (View > Toggle Developer Tools)

### Completion/Hover not working

- Ensure file is saved (`.ttl`, `.tera`, or `ggen.toml`)
- Check file syntax (should parse without errors)
- Restart LSP: Cmd+Shift+P → `ggen-lsp: Restart Server`

### Performance issues

- Increase `workspace_symbol_depth` limit
- Disable `show_hints` if too slow
- Check available disk space (need 200MB+)

## Uninstall

Remove from `~/.claude/settings.json`:

```json
{
  "ggen-lsp": null
}
```

Or delete the entire `ggen-lsp` section.
