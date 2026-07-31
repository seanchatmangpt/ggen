# ggen-lsp Installation & Setup

## Quick Start

1. **Build ggen-lsp from source** (requires nightly Rust):
   ```bash
   git clone https://github.com/seanchatmangpt/ggen
   cd ggen
   cargo build --release -p ggen-lsp --features mcp
   ```

2. **Start the MCP server**:
   ```bash
   ./target/release/ggen-lsp
   ```
   
   Or as an LSP stdio server:
   ```bash
   ./target/release/ggen-lsp lsp start
   ```

3. **Configure Claude Code** — see Configuration section below for MCP server details.

4. **Verify it's running**:
   - Open a ggen project with `.ttl` or `ggen.toml` files
   - Look for syntax highlighting (colors) in the editor
   - Hover over a class name — should show documentation
   - MCP tools `ggen.construct`, `ggen.check`, etc. should appear in Claude Code

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

### Build fails or missing Rust

ggen-lsp requires **nightly Rust** (pinned to a specific date in `rust-toolchain.toml`). Ensure you have
`rustup` installed:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
rustup update nightly
```

### LSP not starting

1. Verify the binary built successfully:
   ```bash
   ./target/release/ggen-lsp --version
   ```

2. Test LSP directly (stdio only):
   ```bash
   ./target/release/ggen-lsp lsp start
   ```

   Or run the MCP protocol server for Claude Code:
   ```bash
   ./target/release/ggen-lsp lsp serve --protocol mcp
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
