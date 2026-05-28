# ggen Language Server Protocol (LSP)

Syntax highlighting, completion, diagnostics, and refactoring for ggen RDF/Tera/TOML files.

## Supported File Types

- `.ttl` — RDF Turtle specifications
- `.tera` — Tera code generation templates
- `ggen.toml` — ggen project configuration

## Features

- **Syntax validation** — Catch RDF parse errors, undefined Tera variables, TOML type mismatches
- **Completion** — Suggest RDF predicates, Tera filters, config keys
- **Hover** — Show documentation, SHACL shapes, type information
- **Definition jumping** — Navigate class definitions, template includes
- **Refactoring** — Rename symbols across files with validation
- **Semantic highlighting** — Color-code classes, properties, variables, literals
- **Document symbols** — Outline view for all definitions
- **Code lenses** — Inline commands ("Show SHACL shape", "Go to include")
- **Code folding** — Collapse RDF shapes, Tera blocks, TOML sections
- **Code formatting** — Auto-format with consistent indentation

## Installation

```bash
# Via Claude Code settings (auto-registered)
```

## Configuration

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

## Usage

### Editor Integration

- **Open `.ttl` file** → LSP server auto-starts in background
- **Type `sh:`** → Completion suggests SHACL predicates
- **Hover `ex:Person`** → Shows class documentation + shape
- **Right-click → Go to Definition** → Jump to class definition
- **Cmd+K Cmd+R** → Rename class across all files
- **Cmd+I** → Show document outline (symbols)

### Command Palette

- `ggen-lsp: Start Server` — Manually start LSP
- `ggen-lsp: Stop Server` — Manually stop LSP
- `ggen-lsp: Restart Server` — Restart if hung
- `ggen-lsp: Show Diagnostics` — View all validation errors
- `ggen-lsp: Format Document` — Auto-format current file

## Performance

- Startup: <200ms
- Completion: <100ms
- Diagnostics: <500ms for large files
- Memory: <100MB typical use

## Requirements

- ggen v26.5.21 or later
- Claude Code (any version with Marketplace support)
- 200MB free disk space

## Support

- Issues: [ggen GitHub](https://github.com/seanchatmangpt/ggen/issues)
- Docs: [ggen LSP ARD-PRD](../../LSP-ARD-PRD.md)
