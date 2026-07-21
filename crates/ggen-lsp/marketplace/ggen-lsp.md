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
- **Document symbols** — Outline view for all definitions
- **Code actions** — Apply repair-route quickfixes (`WorkspaceEdit`)
- **Code folding** — Collapse RDF shapes, Tera blocks, TOML sections
- **Diagnostics** — Surface E00XX law violations live as you type

> Not delivered (do not assume): auto-formatting, semantic tokens/highlighting,
> code lenses, workspace symbol, inlay hints, call/type hierarchy.

## Installation

```bash
# Via Claude Code settings (auto-registered)
```

## Configuration

```json
{
  "ggen-lsp": {
    "enabled": true
  }
}
```

> The LSP server is stdio only. `auto_format_on_save`, `show_hints`, and
> `workspace_symbol_depth` are not enforced by ggen.

## Usage

### Editor Integration

- **Open `.ttl` file** → LSP server auto-starts in background
- **Type `sh:`** → Completion suggests SHACL predicates
- **Hover `ex:Person`** → Shows class documentation + shape
- **Right-click → Go to Definition** → Jump to class definition
- **Cmd+K Cmd+R** → Rename class across all files
- **Cmd+I** → Show document outline (symbols)

### CLI

- `ggen lsp init` — One-command setup: write editor configs + Agent Admissibility Pack
- `ggen lsp start` — Run the language server (stdio only; editors launch this)
- `ggen lsp check` — Headless gate: scan all law surfaces, exit non-zero on ERROR

## Performance

- Startup: <200ms
- Completion: <100ms
- Diagnostics: <500ms for large files
- Memory: <100MB typical use

## Requirements

- ggen v26.5.28 or later
- Claude Code (any version with Marketplace support)
- 200MB free disk space

## Support

- Issues: [ggen GitHub](https://github.com/seanchatmangpt/ggen/issues)
- Docs: [ggen LSP ARD-PRD](../../../docs/architecture/LSP-ARD-PRD.md)
