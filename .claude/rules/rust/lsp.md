# Rust Code Intelligence (LSP — REQUIRED for .rs files)

**ALWAYS use the LSP tool instead of Grep when navigating Rust code.** The `rust-analyzer-lsp@claude-plugins-official` plugin provides semantic navigation that is faster and more precise than text search across this 30-crate workspace.

## Forbidden vs. Recommended

| Task | Forbidden | Use Instead |
|------|-----------|-------------|
| Find struct / enum / trait / fn by name | `Grep "struct Foo"` in .rs | `LSP workspaceSymbol` |
| List all symbols in a .rs file | Read file + scan manually | `LSP documentSymbol` |
| Find where a type/function is defined | `Grep -r "fn foo("` | `LSP goToDefinition` |
| Find all usages across workspace | `Grep -r "foo("` | `LSP findReferences` |
| Find all types implementing a trait | `Grep -r "impl Foo"` (misses blanket impls) | `LSP goToImplementation` |
| Get type signature, trait bounds, visibility | Read source file | `LSP hover` |
| Trace callers of a function | Manual grep chain across crates | `LSP incomingCalls` (after `prepareCallHierarchy`) |
| Trace what a function calls | Read function body | `LSP outgoingCalls` (after `prepareCallHierarchy`) |
| Navigate `mod` / `use` to source file | Read mod.rs, find path | `LSP goToDefinition` on module name |
| Find trait method implementations | `Grep -r "fn method_name"` (ambiguous) | `LSP goToImplementation` on trait method |

## Workflow Patterns

### Pattern 1: Standard Symbol Navigation

```
1. LSP workspaceSymbol  — find symbol position (use any .rs file as filePath)
2. LSP goToDefinition   — jump to definition
3. LSP findReferences   — enumerate all usages
```

### Pattern 2: Trait Implementation Discovery

Critical for Rust's heavy trait usage. Use `goToImplementation` on project-defined traits (not stdlib traits like `From<T>`).

```
1. LSP goToDefinition on trait name     — open the trait definition
2. LSP goToImplementation on trait name — find ALL implementing types
3. LSP hover on each impl               — see trait bounds and derived types
4. LSP findReferences on specific method — find call sites across workspace
```

### Pattern 3: Module Tree Navigation

Essential for navigating the 30-crate workspace.

```
1. LSP documentSymbol on mod.rs or lib.rs — see module structure
2. LSP goToDefinition on mod name         — jump to submodule file
3. LSP findReferences on pub use          — find re-export consumers
4. LSP workspaceSymbol with crate prefix  — search across crate boundaries
```

### Pattern 4: Call Hierarchy Tracing

```
1. LSP prepareCallHierarchy at function position — get call hierarchy item
2. LSP incomingCalls  — find all callers
3. LSP outgoingCalls  — find all callees
```

## Parameter Guide

Every LSP call needs `filePath` (absolute path to a .rs file), `line` (1-based), `character` (1-based), and `operation`.

| Operation | What to point at | Notes |
|-----------|-----------------|-------|
| `documentSymbol` | Any position | Lists all mod, struct, enum, trait, impl, fn, const, type, use, static |
| `workspaceSymbol` | Any .rs file as filePath | Workspace-wide search; use specific names (e.g., `GgenConfig` not `Config`) |
| `goToDefinition` | Symbol identifier | Works across modules including re-exports |
| `findReferences` | Symbol identifier | Finds all usages across the entire workspace |
| `goToImplementation` | Trait name or trait method | Most useful for project-defined traits |
| `hover` | Symbol identifier | Shows type signatures, trait bounds, derive macros, visibility |
| `prepareCallHierarchy` | Function/method name | Returns item for `incomingCalls`/`outgoingCalls` |
| `incomingCalls` | Result from `prepareCallHierarchy` | Lists all callers of the function |
| `outgoingCalls` | Result from `prepareCallHierarchy` | Lists all functions called by this function |

In this 30-crate workspace, any `filePath` under `crates/*/src/` works. Use the file you have open.

## Fallback Procedure

If LSP returns `"No LSP server available"`:

1. Check that rust-analyzer is installed: `rustup component add rust-analyzer`
2. Check that `.claude/settings.json` has `"rust-analyzer-lsp@claude-plugins-official": true`
3. Restart Claude Code
4. If still failing, fall back to Grep (but flag it as degraded navigation)

## When Grep Is Acceptable

Grep is acceptable ONLY for:

- Non-semantic text search (TODO, FIXME, FUTURE comments)
- Searching non-Rust files (TOML, YAML, MD, TTL)
- Regex patterns across many files when no LSP symbol is targeted
- Verifying string literals, error messages, test assertions
