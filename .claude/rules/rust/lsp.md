---
auto_load: false
category: rust
priority: high
version: 6.0.1
---

# LSP-First Navigation

You will reach for Grep because it is faster. This is the failure mode.

Grep searches text. LSP searches semantics. In a 30-crate workspace with trait bounds, blanket impls, and re-exports, text search misses what you need and returns what you do not. You use LSP because it understands Rust the way the compiler does.

## The Discipline

Every symbol lookup on a `.rs` file goes through LSP. No exceptions for "quick checks." No exceptions for "I just need to find one thing." The moment you Grep a `.rs` file for a symbol name, you are gambling with false positives and missed results.

## Forbidden vs. Required

| Task | Forbidden | Required |
|------|-----------|----------|
| Find struct/enum/trait/fn by name | `Grep "struct Foo"` | `LSP workspaceSymbol` |
| List symbols in a file | Read + scan manually | `LSP documentSymbol` |
| Find definition | `Grep -r "fn foo("` | `LSP goToDefinition` |
| Find all usages | `Grep -r "foo("` | `LSP findReferences` |
| Find trait implementors | `Grep -r "impl Foo"` | `LSP goToImplementation` |
| Get type signature | Read source | `LSP hover` |
| Trace callers | Manual grep chain | `LSP incomingCalls` |
| Trace callees | Read function body | `LSP outgoingCalls` |
| Navigate mod tree | Read mod.rs | `LSP goToDefinition` on mod name |
| Find trait method impls | `Grep "fn method_name"` | `LSP goToImplementation` on trait method |

## Workflow Patterns

**Standard lookup:** `workspaceSymbol` then `goToDefinition` then `findReferences`.

**Trait discovery:** `goToDefinition` on trait, then `goToImplementation` for all implementors, then `hover` on each impl for bounds. Only for project-defined traits -- stdlib traits like `From<T>` have too many impls.

**Call hierarchy:** `prepareCallHierarchy` at function position, then `incomingCalls` for callers, `outgoingCalls` for callees.

**Module navigation:** `documentSymbol` on mod.rs or lib.rs for structure, `goToDefinition` on mod names to jump to submodules.

## Parameter Guide

Every LSP call needs `filePath` (absolute path to any `.rs` in the workspace), `line` (1-based), `character` (1-based), and `operation`.

Any `.rs` file under `crates/*/src/` works as the `filePath`.

## When Grep Is Acceptable

Non-semantic text search only: TODO/FIXME/FUTURE comments, searching non-Rust files (TOML, YAML, MD, TTL), regex patterns across many files with no symbol target, verifying string literals and error messages.

## Fallback

If LSP returns "No LSP server available": install rust-analyzer (`rustup component add rust-analyzer`), verify `.claude/settings.json` has the plugin enabled, restart Claude Code. If still failing, fall back to Grep and flag it as degraded navigation.
