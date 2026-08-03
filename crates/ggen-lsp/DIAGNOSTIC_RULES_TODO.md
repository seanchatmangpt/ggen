# ggen-lsp Diagnostic Rule Status

This file records the disposition of the diagnostic gaps discovered during the
2026-06-24 `claude-code-config-lsp` validation.

## GGEN-SRC-004 — implemented

**Severity:** ERROR  
**Scope:** generated Rust outputs declared by `ggen.toml`

`GGEN-SRC-004` now detects semicolon module declarations whose legal Rust module
paths are not owned by any generation rule. It evaluates the admitted project
graph rather than merely checking whether a file happens to exist.

Example:

```rust
pub mod capabilities;
```

For `src/lib.rs`, one of these outputs must be owned by a generation rule:

```text
src/capabilities.rs
src/capabilities/mod.rs
```

The detector:

- reads open generated-Rust buffers before disk;
- ignores inline modules;
- ignores explicit `#[path = "..."]` declarations;
- excludes dynamic output patterns and URL outputs until a concrete path exists;
- folds into the headless checker, request capture, editor diagnostics, route
  selection, registry replacement, Λ_CD gate, and OCEL lifecycle;
- has pure detector tests plus a spawned-binary LSP raise/repair/clear test.

## GGEN-SPARQL-VAR-001 — subsumed

The proposed SPARQL-variable advisory is subsumed by `GGEN-TPL-001`, which
cross-checks template projections against explicit SPARQL `SELECT` variables.
A separate warning would duplicate the same failed binding under another code.

## GGEN-TEMPLATE-CONTEXT-001 — subsumed

Undefined Tera projection variables remain represented by `GGEN-TPL-001`.
Locally set variables and known Tera control variables are handled by the Tera
analyzer rather than a second cross-surface diagnostic family.

## Replay

```bash
cargo test -p ggen-lsp --lib source_contract
cargo test -p ggen-lsp --test lsp_contract_completion_test
cargo test -p ggen-lsp --test ggen_src_004_living_loop
cargo test -p ggen-lsp --all-features
```

Runtime standing requires these commands to execute against the exact published
head; source presence alone is not execution evidence.
