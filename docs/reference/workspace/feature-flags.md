<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Feature Flags (Reference)](#feature-flags-reference)
  - [`ggen-cli` features](#ggen-cli-features)
  - [How feature flags enforce the boundary](#how-feature-flags-enforce-the-boundary)
  - [Build commands](#build-commands)
  - [See also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Feature Flags (Reference)

> Reference. Factual lookup, derived from `crates/ggen-cli/Cargo.toml` `[features]`.
> ggen v26.5.28. `default = []` — the default build is the minimal, fully-proven surface.

## `ggen-cli` features

| Feature | Definition | Effect |
|---------|------------|--------|
| `default` | `[]` | Minimal surface: only the KEEP/PROVE nouns compile (`init`, `sync`, `graph`, `pack`, `policy`, `doctor`, `utils`). |
| `lsp` | `["dep:ggen-lsp", "dep:ggen-lsp-mcp"]` | Enables the LSP/MCP delivery plane — the `ggen lsp` noun and all its verbs (`start`/`check`/`init`/`serve`/`mine`/`metrics`/`replay`/`field-status`/`emit_pack`/`verify_pack`), including `serve --protocol mcp`. Off by default. |
| `autonomic` | `[]` | Autonomic actuation surface (opt-in). |
| `paas` | `[]` | PaaS integration surface (opt-in). |
| `integration` | `[]` | Enables integration-test-only code paths. |
| `live-llm-tests` | `[]` | Enables tests that make real LLM API calls (require credentials + network). |

## How feature flags enforce the boundary

The `lsp` noun is gated with `#[cfg(feature = "lsp")]` in `crates/ggen-cli/src/cmds/mod.rs`.
Because `clap-noun-verb` auto-discovers verbs from `pub mod` declarations, gating a
`pub mod` **removes the noun from the CLI surface entirely** — not just its help text. A
feature flag is therefore a real boundary mechanism, not a cosmetic toggle:

```rust
#[cfg(feature = "lsp")]
pub mod lsp; // absent from the binary unless --features lsp
```

This is why archiving a command is a lawful Oracle-Gap closure: a `pub mod` behind a
default-off feature is genuinely not in the default release surface, while the code
remains in the tree as fossil evidence (non-deletion doctrine).

## Build commands

```bash
cargo make check                 # default features — the rest-gate surface
cargo build --features lsp       # include the LSP/MCP delivery plane
```

## See also

- [Crates](crates.md) — which crates each feature pulls in
- [v26.5.28 boundary](../release/v26-5-28-boundary.md) — KEEP/PROVE vs feature-gated vs ARCHIVE
- [Command-proof matrix](../cli/command-proof-matrix.md) — per-noun proof status
