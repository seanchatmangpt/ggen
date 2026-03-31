# rust-analyzer Configuration for ggen v6.0.1

## Overview

This document describes the optimal rust-analyzer configuration for the ggen workspace, which consists of 30 crates with complex features including procedural macros, generics, and async/await patterns.

## Workspace Characteristics

- **Total crates**: 30 (79 members in workspace Cargo.toml)
- **Rust source files**: 1,833 `.rs` files
- **Workspace size**: 2.5GB (crates/) + 53GB (target/)
- **Largest target directories**:
  - `a2a-generated/target`: 2.0GB
  - `ggen-workflow/target`: 161MB
  - `osiris-core/target`: 82MB

## Configuration Files

### 1. `.vscode/settings.json`

Editor-specific configuration for VS Code. Includes:
- Cargo build configuration
- Proc-macro settings
- Diagnostics configuration
- Completion and inlay hints
- Performance optimizations

### 2. `.rust-analyzer.toml`

Project-wide configuration that works across all editors. Includes:
- Same settings as VS Code but in TOML format
- Can be used with Vim, Emacs, and other LSP clients

## Key Configuration Decisions

### Cargo Build Optimization

```json
{
  "rust-analyzer.cargo.loadOutDirsFromCheck": true,
  "rust-analyzer.cargo.features": ["otel"],
  "rust-analyzer.cargo.extraArgs": ["--locked", "-j", "8"]
}
```

**Rationale**:
- `loadOutDirsFromCheck: true` - Uses `cargo check` output instead of `cargo build`, significantly faster for analysis
- `features: ["otel"]` - Only enable the `otel` feature by default, reducing compilation time
- `extraArgs: ["--locked"]` - Ensures consistent dependencies across builds
- `extraArgs: ["-j", "8"]` - Parallel compilation with 8 threads (adjust based on CPU cores)

### Proc-Macro Configuration

```json
{
  "rust-analyzer.procMacro.enable": true,
  "rust-analyzer.procMacro.attributes.enable": true,
  "rust-analyzer.procMacro.derives.enable": true,
  "rust-analyzer.cargo.buildScripts.enable": true,
  "rust-analyzer.cargo.buildScripts.rebuildOnSave": true
}
```

**Rationale**:
- ggen uses derive macros extensively (e.g., `derive_more`, `serde::derive`)
- Build scripts (`build.rs`) are present in 5 crates
- Proc-macro expansion is critical for accurate code intelligence
- `rebuildOnSave: true` ensures proc-macros are recompiled when changed

### Diagnostics Configuration

```json
{
  "rust-analyzer.diagnostics.disabled": [
    "unresolved-proc-macro",
    "inactive-code",
    "unlinked-file"
  ],
  "rust-analyzer.checkOnSave.command": "clippy",
  "rust-analyzer.checkOnSave.extraArgs": [
    "--",
    "-W", "clippy::all",
    "-W", "clippy::pedantic",
    "-W", "clippy::nursery",
    "-A", "clippy::multiple_crate_versions",
    "-A", "clippy::must_use_candidate"
  ]
}
```

**Rationale**:
- `unresolved-proc-macro` disabled - Temporarily ignore if proc-macro server is slow
- `inactive-code` disabled - Too many false positives in conditional compilation
- `unlinked-file` disabled - Allow standalone Rust files outside workspace
- Clippy with pedantic/nursery lints - Enforces high code quality
- Allow `multiple_crate_versions` - Acceptable in large workspaces
- Allow `must_use_candidate` - Too noisy across 30+ crates

### Performance Optimizations

```json
{
  "rust-analyzer.workspace.symbol.search.limit": 1024,
  "rust-analyzer.workspace.symbol.search.kindOnly": true,
  "rust-analyzer.memoryUsageMegapixels": 8192,
  "files.exclude": {
    "**/target": true,
    "**/target-debug": true,
    "**/.git": true,
    "**/node_modules": true
  }
}
```

**Rationale**:
- `workspace.symbol.search.limit: 1024` - Limits results to prevent UI freezing
- `workspace.symbol.search.kindOnly: true` - Faster symbol search
- `memoryUsageMegapixels: 8192` - Allocate 8GB RAM for rust-analyzer (large workspace)
- `files.exclude` - Exclude large directories from watching and indexing

### Inlay Hints

```json
{
  "rust-analyzer.inlayHints.typeHints.enable": true,
  "rust-analyzer.inlayHints.closureReturnTypeHints.enable": "always",
  "rust-analyzer.inlayHints.lifetimeElisionHints.enable": "always",
  "rust-analyzer.inlayHints.reborrowHints.enable": "always",
  "rust-analyzer.inlayHints.maxLength": 25
}
```

**Rationale**:
- Type hints improve code readability in complex generic code
- Closure return type hints critical for async/await patterns
- Lifetime elision hints help understand lifetime annotations
- Reborrow hints clarify borrowing semantics
- `maxLength: 25` - Prevents hints from being too verbose

## Build Scripts and Proc-Macros

### Identified Build Scripts

1. `crates/ggen-core/examples/embedded-iot/build.rs`
2. `crates/tai-grpc/build.rs`
3. `crates/ggen-node/build.rs`
4. `crates/ggen-domain/src/project/build.rs`
5. `crates/ggen-workflow/native/build.rs`

### Proc-Macro Usage

The workspace uses:
- `derive_more` (v1.0) - Derive macros for common traits
- `serde::derive` - Serialization/deserialization
- `clap::derive` - CLI argument parsing
- `async-trait` - Async trait methods
- Custom proc-macros in `ggen-macros`

## Performance Metrics

### Target Performance

Based on research from `/Users/sac/ggen/docs/research/performance-optimization-2026.md`:

| Operation | Target | rust-analyzer Setting |
|-----------|--------|----------------------|
| **Code completion** | <100ms | `completion.fullFunctionSignatures.enable = true` |
| **LSP navigation** | <50ms | `workspace.symbol.search.kindOnly = true` |
| **Semantic search** | <200ms | `workspace.symbol.search.limit = 1024` |
| **Full analysis** | <2s | `cargo.loadOutDirsFromCheck = true` |

### Memory Usage

- **Baseline**: 2-4 GB for typical Rust projects
- **ggen workspace**: 6-8 GB (30 crates, 1,833 files)
- **Configuration**: `memoryUsageMegapixels: 8192` (8 GB)

## Troubleshooting

### rust-analyzer is slow

1. **Reduce parallel compilation**: Change `-j 8` to `-j 4` in `cargo.extraArgs`
2. **Disable proc-macros temporarily**: Set `procMacro.enable = false`
3. **Exclude more directories**: Add to `files.exclude`
4. **Increase memory limit**: Set `memoryUsageMegapixels: 16384` (16 GB)

### Proc-macros not expanding

1. **Check proc-macro server**: Ensure `procMacro.enable = true`
2. **Rebuild proc-macros**: Run `cargo clean -p <proc-macro-crate>` and restart rust-analyzer
3. **Check build scripts**: Ensure `buildScripts.enable = true`
4. **View logs**: Run with `RUST_LOG=info` and check "Output > Rust Analyzer Language Server"

### High memory usage

1. **Reduce workspace symbol limit**: Set `workspace.symbol.search.limit: 512`
2. **Disable inlay hints**: Set `inlayHints.enable = false`
3. **Disable flycheck**: Set `checkOnSave.enable = false`
4. **Close unused tabs**: Reduces memory footprint

### Diagnostics are noisy

1. **Disable specific diagnostics**: Add to `diagnostics.disabled`
2. **Downgrade to hints**: Add to `diagnostics.warningsAsHint`
3. **Use workspace.lints**: The workspace already has comprehensive lint configuration in `Cargo.toml`

## Alternative Editors

### Vim/Neovim

The `.rust-analyzer.toml` file will be automatically picked up by:
- `coc-rust-analyzer`
- `nvim-lsp`
- `vim-lsp`

### Emacs

Ensure `lsp-rust-server` is set to `'rust-analyzer` in your config.

### Other Editors

Refer to the [rust-analyzer manual](https://rust-analyzer.github.io/manual.html) for editor-specific setup.

## Sources

- [rust-analyzer Manual](https://rust-analyzer.github.io/manual.html)
- [rust-analyzer GitHub](https://github.com/rust-analyzer/rust-analyzer)
- [ggen Performance Research](/Users/sac/ggen/docs/research/performance-optimization-2026.md)
- [Workspace Cargo.toml](/Users/sac/ggen/Cargo.toml)

## Version History

- **v1.0** (2026-03-31): Initial configuration for ggen v6.0.1
  - Optimized for 30-crate workspace
  - Enabled proc-macro support
  - Configured performance optimizations
  - Set up diagnostics and inlay hints

## Maintenance

When updating the workspace:
1. Review `cargo features` - Add new features if needed
2. Check `build.rs` files - Ensure new crates are accounted for
3. Monitor memory usage - Adjust `memoryUsageMegapixels` if needed
4. Update diagnostics - Add/remove based on team preferences

## Related Documentation

- [CLAUDE.md](/Users/sac/ggen/CLAUDE.md) - Project configuration and rules
- [Performance Optimization Research](/Users/sac/ggen/docs/research/performance-optimization-2026.md) - Detailed performance analysis
- [Cargo.toml](/Users/sac/ggen/Cargo.toml) - Workspace configuration
