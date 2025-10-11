<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Build Optimization Guide](#build-optimization-guide)
  - [Current Build Performance](#current-build-performance)
  - [Applied Optimizations](#applied-optimizations)
    - [1. Incremental Compilation (Cargo.toml)](#1-incremental-compilation-cargotoml)
    - [2. Parallel Builds (.cargo/config.toml)](#2-parallel-builds-cargoconfigtoml)
    - [3. Optimized Profiles](#3-optimized-profiles)
  - [Why Builds Were Slow](#why-builds-were-slow)
    - [Root Causes Fixed:](#root-causes-fixed)
    - [Heavy Dependencies:](#heavy-dependencies)
  - [Best Practices](#best-practices)
    - [DO:](#do)
    - [DON'T:](#dont)
  - [Troubleshooting](#troubleshooting)
    - [Still Rebuilding Everything?](#still-rebuilding-everything)
    - [Profile Warning](#profile-warning)
  - [Performance Tips](#performance-tips)
    - [1. Use `cargo-watch` for Auto-Rebuild](#1-use-cargo-watch-for-auto-rebuild)
    - [2. Use `sccache` for Shared Cache](#2-use-sccache-for-shared-cache)
    - [3. Optimize Linking (macOS)](#3-optimize-linking-macos)
    - [4. Use Rust Analyzer](#4-use-rust-analyzer)
  - [Benchmark Your Builds](#benchmark-your-builds)
  - [Cache Sizes](#cache-sizes)
  - [Monitoring Build Times](#monitoring-build-times)
  - [When to Clean](#when-to-clean)
  - [Environment Variables](#environment-variables)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Build Optimization Guide

## Current Build Performance

With the optimizations applied, you should see:
- **Initial build**: ~1-2 minutes (all dependencies)
- **Incremental builds**: 2-3 seconds (cached dependencies)
- **No-change rebuilds**: <1 second

## Applied Optimizations

### 1. Incremental Compilation (Cargo.toml)
```toml
[profile.dev]
incremental = true           # Cache compilation artifacts
codegen-units = 256          # More parallel compilation
split-debuginfo = "unpacked" # Faster debug info on macOS
```

### 2. Parallel Builds (.cargo/config.toml)
```toml
[build]
jobs = 16                    # Use more CPU cores
pipelining = true            # Pipeline compilation stages
incremental = true           # Enable incremental
```

### 3. Optimized Profiles
- **Dev**: Fast compilation, incremental
- **Release**: Thin LTO for balance
- **Test**: No optimization, incremental

## Why Builds Were Slow

### Root Causes Fixed:
1. **Missing Cargo.lock** - Forces full dependency resolution every time
2. **Low codegen-units (4)** - Limited parallelism
3. **No incremental setting** - Recompiled everything
4. **Profile conflicts** - ggen-mcp had conflicting profile settings

### Heavy Dependencies:
- `oxigraph` (RDF database) - 87 dependencies
- `shacl_validation` - Complex graph validation
- Duplicate `reqwest` versions (v0.11 and v0.12)
- Duplicate `base64` versions

## Best Practices

### DO:
✅ **Keep Cargo.lock committed** - Ensures consistent builds
✅ **Use `cargo check`** - Faster than `cargo build` during development
✅ **Run `cargo build` once** - Then use incremental compilation
✅ **Touch only changed files** - Maximizes cache usage

### DON'T:
❌ **Delete Cargo.lock** - Forces full rebuild
❌ **Run `cargo clean`** - Deletes all caches
❌ **Change profile settings** - Invalidates caches
❌ **Update dependencies frequently** - Requires rebuilding deps

## Troubleshooting

### Still Rebuilding Everything?

**Check incremental cache:**
```bash
ls -lh target/debug/incremental/
```

**Check if cache is used:**
```bash
cargo build -vv | grep "reusing"
```

**Clear stale cache:**
```bash
rm -rf target/debug/incremental/*
cargo build
```

### Profile Warning
```
warning: profiles for the non root package will be ignored
```
This is normal for workspace members. Profiles are defined at workspace root.

## Performance Tips

### 1. Use `cargo-watch` for Auto-Rebuild
```bash
cargo install cargo-watch
cargo watch -x check
```

### 2. Use `sccache` for Shared Cache
```bash
cargo install sccache
export RUSTC_WRAPPER=sccache
```

### 3. Optimize Linking (macOS)
```bash
brew install llvm
# Already configured in .cargo/config.toml
```

### 4. Use Rust Analyzer
Editor integration provides instant feedback without rebuilding.

## Benchmark Your Builds

```bash
# Time a clean build
cargo clean && time cargo build

# Time an incremental build
touch ggen-ai/src/lib.rs && time cargo build

# Expected results:
# Clean:       60-90 seconds
# Incremental: 2-3 seconds
# No changes:  <1 second
```

## Cache Sizes

Typical cache sizes:
- `target/debug/`: 2-3 GB (all artifacts)
- `target/debug/incremental/`: 500-800 MB (incremental cache)
- `~/.cargo/registry/`: 1-2 GB (dependency caches)

## Monitoring Build Times

```bash
# Install cargo-bloat to find slow crates
cargo install cargo-bloat

# Check compile times
cargo build --timings
# Opens HTML report with timing graph
```

## When to Clean

Only run `cargo clean` when:
- Switching between debug/release profiles
- Dependency conflicts occur
- Cache corruption suspected
- Disk space critically low

## Environment Variables

```bash
# Enable debug output
export CARGO_LOG=cargo::core::compiler::fingerprint=trace

# Increase parallel jobs
export CARGO_BUILD_JOBS=16

# Use local cache
export CARGO_TARGET_DIR=/path/to/fast/ssd
```

## Summary

The build system is now optimized for:
1. **First build**: Compiles everything once (~1-2 min)
2. **Subsequent builds**: Uses cache (~2-3 sec)
3. **No-change rebuilds**: Instant (<1 sec)

**Key**: Never delete Cargo.lock or target/ unless absolutely necessary!
