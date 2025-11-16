<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Core Team Documentation Tool Recommendation](#core-team-documentation-tool-recommendation)
  - [Decision Framework](#decision-framework)
    - [Core Team Values (from codebase analysis)](#core-team-values-from-codebase-analysis)
  - [Core Team Choice: **mdBook + rustdoc + GitHub Pages**](#core-team-choice-mdbook--rustdoc--github-pages)
    - [Primary Stack](#primary-stack)
  - [Why NOT Other Tools?](#why-not-other-tools)
    - [❌ MkDocs / Sphinx](#-mkdocs--sphinx)
    - [❌ Docusaurus](#-docusaurus)
    - [❌ Read the Docs / Netlify / Vercel](#-read-the-docs--netlify--vercel)
  - [Implementation Plan](#implementation-plan)
    - [Phase 1: mdBook Setup](#phase-1-mdbook-setup)
    - [Phase 2: Convert Diataxis to mdBook](#phase-2-convert-diataxis-to-mdbook)
    - [Phase 3: GitHub Actions Workflow](#phase-3-github-actions-workflow)
    - [Phase 4: Integration with cargo make](#phase-4-integration-with-cargo-make)
  - [Benefits Alignment](#benefits-alignment)
    - [✅ Rust-first](#-rust-first)
    - [✅ Official tools](#-official-tools)
    - [✅ 80/20 principle](#-8020-principle)
    - [✅ Deterministic](#-deterministic)
    - [✅ Fast builds](#-fast-builds)
    - [✅ cargo make integration](#-cargo-make-integration)
    - [✅ Production-ready](#-production-ready)
  - [Comparison with Alternatives](#comparison-with-alternatives)
  - [Recommendation Summary](#recommendation-summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Core Team Documentation Tool Recommendation

## Decision Framework

Based on ggen's core principles and values, here's what the core team would choose:

### Core Team Values (from codebase analysis)

1. **Rust-first**: Everything is Rust-native
2. **Official tools**: Prefer official/standard Rust tools
3. **80/20 principle**: Maximum value with minimal complexity
4. **Deterministic**: Byte-identical, reproducible builds
5. **Fast builds**: SLOs emphasize speed (≤15s first build, ≤2s incremental)
6. **Zero-cost abstractions**: Prefer Rust-native tools
7. **cargo make workflows**: Everything goes through `cargo make`
8. **Type safety**: Rust's type system
9. **Production-ready**: Real implementations, no placeholders
10. **Performance**: <2s generation, fast builds

## Core Team Choice: **mdBook + rustdoc + GitHub Pages**

### Primary Stack

**1. mdBook** (for guides/tutorials/reference)
- ✅ **Rust-native**: Written in Rust, fits ecosystem
- ✅ **Official Rust tool**: Used by Rust project itself
- ✅ **Fast**: Compiles quickly, deterministic output
- ✅ **Simple**: Minimal configuration, works out of the box
- ✅ **cargo make integration**: Can be added to `Makefile.toml`
- ✅ **Deterministic**: Same inputs → identical outputs
- ✅ **Markdown**: Works with existing Diataxis structure

**2. rustdoc** (for API documentation)
- ✅ **Official**: Bundled with Rust, zero configuration
- ✅ **Automatic**: Generates from code comments
- ✅ **Deterministic**: Byte-identical output
- ✅ **Fast**: Part of `cargo doc`, already in workflow
- ✅ **Type-safe**: Integrates with Rust's type system

**3. GitHub Pages** (for hosting)
- ✅ **Free**: No cost for public repos
- ✅ **Simple**: Works with GitHub Actions
- ✅ **Deterministic**: Static files, reproducible
- ✅ **No external dependencies**: Uses existing GitHub infrastructure
- ✅ **Fast**: CDN-backed, global distribution
- ✅ **cargo make integration**: Can be automated in workflows

## Why NOT Other Tools?

### ❌ MkDocs / Sphinx
- **Not Rust-native**: Requires Python
- **External dependency**: Adds complexity
- **Slower builds**: Python runtime overhead
- **Doesn't align with Rust-first principle**

### ❌ Docusaurus
- **Not Rust-native**: Requires Node.js
- **External dependency**: Adds complexity
- **Overkill**: More features than needed
- **Doesn't align with 80/20 principle**

### ❌ Read the Docs / Netlify / Vercel
- **External service**: Adds dependency
- **Not deterministic**: External build environment
- **Overkill**: GitHub Pages sufficient
- **Doesn't align with simplicity principle**

## Implementation Plan

### Phase 1: mdBook Setup

```bash
# Add to Makefile.toml
[tasks.docs-build]
description = "Build documentation with mdBook"
command = "mdbook"
args = ["build", "docs"]

[tasks.docs-serve]
description = "Serve documentation locally"
command = "mdbook"
args = ["serve", "docs", "--open"]

[tasks.docs-watch]
description = "Watch and rebuild documentation"
command = "mdbook"
args = ["watch", "docs"]
```

### Phase 2: Convert Diataxis to mdBook

Current structure:
```
docs/
├── tutorials/
├── how-to-guides/
├── reference/
└── explanations/
```

mdBook structure:
```
docs/
├── book.toml          # mdBook configuration
├── src/
│   ├── SUMMARY.md     # Table of contents
│   ├── tutorials/
│   ├── how-to-guides/
│   ├── reference/
│   └── explanations/
└── book/              # Generated output
```

### Phase 3: GitHub Actions Workflow

```yaml
name: Deploy Documentation

on:
  push:
    branches: [main]
    paths:
      - 'docs/**'
      - 'src/**'

jobs:
  deploy-docs:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
      
      - name: Install mdBook
        run: cargo install mdbook
      
      - name: Build API docs
        run: cargo doc --no-deps
      
      - name: Build mdBook
        run: mdbook build docs/
      
      - name: Deploy to GitHub Pages
        uses: peaceiris/actions-gh-pages@v3
        with:
          github_token: ${{ secrets.GITHUB_TOKEN }}
          publish_dir: |
            target/doc
            docs/book
```

### Phase 4: Integration with cargo make

Add to `Makefile.toml`:
```toml
[tasks.docs]
description = "Build all documentation (API + guides)"
dependencies = ["docs-api", "docs-book"]

[tasks.docs-api]
description = "Build API documentation"
command = "cargo"
args = ["doc", "--no-deps", "--open"]

[tasks.docs-book]
description = "Build mdBook documentation"
command = "mdbook"
args = ["build", "docs"]

[tasks.docs-serve]
description = "Serve documentation locally"
command = "mdbook"
args = ["serve", "docs", "--open"]
```

## Benefits Alignment

### ✅ Rust-first
- mdBook: Written in Rust
- rustdoc: Official Rust tool
- GitHub Pages: Uses existing GitHub (no new stack)

### ✅ Official tools
- mdBook: Used by Rust project
- rustdoc: Official Rust documentation tool
- GitHub Pages: Official GitHub feature

### ✅ 80/20 principle
- **20% effort**: Simple setup, minimal configuration
- **80% value**: Professional documentation site, search, navigation

### ✅ Deterministic
- mdBook: Same inputs → identical outputs
- rustdoc: Deterministic HTML generation
- GitHub Pages: Static files, reproducible

### ✅ Fast builds
- mdBook: Fast compilation (<5s typical)
- rustdoc: Part of cargo, already fast
- GitHub Pages: Static deployment, fast

### ✅ cargo make integration
- All tasks via `cargo make docs-*`
- Consistent with existing workflow
- Timeout protection built-in

### ✅ Production-ready
- mdBook: Battle-tested (used by Rust project)
- rustdoc: Official, production-ready
- GitHub Pages: Reliable, CDN-backed

## Comparison with Alternatives

| Criteria | mdBook + rustdoc | MkDocs | Docusaurus | Sphinx |
|----------|-----------------|--------|------------|--------|
| **Rust-native** | ✅ Yes | ❌ Python | ❌ Node.js | ❌ Python |
| **Official** | ✅ Yes | ❌ No | ❌ No | ❌ No |
| **Setup complexity** | ⭐ Easy | ⭐⭐ Medium | ⭐⭐⭐ Complex | ⭐⭐⭐ Complex |
| **Build speed** | ⭐⭐⭐ Fast | ⭐⭐ Medium | ⭐⭐ Medium | ⭐ Slow |
| **Deterministic** | ✅ Yes | ✅ Yes | ✅ Yes | ✅ Yes |
| **cargo make** | ✅ Easy | ⚠️ Possible | ⚠️ Possible | ⚠️ Possible |
| **80/20 value** | ✅ High | ⚠️ Medium | ⚠️ Medium | ⚠️ Low |

## Recommendation Summary

**The core team would choose:**

1. **mdBook** for guides/tutorials/reference (Rust-native, official, fast)
2. **rustdoc** for API documentation (official, automatic, zero config)
3. **GitHub Pages** for hosting (free, simple, deterministic)

**Why this combination:**
- Aligns with all core team values
- Minimal complexity (80/20 principle)
- Rust-native stack
- Official tools
- Fast, deterministic builds
- Integrates with existing `cargo make` workflow
- Production-ready and battle-tested

**Next steps:**
1. Install mdBook: `cargo install mdbook`
2. Create `docs/book.toml` configuration
3. Convert Diataxis structure to mdBook format
4. Add `cargo make` tasks for documentation
5. Set up GitHub Actions workflow
6. Deploy to GitHub Pages

This choice maximizes value while maintaining simplicity, aligning perfectly with ggen's core principles.

