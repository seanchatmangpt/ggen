# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**rgen** is a deterministic, language-agnostic code generation framework that treats software artifacts as projections of RDF knowledge graphs. It uses template-based generation with semantic metadata (RDF, SPARQL) to produce reproducible, multi-language code from a single ontology.

## Build and Development Commands

### Critical: ALWAYS Use cargo-make (NEVER direct cargo commands)

**The `.cursorrules` file is NON-NEGOTIABLE: ALL development workflows MUST use `cargo make` commands.**

Direct cargo commands (`cargo build`, `cargo test`, `cargo fmt`, `cargo clippy`) are **FORBIDDEN**.

### Essential cargo-make Commands

**Quick Development Workflow:**
```bash
cargo make quick      # Format and run unit tests (alias: q)
cargo make dev        # Format, lint (allow warnings), and test
cargo make check      # Check code without building (alias: c)
cargo make build      # Build in debug mode (alias: b)
```

**Testing:**
```bash
cargo make test                 # Run all tests (alias: t)
cargo make test-unit            # Unit tests only
cargo make test-integration     # Integration tests only
cargo make test-single-threaded # Single-threaded for determinism
cargo make deterministic        # Fixed seeds + single-threaded (alias: det)
cargo make test-release         # Tests in release mode
cargo make test-coverage        # Generate coverage report (alias: cov)
```

**Code Quality:**
```bash
cargo make fmt                  # Format all code (alias: f)
cargo make fmt-check            # Check formatting
cargo make lint                 # Strict clippy (denies all warnings)
cargo make lint-allow           # Clippy with warnings allowed (alias: l)
cargo make audit                # Security vulnerability scan
cargo make deny                 # Check licenses and deps
```

**Build and Release:**
```bash
cargo make build-release        # Release build (alias: r)
cargo make release              # Full release with all checks
cargo make release-check        # Verify release readiness
```

**CI/CD Workflows:**
```bash
cargo make ci                   # Full CI workflow (alias: full)
cargo make pre-commit           # Pre-commit checks (alias: prep)
cargo make pre-push             # Pre-push comprehensive checks (alias: push)
```

**Performance and SLOs:**
```bash
cargo make slo-check            # Verify SLO compliance (alias: slo)
cargo make bench                # Run benchmarks
cargo make profile              # Profile the application
cargo make flamegraph           # Generate flamegraph
```

**Template and RDF Validation:**
```bash
cargo make validate-templates   # Validate all templates (alias: tmpl)
cargo make validate-rdf         # Validate RDF graphs (alias: rdf)
cargo make validate-outputs     # Check deterministic outputs (alias: val)
```

**Development Tools:**
```bash
cargo make watch                # Watch and run tests (alias: w)
cargo make watch-build          # Watch and build (alias: wb)
cargo make debug                # Run with full debug logging
cargo make completions          # Generate shell completions (alias: comp)
```

**Workspace Management:**
```bash
cargo make test-cli             # CLI-specific tests
cargo make test-core            # Core module tests
cargo make test-utils           # Utils module tests
```

**Maintenance:**
```bash
cargo make clean                # Clean build artifacts
cargo make clean-all            # Clean everything including caches (alias: cleanup)
cargo make update               # Update dependencies
cargo make fix                  # Auto-fix common issues (alias: fixup)
cargo make maintenance          # Full maintenance tasks (alias: maint)
```

**Packaging:**
```bash
cargo make package              # Create distribution packages (alias: pkg)
cargo make cross-build          # Cross-compile for multiple targets
```

**Marketplace:**
```bash
cargo make marketplace-demo     # Run marketplace demo (alias: demo)
cargo make registry-test        # Test registry functionality (alias: reg)
```

### rgen CLI Commands (Use with cargo make)

```bash
# Run rgen CLI via cargo make
cargo make run -- <command> [args]

# Common rgen commands:
cargo make run -- gen <template> --vars key=value     # Generate from template
cargo make run -- search <query>                      # Search marketplace
cargo make run -- add <rpack-id>                      # Install rpack
cargo make run -- packs                               # List installed rpacks
cargo make run -- validate <template>                 # Validate template
cargo make run -- list                                # List templates
cargo make run -- show <template>                     # Show template details
cargo make run -- completion <shell>                  # Shell completions
cargo make run -- hazard                              # Generate hazard report
```

## Architecture

### Workspace Structure
```
rgen/
├── src/main.rs           # Binary entry point, panic handlers, app initialization
├── cli/                  # Argument parsing, subcommand dispatch
│   └── src/
│       ├── lib.rs        # Clap CLI definition
│       └── cmds/         # Each subcommand implementation
├── core/                 # Domain logic for generation
│   └── src/
│       ├── pipeline.rs   # Template rendering pipeline
│       ├── template.rs   # Frontmatter + body parsing
│       ├── graph.rs      # RDF graph with SPARQL caching
│       ├── generator.rs  # High-level generation orchestration
│       ├── registry.rs   # Marketplace client
│       ├── resolver.rs   # Template source resolution
│       └── rpack.rs      # Rpack manifest handling
├── utils/                # Cross-cutting concerns
│   └── src/
│       ├── app_config.rs # Configuration management
│       ├── logger.rs     # Structured logging
│       └── error.rs      # Error types
├── templates/            # Local template files (.tmpl)
├── docs/                 # Documentation
└── tests/                # Integration tests
```

### Key Components

**Pipeline (core/src/pipeline.rs)**
- Orchestrates template rendering with Tera
- Manages RDF graph state and SPARQL execution
- Renders frontmatter variables, then body
- Produces `Plan` objects for dry-run or apply

**Template (core/src/template.rs)**
- Parses YAML frontmatter + template body
- Supports `to`, `from`, `inject`, `vars`, `rdf`, `sparql`, `determinism` fields
- Renders frontmatter first to resolve template variables
- Supports injection modes: `prepend`, `append`, `before`, `after`, `at_line`

**Graph (core/src/graph.rs)**
- Thread-safe Oxigraph wrapper with SPARQL caching
- LRU cache for query plans and results
- Epoch-based cache invalidation on graph mutations
- Supports Turtle, N-Triples, RDF/XML formats

**Registry (core/src/registry.rs)**
- Marketplace client for discovering and installing rpacks
- Handles versioning, dependencies, lockfiles
- Local cache management for installed rpacks

## Template System

Templates use YAML frontmatter to declare:
- `to`: Output path (supports Tera variables like `{{name}}.rs`)
- `from`: Alternative source file (overrides body)
- `vars`: Default variables
- `rdf`/`rdf_inline`: RDF data sources (Turtle format)
- `sparql`: SPARQL queries to extract variables
- `prefixes`: Namespace prefixes for SPARQL
- `base`: Base IRI for relative URIs
- `determinism`: Seed for reproducible generation
- `inject`: Enable injection mode for modifying existing files
- `force`: Overwrite existing files
- `unless_exists`: Skip if file exists

### Injection Modes
- `prepend`: Add content at start of file
- `append`: Add content at end of file
- `before: "pattern"`: Insert before matching line
- `after: "pattern"`: Insert after matching line
- `at_line: N`: Insert at specific line number
- `skip_if: "pattern"`: Skip if pattern exists (idempotency)
- `idempotent`: Skip if content already exists

### Shell Hooks
- `sh_before`: Run before file write
- `sh_after`: Run after file write

## Determinism

rgen ensures byte-identical output given:
- Same template + frontmatter
- Same RDF graph data
- Same seed in `determinism` field
- Same variable inputs

Use `--test-threads=1` for deterministic test execution.

## RDF and SPARQL Integration

Templates can embed RDF knowledge graphs:
```yaml
---
prefixes:
  ex: "http://example.org/"
  rdf: "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
base: "http://example.org/"
rdf_inline:
  - "@prefix ex: <http://example.org/> . ex:{{name}} a ex:Person ."
sparql:
  get_name: "SELECT ?name WHERE { ?person ex:name ?name }"
---
```

SPARQL queries execute within templates using the `sparql()` Tera function.

## Error Handling

- Libraries use typed errors via `utils::error::Result`
- Binary uses `anyhow` for error context
- No `unwrap()` or `expect()` in library code
- Panic handlers: `better-panic` (debug), `human-panic` (release)

## Logging

Uses structured logging via `utils::logger`:
```rust
log::info!("Generated file");
log::debug!("Processing template: {}", path);
log::error!("Failed to parse: {}", err);
```

Avoid `println!` in library code (CLI output is acceptable).

## Testing Strategy

- Unit tests colocated with implementation
- Integration tests in `/tests`
- Use `insta` for snapshot testing of generated output
- Fixed seeds for determinism tests
- Mock filesystem and network in tests
- Property tests with `proptest` for parsers and RDF logic

**ALWAYS use cargo-make for testing:**
```bash
cargo make test                 # Run all tests
cargo make test-single-threaded # Single-threaded execution
cargo make deterministic        # Fixed seeds + single-threaded (RNG_SEED=42)
cargo make test-unit            # Unit tests only
cargo make test-integration     # Integration tests only
cargo make test-cli             # CLI-specific tests
cargo make test-core            # Core module tests
cargo make test-release         # Tests in release mode
```

## Security Considerations

- Validate all file paths (prevent traversal)
- Sanitize template inputs
- No arbitrary code execution in templates
- Audit external RDF sources
- Shell hooks run in sandboxed environment

## Performance SLOs

- First build: ≤ 15s
- Incremental build: ≤ 2s
- RDF processing: ≤ 5s for 1k+ triples
- Generation memory: ≤ 100MB
- CLI scaffolding: ≤ 3s end-to-end
- 100% reproducible outputs

## Marketplace (rpacks)

Rpacks are versioned, reusable template packages with:
- Templates with frontmatter
- RDF schemas and SHACL shapes
- SPARQL queries
- Dependencies on other rpacks

Install from marketplace:
```bash
rgen add io.rgen.rust.cli-subcommand
rgen gen io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello
```

## Development Notes

### Non-Negotiable Rules from .cursorrules

1. **NEVER use direct cargo commands** - Always use `cargo make`
2. **NEVER use `cargo fmt`, `cargo clippy`, `cargo test`, `cargo build` directly**
3. **ALWAYS use `cargo make` for ALL development workflows**
4. No `unwrap()` or `expect()` in library code
5. Deny warnings in production code
6. No hardcoded outputs in production paths
7. No TODOs or placeholders in library code
8. All errors must be handled explicitly

### Workspace and Tooling

- Workspace members: `utils`, `cli`, `core`
- MSRV tracked in Cargo.toml
- Use Rust stable toolchain
- Enable LTO in release builds (configured in Cargo.toml)
- Profile with `cargo make profile` or `cargo make flamegraph`
- Minimize allocations in pipeline (use `&str` where possible)
- Cache repeated SPARQL queries and template renders

### Definition of Done (.cursorrules)

**Use `cargo make ci` to verify all requirements:**
- ✅ `cargo make fmt` - Code formatted
- ✅ `cargo make lint` - Clippy clean (strict)
- ✅ `cargo make test` - All tests passing
- ✅ `cargo make slo-check` - SLO compliance verified
- ✅ `cargo make audit` - No security vulnerabilities
- ✅ Clear, actionable error messages
- ✅ Documentation and examples updated
- ✅ Cross-platform validated
- ✅ Deterministic outputs verified
