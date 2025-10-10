# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**ggen** is a deterministic, language-agnostic code generation framework that treats software artifacts as projections of RDF knowledge graphs. It uses template-based generation with semantic metadata (RDF, SPARQL) to produce reproducible, multi-language code from a single ontology.

## Critical Rule: Use cargo-make for ALL Development

**NEVER use direct cargo commands (`cargo build`, `cargo test`, `cargo fmt`, `cargo clippy`). ALWAYS use `cargo make` commands.**

This is non-negotiable per `.cursorrules`. All development workflows must use the comprehensive `cargo-make` task system.

## Essential Commands

### Quick Development Workflow
```bash
cargo make quick      # Format and run unit tests (alias: q)
cargo make dev        # Format, lint (allow warnings), and test
cargo make check      # Check code without building (alias: c)
cargo make build      # Build in debug mode (alias: b)
```

### Testing
```bash
cargo make test                 # Run all tests (alias: t)
cargo make test-unit            # Unit tests only
cargo make test-integration     # Integration tests only
cargo make test-single-threaded # Single-threaded for determinism
cargo make deterministic        # Fixed seeds + single-threaded (alias: det)
cargo make test-coverage        # Generate coverage report (alias: cov)
```

### Code Quality
```bash
cargo make fmt                  # Format all code (alias: f)
cargo make lint                 # Strict clippy (denies all warnings)
cargo make lint-allow           # Clippy with warnings allowed (alias: l)
cargo make audit                # Security vulnerability scan
```

### CI/CD Workflows
```bash
cargo make ci                   # Full CI workflow (alias: full)
cargo make pre-commit           # Pre-commit checks (alias: prep)
cargo make pre-push             # Pre-push comprehensive checks (alias: push)
```

### ggen CLI Commands (via cargo make)
```bash
cargo make run -- gen <template> --vars key=value     # Generate from template
cargo make run -- search <query>                      # Search marketplace
cargo make run -- add <gpack-id>                      # Install gpack
cargo make run -- packs                               # List installed gpacks
cargo make run -- list                                # List templates
cargo make run -- show <template>                     # Show template details
```

## Workspace Structure

```
ggen/
├── src/main.rs           # Binary entry point with panic handlers
├── cli/                  # CLI layer
│   └── src/
│       ├── lib.rs        # Clap CLI definition
│       └── cmds/         # Subcommand implementations
│           ├── gen.rs    # Code generation
│           ├── search.rs # Marketplace search
│           ├── add.rs    # Install gpacks
│           └── ...       # Other commands
├── ggen-core/            # Core domain logic
│   └── src/
│       ├── pipeline.rs   # Template rendering pipeline
│       ├── template.rs   # Frontmatter + body parsing
│       ├── graph.rs      # RDF graph with SPARQL caching
│       ├── generator.rs  # High-level generation orchestration
│       ├── registry.rs   # Marketplace client
│       ├── resolver.rs   # Template source resolution
│       ├── gpack.rs      # Gpack manifest handling
│       └── inject.rs     # File injection modes
├── utils/                # Cross-cutting concerns
│   └── src/
│       ├── app_config.rs # Configuration management
│       ├── logger.rs     # Structured logging
│       └── error.rs      # Error types
├── templates/            # Local template files (.tmpl)
└── tests/                # Integration tests
```

## Architecture

### Key Components

**Pipeline (ggen-core/src/pipeline.rs)**
- Orchestrates template rendering with Tera
- Manages RDF graph state and SPARQL execution
- Renders frontmatter variables first, then body
- Produces `Plan` objects for dry-run or apply
- Key methods: `new()`, `render_file()`, `register_prefixes()`

**Template (ggen-core/src/template.rs)**
- Parses YAML frontmatter + template body
- Frontmatter fields: `to`, `from`, `inject`, `vars`, `rdf`, `sparql`, `determinism`, `force`, `unless_exists`
- Supports injection modes: `prepend`, `append`, `before`, `after`, `at_line`
- Shell hooks: `sh_before`, `sh_after`
- Key methods: `parse()`, `render_frontmatter()`, `render()`, `process_graph()`

**Graph (ggen-core/src/graph.rs)**
- Thread-safe Oxigraph wrapper with SPARQL caching
- LRU cache for query plans and results
- Epoch-based cache invalidation on graph mutations
- Supports Turtle, N-Triples, RDF/XML formats
- Key methods: `new()`, `insert_turtle()`, `query()`, `len()`

**Registry (ggen-core/src/registry.rs)**
- Marketplace client for discovering and installing gpacks
- Handles versioning, dependencies, lockfiles
- Local cache management for installed gpacks
- Key methods: `search()`, `resolve()`, `install()`

**Generator (ggen-core/src/generator.rs)**
- High-level orchestration of generation workflow
- Coordinates Pipeline, Resolver, and Registry
- Key type: `GenContext` with vars, template paths, and options

### Data Flow

1. **CLI Layer** (`cli/`) parses arguments via Clap
2. **Command Handler** (`cli/src/cmds/`) processes subcommand
3. **Generator** (`ggen-core/`) orchestrates:
   - **Resolver** locates template (local, gpack, or marketplace)
   - **Pipeline** loads template and parses frontmatter
   - Frontmatter rendered first to resolve `to` field and variables
   - RDF data loaded into **Graph** from `rdf`/`rdf_inline` fields
   - SPARQL queries executed and results injected into context
   - Template body rendered with full context
   - **Plan** created with output path and content
4. **Plan** applied to write file or show diff (dry-run)

## Template System

Templates use YAML frontmatter to declare metadata:

```yaml
---
to: "{{name}}.rs"                        # Output path (supports Tera vars)
from: "source.rs"                        # Alternative source (overrides body)
vars:                                    # Default variables
  author: "Sean Chatman"
rdf_inline:                              # Inline RDF data
  - "@prefix ex: <http://example.org/> ."
sparql:                                  # SPARQL queries
  get_name: "SELECT ?name WHERE { ?person ex:name ?name }"
prefixes:                                # Namespace prefixes
  ex: "http://example.org/"
base: "http://example.org/"              # Base IRI
determinism: 42                          # Seed for reproducibility
inject: true                             # Enable injection mode
force: false                             # Overwrite existing files
unless_exists: false                     # Skip if file exists
---
Template body here with {{ variables }}
```

### Injection Modes
- `prepend: true` - Add content at start of file
- `append: true` - Add content at end of file
- `before: "pattern"` - Insert before matching line
- `after: "pattern"` - Insert after matching line
- `at_line: N` - Insert at specific line number
- `skip_if: "pattern"` - Skip if pattern exists (idempotency)
- `idempotent: true` - Skip if content already exists

### Shell Hooks
- `sh_before: "command"` - Run before file write
- `sh_after: "cargo fmt"` - Run after file write

## Determinism

ggen ensures byte-identical output given:
- Same template + frontmatter
- Same RDF graph data
- Same seed in `determinism` field
- Same variable inputs

Use `cargo make deterministic` (sets `RNG_SEED=42` and `--test-threads=1`) for deterministic test execution.

## RDF and SPARQL Integration

Templates can embed RDF knowledge graphs and query them:

```yaml
---
prefixes:
  ex: "http://example.org/"
base: "http://example.org/"
rdf_inline:
  - "@prefix ex: <http://example.org/> . ex:{{name}} a ex:Person ; ex:age 30 ."
sparql:
  get_age: "SELECT ?age WHERE { ex:{{name}} ex:age ?age }"
---
Person age: {{ sparql(query=get_age, var="age") }}
```

SPARQL queries execute within templates using the `sparql()` Tera function.

## Error Handling

- Libraries use typed errors via `utils::error::Result`
- Binary uses `anyhow` for error context
- No `unwrap()` or `expect()` in library code (per `.cursorrules`)
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

**ALWAYS use cargo-make for testing:**
```bash
cargo make test                 # Run all tests
cargo make test-single-threaded # Single-threaded execution
cargo make deterministic        # Fixed seeds (RNG_SEED=42)
cargo make test-cli             # CLI-specific tests
cargo make test-core            # Core module tests
```

## Performance SLOs

- First build: ≤ 15s
- Incremental build: ≤ 2s
- RDF processing: ≤ 5s for 1k+ triples
- Generation memory: ≤ 100MB
- CLI scaffolding: ≤ 3s end-to-end
- 100% reproducible outputs

Verify with: `cargo make slo-check`

## Marketplace (gpacks)

Gpacks are versioned, reusable template packages with:
- Templates with frontmatter
- RDF schemas and SHACL shapes
- SPARQL queries
- Dependencies on other gpacks

```bash
cargo make run -- search rust
cargo make run -- add io.ggen.rust.cli-subcommand
cargo make run -- gen io.ggen.rust.cli-subcommand:template.tmpl name=hello
```

## Development Rules from .cursorrules

1. **NEVER use direct cargo commands** - Always use `cargo make`
2. No `unwrap()` or `expect()` in library code
3. Deny warnings in production code
4. No hardcoded outputs in production paths
5. No TODOs or placeholders in library code
6. All errors must be handled explicitly

## Definition of Done

Run `cargo make ci` to verify all requirements:
- ✅ `cargo make fmt` - Code formatted
- ✅ `cargo make lint` - Clippy clean (strict)
- ✅ `cargo make test` - All tests passing
- ✅ `cargo make slo-check` - SLO compliance verified
- ✅ `cargo make audit` - No security vulnerabilities
- ✅ Clear, actionable error messages
- ✅ Deterministic outputs verified
