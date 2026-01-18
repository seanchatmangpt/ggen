# ggen: Specification-Driven Code Generation (v6.0.0)

**Core Equation**: $A = \mu(O)$ — Code (A) precipitates from RDF ontology (O) via five-stage transformation pipeline (μ).

**Version**: 6.0.0 (Production-Ready Core) | **Release**: January 2026

---

## Three Paradigms (Non-Negotiable)

| Paradigm | Practice |
|----------|----------|
| **Big Bang 80/20** | Verify `.specify/*.ttl` closure (100%) before code gen. Single-pass. Receipts prove closure. |
| **EPIC 9** | Non-trivial tasks: 10 parallel agents → collision detection → convergence synthesis. |
| **Deterministic Receipts** | Evidence replaces narrative: `[Receipt] cargo make test: ✓ 347/347, <30s` |

---

## Constitutional Rules (Poka-Yoke)

```
🔴 RED (Compilation/test error)        → STOP immediately (Andon halt)
🟡 YELLOW (Warnings/deprecations)      → Investigate before release
🟢 GREEN (All checks pass)             → Proceed safely
```

| Rule | Requirement |
|------|-------------|
| **Cargo Make Only** | `cargo make [target]` always (never raw `cargo`). Enforces SLOs, timeouts, quality gates. |
| **Result<T,E>** | Production: `Result<T,E>` throughout. Tests: `unwrap()` OK. |
| **No Unwrap/Expect** | Zero in production code. Language-enforced via clippy `-D warnings`. |
| **RDF is Truth** | Edit `.specify/*.ttl` (source). Never edit `.md` (generated artifacts). |
| **Type-First** | Constraints in types, compiler verifies. NewType for domains. Generic + zero-cost. |
| **TTL is Immutable** | Once closed, don't iterate — fix source, regenerate. |

---

## Essential Commands (v6 SLO Targets)

```bash
# V6 UNIFIED COMMAND - REPLACES ALL PREVIOUS GENERATE COMMANDS
ggen sync                           # Full pipeline (μ₁-μ₅): Normalize → Extract → Emit → Canonicalize → Receipt
ggen sync --dry_run true            # Preview changes without writing files
ggen sync --validate_only true      # Pre-flight quality gates only (no generation)
ggen sync --audit true              # Generate cryptographic audit trail
ggen sync --watch true              # Continuous regeneration on file changes
ggen sync --force true --audit true # Safe destructive overwrite with audit

ggen init                           # Initialize new ggen project with manifest

# CARGO MAKE TARGETS (POKA-YOKE ENFORCED)
cargo make check       # <5s    (compile check, warnings-as-errors)
cargo make test-unit   # <16s   (fast feedback, Chicago TDD)
cargo make test        # <30s   (full test suite with timeout enforcement)
cargo make lint        # <60s   (clippy -D warnings, rustfmt)
cargo make pre-commit  # <2min  (check → lint → test-unit quality gate)

# SPECIFICATION WORKFLOW (RDF-FIRST)
cargo make speckit-check     # Verify TTL specs exist for current branch
cargo make speckit-validate  # SHACL validation of .specify/*.ttl
cargo make speckit-render    # Regenerate all markdown from TTL sources
cargo make speckit-full      # Full workflow: validate + render
```

---

## File Organization

```
ggen/
├── .specify/                  # RDF SPECIFICATIONS (SOURCE OF TRUTH)
│   ├── specs/                 # Feature specs (.ttl = source, .md = generated)
│   │   └── NNN-feature/
│   │       ├── feature.ttl    # User stories, requirements (EDIT THIS)
│   │       ├── entities.ttl   # Domain entities (EDIT THIS)
│   │       ├── plan.ttl       # Architecture plan (EDIT THIS)
│   │       ├── tasks.ttl      # Task breakdown (EDIT THIS)
│   │       ├── spec.md        # Generated (DO NOT EDIT)
│   │       └── evidence/      # Test artifacts, receipts
│   ├── templates/             # Tera templates for spec → markdown
│   │   ├── spec.tera          # SPARQL → Markdown transformation
│   │   └── rdf-helpers/       # TTL templates for creating instances
│   ├── memory/                # Project memory (constitution, decisions)
│   └── *.ttl                  # Ontology files (CLI, naming, constraints)
│
├── crates/*/                  # RUST SOURCE (27 CRATES)
│   ├── src/                   # Production code (NO unwrap/expect)
│   └── tests/                 # Chicago TDD tests (AAA pattern, real objects)
│
├── .claude/                   # CLAUDE CODE CONFIGURATION
│   ├── settings.json          # Permissions, environment settings
│   ├── agents/                # Specialized agent definitions (8 agents)
│   ├── hooks/                 # Pre/post hooks for tool safety, validation
│   └── skills/                # Domain skills (11 skills)
│       ├── bb80-*/            # Big Bang 80/20 workflow agents
│       ├── cargo-make-protocol/
│       ├── chicago-tdd-pattern/
│       ├── poka-yoke-patterns/
│       └── rdf-ontologies/
│
├── Makefile.toml              # Cargo Make targets (Poka-Yoke enforced)
├── Cargo.toml                 # Workspace configuration (27 members)
├── docs/, templates/, benches/
└── examples/                  # 40+ production-grade examples

```

**Critical Rule**: Never create working files in root directory. Use appropriate subdirectories.

---

## Stack (v6.0.0)

| Component | Version | Purpose |
|-----------|---------|---------|
| Rust | 1.91.1 | Core language (type-safe, zero-cost abstractions) |
| Tokio | 1.47 | Async runtime (full feature set) |
| Oxigraph | 0.5.1 | RDF store (Turtle, SPARQL, inference) |
| Tera | 1.20 | Template engine (SPARQL-aware code generation) |
| Serde | 1.0 | Serialization (JSON, YAML, TOML) |
| Clap | 4.5 | CLI framework (derive API) |
| genai | 0.4 | Multi-provider LLM client (GPT-4, Claude, local models) |
| chicago-tdd-tools | 1.4.0 | AAA testing (Arrange/Act/Assert, real objects, no mocks) |
| proptest | 1.8 | Property-based testing |
| criterion | 0.7 | Performance benchmarking |
| testcontainers | 0.25 | Integration testing with Docker |
| OpenTelemetry | 0.21 | Observability (traces, metrics, logs) |

---

## Crates (27 Total)

### Core System (8)
- **ggen-core**: RDF processing, SPARQL engine, template rendering
- **ggen-cli**: CLI entry point, command routing, user interface
- **ggen-domain**: Domain models, business logic, validation
- **ggen-utils**: Logging, error handling, shared utilities
- **ggen-config**: Configuration management (TOML, env vars)
- **ggen-macros**: Procedural macros for code generation
- **ggen-node**: Node.js bindings (NAPI-RS)
- **ggen-dod**: Data-oriented design patterns

### CLI & Validation (3)
- **ggen-cli-validation**: Pre/post-flight validation, quality gates
- **ggen-config-clap**: Clap integration for configuration
- **ggen-spec-validator**: SHACL validation for specifications

### Testing & Quality (3)
- **ggen-test-audit**: Test quality metrics, mutation testing analysis
- **ggen-test-opt**: Test optimization, deduplication, performance
- **ggen-e2e**: End-to-end testing with testcontainers

### Marketplace (1)
- **ggen-marketplace**: Package discovery, FMEA risk analysis, template registry

### RevOps / Monetization (4)
- **ggen-api**: REST API layer for SaaS platform
- **ggen-auth**: Authentication (OAuth2, JWT, API keys)
- **ggen-payments**: Payment processing (Stripe integration)
- **ggen-saas**: Multi-tenant management, quota enforcement

### AI Orchestration (1)
- **ggen-ai**: Multi-provider LLM integration (GPT-4, Claude, local models), DSPy predictor, constraint calculus

### KNHK Systems (ETL + KGC-4D + Workflow) (6)
- **knhk-etl**: Extract-Transform-Load pipeline for knowledge graph construction
- **knhk-hot**: C FFI hot-path optimization (performance-critical operations)
- **knhk-connectors**: Connector registry (Kafka, HTTP, databases)
- **knhk-lockchain**: Merkle-linked receipt storage (cryptographic provenance)
- **knhk-otel**: OpenTelemetry integration (tracing, metrics)
- **knhk-orchestrator**: Integration bridge (ETL → KGC-4D → Workflow Engine)

---

## Automatic (No Reminders Needed)

I implement these by default without being asked:
- `Result<T,E>` for all fallible operations
- Zero `unwrap/expect` in production code (tests OK)
- Chicago TDD pattern (AAA: Arrange/Act/Assert with real objects, no mocks)
- Type-safe design (constraints in types, compiler verification)
- Error context mapping (`map_err`, custom error types with `thiserror`)
- Idiomatic Rust (clippy compliance, naming conventions)
- Performance awareness (SLO targets, O(n) complexity analysis)
- Poka-Yoke design (error prevention at compile time)
- Deterministic outputs (same input → same output, always)

---

## Holographic Factory Metaphor (v6)

**Core Equation**: $A = \mu(O)$ where μ is a **five-stage deterministic pipeline**:

```
μ₁ (Normalize)   → RDF validation, SHACL shapes, dependency resolution
μ₂ (Extract)     → SPARQL queries, OWL inference, rule execution
μ₃ (Emit)        → Tera template rendering, code generation
μ₄ (Canonicalize)→ Deterministic formatting, content hashing
μ₅ (Receipt)     → Cryptographic proof generation, audit trail
```

**Substrate** (unrdf): RDF ontology as high-dimensional holographic film encoding domain knowledge.

**History** (KGC-4D): Git snapshots as temporal coherence waypoints. Each commit is a 4D slice of the knowledge graph evolution.

**Transformation** (ggen sync): Five-stage pipeline precipitates code from interference patterns in RDF ontology.

**Corollary**: Bug in generated code? Fix the RDF spec (interference pattern), not the output (projection). The ontology is the source of truth.

**Quality Gates (Poka-Yoke)**: Six pre-flight checks prevent defects before μ₁:
1. Manifest schema validation
2. Ontology dependency resolution
3. SPARQL query syntax validation
4. Template syntax validation
5. File permission checks
6. Rule validation

**Deterministic Receipts**: Every `ggen sync` generates cryptographic proof:
- Execution ID + timestamp (ISO 8601)
- Manifest hash (SHA-256) + Ontology hash (SHA-256)
- Files generated + content hashes (SHA-256 per file)
- Inference rules executed + timings (μs precision)
- Generation rules executed + timings (μs precision)
- Audit trail path (JSON log with full provenance)

---

## Quality Gates (Pre-Commit)

1. `cargo make check` passes (zero errors, warnings-as-errors)
2. `cargo make lint` passes (zero warnings, clippy `-D warnings`)
3. `cargo make test` passes (all tests green, <30s SLO)
4. No `unwrap/expect` in production code (clippy enforced)
5. All APIs return `Result<T, E>` (no naked errors)
6. Deterministic outputs (same input → same output, verified by receipts)
7. SHACL validation passes for all `.specify/*.ttl` files

---

## When to Use EPIC 9 (Big Bang 80/20)

**Trigger Conditions** (use EPIC 9 if ANY apply):
- Non-trivial tasks affecting 5+ files or 3+ systems
- Multiple valid approaches with significant trade-offs
- Large architectural decisions requiring parallel exploration
- Unclear requirements needing hypothesis testing
- New feature domains with unknown unknowns

**Workflow** (EPIC 9 Atomic Cycle):
```
1. Specification Closure (100% coverage in .specify/*.ttl)
   ↓
2. Fan-Out (10 parallel agents, independent exploration)
   ↓
3. Collision Detection (structural + semantic overlap analysis)
   ↓
4. Convergence (selection pressure: coverage, invariants, minimality, elegance)
   ↓
5. Refactoring (DRY, type-safety, performance optimization)
   ↓
6. Closure (deterministic receipts, cryptographic proof)
```

**Claude Agents Available** (8 specialized agents):
- **bb80-specification-validator**: Validates TTL closure (100% coverage check)
- **bb80-parallel-task-coordinator**: Spawns 10+ agents, monitors execution
- **bb80-collision-detector**: Detects structural/semantic overlap
- **bb80-convergence-orchestrator**: Synthesizes optimal solution from parallel outputs
- **rust-coder**: Idiomatic Rust implementation specialist
- **reviewer**: Code review (type safety, security, performance)
- **speckit-architect**: RDF specification designer (Turtle ontologies)
- **test-engineer**: Chicago TDD test specialist (AAA pattern)

**Claude Skills Available** (11 domain skills):
- **cargo-make-protocol**: Master Cargo Make, Poka-Yoke, SLO enforcement
- **chicago-tdd-pattern**: State-based testing, AAA pattern
- **poka-yoke-patterns**: Error-proofing, FMEA, quality gates
- **rdf-ontologies**: Turtle syntax, SPARQL, SHACL validation
- **bb80-specification-closure**: 100% coverage verification
- **bb80-parallel-agents**: 10-agent parallel orchestration
- **bb80-deterministic-receipts**: Cryptographic proof generation
- **bb80-invariant-construction**: Type-safe invariant enforcement
- **session-start-hook**: Repository setup for Claude Code on web
- **mcp-servers**: Model Context Protocol integration

---

## Remember (Critical Reminders)

- **Spec Closure First**: Verify 100% coverage in `.specify/*.ttl` before any code generation (non-negotiable for Big Bang 80/20).
- **Receipts Over Narratives**: Always produce deterministic evidence (test counts, compile times, SLO metrics, cryptographic hashes).
- **RDF is Reality**: Edit `.ttl` files; everything else (code, docs, configs, markdown) is a generated projection via μ.
- **Andon Stops Work**: 🔴 RED = halt immediately; 🟡 YELLOW = investigate before release; 🟢 GREEN = proceed safely.
- **Cargo Make is Law**: All validation through `Makefile.toml`, never raw `cargo` (prevents bypassing quality gates and timeout enforcement).
- **Parallel First**: For non-trivial work, always use EPIC 9 (10 agents + collision + convergence + receipts).
- **V6 Unified Command**: Use `ggen sync` for ALL generation tasks. Old commands (`ggen generate`, `ggen template`, etc.) are deprecated.
- **Quality Gates**: Six pre-flight checks must pass before μ₁ (Normalize) starts. Poka-Yoke prevents defects, not detects them.
- **Deterministic Always**: Same ontology + same templates = identical output every time. Verified by SHA-256 content hashing.

---

## V6 Five-Stage Pipeline (μ)

### μ₁ (Normalize)
- Load and parse RDF ontology (Turtle, RDF/XML, N-Triples)
- SHACL shape validation (ensure ontology conforms to schema)
- Dependency resolution (imports, external ontologies)
- OWL inference (materialize implicit triples)
- **Output**: Validated, normalized RDF graph

### μ₂ (Extract)
- Execute SPARQL queries (SELECT, CONSTRUCT, ASK, DESCRIBE)
- Apply inference rules (RDFS, OWL2-RL)
- Extract template context (JSON/YAML binding for Tera)
- **Output**: Structured data for template rendering

### μ₃ (Emit)
- Tera template rendering (SPARQL-aware, multi-pass)
- Code generation (Rust, TypeScript, Python, Go, etc.)
- Multi-file generation (directory structures, modules)
- **Output**: Raw generated artifacts (code, configs, docs)

### μ₄ (Canonicalize)
- Deterministic formatting (rustfmt, prettier, black)
- Syntax validation (compiler checks, linters)
- Content hashing (SHA-256 per file)
- **Output**: Canonicalized, formatted artifacts

### μ₅ (Receipt)
- Cryptographic proof generation (execution ID, hashes)
- Audit trail logging (JSON with full provenance)
- Manifest + ontology fingerprinting
- File-by-file change tracking
- **Output**: Deterministic receipt (JSON), audit log (JSON)

---

## .claude/ Configuration

### Essential Files
- **settings.json**: Permissions, environment variables, MCP servers
- **agents/**: 8 specialized agent definitions (rust-coder, reviewer, etc.)
- **hooks/**: 9 pre/post hooks for safety and validation
  - `pre-specification-check.sh`: Verify TTL closure before EPIC 9
  - `pre-tool-safety-check.sh`: Prevent destructive operations
  - `post-bash-validation.sh`: Verify bash commands succeeded
  - `post-collision-detection.sh`: Analyze agent overlap
  - `convergence-validation.sh`: Ensure convergence quality
  - `session-start.sh`: Initialize session environment
  - `user-prompt-validation.sh`: Validate user inputs
- **skills/**: 11 domain skill modules (loaded on demand)

### Not in Critical Path
- Documentation files in `.claude/` are reference only (not auto-loaded during sessions)
- Skills are loaded when explicitly requested or when agent descriptions match task
- Agents are spawned on-demand via Task tool when task matches agent capabilities

---

## Development Workflow (v6)

### 1. Create Feature Specification (RDF-First)
```bash
# Create feature directory
mkdir -p .specify/specs/013-feature-name

# Copy template and edit TTL (SOURCE OF TRUTH)
cp .specify/templates/rdf-helpers/user-story.ttl.template \
   .specify/specs/013-feature-name/feature.ttl

# Edit TTL with RDF data
vim .specify/specs/013-feature-name/feature.ttl

# Validate SHACL conformance
ggen validate .specify/specs/013-feature-name/feature.ttl

# Generate markdown for GitHub viewing
cargo make speckit-render
```

### 2. Implement Feature (Chicago TDD)
```bash
# Write failing test FIRST (AAA pattern: Arrange/Act/Assert)
vim crates/ggen-core/tests/feature_test.rs

# Verify test fails (RED)
cargo make test-unit

# Implement minimal code to pass (GREEN)
vim crates/ggen-core/src/feature.rs

# Verify test passes
cargo make test-unit

# Refactor (maintain GREEN)
cargo make pre-commit  # check → lint → test-unit
```

### 3. Generate Code from Ontology
```bash
# Dry-run to preview changes
ggen sync --dry_run true

# Full sync with audit trail
ggen sync --audit true

# Verify receipt and audit log
cat .ggen/receipts/latest.json
cat .ggen/audit/$(date +%Y-%m-%d).json
```

### 4. Commit with Evidence
```bash
# Run full pre-commit quality gate
cargo make pre-commit

# Stage changes
git add .specify/specs/013-feature-name/feature.ttl
git add crates/ggen-core/src/feature.rs
git add crates/ggen-core/tests/feature_test.rs

# Commit with receipt evidence
git commit -m "feat(013): Implement feature

[Receipt] cargo make pre-commit: ✓ 3/3 gates passed
[Receipt] cargo make test: ✓ 347/347 tests, 28.3s
[Receipt] ggen sync --audit: ✓ 12 files generated, 0 conflicts
[Receipt] Audit trail: .ggen/audit/2026-01-18.json"
```

---

## Troubleshooting

### Problem: `ggen sync` times out or hangs
**Solution**: Use timeout enforcement via cargo make:
```bash
# Quick check (15s timeout)
timeout 15s ggen sync --validate_only true

# Full sync with timeout (120s)
timeout 120s ggen sync --audit true
```

### Problem: SHACL validation fails
**Solution**: Check `.specify/*.ttl` for schema violations:
```bash
# Validate specific file
ggen validate .specify/specs/013-feature-name/feature.ttl

# Common issues:
# - Priority must be exactly "P1", "P2", or "P3" (not "HIGH", "MEDIUM", "LOW")
# - Each UserStory must have at least 1 AcceptanceScenario
# - All required properties must be present
```

### Problem: Deterministic receipt hash mismatch
**Solution**: Ensure canonical formatting is applied:
```bash
# Reformat all Rust code
cargo make fmt

# Regenerate with force flag
ggen sync --force true --audit true

# Verify receipt
cat .ggen/receipts/latest.json | jq '.files[] | {path, hash}'
```

### Problem: Cargo make pre-commit fails on warnings
**Solution**: Fix all warnings (treated as errors in v6):
```bash
# Check which warnings exist
cargo make check

# Fix warnings in code
# Common issues:
# - Unused imports, variables, functions
# - Missing documentation
# - Clippy lints (pedantic, nursery, cargo groups enabled)

# Verify clean build
cargo make pre-commit
```

---

## Migration from v5.1.0 to v6.0.0

### Breaking Changes
1. **Unified Command**: Replace all `ggen generate`, `ggen template`, `ggen validate`, `ggen render` with `ggen sync`
2. **Quality Gates**: Pre-flight validation now mandatory (use `--force true` to override, but generates warning in audit log)
3. **Receipts**: Audit trails now required for production deployments (use `--audit true`)
4. **Warnings-as-Errors**: All compiler warnings treated as errors (clippy `-D warnings`)
5. **SHACL Validation**: `.specify/*.ttl` files must pass SHACL validation before generation

### Migration Steps
```bash
# 1. Update Cargo.toml workspace dependencies
cargo update

# 2. Replace old commands in scripts/CI
# OLD: ggen generate --template foo.tera --output bar.rs
# NEW: ggen sync

# 3. Add quality gates to Makefile.toml (if custom targets exist)
# Ensure timeout enforcement and warnings-as-errors

# 4. Validate all .specify/*.ttl files
cargo make speckit-validate

# 5. Run pre-commit to verify no warnings
cargo make pre-commit

# 6. Generate initial receipts
ggen sync --audit true
```

---

## Further Reading

- [V6 Release Notes](V6_RELEASE_NOTES.md) - Complete v6 changelog
- [Big Bang 80/20 Master Plan](BIG_BANG_80_20_MASTER_PLAN.md) - EPIC 9 methodology
- [Poka-Yoke Patterns](Makefile.toml) - Error-proofing mechanisms
- [.specify/ README](.specify/README.md) - RDF-first specification system
- [Testing Guide](TESTING.md) - Chicago TDD patterns
- [Security Guide](SECURITY.md) - SPARQL injection prevention
- [Contributing Guide](CONTRIBUTING.md) - Development workflow
- [Performance Guide](PERFORMANCE.md) - SLO targets and benchmarking

---

**Last Updated**: 2026-01-18 (v6.0.0 production-ready core release)
