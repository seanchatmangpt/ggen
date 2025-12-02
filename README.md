# ggen - Ontology-Driven Code Generation

## For Advanced AI Agents Building Production Projects

**ggen** is a knowledge graph code generator. RDF ontology → polyglot code (Rust, TypeScript, Python). One source of truth, zero drift.

---

## 🚀 Quick Start by Role

### 👤 End Users (Generate Code from Ontologies)
**Goal:** Use ggen to generate code from RDF/OWL ontologies

1. **Install:** [Installation Guide](docs/src/how-to-guides/installation.md)
2. **Learn:** [Getting Started Tutorial](docs/src/tutorials/getting-started.md) (5 minutes)
3. **Reference:** [CLI Reference](docs/src/reference/cli.md)

### 🤖 AI Agents (Build ggen Features)
**Goal:** Build production ggen features

1. **Start:** [`docs/agent/README.md`](docs/agent/README.md) - Agent knowledge base
2. **Architecture:** [`docs/agent/cli-patterns.md`](docs/agent/cli-patterns.md) - clap-noun-verb patterns
3. **Reference:** [Main CLAUDE.md](CLAUDE.md) - Build system rules

### 📚 Contributors (Improve ggen)
**Goal:** Contribute to ggen codebase

1. **Architecture:** [Architecture Explanation](docs/src/explanations/architecture.md)
2. **Testing:** [Testing Strategy](docs/src/how-to-guides/testing-strategy.md)
3. **Build System:** [cargo make reference](Makefile.toml)

```bash
# Define domain once (RDF)
ggen ai generate-ontology --prompt "E-commerce: Product, Order, Review" --output domain.ttl

# Generate across all languages
ggen template generate-rdf --ontology domain.ttl --template rust-models
ggen template generate-rdf --ontology domain.ttl --template typescript-models
ggen template generate-rdf --ontology domain.ttl --template python-pydantic

# Update ontology → regenerate → all languages auto-sync
```

---

## 🤖 For AI Agents

**Start Here:** [`docs/agent/AGENT_KNOWLEDGE_REQUIREMENTS.md`](docs/agent/AGENT_KNOWLEDGE_REQUIREMENTS.md)

This is the complete reference for building advanced ggen projects:
- **Workspace architecture** - Crate dependencies, module hierarchy
- **CLI patterns** - clap-noun-verb auto-discovery, command implementation
- **RDF/SPARQL** - Oxigraph integration, query patterns, template generation
- **Marketplace** - Package system, SPARQL search, security
- **Configuration** - ggen.toml, Makefile.toml, lifecycle hooks
- **Build system** - cargo make, timeout SLAs, CI/CD
- **Error handling** - Error types, validation chains, Poka-Yoke
- **Testing** - Chicago TDD, test organization, coverage patterns
- **Polyglot generation** - Rust, TypeScript, Python patterns
- **Advanced patterns** - Agent integration, extending the system

**Navigation by task:**
- [`docs/agent/cli-patterns.md`](docs/agent/cli-patterns.md) - Building CLI commands
- [`docs/agent/rdf-sparql-guide.md`](docs/agent/rdf-sparql-guide.md) - RDF/SPARQL integration
- [`docs/agent/template-system.md`](docs/agent/template-system.md) - Template generation patterns
- [`docs/agent/marketplace-packages.md`](docs/agent/marketplace-packages.md) - Creating marketplace packages
- [`docs/agent/testing-guide.md`](docs/agent/testing-guide.md) - Chicago TDD patterns
- [`docs/agent/build-system.md`](docs/agent/build-system.md) - cargo make, hooks, validation

---

## 🏗️ Workspace Architecture

**12 crates, layered design:**
```
ggen-cli (binary)
  └─ ggen-domain (business logic)
       ├─ ggen-core (RDF/template engine)
       ├─ ggen-marketplace-v2 (package system)
       ├─ ggen-ai (code analysis/generation)
       └─ ggen-utils (shared error handling)
```

**Core capabilities:**
- RDF graph management (Oxigraph 0.5, SPARQL 1.1)
- Template generation (Tera with RDF integration)
- Marketplace packages (SPARQL search, Ed25519 signing)
- Project scaffolding (Rust, Next.js, FastAPI)
- Lifecycle hooks (pre-commit, CI/CD validation)

---

## 📋 Project Structure

```
ggen/
├── crates/
│   ├── ggen-core/           # RDF engine, templates, project generation
│   ├── ggen-cli/            # CLI commands (noun-verb auto-discovery)
│   ├── ggen-domain/         # Business logic (zero CLI dependencies)
│   ├── ggen-marketplace-v2/ # Package system
│   ├── ggen-ai/             # AI integration
│   ├── ggen-utils/          # Shared utilities
│   └── {5 more crates}      # Config, macros, DoD, Node.js bindings
├── docs/
│   └── agent/               # ← Start here for agent development
├── examples/                # 84+ complete project examples
├── templates/               # Liquid/Jinja2 templates
└── marketplace/             # Package marketplace
```

---

## ⚡ Critical Build Rules (Non-Negotiable)

1. **ALWAYS use `cargo make`** - Never direct `cargo` commands
2. **Timeout SLAs:**
   - Check: 5s
   - Build: 10s
   - Tests: 30s
   - Release: 30s
3. **Andon signals** - Red (errors) → STOP THE LINE, fix immediately
4. **No `.expect()`** in production code - Use `Result<T, E>`
5. **No `println!`** in libraries - Use logging/alert macros

---

## 🧪 Testing (Chicago TDD)

- **State-based testing** - Verify outputs, not implementation
- **Real collaborators** - Use actual systems, minimize mocks
- **AAA pattern** - Arrange, Act, Assert
- **Coverage target** - 80%+ on critical paths
- **Test organization** - `/tests` mirrors `/src` structure

Run tests:
```bash
cargo make test           # All tests
cargo make test-unit      # Unit tests only
cargo make slo-check      # Performance validation
```

---

## 🔗 Links

- **GitHub:** https://github.com/seanchatmangpt/ggen
- **Crates.io:** https://crates.io/crates/ggen
- **Homebrew:** `brew tap seanchatmangpt/tap && brew install ggen`

---

## 📝 Quick Reference

**Essential files agents must understand:**
- `crates/ggen-core/src/graph.rs` - RDF store wrapper
- `crates/ggen-core/src/template/mod.rs` - Template rendering
- `crates/ggen-cli/src/cmds/mod.rs` - Command discovery
- `crates/ggen-domain/src/lib.rs` - Business logic entry points
- `crates/ggen-marketplace-v2/src/models.rs` - Package types
- `Makefile.toml` - Build configuration
- `docs/agent/AGENT_KNOWLEDGE_REQUIREMENTS.md` - Complete reference

---

**Built with Rust | RDF | SPARQL**

Ontology-driven. Zero drift. Production-ready.
