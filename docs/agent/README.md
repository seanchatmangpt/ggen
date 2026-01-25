<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Agent Documentation for ggen](#agent-documentation-for-ggen)
  - [For advanced AI agents building production-ready ggen projects](#for-advanced-ai-agents-building-production-ready-ggen-projects)
  - [📖 Core Knowledge Base](#-core-knowledge-base)
  - [🎯 Specialized Guides (By Task)](#-specialized-guides-by-task)
    - [**Building CLI Commands**](#building-cli-commands)
    - [**RDF & SPARQL Integration**](#rdf--sparql-integration)
    - [**Template System**](#template-system)
    - [**Marketplace Packages**](#marketplace-packages)
    - [**Testing (Chicago TDD)**](#testing-chicago-tdd)
    - [**Build System & Hooks**](#build-system--hooks)
    - [**Error Handling & Validation**](#error-handling--validation)
    - [**Configuration Management**](#configuration-management)
    - [**Polyglot Code Generation**](#polyglot-code-generation)
    - [**Advanced Patterns**](#advanced-patterns)
  - [🗂️ Reference by Crate](#-reference-by-crate)
  - [🔍 Quick Navigation](#-quick-navigation)
  - [📌 Critical Rules (Agents Must Know)](#-critical-rules-agents-must-know)
  - [🚀 Getting Started (Agent Flow)](#-getting-started-agent-flow)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Agent Documentation for ggen

## For advanced AI agents building production-ready ggen projects

This documentation is structured for agent navigation—direct, technical, no fluff.

---

## 📖 Core Knowledge Base

**Start here:** [`AGENT_KNOWLEDGE_REQUIREMENTS.md`](AGENT_KNOWLEDGE_REQUIREMENTS.md)
- Complete architecture overview
- 18 sections covering everything agents need
- 31KB of distilled implementation patterns

---

## 🎯 Specialized Guides (By Task)

### **Building CLI Commands**
[`cli-patterns.md`](cli-patterns.md)
- clap-noun-verb auto-discovery mechanism
- Verb function patterns and macros
- Error handling and result serialization
- Command routing and conventions

### **RDF & SPARQL Integration**
[`rdf-sparql-guide.md`](rdf-sparql-guide.md)
- Oxigraph store patterns
- SPARQL query execution
- Graph caching and invalidation
- Template integration with RDF

### **Template System**
[`template-system.md`](template-system.md)
- Tera template rendering
- SPARQL in templates
- File injection patterns
- Liquid template syntax for Rust contexts

### **Marketplace Packages**
[`marketplace-packages.md`](marketplace-packages.md)
- Package.toml manifest structure
- RDF-backed package registry
- SPARQL package search
- Ed25519 signing and verification

### **Testing (Chicago TDD)**
[`testing-guide.md`](testing-guide.md)
- State-based testing patterns
- Real collaborators (no excessive mocks)
- Chicago TDD vs London TDD
- Test organization structure
- Coverage targets (80/20 rule)

### **Build System & Hooks**
[`build-system.md`](build-system.md)
- cargo make tasks and targets
- Timeout SLAs and wrapper commands
- Pre-commit/push hooks
- CI/CD validation gates
- Andon signals (stop-the-line quality)

### **Error Handling & Validation**
[`error-handling.md`](error-handling.md)
- Error types and context
- Validation chains
- Poka-Yoke (mistake-proofing)
- Security validation patterns

### **Configuration Management**
[`configuration.md`](configuration.md)
- ggen.toml structure and parsing
- Makefile.toml lifecycle tasks
- gpack.toml for packages
- Hierarchical config (workspace → project → package)

### **Polyglot Code Generation**
[`polyglot-generation.md`](polyglot-generation.md)
- Rust, TypeScript, Python patterns
- Framework support (axum, express, fastapi)
- Type mapping (xsd:* → language types)
- Zero-drift architecture

### **Advanced Patterns**
[`advanced-patterns.md`](advanced-patterns.md)
- Type-level state machines (PhantomData)
- Generic Associated Types (GATs)
- Const generics for zero-cost
- Higher-Ranked Trait Bounds (HRTB)
- Performance optimization techniques

---

## 🗂️ Reference by Crate

**ggen-core** - RDF engine, templates, project generation
- `src/graph/` - Oxigraph wrapper, SPARQL caching
- `src/template/` - Tera rendering, RDF metadata
- `src/templates/` - File tree generation
- `src/pipeline/` - Template processing
- `src/marketplace/` - Registry client

**ggen-cli** - Command-line interface
- `src/cmds/` - Noun modules with verb functions
- `src/conventions/` - File-based routing

**ggen-domain** - Business logic
- `src/template/` - Template operations
- `src/graph/` - Graph operations
- `src/marketplace/` - Marketplace operations
- `src/project/` - Project operations

**ggen-marketplace-v2** - Package system
- `src/models.rs` - Package types
- `src/registry.rs` - Package registry
- `src/traits.rs` - AsyncRepository trait

**ggen-utils** - Shared utilities
- `src/error.rs` - Error types
- `src/logging.rs` - Alert macros

---

## 🔍 Quick Navigation

**By Agent Type:**
- **Code-Analyzer** → Start with [`error-handling.md`](error-handling.md), [`testing-guide.md`](testing-guide.md)
- **Coder** → Start with [`cli-patterns.md`](cli-patterns.md), [`template-system.md`](template-system.md)
- **Tester** → Start with [`testing-guide.md`](testing-guide.md), [`build-system.md`](build-system.md)
- **Reviewer** → Start with [`error-handling.md`](error-handling.md), [`testing-guide.md`](testing-guide.md)
- **Marketplace Dev** → Start with [`marketplace-packages.md`](marketplace-packages.md), [`rdf-sparql-guide.md`](rdf-sparql-guide.md)

**By Task:**
- **Build new CLI command** → [`cli-patterns.md`](cli-patterns.md) + [`error-handling.md`](error-handling.md)
- **Create marketplace package** → [`marketplace-packages.md`](marketplace-packages.md) + [`template-system.md`](template-system.md)
- **Implement RDF integration** → [`rdf-sparql-guide.md`](rdf-sparql-guide.md) + [`template-system.md`](template-system.md)
- **Set up CI/CD** → [`build-system.md`](build-system.md) + [`testing-guide.md`](testing-guide.md)
- **Debug compilation errors** → [`error-handling.md`](error-handling.md) + [`build-system.md`](build-system.md)

---

## 📌 Critical Rules (Agents Must Know)

1. **ALWAYS use `cargo make`** - NEVER direct `cargo` commands
2. **No unsafe code** in production paths
3. **No `.expect()` or `.unwrap()`** without `#[allow(...)]`
4. **Result<T, E>** for all fallible operations
5. **Chicago TDD** - State-based, real collaborators, AAA pattern
6. **Timeout SLAs** - Enforce with wrappers
7. **Andon signals** - Stop and fix compiler errors/warnings
8. **Test organization** - Mirror `/src` in `/tests`
9. **Config hierarchy** - Workspace → Project → Package
10. **Zero drift** - One ontology → multiple language implementations

---

## 🚀 Getting Started (Agent Flow)

1. Read: [`AGENT_KNOWLEDGE_REQUIREMENTS.md`](AGENT_KNOWLEDGE_REQUIREMENTS.md) (15 min)
2. Reference: Specialized guides as needed (per task)
3. Implement: Follow patterns from examples in `/examples`
4. Test: Chicago TDD patterns from [`testing-guide.md`](testing-guide.md)
5. Validate: Build system rules from [`build-system.md`](build-system.md)

---

**Everything agents need. Nothing agents don't need.**
