<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Examples Catalog](#examples-catalog)
  - [Quick Navigation](#quick-navigation)
  - [Getting Started Examples](#getting-started-examples)
    - [1. **demo-project**](#1-demo-project)
    - [2. **basic-template-generation**](#2-basic-template-generation)
    - [3. **api-endpoint**](#3-api-endpoint)
  - [CLI Examples](#cli-examples)
    - [4. **frontmatter-cli**](#4-frontmatter-cli)
    - [5. **cli-subcommand**](#5-cli-subcommand)
    - [6. **cli-advanced**](#6-cli-advanced)
    - [7. **clap-noun-verb-demo**](#7-clap-noun-verb-demo)
    - [8. **cli-workspace-example**](#8-cli-workspace-example)
    - [9. **rust-cli-lifecycle**](#9-rust-cli-lifecycle)
  - [AI-Powered Examples](#ai-powered-examples)
    - [10. **ai-template-project**](#10-ai-template-project)
    - [11. **ai-code-generation**](#11-ai-code-generation)
    - [12. **knowledge-graph-builder**](#12-knowledge-graph-builder)
    - [13. **ai-microservice**](#13-ai-microservice)
    - [14. **advanced-ai-usage**](#14-advanced-ai-usage)
    - [15. **ai-template-creation**](#15-ai-template-creation)
    - [16. **ai-templates**](#16-ai-templates)
  - [RDF & SPARQL Examples](#rdf--sparql-examples)
    - [17. **advanced-sparql-graph**](#17-advanced-sparql-graph)
    - [18. **sparql-engine**](#18-sparql-engine)
  - [Template Examples](#template-examples)
    - [19. **complete-project-generation**](#19-complete-project-generation)
    - [20. **ontology-to-code** (in crates/ggen-core/examples/)](#20-ontology-to-code-in-cratesggen-coreexamples)
  - [Marketplace Examples](#marketplace-examples)
    - [21. **natural-market-search**](#21-natural-market-search)
    - [22. **p2p-marketplace**](#22-p2p-marketplace)
  - [Full-Stack Examples](#full-stack-examples)
    - [23. **full-stack-app**](#23-full-stack-app)
    - [24. **comprehensive-rust-showcase**](#24-comprehensive-rust-showcase)
    - [25. **advanced-fullstack-integration**](#25-advanced-fullstack-integration)
    - [26. **e2e-demo**](#26-e2e-demo)
    - [27. **microservices-architecture**](#27-microservices-architecture)
  - [Advanced Examples](#advanced-examples)
    - [28. **advanced-rust-project**](#28-advanced-rust-project)
    - [29. **advanced-rust-api-8020**](#29-advanced-rust-api-8020)
    - [30. **advanced-lifecycle-demo**](#30-advanced-lifecycle-demo)
    - [31. **advanced-cache-registry**](#31-advanced-cache-registry)
    - [32. **advanced-pipeline**](#32-advanced-pipeline)
    - [33. **advanced-error-handling**](#33-advanced-error-handling)
  - [Specialized Domain Examples](#specialized-domain-examples)
    - [34. **embedded-cross**](#34-embedded-cross)
    - [35. **embedded-iot** (in crates/ggen-core/examples/)](#35-embedded-iot-in-cratesggen-coreexamples)
    - [36. **fastapi-from-rdf**](#36-fastapi-from-rdf)
    - [37. **wasm-deploy**](#37-wasm-deploy)
    - [38. **wasm-crypto** (in crates/ggen-core/examples/)](#38-wasm-crypto-in-cratesggen-coreexamples)
    - [39. **source-code-analysis**](#39-source-code-analysis)
    - [40. **telemetry-demo**](#40-telemetry-demo)
  - [Performance & Optimization Examples](#performance--optimization-examples)
    - [41. **lib-benchmarks**](#41-lib-benchmarks)
    - [42. **perf-library** (in crates/ggen-core/examples/)](#42-perf-library-in-cratesggen-coreexamples)
    - [43. **rust-monorepo**](#43-rust-monorepo)
  - [Structured & Data Examples](#structured--data-examples)
    - [44. **database-with-migrations**](#44-database-with-migrations)
    - [45. **safe-error-handling**](#45-safe-error-handling)
  - [Miscellaneous Examples](#miscellaneous-examples)
    - [46. **ggen-usage-wrapping**](#46-ggen-usage-wrapping)
    - [47. **production-readiness-demo**](#47-production-readiness-demo)
  - [Learning Paths](#learning-paths)
    - [Path 1: Beginner (1-2 weeks)](#path-1-beginner-1-2-weeks)
    - [Path 2: Intermediate (2-3 weeks)](#path-2-intermediate-2-3-weeks)
    - [Path 3: Advanced (4+ weeks)](#path-3-advanced-4-weeks)
    - [Path 4: Marketplace Publisher (1-2 weeks)](#path-4-marketplace-publisher-1-2-weeks)
  - [Running Examples](#running-examples)
  - [Example Statistics](#example-statistics)
  - [Contributing Examples](#contributing-examples)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Examples Catalog

Complete guide to all 47 example projects demonstrating ggen features. Each example is a working, self-contained project you can run, modify, and learn from.

## Quick Navigation

- **New to ggen?** Start with [Getting Started Examples](#getting-started-examples)
- **Building a CLI?** See [CLI Examples](#cli-examples)
- **Using AI?** Try [AI-Powered Examples](#ai-powered-examples)
- **Working with RDF/SPARQL?** Check [RDF & SPARQL Examples](#rdf--sparql-examples)
- **Advanced patterns?** Browse [Advanced Examples](#advanced-examples)
- **Testing?** See [Testing Examples](#testing-examples)
- **Contributing a package?** Review [Marketplace Examples](#marketplace-examples)

## Getting Started Examples

These examples are perfect for learning ggen fundamentals. Start here!

### 1. **demo-project**
**What it teaches:** Basic ggen workflow, project scaffolding, code generation

```bash
cd examples/demo-project
cargo run
```

**Complexity:** Beginner
**Time:** 10 minutes
**Topics:** Project setup, ontology basics, template rendering

### 2. **basic-template-generation**
**What it teaches:** Simple template creation and variable substitution

```bash
cd examples/basic-template-generation
ggen template generate-rdf --ontology domain.ttl --template basic
```

**Complexity:** Beginner
**Time:** 5 minutes
**Topics:** Templates, variable substitution, basic SPARQL

### 3. **api-endpoint**
**What it teaches:** Generating a REST API endpoint from RDF schema

```bash
cd examples/api-endpoint
cargo run
```

**Complexity:** Beginner
**Time:** 15 minutes
**Topics:** API generation, REST patterns, Rust web frameworks

---

## CLI Examples

Learn to build powerful command-line interfaces with ggen's CLI tools.

### 4. **frontmatter-cli**
**What it teaches:** Building CLI applications with rich metadata

```bash
cd examples/frontmatter-cli
cargo run -- --help
```

**Key features:**
- Frontmatter metadata in code
- Command introspection
- Help generation
- Subcommands with noun-verb patterns

**Complexity:** Intermediate
**Topics:** CLI design, metadata, help systems

### 5. **cli-subcommand**
**What it teaches:** Building complex CLI with subcommands

**Topics:** Subcommand organization, command hierarchy, routing

### 6. **cli-advanced**
**What it teaches:** Advanced CLI patterns and best practices

**Topics:** Error handling, output formatting, progress bars, interactive input

### 7. **clap-noun-verb-demo**
**What it teaches:** Using ggen's clap-noun-verb command pattern

**Topics:** Noun-verb command syntax, automatic command discovery, routing conventions

### 8. **cli-workspace-example**
**What it teaches:** Building CLI in a workspace with multiple crates

**Topics:** Workspace structure, code sharing, CLI organization

### 9. **rust-cli-lifecycle**
**What it teaches:** Complete CLI lifecycle management

**Topics:** Initialization, configuration, execution, cleanup, hooks

---

## AI-Powered Examples

Generate code with AI assistance. Perfect for understanding AI integration.

### 10. **ai-template-project**
**What it teaches:** Using AI to generate ontologies and templates

```bash
cd examples/ai-template-project
ggen ai generate-ontology \
  --prompt "User, Post, Comment entities" \
  --output domain.ttl
```

**Complexity:** Intermediate
**Time:** 20 minutes
**Topics:** AI prompting, ontology generation, iterative refinement

### 11. **ai-code-generation**
**What it teaches:** AI-powered source code generation

**Topics:** Code generation patterns, AI agents, multiple language targets

### 12. **knowledge-graph-builder**
**What it teaches:** Building knowledge graphs with AI assistance

**Topics:** Semantic modeling, RDF construction, graph validation

### 13. **ai-microservice**
**What it teaches:** Generating complete microservice from AI

**Topics:** Service templates, configuration, deployment patterns

### 14. **advanced-ai-usage**
**What it teaches:** Advanced AI workflows and optimizations

**Topics:** Batching, caching, cost optimization, multiple providers

### 15. **ai-template-creation**
**What it teaches:** Using AI to generate custom templates

**Topics:** Template synthesis, code patterns, quality assurance

### 16. **ai-templates**
**What it teaches:** Collection of AI-generated templates

**Topics:** Template library, pattern recognition, reuse

---

## RDF & SPARQL Examples

Master RDF ontologies and SPARQL queries.

### 17. **advanced-sparql-graph**
**What it teaches:** Complex SPARQL queries and graph navigation

```bash
cd examples/advanced-sparql-graph
cargo run
```

**Key topics:**
- SPARQL SELECT queries
- Graph patterns and filters
- Relationship traversal
- Aggregation and grouping
- Optional patterns
- Property paths

**Complexity:** Advanced
**Time:** 30 minutes

### 18. **sparql-engine**
**What it teaches:** Building custom SPARQL execution engine

**Topics:** Query parsing, graph querying, result processing

---

## Template Examples

Learn template development from simple to advanced.

### 19. **complete-project-generation**
**What it teaches:** Templates for complete project scaffolding

**Topics:** Multi-file generation, directory structures, configuration files

### 20. **ontology-to-code** (in crates/ggen-core/examples/)
**What it teaches:** Core template rendering from RDF

**Complexity:** Intermediate
**Topics:** SPARQL patterns, template variables, type mapping

---

## Marketplace Examples

Learn about the marketplace ecosystem.

### 21. **natural-market-search**
**What it teaches:** Searching the marketplace and discovering packages

```bash
cd examples/natural-market-search
cargo run
```

**Topics:** Marketplace API, search functionality, package discovery

### 22. **p2p-marketplace**
**What it teaches:** Decentralized package distribution patterns

**Topics:** P2P networking, package signing, decentralized registry

---

## Full-Stack Examples

Complete applications from ontology to deployment.

### 23. **full-stack-app**
**What it teaches:** Building complete web applications

**Topics:** Backend generation (Rust), frontend generation (TypeScript), database schema

**Includes:**
- Rust backend with Actix-web
- TypeScript React frontend
- PostgreSQL schema
- API bindings

### 24. **comprehensive-rust-showcase**
**What it teaches:** Comprehensive Rust ecosystem integration

**Topics:** Web frameworks, database drivers, CLI tools, libraries

### 25. **advanced-fullstack-integration**
**What it teaches:** Advanced patterns for full-stack apps

**Topics:** Shared types, API contracts, code generation coordination

### 26. **e2e-demo**
**What it teaches:** End-to-end project generation and deployment

**Topics:** Complete workflows, multi-stage pipelines, validation

### 27. **microservices-architecture**
**What it teaches:** Generating microservices from shared ontology

**Topics:** Service boundaries, API contracts, independent deployment

---

## Advanced Examples

Sophisticated patterns and techniques.

### 28. **advanced-rust-project**
**What it teaches:** Advanced Rust patterns and optimizations

**Topics:** Async/await, trait design, performance optimization

### 29. **advanced-rust-api-8020**
**What it teaches:** 80/20 API development in Rust

**Topics:** Practical API patterns, error handling, validation

### 30. **advanced-lifecycle-demo**
**What it teaches:** Complete project lifecycle management

**Topics:** Initialization, building, testing, deployment, cleanup

### 31. **advanced-cache-registry**
**What it teaches:** Caching strategies for package registry

**Topics:** Cache invalidation, performance optimization, consistency

### 32. **advanced-pipeline**
**What it teaches:** Complex generation pipelines with multiple stages

**Topics:** Pipeline composition, error recovery, progress tracking

### 33. **advanced-error-handling**
**What it teaches:** Comprehensive error handling patterns

**Topics:** Error types, recovery strategies, user messaging

---

## Specialized Domain Examples

Real-world domain applications.

### 34. **embedded-cross**
**What it teaches:** Embedded systems programming with ggen

**Topics:** Cross-compilation, embedded patterns, resource constraints

### 35. **embedded-iot** (in crates/ggen-core/examples/)
**What it teaches:** IoT device code generation

**Topics:** Lightweight code, protocol buffers, device communication

### 36. **fastapi-from-rdf**
**What it teaches:** Generating FastAPI (Python) from RDF ontology

**Topics:** Python generation, web framework patterns, cross-language generation

### 37. **wasm-deploy**
**What it teaches:** WebAssembly deployment patterns

**Topics:** WASM generation, browser APIs, performance optimization

### 38. **wasm-crypto** (in crates/ggen-core/examples/)
**What it teaches:** Cryptographic WASM modules

**Topics:** Cryptographic operations, security, performance

### 39. **source-code-analysis**
**What it teaches:** Analyzing and generating based on source code

**Topics:** AST analysis, code metrics, intelligent generation

### 40. **telemetry-demo**
**What it teaches:** Observability and telemetry patterns

**Topics:** OpenTelemetry, logging, metrics, tracing

---

## Performance & Optimization Examples

Learn to build high-performance systems.

### 41. **lib-benchmarks**
**What it teaches:** Creating and running library benchmarks

**Topics:** Benchmark frameworks, performance testing, profiling

### 42. **perf-library** (in crates/ggen-core/examples/)
**What it teaches:** Building performance-focused libraries

**Topics:** Optimization techniques, benchmarking, profiling

### 43. **rust-monorepo**
**What it teaches:** Monorepo patterns with ggen

**Topics:** Workspace organization, dependency management, code sharing

---

## Structured & Data Examples

Data modeling and manipulation.

### 44. **database-with-migrations**
**What it teaches:** Database schema and migration generation

**Topics:** SQL generation, migration management, schema versioning

### 45. **safe-error-handling**
**What it teaches:** Safe error handling patterns

**Topics:** Result types, error recovery, validation

---

## Miscellaneous Examples

Additional examples demonstrating specific features.

### 46. **ggen-usage-wrapping**
**What it teaches:** Wrapping ggen in other tools

**Topics:** Library integration, API usage, tool composition

### 47. **production-readiness-demo**
**What it teaches:** Making code production-ready

**Topics:** Error handling, logging, monitoring, documentation

---

## Learning Paths

### Path 1: Beginner (1-2 weeks)
1. demo-project
2. basic-template-generation
3. api-endpoint
4. frontmatter-cli
5. ai-template-project

### Path 2: Intermediate (2-3 weeks)
Complete Path 1, then:
1. ontology-to-code
2. advanced-sparql-graph
3. complete-project-generation
4. full-stack-app
5. advanced-rust-project

### Path 3: Advanced (4+ weeks)
Complete Paths 1-2, then:
1. microservices-architecture
2. advanced-cache-registry
3. fastapi-from-rdf
4. wasm-deploy
5. rust-monorepo

### Path 4: Marketplace Publisher (1-2 weeks)
1. complete-project-generation
2. natural-market-search
3. advanced-error-handling
4. safe-error-handling
5. production-readiness-demo

---

## Running Examples

All examples follow the same pattern:

```bash
# Navigate to example
cd examples/<example-name>

# Build
cargo build

# Run
cargo run [args]

# Test
cargo test

# Clean
cargo clean
```

Some examples require additional setup (databases, API keys, etc.). Check the README.md in each example directory.

---

## Example Statistics

| Category | Count | Topics |
|----------|-------|--------|
| Getting Started | 3 | Basic workflows |
| CLI | 6 | Command-line interfaces |
| AI | 7 | AI-powered generation |
| RDF/SPARQL | 2 | Knowledge graphs, queries |
| Templates | 1 | Template development |
| Marketplace | 2 | Package management |
| Full-Stack | 5 | Complete applications |
| Advanced | 6 | Complex patterns |
| Specialized | 6 | Domains (IoT, WASM, etc.) |
| Performance | 3 | Optimization |
| Data | 2 | Data modeling |
| Misc | 4 | Various features |

---

## Contributing Examples

Found a missing pattern or use case? Contribute an example!

1. Create a new directory in `examples/`
2. Add working code demonstrating the concept
3. Include comprehensive README.md explaining:
   - What it teaches
   - Prerequisites
   - How to run it
   - Key concepts covered
   - Complexity level
   - Time to complete

4. Link to this catalog (see priority 1 in gap analysis)
5. Submit PR with updated Examples Catalog

Examples are the best documentation. Great examples unlock new users and use cases!
