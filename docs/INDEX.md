<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen Documentation Index](#ggen-documentation-index)
  - [🚀 Getting Started](#-getting-started)
  - [📚 Documentation by Type](#-documentation-by-type)
    - [Installation & Setup](#installation--setup)
    - [Core Concepts](#core-concepts)
    - [Learning Paths](#learning-paths)
      - [🎓 I want to learn ggen](#-i-want-to-learn-ggen)
      - [🔍 I need to solve a problem](#-i-need-to-solve-a-problem)
      - [📖 I need reference information](#-i-need-reference-information)
      - [💡 I want to understand concepts](#-i-want-to-understand-concepts)
      - [🏗️ I want working examples](#-i-want-working-examples)
      - [📋 Need quick command reference?](#-need-quick-command-reference)
    - [Feature Documentation](#feature-documentation)
      - [🚦 Poka-Yoke Error-Proofing (Quality Control)](#-poka-yoke-error-proofing-quality-control)
      - [🤖 ggen-ai (AI-Native Code Generation)](#-ggen-ai-ai-native-code-generation)
      - [☁️ ggen-paas (Infrastructure-as-Code)](#-ggen-paas-infrastructure-as-code)
      - [📅 Bree Scheduler (Job Orchestration)](#-bree-scheduler-job-orchestration)
    - [Philosophy & Principles](#philosophy--principles)
      - [Constitutional Rules (v6)](#constitutional-rules-v6)
      - [Project Constitution](#project-constitution)
    - [Migration & Upgrade](#migration--upgrade)
      - [v5.1.0 → v6.0.0 Migration](#v510-%E2%86%92-v600-migration)
    - [API & Reference](#api--reference)
      - [Core APIs](#core-apis)
      - [Tools & Utilities](#tools--utilities)
    - [Examples & Patterns](#examples--patterns)
      - [Production Examples](#production-examples)
      - [Common Patterns](#common-patterns)
    - [Contributing](#contributing)
    - [Additional Resources](#additional-resources)
    - [Archive & Historical](#archive--historical)
  - [Quick Navigation](#quick-navigation)
    - [By User Type](#by-user-type)
  - [Key Files Quick Links](#key-files-quick-links)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen Documentation Index

Complete documentation for ggen v6 - specification-driven code generation with manufacturing-grade quality control.

## 🚀 Getting Started

**New to ggen?** Start here:
1. [README.md](../README.md) - Project overview and quick start
2. [Installation & Setup](#installation--setup) - Get ggen running
3. [Your First Project](#quick-start) - 5-minute tutorial
4. [Examples](#examples) - Real-world patterns

## 📚 Documentation by Type

### Installation & Setup
- [Quick Start (5 minutes)](../README.md#quick-start-5-minutes) - Installation and first project
- [Feature Flags](#feature-flags) - Optional capabilities (PaaS, AI, experimental)
- [Platform-Specific Installation](#platform-specific-installation)
  - [macOS/Linux (Homebrew)](#macos/linux-fastest)
  - [Docker](#any-platform-docker)
  - [From Source (Rust)](#from-source-rust)

### Core Concepts
- [What is ggen?](../README.md#what-is-ggen) - Core value proposition
- [Why RDF Ontologies?](../README.md#why-rdf-ontologies) - Semantic web benefits
- [Architecture: The Holographic Factory](../README.md#architecture-the-holographic-factory) - Design metaphor
- [The Chatman Equation (A = μ(O))](../README.md#the-chatman-equation) - Code generation formula

### Learning Paths

#### 🎓 I want to learn ggen
→ [Tutorials](GENERATED_TUTORIALS.md) - Hands-on, step-by-step projects

#### 🔍 I need to solve a problem
→ [How-To Guides](GENERATED_HOWTO_GUIDES.md) - Specific solutions to common tasks

#### 📖 I need reference information
→ [Reference Docs](GENERATED_REFERENCE.md) - CLI, ggen.toml, SPARQL, templates

#### 💡 I want to understand concepts
→ [Explanations](GENERATED_EXPLANATIONS.md) - Philosophical background and architecture

#### 🏗️ I want working examples
→ [Examples](GENERATED_EXAMPLES.md) - REST APIs, databases, microservices

#### 📋 Need quick command reference?
→ [CLI Reference](#cli-reference) - Command-line options and flags

### Feature Documentation

#### 🚦 Poka-Yoke Error-Proofing (Quality Control)
- [Quality & Reliability](../README.md#quality--reliability) - Manufacturing-grade error prevention
- [Andon Signals](../README.md#andon-signals-visual-quality-control) - Visual status indicators (🟢/🟡/🔴)
- [Quality Gates](../README.md#quality-gates-pre-generation-validation) - 6 mandatory validation checkpoints

#### 🤖 ggen-ai (AI-Native Code Generation)
- [AI-Powered Generation](../README.md#ai-powered-generation) - Natural language to code
- [DSPy API](../README.md#dspy-inspired-api) - Type-safe ML composition
- [Multi-Provider LLM Support](../README.md#multi-provider-llm-support) - 8 LLM providers
- [ggen-ai README](../crates/ggen-ai/README.md) - Complete ggen-ai documentation

#### ☁️ ggen-paas (Infrastructure-as-Code)
→ [PaaS CLI Reference](reference/ggen-paas.md) - Docker, Kubernetes, Terraform generation

#### 📅 Bree Scheduler (Job Orchestration)
→ [Bree Semantic Scheduler](reference/bree-scheduler.md) - Production job scheduling

### Philosophy & Principles

#### Constitutional Rules (v6)
- [Big Bang 80/20](../README.md#1-big-bang-8020-specification-closure-first) - Specification closure before generation
- [EPIC 9](../README.md#2-epic-9-parallel-agent-convergence-advanced) - 10-agent parallel execution
- [Deterministic Receipts](../README.md#3-deterministic-receipts-evidence-replaces-narrative) - Cryptographic proofs

#### Project Constitution
- [CLAUDE.md](../CLAUDE.md) - Constitutional rules, paradigms, and development standards
- [Philosophy](../README.md#philosophy) - Specification-first, deterministic, RDF-first approach

### Migration & Upgrade

#### v5.1.0 → v6.0.0 Migration
- [Upgrade Guide](migration/upgrade-guide.md) - Complete migration path
- [Breaking Changes](migration/breaking-changes.md) - What changed and how to adapt
- [Migration Deliverables](migration/deliverables.md) - Tools and scripts for migration
- [FAQ](migration/faq.md) - Common questions about upgrading

### API & Reference

#### Core APIs
- [ggen Core API](reference/core-api.md) - RDF processing and code generation
- [SPARQL Reference](reference/sparql.md) - Query language for ontologies
- [Tera Template Reference](reference/templates.md) - Code generation templates

#### Tools & Utilities
- [ggen.toml Configuration](reference/ggen.toml.md) - Project configuration
- [CLI Reference](reference/cli.md) - Command-line interface
- [Build System (Cargo Make)](reference/build-system.md) - SLO enforcement

### Examples & Patterns

#### Production Examples
See [GENERATED_EXAMPLES.md](GENERATED_EXAMPLES.md) for 20+ working examples:
- REST APIs (Axum, Rocket, Actix)
- GraphQL servers
- Database schemas
- Microservice architectures
- Event sourcing
- AI-powered generation
- And much more...

#### Common Patterns
- [REST API Generation](../README.md#rest-api-generation) - API design from ontology
- [Multi-Language Support](../README.md#multi-language-support) - Generate Rust, TypeScript, Python
- [Database Schema Generation](../README.md#database-schema-generation) - SQL DDL from ontology

### Contributing

- [Contributing Guide](../CONTRIBUTING.md) - How to contribute to ggen
- [Development Setup](reference/development-setup.md) - Local development environment
- [Testing Guide](../TESTING.md) - Test infrastructure and patterns
- [Code of Conduct](#code-of-conduct) - Community standards

### Additional Resources

- [CHANGELOG](../CHANGELOG.md) - Version history and updates
- [Security Policy](../SECURITY.md) - Responsible disclosure and security practices
- [License](../LICENSE) - Apache 2.0 OR MIT
- [Issues & Discussions](https://github.com/seanchatmangpt/ggen/issues) - Report bugs, request features

### Archive & Historical

- [Archive Index](ARCHIVE_INDEX.md) - Preserved research, theses, and historical reports
- [Thesis & Research](archive/thesis/) - PhD thesis on Holographic Factory
- [Validation Reports](archive/validation-reports/) - Benchmarks and quality metrics
- [Agent Reports](archive/agent-reports/) - EPIC 9 multi-agent execution results
- [Infrastructure Guides](archive/infrastructure/) - Deployment and DevOps documentation

---

## Quick Navigation

### By User Type

**👤 Individual Developer**
1. [README.md](../README.md#quick-start-5-minutes) → Quick Start
2. [Tutorials](GENERATED_TUTORIALS.md) → Learn by doing
3. [Examples](GENERATED_EXAMPLES.md) → See real code
4. [How-To Guides](GENERATED_HOWTO_GUIDES.md) → Solve specific problems

**👥 Team Lead / Architect**
1. [Philosophy](../README.md#philosophy) → Understanding principles
2. [Architecture](../README.md#architecture-the-holographic-factory) → Design decisions
3. [Reference Docs](GENERATED_REFERENCE.md) → Technical specifications
4. [Contributing](../CONTRIBUTING.md) → Team standards

**🔧 DevOps / Deployment**
1. [Dockerfile & Docker Compose](../docker/) → Containerization
2. [ggen-paas](reference/ggen-paas.md) → Infrastructure generation
3. [Bree Scheduler](reference/bree-scheduler.md) → Job orchestration
4. [Infrastructure Guides](archive/infrastructure/) → Deployment patterns

**🤖 LLM / AI Integration**
1. [ggen-ai Guide](../crates/ggen-ai/README.md) → AI capabilities
2. [DSPy Patterns](../README.md#dspy-inspired-api) → ML composition
3. [LLM Providers](../README.md#multi-provider-llm-support) → 8 supported providers

**📚 Researcher / Academic**
1. [Philosophy](../README.md#philosophy) → Paradigm shifts
2. [PhD Thesis](archive/thesis/PhD_THESIS_TPS_GGEN.md) → Formal theory
3. [FMEA Analysis](archive/infrastructure/FMEA_ggen_sync_user_simulation.md) → User experience research
4. [Validation Reports](archive/validation-reports/) → Quality metrics

---

## Key Files Quick Links

| What | Location |
|------|----------|
| **Project Constitution** | [CLAUDE.md](../CLAUDE.md) |
| **User Guide** | [README.md](../README.md) |
| **Contribution Guide** | [CONTRIBUTING.md](../CONTRIBUTING.md) |
| **Changelog** | [CHANGELOG.md](../CHANGELOG.md) |
| **Security Policy** | [SECURITY.md](../SECURITY.md) |
| **License** | [LICENSE](../LICENSE) |

---

**Last Updated**: 2026-01-09  
**Version**: ggen v6.0.0  
**Status**: Production Ready

> See something missing or outdated? [Open an issue](https://github.com/seanchatmangpt/ggen/issues) or [contribute](CONTRIBUTING.md)!
