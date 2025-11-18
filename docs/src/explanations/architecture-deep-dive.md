<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Architecture Deep Dive](#architecture-deep-dive)
  - [System Overview](#system-overview)
  - [Crate-by-Crate Breakdown](#crate-by-crate-breakdown)
    - [1. ggen-cli (Command-Line Interface)](#1-ggen-cli-command-line-interface)
      - [Structure](#structure)
      - [Command Module Pattern](#command-module-pattern)
      - [Key Components](#key-components)
    - [2. ggen-domain (Domain/Business Logic Layer)](#2-ggen-domain-domainbusiness-logic-layer)
      - [Structure](#structure-1)
      - [Data Flow Pattern](#data-flow-pattern)
      - [Example: Template Generation](#example-template-generation)
      - [Error Handling](#error-handling)
    - [3. ggen-core (Core Engine)](#3-ggen-core-core-engine)
      - [Structure](#structure-2)
      - [Core Workflows](#core-workflows)
      - [Key Components](#key-components-1)
    - [4. ggen-ai (AI/LLM Integration)](#4-ggen-ai-aillm-integration)
      - [Structure](#structure-3)
      - [AI Workflow](#ai-workflow)
      - [Provider Abstraction](#provider-abstraction)
    - [5. ggen-marketplace (Package Registry)](#5-ggen-marketplace-package-registry)
      - [Structure](#structure-4)
      - [Package Lifecycle](#package-lifecycle)
    - [6. ggen-utils (Shared Utilities)](#6-ggen-utils-shared-utilities)
      - [Structure](#structure-5)
      - [Common Types](#common-types)
    - [7. ggen-node (Node.js Bindings)](#7-ggen-node-nodejs-bindings)
      - [Structure](#structure-6)
      - [Usage from Node.js](#usage-from-nodejs)
  - [Data Flow Diagrams](#data-flow-diagrams)
    - [Ontology → Code Generation Flow](#ontology-%E2%86%92-code-generation-flow)
    - [AI Ontology Generation Flow](#ai-ontology-generation-flow)
    - [Marketplace Package Installation Flow](#marketplace-package-installation-flow)
  - [Module Dependencies](#module-dependencies)
    - [Dependency Graph](#dependency-graph)
    - [No Circular Dependencies](#no-circular-dependencies)
  - [How to Extend ggen](#how-to-extend-ggen)
    - [Adding a New CLI Command](#adding-a-new-cli-command)
    - [Adding RDF Processing](#adding-rdf-processing)
    - [Adding AI Provider](#adding-ai-provider)
    - [Adding Marketplace Features](#adding-marketplace-features)
  - [Performance Considerations](#performance-considerations)
    - [Hot Paths](#hot-paths)
    - [Optimization Techniques](#optimization-techniques)
  - [Testing Architecture](#testing-architecture)
    - [Test Organization](#test-organization)
    - [Test Fixtures](#test-fixtures)
  - [Contributing Guide Reference](#contributing-guide-reference)
  - [Key Design Principles](#key-design-principles)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Architecture Deep Dive

Comprehensive guide to ggen's internal architecture, crate-by-crate breakdown, module organization, and data flows. This is for contributors and advanced users wanting to understand how ggen works internally.

---

## System Overview

ggen is organized as a Rust **workspace** with 7 interdependent crates following a **layered architecture**:

```
┌─────────────────────────────────────────────────────────────────┐
│                        User Interfaces                          │
├─────────────────────────────────────────────────────────────────┤
│                      ggen-cli (Binary)                          │
│              Commands, CLI Parsing, Output Formatting           │
├─────────────────────────────────────────────────────────────────┤
│                    ggen-domain (Domain Layer)                   │
│       Business Logic, Orchestration, Command Handlers           │
├─────────────────────────────────────────────────────────────────┤
│              ggen-core, ggen-ai, ggen-marketplace               │
│         Infrastructure: RDF, Templates, AI, Package Registry    │
├─────────────────────────────────────────────────────────────────┤
│              ggen-utils (Shared Utilities)                      │
│         Errors, Config, Common Types, Helpers                   │
├─────────────────────────────────────────────────────────────────┤
│                   External Dependencies                         │
│     Oxigraph (RDF), Tera (Templates), Tokio (Async)            │
└─────────────────────────────────────────────────────────────────┘
```

---

## Crate-by-Crate Breakdown

### 1. ggen-cli (Command-Line Interface)

**Location:** `crates/ggen-cli/src/`
**Purpose:** Entry point for end users; parses commands and routes to domain layer
**Key Principle:** Thin CLI layer; all business logic in domain layer

#### Structure

```
ggen-cli/
├── src/
│   ├── main.rs                    [ENTRY POINT: Runs CLI loop]
│   ├── lib.rs                     [Library exports for external use]
│   ├── prelude.rs                 [Common imports, types, macros]
│   ├── runtime.rs                 [CLI runtime/executor]
│   ├── runtime_helper.rs          [CLI helper functions]
│   ├── cmds/                      [8 Command modules]
│   │   ├── mod.rs                 [Command module exports]
│   │   ├── ai.rs                  [AI: generate-ontology, chat, analyze]
│   │   ├── ci.rs                  [CI: setup GitHub Actions, GitLab CI]
│   │   ├── graph.rs               [Graph: query, validate, visualize RDF]
│   │   ├── hook.rs                [Hook: create, list, test git hooks]
│   │   ├── marketplace.rs         [Marketplace: search, install, publish]
│   │   ├── project.rs             [Project: new, scaffold, initialize]
│   │   ├── template.rs            [Template: generate, test, validate]
│   │   └── utils.rs               [Utils: doctor, cleanup, diagnose]
│   └── conventions/               [Command conventions/patterns]
│       ├── mod.rs                 [Convention system]
│       ├── resolver.rs            [Command routing logic]
│       ├── planner.rs             [Execution planning]
│       ├── watcher.rs             [File watching for regeneration]
│       └── presets/               [Command presets]
│           ├── mod.rs
│           └── clap_noun_verb.rs  [Noun-verb CLI pattern]
└── Cargo.toml                     [CLI manifest]
```

#### Command Module Pattern

Each command module implements `clap-noun-verb` pattern with auto-discovery:

```rust
// cmds/template.rs
#[verb(name = "generate")]
#[args]
pub async fn generate_rdf(
    #[arg(short, long)] ontology: String,
    #[arg(short, long)] template: String,
    #[arg(short, long)] output: String,
) -> Result<()> {
    // Calls into domain layer:
    ggen_domain::template::generate(
        &ontology,
        &template,
        &output,
    ).await
}
```

**Auto-discovery:** Macros extract `#[verb]` attributes and register commands automatically.

#### Key Components

- **Runtime:** Executes user commands asynchronously
- **Routing:** `clap-noun-verb` converts `ggen NOUN VERB` to function calls
- **Output:** Handles formatting (JSON, YAML, pretty print)
- **Error Handling:** Converts domain errors to CLI messages

---

### 2. ggen-domain (Domain/Business Logic Layer)

**Location:** `crates/ggen-domain/src/`
**Purpose:** Pure business logic; reusable across CLI, APIs, SDKs
**Key Principle:** No CLI dependencies; async by default; error propagation

#### Structure

```
ggen-domain/
├── src/
│   ├── lib.rs                     [Library entry point, module exports]
│   ├── ai/                        [AI operations]
│   │   ├── mod.rs                 [AI module exports]
│   │   ├── ontology_generator.rs  [Generate RDF ontologies from prompts]
│   │   ├── chat.rs                [Interactive AI chat for ontology evolution]
│   │   └── analyzer.rs            [Analyze codebase for structure insights]
│   ├── audit/                     [Audit trails & logging]
│   │   └── mod.rs                 [Track generation events]
│   ├── ci/                        [CI/CD operations]
│   │   ├── mod.rs                 [CI module]
│   │   ├── github_actions.rs      [GitHub Actions setup]
│   │   └── gitlab_ci.rs           [GitLab CI setup]
│   ├── graph/                     [RDF graph queries]
│   │   ├── mod.rs                 [Graph operations]
│   │   ├── query.rs               [SPARQL query execution]
│   │   └── validate.rs            [Ontology validation]
│   ├── hook/                      [Git hook operations]
│   │   ├── mod.rs                 [Hook management]
│   │   ├── pre_commit.rs          [Pre-commit hook logic]
│   │   └── pre_push.rs            [Pre-push hook logic]
│   ├── marketplace/               [Marketplace operations]
│   │   ├── mod.rs                 [Marketplace module]
│   │   ├── search.rs              [Package search]
│   │   ├── install.rs             [Package installation]
│   │   ├── publish.rs             [Package publishing]
│   │   └── verify.rs              [Signature verification]
│   ├── project/                   [Project management]
│   │   ├── mod.rs                 [Project operations]
│   │   ├── scaffold.rs            [Project scaffolding]
│   │   ├── init.rs                [Project initialization]
│   │   └── lifecycle.rs           [Project lifecycle phases]
│   ├── rdf/                       [RDF handling]
│   │   ├── mod.rs                 [RDF operations]
│   │   ├── loader.rs              [Load RDF files]
│   │   └── merger.rs              [Merge multiple ontologies]
│   ├── template/                  [Template operations]
│   │   ├── mod.rs                 [Template module]
│   │   ├── render.rs              [Template rendering]
│   │   ├── test.rs                [Golden file testing]
│   │   └── validate.rs            [Template validation]
│   ├── shell/                     [Shell integration]
│   │   └── mod.rs                 [Execute shell commands safely]
│   └── utils/                     [Domain utilities]
│       ├── mod.rs                 [Utils exports]
│       ├── errors.rs              [Domain errors]
│       └── config.rs              [Configuration loading]
└── Cargo.toml                     [Domain manifest]
```

#### Data Flow Pattern

```
CLI Command
    ↓
Domain Function (async)
    ↓
Load Configuration
    ↓
Execute Business Logic
    ↓
Return Result/Error
    ↓
CLI Output Formatting
```

#### Example: Template Generation

```rust
// ggen-domain/src/template/render.rs

pub async fn generate(
    ontology_path: &Path,
    template_name: &str,
    output_dir: &Path,
) -> Result<GenerationReport> {
    // 1. Load configuration
    let config = load_config()?;

    // 2. Load RDF ontology
    let graph = ggen_core::rdf::load_ontology(ontology_path)?;

    // 3. Get template from marketplace (or local)
    let template = get_template(template_name, &config).await?;

    // 4. Render template against graph
    let output = ggen_core::template::render(&graph, &template)?;

    // 5. Write files
    write_files(&output, output_dir)?;

    // 6. Return report
    Ok(GenerationReport { files_created: output.len() })
}
```

#### Error Handling

```rust
#[derive(Debug)]
pub enum DomainError {
    OntologyNotFound(String),
    TemplateRenderFailed(String),
    MarketplaceError(String),
    FileIoError(std::io::Error),
    // ... more error variants
}

impl fmt::Display for DomainError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DomainError::OntologyNotFound(path) =>
                write!(f, "Ontology not found: {}", path),
            // ... more display implementations
        }
    }
}
```

---

### 3. ggen-core (Core Engine)

**Location:** `crates/ggen-core/src/`
**Purpose:** RDF/SPARQL processing, template rendering, project lifecycle
**Key Principle:** Low-level primitives; no business logic; deterministic output

#### Structure

```
ggen-core/
├── src/
│   ├── lib.rs                     [Core library entry point]
│   ├── cleanroom/                 [Isolated, safe execution environment]
│   │   ├── mod.rs                 [Cleanroom system]
│   │   ├── sandbox.rs             [Execution sandbox]
│   │   └── validator.rs           [Pre-execution validation]
│   ├── cli_generator/             [CLI code generation]
│   │   ├── mod.rs                 [CLI generation]
│   │   └── clap_binding.rs        [Clap integration]
│   ├── config/                    [Configuration handling]
│   │   ├── mod.rs                 [Config system]
│   │   ├── loader.rs              [Load from ggen.toml]
│   │   ├── validator.rs           [Validate configuration]
│   │   └── schema.rs              [Configuration schema]
│   ├── graph/                     [RDF graph processing]
│   │   ├── mod.rs                 [Graph module]
│   │   ├── loader.rs              [Load RDF files]
│   │   ├── validator.rs           [Validate RDF structure]
│   │   └── merger.rs              [Merge graphs]
│   ├── lifecycle/                 [Project lifecycle management]
│   │   ├── mod.rs                 [Lifecycle module]
│   │   ├── phases.rs              [Init, setup, build, test, deploy]
│   │   ├── hooks.rs               [Hook execution]
│   │   └── state.rs               [Track lifecycle state]
│   ├── project_generator/         [Project scaffolding]
│   │   ├── mod.rs                 [Project generation]
│   │   ├── scaffold.rs            [Generate project structure]
│   │   └── templates.rs           [Built-in project templates]
│   ├── rdf/                       [RDF/SPARQL operations]
│   │   ├── mod.rs                 [RDF module]
│   │   ├── oxigraph_wrapper.rs    [Oxigraph integration]
│   │   ├── query.rs               [SPARQL query execution]
│   │   ├── validation.rs          [RDF validation]
│   │   └── type_mapper.rs         [XSD to language type mapping]
│   └── templates/                 [Template rendering engine]
│       ├── mod.rs                 [Template module]
│       ├── renderer.rs            [Tera rendering]
│       ├── query_executor.rs      [SPARQL query in templates]
│       ├── frozen_sections.rs     [Incremental generation support]
│       └── error_handler.rs       [Template error handling]
└── Cargo.toml                     [Core manifest]
```

#### Core Workflows

**RDF Loading & Querying:**

```
User ontology.ttl
    ↓
[ggen_core::rdf::load_ontology]
    ↓
Oxigraph MemoryStore
    ↓
[ggen_core::rdf::query::execute_sparql]
    ↓
Query Results (JSON)
```

**Template Rendering:**

```
Template file + SPARQL queries
    ↓
[ggen_core::templates::renderer::render]
    ↓
Tera environment setup
    ↓
Execute SPARQL queries
    ↓
Load results into template context
    ↓
Tera rendering
    ↓
Generated output
```

#### Key Components

- **Oxigraph Integration:** Wraps Oxigraph RDF/SPARQL engine
- **Type Mapper:** Maps XSD types to Rust/TS/Python/etc.
- **Template Engine:** Tera + SPARQL query integration
- **Frozen Sections:** Preserves manual code across regenerations
- **Lifecycle Management:** Tracks project phases and hooks

---

### 4. ggen-ai (AI/LLM Integration)

**Location:** `crates/ggen-ai/src/`
**Purpose:** AI-powered ontology generation, analysis, agent system
**Key Principle:** Provider-agnostic; pluggable LLM backends

#### Structure

```
ggen-ai/
├── src/
│   ├── lib.rs                     [AI library entry point]
│   ├── agents/                    [AI agent implementations]
│   │   ├── mod.rs                 [Agent system]
│   │   ├── base.rs                [Base agent trait]
│   │   ├── ontology_builder.rs    [Ontology generation agent]
│   │   ├── code_analyst.rs        [Code analysis agent]
│   │   └── chat_agent.rs          [Interactive chat agent]
│   ├── config/                    [AI provider configuration]
│   │   ├── mod.rs                 [Config system]
│   │   ├── anthropic.rs           [Anthropic/Claude config]
│   │   ├── openai.rs              [OpenAI GPT config]
│   │   └── ollama.rs              [Ollama local config]
│   ├── generators/                [Code generation with AI]
│   │   ├── mod.rs                 [Generators]
│   │   ├── ontology.rs            [Generate RDF ontologies]
│   │   └── schema.rs              [Generate schemas]
│   ├── governance/                [AI governance & safety]
│   │   ├── mod.rs                 [Governance]
│   │   ├── cost_limiter.rs        [Track API costs]
│   │   └── safety_checker.rs      [Validate AI output]
│   ├── prompts/                   [Prompt system]
│   │   ├── mod.rs                 [Prompt module]
│   │   ├── builder.rs             [Build prompts dynamically]
│   │   └── templates.rs           [Prompt templates]
│   ├── providers/                 [LLM provider abstraction]
│   │   ├── mod.rs                 [Provider trait]
│   │   ├── anthropic.rs           [Anthropic provider]
│   │   ├── openai.rs              [OpenAI provider]
│   │   └── ollama.rs              [Local Ollama provider]
│   ├── rdf/                       [RDF generation utilities]
│   │   ├── mod.rs                 [RDF generation]
│   │   └── builder.rs             [Build RDF from AI output]
│   ├── swarm/                     [Multi-agent coordination]
│   │   ├── mod.rs                 [Swarm system]
│   │   ├── coordinator.rs         [Coordinate agents]
│   │   └── messaging.rs           [Inter-agent messaging]
│   └── ultrathink/                [Extended thinking mode]
│       ├── mod.rs                 [Ultra thinking]
│       └── executor.rs            [Execute extended thinking]
├── prompts/system/                [System prompt templates]
│   ├── ontology_builder.md        [Ontology generation prompt]
│   ├── code_analyzer.md           [Code analysis prompt]
│   └── validator.md               [Validation prompt]
└── Cargo.toml                     [AI manifest]
```

#### AI Workflow

```
User Prompt
    ↓
[ggen_ai::agents::ontology_builder]
    ↓
Load system prompt + user input
    ↓
Call LLM Provider (Claude, GPT, Ollama)
    ↓
Parse response
    ↓
Validate RDF structure
    ↓
Save as .ttl ontology
```

#### Provider Abstraction

```rust
pub trait LlmProvider: Send + Sync {
    async fn generate(
        &self,
        prompt: &Prompt,
        config: &GenerationConfig,
    ) -> Result<String>;

    async fn chat(
        &self,
        messages: Vec<Message>,
        config: &GenerationConfig,
    ) -> Result<String>;
}

// Implementations: AnthropicProvider, OpenAiProvider, OllamaProvider
```

---

### 5. ggen-marketplace (Package Registry)

**Location:** `crates/ggen-marketplace/src/`
**Purpose:** Package discovery, installation, publishing, signature verification
**Key Principle:** Decentralized-ready; cryptographically signed packages

#### Structure

```
ggen-marketplace/
├── src/
│   ├── lib.rs                     [Marketplace library]
│   ├── backend/                   [Backend services]
│   │   ├── mod.rs                 [Backend module]
│   │   ├── registry.rs            [Package registry API]
│   │   └── storage.rs             [Remote storage]
│   ├── cache/                     [Local caching layer]
│   │   ├── mod.rs                 [Cache system]
│   │   ├── file_cache.rs          [File-based cache]
│   │   └── memory_cache.rs        [In-memory cache]
│   ├── crypto/                    [Package signatures]
│   │   ├── mod.rs                 [Crypto module]
│   │   ├── sign.rs                [Sign packages (ML-DSA)]
│   │   └── verify.rs              [Verify signatures]
│   ├── diagrams/                  [Architecture diagrams]
│   │   └── marketplace.md         [Marketplace architecture]
│   ├── models/                    [Data models]
│   │   ├── mod.rs                 [Models]
│   │   ├── package.rs             [Package metadata]
│   │   ├── manifest.rs            [ggen.toml parsing]
│   │   └── version.rs             [Version management]
│   ├── plugins/                   [Plugin system]
│   │   ├── mod.rs                 [Plugin trait]
│   │   ├── loader.rs              [Load plugins]
│   │   └── registry.rs            [Plugin registry]
│   ├── quality/                   [Quality checks]
│   │   ├── mod.rs                 [Quality module]
│   │   ├── linter.rs              [Lint packages]
│   │   └── test_runner.rs         [Run package tests]
│   ├── recommendations/           [Recommendation engine]
│   │   ├── mod.rs                 [Recommendations]
│   │   └── similarity.rs          [Package similarity]
│   ├── search/                    [Search functionality]
│   │   ├── mod.rs                 [Search engine]
│   │   ├── indexer.rs             [Index packages]
│   │   └── query.rs               [Full-text search]
│   ├── storage/                   [Package storage]
│   │   ├── mod.rs                 [Storage layer]
│   │   ├── local.rs               [Local filesystem]
│   │   ├── s3.rs                  [AWS S3 storage]
│   │   └── github.rs              [GitHub releases]
│   └── traits/                    [Core traits]
│       ├── mod.rs                 [Trait module]
│       ├── package_source.rs      [Package source trait]
│       └── verification.rs        [Verification trait]
├── marketplace/                   [Package registry data]
│   └── packages.json              [Published packages index]
└── Cargo.toml                     [Marketplace manifest]
```

#### Package Lifecycle

```
Package Source (Local Files)
    ↓
[ggen_marketplace::models::Package::from_manifest]
    ↓
Validate ggen.toml
    ↓
Sign package (ML-DSA)
    ↓
Compress .tar.gz
    ↓
Upload to registry
    ↓
Index in packages.json
    ↓
Cache locally
    ↓
Available for ggen marketplace search/install
```

---

### 6. ggen-utils (Shared Utilities)

**Location:** `crates/ggen-utils/src/`
**Purpose:** Common types, error handling, configuration
**Key Principle:** Minimal, widely-used utilities

#### Structure

```
ggen-utils/
├── src/
│   ├── lib.rs                     [Utilities library]
│   ├── error.rs                   [Common error types]
│   ├── config.rs                  [Configuration utilities]
│   ├── logging.rs                 [Logging setup]
│   ├── fs.rs                      [File system utilities]
│   ├── types.rs                   [Common types]
│   └── macros.rs                  [Utility macros]
├── src/bin/                       [Binary utilities]
│   ├── git_hook_pre_commit.rs     [Pre-commit hook binary]
│   ├── git_hook_pre_push.rs       [Pre-push hook binary]
│   └── ggen_utils.rs              [CLI utilities]
└── Cargo.toml                     [Utils manifest]
```

#### Common Types

```rust
pub type Result<T> = std::result::Result<T, UtilsError>;

#[derive(Debug)]
pub enum UtilsError {
    ConfigError(String),
    IoError(std::io::Error),
    ParseError(String),
    // ... more variants
}

pub struct Config {
    pub registry_url: String,
    pub cache_dir: PathBuf,
    pub log_level: LogLevel,
    // ... more fields
}
```

---

### 7. ggen-node (Node.js Bindings)

**Location:** `crates/ggen-node/src/`
**Purpose:** Node.js/JavaScript integration via N-API
**Key Principle:** Expose ggen functionality to Node.js ecosystem

#### Structure

```
ggen-node/
├── src/
│   └── lib.rs                     [N-API bindings]
├── __tests__/                     [JavaScript tests]
│   ├── template.test.js           [Template rendering tests]
│   └── ai.test.js                 [AI functionality tests]
├── package.json                   [Node.js package]
├── index.js                       [Node.js entry point]
└── Cargo.toml                     [Rust manifest]
```

#### Usage from Node.js

```javascript
const ggen = require('ggen-node');

// Generate code from ontology
const result = ggen.generateFromRdf({
    ontology: 'domain.ttl',
    template: 'rust-models',
    output: 'src/',
});
```

---

## Data Flow Diagrams

### Ontology → Code Generation Flow

```
┌─────────────────────┐
│  User Command       │
│  ggen template      │
│  generate-rdf       │
└──────────┬──────────┘
           │
           ↓
┌─────────────────────────────────┐
│  ggen-cli::cmds::template       │
│  Parse arguments                │
└──────────┬──────────────────────┘
           │
           ↓
┌─────────────────────────────────┐
│  ggen-domain::template          │
│  Orchestrate generation         │
└──────────┬──────────────────────┘
           │
           ├──────────────────────────┐
           ↓                          ↓
┌──────────────────────┐  ┌────────────────────┐
│ ggen-core::rdf       │  │ ggen-core::template│
│ Load ontology.ttl    │  │ Load template      │
│ Parse RDF            │  │ Validate syntax    │
└──────────┬───────────┘  └────────────┬───────┘
           │                          │
           └──────────┬───────────────┘
                      ↓
         ┌────────────────────────┐
         │ ggen-core::templates:: │
         │ query_executor         │
         │ Execute SPARQL queries │
         │ against ontology       │
         └──────────┬─────────────┘
                    ↓
         ┌────────────────────────┐
         │ Tera Template Renderer │
         │ Render with results    │
         └──────────┬─────────────┘
                    ↓
         ┌────────────────────────┐
         │ Write output files     │
         │ Handle frozen sections │
         └──────────┬─────────────┘
                    ↓
         ┌────────────────────────┐
         │ Return success report  │
         └────────────────────────┘
```

### AI Ontology Generation Flow

```
┌─────────────────────┐
│ User Prompt         │
│ "Blog system:       │
│  User, Post,        │
│  Comment entities"  │
└──────────┬──────────┘
           │
           ↓
┌─────────────────────────────────┐
│  ggen-cli::cmds::ai             │
│  Parse generate-ontology cmd    │
└──────────┬──────────────────────┘
           │
           ↓
┌─────────────────────────────────┐
│  ggen-domain::ai                │
│  Load config (API keys)         │
└──────────┬──────────────────────┘
           │
           ↓
┌─────────────────────────────────┐
│  ggen-ai::agents::              │
│  ontology_builder               │
│  Create agent instance          │
└──────────┬──────────────────────┘
           │
           ↓
┌─────────────────────────────────┐
│  ggen-ai::prompts::builder      │
│  Build system + user prompt     │
│  Load prompt template           │
└──────────┬──────────────────────┘
           │
           ↓
┌─────────────────────────────────┐
│  ggen-ai::providers             │
│  Select provider:               │
│  Anthropic/OpenAI/Ollama        │
└──────────┬──────────────────────┘
           │
           ↓
┌─────────────────────────────────┐
│  Call LLM API                   │
│  Generate RDF structure         │
└──────────┬──────────────────────┘
           │
           ↓
┌─────────────────────────────────┐
│  ggen-ai::rdf::builder          │
│  Parse response into RDF        │
└──────────┬──────────────────────┘
           │
           ↓
┌─────────────────────────────────┐
│  ggen-core::rdf::validation     │
│  Validate RDF structure         │
└──────────┬──────────────────────┘
           │
           ↓
┌─────────────────────────────────┐
│  Write domain.ttl               │
│  Save to filesystem             │
└────────────────────────────────┘
```

### Marketplace Package Installation Flow

```
┌─────────────────────┐
│ User Command        │
│ ggen marketplace    │
│ install rust-models│
└──────────┬──────────┘
           │
           ↓
┌──────────────────────────────┐
│ ggen-cli::cmds::marketplace  │
│ Parse package identifier     │
└──────────┬───────────────────┘
           │
           ↓
┌──────────────────────────────┐
│ ggen-domain::marketplace     │
│ Orchestrate installation     │
└──────────┬───────────────────┘
           │
           ├─────────────────────────────┐
           ↓                             ↓
┌──────────────────────┐  ┌─────────────────────┐
│ ggen-marketplace::   │  │ ggen-marketplace::  │
│ search::search_by_id │  │ cache::check_local  │
│ Query registry       │  │ Check cache first   │
└──────────┬───────────┘  └──────────┬──────────┘
           │                         │
           └────────────┬────────────┘
                        │
              ┌─────────┴──────────┐
              │                    │
           Found                Not Found
              │                    │
              ↓                    ↓
    [Return cached]      [Download from registry]
              │                    │
              └────────────┬───────┘
                           ↓
        ┌──────────────────────────────┐
        │ ggen-marketplace::crypto::   │
        │ verify_signature             │
        │ Verify ML-DSA signature      │
        └────────────┬─────────────────┘
                     │
                     ↓
        ┌──────────────────────────────┐
        │ Extract package contents     │
        │ Copy to local directory      │
        └────────────┬─────────────────┘
                     │
                     ↓
        ┌──────────────────────────────┐
        │ Update ggen.lock             │
        │ Track installed packages     │
        └────────────┬─────────────────┘
                     │
                     ↓
        ┌──────────────────────────────┐
        │ Return success               │
        │ Package ready for use        │
        └──────────────────────────────┘
```

---

## Module Dependencies

### Dependency Graph

```
ggen-cli
    ├─→ ggen-domain (all operations)
    └─→ ggen-utils (errors, config)

ggen-domain
    ├─→ ggen-core (RDF, templates)
    ├─→ ggen-ai (AI operations)
    ├─→ ggen-marketplace (package ops)
    └─→ ggen-utils (errors, config)

ggen-core
    ├─→ ggen-utils (errors, config)
    └─→ External: oxigraph, tera

ggen-ai
    ├─→ ggen-core (RDF building)
    ├─→ ggen-utils (errors, config)
    └─→ External: anthropic-sdk, openai-sdk

ggen-marketplace
    ├─→ ggen-utils (errors, config)
    └─→ External: serde, tokio

ggen-utils
    └─→ External: serde, tokio

ggen-node
    └─→ All crates (N-API bindings)
```

### No Circular Dependencies

The architecture ensures **no circular dependencies**:
- CLI only calls Domain
- Domain calls Core, AI, Marketplace
- Core, AI, Marketplace only call Utils
- Utils calls nothing

This allows each layer to be used independently.

---

## How to Extend ggen

### Adding a New CLI Command

1. **Create command module** in `ggen-crates/ggen-cli/src/cmds/`
2. **Implement domain function** in `ggen-domain/src/<area>/`
3. **Wire up CLI** with `#[verb]` macro
4. **Write tests** in `tests/` or crate
5. **Update docs** in `docs/`

### Adding RDF Processing

1. **Implement in** `ggen-core/src/rdf/`
2. **Use Oxigraph** for SPARQL queries
3. **Return** `Result<T>` with proper errors
4. **Test** with fixtures in `tests/fixtures/`

### Adding AI Provider

1. **Implement** `LlmProvider` trait
2. **Add to** `ggen-ai/src/providers/`
3. **Update config** in `ggen-ai/src/config/`
4. **Test** with mocked responses

### Adding Marketplace Features

1. **Extend models** in `ggen-marketplace/src/models/`
2. **Implement logic** in relevant submodule
3. **Update storage** layer if needed
4. **Add tests** with fixtures

---

## Performance Considerations

### Hot Paths

- **RDF Loading:** Oxigraph memstore is fast for small-medium ontologies
- **SPARQL Queries:** Pre-compile queries when possible
- **Template Rendering:** Cache compiled templates
- **Marketplace Search:** Use in-memory index for local packages

### Optimization Techniques

1. **Lazy Load:** Only load what you need
2. **Cache Aggressively:** Ontologies, templates, packages
3. **Parallel Processing:** Use Tokio for concurrent operations
4. **Stream Large Files:** Don't load entire files into memory

---

## Testing Architecture

### Test Organization

- **Unit tests:** In crate `tests/` modules
- **Chicago TDD:** `tests/chicago_tdd/` - real dependencies
- **London TDD:** `tests/london_tdd/` - mocked I/O
- **E2E:** `tests/e2e/` - full workflows
- **BDD:** `tests/bdd/` - user stories

### Test Fixtures

- `tests/fixtures/` - Test data (ontologies, templates)
- `tests/data/` - Generated test outputs
- Golden files for snapshot testing

---

## Contributing Guide Reference

For contributing code:
- Follow architecture principles
- Keep layers separated
- No circular dependencies
- Add tests at appropriate level
- Update documentation
- See [CONTRIBUTING.md](../../CONTRIBUTING.md)

---

## Key Design Principles

1. **Layered Architecture** - CLI → Domain → Core
2. **No Circular Dependencies** - Unidirectional flow
3. **Provider Abstraction** - Swappable LLM providers, storage, etc.
4. **Async by Default** - All I/O is async
5. **Error Propagation** - Errors bubble up gracefully
6. **Type Safety** - Rust's type system prevents bugs
7. **Deterministic Output** - Same input = identical output
8. **Testability** - Dependencies injected, easy to mock

---

## Next Steps

- **Implement a Feature:** Follow architecture above
- **Add a Command:** Start in ggen-cli, move to ggen-domain
- **Extend Templates:** Add patterns to ggen-core
- **Contribute Packages:** Build in ggen-marketplace
- **Report Issues:** Include crate context in bug reports
