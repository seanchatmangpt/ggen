# ggen v2.0.0 Architecture Design

**Author**: System Architect Agent (Hive Mind Swarm)
**Date**: 2025-11-01
**Status**: Design Complete
**Target Version**: v2.0.0

---

## Executive Summary

This document defines the complete architecture for ggen v2.0.0, migrating from manual clap structures to the clap-noun-verb v3.0.0 auto-discovery pattern. The design emphasizes:

- **Clean Architecture**: Domain logic separated from CLI concerns
- **Auto-Discovery**: Filesystem-based routing with `#[verb]` attributes
- **Scalability**: Easy addition of new commands without central registration
- **Template Enhancements**: Frozen section support in templates
- **Backwards Compatibility**: Graceful migration path from v1.2.0

---

## Table of Contents

1. [System Overview](#system-overview)
2. [Module Structure](#module-structure)
3. [Command Structure Design](#command-structure-design)
4. [Domain Layer Architecture](#domain-layer-architecture)
5. [Template Engine Updates](#template-engine-updates)
6. [Filesystem Routing](#filesystem-routing)
7. [Data Flow Architecture](#data-flow-architecture)
8. [Migration Sequencing](#migration-sequencing)
9. [API Design](#api-design)
10. [Quality Attributes](#quality-attributes)

---

## 1. System Overview

### 1.1 Architectural Goals

```
┌─────────────────────────────────────────────────────────────┐
│                    ggen v2.0.0 Architecture                  │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌─────────────┐    ┌──────────────┐    ┌───────────────┐  │
│  │   CLI Layer │───>│ Domain Layer │───>│ Infrastructure│  │
│  │ (Commands)  │    │  (Business)  │    │   (Storage)   │  │
│  └─────────────┘    └──────────────┘    └───────────────┘  │
│         │                   │                     │          │
│         v                   v                     v          │
│  Auto-Discovery      Clean Logic         RDF/Templates      │
│  #[verb] attrs       No CLI deps         Frozen Sections    │
│                                                               │
└─────────────────────────────────────────────────────────────┘
```

**Key Principles:**
1. **Separation of Concerns**: CLI ≠ Business Logic
2. **Auto-Discovery**: Commands discovered via filesystem + attributes
3. **Testability**: Domain layer has zero CLI dependencies
4. **Extensibility**: New commands require zero central registration
5. **Performance**: Sub-3s generation, <100MB memory

### 1.2 Technology Stack

| Layer | Technology | Purpose |
|-------|-----------|---------|
| CLI | clap-noun-verb v3.0.0 | Auto-discovery, routing |
| Domain | Pure Rust | Business logic |
| Templates | Tera + RDF | Generation engine |
| Storage | Oxigraph + SPARQL | Knowledge graphs |
| Validation | SHACL + JSON Schema | Input validation |

---

## 2. Module Structure

### 2.1 Directory Layout (v2.0.0)

```
ggen/
├── cli/                           # CLI layer (command coordination)
│   ├── src/
│   │   ├── lib.rs                # Entry point, clap-noun-verb setup
│   │   ├── commands/             # v2.0.0 auto-discovered commands
│   │   │   ├── utils/           # Noun: utils
│   │   │   │   ├── mod.rs       # Auto-discovery setup
│   │   │   │   └── doctor.rs    # #[verb] for doctor
│   │   │   ├── project/         # Noun: project
│   │   │   │   ├── mod.rs
│   │   │   │   ├── new.rs       # #[verb] for new
│   │   │   │   ├── gen.rs       # #[verb] for gen
│   │   │   │   └── ...
│   │   │   ├── market/          # Noun: market
│   │   │   ├── template/        # Noun: template
│   │   │   ├── ai/              # Noun: ai
│   │   │   └── ...
│   │   ├── domain/              # Business logic (CLI-independent)
│   │   │   ├── utils/           # Domain: utils
│   │   │   │   ├── mod.rs
│   │   │   │   └── doctor.rs    # Pure business logic
│   │   │   ├── project/         # Domain: project
│   │   │   ├── market/          # Domain: market
│   │   │   └── ...
│   │   └── cmds/                # v1.2.0 legacy commands (deprecated)
│   └── Cargo.toml
│
├── ggen-core/                    # Template engine + RDF
│   ├── src/
│   │   ├── template/
│   │   │   ├── engine.rs        # Tera + frozen sections
│   │   │   ├── frozen.rs        # Freeze block parsing
│   │   │   └── parser.rs        # Enhanced frontmatter
│   │   ├── rdf/
│   │   │   ├── graph.rs         # Oxigraph integration
│   │   │   └── sparql.rs        # Query engine
│   │   └── ...
│   └── Cargo.toml
│
├── ggen-marketplace/             # Marketplace domain
├── ggen-ai/                      # AI providers
└── utils/                        # Shared utilities
```

### 2.2 Module Responsibilities

#### CLI Layer (`cli/src/commands/`)
- **Responsibility**: Argument parsing, validation, routing
- **Dependencies**: clap, clap-noun-verb, domain layer
- **Key Pattern**: Thin wrappers that delegate to domain layer

```rust
// cli/src/commands/utils/doctor.rs
use clap::Args;
use crate::domain::utils::doctor as domain;

#[derive(Args, Debug)]
#[command(name = "doctor", about = "Check environment health")]
pub struct DoctorArgs {
    #[arg(short, long)]
    verbose: bool,
}

// #[verb] attribute auto-discovered by clap-noun-verb
pub async fn run(args: &DoctorArgs) -> Result<()> {
    domain::check_environment(args.verbose).await
}
```

#### Domain Layer (`cli/src/domain/`)
- **Responsibility**: Business logic, no CLI dependencies
- **Dependencies**: ggen-core, ggen-marketplace, utils
- **Key Pattern**: Pure functions, testable in isolation

```rust
// cli/src/domain/utils/doctor.rs
// No clap imports! Pure business logic
pub async fn check_environment(verbose: bool) -> Result<()> {
    let checks = get_environment_checks();
    for check in checks {
        check.run()?;
    }
    Ok(())
}
```

---

## 3. Command Structure Design

### 3.1 Auto-Discovery Pattern

**Current (v1.2.0)**: Manual enum registration
```rust
// cli/src/cmds/mod.rs - MANUAL REGISTRATION
#[derive(Subcommand)]
pub enum Commands {
    #[command(name = "doctor")]
    Doctor(doctor::DoctorArgs),
    // ... 12 more manual entries
}
```

**Target (v2.0.0)**: Auto-discovery via filesystem
```rust
// cli/src/lib.rs - AUTO-DISCOVERY
use clap_noun_verb::{auto_discover, NounVerb};

#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    command: NounVerb,
}

// Discovers all commands in cli/src/commands/** with #[verb] attributes
let cli = auto_discover::<Cli>("cli/src/commands")?;
```

### 3.2 Noun-Verb Mapping

| Noun | Verbs | Example Commands |
|------|-------|------------------|
| `utils` | `doctor` | `ggen utils doctor` |
| `project` | `new`, `gen`, `plan`, `apply`, `diff`, `test`, `freeze`, `inject`, `validate`, `watch` | `ggen project new my-app` |
| `market` | `search`, `add`, `remove`, `list`, `update`, `info`, `publish`, `sync` | `ggen market search "rust"` |
| `template` | `new`, `list`, `show`, `lint`, `generate-tree`, `regenerate` | `ggen template generate-tree` |
| `ai` | `project`, `template`, `graph`, `sparql`, `config`, `validate`, `demo` | `ggen ai project scaffold` |
| `graph` | `load`, `query`, `export`, `validate`, `snapshot`, `diff`, `stats` | `ggen graph query "SELECT *"` |
| `hook` | `create`, `list`, `run`, `remove`, `validate` | `ggen hook create --trigger change` |
| `lifecycle` | `init`, `validate`, `deploy`, `status` | `ggen lifecycle deploy` |
| `audit` | `security`, `performance`, `hazard` | `ggen audit security` |
| `ci` | `trigger`, `workflow`, `release`, `pages` | `ggen ci release` |
| `shell` | `init`, `completion` | `ggen shell completion bash` |

### 3.3 Attribute-Based Routing

```rust
// cli/src/commands/project/new.rs
use clap::Args;
use clap_noun_verb::verb;

#[derive(Args, Debug)]
#[command(name = "new", about = "Create a new project")]
pub struct NewArgs {
    name: String,
    #[arg(short, long)]
    project_type: String,
}

#[verb] // <-- AUTO-DISCOVERY MAGIC
pub async fn run(args: &NewArgs) -> Result<()> {
    crate::domain::project::create_new_project(
        &args.name,
        &args.project_type
    ).await
}
```

**How it works:**
1. `auto_discover()` scans `cli/src/commands/**/*.rs`
2. Finds functions with `#[verb]` attribute
3. Infers noun from directory structure (`commands/project/` → `project`)
4. Builds routing table: `project new` → `commands::project::new::run()`

---

## 4. Domain Layer Architecture

### 4.1 Separation Principles

```
┌────────────────────────────────────────────────────────┐
│                   CLI Layer                             │
│  • Argument parsing (clap)                             │
│  • Input validation                                     │
│  • Error formatting                                     │
│  • Routing to domain                                    │
└────────────────┬───────────────────────────────────────┘
                 │ Pure function calls
                 v
┌────────────────────────────────────────────────────────┐
│                 Domain Layer                            │
│  • Business logic                                       │
│  • No CLI dependencies                                  │
│  • Testable in isolation                                │
│  • Returns Result<T, DomainError>                       │
└────────────────┬───────────────────────────────────────┘
                 │ Uses
                 v
┌────────────────────────────────────────────────────────┐
│              Infrastructure Layer                       │
│  • ggen-core (template engine)                         │
│  • ggen-marketplace (registry)                         │
│  • ggen-ai (LLM providers)                             │
│  • utils (config, logging)                             │
└────────────────────────────────────────────────────────┘
```

### 4.2 Domain Module Example

```rust
// cli/src/domain/project/mod.rs
use ggen_core::template::Engine;
use ggen_core::rdf::Graph;
use crate::domain::DomainResult;

pub async fn create_new_project(
    name: &str,
    project_type: &str,
) -> DomainResult<ProjectManifest> {
    // 1. Validate inputs (domain logic)
    validate_project_name(name)?;
    validate_project_type(project_type)?;

    // 2. Load template (infrastructure)
    let template = Engine::load_template(project_type)?;

    // 3. Generate files (domain logic)
    let files = template.generate_files(name)?;

    // 4. Create manifest (domain logic)
    Ok(ProjectManifest::new(name, files))
}

// Pure business logic - no CLI dependencies
fn validate_project_name(name: &str) -> DomainResult<()> {
    if name.is_empty() {
        return Err(DomainError::InvalidProjectName("empty"));
    }
    if name.contains('/') {
        return Err(DomainError::InvalidProjectName("slash"));
    }
    Ok(())
}
```

### 4.3 Error Handling Strategy

```rust
// cli/src/domain/mod.rs
pub type DomainResult<T> = Result<T, DomainError>;

#[derive(Debug, thiserror::Error)]
pub enum DomainError {
    #[error("Invalid project name: {0}")]
    InvalidProjectName(&'static str),

    #[error("Template not found: {0}")]
    TemplateNotFound(String),

    #[error("RDF validation failed: {0}")]
    RdfValidation(String),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}

// CLI layer converts to user-friendly messages
impl From<DomainError> for ggen_utils::error::Error {
    fn from(e: DomainError) -> Self {
        match e {
            DomainError::InvalidProjectName(reason) => {
                Error::new(format!("❌ Invalid project name: {}", reason))
            }
            // ... more conversions
        }
    }
}
```

---

## 5. Template Engine Updates

### 5.1 Frozen Section Support

**Feature**: Templates can mark sections as immutable across regenerations

```yaml
---
to: "src/{{name}}.rs"
vars:
  name: "example"
frozen_sections:
  - "impl"          # Freeze all impl blocks
  - "business_logic" # Freeze custom marker
  - "tests"         # Freeze test modules
---
pub struct {{name | capitalize}} {
    name: String,
}

// FREEZE START: impl
impl {{name | capitalize}} {
    // User modifications here are preserved
    pub fn custom_method(&self) -> String {
        self.name.clone()
    }
}
// FREEZE END: impl

// FREEZE START: business_logic
// User business logic preserved here
// FREEZE END: business_logic
```

**Implementation Architecture:**

```rust
// ggen-core/src/template/frozen.rs
pub struct FrozenSection {
    pub marker: String,       // e.g., "impl"
    pub start_line: usize,
    pub end_line: usize,
    pub content: String,      // Original user content
}

pub struct FrozenSectionParser {
    sections: Vec<FrozenSection>,
}

impl FrozenSectionParser {
    pub fn parse_existing(content: &str) -> Result<Self> {
        // Find all // FREEZE START: marker ... // FREEZE END: marker
        let regex = Regex::new(r"// FREEZE START: (\w+)\n(.*?)// FREEZE END: \1")?;
        // ... extract sections
    }

    pub fn merge_with_template(
        &self,
        template_output: &str,
        frozen_markers: &[String],
    ) -> Result<String> {
        // 1. Generate new template output
        // 2. For each frozen_marker in template frontmatter
        // 3. Replace template section with preserved user content
    }
}
```

**Data Flow:**

```
┌─────────────┐
│ Template    │ frozen_sections: ["impl", "tests"]
└──────┬──────┘
       │
       v
┌──────────────────────────────────────┐
│ Template Engine                       │
│ 1. Render template → output_new      │
│ 2. Read existing file → output_old   │
│ 3. Parse frozen sections              │
└──────┬───────────────────────────────┘
       │
       v
┌──────────────────────────────────────┐
│ FrozenSectionParser                   │
│ 1. Extract frozen blocks from old    │
│ 2. Merge into new output              │
│ 3. Preserve user modifications        │
└──────┬───────────────────────────────┘
       │
       v
┌──────────────┐
│ Final Output │ (template + frozen sections)
└──────────────┘
```

### 5.2 Enhanced Frontmatter

**Current (v1.2.0):**
```yaml
---
to: "output.rs"
vars:
  name: "example"
---
```

**Enhanced (v2.0.0):**
```yaml
---
to: "output.rs"
vars:
  name: "example"

# NEW: Frozen section support
frozen_sections:
  - "impl"
  - "business_logic"

# NEW: Template metadata
metadata:
  version: "1.0.0"
  author: "ggen"
  tags: ["rust", "struct"]

# NEW: RDF schema validation
schema: "schema.ttl"
validation:
  shacl: "user_schema.ttl"

# NEW: Streaming for large templates
streaming: true
stream_chunk_size: 1024
---
```

---

## 6. Filesystem Routing

### 6.1 Routing Table Generation

```rust
// cli/src/lib.rs
use clap_noun_verb::{auto_discover, RouteTable};

pub fn build_routing_table() -> Result<RouteTable> {
    let routes = auto_discover::<Cli>("cli/src/commands")?;

    // Generates routing table:
    // "utils doctor" → commands::utils::doctor::run
    // "project new" → commands::project::new::run
    // "market search" → commands::market::search::run

    Ok(routes)
}
```

### 6.2 Directory-Based Nouns

**Convention**: `commands/{noun}/{verb}.rs`

```
commands/
├── utils/
│   ├── mod.rs          # Noun configuration
│   └── doctor.rs       # #[verb] run() for "utils doctor"
├── project/
│   ├── mod.rs          # Noun: project
│   ├── new.rs          # Verb: new
│   ├── gen.rs          # Verb: gen
│   └── ...
└── market/
    ├── mod.rs          # Noun: market
    ├── search.rs       # Verb: search
    └── ...
```

**Noun Module Configuration:**
```rust
// commands/project/mod.rs
use clap_noun_verb::noun;

#[noun(
    name = "project",
    about = "Project scaffolding and generation",
    aliases = ["proj", "p"]
)]
pub mod project;
```

### 6.3 Auto-Discovery Process

```
1. Scan filesystem: cli/src/commands/**/*.rs
   ↓
2. Parse #[verb] attributes
   ↓
3. Parse #[noun] attributes from mod.rs files
   ↓
4. Build routing map:
   {
     "utils": {
       "doctor": commands::utils::doctor::run
     },
     "project": {
       "new": commands::project::new::run,
       "gen": commands::project::gen::run,
       ...
     }
   }
   ↓
5. Generate clap subcommands dynamically
   ↓
6. Route runtime calls: "ggen project new" → project::new::run()
```

---

## 7. Data Flow Architecture

### 7.1 Command Execution Flow

```
User Input: ggen project new my-app --type rust-web
     │
     v
┌─────────────────────────────────────────────────┐
│ CLI Layer (clap-noun-verb)                      │
│ 1. Parse: noun="project", verb="new"            │
│ 2. Lookup route table                           │
│ 3. Resolve: commands::project::new::run()       │
└────────────────┬────────────────────────────────┘
                 │
                 v Arguments validated

┌─────────────────────────────────────────────────┐
│ Command Handler (cli/src/commands/project/new) │
│ 1. Extract args: name="my-app", type="rust-web"│
│ 2. Validate inputs                              │
│ 3. Call domain layer                            │
└────────────────┬────────────────────────────────┘
                 │
                 v Domain function call

┌─────────────────────────────────────────────────┐
│ Domain Layer (cli/src/domain/project)           │
│ 1. Business logic: validate project name       │
│ 2. Load template for "rust-web"                │
│ 3. Generate file structure                      │
│ 4. Return ProjectManifest                       │
└────────────────┬────────────────────────────────┘
                 │
                 v Use infrastructure

┌─────────────────────────────────────────────────┐
│ Infrastructure (ggen-core)                      │
│ 1. Template engine renders files                │
│ 2. RDF graph validates metadata                 │
│ 3. Filesystem writes output                     │
└────────────────┬────────────────────────────────┘
                 │
                 v Success result

┌─────────────────────────────────────────────────┐
│ CLI Output                                       │
│ ✅ Created project: my-app                      │
│ 📁 Generated 12 files                           │
│ 🚀 Run: cd my-app && cargo run                 │
└─────────────────────────────────────────────────┘
```

### 7.2 Template Generation Flow

```
User: ggen project gen rust-service.tmpl --var name=auth
     │
     v
┌──────────────────────────────────────────┐
│ Load Template                             │
│ • Parse frontmatter (YAML)               │
│ • Extract: frozen_sections, vars, RDF   │
└──────┬───────────────────────────────────┘
       │
       v
┌──────────────────────────────────────────┐
│ Check Existing File                       │
│ • Does output file exist?                │
│ • Parse frozen sections if yes           │
└──────┬───────────────────────────────────┘
       │
       v
┌──────────────────────────────────────────┐
│ Render Template (Tera)                    │
│ • Apply variables                         │
│ • Execute SPARQL queries                  │
│ • Generate output_new                     │
└──────┬───────────────────────────────────┘
       │
       v
┌──────────────────────────────────────────┐
│ Merge Frozen Sections                     │
│ • For each frozen marker:                │
│   - Extract old content                   │
│   - Replace in output_new                │
└──────┬───────────────────────────────────┘
       │
       v
┌──────────────────────────────────────────┐
│ Write Final Output                        │
│ • Write to filesystem                     │
│ • Preserve permissions                    │
└──────────────────────────────────────────┘
```

### 7.3 RDF Integration Flow

```
Template with RDF
     │
     v
┌──────────────────────────────────────────┐
│ Parse Frontmatter                         │
│ rdf_inline: ["@prefix ...", "..."]       │
│ schema: "schema.ttl"                     │
│ validation: { shacl: "rules.ttl" }       │
└──────┬───────────────────────────────────┘
       │
       v
┌──────────────────────────────────────────┐
│ Load RDF Schema (if specified)            │
│ • Load schema.ttl into Oxigraph          │
│ • Parse ontology definitions             │
└──────┬───────────────────────────────────┘
       │
       v
┌──────────────────────────────────────────┐
│ Build RDF Graph                           │
│ • Parse rdf_inline triples               │
│ • Insert into Oxigraph store             │
└──────┬───────────────────────────────────┘
       │
       v
┌──────────────────────────────────────────┐
│ Validate RDF (if validation specified)    │
│ • Load SHACL rules                        │
│ • Validate graph structure                │
│ • Return validation errors               │
└──────┬───────────────────────────────────┘
       │
       v
┌──────────────────────────────────────────┐
│ Execute SPARQL Queries                    │
│ • Run queries from frontmatter           │
│ • Return bindings for template vars      │
└──────┬───────────────────────────────────┘
       │
       v
┌──────────────────────────────────────────┐
│ Template Rendering                        │
│ • Inject SPARQL results into Tera vars   │
│ • Generate final output                   │
└──────────────────────────────────────────┘
```

---

## 8. Migration Sequencing

### 8.1 Migration Phases

**Phase 1: Foundation (Week 1)**
- Set up domain layer structure
- Create domain/utils/doctor.rs as proof-of-concept
- Keep v1.2.0 cmds intact (no breaking changes)

**Phase 2: Core Commands (Week 2-3)**
- Migrate critical commands to domain layer:
  - project (new, gen, plan, apply)
  - market (search, add, install)
  - template (generate-tree)
- Implement frozen section support in ggen-core
- Test dual-layer execution (cmds + commands)

**Phase 3: Auto-Discovery (Week 4)**
- Integrate clap-noun-verb v3.0.0
- Implement filesystem routing
- Add #[verb] attributes to commands
- Test auto-discovery with subset of commands

**Phase 4: Full Migration (Week 5-6)**
- Migrate remaining commands to commands/
- Deprecate cmds/ directory
- Update documentation
- Performance benchmarks

**Phase 5: Enhanced Features (Week 7)**
- Template frozen sections
- RDF schema support
- Streaming generation
- Template metadata

**Phase 6: Cleanup (Week 8)**
- Remove deprecated cmds/
- Final testing
- Release v2.0.0

### 8.2 Backwards Compatibility Strategy

```rust
// cli/src/lib.rs
#[derive(Parser)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Command,
}

pub enum Command {
    // v2.0.0: Auto-discovered commands
    #[command(flatten)]
    V2(NounVerb),

    // v1.2.0: Legacy commands (deprecated)
    #[command(name = "legacy", hide = true)]
    Legacy(cmds::Commands),
}

impl Command {
    pub async fn run(&self) -> Result<()> {
        match self {
            Command::V2(nv) => nv.run().await,
            Command::Legacy(cmd) => {
                eprintln!("⚠️  Using deprecated command structure");
                eprintln!("   Migrate to: ggen {}", suggest_v2_syntax(cmd));
                cmd.run().await
            }
        }
    }
}
```

### 8.3 Deprecation Timeline

| Version | Status | Details |
|---------|--------|---------|
| v2.0.0 | Both active | cmds/ and commands/ coexist |
| v2.1.0 | Deprecation warnings | cmds/ shows migration hints |
| v2.2.0 | cmds/ removed | Only commands/ active |

---

## 9. API Design

### 9.1 Domain API Patterns

**Naming Convention:**
```rust
// Domain functions use business language
pub async fn create_new_project(...) -> DomainResult<ProjectManifest>
pub async fn generate_from_template(...) -> DomainResult<GeneratedFiles>
pub async fn search_marketplace(...) -> DomainResult<Vec<Package>>
```

**Input Validation:**
```rust
// Domain layer validates business rules
pub struct ProjectName(String);

impl ProjectName {
    pub fn new(name: impl Into<String>) -> DomainResult<Self> {
        let name = name.into();
        if name.is_empty() {
            return Err(DomainError::InvalidProjectName("empty"));
        }
        if name.contains('/') {
            return Err(DomainError::InvalidProjectName("no slashes"));
        }
        Ok(Self(name))
    }
}
```

**Return Types:**
```rust
// Domain returns rich types, not primitives
pub struct ProjectManifest {
    pub name: String,
    pub project_type: String,
    pub files: Vec<GeneratedFile>,
    pub dependencies: Vec<Dependency>,
}

pub struct GeneratedFile {
    pub path: PathBuf,
    pub content: String,
    pub frozen_sections: Vec<FrozenSection>,
}
```

### 9.2 Template Engine API

```rust
// ggen-core/src/template/engine.rs
pub struct TemplateEngine {
    tera: Tera,
    rdf_graph: Option<Graph>,
    frozen_parser: FrozenSectionParser,
}

impl TemplateEngine {
    pub fn new() -> Self { ... }

    // Load template with enhanced frontmatter
    pub fn load_template(&mut self, path: &Path) -> Result<Template> {
        let content = fs::read_to_string(path)?;
        let (frontmatter, body) = self.parse_frontmatter(&content)?;

        Ok(Template {
            frontmatter,
            body,
            path: path.to_path_buf(),
        })
    }

    // Render with frozen section support
    pub async fn render(
        &self,
        template: &Template,
        vars: &HashMap<String, String>,
    ) -> Result<String> {
        // 1. Render Tera template
        let mut output = self.tera.render_str(&template.body, vars)?;

        // 2. Execute SPARQL queries if RDF present
        if let Some(sparql_queries) = &template.frontmatter.sparql {
            output = self.inject_sparql_results(output, sparql_queries)?;
        }

        // 3. Merge frozen sections if output file exists
        if let Some(frozen_sections) = &template.frontmatter.frozen_sections {
            output = self.frozen_parser.merge(output, frozen_sections)?;
        }

        Ok(output)
    }
}
```

### 9.3 Frozen Section API

```rust
// ggen-core/src/template/frozen.rs
pub struct FrozenSectionParser;

impl FrozenSectionParser {
    // Parse existing file for frozen sections
    pub fn parse(&self, content: &str) -> Result<Vec<FrozenSection>> {
        let regex = Regex::new(
            r"// FREEZE START: (?P<marker>\w+)\n(?P<content>.*?)// FREEZE END: (?P=marker)"
        )?;

        regex.captures_iter(content)
            .map(|cap| FrozenSection {
                marker: cap["marker"].to_string(),
                content: cap["content"].to_string(),
            })
            .collect()
    }

    // Merge frozen sections into new template output
    pub fn merge(
        &self,
        template_output: &str,
        frozen_markers: &[String],
    ) -> Result<String> {
        let existing_sections = self.parse(template_output)?;

        let mut output = template_output.to_string();
        for section in existing_sections {
            if frozen_markers.contains(&section.marker) {
                // Replace template section with preserved content
                output = self.replace_section(output, &section)?;
            }
        }

        Ok(output)
    }
}
```

---

## 10. Quality Attributes

### 10.1 Performance

| Metric | Target | Measurement |
|--------|--------|-------------|
| First build | ≤3s | `cargo make build-release` |
| Incremental build | ≤2s | `cargo make quick` |
| CLI startup | ≤100ms | Time to first output |
| Template generation | ≤1s | Simple template render |
| RDF query | ≤500ms | SPARQL execution |
| Frozen section merge | ≤200ms | Parse + merge |
| Memory usage | ≤100MB | Peak RSS |

### 10.2 Testability

**Domain Layer Testing:**
```rust
// cli/src/domain/project/mod.rs tests
#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_create_project_valid_name() {
        let result = create_new_project("my-app", "rust-web").await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_create_project_invalid_name() {
        let result = create_new_project("my/app", "rust-web").await;
        assert!(matches!(
            result,
            Err(DomainError::InvalidProjectName(_))
        ));
    }
}
```

**Integration Testing:**
```rust
// tests/integration/commands_test.rs
#[tokio::test]
async fn test_project_new_command() {
    let output = run_cli(&["project", "new", "test-app"]).await;
    assert!(output.success);
    assert!(Path::new("test-app/Cargo.toml").exists());
}
```

### 10.3 Maintainability

**Code Organization:**
- Max file size: 500 lines
- Max function complexity: 10
- Max module depth: 4 levels
- Test coverage: ≥90%

**Documentation:**
```rust
/// Create a new project from a template
///
/// # Arguments
/// * `name` - Project name (alphanumeric + hyphens)
/// * `project_type` - Template type (rust-web, rust-cli, etc.)
///
/// # Returns
/// * `Ok(ProjectManifest)` - Project created successfully
/// * `Err(DomainError)` - Validation or generation failed
///
/// # Examples
/// ```
/// let manifest = create_new_project("my-app", "rust-web").await?;
/// assert_eq!(manifest.name, "my-app");
/// ```
pub async fn create_new_project(
    name: &str,
    project_type: &str,
) -> DomainResult<ProjectManifest>
```

### 10.4 Security

**Input Validation:**
- All user inputs validated at domain layer
- Path traversal prevention: `../` rejected
- Template injection prevention: escape user vars
- SPARQL injection prevention: parameterized queries

**Dependencies:**
```toml
# Cargo.toml - security-conscious dependencies
[dependencies]
clap = "4.5"                  # CLI parsing
clap-noun-verb = "3.0"        # Auto-discovery
tera = "1.19"                 # Template engine
oxigraph = "0.3"              # RDF store
thiserror = "1.0"             # Error handling

[dev-dependencies]
cargo-audit = "0.18"          # Security audits
```

---

## 11. Decision Records

### ADR-001: Use clap-noun-verb for Auto-Discovery

**Context**: v1.2.0 requires manual enum registration for every command
**Decision**: Adopt clap-noun-verb v3.0.0 for filesystem-based routing
**Consequences**:
- ✅ Easier to add new commands (no central registration)
- ✅ Clear noun-verb command structure
- ⚠️ Migration complexity (dual systems during transition)
- ❌ Dependency on external crate (mitigated by stability of clap ecosystem)

### ADR-002: Separate Domain from CLI Layer

**Context**: Business logic mixed with CLI argument parsing
**Decision**: Create domain/ layer with zero CLI dependencies
**Consequences**:
- ✅ Domain logic testable in isolation
- ✅ CLI can change without affecting business logic
- ✅ Easier to add alternative interfaces (TUI, web API)
- ⚠️ More boilerplate (command wrapper + domain function)

### ADR-003: Frozen Sections in Templates

**Context**: Users lose modifications when regenerating templates
**Decision**: Implement frozen section markers in template engine
**Consequences**:
- ✅ User modifications preserved across regenerations
- ✅ Clear markers for protected code
- ⚠️ Parser complexity for section extraction
- ❌ Potential conflicts if markers malformed

### ADR-004: Backwards Compatibility via Dual Systems

**Context**: Breaking changes in v2.0.0 affect existing users
**Decision**: Support both cmds/ and commands/ during migration
**Consequences**:
- ✅ Gradual migration path
- ✅ No breaking changes in v2.0.0
- ⚠️ Maintenance burden for dual systems
- ✅ Deprecation warnings guide users

---

## 12. Migration Checklist

### 12.1 Implementation Tasks

**Phase 1: Foundation**
- [ ] Create `cli/src/domain/` directory structure
- [ ] Implement domain error types
- [ ] Migrate `utils/doctor` to domain layer
- [ ] Add integration tests for domain layer
- [ ] Document domain layer patterns

**Phase 2: Core Commands**
- [ ] Migrate `project new` to domain + commands
- [ ] Migrate `project gen` to domain + commands
- [ ] Migrate `market search` to domain + commands
- [ ] Implement frozen section parser in ggen-core
- [ ] Add frozen section tests

**Phase 3: Auto-Discovery**
- [ ] Add clap-noun-verb dependency
- [ ] Implement auto-discovery in lib.rs
- [ ] Add #[verb] attributes to commands
- [ ] Test routing table generation
- [ ] Benchmark auto-discovery overhead

**Phase 4: Template Enhancements**
- [ ] Add frozen_sections to frontmatter schema
- [ ] Implement FrozenSectionParser
- [ ] Add merge logic to template engine
- [ ] Test frozen section preservation
- [ ] Document frozen section usage

**Phase 5: Full Migration**
- [ ] Migrate all remaining commands
- [ ] Add deprecation warnings to cmds/
- [ ] Update all documentation
- [ ] Update examples to use v2.0.0 syntax
- [ ] Run full test suite

**Phase 6: Release**
- [ ] Performance benchmarks (vs v1.2.0)
- [ ] Security audit (cargo audit)
- [ ] Update CHANGELOG.md
- [ ] Tag v2.0.0 release
- [ ] Publish to crates.io

### 12.2 Testing Requirements

**Unit Tests:**
- [ ] Domain layer: 100% coverage for public APIs
- [ ] Frozen section parser: all edge cases
- [ ] Auto-discovery: route resolution
- [ ] Command validation: all error paths

**Integration Tests:**
- [ ] Full command execution (project new, gen, etc.)
- [ ] Frozen section end-to-end (generate → modify → regenerate)
- [ ] Auto-discovery with real filesystem
- [ ] RDF integration with frozen sections

**Performance Tests:**
- [ ] CLI startup time ≤100ms
- [ ] Template generation ≤1s
- [ ] Frozen section merge ≤200ms
- [ ] Auto-discovery overhead ≤50ms

---

## 13. Next Steps

### Immediate Actions (Next 48 Hours)

1. **Code Agent**: Implement domain layer foundation
   - Create `cli/src/domain/mod.rs`
   - Implement `DomainError` types
   - Migrate `utils/doctor` as proof-of-concept

2. **Test Agent**: Create domain layer test suite
   - Unit tests for doctor domain logic
   - Integration tests for commands/utils/doctor

3. **Documentation Agent**: Update architecture docs
   - Document domain layer patterns
   - Create migration guide for v1.2.0 → v2.0.0

### Week 1 Goals

- [ ] Domain layer foundation complete
- [ ] utils/doctor migrated and tested
- [ ] Frozen section parser prototype
- [ ] Auto-discovery proof-of-concept

### Success Criteria

- ✅ Zero breaking changes for v1.2.0 users
- ✅ All tests passing (600+ tests)
- ✅ Performance maintained (≤3s generation)
- ✅ Documentation complete
- ✅ Migration guide published

---

## Appendix A: File Structure Comparison

### v1.2.0 (Current)
```
cli/src/
├── lib.rs                    # Manual clap setup
├── cmds/                     # Manual enum registration
│   ├── mod.rs               # Central Commands enum
│   ├── doctor.rs            # Monolithic command
│   ├── project/mod.rs       # Nested subcommands
│   └── ...
```

### v2.0.0 (Target)
```
cli/src/
├── lib.rs                    # Auto-discovery setup
├── commands/                 # Auto-discovered commands
│   ├── utils/
│   │   ├── mod.rs           # Noun config
│   │   └── doctor.rs        # #[verb] thin wrapper
│   ├── project/
│   └── ...
├── domain/                   # Business logic
│   ├── utils/
│   │   ├── mod.rs
│   │   └── doctor.rs        # Pure business logic
│   ├── project/
│   └── ...
└── cmds/                     # Deprecated (v2.0 only)
```

---

## Appendix B: Code Examples

### Example: Full Command Implementation (v2.0.0)

**CLI Layer:**
```rust
// cli/src/commands/project/new.rs
use clap::Args;
use clap_noun_verb::verb;
use crate::domain::project;

#[derive(Args, Debug)]
pub struct NewArgs {
    /// Project name
    name: String,

    /// Project type (rust-web, rust-cli, etc.)
    #[arg(short = 't', long)]
    project_type: String,
}

#[verb]
pub async fn run(args: &NewArgs) -> Result<()> {
    project::create_new_project(&args.name, &args.project_type)
        .await
        .map_err(Into::into)
}
```

**Domain Layer:**
```rust
// cli/src/domain/project/mod.rs
use ggen_core::template::Engine;

pub async fn create_new_project(
    name: &str,
    project_type: &str,
) -> DomainResult<ProjectManifest> {
    // Business logic
    let project_name = ProjectName::new(name)?;
    let template = load_template(project_type).await?;
    let files = template.generate(&project_name)?;

    Ok(ProjectManifest {
        name: project_name.0,
        files,
    })
}
```

---

**End of Architecture Design Document**

**Next Phase**: Implementation (Code Agent to execute this design)
