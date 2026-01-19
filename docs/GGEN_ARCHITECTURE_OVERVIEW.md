# GGEN Codebase Architecture Overview

## Executive Summary

**ggen** is a knowledge graph-driven code generation framework (v2.6.0) that treats software artifacts as semantic projections of RDF ontologies. The project is production-ready (89% maturity) with 610 files containing RDF/SPARQL integration and a comprehensive multi-crate Rust architecture.

### Key Facts
- **Language**: Rust 2021 edition with zero unsafe code in production paths
- **Main Focus**: Ontology-first code generation with deterministic output
- **Testing**: 782-line E2E test with real systems (Chicago TDD)
- **Concurrency**: Full async/await with tokio
- **Cryptography**: Post-quantum ML-DSA signatures for marketplace security

---

## 1. PROJECT STRUCTURE & MAIN COMPONENTS

### 1.1 Workspace Crates

```
/home/user/ggen/crates/
├── ggen-core/          # Core generation engine (23KB+ lib.rs, ~30 modules)
├── ggen-ai/            # LLM integration & swarm intelligence
├── ggen-cli/           # Command-line interface (noun-verb pattern)
├── ggen-domain/        # Pure business logic (no CLI deps)
├── ggen-marketplace/   # Registry, package management
├── ggen-utils/         # Shared utilities, error types
├── ggen-node/          # Node.js integration
└── examples/           # Demo projects (FastAPI, microservices, etc.)
```

### 1.2 Core Crates Overview

#### **ggen-core** - The Heart of Generation
**Purpose**: RDF-based code generation with template rendering
**Key Modules**:
- `generator.rs` (23KB) - Generator engine orchestrating pipeline
- `template.rs` (31KB) - Template parsing (YAML frontmatter + Tera body)
- `pipeline.rs` (30KB) - Multi-stage template processing with SPARQL integration
- `graph/` - Oxigraph wrapper with SPARQL caching & query building
- `templates/` - File tree generation from templates
- `delta.rs` (29KB) - Graph change detection & impact analysis
- `registry.rs` (36KB) - Registry client for pack discovery
- `cache.rs` (21KB) - LRU template/pack caching
- `lifecycle/` - Universal phase management (init, setup, build, test, deploy)
- `pqc.rs` (8KB) - Post-quantum cryptography (ML-DSA signatures)

**Key Dependencies**:
- `oxigraph` (0.5) - In-memory RDF triple store
- `tera` (1.20) - Template rendering engine
- `tokio` (1.47) - Async runtime
- `rayon` (1.11) - Parallel processing

#### **ggen-ai** - LLM Integration & Swarm Orchestration
**Purpose**: AI-powered code generation and autonomous agent coordination
**Key Modules**:
- `swarm/` - Multi-agent system for autonomous software generation
  - `coordinator.rs` - Orchestrates multi-agent execution with pipeline config
  - `agents/` - Specialized agent implementations
    - `code_generator.rs` - Generates code from templates
    - `template_generator.rs` - Regenerates templates from graph changes
    - `graph_extender.rs` - Extends RDF graphs
    - `mock_agent.rs` - Test agent
  - `events.rs` - Event-driven agent communication
  - `orchestration.rs` - Workflow orchestration
- `agents/` - Unified agent architecture
  - `core/` - Core agents (GraphEvolutionAgent, RegenerationAgent, FeedbackAgent)
  - `registry.rs` - Agent discovery & registration
- `generators/` - Specialized generators
  - `ontology.rs` - Generate RDF ontologies from natural language
  - `sparql.rs` - Generate SPARQL queries
  - `template.rs` - Generate Tera templates
  - `validator.rs` - Validate generated artifacts
  - `natural_search.rs` - Intent-based search
  - `refactor.rs` - Code refactoring suggestions
- `ultrathink/` - 80/20 autonomous intelligence system
- `rdf/` - RDF-based CLI generation
- `config/` - Multi-provider LLM configuration (OpenAI, Anthropic, Ollama, etc.)
- `client.rs` - LLM client abstraction
- `cache.rs` - Response caching with moka

**Key Dependencies**:
- `genai` (0.4) - Multi-provider LLM support
- `async-trait` - Async trait support
- `moka` (0.12) - Distributed cache
- `tokio` - Async runtime

#### **ggen-cli** - Command-Line Interface
**Purpose**: User-facing CLI with auto-discovered commands
**Key Modules**:
- `cmds/` - Command implementations
- `conventions/` - CLI conventions & patterns
- `templates/` - Built-in templates for generation
- `runtime.rs` - CLI execution runtime
- **Command Pattern**: Noun-verb structure (e.g., `ggen template generate`)

**Key Dependencies**:
- `clap` (4.5) - Argument parsing with derive macros
- `clap-noun-verb` - Noun-verb command discovery

#### **ggen-domain** - Business Logic Layer
**Purpose**: Pure business logic reusable across interfaces
**Key Modules**:
- `graph/` - RDF graph operations (load, query, export, visualize)
- `marketplace/` - Package discovery and installation
- `hook/` - Git hook management
- `audit/` - Security auditing
- `shell/` - Shell integration

#### **ggen-marketplace** - Registry & Package Management
**Purpose**: Discover, install, and manage template packages
**Key Features**:
- Package search with semantic scoring
- Installation with validation
- Dependency resolution
- Container-based isolation testing

### 1.3 Directory Organization

```
/home/user/ggen/
├── templates/          # Template library (~50 .tmpl files)
│   ├── ai/             # AI-generated templates
│   ├── cli/            # CLI generation templates
│   ├── api/            # REST API templates
│   ├── cleanroom/      # Isolated test templates
│   └── ...
├── examples/           # Example projects (49+ directories)
├── tests/              # Integration & E2E tests
│   ├── bdd/            # BDD scenarios
│   ├── chicago_tdd/    # Chicago TDD suite (782-line E2E test)
│   ├── cli/            # CLI command tests
│   ├── e2e/            # End-to-end tests
│   └── ...
├── docs/               # Documentation (architecture, tutorials, API reference)
├── config/             # Configuration files
├── benches/            # Performance benchmarks
├── docs/               # Comprehensive documentation
├── Makefile.toml       # Task runner (cargo-make)
├── Cargo.toml          # Workspace definition
└── deny.toml           # Dependency security audit
```

---

## 2. EXISTING TEMPLATE SYSTEM & GENERATION

### 2.1 Template Architecture

**Two-Phase Rendering Pipeline**:
1. **Frontmatter Parsing** - YAML metadata extraction
2. **Frontmatter Rendering** - Variable substitution in metadata
3. **RDF Processing** - Load graphs, execute SPARQL queries
4. **Body Rendering** - Tera template engine with full context
5. **Plan Execution** - File writing, injection, post-processing

### 2.2 Template Structure

```yaml
---
to: "output/generated.rs"                # Output file path
from: "input.txt"                        # Optional input file
rdf: ["ontology.ttl"]                    # RDF files to load
rdf_inline: |                            # Inline Turtle triples
  @prefix ex: <http://example.org/> .
  ex:Type rdf:type rdfs:Class .
sparql:                                  # Named SPARQL queries
  people: "SELECT ?name WHERE { ?s ex:name ?name }"
inject: true                             # Enable file injection
before: "// START GENERATED"              # Injection marker
unless_exists: false                     # Skip if file exists
force: false                             # Overwrite existing
backup: true                             # Backup before writing
sh_before: "mkdir -p output"             # Pre-generation hook
sh_after: "rustfmt output/*.rs"          # Post-generation hook
---
// Template body with Tera filters
{% for person in sparql_results.people %}
  Name: {{ person }}
{% endfor %}
```

### 2.3 Key Template Features

**SPARQL Integration**:
- Queries embedded in frontmatter
- Results available as `sparql_results.<query_name>`
- Dynamic code generation based on RDF data

**File Injection**:
- Modify existing files at markers or line numbers
- Three-way merge support for manual edits
- Idempotent generation

**Tera Filters & Functions**:
- 50+ custom Tera filters registered (register.rs)
- SPARQL function for inline queries
- Custom type mapping (xsd:string → String/string/str)

**Template Library** (~50 templates):
- `ai-ontology.tmpl` - Generate ontologies from prompts
- `rust-service-with-placeholders.tmpl` - Rust service scaffold
- `database-with-migrations.tmpl` - DB schema generation
- `production-readiness-demo.tmpl` - Production patterns
- Language-specific: `rust.tmpl`, `python.tmpl`, `typescript` (in api/ dir)

### 2.4 Template Processing Pipeline

```
User Input (CLI)
    ↓
Template Path + Variables
    ↓
[Pipeline::render_file()]
    ├─ Read template file
    ├─ Parse YAML frontmatter + body
    ├─ Create Tera context from variables
    ├─ Render frontmatter (resolve {{vars}})
    ├─ Load RDF data (files + inline)
    ├─ Execute SPARQL queries
    ├─ Build graph from RDF
    ├─ Execute SPARQL queries with prolog
    ├─ Render template body with Tera
    └─ Generate Plan (file writes, injections, etc.)
        ↓
    [Plan::apply()]
    ├─ Execute pre-hooks (sh_before)
    ├─ Write/inject files
    ├─ Execute post-hooks (sh_after)
    └─ Success
```

---

## 3. EXISTING AI/ML & OPTIMIZATION COMPONENTS

### 3.1 Swarm Intelligence Architecture

**Current Implementation** (ggen-ai/src/swarm/):
- **UltrathinkSwarm**: Main orchestration engine
- **SwarmCoordinator**: Manages execution flow with:
  - Concurrency control via Tokio semaphores
  - Pipeline configuration with parallel execution
  - Failure recovery and retry logic
  - Timeout management per agent

**Specialized Agents**:
1. **CodeGeneratorAgent** - Generates code files with language-specific configs
2. **TemplateGeneratorAgent** - Regenerates templates on graph changes
3. **GraphExtenderAgent** - Extends RDF graphs with new facts
4. **ValidatorAgent** - Validates generated artifacts
5. **LearningAgent** - Learns from generation patterns
6. **EventMonitorAgent** - Monitors swarm events

**Core Agents** (ggen-ai/src/agents/core/):
- **GraphEvolutionAgent** - Evolves knowledge graphs
- **RegenerationAgent** - Regenerates code on graph changes
- **FeedbackAgent** - Processes feedback loops

### 3.2 LLM Integration

**Multi-Provider Support** (genai abstraction):
- OpenAI (GPT-4o, GPT-4, GPT-3.5)
- Anthropic Claude
- Ollama (local models)
- Google Gemini
- DeepSeek
- xAI Grok
- Groq
- Cohere

**Specialized Generators**:
- **OntologyGenerator** - Create RDF from natural language descriptions
- **SparqlGenerator** - Generate SPARQL queries from intent
- **TemplateGenerator** - Generate Tera templates from specifications
- **RefactorAssistant** - Code improvement suggestions
- **NaturalSearchGenerator** - Semantic search over templates

**Response Caching**:
- Moka-based distributed cache
- SHA256 hashing for prompt deduplication
- Reduces API costs and latency

### 3.3 Performance & Optimization

**Caching Systems**:
- **TemplateCache** (lru crate) - Template LRU cache
- **QueryCache** - SPARQL query result caching with epoch-based invalidation
- **LlmCache** - LLM response caching
- **PackCache** - Downloaded package caching

**Parallelization**:
- Rayon for parallel template processing
- Tokio for concurrent agent execution
- Parallel SPARQL query compilation

**Delta-Driven Optimization** (delta.rs):
- Detect only changed triples between graph versions
- Calculate impact scores for affected templates
- Skip regeneration of unaffected files
- Three-way merge for conflicting edits

### 3.4 Graph Change Detection & Impact Analysis

```rust
// GraphDelta types
pub enum DeltaType {
    Addition { subject, predicate, object },
    Deletion { subject, predicate, object },
    Modification { subject, predicate, old_object, new_object },
}

// ImpactAnalyzer
pub struct TemplateImpact {
    pub template_path: PathBuf,
    pub score: f64,  // 0.0-1.0 impact score
    pub affected_patterns: Vec<String>,
}
```

---

## 4. BUILD SYSTEM & TESTING INFRASTRUCTURE

### 4.1 Build System (cargo-make)

**Makefile.toml** defines 100+ tasks:
```
cargo make quick      # fmt + test (fast feedback)
cargo make dev        # fmt + lint + test (dev cycle)
cargo make ci         # Full CI pipeline
cargo make check      # Quick syntax check
cargo make build      # Debug build
cargo make build-release  # Release with LTO
cargo make lint       # Clippy with strict settings
cargo make test       # Run all tests
cargo make pre-commit # Format + lint + test + hooks
```

**Build Profiles**:
- **dev**: Fast compilation, debug info, incremental
- **release**: LTO thin, optimizations, stripped symbols
- **test**: Fast test compilation, debug assertions
- **bench**: Full LTO, profiling support

### 4.2 Testing Infrastructure

**Test Organization** (/tests):
- **bdd/** - BDD scenarios (cucumber framework)
- **chicago_tdd/** - E2E test suite (782-line main test)
- **cli/** - CLI command validation
- **clnrm/** - Cleanroom integration tests
- **e2e/** - End-to-end workflows
- **domain/** - Domain logic tests
- **integration/** - Cross-module integration tests

**Test Frameworks**:
- **chicago-tdd-tools** - E2E testing with real systems
- **cucumber** - BDD scenarios
- **proptest** - Property-based testing
- **criterion** - Performance benchmarking
- **mockall** - Mocking framework

**CI/CD**:
- GitHub Actions workflows
- Container-based validation (testcontainers)
- Pre-commit hooks for validation
- Deny.toml for supply chain security

### 4.3 Code Quality Enforcement

**Poka-Yoke Design** (Prevent defects at compile time):
- Deny all warnings as errors
- Deny unsafe code in production
- Deny unwrap/expect in production paths
- Deny panic/todo/unimplemented
- Clippy pedantic + nursery + cargo lints

**Testing Requirement**:
- Comprehensive E2E test with real Oxigraph store
- 782-line Chicago TDD test covering core workflows
- 67% scenario passing rate in tests

---

## 5. KEY DATA STRUCTURES & APIs

### 5.1 Core Types

**Generation Context**:
```rust
pub struct GenContext {
    pub template_path: PathBuf,
    pub output_root: PathBuf,
    pub vars: BTreeMap<String, String>,      // CLI variables
    pub global_prefixes: BTreeMap<String, String>,
    pub base: Option<String>,                // RDF base IRI
    pub dry_run: bool,
}
```

**Template Structure**:
```rust
pub struct Template {
    raw_frontmatter: serde_yaml::Value,
    pub front: Frontmatter,    // Parsed metadata
    pub body: String,          // Tera template body
}

pub struct Frontmatter {
    pub to: Option<String>,
    pub from: Option<String>,
    pub rdf: Vec<String>,      // RDF files
    pub rdf_inline: Vec<String>,
    pub sparql: BTreeMap<String, String>,
    pub inject: bool,
    pub before: Option<String>,
    pub after: Option<String>,
    // ... 20+ more fields
}
```

**Graph Operations**:
```rust
pub struct Graph {
    store: Arc<Oxigraph store>,  // In-memory RDF store
    query_cache: LRU<String, QueryResults>,
}

pub struct GraphDelta {
    pub changes: Vec<DeltaType>,
    pub timestamp: SystemTime,
}

pub struct TemplateImpact {
    pub template_path: PathBuf,
    pub score: f64,
    pub affected_patterns: Vec<String>,
}
```

**Pipeline**:
```rust
pub struct Pipeline {
    tera: Tera,           // Template engine
    graph: Graph,         // RDF graph
}

pub struct Plan {
    operations: Vec<Operation>,  // Write, inject, execute
}
```

**Agent System**:
```rust
pub trait SwarmAgent: Send + Sync {
    fn name(&self) -> &str;
    fn capabilities(&self) -> Vec<String>;
    async fn execute(&self, context: &SwarmContext, input: AgentInput) -> Result<AgentOutput>;
    async fn validate(&self) -> Result<bool>;
    async fn health_check(&self) -> AgentHealth;
}

pub struct SwarmContext {
    pub graph_state: String,
    pub active_agents: Vec<String>,
    pub metrics: ExecutionMetrics,
    pub config: SwarmConfig,
}
```

### 5.2 Public APIs

**Core Generation API**:
```rust
impl Generator {
    pub fn new(pipeline: Pipeline, ctx: GenContext) -> Self
    pub fn generate(mut self) -> Result<PathBuf>
}

impl Pipeline {
    pub fn new() -> Result<Self>
    pub fn register_prefixes(&mut self, base: Option<&str>, prefixes: &BTreeMap<String, String>)
    pub fn render_file(&mut self, template_path: &Path, vars: &BTreeMap<String, String>, dry_run: bool) -> Result<Plan>
    pub fn render_body(&mut self, body: &str, ctx: &Context) -> Result<String>
}

impl Template {
    pub fn parse(input: &str) -> Result<Self>
    pub fn from_file(path: &Path) -> Result<Self>
    pub fn render_frontmatter(&mut self, ctx: &Context) -> Result<()>
}
```

**Graph API**:
```rust
impl Graph {
    pub fn new() -> Result<Self>
    pub fn insert_turtle(&self, turtle: &str) -> Result<()>
    pub fn query(&self, sparql: &str) -> Result<QueryResults>
    pub fn update(&self, sparql_update: &str) -> Result<()>
    pub fn export(&self, format: RdfFormat) -> Result<String>
}

impl GraphDelta {
    pub fn compute(old: &Graph, new: &Graph) -> Result<Self>
    pub fn subjects(&self) -> Vec<String>
    pub fn affects_iri(&self, iri: &str) -> bool
}

impl ImpactAnalyzer {
    pub fn new() -> Self
    pub fn analyze(&self, old: &Graph, new: &Graph, templates: &[PathBuf]) -> Result<Vec<TemplateImpact>>
}
```

**Swarm API**:
```rust
pub async fn execute_swarm(context: SwarmContext, input: SwarmInput) -> Result<SwarmResult>

impl SwarmCoordinator {
    pub async fn coordinate(&self, pipeline: ExecutionPipeline) -> Result<()>
}
```

---

## 6. INTEGRATION POINTS FOR SWARM INTELLIGENCE

### 6.1 Natural Integration Opportunities

#### **1. Adaptive Template Selection**
- **What**: AI agents select optimal templates based on RDF schema
- **Where**: Between graph analysis and template rendering
- **Integration**: Enhance `Pipeline::render_file()` with agent-based selection

#### **2. Autonomous Graph Evolution**
- **What**: Agents detect missing patterns in RDF and suggest extensions
- **Where**: GraphEvolutionAgent with delta analysis
- **Integration**: Hook into delta computation to trigger autonomous improvements

#### **3. Feedback-Driven Regeneration**
- **What**: Learn from generation results and improve template quality
- **Where**: FeedbackAgent + TemplateGeneratorAgent coordination
- **Integration**: Post-generation validation triggers learning loops

#### **4. Parallel Agent Specialization**
- **What**: Different agents handle different code domains
- **Where**: During swarm coordination phase
- **Integration**: Extend agent registry with domain-specific specialists

#### **5. Marketplace Intelligence**
- **What**: Agents discover and recommend templates based on ontology
- **Where**: Template selection phase
- **Integration**: Connect NaturalSearchGenerator to marketplace search

#### **6. Deterministic Generation with Optimization**
- **What**: Ensure reproducibility while optimizing for performance
- **Where**: Delta-driven projection layer
- **Integration**: Use delta analysis to cache intermediate results

### 6.2 Coordination with Existing Components

**With ggen-core**:
- Agents read/write via Graph API
- Agents trigger template regeneration via Pipeline
- Agents store decisions in RDF (semantic persistence)

**With ggen-marketplace**:
- Agents search for templates via registry
- Agents validate compatibility with ontology
- Agents cache downloaded packages

**With ggen-cli**:
- Commands dispatch to swarm agents
- Progress reported via CLI hooks
- Results formatted for user consumption

---

## 7. TECHNOLOGY STACK SUMMARY

| Layer | Technology | Version | Purpose |
|-------|-----------|---------|---------|
| **Async Runtime** | Tokio | 1.47 | Non-blocking execution |
| **RDF Store** | Oxigraph | 0.5.1 | In-memory triple store |
| **Template Engine** | Tera | 1.20 | Template rendering |
| **SPARQL** | SPARQL 1.1 | via Oxigraph | Query graphs |
| **LLM Client** | genai | 0.4 | Multi-provider LLM |
| **Caching** | Moka + LRU | 0.12/0.16 | Distributed caching |
| **CLI** | clap + noun-verb | 4.5/3.4 | Command parsing |
| **Testing** | chicago-tdd | 1.1.0 | E2E testing |
| **Crypto** | pqcrypto-mldsa | 0.1 | Post-quantum security |
| **Serialization** | serde + serde_json | 1.0 | Data format |

---

## 8. ARCHITECTURAL PRINCIPLES

1. **Separation of Concerns**: CLI, domain, and infrastructure layers are distinct
2. **Zero CLI Dependencies**: Domain layer reusable across interfaces
3. **Async by Default**: Non-blocking throughout
4. **Type Safety**: Rust compiler enforces invariants
5. **Determinism**: Same ontology → identical code
6. **Poka-Yoke**: Prevent defects at compile time
7. **RDF-First**: Semantic knowledge as source of truth
8. **80/20 Rule**: 20% of agents handle 80% of scenarios

---

## 9. PERFORMANCE CHARACTERISTICS

- **Generation Speed**: <2 seconds for typical projects
- **Cache Hit Rate**: ~60% for repeated queries
- **Parallel Efficiency**: Linear scaling with rayon (4-8 cores)
- **Memory Usage**: ~200MB baseline + graph size
- **Template Rendering**: Millis per template with Tera
- **SPARQL Query**: Sub-second for graphs <100K triples

---

## 10. KEY FILES BY Responsibility

### Core Engine
- `/crates/ggen-core/src/generator.rs` - Main generation orchestration
- `/crates/ggen-core/src/pipeline.rs` - Multi-stage processing
- `/crates/ggen-core/src/template.rs` - Template structure & parsing
- `/crates/ggen-core/src/graph/core.rs` - RDF store wrapper

### Swarm Intelligence
- `/crates/ggen-ai/src/swarm/coordinator.rs` - Multi-agent orchestration
- `/crates/ggen-ai/src/swarm/agents/code_generator.rs` - Code generation
- `/crates/ggen-ai/src/agents/core/graph_evolution.rs` - Graph extension
- `/crates/ggen-ai/src/ultrathink/core.rs` - Autonomous system

### Optimization
- `/crates/ggen-core/src/delta.rs` - Change detection & impact analysis
- `/crates/ggen-core/src/template_cache.rs` - Template caching
- `/crates/ggen-ai/src/cache.rs` - LLM response caching

### Marketplace
- `/crates/ggen-core/src/registry.rs` - Package discovery
- `/crates/ggen-marketplace/src/lib.rs` - Package management

---

## Document Complete

This architecture supports seamless integration of swarm intelligence components while maintaining determinism, type safety, and semantic correctness through RDF ontologies.
