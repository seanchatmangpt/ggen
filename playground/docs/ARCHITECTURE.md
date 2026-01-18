# HTF Playground - Comprehensive System Architecture

## Executive Summary

This document presents a comprehensive improved architecture for the clap-noun-verb playground (HTF - Hyper-Thesis Framework). The design leverages proven patterns from the ggen workspace while introducing modular layering, semantic RDF storage, and extensible plugin architecture.

---

## 1. Architecture Overview - Three-Layer Design

```
+============================================================================+
|                           PRESENTATION LAYER                                |
|  (CLI Interface, Output Formatters, Interactive Shell)                      |
+============================================================================+
                                    |
                                    v
+============================================================================+
|                             DOMAIN LAYER                                    |
|  (Business Logic, Validation, Scheduling, Profiling, Checking)              |
+============================================================================+
                                    |
                                    v
+============================================================================+
|                          INFRASTRUCTURE LAYER                               |
|  (RDF Store, Template Engine, Config Management, Plugin System)             |
+============================================================================+
```

### Layer Responsibilities

| Layer          | Responsibility                                  | Dependencies      |
|----------------|------------------------------------------------|-------------------|
| Presentation   | CLI parsing, output formatting, user interaction | Domain only       |
| Domain         | Business logic, validation, orchestration       | Infrastructure    |
| Infrastructure | Data persistence, external services, templates  | None (pure infra) |

---

## 2. Component Interaction Diagram

```
+-----------------------------------------------------------------------------------+
|                              PRESENTATION LAYER                                    |
|                                                                                   |
|  +---------------+    +----------------+    +----------------+    +-----------+   |
|  | CLI Parser    |    | Output         |    | Interactive    |    | Progress  |   |
|  | (clap-noun-   |--->| Formatter      |--->| Shell          |    | Reporter  |   |
|  |  verb v5.1)   |    | (JSON/YAML/    |    | (REPL mode)    |    | (indicatif)|  |
|  +---------------+    |  Table/Plain)  |    +----------------+    +-----------+   |
|         |             +----------------+            |                   |         |
|         |                    |                      |                   |         |
+---------+--------------------+----------------------+-------------------+---------+
          |                    |                      |                   |
          v                    v                      v                   v
+-----------------------------------------------------------------------------------+
|                                DOMAIN LAYER                                        |
|                                                                                   |
|  +------------------+  +------------------+  +------------------+                  |
|  | Scheduler        |  | Profiler         |  | Checker          |                  |
|  | (Lambda-Order)   |  | (Pi-Profile)     |  | (Gamma-Check)    |                  |
|  +--------+---------+  +--------+---------+  +--------+---------+                  |
|           |                     |                     |                            |
|           v                     v                     v                            |
|  +------------------------------------------------------------------+              |
|  |                    Domain Service Layer                          |              |
|  |  +-------------+  +-------------+  +-------------+  +----------+ |              |
|  |  | Thesis      |  | Shard       |  | Chapter     |  | Config   | |              |
|  |  | Service     |  | Service     |  | Service     |  | Service  | |              |
|  |  +-------------+  +-------------+  +-------------+  +----------+ |              |
|  +------------------------------------------------------------------+              |
|           |                     |                     |                            |
|           v                     v                     v                            |
|  +------------------------------------------------------------------+              |
|  |                    Domain Model Layer                            |              |
|  |  +-------------+  +-------------+  +-------------+  +----------+ |              |
|  |  | DeltaShard  |  | ShardFamily |  | PiProfile   |  | Chapter  | |              |
|  |  | (Entity)    |  | (Value Obj) |  | (Aggregate) |  | (Entity) | |              |
|  |  +-------------+  +-------------+  +-------------+  +----------+ |              |
|  +------------------------------------------------------------------+              |
|                                                                                   |
+-----------------------------------------------------------------------------------+
          |                     |                     |
          v                     v                     v
+-----------------------------------------------------------------------------------+
|                           INFRASTRUCTURE LAYER                                     |
|                                                                                   |
|  +------------------+  +------------------+  +------------------+                  |
|  | RDF Repository   |  | Template Engine  |  | Config Manager   |                  |
|  | (Oxigraph)       |  | (Tera)           |  | (TOML/YAML)      |                  |
|  +--------+---------+  +--------+---------+  +--------+---------+                  |
|           |                     |                     |                            |
|  +------------------+  +------------------+  +------------------+                  |
|  | Plugin System    |  | Event Bus        |  | Telemetry        |                  |
|  | (Dynamic Load)   |  | (Pub/Sub)        |  | (OpenTelemetry)  |                  |
|  +------------------+  +------------------+  +------------------+                  |
|                                                                                   |
+-----------------------------------------------------------------------------------+
```

---

## 3. Data Flow Diagram

```
User Input                                                              User Output
    |                                                                       ^
    v                                                                       |
+-------+     +----------+     +-----------+     +----------+     +--------+
| CLI   |---->| Command  |---->| Domain    |---->| Result   |---->| Output |
| Parse |     | Dispatch |     | Execute   |     | Transform|     | Format |
+-------+     +----------+     +-----------+     +----------+     +--------+
                  |                  |                                  ^
                  v                  v                                  |
            +----------+      +------------+                           |
            | Pre-Hook |      | Post-Hook  |---------------------------+
            | (Validate)|     | (Log/Cache)|
            +----------+      +------------+
                  |                  |
                  v                  v
            +-----------------------------------------+
            |           RDF Knowledge Graph           |
            |  +-------------+    +-------------+     |
            |  | Thesis      |    | Ontology    |     |
            |  | Triples     |    | Schema      |     |
            |  +-------------+    +-------------+     |
            +-----------------------------------------+
                          |
                          v
            +-----------------------------------------+
            |        SPARQL Query Engine              |
            |  - Schema validation queries            |
            |  - Coverage analysis queries            |
            |  - Dependency traversal queries         |
            +-----------------------------------------+
```

### Data Flow Steps

1. **Input Parsing**: clap-noun-verb parses CLI arguments into typed commands
2. **Pre-Hook Execution**: Validation, authentication, rate limiting
3. **Domain Dispatch**: Command routed to appropriate domain service
4. **RDF Query/Mutation**: Domain service interacts with knowledge graph
5. **Post-Hook Execution**: Logging, caching, telemetry emission
6. **Result Transformation**: Domain result converted to output model
7. **Output Formatting**: Result formatted per user preference (JSON/YAML/Table)

---

## 4. Domain Model Hierarchy

### 4.1 Core Entities

```
+------------------------------------------------------------------+
|                    DOMAIN MODEL HIERARCHY                         |
+------------------------------------------------------------------+

                         Thesis (Aggregate Root)
                              |
         +--------------------+--------------------+
         |                    |                    |
    ChapterPlan          PiProfile           GammaCheck
         |                    |                    |
    +----+----+          +----+----+          +----+----+
    |         |          |         |          |         |
 Chapter   Chapter    Coverage  Coverage   Passed   Failed
    |         |       (Family)  (Family)  Invariants
    |         |
 Shards    Shards
    |
+---+---+---+---+
|   |   |   |   |
DeltaShard (26 Families)
```

### 4.2 Value Objects

```rust
// Type-safe IDs (prevents primitive obsession)
pub struct ThesisId(Uuid);
pub struct ShardId(String);
pub struct ChapterId(usize);

// Immutable value objects
pub struct WordCount(usize);
pub struct CoveragePercent(f32); // 0.0 - 100.0
pub struct LambdaPosition(usize); // Position in canonical ordering
```

### 4.3 Aggregate Boundaries

| Aggregate      | Root Entity | Child Entities        | Invariants                    |
|----------------|-------------|----------------------|-------------------------------|
| Thesis         | Thesis      | ChapterPlan, Shards  | No cyclic dependencies        |
| PiProfile      | PiProfile   | Coverage map         | Coverage sums to 100%         |
| GammaCheck     | GammaCheck  | Invariant results    | All Q-invariants evaluated    |

---

## 5. Template System Architecture

### 5.1 Template Hierarchy

```
templates/
├── thesis/
│   ├── chapter.tera           # Chapter rendering
│   ├── shard.tera             # Shard content rendering
│   └── toc.tera               # Table of contents
├── export/
│   ├── latex.tera             # LaTeX export
│   ├── markdown.tera          # Markdown export
│   └── html.tera              # HTML export
├── reports/
│   ├── coverage.tera          # Pi-Profile report
│   ├── validation.tera        # Gamma-Check report
│   └── schedule.tera          # Lambda-Schedule report
└── ontology/
    ├── htf-core.ttl           # Core HTF ontology
    ├── htf-families.ttl       # Shard family definitions
    └── htf-constraints.shacl  # SHACL shapes for validation
```

### 5.2 Template Engine Design

```
+--------------------------------------------------------------------+
|                     TEMPLATE ENGINE                                 |
+--------------------------------------------------------------------+
|                                                                    |
|  +-----------------+     +------------------+     +--------------+ |
|  | Template        |     | Context          |     | Output       | |
|  | Registry        |---->| Builder          |---->| Renderer     | |
|  | (Discovery)     |     | (RDF + Domain)   |     | (Tera)       | |
|  +-----------------+     +------------------+     +--------------+ |
|         |                        |                       |         |
|         v                        v                       v         |
|  +-----------------+     +------------------+     +--------------+ |
|  | Template        |     | SPARQL           |     | Post-        | |
|  | Validator       |     | Injector         |     | Processor    | |
|  | (Syntax Check)  |     | (RDF Binding)    |     | (Format)     | |
|  +-----------------+     +------------------+     +--------------+ |
|                                                                    |
+--------------------------------------------------------------------+
```

### 5.3 Extension Points

```rust
/// Template extension trait
pub trait TemplateExtension: Send + Sync {
    fn name(&self) -> &str;
    fn register_filters(&self, tera: &mut Tera);
    fn register_functions(&self, tera: &mut Tera);
}

/// Built-in extensions
pub struct WordCountFilter;    // {{ content | word_count }}
pub struct MarkdownFilter;     // {{ content | markdown }}
pub struct RdfQueryFunction;   // {{ sparql("SELECT...") }}
```

---

## 6. RDF/SPARQL Layer Architecture

### 6.1 Knowledge Graph Schema

```
+--------------------------------------------------------------------+
|                    HTF KNOWLEDGE GRAPH                              |
+--------------------------------------------------------------------+
|                                                                    |
|  +-------------------+          +-------------------+              |
|  | htf:Thesis        |<-------->| htf:ChapterPlan   |              |
|  | - id: xsd:string  |  hasChapters | - number: int |              |
|  | - title: string   |          | - title: string   |              |
|  | - created: date   |          | - words: int      |              |
|  +-------------------+          +-------------------+              |
|          |                               |                         |
|          | hasShards                     | containsShards          |
|          v                               v                         |
|  +-------------------+                                             |
|  | htf:DeltaShard    |<------------------------------------------+ |
|  | - id: xsd:string  |                                           | |
|  | - name: string    |          +-------------------+            | |
|  | - content: string |<-------->| htf:ShardFamily   |            | |
|  | - status: enum    |  hasFamily | - id: F01-F26   |            | |
|  | - words: int      |          | - name: string    |            | |
|  +-------------------+          | - lambdaPos: int  |            | |
|          |                      +-------------------+            | |
|          | dependsOn                                             | |
|          +-------------------------------------------------------+ |
|                                                                    |
+--------------------------------------------------------------------+
```

### 6.2 SPARQL Query Patterns

```sparql
# Coverage Analysis Query
PREFIX htf: <http://thesis.local/htf#>
SELECT ?family (SUM(?words) as ?total_words)
       (COUNT(?shard) as ?shard_count)
WHERE {
    ?shard a htf:DeltaShard ;
           htf:hasFamily ?family ;
           htf:wordCount ?words .
}
GROUP BY ?family
ORDER BY ?family

# Dependency Cycle Detection Query
PREFIX htf: <http://thesis.local/htf#>
ASK {
    ?a htf:dependsOn+ ?a .
}

# Lambda Order Validation Query
PREFIX htf: <http://thesis.local/htf#>
SELECT ?shard ?dep ?shardPos ?depPos
WHERE {
    ?shard htf:dependsOn ?dep ;
           htf:hasFamily/htf:lambdaPosition ?shardPos .
    ?dep htf:hasFamily/htf:lambdaPosition ?depPos .
    FILTER(?depPos >= ?shardPos)
}
```

### 6.3 Repository Pattern

```rust
/// RDF Repository abstraction
#[async_trait]
pub trait RdfRepository: Send + Sync {
    // Queries
    async fn find_thesis(&self, id: &ThesisId) -> Result<Option<Thesis>>;
    async fn find_shards(&self, thesis_id: &ThesisId) -> Result<Vec<DeltaShard>>;
    async fn find_shard(&self, id: &ShardId) -> Result<Option<DeltaShard>>;

    // Commands
    async fn save_thesis(&self, thesis: &Thesis) -> Result<()>;
    async fn save_shard(&self, shard: &DeltaShard) -> Result<()>;
    async fn delete_shard(&self, id: &ShardId) -> Result<()>;

    // SPARQL
    async fn execute_query(&self, sparql: &str) -> Result<QueryResults>;
    async fn execute_update(&self, sparql: &str) -> Result<()>;
}

/// Oxigraph implementation
pub struct OxigraphRepository {
    store: Store,
    namespace: String,
}
```

---

## 7. Middleware and Plugin Architecture

### 7.1 Middleware Pipeline

```
+--------------------------------------------------------------------+
|                    MIDDLEWARE PIPELINE                              |
+--------------------------------------------------------------------+
|                                                                    |
|  Request -->  +--------+   +--------+   +--------+   +---------+   |
|               | Auth   |-->| Rate   |-->| Logging|-->| Metrics |   |
|               | Check  |   | Limit  |   | Pre    |   | Start   |   |
|               +--------+   +--------+   +--------+   +---------+   |
|                                                           |        |
|                                                           v        |
|                                                    +------------+  |
|                                                    | Handler    |  |
|                                                    | (Domain)   |  |
|                                                    +------------+  |
|                                                           |        |
|  Response <-- +--------+   +--------+   +--------+   +---------+   |
|               | Format |<--| Cache  |<--| Logging|<--| Metrics |   |
|               | Output |   | Store  |   | Post   |   | End     |   |
|               +--------+   +--------+   +--------+   +---------+   |
|                                                                    |
+--------------------------------------------------------------------+
```

### 7.2 Middleware Trait

```rust
#[async_trait]
pub trait Middleware: Send + Sync {
    /// Called before command execution
    async fn before(&self, ctx: &mut Context) -> Result<()>;

    /// Called after command execution
    async fn after(&self, ctx: &mut Context, result: &CommandResult) -> Result<()>;

    /// Middleware priority (lower = earlier)
    fn priority(&self) -> i32 { 0 }

    /// Middleware name for debugging
    fn name(&self) -> &str;
}

/// Context passed through middleware chain
pub struct Context {
    pub command: String,
    pub args: HashMap<String, Value>,
    pub user: Option<User>,
    pub start_time: Instant,
    pub metadata: HashMap<String, Value>,
}
```

### 7.3 Plugin System

```
+--------------------------------------------------------------------+
|                      PLUGIN SYSTEM                                  |
+--------------------------------------------------------------------+
|                                                                    |
|  +-------------------+        +-------------------+                |
|  | Plugin Registry   |        | Plugin Loader     |                |
|  | - register()      |<-------| - load_plugin()   |                |
|  | - unregister()    |        | - validate()      |                |
|  | - get_plugins()   |        | - initialize()    |                |
|  +-------------------+        +-------------------+                |
|          |                            |                            |
|          v                            v                            |
|  +------------------------------------------------------------+   |
|  |                    Plugin Interface                         |   |
|  +------------------------------------------------------------+   |
|  |  pub trait Plugin: Send + Sync {                           |   |
|  |      fn name(&self) -> &str;                               |   |
|  |      fn version(&self) -> Version;                         |   |
|  |      fn init(&mut self, config: &Config) -> Result<()>;    |   |
|  |      fn shutdown(&mut self) -> Result<()>;                 |   |
|  |      fn commands(&self) -> Vec<Box<dyn Command>>;          |   |
|  |      fn middleware(&self) -> Vec<Box<dyn Middleware>>;     |   |
|  |      fn template_extensions(&self) -> Vec<Box<dyn Ext>>;   |   |
|  |  }                                                         |   |
|  +------------------------------------------------------------+   |
|                                                                    |
|  Built-in Plugins:                                                 |
|  +-------------+  +-------------+  +-------------+  +------------+ |
|  | Core HTF    |  | Export      |  | AI Assist   |  | Statistics | |
|  | (Scheduler, |  | (LaTeX,     |  | (LLM        |  | (Analytics,| |
|  |  Profiler,  |  |  Markdown,  |  |  Integration)|  |  Charts)   | |
|  |  Checker)   |  |  HTML)      |  |             |  |            | |
|  +-------------+  +-------------+  +-------------+  +------------+ |
|                                                                    |
+--------------------------------------------------------------------+
```

---

## 8. Error Handling and Logging Systems

### 8.1 Error Hierarchy

```rust
/// Domain-specific errors with context
#[derive(Error, Debug)]
pub enum HtfError {
    // Domain errors
    #[error("Thesis not found: {id}")]
    ThesisNotFound { id: ThesisId },

    #[error("Invariant violation: {invariant} - {reason}")]
    InvariantViolation { invariant: String, reason: String },

    #[error("Cyclic dependency detected: {path:?}")]
    CyclicDependency { path: Vec<ShardId> },

    #[error("Invalid lambda ordering: {shard} depends on {dependency}")]
    LambdaOrderViolation { shard: ShardId, dependency: ShardId },

    // Infrastructure errors
    #[error("RDF store error: {0}")]
    RdfError(#[from] oxigraph::Error),

    #[error("Template rendering error: {0}")]
    TemplateError(#[from] tera::Error),

    #[error("Configuration error: {0}")]
    ConfigError(String),

    // Plugin errors
    #[error("Plugin load error: {plugin} - {reason}")]
    PluginLoadError { plugin: String, reason: String },
}

/// Result type alias
pub type Result<T> = std::result::Result<T, HtfError>;
```

### 8.2 Logging Architecture

```
+--------------------------------------------------------------------+
|                    LOGGING ARCHITECTURE                             |
+--------------------------------------------------------------------+
|                                                                    |
|  +------------------+         +------------------+                  |
|  | tracing          |-------->| Subscriber       |                  |
|  | (Instrumentation)|         | Registry         |                  |
|  +------------------+         +------------------+                  |
|                                       |                            |
|              +------------------------+------------------------+    |
|              |                        |                        |    |
|              v                        v                        v    |
|  +------------------+     +------------------+     +----------------+|
|  | Console Layer    |     | File Layer       |     | OTLP Layer   | |
|  | (Human readable) |     | (JSON structured)|     | (Telemetry)  | |
|  +------------------+     +------------------+     +----------------+|
|                                                                    |
|  Log Levels:                                                       |
|  - TRACE: Detailed debugging (SPARQL queries, template rendering)  |
|  - DEBUG: Development info (command flow, middleware execution)    |
|  - INFO:  Normal operation (command start/end, thesis operations)  |
|  - WARN:  Recoverable issues (missing optional config, deprecation)|
|  - ERROR: Failures (validation errors, RDF errors, plugin errors)  |
|                                                                    |
+--------------------------------------------------------------------+
```

### 8.3 Structured Logging

```rust
use tracing::{info, warn, error, instrument, Span};

#[instrument(skip(self), fields(thesis_id = %thesis_id))]
async fn schedule_thesis(&self, thesis_id: &ThesisId) -> Result<ChapterPlan> {
    info!("Starting thesis scheduling");

    let shards = self.repository.find_shards(thesis_id).await
        .map_err(|e| {
            error!(error = %e, "Failed to load shards");
            e
        })?;

    let plan = self.scheduler.schedule(shards)?;

    info!(chapters = plan.chapters.len(), "Scheduling complete");
    Ok(plan)
}
```

---

## 9. Configuration Management System

### 9.1 Configuration Hierarchy

```
+--------------------------------------------------------------------+
|                CONFIGURATION HIERARCHY                              |
+--------------------------------------------------------------------+
|                                                                    |
|  Priority (highest to lowest):                                     |
|                                                                    |
|  1. CLI Arguments (--config, --data-dir, etc.)                     |
|         |                                                          |
|         v                                                          |
|  2. Environment Variables (HTF_DATA_DIR, HTF_LOG_LEVEL)            |
|         |                                                          |
|         v                                                          |
|  3. Project Config (.htf/config.toml in project root)              |
|         |                                                          |
|         v                                                          |
|  4. User Config (~/.config/htf/config.toml)                        |
|         |                                                          |
|         v                                                          |
|  5. Default Values (compiled into binary)                          |
|                                                                    |
+--------------------------------------------------------------------+
```

### 9.2 Configuration Schema

```toml
# ~/.config/htf/config.toml

[core]
data_dir = "~/.htf/data"
log_level = "info"
output_format = "json"  # json | yaml | table | plain

[rdf]
store_type = "memory"   # memory | disk
store_path = "~/.htf/store"
default_graph = "http://thesis.local/default"

[scheduler]
default_chapter_size = 2000
min_shards_per_chapter = 1
max_shards_per_chapter = 10

[profiler]
coverage_threshold = 0.05  # Minimum % to show in report
show_empty_families = true

[checker]
strict_mode = false        # Fail on any invariant violation
skip_invariants = []       # List of invariant names to skip

[templates]
custom_dir = "~/.htf/templates"
cache_enabled = true
cache_ttl = 3600

[plugins]
enabled = ["core", "export"]
disabled = []
plugin_dir = "~/.htf/plugins"

[telemetry]
enabled = false
endpoint = "http://localhost:4317"
service_name = "htf-cli"
```

### 9.3 Configuration Service

```rust
/// Configuration service with layered resolution
pub struct ConfigService {
    cli_args: Option<CliConfig>,
    env_config: EnvConfig,
    project_config: Option<FileConfig>,
    user_config: Option<FileConfig>,
    defaults: DefaultConfig,
}

impl ConfigService {
    /// Get configuration value with priority resolution
    pub fn get<T: ConfigValue>(&self, key: &str) -> T {
        // Try each layer in priority order
        self.cli_args.as_ref().and_then(|c| c.get(key))
            .or_else(|| self.env_config.get(key))
            .or_else(|| self.project_config.as_ref().and_then(|c| c.get(key)))
            .or_else(|| self.user_config.as_ref().and_then(|c| c.get(key)))
            .unwrap_or_else(|| self.defaults.get(key))
    }

    /// Validate configuration
    pub fn validate(&self) -> Result<()> {
        // Validate all required values present
        // Validate value ranges
        // Validate file paths exist
        Ok(())
    }
}
```

---

## 10. Future Extensibility Plan

### 10.1 Extension Points Summary

| Extension Point      | Purpose                          | Implementation                |
|---------------------|----------------------------------|------------------------------|
| New Commands        | Add CLI commands                 | Plugin trait + command trait |
| Output Formats      | Add output formatters            | OutputFormatter trait        |
| Shard Families      | Add new family types             | RDF ontology extension       |
| Validation Rules    | Add Q-invariants                 | Invariant trait              |
| Template Filters    | Add Tera filters/functions       | TemplateExtension trait      |
| Export Formats      | Add thesis export formats        | Exporter trait               |
| Storage Backends    | Add RDF store backends           | RdfRepository trait          |

### 10.2 New Command Types

```rust
/// Command trait for extensible commands
#[async_trait]
pub trait Command: Send + Sync {
    fn name(&self) -> &str;
    fn description(&self) -> &str;
    fn args(&self) -> Vec<ArgSpec>;

    async fn execute(&self, ctx: &Context) -> Result<CommandResult>;
}

// Future command examples:
// - htf ai suggest   - AI-powered shard suggestions
// - htf collaborate  - Multi-user thesis collaboration
// - htf cite         - Citation management
// - htf visualize    - Generate thesis visualization
// - htf sync         - Cloud synchronization
```

### 10.3 New Output Formats

```rust
/// Output formatter trait
pub trait OutputFormatter: Send + Sync {
    fn name(&self) -> &str;
    fn content_type(&self) -> &str;

    fn format<T: Serialize>(&self, data: &T) -> Result<String>;
}

// Future formats:
// - GraphViz DOT    - Dependency graph visualization
// - Mermaid         - Markdown-compatible diagrams
// - CSV             - Spreadsheet export
// - XML             - Structured data exchange
```

---

## 11. Test Architecture

### 11.1 Testing Pyramid

```
                    +-------------------+
                   /  E2E Tests (10%)   \
                  /   (Full CLI flow)    \
                 +------------------------+
                /   Integration Tests      \
               /     (25%)                  \
              /  (RDF store, Templates)      \
             +--------------------------------+
            /        Unit Tests (65%)          \
           /   (Domain logic, Validators,       \
          /     Schedulers, Profilers)           \
         +----------------------------------------+
```

### 11.2 Test Organization

```
tests/
├── unit/
│   ├── domain/
│   │   ├── scheduler_test.rs
│   │   ├── profiler_test.rs
│   │   └── checker_test.rs
│   ├── models/
│   │   ├── shard_test.rs
│   │   └── thesis_test.rs
│   └── services/
│       ├── thesis_service_test.rs
│       └── shard_service_test.rs
├── integration/
│   ├── rdf/
│   │   ├── repository_test.rs
│   │   └── sparql_test.rs
│   ├── templates/
│   │   ├── rendering_test.rs
│   │   └── export_test.rs
│   └── plugins/
│       └── plugin_loading_test.rs
├── e2e/
│   ├── cli_commands_test.rs
│   ├── full_workflow_test.rs
│   └── error_scenarios_test.rs
└── fixtures/
    ├── sample_thesis.ttl
    ├── sample_shards.json
    └── templates/
```

### 11.3 Test Patterns (Chicago TDD)

```rust
/// Unit test example (Chicago TDD - state-based)
#[cfg(test)]
mod scheduler_tests {
    use super::*;

    #[test]
    fn schedule_chapters_respects_lambda_ordering() {
        // Arrange
        let shards = vec![
            create_shard("intro", ShardFamily::Intro, 500),
            create_shard("method", ShardFamily::Method, 500),
            create_shard("result", ShardFamily::Result, 500),
        ];

        // Act
        let plan = schedule_chapters(shards, 600).unwrap();

        // Assert - verify observable state
        assert_eq!(plan.chapters.len(), 3);
        assert!(plan.chapters[0].families.contains(&ShardFamily::Intro));
        assert!(plan.chapters[1].families.contains(&ShardFamily::Method));
    }

    #[test]
    fn schedule_chapters_combines_small_shards() {
        // Arrange
        let shards = vec![
            create_shard("a", ShardFamily::Intro, 100),
            create_shard("b", ShardFamily::Intro, 100),
        ];

        // Act
        let plan = schedule_chapters(shards, 500).unwrap();

        // Assert
        assert_eq!(plan.chapters.len(), 1);
        assert_eq!(plan.chapters[0].shards.len(), 2);
    }
}
```

---

## 12. Performance Considerations

### 12.1 Performance SLOs

| Operation                    | Target      | Measurement Point         |
|-----------------------------|-------------|---------------------------|
| CLI startup                 | < 100ms     | First byte of output      |
| Small thesis schedule       | < 500ms     | 10 shards, 5k words       |
| Large thesis schedule       | < 5s        | 100 shards, 100k words    |
| SPARQL simple query         | < 50ms      | Single triple pattern     |
| SPARQL complex query        | < 500ms     | Multi-join with filter    |
| Template rendering          | < 100ms     | Standard chapter template |
| Full validation (Gamma)     | < 1s        | All Q-invariants          |

### 12.2 Optimization Strategies

```
+--------------------------------------------------------------------+
|                PERFORMANCE OPTIMIZATION                             |
+--------------------------------------------------------------------+
|                                                                    |
|  1. CACHING                                                        |
|  +------------------+  +------------------+  +------------------+   |
|  | SPARQL Results   |  | Template         |  | Validation       |   |
|  | (LRU, 1000 items)|  | (Pre-compiled)   |  | (Incremental)    |   |
|  +------------------+  +------------------+  +------------------+   |
|                                                                    |
|  2. LAZY LOADING                                                   |
|  - Load shards on demand (not full thesis at startup)              |
|  - Defer SPARQL execution until results needed                     |
|  - Stream large outputs instead of buffering                       |
|                                                                    |
|  3. PARALLELIZATION                                                |
|  - Rayon for CPU-bound operations (validation, profiling)          |
|  - Tokio for I/O-bound operations (RDF store, file I/O)           |
|  - Parallel template rendering for multi-chapter export            |
|                                                                    |
|  4. MEMORY EFFICIENCY                                              |
|  - Use string interning for repeated IRI values                    |
|  - Avoid cloning large content strings (use Arc<str>)              |
|  - Stream-based processing for large thesis export                 |
|                                                                    |
+--------------------------------------------------------------------+
```

---

## 13. Scalability Plan

### 13.1 Scaling Dimensions

```
+--------------------------------------------------------------------+
|                    SCALABILITY DIMENSIONS                           |
+--------------------------------------------------------------------+
|                                                                    |
|  VERTICAL SCALING (Single Instance)                                |
|  +---------------------------------------------------------------+ |
|  | - In-memory RDF store with optional disk persistence          | |
|  | - Multi-threaded validation and profiling                     | |
|  | - Efficient SPARQL query optimization                         | |
|  | - Template caching and pre-compilation                        | |
|  +---------------------------------------------------------------+ |
|                                                                    |
|  HORIZONTAL SCALING (Future - Multi-User)                          |
|  +---------------------------------------------------------------+ |
|  | - Shared RDF triplestore (Apache Jena Fuseki, GraphDB)        | |
|  | - Distributed caching (Redis)                                 | |
|  | - Event-driven architecture for real-time collaboration       | |
|  | - Cloud storage for thesis content (S3, GCS)                  | |
|  +---------------------------------------------------------------+ |
|                                                                    |
+--------------------------------------------------------------------+
```

### 13.2 Growth Path

| Phase   | Users      | Theses    | Architecture                          |
|---------|------------|-----------|---------------------------------------|
| Phase 1 | 1 (local)  | 10        | Embedded Oxigraph, file storage       |
| Phase 2 | 10         | 100       | Shared Oxigraph, local collaboration  |
| Phase 3 | 100+       | 1000+     | Distributed store, cloud deployment   |

---

## 14. Best Practices Integration

### 14.1 Rust Best Practices

- **Type-First Thinking**: Use newtypes (`ThesisId`, `ShardId`) to prevent primitive obsession
- **Zero-Cost Abstractions**: Traits for extensibility without runtime overhead
- **Error Handling**: `Result<T, E>` everywhere, no panics in library code
- **Memory Safety**: Ownership semantics, no unsafe code
- **API Design**: Builder patterns for complex construction

### 14.2 DDD Best Practices

- **Bounded Contexts**: Thesis, Scheduling, Profiling, Checking as separate contexts
- **Aggregate Roots**: Thesis is the aggregate root containing shards
- **Value Objects**: Immutable, identity-less objects (WordCount, CoveragePercent)
- **Repository Pattern**: Abstract RDF storage behind repository interface

### 14.3 SOLID Principles

- **S**: Each module has single responsibility (scheduler schedules, profiler profiles)
- **O**: Open for extension via traits, closed for modification
- **L**: Subtypes substitutable (any RdfRepository implementation works)
- **I**: Focused interfaces (separate traits for queries vs. mutations)
- **D**: High-level modules depend on abstractions (traits, not concrete types)

---

## 15. Implementation Roadmap

### Phase 1: Foundation (Week 1-2)
- [ ] Refactor into three-layer architecture
- [ ] Implement RdfRepository trait with Oxigraph
- [ ] Create domain service layer
- [ ] Add proper error hierarchy

### Phase 2: Core Features (Week 3-4)
- [ ] Implement middleware pipeline
- [ ] Add configuration management system
- [ ] Integrate structured logging
- [ ] Create template system with Tera

### Phase 3: Extensibility (Week 5-6)
- [ ] Implement plugin system
- [ ] Add export plugins (LaTeX, Markdown, HTML)
- [ ] Create template extension system
- [ ] Add custom command support

### Phase 4: Polish (Week 7-8)
- [ ] Performance optimization
- [ ] Comprehensive test suite
- [ ] Documentation generation
- [ ] Release preparation

---

## Appendix A: Directory Structure

```
playground/
├── Cargo.toml
├── src/
│   ├── main.rs                    # Entry point
│   ├── lib.rs                     # Library exports
│   │
│   ├── presentation/              # Presentation Layer
│   │   ├── mod.rs
│   │   ├── cli.rs                 # CLI parsing
│   │   ├── commands/              # Command handlers
│   │   │   ├── mod.rs
│   │   │   ├── schedule.rs
│   │   │   ├── profile.rs
│   │   │   └── check.rs
│   │   ├── output/                # Output formatters
│   │   │   ├── mod.rs
│   │   │   ├── json.rs
│   │   │   ├── yaml.rs
│   │   │   └── table.rs
│   │   └── interactive.rs         # REPL mode
│   │
│   ├── domain/                    # Domain Layer
│   │   ├── mod.rs
│   │   ├── models/                # Domain models
│   │   │   ├── mod.rs
│   │   │   ├── thesis.rs
│   │   │   ├── shard.rs
│   │   │   ├── chapter.rs
│   │   │   └── profile.rs
│   │   ├── services/              # Domain services
│   │   │   ├── mod.rs
│   │   │   ├── thesis_service.rs
│   │   │   ├── shard_service.rs
│   │   │   └── chapter_service.rs
│   │   ├── scheduler.rs           # Lambda scheduling
│   │   ├── profiler.rs            # Pi profiling
│   │   ├── checker.rs             # Gamma checking
│   │   └── validators/            # Validation rules
│   │       ├── mod.rs
│   │       └── invariants.rs
│   │
│   ├── infrastructure/            # Infrastructure Layer
│   │   ├── mod.rs
│   │   ├── rdf/                   # RDF/SPARQL
│   │   │   ├── mod.rs
│   │   │   ├── repository.rs
│   │   │   ├── oxigraph_impl.rs
│   │   │   └── queries.rs
│   │   ├── templates/             # Template engine
│   │   │   ├── mod.rs
│   │   │   ├── registry.rs
│   │   │   └── extensions.rs
│   │   ├── config/                # Configuration
│   │   │   ├── mod.rs
│   │   │   └── service.rs
│   │   ├── plugins/               # Plugin system
│   │   │   ├── mod.rs
│   │   │   ├── loader.rs
│   │   │   └── registry.rs
│   │   └── telemetry/             # Observability
│   │       ├── mod.rs
│   │       └── tracing.rs
│   │
│   ├── middleware/                # Middleware
│   │   ├── mod.rs
│   │   ├── logging.rs
│   │   ├── metrics.rs
│   │   └── cache.rs
│   │
│   └── error.rs                   # Error types
│
├── templates/                     # Tera templates
│   ├── thesis/
│   ├── export/
│   └── reports/
│
├── ontology/                      # RDF ontologies
│   ├── htf-core.ttl
│   ├── htf-families.ttl
│   └── htf-constraints.shacl
│
├── tests/                         # Test suites
│   ├── unit/
│   ├── integration/
│   └── e2e/
│
└── docs/                          # Documentation
    ├── ARCHITECTURE.md            # This document
    └── API.md                     # API documentation
```

---

## Appendix B: Key Type Definitions

```rust
// Core domain types (type-safe IDs)
pub struct ThesisId(pub Uuid);
pub struct ShardId(pub String);
pub struct ChapterId(pub usize);

// Value objects
pub struct WordCount(pub usize);
pub struct CoveragePercent(pub f32);
pub struct LambdaPosition(pub usize);

// Aggregate root
pub struct Thesis {
    pub id: ThesisId,
    pub title: String,
    pub created_at: DateTime<Utc>,
    pub shards: Vec<DeltaShard>,
    pub metadata: ThesisMetadata,
}

// Entity
pub struct DeltaShard {
    pub id: ShardId,
    pub name: String,
    pub family: ShardFamily,
    pub content: String,
    pub status: ShardStatus,
    pub dependencies: Vec<ShardId>,
    pub word_count: WordCount,
}

// Value object (26 families)
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum ShardFamily {
    // IMRaD
    Intro, Method, Result, Discussion,
    // Thesis-by-Papers
    Paper, Synthesis,
    // Argument
    Claim, Ground, Proof, Objection, Reply,
    // Contribution
    Gap, Design, Evaluation, Impact,
    // Monograph
    Context, Canon, Analysis, Conclusion,
    // DSR
    Problem, Artifact, Theory,
    // Narrative
    Field, Voice, Pattern, Insight,
}
```

---

*Document Version: 1.0.0*
*Last Updated: 2025-11-21*
*Author: System Architecture Designer*
