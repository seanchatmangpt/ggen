<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [GGen v7 Architecture Design - Post Protocol Layer Removal](#ggen-v7-architecture-design---post-protocol-layer-removal)
  - [Executive Summary](#executive-summary)
  - [1. Current Crate Structure Analysis](#1-current-crate-structure-analysis)
    - [1.1 Domain Organization](#11-domain-organization)
    - [1.2 Dependency Graph Analysis](#12-dependency-graph-analysis)
      - [1.2.1 Core Dependencies](#121-core-dependencies)
      - [1.2.2 Circular Dependency Prevention](#122-circular-dependency-prevention)
      - [1.2.3 Duplicate Dependencies Mitigation](#123-duplicate-dependencies-mitigation)
  - [2. Separation of Concerns](#2-separation-of-concerns)
    - [2.1 Core System Layer](#21-core-system-layer)
      - [2.1.1 ggen-core (Main Engine)](#211-ggen-core-main-engine)
      - [2.1.2 ggen-utils (Foundation)](#212-ggen-utils-foundation)
    - [2.2 Domain Layer](#22-domain-layer)
      - [2.2.1 ggen-domain (Business Logic)](#221-ggen-domain-business-logic)
      - [2.2.2 ggen-ontology-core (Ontology Processing)](#222-ggen-ontology-core-ontology-processing)
  - [3. Integration Patterns](#3-integration-patterns)
    - [3.1 ggen-agent Integration](#31-ggen-agent-integration)
      - [3.1.1 Simplified Protocol Architecture](#311-simplified-protocol-architecture)
      - [3.1.2 Memory Management Patterns](#312-memory-management-patterns)
    - [3.2 Type-Safe API Contracts](#32-type-safe-api-contracts)
      - [3.2.1 Result Type System](#321-result-type-system)
      - [3.2.2 Async Contract Enforcement](#322-async-contract-enforcement)
  - [4. Memory Management & Ownership](#4-memory-management--ownership)
    - [4.1 Ownership Strategy](#41-ownership-strategy)
      - [4.1.1 Arc/Mutex for Shared State](#411-arcmutex-for-shared-state)
      - [4.1.2 Zero-Copy Data Transfer](#412-zero-copy-data-transfer)
    - [4.2 Memory Safety Patterns](#42-memory-safety-patterns)
      - [4.2.1 Borrow Checker Enforcement](#421-borrow-checker-enforcement)
      - [4.2.2 Memory Pool Management](#422-memory-pool-management)
  - [5. Performance Optimization](#5-performance-optimization)
    - [5.1 Parallel Processing](#51-parallel-processing)
      - [5.1.1 Rayon Integration](#511-rayon-integration)
      - [5.1.2 Async Task Pool](#512-async-task-pool)
    - [5.2 Caching Strategies](#52-caching-strategies)
      - [5.2.1 Multi-Level Cache](#521-multi-level-cache)
      - [5.2.2 Cache Invalidation](#522-cache-invalidation)
  - [6. Scalability Considerations](#6-scalability-considerations)
    - [6.1 Crate Organization](#61-crate-organization)
      - [6.1.1 Feature-Based Compilation](#611-feature-based-compilation)
      - [6.1.2 Dependency Optimization](#612-dependency-optimization)
    - [6.2 Build Performance](#62-build-performance)
      - [6.2.1 Parallel Compilation](#621-parallel-compilation)
      - [6.2.2 Incremental Compilation](#622-incremental-compilation)
  - [7. Testing Strategy](#7-testing-strategy)
    - [7.1 Multi-Layer Testing](#71-multi-layer-testing)
      - [7.1.1 Unit Testing](#711-unit-testing)
      - [7.1.2 Integration Testing](#712-integration-testing)
    - [7.2 Property-Based Testing](#72-property-based-testing)
  - [8. Documentation Architecture](#8-documentation-architecture)
    - [8.1 API Documentation](#81-api-documentation)
      - [8.1.1 Structured Documentation](#811-structured-documentation)
      - [8.1.2 Architecture Decision Records](#812-architecture-decision-records)
    - [8.2 Architecture Diagrams](#82-architecture-diagrams)
      - [8.2.1 C4 Model Diagrams](#821-c4-model-diagrams)
  - [9. Future Extensibility](#9-future-extensibility)
    - [9.1 Plugin Architecture](#91-plugin-architecture)
      - [9.1.1 Trait-Based Extension](#911-trait-based-extension)
      - [9.1.2 Dynamic Loading](#912-dynamic-loading)
    - [9.2 Version Migration](#92-version-migration)
      - [9.2.1 Semantic Versioning](#921-semantic-versioning)
  - [10. Production Deployment](#10-production-deployment)
    - [10.1 Containerization](#101-containerization)
      - [10.1.1 Multi-Stage Docker Build](#1011-multi-stage-docker-build)
      - [10.1.2 Kubernetes Deployment](#1012-kubernetes-deployment)
    - [10.2 Monitoring & Observability](#102-monitoring--observability)
      - [10.2.1 OpenTelemetry Integration](#1021-opentelemetry-integration)
  - [11. Quality Assurance](#11-quality-assurance)
    - [11.1 Poka-Yoke Design](#111-poka-yoke-design)
      - [11.1.1 Compile-Time Safety](#1111-compile-time-safety)
      - [11.1.2 Warning Denial](#1112-warning-denial)
  - [12. Conclusion](#12-conclusion)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# GGen v7 Architecture Design - Post Protocol Layer Removal

## Executive Summary

After protocol layer removal, GGen v7 implements a semantic-first, deterministic code generation pipeline with 30 specialized crates organized in a clear architectural hierarchy. The system uses RDF ontologies as the source of truth and precipitates code through a five-stage transformation pipeline μ₁-μ₅.

## 1. Current Crate Structure Analysis

### 1.1 Domain Organization

```
GGen v7 Workspace (30 crates)
├── Core System (8 crates)
│   ├── ggen-core - Main RDF/SPARQL engine with template rendering
│   ├── ggen-utils - Shared utilities (validation, security, paths)
│   ├── ggen-domain - Business logic and entity modeling
│   ├── ggen-cli-lib - Command-line interface
│   ├── ggen-config - Configuration management
│   ├── ggen-config-clap - CLI configuration binding
│   ├── ggen-cli-validation - Input validation framework
│   └── ggen-ontology-core - Ontology processing layer
├── AI Orchestration (2 crates)
│   ├── ggen-ai - LLM integration with multi-provider support
│   └── ggen-dspy - DSPy runtime for LLM program execution
├── Testing & Quality (3 crates)
│   ├── ggen-test-audit - Test quality audit tooling
│   ├── ggen-test-opt - Test optimization tooling
│   └── ggen-e2e - End-to-end testing with testcontainers
├── KNHK Systems (6 crates)
│   ├── knhk-etl - Extract-Transform-Load pipeline
│   ├── knhk-hot - C FFI hot-path optimization
│   ├── knhk-connectors - Connector registry
│   ├── knhk-lockchain - Merkle-linked receipt storage
│   ├── knhk-otel - OpenTelemetry integration
│   └── knhk-orchestrator - Integration bridge
├── RevOps (4 crates)
│   ├── ggen-api - REST API layer for monetization
│   ├── ggen-auth - Authentication (OAuth2, JWT, API keys)
│   ├── ggen-payments - Payment processing (Stripe)
│   └── ggen-saas - SaaS tier management
├── Marketplace (1 crate)
│   └── ggen-marketplace - Package marketplace
├── Folk Strategy (1 crate)
│   └── ggen-folk-strategy - Opportunity field dynamics
└── Specialized Integration (5 crates)
    ├── ggen-agent - Simplified agent coordination
    ├── ggen-craftplan - RDF-driven Elixir generation
    ├── ggen-tps-andon - TPS Andon system
    ├── ggen-dod - Domain-driven design patterns
    └── tai-* - TAI framework components
```

### 1.2 Dependency Graph Analysis

#### 1.2.1 Core Dependencies
```
ggen-cli-lib → ggen-domain → ggen-core → ggen-utils
ggen-ai → ggen-core → ggen-utils
ggen-agent → ggen-core → ggen-utils
ggen-marketplace → ggen-api → ggen-domain → ggen-core
```

#### 1.2.2 Circular Dependency Prevention
- **No direct cycles**: All dependencies flow downward
- **Layered architecture**: CLI → Domain → Core → Utils
- **Feature flags**: Optional dependencies for specialized features

#### 1.2.3 Duplicate Dependencies Mitigation
- **Workspace deduplication**: 160 duplicates reduced through unified dependencies
- **Version consolidation**: Critical packages (axum, tonic, derive_more) centralized
- **Dev/Prod separation**: Test-only dependencies allowed to duplicate

## 2. Separation of Concerns

### 2.1 Core System Layer

#### 2.1.1 ggen-core (Main Engine)
**Responsibilities:**
- RDF/SPARQL processing with Oxigraph
- Template rendering with Tera
- Five-stage pipeline μ₁-μ₅ implementation
- Deterministic code generation
- Cryptographic receipts with SHA-256

**Interface Contracts:**
```rust
pub trait CodeGenerator {
    fn generate(&self, spec: &RdfSpec) -> Result<GeneratedCode, GenerationError>;
    fn validate(&self, spec: &RdfSpec) -> Result<ValidationReport>;
    fn receipt(&self, code: &GeneratedCode) -> Receipt;
}
```

#### 2.1.2 ggen-utils (Foundation)
**Responsibilities:**
- Path validation and security
- Safe command execution
- Memory management utilities
- Logging and observability
- Common error types

**Key Safety Features:**
```rust
pub struct SafePath {
    inner: PathBuf,
    validated: bool,
}

impl SafePath {
    pub fn new(path: &Path) -> Result<Self, PathValidationError> {
        // Validate path safety and normalization
    }
}
```

### 2.2 Domain Layer

#### 2.2.1 ggen-domain (Business Logic)
**Responsibilities:**
- Entity modeling and relationships
- Business rule enforcement
- Domain event handling
- Specification validation

**Type-Safe Interfaces:**
```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Entity {
    pub id: EntityId,
    pub kind: EntityKind,
    pub properties: PropertyMap,
    pub relationships: RelationshipMap,
}

impl Entity {
    pub fn validate(&self) -> Result<(), DomainError> {
        // Business rule validation
    }
}
```

#### 2.2.2 ggen-ontology-core (Ontology Processing)
**Responsibilities:**
- RDF/TTL parsing and validation
- SHACL shape validation
- OWL inference
- SPARQL query optimization

## 3. Integration Patterns

### 3.1 ggen-agent Integration

#### 3.1.1 Simplified Protocol Architecture
After protocol layer removal, ggen-agent uses direct integration:

```rust
// Direct integration without protocol layer
impl Agent {
    async fn execute_generation(&self, spec: RdfSpec) -> Result<GeneratedCode> {
        let core = ggen_core::Core::new();
        core.generate(&spec).await
    }
}
```

#### 3.1.2 Memory Management Patterns
```rust
// Zero-copy data transfer
#[derive(Clone, Debug)]
pub struct GenerationRequest {
    pub spec: Arc<RdfSpec>,
    pub context: Arc<GenerationContext>,
}

// Ownership-based resource management
pub struct AgentResourcePool {
    generators: Vec<GenerationWorker>,
    memory_limit: usize,
}
```

### 3.2 Type-Safe API Contracts

#### 3.2.1 Result Type System
```rust
// Unified error handling
pub type GenerationResult<T> = Result<T, GenerationError>;

// Domain-specific errors
#[derive(Debug, thiserror::Error)]
pub enum GenerationError {
    #[error("RDF validation failed: {0}")]
    Validation(String),
    #[error("Template rendering failed: {0}")]
    Rendering(String),
    #[error("Resource limit exceeded: {0}")]
    ResourceLimit(String),
}
```

#### 3.2.2 Async Contract Enforcement
```rust
#[async_trait]
pub trait GenerationEngine {
    async fn generate(&self, request: GenerationRequest) -> GenerationResult<GeneratedCode>;
    async fn validate(&self, spec: &RdfSpec) -> GenerationResult<ValidationReport>;
}
```

## 4. Memory Management & Ownership

### 4.1 Ownership Strategy

#### 4.1.1 Arc/Mutex for Shared State
```rust
pub struct GenerationState {
    pub specs: Arc<RwLock<HashMap<SpecId, RdfSpec>>>,
    pub templates: Arc<RwLock<HashMap<TemplateId, TeraTemplate>>>,
    pub cache: Arc<RwLock<LruCache<CacheKey, CachedResult>>>,
}
```

#### 4.1.2 Zero-Copy Data Transfer
```rust
#[derive(Clone)]
pub struct GenerationContext {
    pub rdf_data: Arc<RdfDataset>,
    pub template_registry: Arc<TemplateRegistry>,
    pub optimization_flags: OptimizationFlags,
}
```

### 4.2 Memory Safety Patterns

#### 4.2.1 Borrow Checker Enforcement
```rust
impl GenerationEngine {
    // Compile-time ownership guarantees
    fn validate_spec(&self, spec: &RdfSpec) -> Result<()> {
        // No cloning required, borrow checker ensures safety
    }
}
```

#### 4.2.2 Memory Pool Management
```rust
pub struct MemoryPool {
    buffer_pool: BufferPool,
    string_cache: StringCache,
    object_pool: ObjectPool,
}

impl MemoryPool {
    pub fn allocate<T>(&self, value: T) -> Arc<T> {
        // Pool allocation with tracking
    }
}
```

## 5. Performance Optimization

### 5.1 Parallel Processing

#### 5.1.1 Rayon Integration
```rust
use rayon::prelude::*;

impl CodeGenerator {
    fn generate_parallel(&self, specs: &[RdfSpec]) -> Vec<GeneratedCode> {
        specs.par_iter().map(|spec| self.generate_single(spec)).collect()
    }
}
```

#### 5.1.2 Async Task Pool
```rust
pub struct TaskPool {
    workers: Vec<tokio::task::JoinHandle<()>>,
    sender: mpsc::UnboundedSender<GenerationTask>,
}

impl TaskPool {
    pub fn submit(&self, task: GenerationTask) -> Result<()> {
        self.sender.send(task)?;
        Ok(())
    }
}
```

### 5.2 Caching Strategies

#### 5.2.1 Multi-Level Cache
```rust
pub struct MultiLevelCache {
    memory: LruCache<CacheKey, CachedResult>,
    disk: DiskCache,
    remote: RemoteCache,
}

impl MultiLevelCache {
    pub fn get_or_insert(&mut self, key: CacheKey, f: impl FnOnce() -> Result<CachedResult>) -> Result<CachedResult> {
        // Cache hierarchy: memory → disk → remote → compute
    }
}
```

#### 5.2.2 Cache Invalidation
```rust
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct CacheKey {
    pub spec_hash: u64,
    pub template_hash: u64,
    pub context_hash: u64,
}

impl CacheKey {
    pub fn invalidate_on_spec_change(&self, old_spec: &RdfSpec, new_spec: &RdfSpec) -> bool {
        // Cryptographic verification of changes
        !old_spec.sha256_eq(new_spec)
    }
}
```

## 6. Scalability Considerations

### 6.1 Crate Organization

#### 6.1.1 Feature-Based Compilation
```rust
[features]
default = ["core"]
core = []
ai = ["ggen-ai"]
marketplace = ["ggen-marketplace"]
agent = ["ggen-agent"]
otel = ["opentelemetry"]
```

#### 6.1.2 Dependency Optimization
- **Tree shaking**: Only include necessary dependencies
- **Optional features**: Compile-time feature selection
- **Dynamic linking**: Large libraries linked dynamically

### 6.2 Build Performance

#### 6.2.1 Parallel Compilation
```toml
[profile.dev]
codegen-units = 256  # Maximum parallelism

[profile.release]
codegen-units = 1   # Single unit for optimization
```

#### 6.2.2 Incremental Compilation
```toml
[profile.dev]
incremental = true
split-debuginfo = "unpacked"
```

## 7. Testing Strategy

### 7.1 Multi-Layer Testing

#### 7.1.1 Unit Testing
```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generation_pipeline() {
        let spec = RdfSpec::test_fixture();
        let result = generate_code(&spec);
        assert!(result.is_ok());
    }
}
```

#### 7.1.2 Integration Testing
```rust
#[tokio::test]
async fn test_end_to_end_generation() {
    let cli = Cli::new();
    let result = cli.run(&["generate", "--spec", "test.ttl"]).await;
    assert!(result.success());
}
```

### 7.2 Property-Based Testing
```rust
proptest! {
    #[test]
    fn test_generation_idempotency(spec in any::<RdfSpec>()) {
        let result1 = generate_code(&spec);
        let result2 = generate_code(&spec);
        assert_eq!(result1, result2);
    }
}
```

## 8. Documentation Architecture

### 8.1 API Documentation

#### 8.1.1 Structured Documentation
```rust
/// Code generation engine
///
/// Implements the five-stage transformation pipeline:
/// - μ₁: Normalize RDF validation
/// - μ₂: Extract SPARQL queries
/// - μ₃: Emit template rendering
/// - μ₄: Canonicalize formatting
/// - μ₅: Receipt generation
#[derive(Debug, Clone)]
pub struct CodeGenerator {
    // Implementation
}
```

#### 8.1.2 Architecture Decision Records
```markdown
## ADR-001: Five-Stage Pipeline
### Status
Accepted

### Context
Need deterministic code generation with reproducible outputs

### Decision
Implement five-stage transformation pipeline μ₁-μ₅

### Consequences
- Pros: Deterministic outputs, audit trail, incremental updates
- Cons: Processing overhead, pipeline complexity
```

### 8.2 Architecture Diagrams

#### 8.2.1 C4 Model Diagrams
```mermaid
C4Context
title GGen v7 Architecture
Boundary(core_system, "Core System")
Boundary(domain_layer, "Domain Layer")
Boundary(ai_orchestration, "AI Orchestration")

Container_Boundary(core_system, "ggen-core") {
    Component(RDF_Engine, "RDF/SPARQL Engine")
    Component(Template_Engine, "Template Renderer")
    Component(Pipeline, "5-Stage Pipeline")
}

Container_Boundary(domain_layer, "ggen-domain") {
    Component(Entity_Manager, "Entity Management")
    Component(Business_Rules, "Business Rules")
}

Container_Boundary(ai_orchestration, "ggen-ai") {
    Component(LLM_Client, "Multi-Provider LLM")
    Component(DSPy_Runtime, "DSPy Programs")
}

Rel(RDF_Engine, Entity_Manager, "validates")
Rel(Entity_Manager, Business_Rules, "enforces")
Rel(Business_Rules, Template_Engine, "configures")
Rel(Template_Engine, LLM_Client, "enhances")
```

## 9. Future Extensibility

### 9.1 Plugin Architecture

#### 9.1.1 Trait-Based Extension
```rust
pub trait CodeGeneratorPlugin {
    fn name(&self) -> &str;
    fn version(&self) -> &str;
    fn generate(&self, spec: &RdfSpec) -> Result<GeneratedCode>;
}

// Plugin registry
pub struct PluginRegistry {
    plugins: HashMap<String, Box<dyn CodeGeneratorPlugin>>,
}
```

#### 9.1.2 Dynamic Loading
```rust
impl PluginRegistry {
    pub fn load_plugin(&mut self, path: &Path) -> Result<()> {
        let library = libloading::Library::new(path)?;
        let plugin: libloading::Symbol<fn() -> Box<dyn CodeGeneratorPlugin>> = unsafe { library.get(b"create_plugin") }?;
        let instance = plugin();
        self.plugins.insert(instance.name().to_string(), instance);
        Ok(())
    }
}
```

### 9.2 Version Migration

#### 9.2.1 Semantic Versioning
```rust
#[derive(Debug, Clone)]
pub struct Version {
    major: u32,
    minor: u32,
    patch: u32,
}

impl Version {
    pub fn compatible_with(&self, other: &Version) -> bool {
        self.major == other.major
    }
}
```

## 10. Production Deployment

### 10.1 Containerization

#### 10.1.1 Multi-Stage Docker Build
```dockerfile
FROM rust:1.91-alpine AS builder
WORKDIR /app
COPY . .
RUN cargo build --release

FROM alpine:latest
RUN apk add --no-cache ca-certificates
COPY --from=builder /app/target/release/ggen /usr/local/bin/
ENTRYPOINT ["ggen"]
```

#### 10.1.2 Kubernetes Deployment
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: ggen-core
spec:
  replicas: 3
  selector:
    matchLabels:
      app: ggen-core
  template:
    metadata:
      labels:
        app: ggen-core
    spec:
      containers:
      - name: ggen
        image: ggen:latest
        resources:
          limits:
            memory: "2Gi"
            cpu: "1000m"
```

### 10.2 Monitoring & Observability

#### 10.2.1 OpenTelemetry Integration
```rust
use opentelemetry::{global, trace::Tracer};
use opentelemetry::sdk::trace::TracerProvider;

pub struct Observability {
    tracer: Tracer,
}

impl Observability {
    pub fn new() -> Self {
        let provider = TracerProvider::builder().build();
        let tracer = provider.tracer("ggen-core");
        Self { tracer }
    }

    pub fn trace_generation(&self, spec: &RdfSpec) -> impl Trace {
        self.tracer.start("code_generation")
    }
}
```

## 11. Quality Assurance

### 11.1 Poka-Yoke Design

#### 11.1.1 Compile-Time Safety
```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidatedSpec {
    pub rdf: RdfSpec,
    pub validated_at: DateTime<Utc>,
}

impl ValidatedSpec {
    pub fn new(spec: RdfSpec) -> Result<Self, ValidationError> {
        // Compile-time validation
        spec.validate()?;
        Ok(Self { rdf: spec, validated_at: Utc::now() })
    }
}
```

#### 11.1.2 Warning Denial
```toml
[workspace.lints.clippy]
all = "deny"
unwrap_used = "deny"
expect_used = "deny"
```

## 12. Conclusion

GGen v7 architecture provides a solid foundation for semantic code generation with:

- **Clear separation of concerns** between 30 specialized crates
- **Type-safe interfaces** with Result-based error handling
- **Memory management** through ownership and Arc/Mutex patterns
- **Performance optimization** via parallel processing and multi-level caching
- **Scalability** through feature-based compilation and tree shaking
- **Extensibility** via plugin architecture and trait-based design
- **Production readiness** with containerization and observability

The five-stage pipeline μ₁-μ₅ ensures deterministic, reproducible code generation while maintaining flexibility for future enhancements.