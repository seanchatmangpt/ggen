# GGEN Architecture Patterns & Design Analysis

**Analysis Date**: 2025-10-17
**Focus**: Reusable architectural patterns and design decisions

---

## 1. Core Architecture Pattern: Layered Pipeline with Plugin System

### Pattern Overview

GGEN implements a **multi-layer pipeline architecture** with clear separation of concerns:

```
┌─────────────────────────────────────────────────────────────┐
│                    CLI Layer (ggen-cli)                     │
│  • Command parsing (Clap)                                   │
│  • User interaction (progressive help, doctor)              │
│  • Error formatting (enhanced messages)                     │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│              Orchestration Layer (ggen-core)                │
│  • Pipeline coordination                                    │
│  • Lifecycle management (DAG execution)                     │
│  • Registry client (package resolution)                     │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│              Processing Layer (ggen-core)                   │
│  • Template parsing (YAML frontmatter)                      │
│  • Template rendering (Tera engine)                         │
│  • Graph processing (RDF + SPARQL)                          │
│  • File injection (idempotent updates)                      │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                 Extension Layer (ggen-ai)                   │
│  • AI-powered generation                                    │
│  • Multi-provider LLM client                                │
│  • Response caching                                         │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│               Infrastructure Layer (utils)                  │
│  • Configuration management                                 │
│  • Logging and tracing                                      │
│  • Error types                                              │
└─────────────────────────────────────────────────────────────┘
```

### Key Design Decisions

**Decision 1**: Two-Phase Rendering
- **Rationale**: Frontmatter must be rendered first to resolve `to` field before body rendering
- **Benefit**: Variables can be used in output paths and configuration
- **Implementation**: `template.render_frontmatter()` → `template.render()`

**Decision 2**: Graph-First Processing
- **Rationale**: RDF triples must be loaded before SPARQL queries can execute
- **Benefit**: Templates can query semantic knowledge during rendering
- **Implementation**: `template.process_graph()` → SPARQL results injected into context

**Decision 3**: Builder Pattern for Pipeline Configuration
- **Rationale**: Complex pipeline setup requires flexible configuration
- **Benefit**: Fluent API for readable pipeline construction
- **Implementation**: `PipelineBuilder::new().with_prefixes().with_rdf().build()`

---

## 2. Template System Architecture

### Frontmatter-Driven Template Processing

**File Structure**:
```yaml
---
# Frontmatter (YAML) - Configuration
to: "src/{{name}}.rs"
vars:
  name: "example"
rdf:
  - "ontology.ttl"
sparql:
  get_types: "SELECT ?type WHERE { ?s a ?type }"
---
// Body (Tera) - Generation
pub struct {{name | capitalize}} {
    // Generated from: {{ sparql_first(results=sparql_results.get_types, column="type") }}
}
```

**Processing Flow**:

```rust
// 1. Parse frontmatter + body
let template = Template::parse(input)?;

// 2. Render frontmatter (resolve {{vars}})
template.render_frontmatter(&mut tera, &context)?;

// 3. Load RDF and execute SPARQL
template.process_graph(&mut graph, &mut tera, &context, path)?;

// 4. Render body with SPARQL results
let rendered = template.render(&mut tera, &context)?;

// 5. Write to computed output path
fs::write(&template.front.to, rendered)?;
```

### Extensibility Pattern: Custom Tera Functions

**Registration Pattern**:
```rust
// Core pattern for extending Tera
pub fn register_all(tera: &mut Tera) {
    // String manipulation
    tera.register_function("camel_case", make_simple_fn!(camel_case));
    tera.register_function("snake_case", make_simple_fn!(snake_case));

    // SPARQL integration
    tera.register_function("sparql", SparqlFn { graph, prolog });
    tera.register_function("sparql_first", sparql_first_fn);
    tera.register_function("sparql_values", sparql_values_fn);

    // File operations
    tera.register_function("read_file", read_file_fn);
    tera.register_function("file_exists", file_exists_fn);
}
```

**Pattern Benefits**:
- Template authors can use natural function syntax: `{{ camel_case(name="foo_bar") }}`
- Easy to add domain-specific functions
- Functions can access pipeline state (graph, context)

---

## 3. Lifecycle Management Pattern

### DAG-Based Phase Execution

**Lifecycle Model**:
```rust
pub struct Lifecycle {
    phases: Vec<Phase>,           // Ordered execution phases
    state: LifecycleState,         // Current state tracker
    cache: Cache,                  // Phase result caching
    hooks: Vec<Box<dyn Hook>>,    // Pre/post hooks
}

pub struct Phase {
    name: String,
    dependencies: Vec<String>,     // Phase DAG
    commands: Vec<Command>,        // Shell commands to execute
    validators: Vec<Validator>,    // Validation checks
}
```

**Execution Pattern**:
```rust
impl Lifecycle {
    pub async fn run(&mut self, phase: &str) -> Result<()> {
        // 1. Build execution DAG
        let dag = self.build_dag(phase)?;

        // 2. Topological sort for correct order
        let order = dag.topological_sort()?;

        // 3. Execute phases in order
        for phase_name in order {
            // Pre-hooks
            self.run_hooks(HookType::Pre, &phase_name)?;

            // Execute phase
            let result = self.execute_phase(&phase_name).await?;

            // Cache result
            self.cache.store(&phase_name, &result)?;

            // Post-hooks
            self.run_hooks(HookType::Post, &phase_name)?;
        }

        Ok(())
    }
}
```

**Pattern Benefits**:
- Automatic dependency resolution
- Parallel execution of independent phases
- Extensible via hooks
- Cacheable results

### Hook System Pattern

**Hook Trait**:
```rust
pub trait Hook: Send + Sync {
    fn name(&self) -> &str;
    fn run(&self, context: &HookContext) -> Result<()>;
}

pub enum HookType {
    Pre,   // Before phase execution
    Post,  // After phase execution
}
```

**Usage Example**:
```rust
// Custom validation hook
struct ValidationHook;

impl Hook for ValidationHook {
    fn name(&self) -> &str { "validation" }

    fn run(&self, ctx: &HookContext) -> Result<()> {
        // Run custom validation logic
        validate_dependencies()?;
        validate_configuration()?;
        Ok(())
    }
}

// Register hook
lifecycle.register_hook(HookType::Pre, Box::new(ValidationHook));
```

---

## 4. Registry & Marketplace Pattern

### Package Resolution Strategy

**Three-Tier Resolution**:

1. **Local Cache** - Fastest, checked first
2. **Remote Registry** - HTTP API with caching
3. **P2P Network** - Decentralized fallback

**Resolution Algorithm**:
```rust
pub async fn resolve(&self, spec: &PackageSpec) -> Result<ResolvedPack> {
    // 1. Check local cache
    if let Some(cached) = self.cache.get(&spec.name)? {
        if cached.version.matches(&spec.version_req) {
            return Ok(cached);
        }
    }

    // 2. Query remote registry
    if let Some(remote) = self.registry_client.get(&spec.name).await? {
        let resolved = self.select_version(&remote.versions, &spec.version_req)?;
        self.cache.store(&spec.name, &resolved)?;
        return Ok(resolved);
    }

    // 3. Try P2P network
    if self.p2p_enabled {
        return self.p2p_client.resolve(spec).await;
    }

    Err(PackageNotFound(spec.name.clone()))
}
```

**Version Selection**:
```rust
fn select_version(&self, versions: &[Version], req: &VersionReq) -> Result<Version> {
    versions
        .iter()
        .filter(|v| req.matches(v))
        .max()  // Latest compatible version
        .ok_or(NoMatchingVersion)
}
```

### Package Integrity Pattern

**Post-Quantum Signatures**:
```rust
pub struct PqcSigner {
    private_key: MlDsaPrivateKey,  // ML-DSA (Dilithium3)
}

impl PqcSigner {
    pub fn sign(&self, content: &[u8]) -> Result<Signature> {
        let hash = calculate_sha256(content);
        let signature = self.private_key.sign(&hash)?;
        Ok(signature)
    }
}

pub struct PqcVerifier {
    public_key: MlDsaPublicKey,
}

impl PqcVerifier {
    pub fn verify(&self, content: &[u8], signature: &Signature) -> Result<bool> {
        let hash = calculate_sha256(content);
        Ok(self.public_key.verify(&hash, signature)?)
    }
}
```

**Usage in Package Distribution**:
```rust
// Package author signs package
let signer = PqcSigner::new(author_private_key);
let signature = signer.sign(&package_tarball)?;

// Registry stores package + signature
registry.publish(package, signature)?;

// Users verify package integrity
let verifier = PqcVerifier::new(author_public_key);
let is_valid = verifier.verify(&downloaded_package, &signature)?;
```

---

## 5. Error Handling Pattern

### Production-Grade Error Design

**Error Type Hierarchy**:
```rust
// Domain-specific error types with thiserror
#[derive(Debug, thiserror::Error)]
pub enum TemplateError {
    #[error("Failed to parse frontmatter: {0}")]
    FrontmatterParse(String),

    #[error("Template rendering failed: {0}")]
    RenderFailed(#[from] tera::Error),

    #[error("Output path is invalid: {path}")]
    InvalidOutputPath { path: PathBuf },
}

#[derive(Debug, thiserror::Error)]
pub enum LifecycleError {
    #[error("Phase '{phase}' failed: {reason}")]
    PhaseFailed { phase: String, reason: String },

    #[error("Circular dependency detected: {cycle:?}")]
    CircularDependency { cycle: Vec<String> },
}

#[derive(Debug, thiserror::Error)]
pub enum RegistryError {
    #[error("Package '{name}' not found")]
    PackageNotFound { name: String },

    #[error("Version '{version}' does not match requirement '{req}'")]
    VersionMismatch { version: String, req: String },
}
```

**Error Propagation Pattern**:
```rust
// NEVER use .unwrap() or .expect() in production
// ❌ BAD
let result = risky_operation().expect("This will crash");

// ✅ GOOD - Propagate with ?
fn process_template(path: &Path) -> Result<String, TemplateError> {
    let content = fs::read_to_string(path)
        .map_err(|e| TemplateError::InvalidOutputPath {
            path: path.to_path_buf()
        })?;

    let template = Template::parse(&content)?;  // Auto-convert via From
    let rendered = template.render()?;

    Ok(rendered)
}
```

**Error Context Enhancement**:
```rust
use anyhow::{Context, Result};

fn complex_operation(input: &str) -> Result<Output> {
    let parsed = parse_input(input)
        .context("Failed to parse input data")?;

    let processed = process_data(&parsed)
        .with_context(|| format!("Processing failed for input: {}", input))?;

    let validated = validate_output(&processed)
        .context("Output validation failed")?;

    Ok(validated)
}
```

---

## 6. Testing Patterns

### Cleanroom Hermetic Testing

**Pattern**: Isolated test environments with testcontainers

**Implementation**:
```rust
#[tokio::test]
async fn test_database_integration() -> Result<()> {
    // Arrange - Create isolated environment
    let cleanroom = CleanroomEnv::new()
        .with_postgres("15-alpine")
        .with_seed(42)  // Deterministic
        .build()
        .await?;

    // Act - Run test logic
    let db = cleanroom.postgres().client();
    db.execute("CREATE TABLE users (id INT, name TEXT)", &[]).await?;
    db.execute("INSERT INTO users VALUES (1, 'Alice')", &[]).await?;

    let count: i64 = db.query_one("SELECT COUNT(*) FROM users", &[])
        .await?
        .get(0);

    // Assert
    assert_eq!(count, 1);

    // Cleanup automatic on drop
    Ok(())
}
```

**Key Features**:
- Each test gets fresh containers
- Deterministic execution with fixed seeds
- Automatic cleanup
- Resource limits enforced

### Property-Based Testing Pattern

**Pattern**: Verify invariants hold for all inputs

**Implementation**:
```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_template_rendering_is_idempotent(
        vars in prop::collection::hash_map(
            "\\w+",  // Variable names
            ".*",    // Variable values
            0..10    // Map size
        )
    ) {
        // Render template twice with same vars
        let render1 = render_template(&TEMPLATE, &vars)?;
        let render2 = render_template(&TEMPLATE, &vars)?;

        // Output must be identical (idempotent)
        prop_assert_eq!(render1, render2);
    }

    #[test]
    fn test_version_resolution_respects_semver(
        versions in prop::collection::vec(
            semver_strategy(),
            1..20
        ),
        req in version_req_strategy()
    ) {
        let selected = select_version(&versions, &req)?;

        // Selected version must match requirement
        prop_assert!(req.matches(&selected));

        // Selected version must be the latest matching
        for v in &versions {
            if req.matches(v) {
                prop_assert!(selected >= *v);
            }
        }
    }
}
```

---

## 7. Configuration Management Pattern

### Multi-Source Configuration

**Configuration Sources** (in priority order):
1. CLI arguments (highest priority)
2. Environment variables
3. Project config file (`.ggen.toml`)
4. User config file (`~/.config/ggen/config.toml`)
5. Defaults (lowest priority)

**Implementation**:
```rust
pub struct Config {
    // Loaded from multiple sources
    pub template_dir: PathBuf,
    pub output_dir: PathBuf,
    pub registry_url: String,
    pub ai_provider: AiProvider,
}

impl Config {
    pub fn load() -> Result<Self> {
        let mut config = Self::defaults();

        // Layer 1: User config
        if let Some(user_config) = Self::load_user_config()? {
            config.merge(user_config);
        }

        // Layer 2: Project config
        if let Some(project_config) = Self::load_project_config()? {
            config.merge(project_config);
        }

        // Layer 3: Environment variables
        config.apply_env_vars();

        // Layer 4: CLI args (applied by caller)
        Ok(config)
    }
}
```

**Environment Variable Pattern**:
```rust
// Structured env var loading
fn apply_env_vars(&mut self) {
    if let Ok(dir) = env::var("GGEN_TEMPLATE_DIR") {
        self.template_dir = PathBuf::from(dir);
    }

    if let Ok(url) = env::var("GGEN_REGISTRY_URL") {
        self.registry_url = url;
    }

    // AI provider configuration
    if let Ok(provider) = env::var("GGEN_AI_PROVIDER") {
        self.ai_provider = provider.parse().unwrap_or_default();
    }
}
```

---

## 8. Observability Pattern

### Structured Tracing

**Tracing Hierarchy**:
```rust
use tracing::{info, debug, warn, error, instrument};

#[instrument(skip(self), fields(template = %path.display()))]
pub fn render_template(&self, path: &Path) -> Result<String> {
    info!("Starting template rendering");

    let content = fs::read_to_string(path)?;
    debug!(size = content.len(), "Template loaded");

    let parsed = Template::parse(&content)?;
    debug!(vars = parsed.front.vars.len(), "Frontmatter parsed");

    let rendered = parsed.render()?;
    info!(output_size = rendered.len(), "Template rendered successfully");

    Ok(rendered)
}
```

**OpenTelemetry Integration**:
```rust
// Initialize OTEL pipeline
pub fn init_telemetry(config: &TelemetryConfig) -> Result<()> {
    // Tracer provider
    let tracer = opentelemetry_otlp::new_pipeline()
        .tracing()
        .with_exporter(
            opentelemetry_otlp::new_exporter()
                .http()
                .with_endpoint(&config.otlp_endpoint)
        )
        .install_batch(opentelemetry_sdk::runtime::Tokio)?;

    // Subscriber with multiple layers
    tracing_subscriber::registry()
        .with(tracing_opentelemetry::layer().with_tracer(tracer))
        .with(tracing_subscriber::fmt::layer())
        .init();

    Ok(())
}
```

---

## 9. AI Integration Pattern

### Multi-Provider LLM Client

**Provider Abstraction**:
```rust
#[async_trait]
pub trait LlmProvider {
    async fn generate(&self, prompt: &str) -> Result<String>;
    async fn stream(&self, prompt: &str) -> Result<Stream<String>>;
}

pub enum AiProvider {
    OpenAI { api_key: String, model: String },
    Anthropic { api_key: String, model: String },
    Ollama { base_url: String, model: String },
}

impl AiProvider {
    pub fn client(&self) -> Box<dyn LlmProvider> {
        match self {
            Self::OpenAI { api_key, model } => {
                Box::new(OpenAiClient::new(api_key, model))
            }
            Self::Anthropic { api_key, model } => {
                Box::new(AnthropicClient::new(api_key, model))
            }
            Self::Ollama { base_url, model } => {
                Box::new(OllamaClient::new(base_url, model))
            }
        }
    }
}
```

**Response Caching Pattern**:
```rust
pub struct CachedLlmClient {
    provider: Box<dyn LlmProvider>,
    cache: Arc<Cache<String, String>>,  // LRU cache
}

impl CachedLlmClient {
    async fn generate(&self, prompt: &str) -> Result<String> {
        // Compute cache key
        let key = self.cache_key(prompt);

        // Check cache
        if let Some(cached) = self.cache.get(&key).await {
            return Ok(cached);
        }

        // Generate and cache
        let response = self.provider.generate(prompt).await?;
        self.cache.insert(key, response.clone()).await;

        Ok(response)
    }

    fn cache_key(&self, prompt: &str) -> String {
        use sha2::{Sha256, Digest};
        let mut hasher = Sha256::new();
        hasher.update(prompt.as_bytes());
        format!("{:x}", hasher.finalize())
    }
}
```

---

## 10. Build & Release Pattern

### Cargo Workspace Optimization

**Workspace Configuration**:
```toml
[workspace]
members = [
    "cli",
    "ggen-core",
    "ggen-ai",
    "ggen-marketplace",
    "utils",
]
resolver = "2"  # New resolver for better dependency resolution

[workspace.dependencies]
# Shared versions across workspace
tokio = { version = "1.47", features = ["full"] }
serde = { version = "1.0", features = ["derive"] }
anyhow = "1.0"
```

**Build Profile Optimization**:
```toml
[profile.dev]
opt-level = 0
codegen-units = 256  # Faster parallel compilation
incremental = true
split-debuginfo = "unpacked"  # Faster on macOS

[profile.release]
opt-level = 3
lto = "thin"         # Balance speed and optimization
codegen-units = 16   # Fewer units = more optimization
strip = true         # Smaller binaries
```

**Incremental Build Strategy**:
- Development: 2-3 seconds (256 codegen units)
- Release: Optimized binary (thin LTO)
- CI: Cached dependencies (restored from hash)

---

## Summary: Key Patterns for Reuse

**For CLNRM Integration**:

1. **Cleanroom Testing Pattern** ⭐⭐⭐⭐⭐
   - Hermetic test environments
   - Deterministic execution
   - Resource limits
   - **Direct adaptation recommended**

2. **Template System Pattern** ⭐⭐⭐⭐
   - YAML frontmatter + body
   - Two-phase rendering
   - Plugin functions
   - **Useful for test case generation**

3. **Error Handling Pattern** ⭐⭐⭐⭐⭐
   - No `.expect()` in production
   - Domain-specific error types
   - Context propagation
   - **Production-grade standard**

4. **Lifecycle DAG Pattern** ⭐⭐⭐⭐
   - Dependency resolution
   - Hook system
   - Cacheable results
   - **Adaptable for test phases**

5. **Multi-Provider Pattern** ⭐⭐⭐
   - Trait-based abstraction
   - Provider switching
   - Response caching
   - **Optional for AI features**

---

**Pattern Extraction Success**: All core patterns documented and ready for adaptation.
