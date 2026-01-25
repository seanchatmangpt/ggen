# v6.0.0 File Modification Manifest

**Generated**: 2026-01-24
**Purpose**: Detailed file-by-file changes required for production readiness

This document lists every file that requires modification, with specific line numbers and change descriptions.

---

## CRITICAL: SPARQL Injection Prevention

### File: `crates/ggen-core/src/rdf/query.rs`

**Lines to Change**: 193

**Current Code** (VULNERABLE):
```rust
193: let query = format!("SELECT ?s ?o WHERE {{ ?s <{}> ?o }}", predicate);
```

**Required Change**:
```rust
// Remove direct format! usage
// Replace with type-safe builder:
let query = SparqlQuery::select()
    .var("s")
    .var("o")
    .triple(Var("s"), IRI::from_validated(predicate)?, Var("o"))
    .build()?;
```

**Severity**: CRITICAL - Allows SPARQL injection attacks

---

### File: `crates/ggen-core/src/graph/update.rs`

**Lines to Change**: 115, 127, 139

**Line 115** (VULNERABLE):
```rust
115: let update = format!("INSERT DATA {{ {} }}", data);
```

**Required Change**:
```rust
let update = SparqlUpdate::insert()
    .data(RdfData::from_validated(data)?)
    .build()?;
```

**Line 127** (VULNERABLE):
```rust
127: let update = format!("DELETE DATA {{ {} }}", data);
```

**Required Change**:
```rust
let update = SparqlUpdate::delete()
    .data(RdfData::from_validated(data)?)
    .build()?;
```

**Line 139** (VULNERABLE):
```rust
139: let update = format!("DELETE WHERE {{ {} }}", pattern);
```

**Required Change**:
```rust
let update = SparqlUpdate::delete_where()
    .pattern(TriplePattern::from_validated(pattern)?)
    .build()?;
```

**Severity**: CRITICAL - Allows data manipulation attacks

---

### File: `crates/ggen-ontology-core/src/triple_store.rs`

**Lines to Change**: 136-162 (entire `query_sparql` method)

**Current Code**:
```rust
136: pub fn query_sparql(&self, query: &str) -> Result<String> {
137:     #[allow(deprecated)]
138:     let results = self.store.query(query).map_err(|e| {
```

**Required Change**:
```rust
pub fn execute_query(&self, query: &SparqlQuery) -> Result<String> {
    #[allow(deprecated)]
    let results = self.store.query(&query.to_string()).map_err(|e| {
        OntologyError::query(format!("Failed to execute SPARQL query: {}", e))
    })?;
    // ... rest of method
}

// Add validation wrapper
pub fn query_sparql(&self, query_str: &str) -> Result<String> {
    let query = SparqlQuery::parse(query_str)?;
    self.execute_query(&query)
}
```

**Severity**: CRITICAL - Entry point for user SPARQL queries

---

## CRITICAL: Path Traversal Prevention

### File: `crates/ggen-core/src/graph/core.rs`

**Method to Change**: `load_turtle` (currently accepts `&str`)

**Current Signature**:
```rust
pub fn load_turtle(&self, content: &str) -> Result<()>
```

**Note**: This file uses inline content, not paths. But `load_turtle_file` methods need SafePath.

**Search Pattern**: Any method accepting path parameters

---

### File: `crates/ggen-core/src/templates/generator.rs`

**Methods to Change**: All path-accepting methods

**Current Pattern**:
```rust
fn load_template(&self, path: &Path) -> Result<Template>
```

**Required Change**:
```rust
fn load_template(&self, path: &SafePath) -> Result<Template>
```

**Files to Search**: All with `fn.*(&.*Path)` pattern

---

### File: `crates/ggen-core/src/v6/pipeline.rs`

**Lines to Change**: 93, 99, 111, 117, 129

**Current Code**:
```rust
93:  pub fn with_base_path(mut self, path: impl Into<PathBuf>) -> Self {
99:  pub fn with_ontology(mut self, path: impl Into<PathBuf>) -> Self {
111: pub fn with_output_dir(mut self, path: impl Into<PathBuf>) -> Self {
117: pub fn with_receipt_path(mut self, path: impl Into<PathBuf>) -> Self {
129: pub fn with_previous_receipt(mut self, path: impl Into<PathBuf>) -> Self {
```

**Required Change**:
```rust
pub fn with_base_path(mut self, path: SafePath) -> Self {
    self.base_path = path.into_inner();
    self
}

// Or validate at construction:
pub fn with_base_path(mut self, path: impl AsRef<str>) -> Result<Self> {
    let safe_path = SafePath::from_user_input(path.as_ref(), &env::current_dir()?)?;
    self.base_path = safe_path.into_inner();
    Ok(self)
}
```

---

### File: `crates/ggen-cli/src/cmds/sync.rs`

**Lines to Change**: 242 (manifest path construction)

**Current Code**:
```rust
242: options.manifest_path = PathBuf::from(manifest.unwrap_or_else(|| "ggen.toml".to_string()));
```

**Required Change**:
```rust
let manifest_str = manifest.unwrap_or_else(|| "ggen.toml".to_string());
let base_dir = env::current_dir()?;
options.manifest_path = SafePath::from_user_input(&manifest_str, &base_dir)?;
```

---

## HIGH: Rate Limiting

### File: `crates/ggen-api/src/middleware/rate_limit.rs`

**Lines to Replace**: 69-78 (entire `rate_limit` function)

**Current Code** (TODOs):
```rust
69: pub async fn rate_limit(
70:     request: Request,
71:     next: Next,
72: ) -> Response {
73:     // TODO: Extract client IP from request
74:     // TODO: Apply rate limiter
75:     // TODO: Return 429 if limit exceeded
76:
77:     next.run(request).await
78: }
```

**Required Implementation**:
```rust
pub async fn rate_limit(
    State(limiter): State<Arc<ProductionRateLimiter>>,
    request: Request,
    next: Next,
) -> Result<Response, RateLimitError> {
    // Extract client IP
    let ip = extract_client_ip(&request)?;

    // Extract endpoint
    let endpoint = request.uri().path();

    // Check rate limit
    limiter.check(ip, endpoint).await?;

    // Continue if allowed
    Ok(next.run(request).await)
}

fn extract_client_ip(request: &Request) -> Result<IpAddr> {
    // Check X-Forwarded-For header
    if let Some(forwarded) = request.headers().get("X-Forwarded-For") {
        let forwarded_str = forwarded.to_str()?;
        let ip_str = forwarded_str.split(',').next().unwrap_or("").trim();
        return ip_str.parse().map_err(|_| RateLimitError::InvalidIp);
    }

    // Check X-Real-IP header
    if let Some(real_ip) = request.headers().get("X-Real-IP") {
        let ip_str = real_ip.to_str()?;
        return ip_str.parse().map_err(|_| RateLimitError::InvalidIp);
    }

    // Fallback to connection info (if available)
    Err(RateLimitError::NoIpFound)
}
```

---

## HIGH: Resource Limits

### File: `crates/ggen-core/src/graph/core.rs`

**New Field to Add**:
```rust
pub struct Graph {
    store: Store,
    cache: QueryCache,
    epoch: Epoch,
    // NEW:
    limits: GraphLimits,
    current_triples: Arc<AtomicUsize>,
}
```

**Constructor to Change**:
```rust
pub fn new() -> Result<Self> {
    Self::with_limits(GraphLimits::default())
}

pub fn with_limits(limits: GraphLimits) -> Result<Self> {
    Ok(Self {
        store: Store::new()?,
        cache: QueryCache::new(DEFAULT_CACHE_SIZE),
        epoch: Epoch::default(),
        limits,
        current_triples: Arc::new(AtomicUsize::new(0)),
    })
}
```

**Method to Add**:
```rust
fn check_triple_limit(&self, new_triples: usize) -> Result<()> {
    let current = self.current_triples.load(Ordering::Relaxed);
    let new_total = current + new_triples;

    if new_total > self.limits.max_triples {
        return Err(Error::new(&format!(
            "Triple limit exceeded: {} (max: {})",
            new_total,
            self.limits.max_triples
        )));
    }

    self.current_triples.store(new_total, Ordering::Relaxed);
    Ok(())
}
```

---

### File: `crates/ggen-ontology-core/src/triple_store.rs`

**Method to Change**: `load_turtle` (add file size check)

**Line 102**: Add size check before loading

**Current Code**:
```rust
102: pub fn load_turtle<P: AsRef<Path>>(&self, path: P) -> Result<()> {
103:     let path = path.as_ref();
104:     let path_str = path.to_string_lossy().to_string();
105:
106:     let file = std::fs::File::open(path).map_err(|e| {
```

**Required Change**:
```rust
pub fn load_turtle<P: AsRef<Path>>(&self, path: P) -> Result<()> {
    let path = path.as_ref();
    let path_str = path.to_string_lossy().to_string();

    // NEW: Check file size before loading
    let metadata = std::fs::metadata(path).map_err(|e| {
        OntologyError::io(format!("Cannot stat file {}: {}", path_str, e))
    })?;

    if metadata.len() > MAX_RDF_FILE_SIZE {
        return Err(OntologyError::io(format!(
            "RDF file too large: {} bytes (max: {})",
            metadata.len(),
            MAX_RDF_FILE_SIZE
        )));
    }

    let file = std::fs::File::open(path).map_err(|e| {
        // ... rest unchanged
    })?;
```

---

## HIGH: Timeout Enforcement

### File: `crates/ggen-core/src/v6/pipeline.rs`

**Method to Add**: Async `run` with timeout

**After Line 200** (after constructors):

```rust
/// Execute the pipeline with timeout enforcement
pub async fn run(&mut self) -> Result<BuildReceipt> {
    let total_timeout = Duration::from_millis(self.config.total_timeout_ms);
    let pass_timeout = Duration::from_millis(self.config.pass_timeout_ms);

    // Wrap entire pipeline in timeout
    tokio::time::timeout(total_timeout, async {
        // μ₁: Normalization
        self.execute_pass_with_timeout(&self.normalization, pass_timeout).await?;

        // μ₂: Extraction
        self.execute_pass_with_timeout(&self.extraction, pass_timeout).await?;

        // μ₃: Emission
        self.execute_pass_with_timeout(&self.emission, pass_timeout).await?;

        // μ₄: Canonicalization
        self.execute_pass_with_timeout(&self.canonicalization, pass_timeout).await?;

        // μ₅: Receipt
        Ok(self.generate_receipt()?)
    })
    .await
    .map_err(|_| Error::new(&format!(
        "Pipeline timeout exceeded: {:?}",
        total_timeout
    )))??
}

async fn execute_pass_with_timeout<P: Pass>(
    &mut self,
    pass: &P,
    timeout: Duration
) -> Result<PassResult> {
    tokio::time::timeout(timeout, async {
        let ctx = self.create_context();
        pass.execute(&ctx)
    })
    .await
    .map_err(|_| Error::new(&format!(
        "Pass timeout exceeded: {:?}",
        timeout
    )))?
}
```

**PipelineConfig to Change**: Add timeout fields

```rust
pub struct PipelineConfig {
    // ... existing fields ...

    /// NEW: Per-pass timeout in milliseconds
    pub pass_timeout_ms: u64,

    /// NEW: Total pipeline timeout in milliseconds
    pub total_timeout_ms: u64,
}

impl PipelineConfig {
    pub fn new(project_name: impl Into<String>, project_version: impl Into<String>) -> Self {
        Self {
            // ... existing fields ...
            pass_timeout_ms: 10_000,   // 10 seconds per pass
            total_timeout_ms: 60_000,  // 1 minute total
        }
    }
}
```

---

## MEDIUM: Observability

### Files to Instrument: All v6 passes

**Pattern to Apply**:

```rust
use tracing::{info, instrument};

#[instrument(skip(self, ctx), fields(
    pass = "normalization",
    epoch = %ctx.epoch.id,
))]
pub fn execute(&self, ctx: &PassContext<'_>) -> Result<PassResult> {
    let start = Instant::now();

    info!("Starting normalization pass");

    // ... pass logic ...

    let duration = start.elapsed();
    info!(
        rules_executed = result.rules_executed,
        duration_ms = duration.as_millis(),
        "Normalization complete"
    );

    Ok(result)
}
```

**Files to Change**:
- [ ] `crates/ggen-core/src/v6/passes/normalization.rs`
- [ ] `crates/ggen-core/src/v6/passes/extraction.rs`
- [ ] `crates/ggen-core/src/v6/passes/emission.rs`
- [ ] `crates/ggen-core/src/v6/passes/canonicalization.rs`
- [ ] `crates/ggen-core/src/v6/passes/receipt_gen.rs`

---

## Summary Statistics

| Category | Files to Modify | Lines to Change | New Files | Tests Required |
|----------|----------------|-----------------|-----------|----------------|
| SPARQL Injection | 4 | ~50 | 2 | 10 |
| Path Traversal | 15+ | ~100 | 1 | 8 |
| Rate Limiting | 1 | ~80 | 0 | 5 |
| Resource Limits | 3 | ~60 | 2 | 6 |
| Timeout Enforcement | 7 | ~120 | 0 | 5 |
| Observability | 5 | ~40 | 1 | 3 |
| Error Messages | 50+ | ~200 | 1 | 4 |
| Circuit Breakers | 0 | 0 | 3 | 4 |
| **TOTAL** | **85+** | **~650** | **10** | **45** |

---

## Quick Find Commands

### Find all SPARQL format! usage:
```bash
rg 'format!\s*\(\s*"(SELECT|INSERT|DELETE|CONSTRUCT|ASK)' crates/
```

### Find all Path operations:
```bash
rg 'PathBuf::from|Path::new|\.join\(' crates/ggen-core/src/ --type rust
```

### Find all Vec::new without capacity:
```bash
rg 'Vec::new\(\)' crates/ggen-core/src/ --type rust
```

### Find all panic/unwrap in production:
```bash
rg 'panic!|unwrap\(\)|expect\(' crates/ggen-core/src/ --type rust | grep -v tests
```

---

## Priority Order for Implementation

1. **Week 1**: SPARQL Injection (4 files)
2. **Week 2**: SafePath (15 files) + Resource Limits (3 files)
3. **Week 3**: Rate Limiting (1 file) + Timeout (7 files)
4. **Week 4**: Observability (5 files) + Testing
5. **Week 5**: Error Messages (50 files) + Documentation

---

**Next Action**: Begin with `crates/ggen-core/src/rdf/query.rs` line 193 - highest severity SPARQL injection point.
