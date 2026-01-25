<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [v6.0.0 Production Readiness Analysis & Breaking Changes](#v600-production-readiness-analysis--breaking-changes)
  - [Executive Summary](#executive-summary)
  - [🔴 CRITICAL: Security Vulnerabilities](#-critical-security-vulnerabilities)
    - [1. SPARQL Injection Vulnerabilities (SEVERITY: CRITICAL)](#1-sparql-injection-vulnerabilities-severity-critical)
    - [2. Path Traversal Protection Not Universally Enforced (SEVERITY: HIGH)](#2-path-traversal-protection-not-universally-enforced-severity-high)
    - [3. Rate Limiting Not Implemented (SEVERITY: HIGH)](#3-rate-limiting-not-implemented-severity-high)
  - [🟡 HIGH: Resource Exhaustion & DoS Protection](#-high-resource-exhaustion--dos-protection)
    - [4. Unbounded Resource Usage (SEVERITY: HIGH)](#4-unbounded-resource-usage-severity-high)
    - [5. Missing Timeout Enforcement (SEVERITY: MEDIUM)](#5-missing-timeout-enforcement-severity-medium)
  - [🟡 MEDIUM: Observability Gaps](#-medium-observability-gaps)
    - [6. Missing Telemetry in Critical Paths (SEVERITY: MEDIUM)](#6-missing-telemetry-in-critical-paths-severity-medium)
    - [7. Poor Error Messages (SEVERITY: MEDIUM)](#7-poor-error-messages-severity-medium)
    - [8. No Graceful Degradation (SEVERITY: LOW)](#8-no-graceful-degradation-severity-low)
  - [Breaking Changes Summary](#breaking-changes-summary)
  - [Migration Guide for v6.0.0](#migration-guide-for-v600)
    - [Step 1: Update SPARQL Usage](#step-1-update-sparql-usage)
    - [Step 2: Update Path Handling](#step-2-update-path-handling)
    - [Step 3: Add Configuration](#step-3-add-configuration)
  - [Recommended Actions](#recommended-actions)
    - [Immediate (Pre-v6.0.0 Release)](#immediate-pre-v600-release)
    - [Next Release (v6.1.0)](#next-release-v610)
    - [Future (v6.2.0)](#future-v620)
  - [Production Deployment Checklist](#production-deployment-checklist)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# v6.0.0 Production Readiness Analysis & Breaking Changes

**Date**: 2026-01-24
**Status**: CRITICAL - Multiple Production Blockers Identified
**Risk Level**: HIGH

## Executive Summary

This report identifies **8 critical production blockers** that must be addressed before v6 can be deployed to production environments. All findings require **breaking API changes** to enforce security, reliability, and observability at the type level.

**Recommendation**: Implement all breaking changes in v6.0.0 to establish production-grade foundations before public release.

---

## 🔴 CRITICAL: Security Vulnerabilities

### 1. SPARQL Injection Vulnerabilities (SEVERITY: CRITICAL)

**Location**: Multiple files use unsafe string concatenation for SPARQL queries

**Evidence**:
```rust
// ❌ VULNERABLE: crates/ggen-core/src/rdf/query.rs:193
let query = format!("SELECT ?s ?o WHERE {{ ?s <{}> ?o }}", predicate);

// ❌ VULNERABLE: crates/ggen-core/src/graph/update.rs:115
let update = format!("INSERT DATA {{ {} }}", data);

// ❌ VULNERABLE: crates/ggen-core/src/graph/update.rs:127
let update = format!("DELETE DATA {{ {} }}", data);

// ❌ VULNERABLE: crates/ggen-core/src/graph/update.rs:139
let update = format!("DELETE WHERE {{ {} }}", pattern);
```

**Attack Vector**:
```rust
// Attacker-controlled predicate
let predicate = "http://example.org/name> ?o } . ?evil a <http://attacker.com/backdoor";
// Results in: SELECT ?s ?o WHERE { ?s <http://example.org/name> ?o } . ?evil a <http://attacker.com/backdoor> ?o }
```

**Impact**:
- Arbitrary SPARQL query execution
- Data exfiltration from RDF store
- Graph manipulation (INSERT/DELETE)
- Bypass of authorization checks

**BREAKING CHANGE REQUIRED**:

```rust
// Create type-safe SPARQL query builder
pub struct SparqlQuery<'a> {
    store: &'a Store,
    query_type: QueryType,
    triples: Vec<TriplePattern>,
    filters: Vec<Filter>,
}

impl<'a> SparqlQuery<'a> {
    pub fn select(store: &'a Store) -> SelectBuilder<'a> {
        SelectBuilder::new(store)
    }

    pub fn construct(store: &'a Store) -> ConstructBuilder<'a> {
        ConstructBuilder::new(store)
    }
}

// Usage enforces parameterization
let results = SparqlQuery::select(&store)
    .var("s")
    .var("o")
    .triple(Var("s"), IRI::from_validated(predicate)?, Var("o"))
    .execute()?;
```

**Migration Path**:
1. Introduce `SparqlQueryBuilder` trait with type-safe APIs
2. Deprecate all `format!()` usage in query construction
3. Mark v6.0.0 as requiring migration (provide `ggen migrate sparql-safety` command)
4. Remove unsafe APIs in v6.1.0

**Files Requiring Changes**:
- `crates/ggen-core/src/rdf/query.rs` (line 193)
- `crates/ggen-core/src/graph/update.rs` (lines 115, 127, 139)
- `crates/ggen-ontology-core/src/triple_store.rs` (query method)
- All test files using direct SPARQL strings (convert to builders)

---

### 2. Path Traversal Protection Not Universally Enforced (SEVERITY: HIGH)

**Issue**: Path validation exists but isn't enforced at the type level

**Evidence**:
- Good validation in `crates/ggen-core/src/security/validation.rs`
- Good validation in `crates/ggen-core/src/protection/path.rs`
- BUT: 861 files use `PathBuf::from()`, `Path::new()`, `join()` directly without validation

**Current State** (Unsafe):
```rust
// ❌ Nothing prevents this
let user_input = "../../../../etc/passwd";
let path = PathBuf::from(user_input); // No validation!
std::fs::read_to_string(path)?; // Reads /etc/passwd
```

**BREAKING CHANGE REQUIRED**:

```rust
// Create newtype that enforces validation
#[derive(Debug, Clone)]
pub struct SafePath(PathBuf);

impl SafePath {
    /// Only way to create a SafePath - validation required
    pub fn from_user_input(input: &str, base: &Path) -> Result<Self> {
        PathValidator::validate_within(Path::new(input), base)
            .map(SafePath)
    }

    /// For trusted system paths only
    pub fn from_trusted(path: PathBuf) -> Self {
        SafePath(path)
    }

    pub fn as_path(&self) -> &Path {
        &self.0
    }
}

// Update ALL file I/O APIs to require SafePath
impl Graph {
    pub fn load_turtle(&self, path: &SafePath) -> Result<()> { ... }
}

impl TemplateEngine {
    pub fn load_template(&self, path: &SafePath) -> Result<()> { ... }
}
```

**Migration Impact**:
- **All** public APIs accepting `&Path` must change to `&SafePath`
- **Breaking**: User code must explicitly validate paths
- **Benefit**: Path traversal becomes impossible at compile time

---

### 3. Rate Limiting Not Implemented (SEVERITY: HIGH)

**Location**: `crates/ggen-api/src/middleware/rate_limit.rs`

**Evidence**:
```rust
// ❌ TODO comments in production code
pub async fn rate_limit(request: Request, next: Next) -> Response {
    // TODO: Extract client IP from request
    // TODO: Apply rate limiter
    // TODO: Return 429 if limit exceeded

    next.run(request).await  // ❌ No actual rate limiting!
}
```

**Impact**:
- Denial of Service (DoS) attacks possible
- Resource exhaustion
- No protection against brute force
- No per-IP limits

**BREAKING CHANGE REQUIRED**:

```rust
use governor::{Quota, RateLimiter};
use std::net::IpAddr;

pub struct ProductionRateLimiter {
    // Per-IP rate limiting
    ip_limiter: RateLimiter<IpAddr, DefaultHashingStash, DefaultClock>,

    // Per-endpoint rate limiting
    endpoint_limiters: HashMap<String, RateLimiter<String, DefaultHashingStash, DefaultClock>>,

    // Global rate limiting
    global_limiter: RateLimiter<(), DefaultHashingStash, DefaultClock>,
}

impl ProductionRateLimiter {
    pub fn new(config: RateLimitConfig) -> Self {
        Self {
            ip_limiter: RateLimiter::keyed(
                Quota::per_minute(config.requests_per_ip_per_minute)
            ),
            endpoint_limiters: config.endpoint_limits.iter()
                .map(|(path, limit)| {
                    (path.clone(), RateLimiter::keyed(Quota::per_minute(*limit)))
                })
                .collect(),
            global_limiter: RateLimiter::direct(
                Quota::per_second(config.global_requests_per_second)
            ),
        }
    }

    pub async fn check(&self, ip: IpAddr, endpoint: &str) -> Result<(), RateLimitExceeded> {
        // Check global limit first (fail fast)
        self.global_limiter.check().map_err(|_| RateLimitExceeded::Global)?;

        // Check per-IP limit
        self.ip_limiter.check_key(&ip).map_err(|_| RateLimitExceeded::PerIp(ip))?;

        // Check per-endpoint limit if exists
        if let Some(limiter) = self.endpoint_limiters.get(endpoint) {
            limiter.check_key(&endpoint.to_string())
                .map_err(|_| RateLimitExceeded::PerEndpoint(endpoint.to_string()))?;
        }

        Ok(())
    }
}
```

**Configuration** (Breaking):
```toml
# ggen.toml - NEW REQUIRED SECTION
[security.rate_limiting]
enabled = true
requests_per_ip_per_minute = 60
global_requests_per_second = 1000

[[security.rate_limiting.endpoint_limits]]
path = "/api/marketplace/search"
requests_per_minute = 30

[[security.rate_limiting.endpoint_limits]]
path = "/api/sync"
requests_per_minute = 10
```

---

## 🟡 HIGH: Resource Exhaustion & DoS Protection

### 4. Unbounded Resource Usage (SEVERITY: HIGH)

**Finding**:
- **331 instances** of `Vec::new()` / `HashMap::new()` / `String::new()` without capacity hints
- **497 loops** without bounds checking
- **No memory limits** on RDF graph size
- **No limits** on SPARQL query complexity

**Attack Scenarios**:

```rust
// ❌ Attack 1: Unbounded RDF loading
// Attacker uploads 10GB Turtle file
store.load_turtle("attacker_file.ttl")?; // OOM crash

// ❌ Attack 2: Infinite loop in SPARQL
let query = r#"
    SELECT ?s WHERE {
        ?s ?p ?o .
        FILTER(regex(?o, "(.+)*")) # Catastrophic backtracking
    }
"#;
store.query_sparql(query)?; // Hangs forever

// ❌ Attack 3: Allocation bomb
let mut results = Vec::new(); // No capacity
for triple in huge_dataset {
    results.push(triple); // Reallocations on every iteration
}
```

**BREAKING CHANGES REQUIRED**:

```rust
// 1. Add resource limits to Graph
pub struct GraphLimits {
    pub max_triples: usize,           // Default: 1_000_000
    pub max_file_size_bytes: u64,     // Default: 100MB
    pub max_query_time_ms: u64,       // Default: 5000ms
    pub max_memory_bytes: u64,        // Default: 500MB
}

impl Graph {
    pub fn new() -> Result<Self> {
        Self::with_limits(GraphLimits::default())
    }

    pub fn with_limits(limits: GraphLimits) -> Result<Self> {
        // Enforce limits at construction
        Ok(Self {
            store: Store::new()?,
            limits,
            current_triples: Arc::new(AtomicUsize::new(0)),
        })
    }

    pub fn load_turtle(&self, path: &SafePath) -> Result<()> {
        // Check file size BEFORE loading
        let metadata = std::fs::metadata(path.as_path())?;
        if metadata.len() > self.limits.max_file_size_bytes {
            return Err(Error::new(&format!(
                "RDF file too large: {} bytes (max: {})",
                metadata.len(),
                self.limits.max_file_size_bytes
            )));
        }

        // Load with triple count tracking
        // ...
    }
}

// 2. Enforce capacity hints (via Clippy lint)
// Add to .cargo/config.toml:
[target.'cfg(all())']
rustflags = ["-D", "clippy::vec-init-then-push"]

// 3. Add query timeout
pub struct SparqlExecutor {
    store: Store,
    timeout: Duration,
}

impl SparqlExecutor {
    pub async fn query_with_timeout(&self, query: &str) -> Result<String> {
        tokio::time::timeout(
            self.timeout,
            self.execute_query(query)
        )
        .await
        .map_err(|_| Error::new("SPARQL query timeout exceeded"))?
    }
}
```

**Configuration** (Breaking):
```toml
# ggen.toml - NEW REQUIRED SECTION
[resources]
max_triples = 1_000_000
max_file_size_mb = 100
max_query_time_ms = 5000
max_memory_mb = 500
max_template_size_mb = 10
max_output_files = 10000
```

---

### 5. Missing Timeout Enforcement (SEVERITY: MEDIUM)

**Finding**: Timeout support exists but not universally enforced

**Evidence**:
- 20 files mention timeout/Duration
- **v6 pipeline has no timeout** (`crates/ggen-core/src/v6/pipeline.rs`)
- Sync command has timeout (30s default) but not enforced in domain layer
- No cancellation tokens

**BREAKING CHANGE REQUIRED**:

```rust
use tokio::time::timeout;
use tokio_util::sync::CancellationToken;

pub struct PipelineConfig {
    // ... existing fields ...

    /// NEW: Timeout per pass (μ₁-μ₅)
    pub pass_timeout: Duration,

    /// NEW: Total pipeline timeout
    pub total_timeout: Duration,

    /// NEW: Cancellation support
    pub cancellation_token: Option<CancellationToken>,
}

impl StagedPipeline {
    pub async fn run(&mut self) -> Result<BuildReceipt> {
        let start = Instant::now();
        let total_timeout = self.config.total_timeout;
        let cancellation = self.config.cancellation_token.clone();

        // Wrap entire pipeline in timeout
        timeout(total_timeout, async {
            // Check cancellation before each pass
            for pass in &[μ₁, μ₂, μ₃, μ₄, μ₅] {
                if let Some(token) = &cancellation {
                    if token.is_cancelled() {
                        return Err(Error::new("Pipeline cancelled by user"));
                    }
                }

                // Execute pass with per-pass timeout
                timeout(self.config.pass_timeout, pass.execute()).await??;
            }

            Ok(self.generate_receipt())
        })
        .await
        .map_err(|_| Error::new(&format!(
            "Pipeline timeout exceeded: {:?}",
            total_timeout
        )))??
    }
}
```

**Migration**: All long-running operations must accept `CancellationToken`

---

## 🟡 MEDIUM: Observability Gaps

### 6. Missing Telemetry in Critical Paths (SEVERITY: MEDIUM)

**Finding**:
- Good tracing coverage overall (2854 instances across 395 files)
- **BUT**: v6 pipeline has ZERO tracing spans
- No metrics collection
- No error context propagation

**Evidence**:
```bash
# Searched v6 code for tracing
grep -r "#\[instrument\]|tracing::span" crates/ggen-core/src/v6/
# Result: No matches found
```

**Impact**:
- Cannot debug production issues
- No performance metrics
- No error attribution
- No audit trail for security events

**BREAKING CHANGE REQUIRED**:

```rust
use tracing::{info, warn, error, instrument, Span};
use opentelemetry::metrics::{Counter, Histogram};

// Add tracing to ALL v6 passes
#[instrument(skip(self, ctx), fields(
    pass = "normalization",
    epoch = %ctx.epoch.id,
    rules_count = self.rules.len()
))]
pub fn execute(&self, ctx: &mut PassContext<'_>) -> Result<PassResult> {
    let _span = tracing::info_span!("μ₁_normalize").entered();

    // Emit metrics
    PASS_EXECUTIONS.with_label_values(&["normalization"]).inc();

    let start = Instant::now();
    let result = self.run_normalization(ctx)?;
    let duration = start.elapsed();

    PASS_DURATION_MS
        .with_label_values(&["normalization"])
        .observe(duration.as_millis() as f64);

    info!(
        rules_executed = result.rules_executed,
        triples_added = result.triples_added,
        duration_ms = duration.as_millis(),
        "Normalization complete"
    );

    Ok(result)
}

// Add OpenTelemetry metrics
lazy_static! {
    static ref PASS_EXECUTIONS: Counter =
        Counter::new("ggen_pass_executions_total", "Pass execution count");

    static ref PASS_DURATION_MS: Histogram =
        Histogram::new("ggen_pass_duration_ms", "Pass execution time");

    static ref SPARQL_QUERIES: Counter =
        Counter::new("ggen_sparql_queries_total", "SPARQL query count");

    static ref RDF_TRIPLES: Counter =
        Counter::new("ggen_rdf_triples_total", "RDF triples processed");
}
```

**Configuration** (Breaking):
```toml
# ggen.toml - NEW REQUIRED SECTION
[observability]
enabled = true
export_format = "otlp"  # opentelemetry, jaeger, prometheus
endpoint = "http://localhost:4317"
sample_rate = 1.0

[observability.metrics]
enabled = true
prometheus_port = 9090

[observability.tracing]
enabled = true
level = "info"  # trace, debug, info, warn, error
```

---

### 7. Poor Error Messages (SEVERITY: MEDIUM)

**Finding**: Errors lack context for debugging

**Evidence**:
- Many errors use generic messages
- No file/line information in RDF parse errors
- No query context in SPARQL errors
- Stack traces lost in error propagation

**Example**:
```rust
// ❌ Current: Unhelpful
Error: SPARQL query failed

// ✅ Needed: Actionable
Error: SPARQL query failed at line 3, column 15
Query: SELECT ?s WHERE { ?s ?p ?o
                              ^
Expected: predicate IRI or variable
Context: Processing template 'rust-cli.tera'
Rule: 'generate_structs'
File: .specify/specs/001-cli/entities.ttl:42
```

**BREAKING CHANGE REQUIRED**:

```rust
use std::backtrace::Backtrace;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("SPARQL query error at {location}: {message}\n{context}")]
    SparqlQuery {
        message: String,
        location: SourceLocation,
        query: String,
        context: ErrorContext,
        backtrace: Backtrace,
    },

    #[error("RDF parse error in {file}:{line}:{col}: {message}")]
    RdfParse {
        file: String,
        line: usize,
        col: usize,
        message: String,
        source_snippet: String,
        backtrace: Backtrace,
    },

    #[error("Template error in {template}:{line}: {message}\n{context}")]
    Template {
        template: String,
        line: usize,
        message: String,
        context: ErrorContext,
        backtrace: Backtrace,
    },
}

#[derive(Debug)]
pub struct ErrorContext {
    pub rule_name: Option<String>,
    pub file_path: Option<String>,
    pub line_number: Option<usize>,
    pub user_action: Option<String>,
}

impl Error {
    pub fn suggest_fix(&self) -> String {
        match self {
            Error::SparqlQuery { .. } => "Check SPARQL syntax. Use `ggen validate --sparql` for validation.",
            Error::RdfParse { .. } => "Validate RDF/Turtle syntax. Use `ggen validate --rdf <file>` to check.",
            Error::Template { .. } => "Check Tera template syntax. Use `ggen validate --template <file>`.",
        }
    }
}
```

---

### 8. No Graceful Degradation (SEVERITY: LOW)

**Finding**: No circuit breakers or fallbacks

**Impact**:
- Cascade failures
- No retry logic for transient errors
- No exponential backoff

**BREAKING CHANGE REQUIRED**:

```rust
use std::sync::Arc;
use tokio::sync::Semaphore;

pub struct CircuitBreaker {
    failure_threshold: usize,
    success_threshold: usize,
    timeout: Duration,
    state: Arc<Mutex<CircuitState>>,
}

enum CircuitState {
    Closed { failures: usize },
    Open { opened_at: Instant },
    HalfOpen { successes: usize },
}

impl CircuitBreaker {
    pub async fn call<F, T>(&self, f: F) -> Result<T>
    where
        F: Future<Output = Result<T>>,
    {
        match *self.state.lock().await {
            CircuitState::Open { opened_at } => {
                if opened_at.elapsed() > self.timeout {
                    // Try half-open
                    *self.state.lock().await = CircuitState::HalfOpen { successes: 0 };
                } else {
                    return Err(Error::new("Circuit breaker open"));
                }
            }
            _ => {}
        }

        match f.await {
            Ok(result) => {
                self.on_success().await;
                Ok(result)
            }
            Err(err) => {
                self.on_failure().await;
                Err(err)
            }
        }
    }
}
```

---

## Breaking Changes Summary

| # | Change | Severity | Migration Effort | Timeline |
|---|--------|----------|------------------|----------|
| 1 | Type-safe SPARQL builders | CRITICAL | HIGH | v6.0.0 |
| 2 | SafePath newtype | CRITICAL | MEDIUM | v6.0.0 |
| 3 | Rate limiting | HIGH | LOW | v6.0.0 |
| 4 | Resource limits | HIGH | MEDIUM | v6.0.0 |
| 5 | Timeout enforcement | HIGH | MEDIUM | v6.0.0 |
| 6 | OpenTelemetry metrics | MEDIUM | LOW | v6.1.0 |
| 7 | Rich error types | MEDIUM | MEDIUM | v6.1.0 |
| 8 | Circuit breakers | LOW | LOW | v6.2.0 |

---

## Migration Guide for v6.0.0

### Step 1: Update SPARQL Usage

```bash
# Before
let query = format!("SELECT ?s WHERE {{ ?s <{}> ?o }}", user_input);
store.query(query)?;

# After
let query = SparqlQuery::select(&store)
    .var("s")
    .triple(Var("s"), IRI::from_validated(user_input)?, Var("o"))
    .build()?;
store.execute(query)?;
```

### Step 2: Update Path Handling

```bash
# Before
let path = PathBuf::from(user_input);
graph.load_turtle(&path)?;

# After
let safe_path = SafePath::from_user_input(user_input, &base_dir)?;
graph.load_turtle(&safe_path)?;
```

### Step 3: Add Configuration

```toml
# Add to ggen.toml
[security.rate_limiting]
enabled = true
requests_per_ip_per_minute = 60

[resources]
max_triples = 1_000_000
max_file_size_mb = 100

[observability]
enabled = true
export_format = "otlp"
```

---

## Recommended Actions

### Immediate (Pre-v6.0.0 Release)
1. ✅ Implement SPARQL injection prevention (#1)
2. ✅ Enforce SafePath everywhere (#2)
3. ✅ Add resource limits (#4)
4. ✅ Implement rate limiting (#3)
5. ✅ Add timeout enforcement (#5)

### Next Release (v6.1.0)
6. ⏳ Add OpenTelemetry (#6)
7. ⏳ Improve error messages (#7)

### Future (v6.2.0)
8. ⏳ Add circuit breakers (#8)

---

## Production Deployment Checklist

Before deploying v6 to production:

- [ ] All SPARQL queries use type-safe builders
- [ ] All path operations use SafePath
- [ ] Rate limiting configured and tested
- [ ] Resource limits set appropriately for workload
- [ ] Timeout enforcement verified (load test)
- [ ] Observability configured (metrics + tracing)
- [ ] Error monitoring integrated (Sentry/Datadog)
- [ ] Circuit breakers configured for external dependencies
- [ ] Security audit completed
- [ ] Load testing completed (100 concurrent users)
- [ ] Chaos engineering tests passed
- [ ] Incident response playbook created

---

**Conclusion**: v6 has solid foundations but requires these breaking changes for production readiness. Implementing all changes in v6.0.0 establishes the right precedent and avoids painful migrations later.
