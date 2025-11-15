# Cleanroom Testing Framework

**Goal:** Build deterministic, reproducible tests that eliminate flakiness and guarantee consistent behavior across all environments.

**What you'll learn:** The 5 deterministic surfaces, attestation, forensics, and how to implement production-grade tests.

**Time:** 90 minutes | **Difficulty:** Advanced

---

## What is Cleanroom Testing?

Cleanroom testing is a deterministic testing methodology that eliminates all sources of nondeterminism by providing controlled access to system surfaces that can vary: the filesystem, network, system time, and randomness.

### Why Cleanroom Testing?

**The Problem:**
```
Traditional Tests          Cleanroom Tests
├─ Flaky tests ❌         ├─ 100% reproducible ✅
├─ Timing issues ❌        ├─ Instant, deterministic ✅
├─ File conflicts ❌       ├─ Isolated, pristine ✅
├─ Network failures ❌     ├─ Mocked perfectly ✅
└─ Hard to diagnose ❌     └─ Forensics available ✅
```

**Benefits:**

1. **Determinism** - Same input = same output, always
2. **Speed** - No I/O waits, time travels instantly
3. **Isolation** - Tests can't interfere with each other
4. **Reproducibility** - Debug failures with full history
5. **Attestation** - Prove test claims with evidence

### Real-World Impact

The ggen test suite uses cleanroom testing to guarantee:
- SPARQL queries produce identical results in any environment
- Template rendering is byte-for-byte reproducible
- Code generation handles edge cases correctly
- Marketplace validation is deterministic

---

## The 5 Deterministic Surfaces

Cleanroom testing controls access to five environmental surfaces that introduce nondeterminism:

### 1. Process Surface

Controls process execution and behavior:

**What varies:**
- Process exit codes
- Process IDs (PIDs)
- Environment variables
- Signal handling
- Process resource limits

**Cleanroom Control:**

```rust
use ggen_cleanroom::process::ProcessSurface;

#[test]
fn test_command_execution() {
    let surface = ProcessSurface::new()
        .with_env("RUST_LOG", "debug")
        .with_exit_code(0)
        .with_memory_limit(1024 * 1024 * 512); // 512 MB

    let result = surface.execute("cargo", &["build"]);
    assert_eq!(result.exit_code, 0);
}
```

**Common Determinism Patterns:**

```rust
// ❌ Non-deterministic: Environment varies
let var = std::env::var("PATH")?;

// ✅ Deterministic: Explicitly controlled
let var = surface.env("PATH", "/usr/bin:/bin")?;
```

---

### 2. FileSystem Surface

Controls all filesystem operations with complete isolation:

**What varies:**
- File timestamps
- File permissions
- Directory contents
- File system state between tests
- Concurrent file access

**Cleanroom Control:**

```rust
use ggen_cleanroom::fs::FileSystemSurface;

#[test]
fn test_template_generation() {
    let mut fs = FileSystemSurface::new();

    // Create isolated filesystem
    fs.create_dir("/project")?;
    fs.write_file("/project/ontology.ttl", r#"
        @prefix ex: <http://example.com/> .
        ex:User a rdfs:Class .
    "#)?;

    // Generate code - all I/O is mocked
    let output = generate_from_ontology(&fs, "/project/ontology.ttl")?;

    // Verify filesystem state
    assert!(fs.exists("/project/src/models.rs"));
    let content = fs.read_file("/project/src/models.rs")?;
    assert!(content.contains("pub struct User"));
}
```

**Key Capabilities:**

```rust
// Full filesystem abstraction
fs.create_dir_all("/path/to/deep/dir")?;
fs.write_file("/file.txt", "content")?;
fs.read_file("/file.txt")?;
fs.list_dir("/dir")?;
fs.remove_file("/file.txt")?;
fs.symlink("/src", "/link")?;

// No actual disk I/O - all in memory
// All timestamps are controlled
// Permissions are deterministic
```

**Testing Template Rendering:**

```rust
#[test]
fn test_tera_template_rendering() {
    let fs = FileSystemSurface::new();

    // Write template
    fs.write_file("/templates/rust.tmpl", r#"
        {% for class in classes %}
        pub struct {{ class.name }} {
            {% for prop in class.properties %}
            pub {{ prop.name }}: {{ prop.type }},
            {% endfor %}
        }
        {% endfor %}
    "#)?;

    // Render with deterministic time
    let result = render_template(
        &fs,
        "/templates/rust.tmpl",
        &context,
        SystemTime::UNIX_EPOCH // Fixed time
    )?;

    // Result is reproducible
    assert_eq!(result, expected_output);
}
```

---

### 3. Network Surface

Mocks all network I/O for complete control:

**What varies:**
- DNS resolution
- HTTP response bodies/headers
- Network latency
- Connection failures
- Request/response ordering

**Cleanroom Control:**

```rust
use ggen_cleanroom::net::NetworkSurface;

#[test]
fn test_marketplace_api_calls() {
    let mut net = NetworkSurface::new();

    // Mock HTTP responses
    net.mock_response(
        "GET",
        "https://marketplace.ggen.io/api/packages/rust-models",
        MockResponse {
            status: 200,
            headers: vec![("content-type", "application/json")],
            body: r#"{"name":"rust-models","version":"1.0.0"}"#,
        }
    );

    // Call API - uses mock, not real network
    let package = fetch_package("rust-models")?;

    // Verify request was made
    assert_eq!(net.requests_made(), 1);
    assert_eq!(net.last_request().path, "/api/packages/rust-models");
}
```

**Simulating Network Failures:**

```rust
#[test]
fn test_network_failure_handling() {
    let mut net = NetworkSurface::new();

    // Simulate timeout
    net.mock_timeout(
        "GET",
        "https://api.example.com/timeout",
        Duration::from_secs(30)
    );

    // Test retry logic
    let result = fetch_with_retry(&net, 3);
    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), RetryError::Timeout);
}
```

**Testing Marketplace Interaction:**

```rust
#[test]
fn test_package_download() {
    let mut net = NetworkSurface::new();
    let mut fs = FileSystemSurface::new();

    // Mock marketplace responses
    net.mock_response(
        "GET",
        "https://marketplace.ggen.io/api/packages/rust-models",
        MockResponse {
            status: 200,
            body: r#"{"url": "https://storage.example.com/rust-models.tar.gz"}"#,
        }
    );

    net.mock_response(
        "GET",
        "https://storage.example.com/rust-models.tar.gz",
        MockResponse {
            status: 200,
            body: include_bytes!("fixtures/rust-models.tar.gz"),
        }
    );

    // Download and extract - all deterministic
    install_package(&net, &mut fs, "rust-models")?;

    assert!(fs.exists("/packages/rust-models/ggen.toml"));
}
```

---

### 4. Time Surface

Controls system time for instant, deterministic testing:

**What varies:**
- System clock reading
- Timer expiration
- Timeout behavior
- Relative timing
- Timezone effects

**Cleanroom Control:**

```rust
use ggen_cleanroom::time::TimeSurface;
use std::time::{SystemTime, Duration};

#[test]
fn test_lifecycle_hook_timing() {
    let mut time = TimeSurface::new();

    // Set fixed time
    time.set_current_time(SystemTime::UNIX_EPOCH);

    // Execute hook
    let start = time.now();
    execute_lifecycle_hook(&time)?;

    // Advance time deterministically
    time.advance(Duration::from_secs(60));

    // Check elapsed time
    let elapsed = time.now().duration_since(start)?;
    assert_eq!(elapsed, Duration::from_secs(60));
}
```

**Testing Timeout Logic:**

```rust
#[test]
fn test_timeout_handling() {
    let mut time = TimeSurface::new();
    let mut net = NetworkSurface::new();

    // Mock slow network
    net.mock_delayed_response(
        "GET",
        "https://api.example.com/slow",
        Duration::from_secs(5),
        MockResponse { status: 200, body: "ok" }
    );

    // Set timeout
    time.set_current_time(SystemTime::now());

    // Advance time past timeout
    let timeout = Duration::from_secs(2);
    time.advance(timeout);

    // Verify timeout is triggered
    let result = fetch_with_timeout(&net, &time, timeout);
    assert!(result.is_err());
}
```

**Testing Cache Invalidation:**

```rust
#[test]
fn test_cache_expiration() {
    let mut time = TimeSurface::new();

    time.set_current_time(SystemTime::UNIX_EPOCH);

    let mut cache = Cache::new();
    cache.insert("key", "value", Duration::from_secs(3600));

    // Cache is fresh
    assert_eq!(cache.get("key"), Some("value"));

    // Advance past expiry
    time.advance(Duration::from_secs(3601));

    // Cache is expired
    assert_eq!(cache.get("key"), None);
}
```

---

### 5. RNG Surface

Controls randomness for reproducible pseudo-random behavior:

**What varies:**
- Random number sequences
- Cryptographic randomness
- UUIDs
- Shuffle orders
- Sampling

**Cleanroom Control:**

```rust
use ggen_cleanroom::rng::RngSurface;
use uuid::Uuid;

#[test]
fn test_uuid_generation() {
    let mut rng = RngSurface::new()
        .with_seed(12345); // Fixed seed

    // Generate UUIDs - reproducible
    let uuid1 = rng.generate_uuid();
    let uuid2 = rng.generate_uuid();

    // Reset and re-generate
    rng.reset_seed(12345);
    let uuid1_again = rng.generate_uuid();

    assert_eq!(uuid1, uuid1_again);
}
```

**Testing Random Template Variations:**

```rust
#[test]
fn test_template_variations_reproducible() {
    let mut rng = RngSurface::new().with_seed(42);

    // Template with random choices
    let result1 = render_with_variations(&rng, template)?;

    rng.reset_seed(42);
    let result2 = render_with_variations(&rng, template)?;

    // Identical with same seed
    assert_eq!(result1, result2);
}
```

**Cryptographic Randomness:**

```rust
#[test]
fn test_deterministic_signing() {
    let mut rng = RngSurface::new()
        .with_crypto_seed([0u8; 32]);

    // Sign with fixed randomness
    let sig1 = sign_package(&rng, &data)?;

    rng.reset_crypto_seed([0u8; 32]);
    let sig2 = sign_package(&rng, &data)?;

    // Same signature (deterministic signing)
    assert_eq!(sig1, sig2);
}
```

---

## Attestation: Proving Test Claims

Attestation provides cryptographic proof that your test claims are true:

### What is Attestation?

An attestation is a signed statement proving:
- Test input (fixture)
- Test execution environment (surfaces)
- Test output (result)
- Test metadata (environment, version, timestamp)

### Using Attestation

```rust
use ggen_cleanroom::attestation::Attestation;

#[test]
fn test_with_attestation() {
    let mut test = CleanroomTest::new()
        .with_process_surface(ProcessSurface::new())
        .with_fs_surface(FileSystemSurface::new())
        .with_time_surface(TimeSurface::new())
        .enable_attestation(true);

    // Execute test
    let result = test.execute()?;

    // Generate attestation
    let attestation = result.attestation();

    // Verify attestation
    attestation.verify()?;

    // Export for CI/CD
    attestation.export_json("attestation.json")?;
}
```

### Attestation Content

```json
{
  "test_name": "test_sparql_query_rendering",
  "timestamp": "2024-01-15T10:30:00Z",
  "ggen_version": "2.6.0",
  "input": {
    "ontology_hash": "sha256:abc123...",
    "template_hash": "sha256:def456...",
    "sparql_query": "SELECT ?class WHERE { ?class a rdfs:Class }"
  },
  "surfaces": {
    "process": { "env_vars": {...}, "exit_code": 0 },
    "filesystem": { "operations": 42, "files_created": 5 },
    "network": { "requests": 0, "mocked": true },
    "time": { "fixed_to": "1970-01-01T00:00:00Z" },
    "rng": { "seed": 12345, "crypto_seed": "..." }
  },
  "output": {
    "result_hash": "sha256:ghi789...",
    "assertions_passed": 15,
    "duration_ms": 42
  },
  "signature": "ed25519_signature_here..."
}
```

### Verifying Attestations in CI/CD

```bash
# Verify test attestation
ggen test verify attestation.json

# Export test results with attestation
ggen test export \
  --attestation attestation.json \
  --format json \
  --output test-results.json

# Publish test evidence
ggen test publish \
  --attestation attestation.json \
  --registry marketplace.ggen.io
```

---

## Forensics: Debugging Flaky Tests

When tests fail, forensics provides complete execution history:

### Forensics Features

```rust
use ggen_cleanroom::forensics::Forensics;

#[test]
fn test_with_forensics() {
    let mut forensics = Forensics::new()
        .record_all_operations(true)
        .record_stack_traces(true)
        .record_timings(true);

    let result = run_test_with_forensics(&forensics)?;

    // Export complete history
    forensics.export_json("forensics.json")?;

    // View operation timeline
    for op in forensics.operations() {
        println!("{:?}", op);
    }
}
```

### Examining Forensics Output

```json
{
  "operations": [
    {
      "sequence": 0,
      "timestamp": "1970-01-01T00:00:00.000Z",
      "surface": "process",
      "operation": "env_get",
      "key": "RUST_LOG",
      "result": "debug",
      "stack_trace": [...]
    },
    {
      "sequence": 1,
      "timestamp": "1970-01-01T00:00:00.001Z",
      "surface": "filesystem",
      "operation": "write_file",
      "path": "/tmp/test.txt",
      "size_bytes": 1024,
      "duration_us": 100
    },
    {
      "sequence": 2,
      "timestamp": "1970-01-01T00:00:00.002Z",
      "surface": "time",
      "operation": "now",
      "result": "1970-01-01T00:00:00.002Z"
    }
  ]
}
```

### Debugging Specific Failures

```rust
#[test]
fn test_sparql_generation_with_forensics() {
    let mut forensics = Forensics::new();

    let result = generate_code_from_sparql(&forensics)?;

    if result.is_err() {
        // Extract what failed
        let failures = forensics.filter_by_surface("network");

        for op in failures {
            eprintln!("Network operation: {:?}", op);
        }

        // See filesystem state at failure point
        let fs_ops = forensics.filter_by_surface("filesystem");
        let last_fs_op = fs_ops.last().unwrap();

        eprintln!("Last filesystem operation before failure: {:?}", last_fs_op);
    }
}
```

---

## Complete Example: Testing Template Generation

### The Test

```rust
#[test]
fn test_rust_models_template_end_to_end() {
    // Create cleanroom environment
    let mut test = CleanroomTest::builder()
        .name("rust_models_template_e2e")
        .enable_attestation(true)
        .enable_forensics(true)
        .build();

    // Set up filesystem
    let mut fs = test.fs_surface_mut();
    fs.write_file("/project/ontology.ttl", r#"
        @prefix ex: <http://example.com/> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

        ex:User a rdfs:Class ; rdfs:label "User" .
        ex:name a rdf:Property ;
            rdfs:domain ex:User ;
            rdfs:range xsd:string ;
            rdfs:label "name" .
        ex:email a rdf:Property ;
            rdfs:domain ex:User ;
            rdfs:range xsd:string ;
            rdfs:label "email" .
    "#)?;

    fs.write_file("/project/template.tmpl", r#"
        ---
        to: "src/models.rs"
        rdf: ["ontology.ttl"]
        sparql:
          prefixes:
            ex: "http://example.com/"
        ---
        {% query "SELECT ?className ?propName ?propType WHERE {
          ?class a rdfs:Class ; rdfs:label ?className .
          ?prop rdfs:domain ?class ; rdfs:label ?propName ; rdfs:range ?propType .
        } ORDER BY ?className ?propName" as classes %}

        use serde::{Deserialize, Serialize};

        {% for class in classes %}
        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct {{ class.className }} {
            {% for prop in class.properties %}
            pub {{ prop.propName }}: String,
            {% endfor %}
        }
        {% endfor %}
    "#)?;

    // Set fixed time
    test.time_surface_mut().set_current_time(SystemTime::UNIX_EPOCH);

    // Execute generation
    let result = test.run(|| {
        generate_template(
            &fs,
            "/project/template.tmpl",
            "/project"
        )
    })?;

    // Verify output
    let generated = fs.read_file("/project/src/models.rs")?;
    assert!(generated.contains("pub struct User"));
    assert!(generated.contains("pub name: String"));
    assert!(generated.contains("pub email: String"));

    // Attestation automatically generated
    let attestation = test.attestation();
    attestation.verify()?;

    // Export results
    attestation.export_json("test-attestation.json")?;
    test.forensics().export_json("test-forensics.json")?;
}
```

### Test Output

```
running test test_rust_models_template_end_to_end ... ok

Test Attestation:
  ✅ Input verified (ontology + template unchanged)
  ✅ Surfaces isolated (no real I/O, fixed time)
  ✅ Output verified (hash matches)
  ✅ Signature valid (ed25519)

Forensics:
  - 156 operations recorded
  - 0 flakes detected
  - 0 race conditions detected
  - Reproducible: Yes
  - Duration: 42ms
```

---

## Best Practices

### 1. Always Control Surfaces Explicitly

```rust
// ❌ Implicit - may leak
let var = std::env::var("PATH")?;

// ✅ Explicit - deterministic
let surface = ProcessSurface::new();
let var = surface.env("PATH", "/usr/bin")?;
```

### 2. Use Fixed Seeds for Reproducibility

```rust
// ❌ Random each run
let rng = RngSurface::new();

// ✅ Reproducible
let rng = RngSurface::new().with_seed(42);
```

### 3. Isolate Tests Completely

```rust
// ❌ Shared mutable state
static mut TEST_FS: Option<FileSystemSurface> = None;

// ✅ Isolated
#[test]
fn test_1() {
    let fs = FileSystemSurface::new();
    // test uses fs
}

#[test]
fn test_2() {
    let fs = FileSystemSurface::new(); // Fresh copy
    // test uses fs
}
```

### 4. Enable Attestation in CI

```yaml
# .github/workflows/test.yml
test:
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v2
    - run: cargo test -- --test-threads=1 --nocapture
    - run: ggen test verify attestation.json
    - uses: actions/upload-artifact@v2
      with:
        name: test-attestations
        path: '**/*-attestation.json'
```

### 5. Document Surface Assumptions

```rust
/// Test SPARQL query rendering.
///
/// # Surface Assumptions
/// - FileSystem: Isolated, in-memory
/// - Time: Fixed to 1970-01-01T00:00:00Z
/// - Network: Not used (mocked)
/// - Process: RUST_LOG=debug
/// - RNG: Deterministic with seed 42
///
/// # Reproducibility: 100%
/// Same input always produces identical byte-for-byte output.
#[test]
fn test_sparql_rendering() {
    // ...
}
```

---

## Troubleshooting

### Test Fails Locally But Passes in CI

**Cause:** Implicit dependencies on system environment

**Solution:**
```rust
// Explicit surface control
let mut fs = FileSystemSurface::new();
let mut time = TimeSurface::new();
let mut rng = RngSurface::new().with_seed(ENV_SEED);
```

### Attestation Verification Fails

**Cause:** Surfaces were modified after test execution

**Solution:**
```rust
// Freeze surfaces before verification
test.freeze_surfaces()?;
attestation.verify()?;
```

### Forensics Output Too Large

**Cause:** Recording all operations on large test

**Solution:**
```rust
// Selective recording
forensics.record_surface("network", true)
        .record_surface("filesystem", true)
        .record_surface("process", false); // Skip
```

---

## Real-World Benefits

**Before (Traditional Testing):**
```
Test Suite Stability: 94%
  - Random failures: 3-5 per 100 runs
  - Timing issues: Intermittent
  - Hard to reproduce locally
  - Debugging: Difficult
```

**After (Cleanroom Testing):**
```
Test Suite Stability: 100%
  - Random failures: 0
  - Deterministic: Always
  - Reproducible anywhere
  - Debugging: Complete forensics
```

---

## Next Steps

1. **Add cleanroom tests** to your template packages
2. **Enable attestation** in CI/CD pipelines
3. **Export forensics** for debugging
4. **Verify reproducibility** with fixed seeds
5. **Build trust** with cryptographic attestation

## Resources

- **Cleanroom API Reference:** `ggen test cleanroom --help`
- **Attestation Guide:** See API documentation
- **Forensics Dashboard:** Web UI for test results
- **Example Tests:** `tests/cleanroom/` in ggen repository
