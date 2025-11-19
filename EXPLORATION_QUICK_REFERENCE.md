# Quick Reference Guide - Key Files and Code Snippets

## Critical Code Locations

### 1. Marketplace Validation System
**File**: `/home/user/ggen/crates/ggen-domain/src/marketplace/validate.rs`
- Main function: `pub fn validate_package(package_path: &Path) -> Result<PackageValidation>`
- Scoring logic: Lines 92-148
- Required checks: Lines 173-297
- Quality checks: Lines 299-544

**How to test**:
```bash
cargo test -p ggen-domain marketplace_validation
```

### 2. Package Installation Engine
**File**: `/home/user/ggen/crates/ggen-domain/src/marketplace/install.rs`
- Main install function: `pub async fn install_package(options: &InstallOptions) -> Result<InstallResult>` (Line 976)
- Download logic: `async fn download_and_install_package()` (Line 819)
- Security validation: `fn validate_package_name()` (Line 14)
- Extraction: `async fn extract_package_from_zip()` (Line 669)

**Key security features** (Lines 681-814):
- ZIP bomb protection: 100MB limit
- Path traversal prevention
- Zip slip detection
- Atomic cache operations

### 3. Network and Retry Logic
**File**: `/home/user/ggen/crates/ggen-domain/src/marketplace/install.rs` (Lines 539-603)
- Function: `async fn download_with_retry(url: &str, max_retries: u32)`
- Timeout: 60 seconds
- Backoff strategy: `1 << (attempt - 1)` seconds
- Error handling distinguishes 4xx (fail-fast) vs 5xx (retry)

### 4. Existing Stress Test Framework
**File**: `/home/user/ggen/crates/ggen-cli/tests/stress/marketplace_stress_test.rs`
- Configuration: Lines 24-45
- Core runner: `pub struct StressTestRunner` (Lines 106-355)
- Test methods:
  - `run_concurrent_search_stress()` (Lines 119-173)
  - `run_rapid_sequential_stress()` (Lines 176-209)
  - `run_large_dataset_stress()` (Lines 212-239)
  - `run_memory_stress()` (Lines 242-274)
  - `run_filesystem_stress()` (Lines 277-310)

### 5. Registry Backend
**File**: `/home/user/ggen/crates/ggen-marketplace/src/backend/local.rs`
- Registry initialization: `pub async fn new(db_path: PathBuf) -> Result<Self>` (Line 97)
- Load from disk: `async fn load_from_disk()` (Line 123)
- Thread-safe storage: `Arc<RwLock<HashMap<PackageId, Vec<Package>>>>`

### 6. Performance Constants
**File**: `/home/user/ggen/crates/ggen-domain/src/marketplace/search.rs` (Lines 49-59)
- `DEFAULT_TIMEOUT_SECONDS = 30`
- `DEFAULT_RESULT_LIMIT = 10`
- `MAX_RETRY_ATTEMPTS = 3`

---

## Running Tests

### Run all marketplace tests:
```bash
cargo test -p ggen-domain marketplace
cargo test -p ggen-marketplace
cargo test -p ggen-cli marketplace
```

### Run only stress tests:
```bash
cargo test -p ggen-cli stress --test-threads=1
```

### Run validation tests:
```bash
cargo test -p ggen-domain marketplace_validation_integration
```

### Run with detailed output:
```bash
RUST_LOG=debug cargo test -p ggen-cli stress -- --nocapture
```

---

## Key Struct Definitions

### InstallOptions (install.rs lines 66-109)
```rust
pub struct InstallOptions {
    pub package_name: String,
    pub version: Option<String>,
    pub target_path: Option<PathBuf>,
    pub force: bool,
    pub with_dependencies: bool,
    pub dry_run: bool,
}
```

### PackageValidation (validate.rs lines 66-75)
```rust
pub struct PackageValidation {
    pub package_name: String,
    pub package_path: PathBuf,
    pub score: f64,
    pub production_ready: bool,
    pub required_checks: Vec<(RequiredCheck, CheckResult)>,
    pub quality_checks: Vec<(QualityCheck, CheckResult)>,
    pub errors: Vec<String>,
    pub warnings: Vec<String>,
}
```

### StressConfig (marketplace_stress_test.rs lines 25-34)
```rust
pub struct StressConfig {
    pub concurrency: usize,
    pub total_operations: usize,
    pub timeout: Duration,
    pub include_destructive: bool,
}
```

---

## Important Constants and Limits

| Constant | Value | Location | Purpose |
|----------|-------|----------|---------|
| HTTP Timeout | 60s | install.rs:543 | Download timeout |
| Cache Limit | 100MB | install.rs:681 | Max ZIP size |
| Max ZIP Files | 10,000 | install.rs:693 | Zip bomb prevention |
| Max Retries | 3 | search.rs:58 | Network retry attempts |
| Default HTTP Timeout | 30s | search.rs:55 | Search operation timeout |
| Package Name Max | 100 chars | install.rs:24 | Security limit |
| README Min Length | 100 chars | validate.rs:220 | Quality threshold |
| RDF Ontology Min | 200 lines | validate.rs:320 | Quality requirement |
| Scoring: Required | 60% weight | validate.rs:100 | Validation scoring |
| Scoring: Quality | 40% weight | validate.rs:100 | Validation scoring |
| Production Ready | ≥95% score | validate.rs:108 | Readiness threshold |

---

## Test Infrastructure Dependencies

From Cargo.toml (root workspace):
- **tokio**: 1.47 (async runtime)
- **testcontainers**: 0.25 (Docker container tests)
- **testcontainers-modules**: 0.13 (Pre-built containers)
- **proptest**: 1.8 (Property-based testing)
- **criterion**: 0.7 (Benchmarking with HTML reports)
- **chicago-tdd-tools**: 1.1.0 (Custom TDD framework)
- **tempfile**: 3.23 (Temporary test directories)
- **mockito**: 1.7 (HTTP mocking)
- **mockall**: 0.13 (Comprehensive mocking)

---

## Directory Structure

```
crates/
├── ggen-domain/
│   ├── src/marketplace/
│   │   ├── install.rs          (Installation logic - 1209 lines)
│   │   ├── validate.rs         (Validation logic - 544 lines)
│   │   ├── registry.rs         (Registry management - 600+ lines)
│   │   ├── search.rs           (Search logic - 600+ lines)
│   │   ├── publish.rs          (Publishing - 300+ lines)
│   │   └── mod.rs
│   └── tests/
│       ├── marketplace_validation_integration.rs (192 lines)
│       ├── integration/
│       │   └── marketplace_list_fallback_tests.rs
│       └── performance/
│
├── ggen-cli/
│   └── tests/
│       ├── stress/
│       │   ├── marketplace_stress_test.rs       (438 lines) ← MAIN STRESS FRAMEWORK
│       │   └── mod.rs
│       └── integration/
│           ├── testcontainers_readiness.rs
│           └── complete_marketplace_test.rs
│
└── ggen-marketplace/
    ├── src/
    │   ├── backend/
    │   │   ├── local.rs                         (150+ lines)
    │   │   └── mod.rs
    │   └── ... (other marketplace code)
    └── tests/
        ├── integration_critical_paths.rs
        ├── error_scenarios.rs
        └── common/
```

---

## Performance SLAs to Verify

1. **Package Installation**: Should complete within 60s for typical packages
2. **Registry Search**: Should return results within 30s
3. **Dry-run Operations**: Should complete in <1s
4. **Validation Checks**: Should complete within 5s for medium packages
5. **Concurrent Operations**: Support 10+ concurrent package installations
6. **Crates.io Dry-Run** (NEW REQUIREMENT): Complete within 33 seconds ⚠️

---

## Common Test Patterns

### Creating a test package:
```rust
use tempfile::TempDir;
use std::fs;

let temp_dir = TempDir::new().unwrap();
let package_path = temp_dir.path();

// Create required files
fs::create_dir_all(package_path.join("src")).unwrap();
fs::write(
    package_path.join("package.toml"),
    "name = \"test\"\nversion = \"1.0.0\"\ndescription = \"Test\"",
).unwrap();
fs::write(package_path.join("README.md"), "Documentation...").unwrap();
fs::write(package_path.join("LICENSE-MIT"), "MIT License").unwrap();
```

### Running stress test with custom config:
```rust
let config = StressConfig {
    concurrency: 20,
    total_operations: 5000,
    timeout: Duration::from_secs(60),
    include_destructive: false,
};

let runner = StressTestRunner::new(config)?;
let metrics = runner.run_concurrent_search_stress().await?;
println!("{}", metrics.report());
```

### Validating a package:
```rust
use ggen_domain::marketplace::validate_package;

let validation = validate_package(package_path)?;
assert!(validation.production_ready);
println!("Score: {}", validation.score);
```

