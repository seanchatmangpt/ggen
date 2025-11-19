# Marketplace Container Stress Tests - Codebase Exploration Summary

## 1. Marketplace Container Validation System Location

### Primary Files:
- **Main Validation Logic**: `/home/user/ggen/crates/ggen-domain/src/marketplace/validate.rs` (544 lines)
  - Comprehensive validation system with required and quality checks
  - Score calculation: Required checks (60% weight) + Quality checks (40% weight)
  - Production ready threshold: ≥95% AND all required checks must pass

### Validation Components:

#### Required Checks (Critical - 60% weight):
1. **PackageToml**: Must have name, version, description fields
2. **Readme**: Must exist with >100 characters
3. **SourceCode**: src/main.rs or src/lib.rs OR template-only packages
4. **License**: LICENSE, LICENSE-MIT, or LICENSE-APACHE

#### Quality Checks (Bonus - 40% weight):
1. **RdfOntology**: ≥200 lines in rdf/ontology.ttl file
2. **SparqlQueries**: .rq or .sparql files in sparql/ directory
3. **Examples**: Example files in examples/ directory
4. **Tests**: Test files (.rs, .py, .ts) in tests/ directory
5. **Documentation**: .md files in docs/ directory

### Key Implementation Details:
- Located in `ggen-domain` crate for separation from CLI concerns
- Uses `ggen_core::graph::Graph` API for RDF validation
- Returns detailed validation report with errors and warnings
- Supports batch validation of all packages in a directory

---

## 2. Container Management and Validation Code

### Package Installation System:
**File**: `/home/user/ggen/crates/ggen-domain/src/marketplace/install.rs` (1,209 lines)

#### Key Features:
- **Package Validation**: Name validation to prevent injection attacks
  - Alphanumeric with hyphens/underscores
  - Max 100 characters
  - No path traversal sequences ("../", "/", "\\")
  - No control characters

- **Installation Options**:
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

- **Dependency Graph Management**:
  - Circular dependency detection via DFS
  - Topological sorting for install order (Kahn's algorithm)
  - Version resolution (semver: ^, ~, >=, latest)

- **Download and Extraction**:
  - HTTP client with 60-second timeout
  - Retry logic with exponential backoff (1s, 2s, 4s)
  - ZIP extraction with security validation
  - Zip slip prevention, path traversal prevention
  - Zip bomb protection: 100MB max size, 10,000 file limit

- **Cache Management**:
  - Checksum verification (SHA256)
  - Atomic operations for lockfile updates
  - Corrupted cache detection and cleanup

- **Lockfile Management**:
  - Atomic write using temp file + rename pattern
  - Stores resolved versions and integrity hashes
  - Prevents concurrent write corruption

---

## 3. Testing Infrastructure

### Test Organization:
```
/home/user/ggen/crates/
├── ggen-domain/tests/
│   ├── marketplace_validation_integration.rs    # Validation tests
│   ├── integration/
│   │   ├── marketplace_list_fallback_tests.rs
│   │   ├── project_lifecycle_tests.rs
│   │   └── ...
│   ├── unit/
│   ├── property/
│   ├── performance/
│   └── ...
├── ggen-cli/tests/
│   ├── stress/
│   │   ├── marketplace_stress_test.rs          # Stress test framework
│   │   └── mod.rs
│   ├── integration/
│   │   ├── complete_marketplace_test.rs
│   │   ├── marketplace_tests.rs
│   │   └── testcontainers_readiness.rs        # Docker/container tests
│   └── ...
└── ggen-marketplace/tests/
    ├── integration_critical_paths.rs
    ├── property_based_invariants.rs
    ├── crypto_ed25519.rs
    ├── error_scenarios.rs
    └── ...
```

### Testing Frameworks Available:
- **Unit Tests**: Rust's built-in #[test]
- **Integration Tests**: Full feature tests
- **Property-Based**: proptest for invariant testing
- **Containers**: testcontainers v0.25, testcontainers-modules v0.13
- **BDD**: Cucumber v0.21 with macros
- **Mocking**: mockito v1.7, mockall v0.13
- **Performance**: Criterion v0.7 with HTML reports
- **Chicago TDD Tools**: Custom testing framework for deterministic tests

### Test Helper Modules:
- `crates/ggen-cli/tests/utils/`: Common testing utilities
- `crates/ggen-marketplace/tests/common/`: Marketplace-specific helpers
- Permutation utilities for edge case generation

---

## 4. Stress Test Location and Framework

### Stress Test Files:
**Location**: `/home/user/ggen/crates/ggen-cli/tests/stress/`

#### Main Components:
- `marketplace_stress_test.rs` (438 lines): Core stress testing framework
- `mod.rs`: Module definition

### StressTestRunner Features:

#### Configuration:
```rust
pub struct StressConfig {
    pub concurrency: usize,              // Default: 10
    pub total_operations: usize,         // Default: 1000
    pub timeout: Duration,               // Default: 300s
    pub include_destructive: bool,       // Default: false
}
```

#### Stress Test Methods:
1. **run_concurrent_search_stress()**: High-concurrency queries
2. **run_rapid_sequential_stress()**: Rapid sequential operations
3. **run_large_dataset_stress()**: Large dataset handling
4. **run_memory_stress()**: Memory allocation stress
5. **run_filesystem_stress()**: File I/O stress

#### Metrics Collected:
- Operations completed/failed
- Average/min/max latency (ms)
- Throughput (ops/sec)
- Peak memory usage
- Total duration

#### Built-in Tests:
- test_concurrent_search_stress (10 concurrency, 50 ops, 10s timeout)
- test_rapid_sequential_stress (100 ops, 10s timeout)
- test_memory_stress (100 ops allocation)
- test_filesystem_stress (50 ops)

---

## 5. Performance Requirements and Timeouts

### Current Timeout Configurations:

**Default HTTP Timeout**: 30 seconds
- Located in `crates/ggen-domain/src/marketplace/search.rs`
- Constant: `DEFAULT_TIMEOUT_SECONDS = 30`

**Installation Download Timeout**: 60 seconds
- Located in `crates/ggen-domain/src/marketplace/install.rs`
- Used in `reqwest::Client::builder().timeout(Duration::from_secs(60))`

**Retry Logic**:
- Max retries: 3 attempts
- Exponential backoff: 1s, 2s, 4s between retries

### Crates.io Dry-Run Requirement:
**Note**: The "33 seconds crates.io dry-run requirement" was not found explicitly in the current codebase. This may be:
- A new requirement to be added
- A performance SLA being established
- Referenced in branch-specific documentation not yet committed

**Recommendation**: This requirement should be documented in:
- `/home/user/ggen/crates/ggen-marketplace/docs/`
- As a constant in `search.rs` or `install.rs`
- In the stress test configuration

---

## 6. Package Installation and Uninstallation

### Installation Flow:

1. **Validation Phase**:
   ```rust
   validate_package_name(package_name)  // Security check
   load_package_info_from_registry()     // Get metadata from registry
   ```

2. **Download Phase**:
   ```rust
   get_cache_path()                      // Determine cache location
   verify_cache()                        // Check cached file
   download_with_retry()                 // HTTP download with retries
   verify_checksum()                     // SHA256 validation
   save_cache()                          // Store for future use
   ```

3. **Extraction Phase**:
   ```rust
   extract_package_from_zip()            // Security: path traversal checks
   create_dir_all()                      // Create target directory
   ```

4. **Lockfile Phase**:
   ```rust
   load_lockfile()                       // Read existing lockfile
   update_lockfile()                     // Add new package entry
   save_lockfile()                       // Atomic write
   ```

### Uninstallation:
**Note**: Uninstallation logic is in install.rs module but focused on installation.
For uninstallation, would need:
- Reverse dependency resolution
- Safe directory removal
- Lockfile cleanup

---

## 7. Network and Registry Handling Code

### Network Operations:

**HTTP Client Setup** (`install.rs`):
```rust
reqwest::Client::builder()
    .timeout(Duration::from_secs(60))
    .user_agent(format!("ggen/{}", env!("CARGO_PKG_VERSION")))
    .build()
```

**Retry Strategy**:
- Function: `download_with_retry(url: &str, max_retries: u32)`
- Exponential backoff: `delay = 1 << (attempt - 1)` seconds
- Handles both network errors and HTTP errors
- Distinguishes between client errors (4xx - fail fast) and server errors (5xx - retry)

### Registry Management:

**Files**:
- `crates/ggen-domain/src/marketplace/registry.rs` (600+ lines)
- `crates/ggen-marketplace/src/backend/mod.rs`
- `crates/ggen-marketplace/src/backend/local.rs` (150+ lines)

**Registry Index Format**:
```rust
pub struct RegistryIndex {
    pub version: String,                              // e.g., "1.0"
    pub updated_at: String,                           // RFC3339 timestamp
    pub packages: HashMap<String, PackageMetadata>,   // Name -> metadata
}
```

**Package Metadata**:
```rust
pub struct PackageMetadata {
    pub name: String,
    pub versions: Vec<VersionMetadata>,
    pub description: String,
    pub author: Option<String>,
    pub category: Option<String>,
    pub tags: Vec<String>,
    pub repository: Option<String>,
    pub license: Option<String>,
    pub homepage: Option<String>,
}
```

**Version Resolution**:
- Downloads registry index from GitHub Pages
- Supports semver ranges: `^`, `~`, `>=`
- Supports `latest` version specifier
- Caches registry data locally

**Local Registry Backend**:
- File-system based storage in `{db_path}/index.json`
- Thread-safe with `Arc<RwLock<HashMap>>`
- Async I/O with tokio
- Supports search and discovery

---

## 8. Recommended Locations for New Stress Tests

### For Marketplace Container Stress Tests:

#### 1. **Core Stress Test Framework**:
Path: `/home/user/ggen/crates/ggen-cli/tests/stress/`
- Extend `marketplace_stress_test.rs`
- Add container-specific stress scenarios

#### 2. **Container Validation Tests**:
Path: `/home/user/ggen/crates/ggen-cli/tests/integration/`
- New file: `container_stress_validation.rs`
- Use testcontainers framework
- Test Docker container orchestration

#### 3. **Package Installation Stress**:
Path: `/home/user/ggen/crates/ggen-domain/tests/marketplace/`
- New subdirectory: `performance/`
- Test installation under load
- Test with network latency simulation

#### 4. **Registry Stress**:
Path: `/home/user/ggen/crates/ggen-marketplace/tests/`
- New file: `registry_stress_tests.rs`
- Test concurrent registry operations
- Test cache behavior under load

#### 5. **End-to-End Stress**:
Path: `/home/user/ggen/tests/integration/`
- New file: `marketplace_container_e2e_stress.rs`
- Full workflow: search → install → validate → use
- Multi-container orchestration

---

## 9. Key Files Summary Table

| Component | File | Lines | Purpose |
|-----------|------|-------|---------|
| Validation | `domain/marketplace/validate.rs` | 544 | Package validation scoring |
| Installation | `domain/marketplace/install.rs` | 1209 | Package installation logic |
| Registry | `domain/marketplace/registry.rs` | 600+ | Registry management |
| Search | `domain/marketplace/search.rs` | 600+ | Package search logic |
| Publish | `domain/marketplace/publish.rs` | 300+ | Package publishing |
| Stress Framework | `cli/tests/stress/marketplace_stress_test.rs` | 438 | Stress testing runner |
| Local Registry | `marketplace/src/backend/local.rs` | 150+ | File-based registry |
| Validation Tests | `domain/tests/marketplace_validation_integration.rs` | 192 | Integration tests |

---

## 10. Architecture Insights

### Multi-Crate Design:
- **ggen-utils**: Common utilities and error handling
- **ggen-domain**: Domain logic (marketplace operations)
- **ggen-core**: Core RDF/graph operations
- **ggen-marketplace**: Marketplace library and backend implementations
- **ggen-cli**: CLI interface and integration tests

### Security Architecture:
- **Input Validation**: Package name injection prevention
- **Download Security**: Checksum verification, retry logic, timeouts
- **ZIP Security**: Size limits, path traversal checks, file count limits
- **Atomic Operations**: Lockfile updates prevent corruption

### Performance Considerations:
- **Caching**: Package cache in `~/.ggen/cache/downloads/`
- **Async I/O**: Full tokio async runtime
- **Connection Pooling**: HTTP client reuse
- **Concurrent Operations**: Semaphore-based concurrency control

### Testing Strategy:
- Unit tests for individual functions
- Integration tests for workflows
- Property-based tests for invariants
- Stress tests for load scenarios
- Container tests for infrastructure validation
- E2E tests for user workflows

