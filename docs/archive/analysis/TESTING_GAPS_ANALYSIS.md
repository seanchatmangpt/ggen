# TESTING GAPS ANALYSIS - 80/20 Opportunities

## Executive Summary

This analysis identifies the critical 20% of testing gaps that would have the biggest impact on code reliability. The focus is on the three main ggen crates: **ggen-core**, **ggen-domain**, and **ggen-cli**.

---

## 1. ERROR PATH TESTING GAPS

### High Priority: Result Types Without Error Tests

These functions return `Result<T>` but have insufficient or missing error condition testing.

#### **ggen-core/src/graph/store.rs**

- **Function**: `GraphStore::open(path)` (Line 58)
  - **Gap**: No tests for corrupted store, permission denied, invalid path encoding
  - **Impact**: Silent failures in production when store is corrupted
  - **Test Case Missing**: 
    - Call with non-existent parent directory
    - Call with read-only parent directory
    - Call with invalid UTF-8 path

- **Function**: `GraphStore::new()` (Line 73)
  - **Gap**: No error tests; only happy path covered
  - **Impact**: Unknown error behavior if Store creation fails
  - **Note**: Tests only use `.unwrap()`

#### **ggen-domain/src/project/gen.rs**

- **Function**: `execute_gen(input)` (Line 105)
  - **Errors Missing**:
    - Template resolution failure scenarios
    - File reading errors during generation
    - Output directory inaccessible
  - **Found Tests**: Only template validation errors (empty ref, invalid vars, length)
  - **Gap**: No tests for file system errors (permission denied, disk full)
  - **File**: `/home/user/ggen/crates/ggen-domain/tests/unit/project_gen_tests.rs`

- **Function**: `parse_vars(vars)` (Line 33)
  - **Gap**: Returns error for "key=value" format but error path tested minimally
  - **Missing**: What if both equals signs are missing? Edge case with multiple '='

- **Function**: `collect_generated_files(output_dir)` (Line 176)
  - **Gap**: No error tests for permission denied on subdirectory traversal
  - **Impact**: Silent failures when directory becomes inaccessible mid-scan

#### **ggen-core/src/cache.rs**

- **Function**: `CacheManager::new()` (Line 113)
  - **Gap**: No tests for cache directory creation failure
  - **Impact**: If ~/.cache/ggen/gpacks cannot be created, error message is generic
  - **Missing**: Permission denied, disk full, invalid directory name

- **Function**: `CacheManager::with_dir(path)` (Line 139)
  - **Gap**: Similar to `new()` - no fs::create_dir_all error scenarios
  - **Impact**: Partial directory creation without cleanup on failure

#### **ggen-domain/src/marketplace/validate.rs**

- **Function**: `validate_package_name(name)` (Line 15)
  - **Tested**: Empty string, length > 100, special chars (.. / \), control chars
  - **Gap**: No tests for Unicode edge cases (homograph attacks with lookalike chars)
  - **Gap**: No tests for null bytes (`\0`)

- **Function**: `validate_toml_file(path, required_fields)` (Line 259)
  - **Gap**: File exists but has read permission denied
  - **Gap**: File is directory (not a file)
  - **Gap**: TOML parsing succeeds but required fields have type mismatches (e.g., boolean instead of string)

#### **ggen-domain/src/hook/create.rs**

- **Function**: `execute_create(input)` (Line 22)
  - **Errors Returned**:
    - Home directory not found (Line 43)
    - fs::create_dir_all fails (Line 47)
    - fs::write fails (Line 60-61)
  - **Gap**: No tests for permission denied on ~/.ggen/hooks
  - **Gap**: No tests for file already exists scenario
  - **Gap**: No tests when input.trigger is extremely long (DOS attack)

#### **ggen-core/src/templates/file_tree_generator.rs**

- **Function**: `FileTreeTemplate::from_file(path)` (Line 198)
  - **Gap**: No error tests for file read permission denied
  - **Gap**: No tests for corrupted YAML
  - **Gap**: No tests for missing required fields in YAML

#### **ggen-core/src/lockfile.rs**

- **Function**: `LockfileManager::upsert()` and related file I/O methods
  - **Gap**: No tests for permission denied on ggen.lock file
  - **Gap**: No tests for corrupted TOML in existing lockfile

#### **ggen-domain/src/project/init.rs**

- **Function**: `init_project(path, name)` (Line 41)
  - **Gap**: No tests for name containing path separators (injection attack)
  - **Gap**: No tests for empty string checking when name = "" edge case
  - **Impact**: Could create hidden directories with path traversal

#### **ggen-core/src/resolver.rs**

- **Function**: `TemplateResolver::resolve(template_ref)` (Line 143+)
  - **Gap**: Missing error path tests for:
    - Malformed template reference (missing colon)
    - Path traversal injection (`../../etc/passwd:template`)
    - Pack not found in lockfile
    - Template path doesn't exist in pack

#### **ggen-domain/src/marketplace/install.rs**

- **Function**: `validate_package_name(name)` (Line 15)
  - **Same as marketplace/validate.rs** - see above

---

## 2. BOUNDARY CONDITION TESTING GAPS

### Critical Boundaries Not Tested

These functions handle strings, numbers, or collections but lack boundary tests.

#### **ggen-domain/src/project/gen.rs**

- **Function**: `validate_input(template_ref, vars)` (Line 49)
  - **Boundary Tests Missing**:
    - template_ref = "" (empty) → **TESTED** ✓
    - template_ref length = 500 chars → **TESTED** ✓
    - template_ref length = 501 chars → **TESTED** ✓
    - template_ref = single space → **NOT TESTED** ✗
    - vars length = 0 (empty vec) → **TESTED** ✓
    - vars length with 1000+ variables → **NOT TESTED** ✗
    - var key = "" (empty) → **TESTED** ✓
    - var value = "" (empty value is valid?) → **NOT TESTED** ✗
    - var with '=' in value (e.g., "url=http://ex.com?a=b") → **TESTED** ✓

- **Function**: `parse_vars(vars)` (Line 33)
  - **Boundary Gap**: 
    - Single variable split on first '=' only → Implementation uses `splitn(2, '=')`
    - If var = "a=" → Produces ["a", ""], is "" valid value? **NOT TESTED**
    - If var = "=" → Produces ["", ""], empty key error → **TESTED** ✓

#### **ggen-domain/src/marketplace/validate.rs**

- **Function**: `validate_package_name(name)` (Line 15)
  - **Tested Boundaries**:
    - name.is_empty() → 0 chars → **TESTED** ✓
    - name.len() > 100 → **TESTED** ✓
    - name with path separators (.., /, \) → **TESTED** ✓
  - **Missing Boundaries**:
    - name.len() = 1 (minimum valid) → **NOT TESTED** ✗
    - name.len() = 100 (maximum valid) → **NOT TESTED** ✗
    - name = " " (only whitespace) → **NOT TESTED** ✗
    - name with only special allowed chars → **NOT TESTED** ✗

- **Function**: `calculate_required_score()` (Line 213)
  - **Boundary Gap**: 
    - required_checks is empty → Division by zero risk? → Uses `.is_empty()` guard → **SAFE**
    - quality_checks is empty → Uses `.is_empty()` check → **SAFE**
    - But no test for 0 applicable quality checks → **TESTED** (returns 100%)

#### **ggen-core/src/templates/file_tree_generator.rs**

- **Function**: `FileTreeTemplate` operations
  - **Boundary Gap**: 
    - Empty template (no nodes) → **NOT TESTED** ✗
    - Single node (minimal template) → **NOT TESTED** ✗
    - Very deep nesting (100+ levels) → **NOT TESTED** ✗
    - Template with circular references (if possible) → **NOT TESTED** ✗

#### **ggen-core/src/graph/core.rs**

- **Function**: Graph query operations
  - **Boundary Gap**:
    - Empty graph → Tested with `assert!(graph.is_empty())`
    - Query returning 0 results → **NOT TESTED** ✗
    - Query returning 1000+ results → **NOT TESTED** ✗
    - Very long SPARQL query (>10KB) → **NOT TESTED** ✗

#### **ggen-core/src/template_cache.rs**

- **Function**: `TemplateCache::new(capacity)` (Line 72)
  - **Boundary Gap**:
    - capacity = 0 → Uses NonZeroUsize fallback to 100 → **SAFE** but **NOT EXPLICITLY TESTED**
    - capacity = 1 (minimum) → **NOT TESTED** ✗
    - capacity = usize::MAX → **NOT TESTED** ✗
  - **Missing**:  Behavior when capacity is set to 1 and we add 2 templates (LRU eviction)

#### **ggen-domain/src/marketplace/search.rs**

- **Constants with Boundary Issues**:
  - `SearchInput.limit` default = 10
  - **Missing Tests**:
    - limit = 0 (should this be invalid?) → **NOT TESTED** ✗
    - limit = 1000000 (DOS attack) → **NOT TESTED** ✗
  - **Scoring multipliers** (Lines 19-46):
    - All hardcoded floats, no boundary validation
    - What if fuzzy similarity returns > 1.0? → **NOT TESTED** ✗

---

## 3. RESOURCE CLEANUP TESTING GAPS

### File/Persistent Resources Without Proper Cleanup Tests

#### **ggen-core/src/graph/store.rs**

- **Drop Implementation**: Implicit (Oxigraph Store handles it)
  - **Gap**: No explicit test for resource cleanup on drop
  - **Test Exists** (`test_store_resource_cleanup`): 
    - Creates store, adds data, drops, reopens
    - **Limitation**: Only verifies data, not that file handles were closed
  - **Missing**: 
    - Lock file cleanup verification
    - Concurrent access cleanup (if two stores open same path)

#### **ggen-core/src/cache.rs**

- **Resource Type**: Temporary directories, Git clones
  - **Function**: `ensure_cached(resolved_pack)` (async)
  - **Gap**: No test for cleanup of temporary directory if git clone fails mid-way
  - **Impact**: Orphaned temp directories in /tmp accumulate
  - **Missing**:
    - Test git clone partial failure → verify temp cleanup
    - Test SHA256 verification failure → verify temp cleanup

#### **ggen-domain/src/hook/create.rs**

- **File I/O**: Creates files in ~/.ggen/hooks
  - **Gap**: No cleanup test if subsequent file operations fail
  - **Example**: 
    - fs::create_dir_all succeeds
    - fs::write fails (permission denied)
    - Hook file not created, but directory remains
  - **Missing**: Error rollback behavior

#### **ggen-domain/src/marketplace/install.rs**

- **Zip File Handling**:
  - **Function**: Unzip operations (if present)
  - **Gap**: No tests for cleanup if extraction fails mid-way
  - **Missing**: 
    - Partial file extraction error handling
    - Temp directory cleanup on extraction failure
    - File permission bits preserved (Unix mode)

#### **ggen-core/src/templates/file_tree_generator.rs**

- **Resource**: File system operations (if writes to disk)
  - **Gap**: No panic-safety tests
  - **Missing**:
    - What happens if write fails mid-tree-generation?
    - Are partially created files/dirs cleaned up?

#### **ggen-core/src/lifecycle/loader.rs**

- **Function**: `load_make(path)` (Line 56)
  - **Resource**: File handle from read_to_string
  - **Status**: Properly cleaned up by Rust
  - **Gap**: No test for large file handling (>1GB make.toml)
  - **Impact**: Could hang or OOM

---

## 4. CONCURRENCY TESTING GAPS

### Arc/Mutex Usage Without Concurrent Access Tests

#### **ggen-core/src/template_cache.rs**

- **Structure** (Line 45-46):
  ```rust
  pub struct TemplateCache {
      cache: Arc<Mutex<LruCache<String, Arc<Template>>>>,
  }
  ```
  - **Concurrency Issues Missing**:
    - **No tests for concurrent `get_or_parse()` calls** from multiple threads
    - **No lock poisoning recovery tests**: If `lock()` returns `Err`, only returns error, no recovery
    - **No starvation tests**: Multiple threads competing for cache lock
    - **Missing**: Send/Sync trait bounds verification
  - **Specific Gap**: Line 118-119 - handles lock poisoning but not tested
    ```rust
    .map_err(|_| Error::new("Cache lock poisoned"))?;
    ```

#### **ggen-core/src/graph/core.rs**

- **Concurrency Structures** (Lines 25-28, 92-97):
  ```rust
  pub struct Graph {
      inner: Arc<Store>,
      epoch: Arc<AtomicU64>,
      plan_cache: Arc<Mutex<LruCache<u64, String>>>,
      result_cache: Arc<Mutex<LruCache<(u64, u64), CachedResult>>>,
  }
  ```
  - **Missing Tests**:
    - **Concurrent graph.query() from 10+ threads**
    - **Concurrent graph.insert_turtle() from multiple threads** (cache invalidation correctness)
    - **Epoch counter overflow**: AtomicU64 wraps at u64::MAX, no test for wrap-around behavior
    - **Lock poisoning on plan_cache**: Line 191+ has `lock().map_err()` handling, but not tested
    - **Race condition between epoch increment and cache check**: 
      - Thread 1 inserts, bumps epoch
      - Thread 2 checks old epoch, uses stale cached result
  - **Missing**: Proper happens-before guarantees with Ordering::Relaxed (Line 143, 148)

#### **ggen-core/src/rdf/template_metadata.rs**

- **Mutex Usage** (Line 14):
  ```rust
  use std::sync::{Arc, Mutex};
  ```
  - **Gap**: These imports exist but appear to be unused in the visible code
  - **OR**: They're used in other methods not shown in the 200-line excerpt
  - **Missing**: If Mutex is actually used, need concurrent access tests

#### **Shared Patterns Across Crates**

- **No General Concurrency Tests Found**:
  - No tests using `std::thread::spawn()` for concurrent access
  - No tests using `tokio::task::spawn()` for async concurrent access
  - No tests for Send/Sync trait bounds on public types
  - No property-based tests for race conditions

---

## SPECIFIC CRITICAL FINDINGS

### 1. **Lock Poisoning Never Tested** (Critical)
   - **Location**: `template_cache.rs` Line 118-119, `graph/core.rs` Line 191
   - **Code**:
     ```rust
     .lock().map_err(|_| Error::new("Cache lock poisoned"))?
     ```
   - **Impact**: If a thread panics while holding the lock, subsequent accesses fail
   - **Missing**: Test that deliberately causes panic inside critical section

### 2. **Path Traversal Security Not Fully Tested** (Critical)
   - **Location**: `marketplace/install.rs` - `validate_package_name()` Line 30
   - **Test**: Checks for "..", "/", "\"
   - **Missing**: 
     - Encoded path traversal: `..%2F..%2Fetc%2Fpasswd`
     - Symlinks pointing outside package directory
     - Unicode normalization attacks

### 3. **Empty Collection Handling Not Tested** (High)
   - **Location**: Multiple collection operations across codebase
   - **Functions**:
     - `marketplace/validate.rs` Line 214: `required_checks.is_empty()`
     - `marketplace/validate.rs` Line 228: `quality_checks.is_empty()`
   - **Missing**: What if ALL required checks fail? Score becomes 0% (no explicit test)

### 4. **File Permission Error Scenarios Never Tested** (High)
   - **Location**: All file I/O operations
   - **Functions**:
     - `fs::read_to_string()` - no permission denied test
     - `fs::create_dir_all()` - no readonly parent test
     - `File::create()` - no permission denied test
   - **Impact**: Silent failures in CI/CD with restricted permissions

### 5. **Async Error Paths Not Fully Tested** (Medium)
   - **Location**: All async functions across ggen-domain
   - **Example**: `execute_gen()` is async but tests use `tokio_test::block_on()`
   - **Missing**:
     - What if future is dropped mid-execution?
     - Cancellation token handling?

---

## RECOMMENDED PRIORITY FIXES (80/20)

### Top 5 High-Impact Tests to Add (20% effort, 80% coverage improvement)

1. **Lock Poisoning Test** (template_cache.rs + graph/core.rs)
   - Test concurrent access with deliberate panic inside critical section
   - Test recovery after poisoning

2. **Path Traversal Security Tests** (marketplace/install.rs)
   - Test URL-encoded path traversal
   - Test symlink attacks
   - Test Unicode normalization attacks

3. **Permission Denied Tests** (All file I/O functions)
   - Test readonly directories
   - Test permission denied on write
   - Use `chmod 000` on test directories

4. **Empty Collection Edge Cases** (marketplace/validate.rs + others)
   - Test division by zero guards
   - Test empty vector operations
   - Test zero-capacity containers

5. **Concurrent Cache Access Tests** (template_cache.rs + graph/core.rs)
   - Test 10+ threads competing for get_or_parse()
   - Test cache eviction under concurrent load
   - Test epoch invalidation race conditions

---

## FILES TO PRIORITIZE FOR TESTING

### ggen-core/src
- `template_cache.rs` - Add concurrency tests
- `graph/core.rs` - Add lock poisoning + concurrency tests
- `cache.rs` - Add directory creation error tests
- `resolver.rs` - Add path traversal tests
- `lockfile.rs` - Add permission denied tests

### ggen-domain/src
- `project/gen.rs` - Add file I/O error tests
- `marketplace/validate.rs` - Add permission denied + Unicode tests
- `marketplace/install.rs` - Add path traversal + zip error tests
- `hook/create.rs` - Add permission denied tests
- `project/init.rs` - Add injection attack tests

### ggen-cli/src
- No critical gaps identified (mostly command routing)

---

## QUANTIFIED IMPACT

- **Functions with Error Paths**: ~50 functions across 3 crates
- **Functions Tested for Errors**: ~15 functions (30%)
- **Coverage Gap**: ~35 functions need error path tests
- **Boundary Conditions Missing**: ~25 edge cases
- **Concurrency Tests Missing**: 100% (zero concurrent access tests found)

**Estimated Risk**: Medium-to-High for production reliability, especially around:
- File permission errors in CI/CD
- Concurrent cache access under load
- Path traversal security vulnerabilities
