# FMEA Failure Mode Analysis: ggen.toml Configuration System

**Analysis Date:** 2025-11-20
**Scope:** ggen-config crate + ggen.toml configuration system
**Components Analyzed:** parser.rs, schema.rs, validator.rs, error.rs, integration tests

---

## Executive Summary

This FMEA identified **47 potential failure modes** across 8 categories for the ggen.toml configuration system. Critical findings include:

- **3 CRITICAL security vulnerabilities** (SPARQL injection, path traversal, environment variable expansion)
- **5 HIGH-severity data integrity issues** (non-deterministic serialization, lock file corruption)
- **12 design-level type safety gaps** (phantom data misuse, lifetime elision)
- **8 runtime stability risks** (stack overflow, memory exhaustion, panic conditions)

**Priority Focus:**
1. Fix SPARQL injection vulnerability (FM-019)
2. Implement deterministic HashMap ordering (FM-022)
3. Add lock file integrity verification (FM-023)
4. Fix path traversal protection bypass (FM-018)

---

## Category 1: Design Failure Modes

### FM-001: Missing PhantomData State Machine
- **Component**: `schema.rs` - GgenConfig struct
- **Description**: Configuration structs don't use PhantomData for type-level state tracking (Valid/Invalid/Partial states)
- **Trigger**: Attempting to use unvalidated configuration
- **Effect**: Runtime validation errors instead of compile-time safety
- **Detection**: Type system allows invalid state transitions; tests show runtime panics
- **Severity**: HIGH
- **Likelihood**: MEDIUM

### FM-002: Const Generic Validation Missing
- **Component**: `validator.rs` - validation logic
- **Description**: Validation rules hardcoded as runtime checks instead of const generics
- **Trigger**: Invalid configuration passed to validator
- **Effect**: Validation happens at runtime instead of compile-time
- **Detection**: Compiler doesn't catch invalid configs; tests catch at runtime
- **Severity**: MEDIUM
- **Likelihood**: HIGH

### FM-003: Zero-Cost Abstraction Violation - HashMap
- **Component**: `schema.rs` - HashMap usage for prefixes, env overrides
- **Description**: HashMap incurs heap allocation cost vs compile-time BTreeMap ordering
- **Trigger**: Large configuration files with many namespaces/overrides
- **Effect**: Unexpected heap allocations, non-deterministic ordering
- **Detection**: Profiling shows heap allocations; serialization non-deterministic
- **Severity**: MEDIUM
- **Likelihood**: HIGH

### FM-004: Lifetime Elision Unsoundness
- **Component**: `parser.rs` - ConfigLoader struct
- **Description**: ConfigLoader uses PathBuf (owned) instead of &Path (reference)
- **Trigger**: Multiple ConfigLoader instances for same file
- **Effect**: Unnecessary clones, memory overhead
- **Detection**: Memory profiler shows duplicate path allocations
- **Severity**: LOW
- **Likelihood**: MEDIUM

### FM-005: Type-Level Validation Missing
- **Component**: `schema.rs` - String fields for enums
- **Description**: provider, log level, format stored as String instead of typed enums
- **Trigger**: Typo in configuration ("opeani" instead of "openai")
- **Effect**: Runtime validation failure instead of compile-time type error
- **Detection**: Tests catch invalid strings; no compile-time safety
- **Severity**: HIGH
- **Likelihood**: HIGH

### FM-006: Trait Object Dispatch Cost
- **Component**: `validator.rs` - validator functions
- **Description**: If validator used dyn Trait, would incur vtable dispatch cost
- **Trigger**: Generic validation logic
- **Effect**: Dynamic dispatch overhead vs monomorphization
- **Detection**: Current implementation uses static functions (no issue yet)
- **Severity**: LOW
- **Likelihood**: LOW (not currently an issue)

### FM-007: Smart Pointer Overhead
- **Component**: `schema.rs` - Option<T> usage
- **Description**: Excessive Option wrapping forces runtime checks
- **Trigger**: Accessing optional configuration sections
- **Effect**: Pattern matching overhead, branch misprediction
- **Detection**: Profiling shows branch misses on Option checks
- **Severity**: LOW
- **Likelihood**: MEDIUM

### FM-008: Macro Expansion Complexity
- **Component**: `schema.rs` - serde derive macros
- **Description**: Complex serde attributes increase compile time
- **Trigger**: Adding new configuration sections with derives
- **Effect**: Slow incremental compilation
- **Detection**: Cargo build times increase beyond SLO (2s incremental)
- **Severity**: MEDIUM
- **Likelihood**: MEDIUM

### FM-009: Const Evaluation Limits
- **Component**: `schema.rs` - default value functions
- **Description**: Default values use runtime functions instead of const evaluation
- **Trigger**: Const evaluation of complex default logic
- **Effect**: Runtime overhead for defaults vs compile-time constants
- **Detection**: Default functions not const; runtime initialization
- **Severity**: LOW
- **Likelihood**: MEDIUM

### FM-010: Type Alias Transparency
- **Component**: `error.rs` - Result<T> type alias
- **Description**: Type alias allows error type to leak implementation details
- **Trigger**: Refactoring error types breaks public API
- **Effect**: Breaking changes to consumers
- **Detection**: Semver checker shows breaking API changes
- **Severity**: MEDIUM
- **Likelihood**: LOW

### FM-011: Generic Constraints Missing
- **Component**: `parser.rs` - from_str generic parameter
- **Description**: No trait bounds on generic parameters
- **Trigger**: Passing type that doesn't implement required traits
- **Effect**: Confusing error messages, late error detection
- **Detection**: Compiler errors instead of trait bound violations
- **Severity**: LOW
- **Likelihood**: LOW

### FM-012: Phantom Data Misuse
- **Component**: Future type-level state machine implementation
- **Description**: PhantomData used incorrectly (covariance/contravariance)
- **Trigger**: Invalid variance annotations
- **Effect**: Unsound API allowing invalid state transitions
- **Detection**: Miri/unsafe code checker flags variance issues
- **Severity**: CRITICAL (if implemented incorrectly)
- **Likelihood**: LOW (not yet implemented)

---

## Category 2: Implementation Failure Modes

### FM-013: TOML Parsing Ambiguity
- **Component**: `parser.rs` - ConfigLoader::from_str
- **Description**: Toml crate doesn't handle all TOML 1.0 edge cases
- **Trigger**: Exotic TOML syntax (inline tables, dotted keys, multi-line strings)
- **Effect**: Parse errors or incorrect values
- **Detection**: Integration tests with complex TOML fail
- **Severity**: MEDIUM
- **Likelihood**: MEDIUM

### FM-014: Serialization Non-Determinism
- **Component**: `schema.rs` - HashMap serialization order
- **Description**: HashMap serialization order is non-deterministic
- **Trigger**: Serializing config with HashMap fields (env, prefixes)
- **Effect**: Non-reproducible ggen.toml output, diff noise
- **Detection**: Multiple serializations produce different byte-level output
- **Severity**: HIGH (breaks deterministic requirement)
- **Likelihood**: HIGH

### FM-015: Validation Incomplete Coverage
- **Component**: `validator.rs` - validate_all()
- **Description**: Validator doesn't check all configuration sections
- **Trigger**: Adding new config section without validation
- **Effect**: Invalid configs pass validation
- **Detection**: Tests show invalid configs accepted
- **Severity**: HIGH
- **Likelihood**: MEDIUM

### FM-016: Environment Override Injection
- **Component**: `parser.rs` - apply_env_overrides()
- **Description**: Dotted key notation allows arbitrary override paths
- **Trigger**: Malicious env override like "../../etc/passwd"
- **Effect**: Unintended configuration modification, potential security issue
- **Detection**: Security tests with malicious override paths
- **Severity**: CRITICAL (security)
- **Likelihood**: MEDIUM

### FM-017: Default Value Inconsistency
- **Component**: `schema.rs` - default functions
- **Description**: Default values differ between schema.rs and validator.rs expectations
- **Trigger**: Changing defaults without updating validators
- **Effect**: Valid configs rejected, invalid configs accepted
- **Detection**: Tests show defaults rejected by validator
- **Severity**: HIGH
- **Likelihood**: MEDIUM

### FM-018: Path Traversal in file_path
- **Component**: `parser.rs` - ConfigLoader::new()
- **Description**: No path canonicalization or traversal protection
- **Trigger**: Loading "../../../../etc/passwd" as ggen.toml
- **Effect**: Reading arbitrary files outside project
- **Detection**: Security tests with traversal paths
- **Severity**: CRITICAL (security)
- **Likelihood**: HIGH

### FM-019: SPARQL Injection
- **Component**: `schema.rs` - graph queries stored as String
- **Description**: No SPARQL parameterization; queries vulnerable to injection
- **Trigger**: User-provided SPARQL query with malicious syntax
- **Effect**: Arbitrary RDF graph queries, data exfiltration
- **Detection**: Security tests with injection payloads
- **Severity**: CRITICAL (security)
- **Likelihood**: MEDIUM (if SPARQL queries user-controlled)

### FM-020: Integer Overflow in Size Parsing
- **Component**: `validator.rs` - is_valid_size_format()
- **Description**: Size parsing (1GB, 512MB) doesn't check for u32 overflow
- **Trigger**: Config with cache_size = "99999999999GB"
- **Effect**: Integer overflow, incorrect size limits
- **Detection**: Tests with extreme size values cause overflow
- **Severity**: MEDIUM
- **Likelihood**: LOW

### FM-021: Regex Denial of Service
- **Component**: `validator.rs` - version regex, size format regex
- **Description**: No regex timeout; complex inputs cause catastrophic backtracking
- **Trigger**: Malicious version string with nested patterns
- **Effect**: Validator hangs, DoS condition
- **Detection**: Fuzz testing with complex strings causes timeout
- **Severity**: HIGH
- **Likelihood**: LOW

### FM-022: Non-Deterministic HashMap Iteration
- **Component**: `schema.rs` - env overrides, prefixes iteration
- **Description**: HashMap iteration order is non-deterministic
- **Trigger**: Applying environment overrides in different order
- **Effect**: Different config outputs from same inputs
- **Detection**: Tests show order-dependent results
- **Severity**: HIGH (breaks determinism)
- **Likelihood**: HIGH

---

## Category 3: Runtime Failure Modes

### FM-023: Stack Overflow from Deep Nesting
- **Component**: `parser.rs` - recursive TOML parsing
- **Description**: Deeply nested TOML structures cause stack overflow
- **Trigger**: Config with 1000+ levels of nesting
- **Effect**: Stack overflow panic
- **Detection**: Fuzz tests with deep nesting trigger stack overflow
- **Severity**: HIGH
- **Likelihood**: LOW

### FM-024: Memory Exhaustion from Large Configs
- **Component**: `parser.rs` - loading entire file into String
- **Description**: read_to_string() loads entire file into memory
- **Trigger**: Extremely large ggen.toml (100MB+)
- **Effect**: Out of memory panic
- **Detection**: Tests with large configs cause OOM
- **Severity**: MEDIUM
- **Likelihood**: LOW

### FM-025: Panic from unwrap() in Default Functions
- **Component**: `schema.rs` - num_cpus() function
- **Description**: available_parallelism() can fail, unwrap_or(4) used
- **Trigger**: Environment where thread detection fails
- **Effect**: Potential panic if unwrap_or fails
- **Detection**: Tests in restricted environments
- **Severity**: LOW (has fallback)
- **Likelihood**: LOW

### FM-026: LRU Cache Contention
- **Component**: Future cache implementation
- **Description**: If LRU cache added for parsed configs, lock contention possible
- **Trigger**: Concurrent config loads
- **Effect**: Lock contention, performance degradation
- **Detection**: Benchmarks show mutex contention
- **Severity**: MEDIUM
- **Likelihood**: LOW (not yet implemented)

### FM-027: File Handle Exhaustion
- **Component**: `parser.rs` - repeated file loads
- **Description**: No file handle pooling; repeated loads exhaust handles
- **Trigger**: Loading many configs in tight loop
- **Effect**: Too many open files error
- **Detection**: Tests with 1000+ config loads fail
- **Severity**: LOW
- **Likelihood**: LOW

### FM-028: Thread Explosion from Parallel Validation
- **Component**: Future parallel validator
- **Description**: If validation parallelized, unbounded thread spawning
- **Trigger**: Validating large configs with many sections
- **Effect**: Thread exhaustion, OOM
- **Detection**: Tests with parallel validation show thread leak
- **Severity**: MEDIUM
- **Likelihood**: LOW (not yet implemented)

### FM-029: Deadlock in Circular Dependency Resolution
- **Component**: Future workspace dependency resolution
- **Description**: Circular workspace dependencies cause deadlock
- **Trigger**: Workspace member A depends on B, B depends on A
- **Effect**: Infinite loop, hang
- **Detection**: Tests with circular deps hang
- **Severity**: HIGH
- **Likelihood**: MEDIUM

### FM-030: Race Condition in Concurrent Config Updates
- **Component**: Future concurrent config writer
- **Description**: Multiple threads updating config simultaneously
- **Trigger**: Concurrent write operations
- **Effect**: Data corruption, partial writes
- **Detection**: Concurrency tests show corrupted configs
- **Severity**: HIGH
- **Likelihood**: LOW (not yet implemented)

---

## Category 4: Security Failure Modes

### FM-031: Command Injection via lifecycle hooks
- **Component**: Future lifecycle hook execution
- **Description**: Shell commands in lifecycle hooks not sanitized
- **Trigger**: Malicious lifecycle.hooks.before_build = "rm -rf /"
- **Effect**: Arbitrary command execution
- **Detection**: Security tests with injection payloads
- **Severity**: CRITICAL
- **Likelihood**: HIGH (if hooks implemented)

### FM-032: Template Injection via template variables
- **Component**: Future template rendering
- **Description**: Template variables not escaped, allow code injection
- **Trigger**: Malicious templates.vars with Tera injection syntax
- **Effect**: Arbitrary code execution during template render
- **Detection**: Security tests with injection payloads
- **Severity**: CRITICAL
- **Likelihood**: HIGH (if templates implemented)

### FM-033: Plugin Permission Bypass
- **Component**: Future plugin system
- **Description**: Plugin permissions not enforced at runtime
- **Trigger**: Malicious plugin exceeds declared permissions
- **Effect**: Filesystem/network access beyond permissions
- **Detection**: Security tests with permission violations
- **Severity**: CRITICAL
- **Likelihood**: MEDIUM (if plugins implemented)

### FM-034: Secret Exposure in Environment Variables
- **Component**: `parser.rs` - env overrides
- **Description**: Environment variables logged/exposed in error messages
- **Trigger**: Config error with env var containing secrets
- **Effect**: Secret leakage via logs/errors
- **Detection**: Security tests check error messages
- **Severity**: HIGH
- **Likelihood**: MEDIUM

### FM-035: Insecure Deserialization
- **Component**: `parser.rs` - toml::from_str
- **Description**: Deserializing untrusted TOML without size limits
- **Trigger**: Malicious TOML with billion laughs attack
- **Effect**: Memory exhaustion, DoS
- **Detection**: Fuzz tests with expansion attacks
- **Severity**: HIGH
- **Likelihood**: MEDIUM

### FM-036: Registry MITM Attack
- **Component**: Future package registry integration
- **Description**: HTTPS not enforced for registry downloads
- **Trigger**: MITM attacker intercepts package downloads
- **Effect**: Malicious package installation
- **Detection**: Security tests with HTTP registries
- **Severity**: CRITICAL
- **Likelihood**: LOW (if registry implemented securely)

### FM-037: Unsafe Plugin Loading
- **Component**: Future plugin dynamic loading
- **Description**: Plugins loaded without signature verification
- **Trigger**: User installs malicious plugin
- **Effect**: Arbitrary code execution
- **Detection**: Security tests with unsigned plugins
- **Severity**: CRITICAL
- **Likelihood**: HIGH (if plugins implemented)

### FM-038: Time-of-Check Time-of-Use (TOCTOU)
- **Component**: `parser.rs` - file existence check then read
- **Description**: File checked with exists() then read with read_to_string()
- **Trigger**: File replaced between check and read
- **Effect**: Reading unexpected file content
- **Detection**: Concurrency tests with file replacement
- **Severity**: MEDIUM
- **Likelihood**: LOW

---

## Category 5: Data Integrity Failure Modes

### FM-039: Lock File Corruption
- **Component**: Future ggen.lock implementation
- **Description**: Partial writes to lock file if process killed
- **Trigger**: SIGKILL during lock file write
- **Effect**: Corrupted lock file, build failures
- **Detection**: Tests with interrupted writes
- **Severity**: HIGH
- **Likelihood**: MEDIUM

### FM-040: Version Mismatch Between Config and Lock
- **Component**: Future lock file version tracking
- **Description**: ggen.toml updated but ggen.lock not regenerated
- **Trigger**: Manual config edit without lock regeneration
- **Effect**: Dependency version mismatch, build errors
- **Detection**: Tests comparing config vs lock timestamps
- **Severity**: HIGH
- **Likelihood**: HIGH

### FM-041: Dependency Resolution Circular Loop
- **Component**: Future dependency resolution
- **Description**: Circular dependencies not detected
- **Trigger**: Package A depends on B, B depends on A
- **Effect**: Infinite loop in resolver
- **Detection**: Tests with circular deps hang
- **Severity**: HIGH
- **Likelihood**: MEDIUM

### FM-042: Non-Deterministic Output Ordering
- **Component**: `schema.rs` - HashMap serialization
- **Description**: HashMap fields serialize in random order
- **Trigger**: Serializing config to TOML
- **Effect**: Diff noise, non-reproducible builds
- **Detection**: Multiple serializations differ
- **Severity**: HIGH (violates determinism requirement)
- **Likelihood**: HIGH

### FM-043: Floating Point Precision Loss
- **Component**: `schema.rs` - f32 temperature field
- **Description**: Float serialization/deserialization loses precision
- **Trigger**: temperature = 0.7 becomes 0.699999988
- **Effect**: Configuration drift, validation failures
- **Detection**: Round-trip tests show precision loss
- **Severity**: LOW
- **Likelihood**: MEDIUM

### FM-044: Unicode Normalization Inconsistency
- **Component**: `parser.rs` - String handling
- **Description**: Unicode strings not normalized (NFC vs NFD)
- **Trigger**: Config with accented characters in different forms
- **Effect**: String comparison failures, non-determinism
- **Detection**: Tests with Unicode strings fail
- **Severity**: MEDIUM
- **Likelihood**: LOW

### FM-045: Timestamp Drift in Lock File
- **Component**: Future lock file timestamps
- **Description**: Lock file timestamps not deterministic
- **Trigger**: Lock file regenerated at different times
- **Effect**: Non-reproducible builds
- **Detection**: Lock file comparison shows timestamp changes
- **Severity**: MEDIUM
- **Likelihood**: HIGH

---

## Category 6: Integration Failure Modes

### FM-046: Workspace Member Not Found
- **Component**: Future workspace resolution
- **Description**: Workspace member path doesn't exist
- **Trigger**: workspace.members = ["crates/*"] but no crates/ dir
- **Effect**: Build failure, missing dependency
- **Detection**: Tests with missing member paths
- **Severity**: HIGH
- **Likelihood**: MEDIUM

### FM-047: Template Inheritance Infinite Loop
- **Component**: Future template system
- **Description**: Template extends itself or creates circular chain
- **Trigger**: Template A extends B, B extends A
- **Effect**: Infinite loop in template resolver
- **Detection**: Tests with circular extends hang
- **Severity**: HIGH
- **Likelihood**: MEDIUM

---

## Priority Matrix

### CRITICAL (Must Fix Immediately)
1. **FM-016**: Environment Override Injection - SPARQL query injection
2. **FM-018**: Path Traversal in file_path - arbitrary file read
3. **FM-019**: SPARQL Injection - graph query exploitation
4. **FM-031**: Command Injection via lifecycle hooks (if implemented)
5. **FM-032**: Template Injection via variables (if implemented)
6. **FM-036**: Registry MITM Attack (if implemented)
7. **FM-037**: Unsafe Plugin Loading (if implemented)

### HIGH (Fix Before Production)
1. **FM-005**: Type-Level Validation Missing - String enums
2. **FM-014**: Serialization Non-Determinism - HashMap ordering
3. **FM-015**: Validation Incomplete Coverage - missing checks
4. **FM-017**: Default Value Inconsistency
5. **FM-022**: Non-Deterministic HashMap Iteration
6. **FM-039**: Lock File Corruption
7. **FM-040**: Version Mismatch Between Config and Lock
8. **FM-042**: Non-Deterministic Output Ordering
9. **FM-046**: Workspace Member Not Found

### MEDIUM (Address in Next Sprint)
1. **FM-002**: Const Generic Validation Missing
2. **FM-003**: Zero-Cost Abstraction Violation - HashMap
3. **FM-008**: Macro Expansion Complexity
4. **FM-013**: TOML Parsing Ambiguity
5. **FM-020**: Integer Overflow in Size Parsing
6. **FM-024**: Memory Exhaustion from Large Configs
7. **FM-034**: Secret Exposure in Environment Variables
8. **FM-044**: Unicode Normalization Inconsistency

### LOW (Technical Debt / Future Enhancement)
1. **FM-004**: Lifetime Elision Unsoundness
2. **FM-007**: Smart Pointer Overhead
3. **FM-009**: Const Evaluation Limits
4. **FM-025**: Panic from unwrap() in Default Functions
5. **FM-027**: File Handle Exhaustion

---

## Recommended Actions

### Immediate (Week 1)
1. **Fix FM-018**: Add path canonicalization in ConfigLoader::new()
2. **Fix FM-019**: Implement SPARQL parameterization
3. **Fix FM-022**: Replace HashMap with BTreeMap for determinism
4. **Add integration test**: Test path traversal protection

### Short-Term (Week 2-3)
1. **Fix FM-005**: Convert String enums to typed enums (AiProvider, LogLevel, etc.)
2. **Fix FM-014**: Ensure all HashMap usage replaced with BTreeMap
3. **Fix FM-015**: Add validation for all config sections
4. **Fix FM-017**: Audit default values across schema and validator

### Medium-Term (Month 1)
1. **Implement ggen.lock**: Address FM-039, FM-040, FM-045
2. **Add fuzzing tests**: Detect FM-021, FM-023, FM-035
3. **Security audit**: Review all user-controlled inputs
4. **Performance profiling**: Measure FM-003, FM-007, FM-009

### Long-Term (Ongoing)
1. **Type-level state machine**: PhantomData for Valid/Invalid states
2. **Const generic validation**: Compile-time config checks
3. **Plugin security model**: Permissions, sandboxing, signatures
4. **Registry security**: HTTPS, signatures, checksum verification

---

## Testing Gaps

### Critical Missing Tests
1. **Path traversal protection** - FM-018
2. **SPARQL injection** - FM-019
3. **Environment override injection** - FM-016
4. **Deterministic serialization** - FM-014, FM-022, FM-042
5. **Circular dependency detection** - FM-041, FM-047

### Required Test Suites
1. **Security**: Injection, traversal, overflow, DoS
2. **Fuzzing**: Parse errors, deep nesting, large inputs
3. **Concurrency**: Race conditions, deadlocks, TOCTOU
4. **Integration**: Workspace resolution, template composition
5. **Property-based**: Round-trip, determinism, invariants

---

## Conclusion

The ggen.toml configuration system has **7 CRITICAL**, **9 HIGH**, **8 MEDIUM**, and **5 LOW** severity failure modes identified. Primary risks are:

1. **Security vulnerabilities** - Injection attacks, path traversal
2. **Determinism violations** - HashMap ordering, timestamp drift
3. **Type safety gaps** - String-based enums, runtime validation
4. **Data integrity** - Lock file corruption, version mismatch

**Immediate action required** on CRITICAL security issues before production deployment.

**Next Steps:**
1. Implement path traversal protection (FM-018)
2. Replace HashMap with BTreeMap (FM-014, FM-022)
3. Add typed enums for providers/levels (FM-005)
4. Implement SPARQL parameterization (FM-019)
5. Add comprehensive security test suite
