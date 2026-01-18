<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen Development Best Practices](#ggen-development-best-practices)
  - [Code Quality Standards](#code-quality-standards)
    - [Testing Philosophy: Chicago School TDD](#testing-philosophy-chicago-school-tdd)
    - [Error Handling: No unwrap/expect in Production](#error-handling-no-unwrapexpect-in-production)
    - [Code Coverage: 80%+ on Critical Paths](#code-coverage-80-on-critical-paths)
  - [Documentation Standards](#documentation-standards)
    - [Framework: Diataxis (4 Quadrants)](#framework-diataxis-4-quadrants)
    - [Code Examples: JavaScript + JSDoc + Zod (NOT TypeScript)](#code-examples-javascript--jsdoc--zod-not-typescript)
    - [Documentation Validation: All Examples Must Pass](#documentation-validation-all-examples-must-pass)
  - [Performance Standards](#performance-standards)
    - [Build System: cargo make (NOT direct cargo)](#build-system-cargo-make-not-direct-cargo)
    - [Andon Signals: Stop the Line on Errors](#andon-signals-stop-the-line-on-errors)
    - [Service Level Objectives (SLOs)](#service-level-objectives-slos)
  - [Rust-Specific Best Practices](#rust-specific-best-practices)
    - [Type-First Thinking](#type-first-thinking)
    - [Zero-Cost Abstractions](#zero-cost-abstractions)
    - [Explicit Ownership Semantics](#explicit-ownership-semantics)
  - [Git Workflow Best Practices](#git-workflow-best-practices)
    - [Commit Messages: Conventional Commits](#commit-messages-conventional-commits)
    - [Git Hooks: Pre-Commit Validation](#git-hooks-pre-commit-validation)
  - [Security Best Practices](#security-best-practices)
    - [Never Hardcode Secrets](#never-hardcode-secrets)
    - [Allowed Domains: Whitelist External Resources](#allowed-domains-whitelist-external-resources)
  - [Key Takeaways (80/20 Rule)](#key-takeaways-8020-rule)
  - [Detailed Best Practices](#detailed-best-practices)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen Development Best Practices

**Quick Reference**: Coding standards and conventions
**Last Updated**: 2025-12-11

---

## Code Quality Standards

### Testing Philosophy: Chicago School TDD

**Approach**: State-based testing with real collaborators (NOT mocks)

**Core Principle**: Tests verify observable behavior, not implementation details

**Example**:
```rust
// ✅ GOOD: Chicago School TDD
#[test]
fn test_lockfile_upsert() {
    // Arrange: Real objects
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    // Act: Call public API
    manager.upsert("pkg", "1.0.0", "sha256", "url").unwrap();

    // Assert: Verify observable state
    let entry = manager.get("pkg").unwrap().unwrap();
    assert_eq!(entry.version, "1.0.0");
    assert_eq!(entry.checksum, "sha256");
}

// ❌ BAD: London School (mocks)
#[test]
fn test_with_mocks() {
    let mock_repo = MockRepository::new();
    mock_repo.expect_save().times(1);  // Fragile to refactoring
    // ...
}
```

**Why**: Chicago School tests are resilient to refactoring and verify actual behavior

**See**: `docs/contributing/TESTING.md`

---

### Error Handling: No unwrap/expect in Production

**Rule**: Production code MUST use `Result<T, E>` - NO `unwrap()` or `expect()`

**Exemption**: Tests, benchmarks, and examples MAY use `unwrap()`/`expect()`

**Examples**:
```rust
// ❌ WRONG: Production code
pub fn load_config(path: &Path) -> Config {
    let content = fs::read_to_string(path).unwrap();  // Can panic!
    toml::from_str(&content).unwrap()
}

// ✅ CORRECT: Production code
pub fn load_config(path: &Path) -> Result<Config, Error> {
    let content = fs::read_to_string(path)
        .map_err(|e| Error::new(&format!("Failed to read config: {}", e)))?;
    toml::from_str(&content)
        .map_err(|e| Error::new(&format!("Failed to parse config: {}", e)))
}

// ✅ CORRECT: Test code (EXEMPT)
#[test]
fn test_config_loading() {
    let config = load_config(test_path).unwrap();  // Tests SHOULD panic
    assert_eq!(config.name, "test");
}
```

**Why**: Production code must handle errors gracefully; tests should fail fast

**See**: `crates/ggen-utils/src/error.rs`

---

### Code Coverage: 80%+ on Critical Paths

**Requirements**:
- **Core logic**: 80%+ coverage (domain, RDF engine, templates)
- **CLI commands**: 60%+ coverage (integration tests)
- **Utilities**: 40%+ coverage (helpers, formatting)

**How to Check**:
```bash
cargo make coverage      # Generate coverage report
open target/coverage/index.html
```

**Focus Areas** (80/20 rule):
- 20% of code = 80% of bugs → Focus coverage here
- Public APIs MUST be tested
- Error paths MUST be tested
- Happy paths are baseline

**See**: `docs/TESTING_STRATEGY.md`

---

## Documentation Standards

### Framework: Diataxis (4 Quadrants)

**Quadrants**:
1. **Tutorials**: Learning-oriented (step-by-step for beginners)
2. **How-To Guides**: Problem-solving (practical recipes)
3. **Reference**: Information-oriented (API docs, config reference)
4. **Explanations**: Understanding-oriented (concepts, design decisions)

**Rule**: Every doc MUST fit exactly one quadrant (no mixing)

**Examples**:
- Tutorial: `docs/tutorials/01-quick-start.md` (learn by doing)
- How-To: `docs/how-to/generation/use-templates.md` (solve specific problem)
- Reference: `docs/reference/cli/commands.md` (lookup information)
- Explanation: `docs/explanations/fundamentals/ontology-driven-development.md` (understand concepts)

**See**: `docs/diataxis/README.md`

---

### Code Examples: JavaScript + JSDoc + Zod (NOT TypeScript)

**Standard**: All code examples use JavaScript with JSDoc type annotations

**Rationale**: TypeScript is a build-time dependency; JSDoc works everywhere

**Examples**:
```javascript
// ✅ CORRECT: JavaScript + JSDoc
/**
 * @typedef {Object} GenerateOptions
 * @property {string} template - Template file path
 * @property {string} output - Output file path
 * @property {Record<string, any>} [context] - Template variables
 */

/**
 * Generate code from template
 * @param {GenerateOptions} options - Generation options
 * @returns {Promise<void>}
 */
async function generate(options) {
  // Implementation...
}

// ❌ WRONG: TypeScript
interface GenerateOptions {
  template: string;
  output: string;
  context?: Record<string, any>;
}

async function generate(options: GenerateOptions): Promise<void> {
  // ...
}
```

**Validation**: Run `scripts/validate-docs/check-no-typescript.sh`

**See**: `docs/contributing/DOCUMENTATION.md`

---

### Documentation Validation: All Examples Must Pass

**Rule**: Every code example in documentation MUST be validated

**How**:
```bash
# Validate all documentation
./scripts/validate-docs/validate-all.sh

# Checks include:
# - No TypeScript usage
# - All internal links valid
# - All code examples execute
# - No broken image references
```

**CI Enforcement**: Documentation validation runs on every commit

**See**: `docs/contributing/VALIDATION.md`

---

## Performance Standards

### Build System: cargo make (NOT direct cargo)

**Rule**: ALWAYS use `cargo make` - NEVER direct `cargo` commands

**Why**:
- Enforces timeouts (prevents hanging builds)
- Enforces SLOs (service level objectives)
- Integrates with hooks and validation
- Consistent across team

**Examples**:
```bash
# ❌ WRONG
cargo check
cargo test
cargo clippy

# ✅ CORRECT
cargo make check     # <5s timeout
cargo make test      # All tests with proper timeout
cargo make lint      # Clippy with timeout

# Full validation
cargo make pre-commit
```

**See**: `Makefile.toml` for all available commands

---

### Andon Signals: Stop the Line on Errors

**Philosophy**: Borrowed from Toyota Production System - stop immediately when defects detected

**Signals**:
- 🔴 **RED**: Compilation errors, test failures → **STOP** and fix
- 🟡 **YELLOW**: Clippy warnings, format issues → Investigate before release
- 🟢 **GREEN**: Clean output → Continue

**Workflow**:
1. Monitor build output for signals
2. Stop work when RED signal appears
3. Investigate and fix root cause
4. Verify fix (signal cleared)
5. Continue only when GREEN

**Example**:
```bash
# Running cargo make check
    Checking ggen-core v4.0.0
error[E0277]: the trait bound `Config: Serialize` is not satisfied
 --> crates/ggen-core/src/config.rs:42:5

# 🔴 RED SIGNAL - STOP THE LINE
# Fix: Add #[derive(Serialize)] to Config struct
# Verify: cargo make check again → 🟢 GREEN
```

**See**: `docs/BEST_PRACTICES.md` (this document)

---

### Service Level Objectives (SLOs)

**Requirements**:
- First build: ≤ 15s
- Incremental build: ≤ 2s
- `cargo make check`: ≤ 5s
- RDF processing: ≤ 5s for 1k+ triples
- Template rendering: < 1ms per template
- CLI startup: ≤ 50ms

**How to Verify**:
```bash
cargo make slo-check   # Runs benchmarks and verifies SLOs
```

**Current Status**: 84% under 5s SLO (see `docs/PERFORMANCE.md`)

---

## Rust-Specific Best Practices

### Type-First Thinking

**Questions to Ask**:
1. "What can I express in types?" (before runtime values)
2. "Is this abstraction zero-cost?" (generics yes, trait objects no)
3. "What are the ownership semantics?" (explicit is better)
4. "How can I make misuse impossible?" (type safety > runtime checks)

**Example**:
```rust
// ❌ WRONG: Runtime validation
pub fn set_temperature(value: f32) {
    if value < 0.0 || value > 1.0 {
        panic!("Temperature must be between 0 and 1");
    }
    // ...
}

// ✅ BETTER: Type-level validation
pub struct Temperature(f32);

impl Temperature {
    pub fn new(value: f32) -> Result<Self, Error> {
        if value < 0.0 || value > 1.0 {
            return Err(Error::new("Temperature must be between 0 and 1"));
        }
        Ok(Self(value))
    }
}

// ✅ BEST: Zero-cost newtype with validated constructor
// Misuse is impossible after construction
```

---

### Zero-Cost Abstractions

**Prefer**:
- ✅ Generics (monomorphization, zero runtime cost)
- ✅ Stack allocation (fast, no GC)
- ✅ References (borrowing, no copying)
- ✅ Const generics (compile-time computation)

**Avoid** (unless necessary):
- ❌ Trait objects (`dyn Trait`) - dynamic dispatch overhead
- ❌ Heap allocation - slower, fragmented memory
- ❌ Cloning large structures - unnecessary copies
- ❌ Runtime type checks - compile-time is better

**Example**:
```rust
// ✅ GOOD: Zero-cost generic
fn process<T: Serialize>(data: &T) -> String {
    serde_json::to_string(data).unwrap()
}

// ❌ SLOWER: Trait object (dynamic dispatch)
fn process(data: &dyn Serialize) -> String {
    serde_json::to_string(data).unwrap()
}
```

---

### Explicit Ownership Semantics

**Rule**: Make ownership clear in function signatures

**Patterns**:
```rust
// Takes ownership (consumes value)
fn consume(data: String) { ... }

// Borrows immutably (read-only)
fn read(data: &str) { ... }

// Borrows mutably (can modify)
fn modify(data: &mut String) { ... }

// Returns owned value (caller owns result)
fn create() -> String { ... }

// Returns borrowed value (tied to input lifetime)
fn extract<'a>(data: &'a str) -> &'a str { ... }
```

**Why**: Explicit ownership prevents memory leaks and use-after-free bugs

---

## Git Workflow Best Practices

### Commit Messages: Conventional Commits

**Format**: `<type>(<scope>): <description>`

**Types**:
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `refactor`: Code restructuring (no behavior change)
- `test`: Adding/updating tests
- `chore`: Build system, dependencies

**Examples**:
```
feat(rdf): Add SPARQL caching for 5s performance improvement
fix(templates): Handle empty variable substitution correctly
docs(tutorials): Add prerequisites to quick-start guide
refactor(cli): Extract command registration into macro
test(lockfile): Add edge case tests for concurrent writes
chore(deps): Update clap to 4.5.0
```

**See**: `.github/COMMIT_CONVENTION.md`

---

### Git Hooks: Pre-Commit Validation

**What Runs**:
- Format check (`cargo fmt --check`)
- Clippy lint (`cargo clippy`)
- Unit tests (`cargo test`)

**How to Enable**:
```bash
# Install git hooks
cargo make install-hooks

# Manual pre-commit check
cargo make pre-commit
```

**NEVER** use `--no-verify` to bypass hooks (defeats purpose)

**See**: `scripts/git-hooks/pre-commit.sh`

---

## Security Best Practices

### Never Hardcode Secrets

**Rule**: All secrets MUST come from environment variables

**Examples**:
```rust
// ❌ WRONG
const API_KEY: &str = "sk-1234567890abcdef";

// ✅ CORRECT
use std::env;

fn get_api_key() -> Result<String, Error> {
    env::var("ANTHROPIC_API_KEY")
        .map_err(|_| Error::new("ANTHROPIC_API_KEY not set"))
}
```

**Validation**: Security scanning via `cargo audit` (runs in CI)

---

### Allowed Domains: Whitelist External Resources

**Configuration** (ggen.toml):
```toml
[security]
allowed_domains = [
    "schema.org",
    "w3.org",
    "ggen.dev",
    "github.com"
]
max_file_size = 104857600  # 100MB
validate_ssl = true
```

**Enforcement**: RDF engine rejects URLs not in whitelist

---

## Key Takeaways (80/20 Rule)

**Focus on these 20% of practices that deliver 80% of quality**:

1. ✅ **Chicago TDD**: Tests verify behavior, not implementation
2. ✅ **No unwrap in production**: Use `Result<T, E>` always
3. ✅ **cargo make, not cargo**: Enforces timeouts and SLOs
4. ✅ **Andon signals**: Stop immediately when RED appears
5. ✅ **Diataxis docs**: One quadrant per document
6. ✅ **JavaScript + JSDoc**: No TypeScript in examples
7. ✅ **Type-first thinking**: Express invariants in types
8. ✅ **Explicit ownership**: Clear who owns what
9. ✅ **Pre-commit hooks**: Catch issues before CI
10. ✅ **No hardcoded secrets**: Environment variables only

---

## Detailed Best Practices

This is a **quick reference**. For detailed documentation, see:

- **Contributing Guide**: `docs/contributing/`
  - Getting started with development
  - Code review process
  - Release workflow

- **Testing Guide**: `docs/contributing/TESTING.md`
  - Chicago School TDD philosophy
  - Writing effective tests
  - Coverage requirements

- **Validation Guide**: `docs/contributing/VALIDATION.md`
  - Running validation suite
  - CI pipeline details
  - Debugging validation failures

- **Architecture Docs**: `docs/architecture/`
  - System design principles
  - Component organization
  - Integration patterns

---

**Next Steps**:
- Setting up dev environment? → `docs/contributing/GETTING_STARTED.md`
- Writing tests? → `docs/contributing/TESTING.md`
- Contributing docs? → `docs/contributing/DOCUMENTATION.md`
