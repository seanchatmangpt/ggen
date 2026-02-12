<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [AI Testing Patterns - Quick Reference](#ai-testing-patterns---quick-reference)
  - [Critical Statistics](#critical-statistics)
  - [Quick Testing Checklist for AI Code](#quick-testing-checklist-for-ai-code)
  - [Essential Test Commands (Add to ggen)](#essential-test-commands-add-to-ggen)
  - [Top 5 AI Testing Patterns](#top-5-ai-testing-patterns)
    - [1. Proof-Based Workflow](#1-proof-based-workflow)
    - [2. Mutation Testing (60%+ score)](#2-mutation-testing-60-score)
    - [3. Property-Based Testing](#3-property-based-testing)
    - [4. Security Validation](#4-security-validation)
    - [5. Snapshot Testing](#5-snapshot-testing)
  - [Priority Security Checks](#priority-security-checks)
    - [CWE Patterns to Target (2026 Data)](#cwe-patterns-to-target-2026-data)
    - [Automated Detection](#automated-detection)
  - [Test Organization Structure](#test-organization-structure)
  - [Quality Metrics (2026 Standards)](#quality-metrics-2026-standards)
  - [CI/CD Quality Gates](#cicd-quality-gates)
  - [Recommended Tools](#recommended-tools)
    - [Core Stack (Already in ggen)](#core-stack-already-in-ggen)
    - [Add for 2026](#add-for-2026)
  - [Common AI Code Issues](#common-ai-code-issues)
    - [Issue 1: Missing Authorization](#issue-1-missing-authorization)
    - [Issue 2: Hardcoded Secrets](#issue-2-hardcoded-secrets)
    - [Issue 3: Unvalidated Input](#issue-3-unvalidated-input)
  - [Test-Driven AI Development Workflow](#test-driven-ai-development-workflow)
  - [Meta TestGen-LLM Pattern](#meta-testgen-llm-pattern)
  - [EQS Metric (2026 Composite)](#eqs-metric-2026-composite)
  - [Implementation Priority](#implementation-priority)
    - [Week 1-2: Foundation](#week-1-2-foundation)
    - [Week 3-4: Enhancement](#week-3-4-enhancement)
    - [Week 5-6: Advanced](#week-5-6-advanced)
  - [Useful Commands](#useful-commands)
  - [Resources](#resources)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# AI Testing Patterns - Quick Reference

**Last Updated:** 2026-02-08

## Critical Statistics

- **96%** of developers acknowledge AI-generated code has correctness issues
- **48%** consistently verify AI-generated code (verification crisis)
- **3x** more security vulnerabilities in AI-assisted code
- **2x** rate of hardcoded credentials in AI code
- **60-70%** minimum mutation score recommended (vs traditional coverage)
- **84.8%** solve rate achieved with proper AI testing patterns

---

## Quick Testing Checklist for AI Code

```bash
# Before accepting AI-generated code:
[ ] Code compiles and type-checks
[ ] Unit tests pass (Chicago TDD style)
[ ] No hardcoded secrets detected
[ ] Authorization checks present
[ ] Input validation comprehensive
[ ] Property tests pass (invariants hold)
[ ] Mutation score ≥60%
[ ] Snapshot tests match expected output
[ ] Security scan clean
[ ] Human review of business logic
```

---

## Essential Test Commands (Add to ggen)

```bash
# Mutation testing (NEW - validate test quality)
cargo make test-mutation           # Run mutation tests
cargo make test-mutation-check     # Enforce 60%+ score

# AI-specific validation (NEW)
cargo make test-ai-validation      # Hardcoded secrets, auth, etc.

# Property-based testing (EXISTING - expand)
cargo make test-proptest           # Property tests

# Snapshot testing (NEW - deterministic outputs)
cargo make test-snapshot           # Run snapshot tests
cargo make test-snapshot-review    # Review changes

# Comprehensive suite
cargo make test-comprehensive      # All test types

# Quality metrics
cargo make test-quality-metrics    # Generate metrics report
```

---

## Top 5 AI Testing Patterns

### 1. Proof-Based Workflow
```rust
pub struct AICodeProof {
    pub intent: String,              // What/why in 1-2 sentences
    pub test_evidence: Vec<TestResult>,  // Tests passed
    pub ai_contribution: AIRole,     // Risk level
    pub review_areas: Vec<String>,   // Human focus
}
```

### 2. Mutation Testing (60%+ score)
```bash
# Validates tests catch real bugs, not just coverage
cargo mutants --workspace --output target/mutants
```

### 3. Property-Based Testing
```rust
proptest! {
    #[test]
    fn prop_rdf_roundtrip(triple in any_rdf_triple()) {
        let serialized = serialize_triple(&triple);
        let parsed = parse_triple(&serialized).unwrap();
        prop_assert_eq!(triple, parsed);
    }
}
```

### 4. Security Validation
```rust
#[test]
fn test_no_hardcoded_secrets() {
    let patterns = vec![
        r"(?i)api[_-]?key\s*=\s*['\"][^'\"]+['\"]",
        r"(?i)password\s*=\s*['\"][^'\"]+['\"]",
    ];
    // Scan all source files
}
```

### 5. Snapshot Testing
```rust
#[test]
fn test_deterministic_output() {
    let output = generate_from_template(&context);
    assert_snapshot!(output);  // Exact match required
}
```

---

## Priority Security Checks

### CWE Patterns to Target (2026 Data)
1. **SQL Injection (CWE-89)** - AI often generates unsafe queries
2. **Cryptographic Failures (CWE-327)** - Weak or missing crypto
3. **Cross-Site Scripting (CWE-79)** - Input validation gaps
4. **Log Injection (CWE-117)** - Unsanitized log outputs

### Automated Detection
```bash
#!/bin/bash
# scripts/detect-ai-vulnerabilities.sh

# Hardcoded secrets (2x rate in AI code)
grep -rE "(api[_-]?key|password|secret)\s*=\s*['\"][^'\"]+['\"]" crates/*/src/

# Missing authorization (common AI omission)
grep -rL "check_permission\|authorize\|require_auth" crates/*/src/handlers/

# Unvalidated inputs
grep -rL "validate\|sanitize" crates/*/src/api/
```

---

## Test Organization Structure

```
crates/ggen-core/
├── tests/
│   ├── chicago_tdd/          # State-based, real dependencies
│   ├── property_based/       # Invariants, round-trips
│   ├── snapshot/             # Deterministic outputs
│   ├── security/             # AI-specific vulnerabilities
│   ├── performance/          # SLO validation
│   └── ai_generated/         # NEW: AI code validation
├── benches/                  # Criterion performance
└── Cargo.toml
```

---

## Quality Metrics (2026 Standards)

```yaml
targets:
  line_coverage: 80%          # Standard
  mutation_score: 60%         # NEW: Critical
  test_pass_rate: 100%        # Required
  hardcoded_secrets: 0        # Zero tolerance
  build_time: <15s            # First build
  test_time: <30s             # Full suite
```

---

## CI/CD Quality Gates

```yaml
gates:
  - compilation: "Must compile"
  - formatting: "cargo fmt --check"
  - linting: "clippy -D warnings"
  - unit_tests: "80%+ pass rate"
  - coverage: "80%+ line coverage"
  - mutation: "60%+ mutation score"
  - security: "cargo audit clean"
  - ai_validation: "No hardcoded secrets"
  - performance: "SLOs met"
```

---

## Recommended Tools

### Core Stack (Already in ggen)
- `proptest` - Property-based testing
- `insta` - Snapshot testing
- `criterion` - Benchmarking
- `testcontainers` - Integration testing
- `chicago-tdd-tools` - Chicago TDD patterns

### Add for 2026
- `cargo-mutants` - Mutation testing (CRITICAL)
- `cargo-llvm-cov` - Coverage reporting
- `semgrep` - Security scanning
- `arnica-sast` - AI-aware SAST (optional)

---

## Common AI Code Issues

### Issue 1: Missing Authorization
```rust
// ❌ AI-generated (often forgets auth)
pub async fn delete_user(id: i32) -> Result<()> {
    database.delete_user(id).await
}

// ✅ Fixed with authorization
pub async fn delete_user(auth: &Auth, id: i32) -> Result<()> {
    auth.require_permission("user:delete")?;
    database.delete_user(id).await
}
```

### Issue 2: Hardcoded Secrets
```rust
// ❌ AI-generated (2x rate)
const API_KEY: &str = "sk-1234567890abcdef";

// ✅ Fixed with environment variables
fn api_key() -> Result<String> {
    env::var("API_KEY")
        .map_err(|_| Error::MissingApiKey)
}
```

### Issue 3: Unvalidated Input
```rust
// ❌ AI-generated (skips validation)
pub fn execute_query(query: &str) -> Result<Vec<Row>> {
    database.execute(query)  // SQL injection!
}

// ✅ Fixed with validation
pub fn execute_query(query: &str) -> Result<Vec<Row>> {
    validate_query_syntax(query)?;
    sanitize_query(query)?;
    database.execute(query)
}
```

---

## Test-Driven AI Development Workflow

```bash
# 1. Write test first (human)
vim crates/ggen-core/tests/feature_test.rs

# 2. Verify test fails (RED)
cargo make test-unit

# 3. AI generates implementation
# Provide: test + context + requirements
# Receive: implementation

# 4. Verify test passes (GREEN)
cargo make test-unit

# 5. AI-specific validation (NEW)
cargo make test-ai-validation      # Hardcoded secrets, auth
cargo make test-mutation-check     # Test quality ≥60%
cargo make test-security           # Security audit

# 6. Refactor (maintain GREEN)
cargo make pre-commit
```

---

## Meta TestGen-LLM Pattern

**Results:** 75% build rate, 57% pass rate, 25% coverage increase

```rust
pub struct TestGenerator {
    llm: LLMClient,
}

impl TestGenerator {
    pub async fn generate_tests(&self, code: &str) -> Result<String> {
        let prompt = format!(
            "Generate comprehensive Rust tests using Chicago TDD:\n\n\
             Code: {}\n\n\
             - Use AAA pattern (Arrange/Act/Assert)\n\
             - Test happy path + error cases\n\
             - Include edge cases\n\
             - Real dependencies (no mocking)\n\
             - Verify observable outputs",
            code
        );
        self.llm.generate(&prompt).await
    }
}
```

---

## EQS Metric (2026 Composite)

**Early Quality Score = Coverage + Mutation Score + Method-Scope Coverage**

```rust
pub struct EarlyQualityScore {
    pub line_coverage: f64,        // 80%+ target
    pub mutation_score: f64,       // 60%+ target
    pub method_coverage: f64,      // NEW: Method-level
    pub eqs: f64,                  // Composite score
}

impl EarlyQualityScore {
    pub fn grade(&self) -> Grade {
        match self.eqs {
            x if x >= 0.85 => Grade::Gold,    // Excellent
            x if x >= 0.70 => Grade::Silver,  // Good
            x if x >= 0.60 => Grade::Bronze,  // Acceptable
            _ => Grade::NeedsWork,
        }
    }
}
```

---

## Implementation Priority

### Week 1-2: Foundation
1. Add `cargo-mutants` to dev-dependencies
2. Create `tests/ai_generated/` directories
3. Implement hardcoded secret detection
4. Add mutation testing to Makefile.toml
5. Setup snapshot testing for RDF outputs

### Week 3-4: Enhancement
6. Generate property tests for RDF operations
7. Add mutation score quality gate (60%+)
8. Create AI verification test suite
9. Integrate security scanning
10. Update CI/CD with new gates

### Week 5-6: Advanced
11. Implement formal contracts for SPARQL
12. Add confidence-level testing
13. Create agent swarm integration tests
14. Setup continuous benchmarking
15. Generate quality metrics dashboard

---

## Useful Commands

```bash
# Generate this report
cargo make metrics-status

# Check mutation score
cargo mutants --workspace --json | jq '.mutation_score'

# Find hardcoded secrets
grep -rE "(api[_-]?key|password|secret)\s*=\s*['\"][^'\"]+['\"]" crates/

# Coverage report
cargo llvm-cov --workspace --html

# Security audit
cargo audit --deny warnings && semgrep --config=auto crates/

# Full validation
cargo make test-comprehensive
```

---

## Resources

- Full Report: `/home/user/ggen/docs/research/AI_TESTING_PATTERNS_2026.md`
- Makefile Tasks: `/home/user/ggen/Makefile.toml`
- Chicago TDD: `crates/ggen-cli/tests/chicago_tdd_*.rs`
- Property Tests: `crates/ggen-ai/tests/dspy_property_tests.rs`

---

**Next Steps:**
1. Review full research report
2. Add mutation testing to project
3. Implement AI-specific validation tests
4. Update CI/CD quality gates
5. Monitor mutation score metrics

**Key Takeaway:** Test quality matters more than test quantity. Mutation testing (60%+ score) is the 2026 standard for validating that tests actually catch bugs.
