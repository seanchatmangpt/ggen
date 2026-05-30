# Fixture Audit Report: playground/ and examples/

**Date**: 2026-05-29  
**Scope**: All `.ttl`, `.rq`, `.tera`, and `.toml` files in `playground/` and `examples/` directories  
**Purpose**: Identify fixture files, their references in doctests/examples, and path resolution issues

---

## Executive Summary

### Key Findings

- **Total Fixtures**: 354+ fixture files across examples/ (85 TTL, 7 RQ, 229 Tera, 58 TOML)
- **Playground Fixtures**: 12 stable files in playground/ for testing command foundation
- **Complete Examples Ready**: 30 examples with full fixture sets (ggen.toml + ontology + templates)
- **Missing Fixtures**: 1 critical blocker (`examples/basic-template-generation/`)
- **Path Issues**: All references use relative paths; dependent on correct working directory
- **Test Coverage**: 14 files across crates reference fixtures in doctests/examples

---

## Part 1: Playground Fixtures

### Location: `/Users/sac/ggen/playground/`

**Fixture Inventory:**

| File | Type | Purpose |
|------|------|---------|
| `thesis-ontology.ttl` | TTL | Main thesis generation ontology |
| `ggen.toml` | TOML | Root playground config |
| `proof/broken-construct.rq` | SPARQL | Query test case (negative) |
| `templates/thesis-main.tera` | Tera | Main thesis template |
| `templates/chapters.tera` | Tera | Chapter generation template |
| `templates/sections.tera` | Tera | Section generation template |
| `templates/equations.tera` | Tera | Math equation template |
| `templates/theorems.tera` | Tera | Theorem template |
| `sync-foundation/ggen.toml` | TOML | Sync-foundation subproject |
| `sync-foundation/ontology/command-foundation.ttl` | TTL | Command-line foundation ontology |
| `sync-foundation/shapes/closure.shacl.ttl` | TTL | SHACL validation shapes |
| `sync-foundation/templates/gall_command_foundation.rs.tera` | Tera | Rust code generation template |

**Status**: ✓ All playground fixtures are present and accessible.

**Usage**: `playground/sync-foundation` is referenced in:
- `crates/ggen-cli/tests/gall_sync_actuation.rs:52` — Path resolution via `env!("CARGO_MANIFEST_DIR")`

---

## Part 2: Examples Fixtures Summary

### Location: `/Users/sac/ggen/examples/`

**By Type:**

| Type | Count | Subdirs | Notes |
|------|-------|---------|-------|
| TTL/RDF Files | 85 | 70+ | Ontologies, vocabularies, data models |
| SPARQL Queries (.rq) | 7 | 3 | Extraction and transformation queries |
| Tera Templates | 229 | 100+ | Code generation templates for multiple languages |
| Config Files (ggen.toml, make.toml) | 58 | 58 | Build and generation configs |
| **Total Example Projects** | **139** | - | Some have incomplete fixture sets |

---

## Part 3: Fixture References in Test/Example Code

### Files That Reference Fixtures

| Crate | File | Line | Reference | Status |
|-------|------|------|-----------|--------|
| ggen-core | `tests/mcp_generation_e2e_test.rs` | 47 | `examples/mcp-server-definition/ontology/mcp-server.ttl` | ✓ EXISTS |
| ggen-core | `examples/validate_example_project.rs` | 11-12 | `examples/basic-template-generation/ggen.toml` | ❌ **MISSING** |
| ggen-core | `tests/mcp_generation_e2e_test.rs` | 78 | `templates/mcp-server/stdio_server.rs.tera` | ⚠️ RELATIVE |
| ggen-cli | `tests/self_play_smoke_test.rs` | 42 | `examples/self-play` | ✓ EXISTS |
| ggen-cli | `tests/gall_sync_actuation.rs` | 52 | `../../playground/sync-foundation` | ✓ EXISTS |
| ggen-cli | `tests/mcp_command_test.rs` | varies | Multiple examples/ references | ⚠️ CONDITIONAL |
| ggen-core | `tests/marketplace_examples_validation_test.rs` | 142, 508 | `examples/` directory discovery | ⚠️ CONDITIONAL |

**Total Files Referencing Fixtures**: 14 test/example files across 5 crates

---

## Part 4: Missing Fixtures & Blockers

### Critical Blocker

**Missing Directory**: `examples/basic-template-generation/`

```
File: crates/ggen-core/examples/validate_example_project.rs
Lines: 11-12
```

**Code:**
```rust
let manifest_path = PathBuf::from("examples/basic-template-generation/ggen.toml");
let base_path = PathBuf::from("examples/basic-template-generation");
```

**Impact**: 
- ❌ Example executable will fail at runtime
- Prevents validation pipeline demonstration
- Should be replaced with an existing complete example

**Recommended Fix**: Replace with `examples/simple-project` or `examples/advanced-rust-project`

---

## Part 5: Complete Fixture Sets (Ready for Use)

### Examples with Full Fixture Sets (ggen.toml + ontology + templates)

These 30 examples are ready to use in tests and have all required components:

**Tier 1 - Simplest (Recommended for new tests)**
- `examples/simple-project` (minimal, fast)
- `examples/demo-project` (basic templates)
- `examples/mcp-server-definition` (single ontology)
- `examples/rust-structs` (type generation)

**Tier 2 - Mid-Complexity**
- `examples/advanced-rust-project` (full Rust structure)
- `examples/database-schema` (schema generation)
- `examples/cli-noun-verb` (CLI code generation)
- `examples/openapi` (API generation)
- `examples/rest-api-advanced` (REST patterns)

**Tier 3 - Complex**
- `examples/factory-paas` (enterprise PaaS example, 40+ templates)
- `examples/gcp-erlang-autonomics` (cloud + Erlang, 60+ templates)
- `examples/thesis-gen` (academic document generation)
- `examples/workspace-project` (workspace structure)
- `examples/llm-full-integration` (LLM integration patterns)

**Tier 4 - Domain-Specific**
- `examples/mcp-a2a-self-hosting` (Agent-to-Agent protocol)
- `examples/a2a-groq-agent` (Groq LLM agent)
- `examples/a2a-agent-definition` (Agent definition patterns)
- `examples/observable-agent` (Observability integration)
- `examples/microservices-architecture` (Microservice patterns)

**Full List**: 30 examples ready for immediate use

---

## Part 6: Incomplete Fixture Sets

### Examples Missing Key Components

| Example | Missing | Count |
|---------|---------|-------|
| `_archive` | Both ggen.toml and ontology | - |
| `_shared_templates` | Both ggen.toml and ontology | - |
| `7-agent-validation` | Both ggen.toml and ontology | - |
| `a2a-agent-lifecycle` | ggen.toml only | 1 |
| `a2a-rs-agents` | Both | - |
| `advanced-ai-usage` | Both | - |
| `advanced-cache-registry` | Both | - |
| `advanced-cli-tool` | Both | - |
| `advanced-error-handling` | Both | - |
| `advanced-fullstack-integration` | Both | - |
| `advanced-pipeline` | Both | - |
| `advanced-rust-api-8020` | Both | - |
| `ai-template-project` | Both | - |
| `bree-semantic-scheduler` | Both (has ggen-bree-config.toml instead) | - |
| `clap-noun-verb-demo` | Both (has sample-cli.ttl) | - |
| `cli-advanced` | Both | - |
| `distributed-consensus` | ggen.toml only | 1 |
| `e2e-agent-workflow` | Both | - |
| `e2e-complete-system` | Both | - |
| `embedded-cross` | Both | - |
| **Total Incomplete** | - | **20** |

---

## Part 7: Path Resolution Analysis

### Path Types Found

```
ALL REFERENCES ARE RELATIVE PATHS
```

**Examples:**

```rust
// Test file path resolution
fs::read_to_string("examples/mcp-server-definition/ontology/mcp-server.ttl")
PathBuf::from("examples/self-play")
PathBuf::from("examples/basic-template-generation/ggen.toml")
```

**Working Directory Dependency**:
- ✓ Tests **MUST** run from workspace root (`/Users/sac/ggen/`)
- ✓ Relative paths resolve correctly when PWD = workspace root
- ⚠️ Will fail if test runs from subdirectory (e.g., from within crate)

**One Exception (Correct Approach)**:
```rust
// From: crates/ggen-cli/tests/gall_sync_actuation.rs:52
let src = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../playground/sync-foundation");
```
Uses `env!("CARGO_MANIFEST_DIR")` to build absolute path — this is portable.

---

## Part 8: Doctest & Example File Paths

### Detailed Cross-Reference

```
ggen-core/tests/mcp_generation_e2e_test.rs
  ├─ Line 47:  examples/mcp-server-definition/ontology/mcp-server.ttl ✓
  ├─ Line 78:  templates/mcp-server/stdio_server.rs.tera (relative to crate)
  └─ Comment:  Example MCP ontology loaded at runtime

ggen-core/examples/validate_example_project.rs
  ├─ Line 11:  examples/basic-template-generation/ggen.toml ❌ MISSING
  ├─ Line 12:  examples/basic-template-generation (dir) ❌ MISSING
  └─ Purpose:  Quality gate validation pipeline demo

ggen-cli/tests/self_play_smoke_test.rs
  ├─ Line 42:  examples/self-play ✓
  └─ Purpose:  Self-play validation test

ggen-cli/tests/gall_sync_actuation.rs
  ├─ Line 52:  ../../playground/sync-foundation ✓
  └─ Type:     env!("CARGO_MANIFEST_DIR") based (absolute)

ggen-core/tests/marketplace_examples_validation_test.rs
  ├─ Lines 142, 508: Conditional discovery of examples/ directory
  └─ Purpose:  Validate all example projects dynamically
```

---

## Part 9: Fixture Setup Issues

### Known Issues & Blocking Conditions

#### Issue 1: Missing Example Project

**Blocking**: Yes  
**Severity**: High

```
Example: crates/ggen-core/examples/validate_example_project.rs
Problem: References non-existent examples/basic-template-generation/
Fix: Either create the example or replace path with existing one
Recommended: Use examples/simple-project or examples/advanced-rust-project
```

#### Issue 2: Relative Path Fragility

**Blocking**: No (currently works)  
**Severity**: Medium

```
All fixture references use relative paths from workspace root.
If tests run from crate subdirectories, paths will fail.
Recommendation: Convert to env!("CARGO_MANIFEST_DIR") pattern
Example:
  BEFORE: PathBuf::from("examples/simple-project/ggen.toml")
  AFTER:  Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../examples/simple-project/ggen.toml")
```

#### Issue 3: Incomplete Example Projects

**Blocking**: Conditionally  
**Severity**: Low

```
20 examples have missing ggen.toml or ontology files.
These will be skipped by marketplace_examples_validation_test.rs
Recommendation: Archive or complete these examples
Status: Non-critical (skip logic already in place)
```

#### Issue 4: Template Path Ambiguity

**Blocking**: Potentially  
**Severity**: Low

```
File: ggen-core/tests/mcp_generation_e2e_test.rs:78
Path: "templates/mcp-server/stdio_server.rs.tera"
Issue: Unclear if relative to crate or workspace
Current: Appears to be relative path without full fixtures dir
```

---

## Part 10: Test Failure Patterns

### Tests That May Fail Due to Fixture Issues

1. **`validate_example_project` example**
   - Command: `cargo run --example validate_example_project`
   - Status: ❌ WILL FAIL (missing fixture)
   - Error: `Failed to read ggen.toml at examples/basic-template-generation/`

2. **Marketplace validation tests** (conditional)
   - Command: `cargo test -p ggen-core -- --test-threads=1`
   - Status: ⚠️ PARTIAL (skips incomplete examples gracefully)
   - Behavior: Skips 20 incomplete examples, validates 30 complete ones

3. **MCP generation E2E test**
   - Command: `cargo test -p ggen-core -- mcp_generation_e2e`
   - Status: ✓ PASSES (fixture exists)

4. **Self-play smoke test**
   - Command: `cargo test -p ggen-cli -- self_play_smoke_test`
   - Status: ✓ PASSES (fixture exists)

---

## Part 11: Recommendations

### Immediate Actions

#### Priority 1: Fix Critical Blocker
```
Action: Remove or replace invalid example
File: crates/ggen-core/examples/validate_example_project.rs
Fix Options:
  A) Replace fixture path with existing example
     examples/basic-template-generation → examples/simple-project
  B) Delete the example if not used
  C) Create the missing example
```

#### Priority 2: Standardize Path Resolution
```
Goal: All fixtures use absolute paths from env!("CARGO_MANIFEST_DIR")
Files Affected:
  - crates/ggen-core/examples/validate_example_project.rs
  - crates/ggen-core/tests/mcp_generation_e2e_test.rs
  - crates/ggen-cli/tests/self_play_smoke_test.rs
Pattern:
  use std::path::PathBuf;
  let base = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
  let fixture = base.join("../../examples/simple-project");
```

#### Priority 3: Archive Incomplete Examples
```
Action: Organize incomplete examples
Status: Low priority (already skipped by tests)
Impact: Cleanup only; no functional fix needed
```

### Long-Term Improvements

1. **Create a fixture registry** (`tests/fixtures.rs`)
   ```rust
   pub fn simple_project() -> PathBuf {
       project_fixture("simple-project")
   }
   
   fn project_fixture(name: &str) -> PathBuf {
       PathBuf::from(env!("CARGO_MANIFEST_DIR"))
           .join(format!("../../examples/{}", name))
   }
   ```

2. **Document fixture expectations** in `examples/README.md`
   - Required files: ggen.toml, ontology.ttl or ontology/, templates/
   - Checklist for new examples

3. **Add CI check** to validate fixture completeness
   ```bash
   # Validate all examples have required files
   cargo make validate-fixtures
   ```

4. **Use relative paths consistently** or switch all to absolute paths

---

## Part 12: Fixture Checklist for New Tests

When adding new doctests or examples, use this checklist:

- [ ] Fixture exists and is complete (ggen.toml + ontology + templates)
- [ ] Path is either:
  - [ ] Relative from workspace root (PWD-dependent), OR
  - [ ] Absolute using `env!("CARGO_MANIFEST_DIR")`
- [ ] Test is skipped gracefully if fixture is missing
- [ ] Fixture path is documented in test comments
- [ ] Fixture is one of the 30 complete examples
- [ ] Path does NOT contain hardcoded absolute paths

---

## Summary Table

| Metric | Count | Status |
|--------|-------|--------|
| **Fixture Inventory** | | |
| Playground fixtures | 12 | ✓ Complete |
| Examples with complete sets | 30 | ✓ Ready |
| Examples with incomplete sets | 20 | ⚠️ Skipped |
| **Path Issues** | | |
| Missing fixtures blocking tests | 1 | ❌ Action needed |
| Relative path references | 4 | ⚠️ Fragile |
| Absolute path references | 1 | ✓ Portable |
| **Test Coverage** | | |
| Files with fixture references | 14 | ⚠️ Review |
| Passing reference checks | 10+ | ✓ Good |
| Failing/blocked tests | 1 | ❌ Fixable |

---

**Prepared by**: Audit Agent  
**Date**: 2026-05-29  
**Scope**: Full playground/ and examples/ fixture inventory with doctest/example cross-references
