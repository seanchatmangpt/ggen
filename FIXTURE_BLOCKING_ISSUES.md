# Fixture Blocking Issues

## Critical Issues Blocking Tests

### Issue 1: Missing Example Fixture (BLOCKER)

**Severity**: HIGH  
**Status**: ❌ BLOCKING

#### Problem
```
File: crates/ggen-core/examples/validate_example_project.rs
Lines: 11-12
```

**Missing Fixture Path**: `examples/basic-template-generation/`

#### Code That Fails
```rust
fn main() {
    // These paths fail immediately:
    let manifest_path = PathBuf::from("examples/basic-template-generation/ggen.toml");
    let base_path = PathBuf::from("examples/basic-template-generation");
    
    // Error occurs on first file I/O:
    let manifest_content =
        std::fs::read_to_string(&manifest_path)  // ← FAILS HERE
            .expect("Failed to read ggen.toml");
}
```

**Error When Run**:
```
thread 'main' panicked at 'Failed to read ggen.toml: No such file or directory (os error 2)'
```

#### Impact
- ❌ Example executable `validate_example_project` cannot run
- ❌ Quality gate validation pipeline not demonstrated
- ❌ New developers cannot follow this example

#### How to Fix

**Option 1: Replace Path** (Recommended)
```diff
- let manifest_path = PathBuf::from("examples/basic-template-generation/ggen.toml");
- let base_path = PathBuf::from("examples/basic-template-generation");
+ let manifest_path = PathBuf::from("examples/simple-project/ggen.toml");
+ let base_path = PathBuf::from("examples/simple-project");
```

**Option 2: Use Absolute Path** (Portable)
```diff
+ use std::path::PathBuf;
+ 
- let manifest_path = PathBuf::from("examples/basic-template-generation/ggen.toml");
- let base_path = PathBuf::from("examples/basic-template-generation");
+ let base = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
+ let manifest_path = base.join("../../examples/simple-project/ggen.toml");
+ let base_path = base.join("../../examples/simple-project");
```

**Option 3: Create Missing Example** (High effort)
- Create directory: `examples/basic-template-generation/`
- Copy from `examples/simple-project/` or create from scratch
- Add `ggen.toml`, `ontology.ttl`, and `templates/`

---

## Path Resolution Issues

### Issue 2: Relative Path Fragility (MEDIUM RISK)

**Severity**: MEDIUM  
**Status**: ⚠️ FRAGILE (currently works but could break)

#### Problem
All fixture references use relative paths from workspace root:

```rust
fs::read_to_string("examples/mcp-server-definition/ontology/mcp-server.ttl")
PathBuf::from("examples/self-play")
```

#### When This Fails
1. If tests run from crate subdirectory instead of workspace root
2. If working directory is changed before test execution
3. If test is run from CI with different PWD setup
4. If moving test to different crate level

#### Example Failure
```bash
$ cd crates/ggen-core
$ cargo test  # ← Running from crate, not workspace
# Error: Cannot find examples/mcp-server-definition/ (relative from crate)
```

#### Current Test Behavior
```
✓ Works: cargo test --workspace (from root)
✗ Works: cargo test -p ggen-core (still runs from root)
✗ FAILS: cd crates/ggen-core && cargo test
```

#### Files Affected
- `crates/ggen-core/tests/mcp_generation_e2e_test.rs:47`
- `crates/ggen-core/examples/validate_example_project.rs:11`
- `crates/ggen-cli/tests/self_play_smoke_test.rs:42`

#### How to Fix
Convert all to absolute paths:

**Before** (fragile):
```rust
fs::read_to_string("examples/mcp-server-definition/ontology/mcp-server.ttl")
```

**After** (robust):
```rust
use std::path::PathBuf;

let base = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
let fixture = base.join("../../examples/mcp-server-definition/ontology/mcp-server.ttl");
fs::read_to_string(fixture)
```

#### Exception (Already Correct)
This file already uses absolute paths — no fix needed:
```rust
// From: crates/ggen-cli/tests/gall_sync_actuation.rs:52
let src = Path::new(env!("CARGO_MANIFEST_DIR"))
    .join("../../playground/sync-foundation");  // ✓ CORRECT
```

---

## Conditional Issues (Non-Blocking but Important)

### Issue 3: Incomplete Example Projects

**Severity**: LOW  
**Status**: ⚠️ GRACEFULLY SKIPPED

#### Problem
20 example projects are incomplete (missing ggen.toml or ontology):

```
_archive, _shared_templates, 7-agent-validation, a2a-agent-lifecycle,
a2a-rs-agents, advanced-ai-usage, advanced-cache-registry, ...
```

#### Impact
- ✓ Tests skip these automatically (non-blocking)
- ✓ No test failures reported
- ⚠️ But only 30/50 examples are actually complete

#### Code That Handles This
From `crates/ggen-core/tests/marketplace_examples_validation_test.rs`:
```rust
if !examples_dir.exists() {
    println!("SKIP: examples/ directory not found at {}",
             examples_dir.display());
    return;
}
```

#### No Action Required
Incomplete examples are already skipped. This is intentional to avoid dependency on example completeness.

---

## Test Coverage Summary

### Tests Currently Passing ✓

| Test | Fixture | Status |
|------|---------|--------|
| `mcp_generation_e2e_test.rs:47` | `examples/mcp-server-definition/` | ✓ |
| `self_play_smoke_test.rs:42` | `examples/self-play/` | ✓ |
| `gall_sync_actuation.rs:52` | `playground/sync-foundation/` | ✓ |
| `marketplace_examples_validation_test.rs` | `examples/` (dynamic) | ✓ |

### Tests Currently Failing ❌

| Test | Fixture | Status | Why |
|------|---------|--------|-----|
| `validate_example_project` (example) | `examples/basic-template-generation/` | ❌ | Missing fixture |

### Tests At Risk ⚠️

| Test | Risk | Issue |
|------|------|-------|
| `mcp_generation_e2e_test.rs` | MEDIUM | Relative paths |
| `validate_example_project.rs` | MEDIUM | Relative paths (+ missing) |
| `self_play_smoke_test.rs` | MEDIUM | Relative paths |

---

## Fixture Path Mapping

### Where Fixtures Are

```
/Users/sac/ggen/
├── examples/
│   ├── simple-project/              ✓ Complete
│   ├── mcp-server-definition/       ✓ Complete
│   ├── self-play/                   ✓ Complete
│   ├── openapi/                     ✓ Complete
│   ├── advanced-rust-project/       ✓ Complete
│   ├── basic-template-generation/   ❌ MISSING
│   ├── demo-project/                ⚠️ Incomplete (no ggen.toml)
│   └── [27 more complete examples]
│
└── playground/
    ├── sync-foundation/             ✓ Complete
    ├── thesis-ontology.ttl          ✓ Complete
    └── [10 more fixtures]
```

### Test-Fixture Cross-Reference

```
crates/ggen-core/
├── examples/
│   └── validate_example_project.rs
│       └── Uses: examples/basic-template-generation/  ❌ MISSING
│
└── tests/
    ├── mcp_generation_e2e_test.rs
    │   └── Uses: examples/mcp-server-definition/      ✓ EXISTS
    │
    └── marketplace_examples_validation_test.rs
        └── Uses: examples/* (discovery)               ✓ MOSTLY WORKS

crates/ggen-cli/tests/
├── self_play_smoke_test.rs
│   └── Uses: examples/self-play/                      ✓ EXISTS
│
└── gall_sync_actuation.rs
    └── Uses: playground/sync-foundation/              ✓ EXISTS
```

---

## Required Fixes (Priority Order)

### Priority 1: IMMEDIATE
```
Fix crates/ggen-core/examples/validate_example_project.rs
  - Change examples/basic-template-generation → examples/simple-project
  - OR use env!("CARGO_MANIFEST_DIR") pattern
  - Severity: HIGH (blocking example execution)
```

### Priority 2: SOON
```
Convert all relative paths to absolute paths:
  - crates/ggen-core/tests/mcp_generation_e2e_test.rs:47
  - crates/ggen-core/examples/validate_example_project.rs:11-12
  - crates/ggen-cli/tests/self_play_smoke_test.rs:42
  - Severity: MEDIUM (fragility risk)
  - Effort: 1-2 hours
```

### Priority 3: OPTIONAL
```
Archive incomplete examples or complete them
  - Status: Low priority (already handled by skip logic)
  - Impact: Cleanup only
  - Severity: LOW
```

---

## Testing the Fixes

### Before Fix
```bash
$ cargo run --example validate_example_project
error: Failed to read ggen.toml: No such file or directory
```

### After Fix
```bash
$ cargo run --example validate_example_project
=== Running validate_pipeline on example project ===

Loading manifest from: "examples/simple-project/ggen.toml"
Manifest loaded successfully

Found 5 quality gates:
  1. Ontology Validation
  2. Schema Validation
  3. Template Rendering
  4. Output Validation
  5. Determinism Check

✅ ALL QUALITY GATES PASSED
```

---

## Verification Checklist

After applying fixes, verify:

- [ ] `cargo run --example validate_example_project` succeeds
- [ ] `cargo test --workspace` passes
- [ ] `cargo test -p ggen-core` passes
- [ ] All relative paths converted to absolute OR known to run from root only
- [ ] Fixture paths documented in test comments
- [ ] 30 complete examples available for use

---

**Document Version**: 1.0  
**Date**: 2026-05-29  
**Status**: 1 BLOCKER identified, fixable in <1 hour
