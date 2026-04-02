# validate_pipeline_test.rs - Test Report

## Test Overview
**File:** `/Users/sac/ggen/crates/ggen-a2a-mcp/tests/validate_pipeline_test.rs`
**Purpose:** Chicago TDD tests for the `validate_pipeline` MCP tool
**Status:** Test exists but cannot run due to compilation errors in other tests in the same crate

## Test Structure

### Test 1: `test_validate_pipeline_params_with_manifest_path`
**Purpose:** Validate parameter handling with manifest_path provided
```rust
let params = serde_json::json!({
    "manifest_path": "../examples/basic-template-generation/ggen.toml"
});
```
**Expected:** Parameters are valid JSON object with manifest_path

### Test 2: `test_validate_pipeline_params_without_manifest_path`
**Purpose:** Validate default behavior (no manifest_path provided)
```rust
let params = serde_json::json!({});
```
**Expected:** Parameters default to `./ggen.toml`

### Test 3: `test_validate_pipeline_example_manifest_exists`
**Purpose:** Verify test fixture exists
**Path:** `/Users/sac/ggen/examples/basic-template-generation/ggen.toml`
**Expected:** Manifest file exists

### Test 4: `test_quality_gate_runner_can_be_created`
**Purpose:** Verify QualityGateRunner initialization and gate count
**Expected Output:**
```
Found 11 checkpoints:
  1: Manifest Schema
  2: Ontology Dependencies
  3: SPARQL Validation
  4: Template Validation
  5: File Permissions
  6: Rule Validation
  7: Lean Six Sigma: Define
  8: Lean Six Sigma: Measure
  9: Lean Six Sigma: Analyze
  10: Lean Six Sigma: Improve
  11: Lean Six Sigma: Control
```

## Quality Gates Checked

The `validate_pipeline` MCP tool validates **11 quality gates** (not 6 as the tool description states):

### Core Gates (1-6)
1. **Manifest Schema** - ggen.toml structure is valid
2. **Ontology Dependencies** - All .ttl files exist, no circular imports
3. **SPARQL Validation** - All queries have valid syntax
4. **Template Validation** - All templates exist and have valid Tera syntax
5. **File Permissions** - Output directory is writable
6. **Rule Validation** - All rules reference existing templates/queries

### Lean Six Sigma DMAIC Gates (7-11)
7. **Define Phase** - Project goals and stakeholder requirements defined
8. **Measure Phase** - Metrics and data collection plan established
9. **Analyze Phase** - Root cause analysis completed
10. **Improve Phase** - Solutions implemented and validated
11. **Control Phase** - Control plan established for sustainability

## Expected MCP Tool Output

When `validate_pipeline` runs successfully, it returns JSON like:

```json
{
  "passed": true,
  "errors": [],
  "warnings": [],
  "duration_ms": 123,
  "gates": [
    {"name": "Manifest Schema", "status": "passed"},
    {"name": "Ontology Dependencies", "status": "passed"},
    {"name": "SPARQL Validation", "status": "passed"},
    {"name": "Template Validation", "status": "passed"},
    {"name": "File Permissions", "status": "passed"},
    {"name": "Rule Validation", "status": "passed"}
  ]
}
```

**Note:** The tool only reports the 6 core gates in JSON output, but actually runs all 11 gates internally.

## Compilation Blockers

The test cannot currently run due to compilation errors in other files:
- `crates/ggen-a2a-mcp/tests/phase_c_d_self_play_standalone.rs` - Type annotation errors
- `crates/ggen-a2a-mcp/src/state/togaf_state.rs` - Method call errors on Result types
- `crates/ggen-a2a-mcp/src/bin/test_mcp_generate_sync.rs` - Private method access errors
- `crates/ggen-a2a-mcp/src/bin/test_validate_pipeline.rs` - Missing tracing_subscriber dependency

## How to Run Test (Once Fixed)

```bash
cargo test -p ggen-a2a-mcp validate_pipeline_test -- --nocapture
```

## Test File Location
- **Test:** `/Users/sac/ggen/crates/ggen-a2a-mcp/tests/validate_pipeline_test.rs`
- **Tool Implementation:** `/Users/sac/ggen/crates/ggen-a2a-mcp/src/ggen_server.rs` (line ~1000)
- **Quality Gates:** `/Users/sac/ggen/crates/ggen-core/src/poka_yoke/quality_gates.rs`

## Summary

The `validate_pipeline_test.rs` file contains 4 Chicago TDD tests that verify:
1. Parameter validation (with and without manifest_path)
2. Test fixture existence
3. QualityGateRunner initialization
4. Correct count of quality gates (11 total)

**Actual Quality Gate Count:** 11 gates (6 core + 5 Lean Six Sigma)
**Tool Description States:** 6 gates (incorrect - only reports core gates)

The test would show output listing all 11 checkpoints by name and confirming the count matches expectations.
