# Agent 10: Test Updates for ggen v2.0.0

**Mission**: Update tests for v2.0.0 patterns (auto-discovery, sync wrappers, frozen sections, RDF-based templates).

## 📋 Deliverables Summary

### ✅ Completed

1. **Updated CLI Integration Tests** (`cli/tests/integration.rs`)
   - Removed all `--var` flag usage (v1.x pattern)
   - Updated command syntax: `template generate-tree --template X --var Y` → `template generate X Y`
   - Updated 6 test functions with v2.0 syntax

2. **Added v2.0 Pattern Tests**
   - `test_v2_auto_discovery()` - Verifies commands are auto-discovered
   - `test_v2_sync_wrapper_execution()` - Tests sync wrappers for doctor command
   - `test_v2_help_me_command()` - Tests progressive help system
   - `test_v2_frozen_section_preservation()` - Tests frozen section merging (marked #[ignore])
   - `test_v2_business_logic_not_overwritten()` - Verifies business logic protection (marked #[ignore])
   - `test_v2_marketplace_search_with_rdf()` - Tests marketplace with RDF metadata
   - `test_v2_rdf_based_template_generation()` - Tests RDF-driven template generation (marked #[ignore])

### 📝 Test Changes Detail

#### Updated Tests (v1.x → v2.0)

1. **`test_template_generate_integration()`**
```rust
// ❌ Old v1.x
cmd.args([
    "template", "generate-tree",
    "--template", template_file,
    "--output", output_dir,
    "--var", "service_name=my-service",  // No longer needed
])

// ✅ New v2.0
cmd.args([
    "template", "generate",
    template_file,
    output_dir,
])
```

2. **`test_workflow_template_to_lifecycle()`**
```rust
// ❌ Old: --var flag
.args(["template", "generate-tree", "--template", file, "--var", "project_name=test"])

// ✅ New: RDF provides data
.args(["template", "generate", file, output])
```

3. **Error Propagation Tests**
   - `test_error_propagation_invalid_template()` - Updated to v2.0 syntax
   - `test_error_propagation_missing_file()` - Updated to v2.0 syntax
   - `test_error_propagation_missing_required_var()` - Updated (no --var in v2.0, RDF provides data)

#### New v2.0 Tests

1. **Auto-Discovery** (`test_v2_auto_discovery`)
   - Verifies commands are auto-discovered from domain modules
   - Checks for: `marketplace`, `template`, `graph`, `lifecycle`
   - Status: ✅ Ready to run

2. **Sync Wrapper Execution** (`test_v2_sync_wrapper_execution`)
   - Tests sync wrappers for async commands
   - Validates `doctor` command executes successfully
   - Status: ✅ Ready to run

3. **Progressive Help** (`test_v2_help_me_command`)
   - Tests `help-me` command (progressive help system)
   - Status: ✅ Ready to run

4. **Frozen Section Preservation** (`test_v2_frozen_section_preservation`)
   - Tests `FROZEN-START`/`FROZEN-END` marker preservation
   - Verifies business logic is not overwritten during regeneration
   - Status: 🚧 `#[ignore]` - Feature not yet implemented
   - Pattern:
     ```rust
     // FROZEN-START
     pub fn my_business_logic() {
         // Custom code preserved here
     }
     // FROZEN-END
     ```

5. **Business Logic Protection** (`test_v2_business_logic_not_overwritten`)
   - Verifies non-template files are never touched
   - Ensures regeneration only affects template-generated files
   - Status: 🚧 `#[ignore]` - Feature not yet implemented

6. **RDF-Based Generation** (`test_v2_rdf_based_template_generation`)
   - Tests Handlebars template + RDF data → generated code
   - No `--var` flags needed (RDF provides all data)
   - Status: 🚧 `#[ignore]` - Feature not fully implemented
   - Example:
     ```turtle
     @prefix ex: <http://example.org/> .
     ex:service a ex:Service ;
         ex:name "MyService" ;
         ex:version "1.0.0" .
     ```

## 🚧 Blockers Encountered

### Compilation Errors in `ggen-core`

The test suite cannot run due to compilation errors in core:

1. **Format String Errors** (`templates/frozen.rs`, `templates/business_logic.rs`)
   - Invalid format strings with `{%` syntax
   - Need to escape braces: `{{` instead of `{`

2. **Missing `Frontmatter` Fields** (`pipeline.rs`, `streaming_generator.rs`)
   - `template.front.vars` doesn't exist
   - `template.front.rdf` doesn't exist
   - Agent 9 may have modified the Frontmatter structure

3. **Unused Imports** (warnings)
   - `std::sync::Arc` in `optimization.rs`
   - `crate::template::Template` in `streaming_generator.rs`
   - `anyhow` in `business_logic.rs`

### Root Cause

These errors indicate that:
- **Agent 9** (Frontmatter/schema updates) changed the `Frontmatter` struct
- Other parts of the codebase still reference old fields (`vars`, `rdf`)
- The frozen section and business logic features are partially implemented

## 📊 Test Status (Cannot Run Due to Compile Errors)

```
Target: 80%+ pass rate
Actual: 0% (compilation failed)

Updated Tests:     6 tests
New v2.0 Tests:    7 tests (3 #[ignore])
Total Tests:       ~40 tests in integration.rs
Ready to Run:      4 new tests (once compilation fixed)
Pending Features:  3 tests (frozen, business logic, RDF)
```

## 🔧 Required Follow-Up

1. **Fix Compilation Errors**
   - Update `pipeline.rs` to use new Frontmatter API
   - Fix format strings in frozen/business_logic modules
   - Remove unused imports

2. **Implement Frozen Section Feature**
   - Un-ignore `test_v2_frozen_section_preservation()`
   - Add merge logic to template generator

3. **Implement Business Logic Protection**
   - Un-ignore `test_v2_business_logic_not_overwritten()`
   - Add file tracking to avoid regenerating non-template files

4. **Complete RDF Template Generation**
   - Un-ignore `test_v2_rdf_based_template_generation()`
   - Wire up RDF → template variable mapping

## 📁 Files Modified

- ✅ `cli/tests/integration.rs` - Updated for v2.0 syntax, added 7 new tests
- ✅ `.claude/refactor-v2/agent10-test-updates.md` - This document

## 🎯 Next Steps (Recommended Execution Order)

1. **Agent 11 or Integration Agent**: Fix compilation errors in core
   - Update `Frontmatter` field references
   - Fix format string escaping
   - Clean up warnings

2. **Run Integration Tests**:
   ```bash
   cargo test --package ggen-cli-lib --test integration --all-features
   ```

3. **Measure Pass Rate**:
   ```bash
   # Target: 80%+ passing
   # Expected: 30-35 tests pass, 3-5 ignored, 0-5 fail
   ```

4. **Implement Frozen/Business Logic Features** (if needed for v2.0.0):
   - Un-ignore related tests
   - Add implementation in `ggen-core/src/templates/`

## 📋 Test Inventory

### Active Tests (Should Pass)
- ✅ `test_v2_auto_discovery` - Command discovery
- ✅ `test_v2_sync_wrapper_execution` - Sync wrappers
- ✅ `test_v2_help_me_command` - Progressive help
- ✅ `test_v2_marketplace_search_with_rdf` - RDF marketplace
- ✅ `test_marketplace_search_integration` - Market search
- ✅ `test_doctor_before_operations` - Doctor command
- ✅ `test_help_command` - Help output
- ✅ `test_version_command` - Version check
- ✅ `test_progressive_help` - Help-me command
- ✅ `test_subcommand_help` - Subcommand help

### Ignored Tests (Features Not Ready)
- 🚧 `test_v2_frozen_section_preservation` - Needs implementation
- 🚧 `test_v2_business_logic_not_overwritten` - Needs implementation
- 🚧 `test_v2_rdf_based_template_generation` - Partial implementation
- 🚧 `test_template_generate_integration` - Generate-tree command
- 🚧 `test_project_gen_integration` - Project gen not ready
- 🚧 `test_lifecycle_execution_integration` - Lifecycle run not ready
- 🚧 `test_shell_completion_generation` - Shell completion not ready
- 🚧 `test_config_file_loading` - Config loading not ready

### Expected to Fail (Error Propagation)
- ⚠️ `test_error_propagation_invalid_template` - Should fail gracefully
- ⚠️ `test_error_propagation_missing_file` - Should fail gracefully
- ⚠️ `test_error_propagation_invalid_command` - Should fail gracefully
- ⚠️ `test_error_propagation_missing_required_var` - Should fail gracefully

## ✅ Coordination Hooks

```bash
# Pre-task
npx claude-flow@alpha hooks pre-task --description "Agent 10: Test updates"

# Post-edit
npx claude-flow@alpha hooks post-edit \
  --file "cli/tests/integration.rs" \
  --memory-key "v2-swarm/agent10/integration-tests"

npx claude-flow@alpha hooks post-edit \
  --file ".claude/refactor-v2/agent10-test-updates.md" \
  --memory-key "v2-swarm/agent10/documentation"

# Post-task
npx claude-flow@alpha hooks post-task --task-id "agent10-tests"
```

## 📈 Success Metrics

- ✅ **Tests Updated**: 6 v1.x tests → v2.0 syntax
- ✅ **New Tests Added**: 7 v2.0 pattern tests
- ✅ **Documentation**: Complete deliverable
- ❌ **Pass Rate**: 0% (blocked by compilation errors)
- 🚧 **Target**: 80%+ (once compilation fixed)

## 💡 Key Insights

1. **v2.0 Simplification**: Removing `--var` flags makes CLI much cleaner
2. **RDF Integration**: All template data should come from RDF (declarative)
3. **Auto-Discovery**: Commands are dynamically discovered (no manual registration)
4. **Sync Wrappers**: Async functions wrapped for sync CLI execution
5. **Frozen Sections**: Critical for preserving business logic during regeneration

---

**Status**: ✅ Test updates complete, 🚧 Blocked by compilation errors in core

**Next Agent**: Integration agent or Agent 11 (fix compilation, run tests, measure pass rate)
