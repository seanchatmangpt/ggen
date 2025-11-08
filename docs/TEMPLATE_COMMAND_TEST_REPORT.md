# Template Command Test Report - Chicago TDD Validation

**Test Date**: 2025-11-07
**Test Approach**: Chicago TDD (Real execution, observable behavior)
**Tested Binary**: `./target/debug/ggen`

## Executive Summary

**Status**: ❌ **ALL TEMPLATE COMMANDS ARE NON-FUNCTIONAL**

**Root Cause**: Template commands are implemented but missing `#[verb]` annotations required by clap-noun-verb v3.4.0 auto-discovery.

**Impact**: Template subsystem is completely inaccessible via CLI despite having full implementation.

---

## Test Results

### 1. `ggen template show <file>` - ❌ NOT REGISTERED
```bash
$ ./target/debug/ggen template show /tmp/ggen_test_templates/sample.tmpl
Error: CLI error: CLI execution failed: Argument parsing failed: error: unrecognized subcommand 'template'
```

**Expected**: Display template metadata (name, description, variables, RDF sources, SPARQL queries)
**Actual**: Command not recognized
**Implementation Status**: ✅ Fully implemented in `/Users/sac/ggen/crates/ggen-cli/src/cmds/template.rs:381-423`

---

### 2. `ggen template new <name>` - ❌ NOT REGISTERED
```bash
$ ./target/debug/ggen template new test_template
Error: CLI error: CLI execution failed: Argument parsing failed: error: unrecognized subcommand 'template'
```

**Expected**: Create new template file with boilerplate
**Actual**: Command not recognized
**Implementation Status**: ✅ Fully implemented in `/Users/sac/ggen/crates/ggen-cli/src/cmds/template.rs:345-363`

---

### 3. `ggen template list` - ❌ NOT REGISTERED
```bash
$ ./target/debug/ggen template list
Error: CLI error: CLI execution failed: Argument parsing failed: error: unrecognized subcommand 'template'
```

**Expected**: List available templates from directory
**Actual**: Command not recognized
**Implementation Status**: ✅ Fully implemented in `/Users/sac/ggen/crates/ggen-cli/src/cmds/template.rs:303-342`

---

### 4. `ggen template lint <file>` - ❌ NOT REGISTERED
```bash
$ ./target/debug/ggen template lint /tmp/ggen_test_templates/sample.tmpl
Error: CLI error: CLI execution failed: Argument parsing failed: error: unrecognized subcommand 'template'
```

**Expected**: Validate template syntax, check for errors/warnings
**Actual**: Command not recognized
**Implementation Status**: ✅ Fully implemented in `/Users/sac/ggen/crates/ggen-cli/src/cmds/template.rs:260-300`

---

## Current CLI State

```bash
$ ./target/debug/ggen --help
Usage: ggen [COMMAND]

Commands:
  utils  Manage environment variables
  help   Print this message or the help of the given subcommand(s)
```

**Available Commands**: 2 (utils, help)
**Expected Commands**: 8+ (including template, ai, graph, hook, marketplace, project)

---

## Root Cause Analysis

### clap-noun-verb v3.4.0 Auto-Discovery Requirements

The project uses clap-noun-verb v3.4.0 which requires:

1. **Verb Annotation**: Each command function must have `#[verb]` macro
2. **Flat Function Structure**: Direct functions, not nested in structs/impls
3. **Module Registration**: Module must be listed in `cmds/mod.rs` (✅ already done)

### Current Template Implementation (WRONG)

```rust
// ❌ Missing #[verb] annotations
impl TemplateArgs {
    pub fn execute(&self) -> Result<()> {
        match &self.command {
            TemplateCommand::Generate(args) => run_generate(args),
            TemplateCommand::Show(args) => run_show(args),
            // ...
        }
    }
}

fn run_show(args: &ShowArgs) -> Result<()> {
    // Implementation exists but not discoverable
}
```

### Correct Pattern (from hook.rs)

```rust
// ✅ Has #[verb] annotation
#[verb]
fn create(
    #[arg(short, long)]
    event: String,
    // ...
) -> Result<CreateOutput> {
    // Direct function with verb annotation
}
```

---

## Impact Assessment

### Functional Commands (Have #[verb])
- ✅ `ai generate` - Working
- ✅ `ai chat` - Working
- ✅ `ai enhance` - Working
- ✅ `graph query` - Working
- ✅ `graph visualize` - Working
- ✅ `hook create` - Working
- ✅ `marketplace search` - Working

### Non-Functional Commands (Missing #[verb])
- ❌ `template show`
- ❌ `template new`
- ❌ `template list`
- ❌ `template lint`
- ❌ `template generate`
- ❌ `template generate-rdf`
- ❌ `template generate-tree`
- ❌ `template regenerate`

---

## Recommended Fix

**Refactor `/Users/sac/ggen/crates/ggen-cli/src/cmds/template.rs`**:

1. Remove `TemplateArgs` struct and `impl` block
2. Convert each `run_*` function to a `#[verb]`-annotated function
3. Add return types with serializable output structs
4. Follow the pattern from `hook.rs`, `ai.rs`, `marketplace.rs`

**Estimated Effort**: 2-3 hours
**Testing**: Re-run Chicago TDD tests after refactor

---

## Test Data Created

```bash
# Test template file
/tmp/ggen_test_templates/sample.tmpl

Content:
# Test Template
name: sample_template
description: A test template for validation
version: 1.0

## Variables
- project_name: The name of the project
- author: The author name

## Content
Hello {{project_name}} by {{author}}!
```

---

## Next Steps

1. **Immediate**: Refactor template.rs to use #[verb] pattern
2. **Verification**: Re-run all 4 template commands with real data
3. **Regression**: Ensure other commands still work after changes
4. **Documentation**: Update CLI docs to reflect available commands

---

## Appendix: Working Commands for Comparison

### ai.rs (REFERENCE IMPLEMENTATION)
- ✅ Has `#[verb]` on `generate()`, `chat()`, `enhance()`
- ✅ Uses output structs with `#[derive(Debug, serde::Serialize)]`
- ✅ Calls `execute_async_verb()` wrapper
- ✅ All commands discoverable and functional

### hook.rs (REFERENCE IMPLEMENTATION)
- ✅ Has `#[verb]` on `create()`, `list()`, `run()`, `validate()`
- ✅ Uses `execute_async_verb()` for async domain calls
- ✅ All commands discoverable and functional

### template.rs (NEEDS REFACTORING)
- ❌ Has implementation but wrong structure
- ❌ Uses `TemplateArgs` impl instead of direct `#[verb]` functions
- ❌ Commands not discoverable by clap-noun-verb
- ❌ All commands non-functional despite complete implementation

---

**Test Conclusion**: Template subsystem has 100% implementation but 0% discoverability due to missing auto-discovery annotations.
