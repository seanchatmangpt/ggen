# Chicago TDD Validation Report: Graph Commands

**Date:** 2025-11-07
**Test Execution:** Real RDF data with actual command invocation
**Working Directory:** `/Users/sac/ggen`

## Executive Summary

❌ **ALL GRAPH COMMANDS ARE NOT DISCOVERABLE**

The graph commands are **fully implemented** in the codebase with proper domain logic, but they are **not registered** with the CLI router, making them completely inaccessible to users.

## Test Procedure

### Test Data Created
```turtle
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex: <http://example.org/> .

ex:Alice rdf:type ex:Person ;
         rdfs:label "Alice Smith" ;
         ex:knows ex:Bob .

ex:Bob rdf:type ex:Person ;
       rdfs:label "Bob Jones" ;
       ex:age 30 .

ex:Charlie rdf:type ex:Person ;
           rdfs:label "Charlie Brown" ;
           ex:knows ex:Alice .
```

File: `/tmp/test.ttl`

## Command Test Results

### 1. `ggen graph load <file>`

**Command Attempted:**
```bash
./target/debug/ggen graph load /tmp/test.ttl
```

**Result:** ❌ **COMMAND NOT RECOGNIZED**
```
Error: CLI error: CLI execution failed: Argument parsing failed: error: unrecognized subcommand 'graph'

Usage: ggen [COMMAND]

For more information, try '--help'.
```

**Expected Behavior:** Should load RDF triples from Turtle file and display loading statistics

**Actual Behavior:** Command does not exist in CLI router

---

### 2. `ggen graph export <input> <output>`

**Command Attempted:**
```bash
./target/debug/ggen graph export /tmp/test.ttl /tmp/output.ttl
```

**Result:** ❌ **COMMAND NOT RECOGNIZED**
```
Error: CLI error: CLI execution failed: Argument parsing failed: error: unrecognized subcommand 'graph'
```

**Expected Behavior:** Should export RDF graph to specified format (turtle, json-ld, etc.)

**Actual Behavior:** Command does not exist in CLI router

---

### 3. `ggen graph visualize <file>`

**Command Attempted:**
```bash
./target/debug/ggen graph visualize /tmp/test.ttl
./target/debug/ggen graph visualize /tmp/test.ttl --format dot
./target/debug/ggen graph visualize /tmp/test.ttl --format json
```

**Result:** ❌ **COMMAND NOT RECOGNIZED** (all variants)

**Expected Behavior:** Should create visualization in DOT, JSON, or SVG format

**Actual Behavior:** Command does not exist in CLI router

---

## Available Commands

**Current CLI State:**
```bash
./target/debug/ggen --help
```

**Output:**
```
Usage: ggen [COMMAND]

Commands:
  utils  Manage environment variables
  help   Print this message or the help of the given subcommand(s)

Options:
  -h, --help  Print help
```

**Analysis:** Only `utils` command is registered, all other commands (graph, ai, marketplace, template, project) are missing.

---

## Root Cause Analysis

### Issue: clap-noun-verb v3.4.0 Auto-Discovery Not Working

**Code Location:** `/Users/sac/ggen/crates/ggen-cli/src/cmds/graph.rs`

**Implementation Status:** ✅ **FULLY IMPLEMENTED**
- `load()` function with `#[verb]` macro (line 68-115)
- `query()` function with `#[verb]` macro (line 135-172)
- `export()` function with `#[verb]` macro (line 192-238)
- `visualize()` function with `#[verb]` macro (line 263-323)

**Domain Layer Status:** ✅ **FULLY IMPLEMENTED**
- Location: `/Users/sac/ggen/crates/ggen-domain/src/graph/`
- Modules: `load.rs`, `query.rs`, `export.rs`, `visualize.rs`
- All domain functions are properly exported

**Router Configuration:** ❌ **NOT REGISTERING VERBS**
- Location: `/Users/sac/ggen/crates/ggen-cli/src/cmds/mod.rs`
- Uses: `clap_noun_verb::run()` for auto-discovery
- Problem: Auto-discovery is not finding the `#[verb]` macros

**Main Entry Point:** `/Users/sac/ggen/crates/ggen-cli/src/main.rs`
```rust
#[tokio::main]
async fn main() -> anyhow::Result<()> {
    ggen_cli_lib::cli_match().await
        .map_err(|e| anyhow::anyhow!("CLI error: {}", e))
}
```

**CLI Match Function:** `/Users/sac/ggen/crates/ggen-cli/src/lib.rs`
```rust
pub async fn cli_match() -> ggen_utils::error::Result<()> {
    clap_noun_verb::run()
        .map_err(|e| anyhow::anyhow!("CLI execution failed: {}", e))?;
    Ok(())
}
```

---

## Why Auto-Discovery is Failing

### Possible Causes:

1. **Missing linkme registration**: The `#[verb]` macro may require explicit registration via `linkme` crate
2. **Module visibility**: The verb functions may not be public or properly exported
3. **Macro expansion**: Build-time macro expansion may not be collecting the verbs
4. **Incomplete migration**: The codebase may be mid-migration from old command structure

### Evidence from Codebase:

**Graph command implementation has proper structure:**
```rust
use clap_noun_verb_macros::verb;
use clap_noun_verb::Result;

#[verb]
fn load(
    #[arg(short = 'f', long)]
    file: PathBuf,
    // ... more args
) -> Result<LoadOutput> {
    // Implementation delegates to domain layer
}
```

**But registration is missing in:**
- No explicit command registration in `main.rs`
- No linkme distributed slice usage visible
- Auto-discovery not working as expected

---

## Comparison with Working Command

**Working Command:** `utils`
- Registered and accessible
- Shows in `--help` output
- Can be executed

**Non-Working Commands:** `graph`, `ai`, `marketplace`, `template`, `project`
- Fully implemented with `#[verb]` macros
- NOT showing in `--help` output
- Cannot be executed

---

## Chicago TDD Validation Summary

### Tests Performed:
1. ✅ Created real RDF test data (Turtle format with 3 entities)
2. ✅ Attempted to load RDF file with actual file path
3. ✅ Attempted to export to different formats
4. ✅ Attempted to visualize with format options
5. ✅ Checked binary help output
6. ✅ Verified no output files created
7. ✅ Examined source code implementation

### Findings:
- ❌ **0/4 graph commands are functional**
- ✅ **Implementation code exists and appears complete**
- ✅ **Domain layer is properly structured**
- ❌ **CLI routing/registration is broken**
- ❌ **No observable OTEL traces (commands don't run)**
- ❌ **No file outputs (commands don't execute)**

---

## Observable Behavior vs Expected

| Command | Expected | Actual | Status |
|---------|----------|--------|--------|
| `graph load` | Parse RDF, load triples, show stats | "unrecognized subcommand" | ❌ BROKEN |
| `graph export` | Convert and export RDF format | "unrecognized subcommand" | ❌ BROKEN |
| `graph visualize` | Generate DOT/SVG/JSON viz | "unrecognized subcommand" | ❌ BROKEN |
| `graph query` | Execute SPARQL query | "unrecognized subcommand" | ❌ BROKEN |

---

## Recommended Fix

### Option 1: Debug clap-noun-verb Auto-Discovery
- Verify linkme registration is working
- Check if macros are expanding correctly
- Add debug logging to auto-discovery

### Option 2: Manual Command Registration
- Explicitly register commands in main.rs
- Use `CommandRouter::new()` and `.add_noun()` pattern
- Bypass auto-discovery temporarily

### Option 3: Verify Build Configuration
- Check if feature flags are preventing command registration
- Verify workspace dependencies are correct
- Ensure proc-macro expansion is enabled

---

## Test Evidence

**All test commands logged:**
```bash
# Command not found - all variants
./target/debug/ggen graph load /tmp/test.ttl
./target/debug/ggen graph export /tmp/test.ttl /tmp/output.ttl
./target/debug/ggen graph export /tmp/test.ttl /tmp/output.jsonld
./target/debug/ggen graph visualize /tmp/test.ttl
./target/debug/ggen graph visualize /tmp/test.ttl --format dot
./target/debug/ggen graph visualize /tmp/test.ttl --format json
```

**Verified no output created:**
```bash
ls -lh /tmp/output.* /tmp/*.dot /tmp/*.json
# Result: No such file or directory
```

**Help output shows only utils:**
```bash
./target/debug/ggen --help
# Commands:
#   utils  Manage environment variables
#   help   Print this message or the help of the given subcommand(s)
```

---

## Conclusion

**Chicago TDD Verdict:** The graph commands are **STUBS at the CLI level** even though they have **full implementation in code**.

This is a **registration/discovery issue**, not an implementation issue. The underlying RDF functionality exists and is ready to use, but the CLI router is not making it accessible to users.

**Next Steps:**
1. Investigate clap-noun-verb v3.4.0 auto-discovery mechanism
2. Add explicit command registration if auto-discovery is broken
3. Test with minimal example to isolate the issue
4. Consider temporary workaround with manual registration
