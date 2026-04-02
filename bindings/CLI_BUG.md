# CLI Verb Discovery Bug

## Issue

The `ggen sync` command exists in the codebase but isn't being discovered at runtime by clap-noun-verb's auto-discovery mechanism.

## Evidence

```bash
$ ./target/release/ggen --help
Usage: ggen

Options:
  -h, --help     Print help
  -V, --version  Print version

# NO SUBCOMMANDS LISTED!

$ ./target/release/ggen sync
ERROR: Argument parsing failed: error: unexpected argument 'sync' found
```

## Root Cause Analysis

1. **Verb Definition Exists**: `/home/user/ggen/crates/ggen-cli/src/cmds/sync.rs` has proper `#[verb("sync", "root")]` annotation
2. **Binary Compiled**: The sync module compiles successfully into the binary
3. **linkme Registration**: The verb is registered in the linkme distributed slice
4. **Runtime Discovery Fails**: `CliBuilder::new().run()` doesn't discover the registered verb

## Potential Causes

- `CliBuilder::run()` may not be scanning the linkme registry correctly
- The sync module may not be properly imported for linkme discovery
- clap-noun-verb v5.3.4 may require additional configuration
- Feature flags may be missing (`default-features = false` is set)

## Workaround

Use `SyncExecutor` directly from `ggen-core::codegen`:

```rust
use ggen_core::codegen::{SyncExecutor, SyncOptions};

let options = SyncOptions::new();
let result = SyncExecutor::new(options).execute()?;
```

## Files Affected

- `crates/ggen-cli/src/cmds/sync.rs` - Verb definition (line 147)
- `crates/ggen-cli/src/lib.rs` - CLI entry point using CliBuilder (line 278)
- `crates/ggen-cli/src/cmds/mod.rs` - Alternative `clap_noun_verb::run()` path (line 73)

## Recommended Fix

1. Test switching from `CliBuilder::run()` to direct `clap_noun_verb::run()` call
2. Ensure sync module is explicitly imported/referenced
3. Verify clap-noun-verb v5.3.4 documentation for required setup
4. Try enabling features: `features = ["full"]`

## Impact on Dogfooding

Since `ggen sync` cannot be invoked via CLI, the dogfooding process (using ggen to generate its own bindings) requires manual template processing or using `SyncExecutor` directly.

## Status

- CLI Bug: **CONFIRMED**
- Workaround: **Manual generation using templates**
- Fix Priority: **HIGH** (blocks primary use case)
