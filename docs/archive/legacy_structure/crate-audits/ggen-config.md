<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen-config — Crate Audit](#ggen-config--crate-audit)
  - [ISSUES](#issues)
    - [ConfigValidator — `validate_a2a()` has dead code](#configvalidator--validate_a2a-has-dead-code)
    - [ggen-config-clap — Unimplemented Trait](#ggen-config-clap--unimplemented-trait)
  - [FIX / DELETE / REFACTOR](#fix--delete--refactor)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen-config — Crate Audit

**Path:** `crates/ggen-config/`
**Lines:** ~5 modules, ~15 test markers
**Role:** ggen.toml parsing and validation (GgenConfig with 15 sub-configs)

---

## ISSUES

### ConfigValidator — `validate_a2a()` has dead code

| File:Line | Issue |
|-----------|-------|
| `validator.rs:303` | Unknown field `#[allow(dead_code)]` |

### ggen-config-clap — Unimplemented Trait

| File:Line | Issue |
|-----------|-------|
| `ggen-config-clap/src/loader.rs:10` | `LoadConfigFromGgenToml` trait defined with zero implementors |
| `ggen-config-clap/src/loader.rs:25` | `load_ggen_config()` free function never called by other crates |
| `ggen-config-clap/src/loader.rs:39` | `expand_env_vars()` function defined but not used in main codebase |

The entire `ggen-config-clap` crate is a bridge (ggen.toml → clap) that nothing crosses.

---

## FIX / DELETE / REFACTOR

| Action | Item | Priority |
|--------|------|----------|
| **FIX** | Implement `LoadConfigFromGgenToml` on CLI argument structs | P2 |
| **FIX** | Wire `expand_env_vars` into config loading path | P2 |
| **DELETE** | `ggen-config-clap` crate if bridge pattern is abandoned | P3 |
