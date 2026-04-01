# ggen-utils — Crate Audit

**Path:** `crates/ggen-utils/`
**Lines:** 15+ modules, ~340 test markers
**Role:** Shared infrastructure — universal Error type, SafePath, SafeCommand, path validation, logging, alerts

---

## ARCHITECTURE NOTES

This is the real foundation of the workspace. 10 crates depend on it directly.

### Universal Error Type

`error.rs` defines `Error` with 14 `From` conversions (io, serde, toml, oxigraph, anyhow, etc.) plus `bail!`/`ensure!` macros. Only 2 crates re-export it (ggen-domain, ggen-cli). The other 21 define local error modules.

### SafePath / SafeCommand

Cross-cutting security infrastructure used by 25+ files. `SafePath` prevents path traversal. `SafeCommand` prevents command injection. `path_validator` provides enterprise-grade workspace-bounded validation.

---

## COMMENTED-OUT MODULES

| File:Line | Module | Status |
|-----------|--------|--------|
| `lib.rs:58` | `secrets` | Commented out — compilation errors from Week 8/9 security work |
| `lib.rs:59` | `supply_chain` | Commented out — compilation errors from Week 8/9 security work |

---

## FIX / DELETE / REFACTOR

| Action | Item | Priority |
|--------|------|----------|
| **FIX** | Restore `secrets` module — fix compilation errors | P2 |
| **FIX** | Restore `supply_chain` module — fix compilation errors | P2 |
| **REFACTOR** | Consider making ggen-utils Error the workspace-wide standard (currently 23 local error modules) | P3 |
