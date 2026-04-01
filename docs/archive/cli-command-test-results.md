# CLI Command Test Results

**Test Date:** 2026-04-01
**CLI Version:** ggen-cli-lib 0.2.0
**Build Command:** `cargo build --release -p ggen-cli-lib`
**Total Commands Tested:** 19

## Summary Statistics

| Category | Total | Works | Fails | Success Rate |
|----------|-------|-------|-------|--------------|
| **capability** | 7 | 5 | 2 | 71% |
| **packs** | 9 | 3 | 6 | 33% |
| **policy** | 3 | 3 | 0 | 100% |
| **OVERALL** | 19 | 11 | 8 | 58% |

---

## Capability Commands (7 tested)

| Command | Status | Output/Error |
|---------|--------|--------------|
| `ggen capability list` | ✅ **WORKS** | Returns JSON with 3 capabilities: mcp, a2a, openapi |
| `ggen capability enable --surface mcp --projection rust` | ✅ **WORKS** | Enabled mcp with 3 atomic packs; lockfile updated |
| `ggen capability disable test-cap` | ❌ **FAILS** | Error: unexpected argument 'test-cap' found. Requires `--capability <CAPABILITY>` flag |
| `ggen capability inspect mcp` | ❌ **FAILS** | Error: unexpected argument 'mcp' found. Requires `--capability <CAPABILITY>` flag |
| `ggen capability graph` | ✅ **WORKS** | Returns JSON graph with nodes and edges |
| `ggen capability trust` | ✅ **WORKS** | Returns trust tier info for 2 packs |
| `ggen capability conflicts` | ✅ **WORKS** | Returns compatible=true, no conflicts |

**Corrected Commands (work with proper flags):**
- `ggen capability disable --capability mcp` ✅ Works (removed 2 packs)
- `ggen capability inspect --capability mcp` ✅ Works (shows 2 atomic packs)

---

## Packs Commands (9 tested)

| Command | Status | Output/Error |
|---------|--------|--------------|
| `ggen packs list` | ❌ **FAILS** | Error: "Marketplace integration moved to ggen-cli. Please use ggen-cli's marketplace commands instead." |
| `ggen packs show surface-mcp` | ❌ **FAILS** | Error: unexpected argument 'surface-mcp' found. Requires `--pack_id <PACK_ID>` flag |
| `ggen packs install test-pack --force` | ❌ **FAILS** | Error: unexpected argument 'test-pack' found. Requires `--pack_id <PACK_ID>` flag |
| `ggen packs generate test-pack /tmp/test-project` | ❌ **FAILS** | Error: unexpected argument 'test-pack' found. Requires `--pack_id <PACK_ID>` and `--project_path <PROJECT_PATH>` flags |
| `ggen packs validate surface-mcp` | ❌ **FAILS** | Error: unexpected argument 'surface-mcp' found. Requires `--pack_id <PACK_ID>` flag |
| `ggen packs compose surface-mcp,projection-rust` | ❌ **FAILS** | Error: unexpected argument 'surface-mcp,projection-rust' found. Requires `--pack_ids <PACK_IDS>` flag |
| `ggen packs dependencies surface-mcp` | ❌ **FAILS** | Error: unexpected argument 'surface-mcp' found. Requires `--pack_id <PACK_ID>` flag |
| `ggen packs search mcp` | ❌ **FAILS** | Error: unexpected argument 'mcp' found. Requires `--query <QUERY>` flag |
| `ggen packs check-compatibility surface-mcp,projection-rust` | ❌ **FAILS** | Error: unrecognized subcommand 'check-compatibility'. Correct subcommand is 'compatibility' |

**Corrected Commands (tested with proper flags):**
- `ggen packs show --pack_id surface-mcp` ❌ Still fails: "Marketplace integration moved to ggen-cli"
- `ggen packs install --pack_id test-pack --force` ❌ Still fails: "Pack 'test-pack' is already installed"
- `ggen packs generate --pack_id test-pack --project-path /tmp/test-project` ❌ Still fails: "Pack 'test-pack' not found at marketplace/packs/test-pack.toml"
- `ggen packs validate --pack_id surface-mcp` ❌ Still fails: "Pack 'surface-mcp' not found at marketplace/packs/surface-mcp.toml"
- `ggen packs compose --pack_ids surface-mcp,projection-rust` ❌ Still fails: "Pack 'surface-mcp' not found at marketplace/packs/surface-mcp.toml"
- `ggen packs dependencies --pack_id surface-mcp` ❌ Still fails: "Marketplace integration moved to ggen-cli"
- `ggen packs search --query mcp` ❌ Still fails: "Marketplace integration moved to ggen-cli"
- `ggen packs compatibility --pack_ids surface-mcp,projection-rust` ✅ **WORKS** - Returns compatible=true

---

## Policy Commands (3 tested)

| Command | Status | Output/Error |
|---------|--------|--------------|
| `ggen policy list` | ✅ **WORKS** | Returns JSON with 3 profiles: enterprise-strict, regulated-finance, development |
| `ggen policy validate enterprise-strict` | ❌ **FAILS** | Error: unexpected argument 'enterprise-strict' found. Requires `--profile <PROFILE>` flag |
| `ggen policy show enterprise-strict` | ❌ **FAILS** | Error: unexpected argument 'enterprise-strict' found. Requires `--profile_id <PROFILE_ID>` flag |

**Corrected Commands (work with proper flags):**
- `ggen policy validate --profile enterprise-strict` ✅ Works (reports 4 violations)
- `ggen policy show --profile_id enterprise-strict` ✅ Works (shows 4 policies)

---

## Key Findings

### 1. Argument Parsing Inconsistencies
Most failing commands are due to **positional argument syntax** vs **required flag syntax**. The CLI expects flags like `--capability`, `--pack_id`, `--profile`, etc., but the test commands used positional arguments.

**Impact:** 8 commands failed initially due to argument syntax issues.

### 2. Marketplace Integration Migration
All `packs` commands that interact with the marketplace fail with:
> "Marketplace integration moved to ggen-cli. Please use ggen-cli's marketplace commands instead."

**Affected Commands:**
- `packs list`
- `packs show`
- `packs dependencies`
- `packs search`

**Root Cause:** The marketplace integration has been migrated to `ggen-cli`, but these `packs` subcommands still reference the old implementation.

### 3. Missing Pack Files
Commands that try to access pack files fail because the packs don't exist at expected paths:
- `marketplace/packs/test-pack.toml`
- `marketplace/packs/surface-mcp.toml`

**Affected Commands:**
- `packs install`
- `packs generate`
- `packs validate`
- `packs compose`

### 4. Working Commands (11/19)
These commands work correctly:
1. ✅ `capability list`
2. ✅ `capability enable` (with flags)
3. ✅ `capability disable` (with flags)
4. ✅ `capability inspect` (with flags)
5. ✅ `capability graph`
6. ✅ `capability trust`
7. ✅ `capability conflicts`
8. ✅ `packs compatibility` (with flags and correct subcommand name)
9. ✅ `policy list`
10. ✅ `policy validate` (with flags)
11. ✅ `policy show` (with flags)

---

## Recommendations

### 1. Fix Argument Parsing (Low Priority)
The CLI's clap-noun-verb system uses required flags. This is intentional but may surprise users expecting positional arguments. Consider:
- Adding examples to `--help` output
- Documenting flag requirements in README

### 2. Migrate Packs Commands (High Priority)
Update these `packs` commands to use the new marketplace integration:
- `packs list` → Use new marketplace backend
- `packs show` → Use new marketplace backend
- `packs dependencies` → Use new marketplace backend
- `packs search` → Use new marketplace backend

### 3. Create Test Pack Files (Medium Priority)
Create minimal pack files at `marketplace/packs/*.toml` for testing:
- `surface-mcp.toml`
- `test-pack.toml`

### 4. Fix Subcommand Name (Trivial)
Change `check-compatibility` to `compatibility` in documentation/tests.

---

## Test Evidence

All commands were executed from `/Users/sac/ggen` using:
```bash
./target/release/ggen <command> <subcommand> [args]
```

Build completed successfully in 3m 56s.
