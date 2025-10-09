# README Validation Report

**Test Date:** 2025-10-08
**Version Tested:** 1.0.0 (local build)
**Test Environment:** macOS (Darwin 24.5.0)

## Summary

This document reports the results of testing all instructions provided in README.md against the actual implementation. Tests were performed in a clean temporary directory.

---

## ✅ What Works

### 1. Local Binary Building
- ✅ `cargo make build` successfully builds the project
- ✅ Binary version reports correctly: `rgen 1.0.0`

### 2. CLI Commands Available
The following commands are implemented and functional:
- ✅ `rgen --help` - Shows comprehensive help
- ✅ `rgen packs` - Lists installed rpacks (shows "No rpacks installed" when none present)
- ✅ `rgen list` - Lists available local templates
- ✅ `rgen hazard` - Generates hazard report with useful recommendations
- ✅ `rgen completion <shell>` - Generates shell completion scripts (bash, zsh, fish)

### 3. Template Discovery
- ✅ `rgen list` correctly finds and displays templates in `templates/` directory
- ✅ Shows template metadata including output path and variables

---

## ❌ What Doesn't Work

### 1. Installation Methods

#### Homebrew Installation (BROKEN)
```bash
brew tap rgen-dev/tap
brew install rgen
```
**Issue:** Repository `https://github.com/rgen-dev/homebrew-tap/` does not exist
**Error:** `fatal: repository 'https://github.com/rgen-dev/homebrew-tap/' not found`
**Impact:** HIGH - Primary installation method documented in README is non-functional

#### Cargo Installation (BROKEN)
```bash
cargo install rgen
```
**Issue:** Published crate `rgen v0.5.0` on crates.io is a library, not a binary
**Error:** `there is nothing to install in rgen v0.5.0, because it has no binaries`
**Impact:** HIGH - Alternative installation method is also non-functional
**Note:** Local version is `1.0.0`, but published version is `0.5.0` (out of sync)

### 2. Marketplace Functionality (BROKEN)

All marketplace-related commands fail due to missing registry backend:

#### Search Command (BROKEN)
```bash
rgen search "rust cli"
```
**Error:** `Failed to fetch registry index`
**Impact:** HIGH - Core marketplace feature is non-functional

#### Categories Command (BROKEN)
```bash
rgen categories
```
**Error:** `Failed to fetch registry index`
**Impact:** HIGH - Cannot browse marketplace categories

#### Add Command (BROKEN)
```bash
rgen add io.rgen.rust.cli-subcommand
```
**Error:** `Failed to resolve rpack 'io.rgen.rust.cli-subcommand'`
**Impact:** HIGH - Cannot install marketplace packages

### 3. Template Generation Syntax (INCORRECT IN README)

#### README Shows (WRONG):
```bash
rgen gen cli subcommand --vars cmd=hello summary="Print a greeting"
```

#### Actual Syntax Required:
```bash
# For local templates - format appears broken
rgen gen cli/subcommand/rust.tmpl --var slug=hello
```

**Issues:**
- Flag is `--var` (singular) not `--vars` (plural)
- Multiple space-separated arguments not supported (e.g., `cli subcommand`)
- Template path format requires full path from templates directory
- Template reference format expects `pack_id:template_path` but this doesn't work for local templates
- Error message: `Invalid template reference format: 'cli/subcommand/rust.tmpl'. Expected 'pack_id:template_path'`

**Impact:** HIGH - README quick start instructions don't work as written

### 4. Missing Commands (DOCUMENTED BUT NOT IMPLEMENTED)

#### Validate Command (MISSING)
```bash
rgen validate <template>
```
**Error:** `unrecognized subcommand 'validate'`
**Actual Command:** `rgen lint` (for template validation)
**Impact:** MEDIUM - README documentation is incorrect

### 5. Show Command (BROKEN)
```bash
rgen show cli/subcommand/rust.tmpl
```
**Error:** `Template rendering error: Failed to render '__tera_one_off'`
**Impact:** MEDIUM - Cannot preview template metadata

### 6. Marketplace Examples (BROKEN)

All marketplace examples in README are non-functional:

```bash
# None of these work:
rgen search rust cli              # Fails: registry not found
rgen show io.rgen.rust.cli-subcommand  # Fails: registry not found
rgen add io.rgen.rust.cli-subcommand   # Fails: registry not found
rgen gen io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello  # Fails: rpack not installed
```

---

## 🔧 Documentation Errors

### README.md Issues

1. **Installation Section:**
   - Both Homebrew and Cargo install methods are broken
   - No working installation instructions provided
   - Recommend adding: "Build from source: `git clone && cd rgen && cargo make build`"

2. **Quick Start Section:**
   - Marketplace quick start completely non-functional (registry doesn't exist)
   - Local template syntax is incorrect
   - Should use `--var` not `--vars`
   - Template path format unclear

3. **Commands Table:**
   - Lists `rgen validate <template>` but should be `rgen lint <template>`
   - Search command syntax shown as `rgen search <query>` but doesn't specify query must be quoted if multi-word
   - No mention that marketplace commands require a working registry backend

4. **Marketplace Section:**
   - All marketplace features documented but none are functional
   - Registry endpoints not configured or don't exist
   - Should include disclaimer: "⚠️ Marketplace functionality requires registry setup"

5. **Example Syntax:**
   - Multiple examples use non-functional syntax
   - Examples assume marketplace works (it doesn't)

---

## 📋 Recommended Fixes

### High Priority

1. **Fix Installation Methods:**
   - Create `rgen-dev/homebrew-tap` repository OR remove Homebrew instructions
   - Publish binary crate to crates.io OR document source installation only
   - Update version sync between local (1.0.0) and published (0.5.0)

2. **Fix Marketplace or Document Status:**
   - Either implement working registry backend
   - OR clearly document that marketplace is "Coming Soon / In Development"
   - Add fallback documentation for local-only workflow

3. **Fix Template Generation Syntax:**
   - Clarify `--var` vs `--vars` (README uses wrong flag)
   - Document actual template path format for local templates
   - Provide working examples that can be copy-pasted

4. **Fix Command Documentation:**
   - Change `validate` to `lint` throughout README
   - Document that `show` command may have rendering issues
   - Add troubleshooting section

### Medium Priority

5. **Add "Development/Testing" Section:**
   ```markdown
   ## Development Build

   To build and test rgen locally:

   ```bash
   git clone https://github.com/seanchatman/rgen
   cd rgen
   cargo make build
   ./target/debug/rgen --help
   ```

6. **Add "Known Limitations" Section:**
   - Marketplace functionality not yet available
   - Local template workflow is primary supported mode
   - Registry backend in development

---

## 🧪 Test Commands Reference

Commands that were tested and their results:

| Command | Status | Notes |
|---------|--------|-------|
| `cargo make build` | ✅ Works | Successfully builds project |
| `rgen --version` | ✅ Works | Shows version 1.0.0 |
| `rgen --help` | ✅ Works | Comprehensive help text |
| `rgen list` | ✅ Works | Lists local templates |
| `rgen packs` | ✅ Works | Shows installed rpacks |
| `rgen hazard` | ✅ Works | Generates useful report |
| `rgen completion bash` | ✅ Works | Generates completions |
| `brew tap rgen-dev/tap` | ❌ Broken | Repo doesn't exist |
| `cargo install rgen` | ❌ Broken | Published crate is library only |
| `rgen search <query>` | ❌ Broken | Registry not found |
| `rgen categories` | ❌ Broken | Registry not found |
| `rgen add <rpack>` | ❌ Broken | Registry not found |
| `rgen show <template>` | ❌ Broken | Template rendering error |
| `rgen validate <template>` | ❌ Wrong Command | Should be `rgen lint` |
| `rgen gen cli subcommand --vars ...` | ❌ Broken | Wrong syntax in README |

---

## 📝 Conclusion

The rgen codebase has solid core functionality for local template-based generation, but the README documentation is significantly out of sync with the implementation:

**Major Issues:**
- No working installation methods (Homebrew tap missing, crates.io version wrong)
- Marketplace features entirely non-functional (no registry backend)
- README syntax examples use incorrect command-line flags
- Several commands documented incorrectly or don't exist

**Recommendations:**
1. Update README to focus on local template workflow (which works)
2. Mark marketplace features as "Coming Soon" or implement registry
3. Fix all command syntax examples to match actual implementation
4. Add troubleshooting section for common issues
5. Sync published crate version with local version

**Working Use Case:**
Users who clone the repo and build locally CAN successfully:
- Build with `cargo make build`
- Create local templates in `templates/` directory
- List templates with `rgen list`
- Generate code (once correct syntax is determined)
- Get helpful diagnostics with `rgen hazard`

The project shows promise but needs significant documentation updates to match reality.
