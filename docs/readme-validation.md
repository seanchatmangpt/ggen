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
- ✅ Binary version reports correctly: `ggen 1.0.0`

### 2. CLI Commands Available
The following commands are implemented and functional:
- ✅ `ggen --help` - Shows comprehensive help
- ✅ `ggen packs` - Lists installed gpacks (shows "No gpacks installed" when none present)
- ✅ `ggen list` - Lists available local templates
- ✅ `ggen hazard` - Generates hazard report with useful recommendations
- ✅ `ggen completion <shell>` - Generates shell completion scripts (bash, zsh, fish)

### 3. Template Discovery
- ✅ `ggen list` correctly finds and displays templates in `templates/` directory
- ✅ Shows template metadata including output path and variables

---

## ❌ What Doesn't Work

### 1. Installation Methods

#### Homebrew Installation (BROKEN)
```bash
brew tap ggen-dev/tap
brew install ggen
```
**Issue:** Repository `https://github.com/ggen-dev/homebrew-tap/` does not exist
**Error:** `fatal: repository 'https://github.com/ggen-dev/homebrew-tap/' not found`
**Impact:** HIGH - Primary installation method documented in README is non-functional

#### Cargo Installation (BROKEN)
```bash
cargo install ggen
```
**Issue:** Published crate `ggen v0.5.0` on crates.io is a library, not a binary
**Error:** `there is nothing to install in ggen v0.5.0, because it has no binaries`
**Impact:** HIGH - Alternative installation method is also non-functional
**Note:** Local version is `1.0.0`, but published version is `0.5.0` (out of sync)

### 2. Marketplace Functionality (BROKEN)

All marketplace-related commands fail due to missing registry backend:

#### Search Command (BROKEN)
```bash
ggen search "rust cli"
```
**Error:** `Failed to fetch registry index`
**Impact:** HIGH - Core marketplace feature is non-functional

#### Categories Command (BROKEN)
```bash
ggen categories
```
**Error:** `Failed to fetch registry index`
**Impact:** HIGH - Cannot browse marketplace categories

#### Add Command (BROKEN)
```bash
ggen add io.ggen.rust.cli-subcommand
```
**Error:** `Failed to resolve gpack 'io.ggen.rust.cli-subcommand'`
**Impact:** HIGH - Cannot install marketplace packages

### 3. Template Generation Syntax (INCORRECT IN README)

#### README Shows (WRONG):
```bash
ggen gen cli subcommand --vars cmd=hello summary="Print a greeting"
```

#### Actual Syntax Required:
```bash
# For local templates - format appears broken
ggen gen cli/subcommand/rust.tmpl --var slug=hello
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
ggen validate <template>
```
**Error:** `unrecognized subcommand 'validate'`
**Actual Command:** `ggen lint` (for template validation)
**Impact:** MEDIUM - README documentation is incorrect

### 5. Show Command (BROKEN)
```bash
ggen show cli/subcommand/rust.tmpl
```
**Error:** `Template rendering error: Failed to render '__tera_one_off'`
**Impact:** MEDIUM - Cannot preview template metadata

### 6. Marketplace Examples (BROKEN)

All marketplace examples in README are non-functional:

```bash
# None of these work:
ggen search rust cli              # Fails: registry not found
ggen show io.ggen.rust.cli-subcommand  # Fails: registry not found
ggen add io.ggen.rust.cli-subcommand   # Fails: registry not found
ggen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello  # Fails: gpack not installed
```

---

## 🔧 Documentation Errors

### README.md Issues

1. **Installation Section:**
   - Both Homebrew and Cargo install methods are broken
   - No working installation instructions provided
   - Recommend adding: "Build from source: `git clone && cd ggen && cargo make build`"

2. **Quick Start Section:**
   - Marketplace quick start completely non-functional (registry doesn't exist)
   - Local template syntax is incorrect
   - Should use `--var` not `--vars`
   - Template path format unclear

3. **Commands Table:**
   - Lists `ggen validate <template>` but should be `ggen lint <template>`
   - Search command syntax shown as `ggen search <query>` but doesn't specify query must be quoted if multi-word
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
   - Create `ggen-dev/homebrew-tap` repository OR remove Homebrew instructions
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

   To build and test ggen locally:

   ```bash
   git clone https://github.com/seanchatman/ggen
   cd ggen
   cargo make build
   ./target/debug/ggen --help
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
| `ggen --version` | ✅ Works | Shows version 1.0.0 |
| `ggen --help` | ✅ Works | Comprehensive help text |
| `ggen list` | ✅ Works | Lists local templates |
| `ggen packs` | ✅ Works | Shows installed gpacks |
| `ggen hazard` | ✅ Works | Generates useful report |
| `ggen completion bash` | ✅ Works | Generates completions |
| `brew tap ggen-dev/tap` | ❌ Broken | Repo doesn't exist |
| `cargo install ggen` | ❌ Broken | Published crate is library only |
| `ggen search <query>` | ❌ Broken | Registry not found |
| `ggen categories` | ❌ Broken | Registry not found |
| `ggen add <gpack>` | ❌ Broken | Registry not found |
| `ggen show <template>` | ❌ Broken | Template rendering error |
| `ggen validate <template>` | ❌ Wrong Command | Should be `ggen lint` |
| `ggen gen cli subcommand --vars ...` | ❌ Broken | Wrong syntax in README |

---

## 📝 Conclusion

The ggen codebase has solid core functionality for local template-based generation, but the README documentation is significantly out of sync with the implementation:

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
- List templates with `ggen list`
- Generate code (once correct syntax is determined)
- Get helpful diagnostics with `ggen hazard`

The project shows promise but needs significant documentation updates to match reality.
