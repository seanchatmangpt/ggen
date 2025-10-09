# Marketplace and Documentation Issues - Analysis and Fix Plan

## Testing Results Summary

### ✅ What Works

1. **Marketplace Search**
   ```bash
   cargo run -- search "rust" --limit 5
   ```
   - Successfully connects to production registry
   - Returns results: `io.ggen.rust.cli-subcommand`
   - **Issue**: Panic after results due to uninitialized `slog-scope` logger

2. **Pack Installation**
   ```bash
   cargo run -- add io.ggen.rust.cli-subcommand
   ```
   - Successfully installs pack
   - Downloads from GitHub
   - Creates cache entry

3. **List Installed Packs**
   ```bash
   cargo run -- packs
   ```
   - Shows installed pack with version 0.1.0
   - **Issue**: SHA256 shows "to_be_calculated"

4. **List Templates**
   ```bash
   cargo run -- list
   ```
   - Successfully lists 2 templates from installed pack
   - Shows template metadata correctly

### ❌ What Doesn't Work

1. **Template Generation**
   ```bash
   cargo run -- gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl --var cmd=test
   ```
   - **Error**: `Pack 'io.ggen.rust.cli-subcommand' not found in lockfile`
   - **Root Cause**: Lockfile is not being created/updated when pack is installed

2. **Documentation References**
   - mdBook documentation still references "rgen" instead of "ggen"
   - Makes entire documentation invalid/confusing

3. **Logger Panic**
   - `slog-scope: No logger set` panic after successful operations
   - Non-fatal but pollutes output with backtrace

## Issues Breakdown

### Issue #1: Lockfile Not Created on Pack Installation

**Severity**: HIGH - Blocks template generation

**Location**: `ggen-core/src/lockfile.rs` and `cli/src/cmds/add.rs`

**Problem**:
- Pack is installed to cache (`~/.ggen/cache/`)
- Pack appears in `packs` command output
- But lockfile (`ggen-lock.toml`) is not created/updated
- Generation fails because it checks lockfile, not cache

**Fix Required**:
1. Ensure `LockfileManager::add_pack()` is called after download
2. Ensure lockfile is written to disk
3. Verify lockfile is created in current directory or project root

### Issue #2: SHA256 Not Calculated

**Severity**: MEDIUM - Security/integrity issue

**Location**: `ggen-core/src/cache.rs`

**Problem**:
- Pack shows SHA256 as "to_be_calculated" in output
- Should calculate actual SHA256 hash of downloaded pack

**Fix Required**:
1. Calculate SHA256 after download
2. Store in lockfile
3. Verify on subsequent loads

### Issue #3: Logger Not Initialized

**Severity**: LOW - Cosmetic but annoying

**Location**: `src/main.rs` or `utils/src/logger.rs`

**Problem**:
- HTTP client libraries (reqwest/rustls) try to log
- `slog-scope` global logger is not set
- Causes panic with backtrace

**Fix Required**:
1. Initialize `slog-scope` with global logger
2. Or suppress HTTP library logging
3. Or switch to different logging backend

### Issue #4: Documentation References "rgen"

**Severity**: HIGH - User confusion

**Location**: `docs/src/**/*.md` files

**Problem**:
- All mdBook documentation refers to "rgen" (old name)
- Should be "ggen" throughout
- Makes documentation invalid and confusing

**Fix Required**:
1. Search and replace "rgen" → "ggen" in all docs
2. Update examples and code snippets
3. Update package names and URLs
4. Rebuild mdBook

## Comprehensive Fix Plan

### Phase 1: Fix Critical Blockers (Required Before Docs)

#### Task 1.1: Fix Lockfile Creation
**Files to modify**:
- `cli/src/cmds/add.rs`
- `ggen-core/src/lockfile.rs`

**Changes**:
```rust
// In add.rs:
// After successful download, ensure lockfile is updated
let mut lockfile_manager = LockfileManager::new()?;
lockfile_manager.add_pack(
    pack_id,
    version,
    source_url,
    sha256
)?;
lockfile_manager.write()?; // Ensure this is called!
```

**Testing**:
```bash
# Clean test
rm -rf ~/.ggen/cache
rm -f ggen-lock.toml

# Install pack
cargo run -- add io.ggen.rust.cli-subcommand

# Verify lockfile exists
cat ggen-lock.toml

# Test generation
cargo run -- gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl --var cmd=test
```

#### Task 1.2: Calculate SHA256
**Files to modify**:
- `ggen-core/src/cache.rs`

**Changes**:
```rust
use sha2::{Sha256, Digest};

fn calculate_sha256(path: &Path) -> Result<String> {
    let mut file = File::open(path)?;
    let mut hasher = Sha256::new();
    std::io::copy(&mut file, &mut hasher)?;
    Ok(format!("{:x}", hasher.finalize()))
}

// Call after download
let sha256 = calculate_sha256(&pack_path)?;
```

#### Task 1.3: Fix Logger Initialization
**Files to modify**:
- `src/main.rs`

**Changes**:
```rust
use slog::{Drain, Logger, o};
use slog_scope;

fn main() {
    // Initialize slog before any async operations
    let decorator = slog_term::TermDecorator::new().build();
    let drain = slog_term::CompactFormat::new(decorator).build().fuse();
    let drain = slog_async::Async::new(drain).build().fuse();
    let logger = Logger::root(drain, o!());

    let _guard = slog_scope::set_global_logger(logger);

    // Rest of main...
}
```

**Or simpler - disable library logging**:
```rust
// In Cargo.toml or at runtime
env_logger::builder()
    .filter_module("rustls", log::LevelFilter::Off)
    .init();
```

### Phase 2: Fix Documentation

#### Task 2.1: Find All "rgen" References
```bash
cd docs
grep -r "rgen" src/ | wc -l
```

#### Task 2.2: Replace "rgen" with "ggen"
**Strategy**: Use sed with careful patterns

```bash
cd docs/src

# Find all references
grep -r "rgen" . --include="*.md"

# Replace rgen → ggen (but not in URLs or specific contexts)
find . -name "*.md" -exec sed -i '' 's/\brgen\b/ggen/g' {} \;

# Manual fixes for:
# - Package names (io.rgen.* → io.ggen.*)
# - Repository URLs
# - Code examples
# - CLI commands
```

**Files likely to need updates**:
- `docs/src/README.md`
- `docs/src/introduction.md`
- `docs/src/quickstart.md`
- `docs/src/templates.md`
- `docs/src/cli.md`
- `docs/src/marketplace.md`
- Any examples or tutorials

#### Task 2.3: Update Examples
**Changes needed**:
- CLI commands: `rgen gen` → `ggen gen`
- Package references: `io.rgen.*` → `io.ggen.*`
- Repository URLs
- Installation commands

#### Task 2.4: Rebuild and Verify
```bash
cd docs
mdbook build
mdbook serve --open

# Verify:
# 1. All pages render correctly
# 2. No "rgen" references remain
# 3. Examples are accurate
# 4. Links work
```

### Phase 3: Integration Testing

#### Task 3.1: End-to-End Test Script
```bash
#!/bin/bash
set -e

echo "=== Clean Environment ==="
rm -rf ~/.ggen/cache
rm -f ggen-lock.toml

echo "=== Search Marketplace ==="
cargo run -- search "rust"

echo "=== Install Pack ==="
cargo run -- add io.ggen.rust.cli-subcommand

echo "=== Verify Lockfile ==="
if [ ! -f "ggen-lock.toml" ]; then
    echo "ERROR: Lockfile not created"
    exit 1
fi
cat ggen-lock.toml

echo "=== List Templates ==="
cargo run -- list

echo "=== Generate from Template ==="
mkdir -p /tmp/ggen-test
cd /tmp/ggen-test
ggen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl --var cmd=hello

echo "=== Verify Generated File ==="
if [ ! -f "src/cmds/hello.rs" ]; then
    echo "ERROR: File not generated"
    exit 1
fi
cat src/cmds/hello.rs

echo "=== SUCCESS ==="
```

#### Task 3.2: Documentation Verification
```bash
#!/bin/bash

echo "=== Verify No 'rgen' References ==="
cd docs/src
RGEN_COUNT=$(grep -r "rgen" . --include="*.md" | wc -l)
if [ "$RGEN_COUNT" -gt 0 ]; then
    echo "ERROR: Found $RGEN_COUNT 'rgen' references:"
    grep -r "rgen" . --include="*.md"
    exit 1
fi

echo "=== Build Documentation ==="
cd ..
mdbook build

echo "=== Check Build Artifacts ==="
if [ ! -d "book" ]; then
    echo "ERROR: Documentation not built"
    exit 1
fi

echo "=== SUCCESS ==="
```

## Priority Order

### Must Fix (Blocking Documentation)
1. ✅ **Logger initialization** (quick fix, prevents panic noise)
2. ✅ **Lockfile creation** (critical for template generation)
3. ✅ **SHA256 calculation** (security/integrity)

### After Code Fixes
4. ✅ **Test end-to-end workflow** (verify everything works)
5. ✅ **Fix rgen→ggen in docs** (search and replace)
6. ✅ **Manual review of docs** (check context-sensitive changes)
7. ✅ **Rebuild mdBook** (generate updated docs)
8. ✅ **Verify deployed docs** (check GitHub Pages)

## Estimated Time

- **Phase 1 (Code Fixes)**: 2-3 hours
  - Lockfile: 1 hour
  - SHA256: 30 minutes
  - Logger: 30 minutes
  - Testing: 1 hour

- **Phase 2 (Documentation)**: 1-2 hours
  - Find/replace: 15 minutes
  - Manual review: 30 minutes
  - Update examples: 30 minutes
  - Testing: 30 minutes

- **Phase 3 (Integration)**: 1 hour
  - Test scripts: 30 minutes
  - Verification: 30 minutes

**Total**: 4-6 hours for complete fix

## Risk Assessment

**Low Risk**:
- Logger fix (isolated change)
- SHA256 calculation (add-only)
- Documentation text changes

**Medium Risk**:
- Lockfile creation (affects state management)

**Mitigation**:
- Test thoroughly with clean environment
- Verify existing packs still work
- Check that lockfile format is valid TOML

## Success Criteria

1. ✅ `cargo run -- add <pack>` creates lockfile
2. ✅ `cargo run -- gen <pack>:<template>` generates file
3. ✅ SHA256 shows actual hash, not "to_be_calculated"
4. ✅ No logger panics during normal operations
5. ✅ Documentation contains zero "rgen" references
6. ✅ All mdBook pages render correctly
7. ✅ Examples in docs are copy-pasteable and work
8. ✅ GitHub Pages shows updated documentation

## Next Steps

**Immediate**:
1. Start with logger fix (quick win, prevents noise)
2. Fix lockfile creation (critical blocker)
3. Add SHA256 calculation
4. Test end-to-end

**After Code Works**:
5. Fix documentation references
6. Rebuild mdBook
7. Push to GitHub Pages

## Recommendation

**Should we fix everything, or just the documentation?**

Given the issues found:

**Option A: Quick Doc Fix Only** (30 minutes)
- Fix rgen→ggen in docs
- Rebuild mdBook
- Note known issues in documentation
- ⚠️ Users will find broken marketplace functionality

**Option B: Complete Fix** (4-6 hours)
- Fix all code issues first
- Then update documentation with working examples
- Deploy fully functional system
- ✅ Users get working product

**Recommendation**: **Option B** - Fix code first, then docs. The documentation should show a working system, not document broken functionality.
