# Archival Guide - Complete Process Documentation

**Version**: 1.0
**Created**: 2025-12-14
**Feature**: 009-cleanup-old-artifacts
**Branch**: 009-cleanup-old-artifacts

---

## Overview

This guide provides comprehensive documentation for the **ggen archival system** - a deterministic, reproducible process for identifying and archiving obsolete code, tests, and documentation.

The system uses **simple, verifiable criteria** for archival decisions:
- Files unmodified for 7+ days
- Files marked with `#[deprecated]` Rust attribute

All archived files are **preserved in `.archive/` directory** with complete git history for recovery.

---

## Why Archive?

**Problem**: Old code accumulates in repositories, creating noise and maintenance burden:
- Stale tests generate false negatives
- Deprecated code tempts developers to reuse it
- Historical documentation confuses users
- Build artifacts clutter the workspace

**Solution**: Deterministic archival to `.archive/` preserves history while cleaning active code.

**Benefits**:
- ✅ **Clean workspace** - Active code free from clutter
- ✅ **Git history preserved** - Full recovery capability via `git log`
- ✅ **Deterministic criteria** - No subjective judgment, reproducible decisions
- ✅ **Audit trail** - `.archive/MANIFEST.md` documents what was archived and why
- ✅ **Zero data loss** - Nothing deleted, everything recoverable

---

## Architecture

### Two-Phase Process

```
Phase 1: IDENTIFICATION
├── Input: ggen workspace
├── Process: identify-archived.sh
│   ├── Phase 1a: File discovery (.rs, .md, tests/*)
│   ├── Phase 1b: 7-day scan (git log modification date)
│   ├── Phase 1c: Deprecated scan (grep #[deprecated])
│   └── Phase 1d: Consolidation & deduplication
└── Output: archive-list.txt (file paths to archive)

Phase 2: ARCHIVAL
├── Input: archive-list.txt
├── Process: archive-files.sh
│   ├── Step 1: Create .archive/ directory
│   ├── Step 2: Move files with path preservation
│   ├── Step 3: Generate .archive/MANIFEST.md
│   ├── Step 4: Update Cargo.toml workspace members
│   └── Step 5: Clean up import statements
└── Output: .archive/ with MANIFEST.md
```

### Key Invariants

1. **Deterministic Criteria**: No judgment calls, only objective facts
   - Modification date (git log)
   - Deprecated marker (grep for `#[deprecated]`)

2. **File Preservation**: Nothing deleted
   - Files moved to `.archive/` with path preserved
   - Manifest logs timestamp, original path, reason
   - Full git history available for recovery

3. **Workspace Integrity**: No orphaned references
   - Cargo.toml updated to remove archived crates
   - Import statements removed from active code
   - Build remains clean after archival

---

## Scripts

### identify-archived.sh

**Location**: `.specify/scripts/bash/identify-archived.sh`

**Purpose**: Identify files matching archival criteria

**Usage**:
```bash
# Default: 7+ days unmodified
./identify-archived.sh

# Custom days threshold
./identify-archived.sh --days 14

# Save to file
./identify-archived.sh --days 7 --output archive-list.txt

# JSON output
./identify-archived.sh --json --output results.json
```

**How It Works**:

1. **Phase 1: File Discovery**
   - Finds all `.rs` files
   - Finds all test files (`*_test.rs`, `*_tests.rs`, `tests/*`)
   - Finds all `.md` files
   - Excludes: `.git/`, `.archive/`, `target/`, `node_modules/`

2. **Phase 2: 7-Day Scan**
   - Uses `git log` to find last modification timestamp
   - Calculates cutoff date (N days ago)
   - Compares file modification timestamps
   - Identifies files older than threshold

3. **Phase 3: Deprecated Scan**
   - Searches all `.rs` files for `#[deprecated]` attribute
   - Includes regardless of modification date
   - Example: `#[deprecated(since = "0.5.0", note = "use new_function instead")]`

4. **Phase 4: Consolidation**
   - Merges 7-day and deprecated lists
   - Removes duplicates
   - Sorts alphabetically
   - Outputs as plain text or JSON

**Output Format** (plain text):
```
./crates/old-crate/src/lib.rs
./crates/old-crate/tests/test_module.rs
./docs/legacy-api.md
./tests/deprecated_test.rs
```

**Output Format** (JSON):
```json
{
  "timestamp": "2025-12-14T21:15:30Z",
  "days_threshold": 7,
  "total_files": 396,
  "seven_day_old_count": 396,
  "deprecated_count": 0,
  "files": [
    "./crates/old-crate/src/lib.rs",
    "./crates/old-crate/tests/test_module.rs",
    "./docs/legacy-api.md"
  ]
}
```

### archive-files.sh

**Location**: `.specify/scripts/bash/archive-files.sh`

**Purpose**: Execute archival - move files and update workspace

**Usage**:
```bash
# Dry run (preview changes)
./archive-files.sh archive-list.txt --dry-run

# Execute archival
./scripts/archive-files.sh archive-list.txt

# Without Cargo.toml updates
./scripts/archive-files.sh archive-list.txt --no-cargo-update

# Without import cleanup
./scripts/archive-files.sh archive-list.txt --no-import-cleanup
```

**What It Does**:

1. **Creates Archive Directory**
   - Creates `.archive/` if not exists
   - Sets up for manifest generation

2. **Moves Files**
   - Preserves original directory structure
   - Example: `crates/old/src/lib.rs` → `.archive/crates-old-src-lib.rs`
   - Formula: `.archive/{directory-with-slashes-replaced-by-dashes}-{filename}`

3. **Generates Manifest**
   - Creates `.archive/MANIFEST.md` with:
     - Timestamp of archival
     - Original file path
     - Archive path
     - Reason (unmodified 7+ days OR marked deprecated)
   - Example entry:
     ```markdown
     - **Original Path**: `./crates/old/src/lib.rs`
       - **Archived**: 2025-12-14T21:15:30Z
       - **Archive Path**: `.archive/crates-old-src-lib.rs`
       - **Reason**: unmodified 7+ days or marked deprecated
     ```

4. **Updates Cargo.toml**
   - Parses file list for `Cargo.toml` entries
   - Extracts crate names
   - Removes from `[workspace] members = [...]` list
   - Example: `crates/old-crate/Cargo.toml` → removes `"crates/old-crate"` from workspace

5. **Cleans Up Imports**
   - Extracts module paths from archived files
   - Searches active `.rs` files for `use module_path` statements
   - Removes matching import lines
   - Example: `crates/old/src/lib.rs` → removes `use old::...` from active code

**Dry Run**:
```bash
./scripts/archive-files.sh archive-list.txt --dry-run
```
Shows exactly what would be archived without making changes. **Always do dry run first!**

---

## Archive Structure

Files are organized in `.archive/` with path preservation:

```
.archive/
├── crates-old-crate-src-lib.rs              # from crates/old-crate/src/lib.rs
├── crates-old-crate-tests-module_test.rs    # from crates/old-crate/tests/module_test.rs
├── docs-legacy-api.md                       # from docs/legacy-api.md
├── tests-deprecated_test.rs                 # from tests/deprecated_test.rs
└── MANIFEST.md                              # Archive inventory
```

**Path Preservation Logic**:
1. Take original path: `crates/old-crate/src/lib.rs`
2. Replace `/` with `-`: `crates-old-crate-src-lib.rs`
3. Prepend `.archive/`: `.archive/crates-old-crate-src-lib.rs`

**Rationale**: Preserves hierarchy in flat namespace for easy recovery

---

## Identification Criteria

### Criterion 1: 7+ Days Unmodified

**Definition**: Files with no changes for 7 or more days

**Detection Method**:
- Uses `git log` to find last modification timestamp
- Compares against current time minus 7 days
- Applies to: `.rs` files, test files, `.md` documentation

**Why 7 Days?**
- Long enough to capture truly stale code
- Short enough for regular maintenance cycles
- Aligns with typical development iterations

**Example**:
```
File: crates/old/src/lib.rs
Last modified: 2025-12-07 (7 days ago)
Current time: 2025-12-14
Status: ARCHIVABLE (exactly at threshold)
```

### Criterion 2: Deprecated Marker

**Definition**: Any file containing `#[deprecated]` Rust attribute

**Detection Method**:
- Grep search for `#[deprecated]` in all `.rs` files
- Includes regardless of modification date
- Complete file archived if marker present

**Example**:
```rust
#[deprecated(since = "0.5.0", note = "use new_function instead")]
pub fn old_function() {
    // Implementation
}
```

**Why Deprecated Matters**:
- Explicit intent: developer marked code as "please don't use"
- Should be archived immediately, not based on time
- Prevents accidental reuse

---

## Workflow

### Step 1: Review Specification

Read `.specify/specs/009-cleanup-old-artifacts/spec.md` to understand requirements

### Step 2: Identify Archivable Files

```bash
# From workspace root
./.specify/scripts/bash/identify-archived.sh --output archive-list.txt
```

**Output**: `archive-list.txt` containing file paths (one per line)

**Review Options**:
```bash
# How many files?
wc -l archive-list.txt

# Sample of files
head -20 archive-list.txt

# Search for specific files
grep "module-name" archive-list.txt
```

### Step 3: Dry Run (Preview)

```bash
# See what WOULD be archived
./scripts/archive-files.sh archive-list.txt --dry-run
```

**Review Output**:
- ✓ Correct archive paths?
- ✓ All files exist?
- ✓ No accidental files included?

### Step 4: Execute Archival

```bash
# Actually archive
./scripts/archive-files.sh archive-list.txt
```

**Process**:
- Files moved to `.archive/`
- `Cargo.toml` updated (if crates archived)
- Imports cleaned (if modules archived)
- `.archive/MANIFEST.md` generated

### Step 5: Verify

```bash
# Check archive directory exists
ls -la .archive/ | head -20

# View manifest
cat .archive/MANIFEST.md

# Confirm count matches
wc -l archive-list.txt
ls .archive/ | wc -l

# Verify git history preserved
git log --follow -- .archive/ | head
```

### Step 6: Commit

```bash
# Stage archived files and manifest
git add .archive/ Cargo.toml

# Commit with descriptive message
git commit -m "Archive $(wc -l < archive-list.txt) files unmodified 7+ days"

# Push to remote
git push origin 009-cleanup-old-artifacts
```

---

## Recovery

### If You Need an Archived File

**Step 1: Find the File in Git History**

```bash
# See all archive commits
git log --all --full-history -- .archive/

# Find the specific file
git log --follow -- ".archive/crates-old-lib.rs"
```

**Step 2: Restore from Archive**

```bash
# Restore to original location
git show <commit>:.archive/crates-old-lib.rs > crates/old/lib.rs

# Or restore to different location
git show <commit>:.archive/crates-old-lib.rs > recovery/old/lib.rs
```

**Step 3: Or Restore via Original Path**

```bash
# Get file from before it was archived
git show HEAD~1:crates/old/lib.rs > crates/old/lib.rs

# Or from any commit
git show <commit>:crates/old/lib.rs > crates/old/lib.rs
```

**Step 4: Re-add to Workspace (if needed)**

```bash
# If it's a crate, add back to Cargo.toml [workspace] members
# If it's a module, add back `use` statements to importing files
# Run tests to ensure re-integration works
```

### Full Rollback

To completely undo archival (restore workspace to pre-archival state):

```bash
# Find the archival commit
git log --oneline | grep -i archive

# Revert the archival commit
git revert <archival-commit>

# This will:
# - Restore all files to original locations
# - Restore Cargo.toml
# - Restore import statements
# - Create new commit documenting the reversion
```

---

## Safety Considerations

### Before Archival

✓ **Commit all work** - Ensure git working tree is clean
```bash
git status  # Should show "nothing to commit"
```

✓ **Create backup** - Optional but recommended
```bash
git tag backup-pre-archive-$(date +%Y%m%d)
git push origin backup-pre-archive-$(date +%Y%m%d)
```

✓ **Test build** - Ensure workspace builds before archival
```bash
cargo make check
cargo make test
```

✓ **Review list** - Check archive-list.txt for surprises
```bash
# Are there files you're not ready to archive?
grep "important-module" archive-list.txt
```

✓ **Dry run** - Always preview before executing
```bash
./scripts/archive-files.sh archive-list.txt --dry-run
```

### After Archival

✓ **Verify structure** - Check .archive/ created correctly
```bash
ls -la .archive/ | head
wc -l .archive/MANIFEST.md
```

✓ **Test build** - Ensure workspace still builds
```bash
cargo make check
cargo make test
```

✓ **Commit** - Document archival in git
```bash
git commit -m "Archive: $(wc -l < archive-list.txt) files unmodified 7+ days"
```

### If Something Goes Wrong

**Option 1: Restore Individual File**
```bash
git show HEAD~1:path/to/file.rs > path/to/file.rs
git add path/to/file.rs
```

**Option 2: Full Rollback**
```bash
git revert <archival-commit>
```

**Option 3: Restore from Tag**
```bash
git show backup-pre-archive-20251214:.archive/ | tar x
```

---

## Examples

### Example 1: Archive Old Tests

```bash
# Identify 7+ day old files
./.specify/scripts/bash/identify-archived.sh -o to-archive.txt

# Preview what would be archived
./scripts/archive-files.sh to-archive.txt --dry-run

# Count files
wc -l to-archive.txt
# Output: 396 to-archive.txt

# Execute
./scripts/archive-files.sh to-archive.txt

# Verify
ls .archive/ | wc -l
# Output: 396

# Commit
git add .archive/ Cargo.toml
git commit -m "Archive: 396 files unmodified 7+ days"
```

### Example 2: Archive Only Deprecated Code

```bash
# Create custom list of deprecated-only files
find . -name "*.rs" -exec grep -l "#\[deprecated\]" {} \; > deprecated-only.txt

# Dry run
./scripts/archive-files.sh deprecated-only.txt --dry-run

# Execute
./scripts/archive-files.sh deprecated-only.txt

# Verify
grep "deprecated" .archive/MANIFEST.md | wc -l
```

### Example 3: Recover an Archived File

```bash
# Find when a file was archived
git log --follow -- ".archive/crates-old-lib.rs" --oneline

# Restore to original location
git show abc1234:.archive/crates-old-lib.rs > crates/old/lib.rs

# Verify it's there
ls -la crates/old/lib.rs

# Add back to workspace if needed
# (Update Cargo.toml, imports, etc.)
```

### Example 4: Selective Archival (No Cargo Update)

```bash
# Create list of specific files to archive
cat > selective-archive.txt << EOF
./some/old/module.rs
./tests/old_integration_test.rs
EOF

# Archive without touching Cargo.toml
./scripts/archive-files.sh selective-archive.txt --no-cargo-update

# Only imports are cleaned, not crate references
```

---

## Troubleshooting

### Problem: "Not a git repository"

```bash
# Ensure you're in workspace root
cd /path/to/ggen
git rev-parse --git-dir
# Should output: .git/
```

### Problem: No Files Found

```bash
# Verify .rs files exist
find . -name "*.rs" | wc -l

# Check modification dates
stat crates/some-file.rs | grep Modify

# Verify git can access files
git ls-files | grep -c "\.rs$"
```

### Problem: Archive Script Fails

```bash
# Always try dry run first
./scripts/archive-files.sh archive-list.txt --dry-run

# Check file list format
head archive-list.txt
# Should be one path per line, relative to repo root

# Check .archive/ permissions
ls -la .archive/ 2>/dev/null || echo "Not created yet"
```

### Problem: Cargo.toml Update Went Wrong

```bash
# Check the diff
git diff Cargo.toml

# If wrong, revert just the Toml
git checkout Cargo.toml

# Then re-run without cargo update
./scripts/archive-files.sh archive-list.txt --no-cargo-update
```

---

## Assumptions & Constraints

### Assumptions

1. **Git Repository**: Scripts require `git log` for modification dates
2. **File Preservation**: `.archive/` is committed to git (files preserved in history)
3. **Deterministic Only**: Only uses modification date + deprecated marker, no opinion
4. **Minimal Scope**: Only archives files, doesn't refactor or reorganize
5. **Manual Integration**: Complex import removals may need manual review

### Constraints

1. **One-Way Operation**: Once archived, recovery requires git knowledge
2. **Manual Import Cleanup**: Very complex imports may need hand-editing
3. **Workspace-Only**: Works within ggen workspace, not external deps
4. **Read Access**: Requires read permission on all workspace files
5. **Platform-Specific**: Uses `git`, `find`, `grep`, `stat` (may need GNU versions on macOS)

---

## Integration with CI/CD

### Pre-Push Hook

Add to `.git/hooks/pre-push` to catch archival issues:

```bash
#!/bin/bash
# Verify .archive/MANIFEST.md is valid
if [[ -f .archive/MANIFEST.md ]]; then
  # Check that all archived files exist
  grep "Original Path" .archive/MANIFEST.md | \
    sed "s|.*\`\(.*\)\`.*|\1|" | \
    while read file; do
      if [[ ! -f .archive/*$(basename "$file") ]]; then
        echo "ERROR: Missing archived file: $file"
        exit 1
      fi
    done
fi
exit 0
```

### GitHub Actions Workflow

```yaml
name: Archive Old Code
on:
  schedule:
    - cron: "0 0 1 * *"  # Monthly
  workflow_dispatch:

jobs:
  archive:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Identify archivable files
        run: ./.specify/scripts/bash/identify-archived.sh -o archive-list.txt

      - name: Archive files
        run: ./scripts/archive-files.sh archive-list.txt

      - name: Verify build
        run: cargo make check && cargo make test

      - name: Create PR
        uses: peter-evans/create-pull-request@v4
        with:
          commit-message: "Archive: $(wc -l < archive-list.txt) files unmodified 7+ days"
          title: "Archive: 7+ day old files"
          body: "Automated archival of files unmodified 7+ days. Review changes before merging."
```

---

## Questions?

See related documentation:
- **Feature Spec**: `.specify/specs/009-cleanup-old-artifacts/spec.md`
- **Implementation Plan**: `.specify/specs/009-cleanup-old-artifacts/plan.md`
- **Task Breakdown**: `.specify/specs/009-cleanup-old-artifacts/tasks.md`
- **Script Help**: Run `./identify-archived.sh --help` or `./archive-files.sh --help`
- **Manifest Format**: Check `.archive/MANIFEST.md` after archival
- **Recovery**: See "Recovery" section above for restore procedures

---

**Archived**: 2025-12-14 - ggen v5 cleanup initiative
**Author**: Claude Code
**License**: Same as ggen project
