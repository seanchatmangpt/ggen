# Archival Scripts Documentation

## Overview

The archival system provides tools to identify and archive obsolete code files that haven't been modified for 7+ days or are marked as deprecated.

## Scripts

### identify-archived.sh

Identifies files that should be archived based on modification date and deprecated markers.

**Location**: `.specify/scripts/bash/identify-archived.sh`

**Usage**:
```bash
./identify-archived.sh [options]
```

**Options**:
- `-d, --days N` - Check for files unmodified N+ days (default: 7)
- `-o, --output FILE` - Write output to FILE (default: stdout)
- `-j, --json` - Output as JSON instead of plain text
- `-h, --help` - Show help message

**Examples**:

```bash
# Identify files unmodified 7+ days, output to stdout
./identify-archived.sh

# Identify files unmodified 14+ days, save to file
./identify-archived.sh --days 14 --output old-files.txt

# Identify files unmodified 7+ days, output as JSON
./identify-archived.sh --json

# Identify and save with custom path
./.specify/scripts/bash/identify-archived.sh -o archive-list.txt
```

**Output Format**:

Plain text (one file per line):
```
./path/to/file1.rs
./path/to/file2.md
./tests/old_test.rs
```

JSON format:
```json
{
  "timestamp": "2025-12-14T21:15:30Z",
  "days_threshold": 7,
  "total_files": 396,
  "seven_day_old_count": 396,
  "deprecated_count": 0,
  "files": [
    "./path/to/file1.rs",
    "./path/to/file2.md"
  ]
}
```

**How It Works**:

1. **Phase 1: 7-Day Scan** - Finds all `.rs`, test files, and `.md` files unmodified for 7+ days
   - Uses `git log` to determine last modification date
   - Calculates cutoff date as N days ago
   - Compares file modification timestamps

2. **Phase 2: Deprecated Scan** - Finds all files marked with `#[deprecated]` attribute
   - Scans all `.rs` files
   - Searches for `#[deprecated]` Rust attribute
   - Includes regardless of modification date

3. **Phase 3: Consolidation** - Merges results
   - Combines 7-day and deprecated lists
   - Removes duplicates
   - Sorts alphabetically

4. **Phase 4: Output** - Formats results
   - Plain text: one file per line
   - JSON: structured output with metadata

### archive-files.sh

Moves identified files to `.archive/` directory and updates workspace.

**Location**: `.specify/scripts/bash/archive-files.sh`

**Usage**:
```bash
./archive-files.sh <file-list> [options]
```

**Arguments**:
- `file-list` - Path to file containing list of files to archive (one per line)

**Options**:
- `--dry-run` - Show what would be archived without making changes
- `--no-cargo-update` - Don't update Cargo.toml
- `--no-import-cleanup` - Don't remove import statements
- `-h, --help` - Show help message

**Examples**:

```bash
# Dry run to preview changes
./archive-files.sh archive-list.txt --dry-run

# Archive files
./scripts/archive-files.sh specs/009-cleanup-old-artifacts/sample-archive-list.txt

# Archive without updating Cargo.toml
./scripts/archive-files.sh archive-list.txt --no-cargo-update

# Archive and skip import cleanup
./scripts/archive-files.sh archive-list.txt --no-import-cleanup
```

**What It Does**:

1. **Creates Archive Directory** - Establishes `.archive/` if not present
2. **Moves Files** - Preserves original directory structure:
   ```
   .archive/
   ├── crates-ggen-domain-src-module.rs
   ├── crates-ggen-cli-tests-old.rs
   └── docs-legacy-api.md
   ```

3. **Creates Manifest** - Generates `.archive/MANIFEST.md`:
   ```markdown
   # Archive Manifest

   **Created**: 2025-12-14T21:15:30Z
   **Reason**: Archival of files unmodified 7+ days or marked deprecated

   ## Archived Files

   - **Original Path**: `./path/to/file.rs`
     - **Archived**: 2025-12-14T21:15:30Z
     - **Archive Path**: `.archive/path-to-file.rs`
     - **Reason**: unmodified 7+ days or marked deprecated
   ```

4. **Updates Configuration**:
   - Removes archived crates from `Cargo.toml` `[workspace]` members
   - Cleans up import statements in remaining code

5. **Preserves Git History** - Uses file operations that maintain full git history

## Identification Criteria

### 7-Day Unmodified Files

Files that haven't been changed for 7 or more days are candidates for archival. This includes:
- Source code files (`.rs`)
- Test files in `tests/` directories
- Documentation (`.md`)

**Why 7 days?**
- Long enough to capture truly stale code
- Short enough to archive inactive code regularly
- Aligns with typical development cycles

### Deprecated Files

Any file containing the Rust `#[deprecated]` attribute is archived regardless of modification date:

```rust
#[deprecated(since = "0.5.0", note = "use new_function instead")]
pub fn old_function() {
    // ...
}
```

## Workflow

### Step 1: Identify Files

```bash
./.specify/scripts/bash/identify-archived.sh -o archive-list.txt
```

**Output**: `archive-list.txt` with 396 files

### Step 2: Review (Optional)

```bash
# Preview what would be archived
./scripts/archive-files.sh archive-list.txt --dry-run

# View the list
cat archive-list.txt | head -20
```

### Step 3: Archive Files

```bash
./scripts/archive-files.sh archive-list.txt
```

**Output**:
- Files moved to `.archive/`
- `.archive/MANIFEST.md` created with inventory
- `Cargo.toml` updated
- Import statements cleaned

### Step 4: Verify

```bash
# Check archive structure
ls -la .archive/ | head -20

# View manifest
cat .archive/MANIFEST.md

# Check that git history is preserved
git log --follow -- .archive/
```

## Recovery

### Restore Individual Archived File

To restore a single archived file:

```bash
# Find the commit where it was archived
git log --follow -- .archive/path-to-file.rs

# Restore from archive (after archival)
git show <commit>:.archive/crates-old-lib.rs > crates/old/lib.rs
```

Or restore from git history directly (before archival):

```bash
# Get the file from before archival
git show HEAD~1:path/to/file.rs > path/to/file.rs
```

### Full Rollback (Undo All Archival)

To completely undo an archival operation:

```bash
# Find the archival commit
git log --oneline | grep -i archive

# Revert the archival commit (creates new commit undoing changes)
git revert <archival-commit>
```

This will:
- Restore all archived files to original locations
- Restore `Cargo.toml` to pre-archival state
- Restore all `use` statements in active code
- Create a new commit documenting the reversion

**Important**: This creates a new commit with the revert, it doesn't erase the archival commit from history.

### If Archival Went Wrong

If you need to abort immediately after archival (before committing):

```bash
# Restore filesystem to pre-archival state
git checkout HEAD

# This will:
# - Remove all files from .archive/
# - Restore original files to their locations
# - Restore Cargo.toml
# - Restore import statements
```

### Create Backup Tag (Optional)

Before archival, create a backup tag:

```bash
# Tag current state as backup
git tag backup-pre-archive-$(date +%Y%m%d)

# Push backup tag to remote
git push origin backup-pre-archive-$(date +%Y%m%d)
```

Then recovery is simple:

```bash
# Restore from backup tag
git checkout backup-pre-archive-20251214

# Or use it as recovery point
git show backup-pre-archive-20251214:path/to/file.rs > path/to/file.rs
```

## Assumptions & Constraints

### Assumptions

1. **Git Repository** - Scripts assume you're in a git repository (required for modification date detection)
2. **File Preservation** - `.archive/` directory is committed to git (files are preserved)
3. **Minimal Changes** - Only removes files from active code, doesn't refactor
4. **Deterministic Criteria** - Uses only modification date and deprecated marker, no subjective judgment

### Constraints

1. **One-Way Operation** - Archival is straightforward; recovery requires git history
2. **Manual Import Cleanup** - Complex import removals may need manual review
3. **Workspace-Only** - Works within ggen workspace; doesn't handle external dependencies
4. **Read Access** - Requires read access to all workspace files

## Archival Structure

Files are organized in `.archive/` preserving original paths:

```
.archive/
├── crates-ggen-cli-src-oldmodule.rs       ← from crates/ggen-cli/src/oldmodule.rs
├── crates-ggen-domain-tests-old-test.rs   ← from crates/ggen-domain/tests/old_test.rs
├── docs-legacy-api.md                      ← from docs/legacy-api.md
├── vendors-knhks-tests-data-file.ttl       ← from vendors/knhks/tests/data/file.ttl
└── MANIFEST.md                             ← Archive inventory
```

**Path Preservation Logic**:
- Replace `/` with `-` in original path
- Keep original filename
- Create archive path: `.archive/{directory-path-}-{filename}`

## Configuration

### Environment Variables

None currently required. Scripts use git and system utilities.

### Dependencies

Required system utilities:
- `git` - For modification date detection and history
- `find` - For file discovery
- `grep` - For deprecated marker detection
- `stat` or `gstat` - For file timestamp comparison (platform-specific)

### Platform Support

- **Linux/Unix**: Fully supported
- **macOS**: Supported (uses `gstat` from GNU coreutils if available)
- **Windows (Git Bash)**: Should work with MSYS2 utilities

## Troubleshooting

### "Not a git repository" Error

```bash
# Ensure you're in the ggen workspace root
cd /path/to/ggen
git rev-parse --git-dir  # Should show .git/
```

### No Files Found

```bash
# Check that workspace has .rs, test, and .md files
find . -name "*.rs" | wc -l
find . -name "*.md" | wc -l

# Verify modification dates
stat some-file.rs | grep Modify
```

### Archive Script Fails

```bash
# Try dry-run first
./scripts/archive-files.sh archive-list.txt --dry-run

# Check permissions on .archive/
ls -la .archive/ 2>/dev/null || echo "Not created yet"

# Verify file list exists
wc -l archive-list.txt
```

### Git History Issues

```bash
# Check what happened to archived files
git log --all --full-history -- .archive/

# Verify git can access the files
git ls-files | grep archive | wc -l
```

## Examples

### Archive Files Updated Before Today

```bash
# Find files modified more than 7 days ago
./.specify/scripts/bash/identify-archived.sh --days 7 -o old-files.txt

# Archive them
./scripts/archive-files.sh old-files.txt

# See what was archived
git status
cat .archive/MANIFEST.md
```

### Dry Run Before Committing

```bash
# Preview changes
./scripts/archive-files.sh archive-list.txt --dry-run

# Review the output, then run for real
./scripts/archive-files.sh archive-list.txt

# Commit results
git add .archive/ Cargo.toml
git commit -m "Archive old code: $(wc -l < archive-list.txt) files"
```

### Selective Archival

```bash
# Create custom list of specific files
cat > my-files.txt << EOF
crates/old-crate/src/lib.rs
docs/legacy.md
tests/deprecated_test.rs
EOF

# Archive only these files
./scripts/archive-files.sh my-files.txt
```

## Safety Notes

⚠️ **Before Running Archival**:

1. **Commit all work** - Ensure clean git working tree
2. **Dry run first** - Review what would be archived
3. **Have backups** - Full git history is preserved, but commits represent snapshots
4. **Communicate** - Let team know before archiving in shared repository

## Questions?

For more information:
- See `.specify/specs/009-cleanup-old-artifacts/ARCHIVAL-GUIDE.md` for full process documentation
- Check `.archive/MANIFEST.md` for inventory after archival
- Use `git log --follow` to find archived files in history
