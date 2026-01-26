# Patch All Packages Script

## Overview

The `patch-all-packages.sh` script provides a unified way to update and patch all workspace packages (crates) consistently. It ensures version consistency across all crates and can update dependencies.

## Features

- ✅ **Version Synchronization**: Update all package versions to match
- ✅ **Validation**: Verify version consistency across all crates
- ✅ **Dry Run**: Preview changes before applying
- ✅ **Workspace Aware**: Automatically finds all workspace members
- ✅ **Safe**: Validates changes after applying

## Usage

### Validate Version Consistency

Check if all package versions are consistent:

```bash
./scripts/patch-all-packages.sh --validate-only
```

### Update All Versions

Update all package versions to a new version:

```bash
./scripts/patch-all-packages.sh --version 5.2.0
```

### Dry Run (Preview Changes)

See what would change without making changes:

```bash
./scripts/patch-all-packages.sh --version 5.2.0 --dry-run
```

### Update Dependencies

Update workspace dependencies (future feature):

```bash
./scripts/patch-all-packages.sh --update-deps
```

## Examples

### Example 1: Version Bump for Release

```bash
# Preview the changes
./scripts/patch-all-packages.sh --version 5.2.0 --dry-run

# Apply the changes
./scripts/patch-all-packages.sh --version 5.2.0

# Verify
./scripts/patch-all-packages.sh --validate-only
```

### Example 2: Validate Before Release

```bash
# Check all versions are consistent
./scripts/patch-all-packages.sh --validate-only
```

### Example 3: Patch Specific Type

```bash
# Only update versions
./scripts/patch-all-packages.sh --version 5.2.0 --patch-type version

# Only update dependencies
./scripts/patch-all-packages.sh --update-deps --patch-type deps
```

## What Gets Updated

When updating versions, the script updates:

1. **Root Cargo.toml** - Main package version
2. **VERSION file** - Version file (if exists)
3. **All workspace crates** - Every crate in `[workspace.members]`:
   - `crates/ggen-utils`
   - `crates/ggen-cli`
   - `crates/ggen-domain`
   - `crates/ggen-core`
   - `crates/ggen-config`
   - `crates/ggen-cli-validation`
   - `crates/ggen-config-clap`
   - `crates/ggen-marketplace`
   - `crates/ggen-test-audit`
   - `crates/ggen-test-opt`
   - `crates/ggen-e2e`
   - `crates/ggen-node`
   - `crates/ggen-macros`
   - `crates/ggen-api`
   - `crates/ggen-auth`
   - `crates/ggen-payments`
   - `crates/ggen-saas`
   - `playground`

## Validation

The script validates:

- ✅ Root `Cargo.toml` version matches `VERSION` file
- ✅ All workspace crates have the same version
- ✅ No version mismatches

## Workflow

### Before Release

```bash
# 1. Validate current state
./scripts/patch-all-packages.sh --validate-only

# 2. Update to new version
./scripts/patch-all-packages.sh --version 5.2.0

# 3. Verify changes
git diff

# 4. Test build
cargo make check

# 5. Commit
git commit -m "chore: bump version to 5.2.0"
```

### After Version Update

```bash
# Verify everything is consistent
./scripts/patch-all-packages.sh --validate-only
```

## Options

| Option | Description |
|--------|-------------|
| `--version VERSION` | Update all package versions to VERSION |
| `--update-deps` | Update workspace dependency versions |
| `--validate-only` | Only validate, don't make changes |
| `--dry-run` | Show what would be done without making changes |
| `--patch-type TYPE` | Type of patch: 'version', 'deps', 'all' (default: all) |
| `-h, --help` | Show help message |

## Integration with Release Process

This script is designed to work with the release process:

1. **Version Bump**: Use this script to bump versions
2. **Publish**: Publish to crates.io
3. **Homebrew**: Update Homebrew formula (uses version from Cargo.toml)

## Troubleshooting

### Version Mismatch Error

If you see version mismatch errors:

```bash
# Check current versions
./scripts/patch-all-packages.sh --validate-only

# Fix by updating all to same version
./scripts/patch-all-packages.sh --version 5.1.0
```

### Crate Not Found

If a crate is not found, check:
- The crate is listed in `[workspace.members]` in root `Cargo.toml`
- The crate has a `Cargo.toml` file
- The path is correct

### Dry Run Shows No Changes

If dry run shows no changes but validation fails:
- Check for trailing whitespace in version strings
- Verify version format is correct (e.g., "5.1.0" not "5.1.0 ")
- Check for comments or other text on version lines

## Related Scripts

- `scripts/release-validate-version.sh` - Legacy version validation
- `scripts/update-homebrew-formula-e2e.sh` - Update Homebrew formula
- `scripts/validate-homebrew-formula.sh` - Validate Homebrew formula

