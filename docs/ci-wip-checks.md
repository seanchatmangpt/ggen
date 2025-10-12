# CI WIP Enforcement Checks

## Overview

Three CI checks enforce the WIP queue workflow and prevent looping:

1. **check-wip-marker.sh** - Verify WIP marker was flipped or removed
2. **check-wip-locks.sh** - Verify no locks remain after PR
3. **check-no-new-wip.sh** - Warn about new WIP markers in unrelated files

## Check 1: WIP Marker Change

### Purpose
Ensures that WIP branches actually address the marker they claim to fix.

### Logic

```bash
# Extract line number from branch name
# Branch format: wip/<path>__L<line>

# Check that the line at <line> in <path>:
# - Previously contained a WIP marker (WIP:, TODO:, UNIMPL:, etc.)
# - Now either:
#   a) Contains "// READY:" instead
#   b) Is different (marker removed)
#   c) File structure changed significantly
```

### Usage

```bash
./scripts/ci/check-wip-marker.sh
```

### Exit Codes

- `0` - Pass: Marker was changed or removed
- `1` - Fail: Marker still present or line unchanged

### Example Output

**Success:**
```
🔍 Checking WIP marker change in src/auth.rs:42
✅ PASS: WIP marker was modified
Base:    // WIP: implement JWT validation
Current: // READY: JWT validation with RS256 support
```

**Failure:**
```
🔍 Checking WIP marker change in src/auth.rs:42
❌ FAIL: WIP marker still present at src/auth.rs:42
Base:    // WIP: implement JWT validation
Current: // WIP: implement JWT validation

The WIP marker must be either:
  1. Changed to '// READY: <description>'
  2. Removed entirely
```

## Check 2: Lock Cleanup

### Purpose
Ensures all locks are released before merging, preventing lock leaks.

### Logic

```bash
# Check if .wiplocks/ directory exists
# Count lock directories (exclude .gitignore)
# Fail if any locks remain
```

### Usage

```bash
./scripts/ci/check-wip-locks.sh
```

### Exit Codes

- `0` - Pass: No locks remain
- `1` - Fail: Locks still exist

### Example Output

**Success:**
```
🔍 Checking for remaining WIP locks in .wiplocks/
✅ PASS: No locks found in .wiplocks/
```

**Failure:**
```
🔍 Checking for remaining WIP locks in .wiplocks/
❌ FAIL: Found 2 remaining lock(s) in .wiplocks/

Remaining locks:
  - a1b2c3d4e5f6 (src/auth.rs:42)
  - f6e5d4c3b2a1 (src/db.rs:15)

Locks must be released before merging. Run:
  ./scripts/wip-lock release <path>
```

## Check 3: No New WIP Markers

### Purpose
Warns about new WIP markers added in files other than the claimed file.

### Logic

```bash
# Get list of modified files
# For each file (except claimed file):
#   - Count WIP markers in base branch
#   - Count WIP markers in current branch
#   - Warn if current > base
```

### Usage

```bash
./scripts/ci/check-no-new-wip.sh
```

### Exit Codes

- `0` - Always pass (warning only)

### Example Output

**No new markers:**
```
🔍 Checking for new WIP markers in modified files
✅ PASS: No new WIP markers added outside claimed file
```

**New markers found:**
```
🔍 Checking for new WIP markers in modified files
⚠️  Warning: New WIP markers added in src/utils.rs
Base: 2 markers, Current: 4 markers

New markers:
> 25:  // WIP: add validation
> 68:  // TODO: optimize performance

⚠️  New WIP markers were added outside the claimed file.
This is allowed but should be intentional.
Consider creating separate tasks for these WIP items.
```

## GitHub Actions Integration

### Example Workflow

```yaml
name: WIP Queue Checks

on:
  pull_request:
    branches: [main, master]

jobs:
  wip-checks:
    name: WIP Queue Enforcement
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0  # Need full history for diffs

      - name: Make scripts executable
        run: |
          chmod +x scripts/ci/*.sh

      - name: Check WIP marker changed
        run: ./scripts/ci/check-wip-marker.sh

      - name: Check locks cleaned up
        run: ./scripts/ci/check-wip-locks.sh

      - name: Check no new WIP markers
        run: ./scripts/ci/check-no-new-wip.sh
        continue-on-error: true  # Warning only
```

### Required Permissions

```yaml
permissions:
  contents: read
  pull-requests: read
```

## Local Testing

### Test All Checks

```bash
# Run all checks locally
./scripts/ci/check-wip-marker.sh
./scripts/ci/check-wip-locks.sh
./scripts/ci/check-no-new-wip.sh
```

### Test Specific Branch

```bash
# Checkout WIP branch
git checkout wip/src__auth.rs__L42

# Verify marker changed
./scripts/ci/check-wip-marker.sh

# Verify no locks
./scripts/ci/check-wip-locks.sh
```

## Troubleshooting

### False Positive: File Structure Changed

**Symptom**: Check fails because file was refactored and line numbers changed

**Solution**: Add exception logic or manually verify the change
```bash
# Check if WIP was truly addressed
git diff origin/master...HEAD | grep -C 5 "WIP:"
```

### False Positive: Lock Stuck

**Symptom**: Lock remains after legitimate release

**Solution**: Manually clean locks
```bash
rm -rf .wiplocks/*
git commit -am "chore: clean up stuck locks"
```

### Check Not Running

**Symptom**: CI passes but checks didn't run

**Solution**: Verify branch name format
```bash
# Must match: wip/<path>__L<line>
git branch --show-current
```

## Best Practices

### For PR Authors

1. Always run checks locally before pushing
2. Release locks immediately after implementation
3. Don't modify unrelated files in WIP PRs
4. Use `// READY:` to clearly mark completion

### For Reviewers

1. Verify CI checks passed
2. Manually verify WIP was addressed
3. Check that tests cover the new code
4. Ensure no locks remain

### For Maintainers

1. Keep CI scripts up to date
2. Monitor for stuck locks
3. Review new WIP markers regularly
4. Adjust checks based on team feedback

## See Also

- [WIP Queue System](./wip-queue.md) - Architecture and design
- [Agent Runbook](./wip-agent-runbook.md) - Agent workflow
- [Lifecycle Guide](./lifecycle.md) - Ggen lifecycle system
