# WIP Queue System Setup

## Overview

The WIP Queue system has been successfully integrated into ggen. This system ensures agents always pick the next unimplemented item via deterministic DFS traversal, work it to completion, and move on without looping.

## Components Installed

### 1. Queue Tool (`scripts/wipqueue`)

A Rust binary that scans the codebase for WIP markers and outputs tasks in deterministic DFS order.

**Location**: `scripts/wipqueue/`

**Usage**:
```bash
# Get all tasks (unlocked first, then locked)
cargo run -p wipqueue --quiet

# Get first unlocked task
cargo run -p wipqueue --quiet | jq -r '.[0]'

# Pretty print all tasks
cargo run -p wipqueue --quiet | jq
```

**Build**:
```bash
cargo build -p wipqueue --release
```

The compiled binary will be at: `target/release/wipqueue`

### 2. Lock Manager (`scripts/wip-lock`)

A bash script for claiming and releasing file locks.

**Location**: `scripts/wip-lock`

**Usage**:
```bash
# Claim a file lock
./scripts/wip-lock claim "path/to/file.rs" "42"

# Release a file lock
./scripts/wip-lock release "path/to/file.rs"

# Check lock status
./scripts/wip-lock status "path/to/file.rs"
```

### 3. CI Validation Script (`scripts/validate-wip-markers.sh`)

A script to validate WIP markers in CI/CD pipelines.

**Location**: `scripts/validate-wip-markers.sh`

**Usage**:
```bash
# Run validation (typically in CI)
./scripts/validate-wip-markers.sh
```

**Checks**:
1. Branch name contains `__L<line>` and that line was changed
2. No unreleased locks in `.wiplocks/`
3. No unintentional new WIP markers

### 4. Documentation

**Agent Runbook**: `docs/wip-queue-agent-runbook.md`
- Complete agent workflow guide
- Standard WIP marker taxonomy
- Example workflows
- Troubleshooting

**This Setup Guide**: `docs/wip-queue-setup.md`

### 5. Lock Directory

**Location**: `.wiplocks/`

Contains ephemeral lock files (not committed to git).

- `.wiplocks/.gitkeep` - Ensures directory is tracked
- `.wiplocks/<sha1_hash>/` - Individual file locks

## Configuration

### Cargo Workspace

The `wipqueue` crate has been added to the workspace in `Cargo.toml`:

```toml
[workspace]
members = [
  # ... other members ...
  "scripts/wipqueue",
]
```

### Git Ignore

The `.wiplocks/` directory is automatically ignored:

```gitignore
# WIP queue lock files
.wiplocks/
```

## Standard WIP Markers

Use these markers consistently across the codebase:

- `// WIP: <description>` - Work in progress
- `// TODO: <description>` - Todo item
- `// UNIMPL: <description>` - Unimplemented feature
- `unimplemented!("WIP: <description>")` - Rust macro
- `todo!("WIP: <description>")` - Rust macro

**Completion markers**:
- `// READY: <description>` - Task completed
- Or remove the marker entirely

## Quick Start for Agents

### 1. Get Next Task
```bash
task=$(cargo run -p wipqueue --quiet | jq -r '.[0]')
path=$(echo "$task" | jq -r '.path')
line=$(echo "$task" | jq -r '.line')
echo "Next task: $path:$line"
```

### 2. Claim Lock
```bash
./scripts/wip-lock claim "$path" "$line"
```

### 3. Create Branch
```bash
branch="wip/$(echo "$path" | tr '/' '__')__L$line"
git switch -c "$branch"
```

### 4. Implement & Test
```bash
# Edit the file, replace WIP marker
# Run tests
cargo fmt
cargo clippy -D warnings
cargo test  # or cargo nextest run
```

### 5. Commit
```bash
git add "$path"
git commit -m "feat: complete WIP at $path:$line"
```

### 6. Release Lock
```bash
./scripts/wip-lock release "$path"
```

### 7. Push & PR
```bash
git push -u origin "$branch"
gh pr create --draft --title "WIP: complete $path:L$line"
```

## Integration with CI

Add to your CI workflow (e.g., `.github/workflows/ci.yml`):

```yaml
- name: Validate WIP Markers
  run: ./scripts/validate-wip-markers.sh
```

This ensures:
- WIP branches properly flip or remove markers
- No unreleased locks
- No accidental new WIP markers

## Current WIP Tasks

To see all current WIP tasks in the codebase:

```bash
cargo run -p wipqueue --quiet | jq -r '.[] | "\(.path):\(.line) - \(.text)"'
```

Example output:
```
.github/workflows/marketplace.yml:99 - Parse TOML and display dynamically
cleanroom/src/artifacts.rs:246 - Collect coverage data
cleanroom/src/artifacts.rs:247 - Calculate actual coverage
```

## Troubleshooting

### Compilation Errors

If `wipqueue` fails to compile:

```bash
cd scripts/wipqueue
cargo check
cargo build
```

### Lock Issues

If a file is unexpectedly locked:

```bash
# Check lock status
./scripts/wip-lock status "path/to/file.rs"

# Force release (use with caution)
rm -rf .wiplocks/<hash>

# Or release all locks
rm -rf .wiplocks/*
```

### No Tasks Found

If the queue returns empty:

```bash
# Check for WIP markers manually
grep -r "WIP:" . --include="*.rs"
grep -r "TODO:" . --include="*.rs"
```

## Benefits

- **No Duplicate Work**: File locks prevent collisions
- **Deterministic Order**: DFS ensures consistent prioritization
- **Progress Tracking**: Clear completion markers
- **Small PRs**: One task per PR
- **No Looping**: Completed tasks are skipped
- **Clear History**: Git branches encode task location

## Next Steps

1. **Add More WIP Markers**: Identify unimplemented features and mark them
2. **CI Integration**: Add validation to your CI pipeline
3. **Agent Training**: Familiarize agents with the workflow
4. **Lifecycle Hooks**: Integrate with ggen lifecycle commands

## Resources

- **Agent Runbook**: `docs/wip-queue-agent-runbook.md`
- **Wipqueue Source**: `scripts/wipqueue/src/main.rs`
- **Lock Manager**: `scripts/wip-lock`
- **CI Validation**: `scripts/validate-wip-markers.sh`

---

**Setup completed successfully!** ✅
