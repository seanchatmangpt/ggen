# WIP Queue System - Deterministic Agent Workflow

## Overview

The WIP (Work In Progress) Queue System provides a deterministic, DFS-based task management system for multi-agent workflows. It ensures agents always pick the next unimplemented item, work it to completion, and move on without duplication or looping.

## Key Features

1. **Deterministic DFS Traversal** - Tasks are discovered in lexicographic, depth-first order
2. **Advisory File Locks** - Prevents multiple agents from working on the same file
3. **Standard Taxonomy** - Consistent markers across code, tests, and docs
4. **Single Source of Truth** - Binary queue tool enumerates all tasks
5. **Lifecycle Integration** - Seamless integration with ggen lifecycle system

## WIP Taxonomy

### Standard Markers

Use these exact markers consistently across all code:

```rust
// WIP: Short description of what's missing
// TODO: Short description of what needs to be done
// UNIMPL: Short description of unimplemented feature

// Or in Rust code:
unimplemented!("WIP: short description")
todo!("WIP: short description")
```

### Completion Markers

When a task is complete, flip the marker:

```rust
// WIP: implement authentication → // READY: authentication implemented
// Or simply remove the marker entirely
```

## Architecture

### 1. wipqueue Tool

Located in `wipqueue/`, this Rust binary:
- Scans the codebase for WIP markers
- Traverses in deterministic DFS order
- Checks lock status for each task
- Outputs JSON array of tasks (unlocked first)

**Usage:**
```bash
cargo run -p wipqueue --quiet
```

**Output:**
```json
[
  {
    "path": "src/main.rs",
    "line": 42,
    "col": 5,
    "kind": "WIP",
    "text": "implement authentication",
    "lock_path": ".wiplocks/abc123def456",
    "locked": false
  }
]
```

### 2. wip-lock Script

Located in `scripts/wip-lock`, this bash script manages advisory locks:

**Commands:**
```bash
# Claim exclusive lock on a file
./scripts/wip-lock claim "src/main.rs" "42"

# Release lock
./scripts/wip-lock release "src/main.rs"

# Check lock status
./scripts/wip-lock status "src/main.rs"
```

**Lock Storage:**
- Locks stored in `.wiplocks/<sha1(path)>/`
- Lock is a directory (creation = lock, removal = unlock)
- Metadata: `owner`, `timestamp`, `pid`

### 3. Lifecycle Integration

The WIP system integrates with ggen's lifecycle system via `make.toml`:

```bash
# Get next WIP task and claim it
ggen lifecycle run wip.next

# Release WIP lock when done
ggen lifecycle run wip.done
```

These commands:
1. Query wipqueue for next task
2. Create advisory lock
3. Create feature branch
4. Track progress

## Agent Workflow

### Minimal Agent Runbook

```bash
# 1. Get the queue
cargo run -p wipqueue --quiet

# 2. Claim the first unlocked task
./scripts/wip-lock claim "<path>" "<line>"

# 3. Create branch
git switch -c "wip/$(echo "<path>" | tr '/' '__')__L<line>"

# 4. Implement
# - Replace the specific marker with working code
# - Add or extend tests
# - Run: cargo fmt && cargo clippy -D warnings && cargo test

# 5. Flip marker
# - Change `// WIP:` to `// READY:` (or remove)
# - Commit with Conventional Commit:
#   feat(<area>): complete WIP at <path>:<line>

# 6. Release lock
./scripts/wip-lock release "<path>"

# 7. Open draft PR with title:
#   WIP: <area>: complete <file>:L<line>
```

### Full Workflow Example

```bash
# Start new task
ggen lifecycle run wip.next
# Output: claimed src/auth.rs:15

# Implement the feature
# Edit src/auth.rs line 15
# Write tests
# Run validation
cargo fmt
cargo clippy -D warnings
cargo test

# Commit changes
git add src/auth.rs tests/auth_test.rs
git commit -m "feat(auth): complete WIP at src/auth.rs:15

Implemented OAuth2 authentication flow.

- Added token validation
- Added refresh token logic
- Added comprehensive tests"

# Release lock and push
ggen lifecycle run wip.done
git push -u origin wip/src__auth.rs__L15

# Create PR via GitHub CLI
gh pr create --draft --title "WIP: auth: complete src/auth.rs:L15"
```

## Deterministic DFS Order

### Directory Traversal

1. Lexicographic ordering of paths
2. Depth-first: visit children before siblings
3. Stable: same order every run

### Example Order

```
.
├── cli/           # Visited first
│   ├── src/
│   │   ├── commands.rs  # Line 10
│   │   └── main.rs      # Line 42
│   └── tests/
├── ggen-core/     # Visited second
│   └── src/
│       └── lib.rs       # Line 5
└── src/
    └── main.rs          # Line 100
```

**Queue Order:**
1. `cli/src/commands.rs:10`
2. `cli/src/main.rs:42`
3. `ggen-core/src/lib.rs:5`
4. `src/main.rs:100`

## Lock Semantics

### Advisory Locks

- **Creation**: `mkdir .wiplocks/<sha1(path)>` (atomic)
- **Check**: Test if directory exists
- **Release**: `rm -rf .wiplocks/<sha1(path)>`

### Lock Metadata

Each lock directory contains:
- `owner`: Path and line number (e.g., `src/main.rs:42`)
- `timestamp`: ISO 8601 timestamp
- `pid`: Process ID of locking agent

### Lock Behavior

- **Claim fails**: If lock exists, agent skips to next task
- **Stale locks**: Manual cleanup required (future: timeout)
- **Unlock**: Agent must release before moving to next task

## Guardrails

### CI Checks

Two critical CI checks prevent loops:

**Check 1: Marker Flip Validation**
```bash
# Branch name contains __L<line>
# That line must change from WIP → READY or be removed
./scripts/ci/check-wip-flip.sh
```

**Check 2: Lock Cleanup Validation**
```bash
# .wiplocks/ must be empty after job
test -z "$(find .wiplocks -mindepth 1 -maxdepth 1 -type d)"
```

### Editorial Rules

1. **Placement**: Markers at exact location of missing behavior
2. **Granularity**: One marker per missing behavior
3. **Code stubs over comments**: Prefer code when possible:

   ```rust
   // WIP: enforce Offline net profile
   return Err(CleanroomError::Policy(
       PolicyError::Msg("net profile not enforced yet".into())
   ));
   ```

4. **Update on completion**: Flip or remove the specific marker line

## Integration with ggen Lifecycle

### Phase Definitions

In `make.toml`:

```toml
[phases.wip.next]
description = "Get next WIP task and claim it"
commands = [
    "cargo run -p wipqueue --quiet | jq -r '.[0] | @base64' > .wip_next || true",
    "if [ ! -s .wip_next ]; then echo 'no WIP found'; exit 0; fi",
    "data=$(cat .wip_next)",
    "path=$(echo $data | base64 -d | jq -r .path)",
    "line=$(echo $data | base64 -d | jq -r .line)",
    "./scripts/wip-lock claim \"$path\" \"$line\"",
    "git switch -c \"wip/$(echo \"$path\" | tr '/' '__')__L$line\"",
    "echo \"claimed $path:$line\""
]

[phases.wip.done]
description = "Release WIP lock"
commands = [
    "test -f .wip_next || { echo 'no task claimed'; exit 1; }",
    "data=$(cat .wip_next)",
    "path=$(echo $data | base64 -d | jq -r .path)",
    "./scripts/wip-lock release \"$path\"",
    "rm -f .wip_next"
]
```

### Pipeline Integration

```toml
[pipelines]
wip = ["wip.next"]
```

## Benefits

1. **Zero Duplication**: Lock prevents multiple agents working same file
2. **Deterministic**: Same order every run
3. **Complete Coverage**: DFS ensures all WIP markers found
4. **Simple**: Minimal dependencies (Rust, bash, git, jq)
5. **Auditable**: Lock metadata tracks who/when/what
6. **CI-Safe**: Guardrails prevent incomplete work merging

## Future Enhancements

1. **Lock Timeouts**: Auto-release stale locks
2. **Parallel Work**: Allow multiple agents on different files
3. **Priority System**: High-priority tasks first
4. **Web Dashboard**: Visual task queue and lock status
5. **Agent Coordination**: Distributed lock server

## Troubleshooting

### Problem: Lock won't release

```bash
# Check lock status
./scripts/wip-lock status "path/to/file.rs"

# Manually remove stale lock
rm -rf .wiplocks/<sha1(path)>
```

### Problem: No tasks found

```bash
# Verify markers exist
rg "// WIP:|// TODO:|// UNIMPL:|unimplemented!|todo!"

# Run wipqueue with verbose output
cargo run -p wipqueue
```

### Problem: Tasks out of order

- Ensure consistent line endings (LF not CRLF)
- Check .gitignore patterns
- Verify lexicographic sorting

## Best Practices

1. **Small Tasks**: Keep WIP items focused and small
2. **Test First**: Add test case before removing WIP marker
3. **Commit Often**: Commit after each WIP completion
4. **Document Intent**: WIP message should explain what's missing
5. **Clean Branches**: One WIP per branch/PR

## References

- [wipqueue source](../wipqueue/src/main.rs)
- [wip-lock script](../scripts/wip-lock)
- [make.toml phases](../make.toml)
- [ggen lifecycle docs](./lifecycle.md)

---

**Remember**: The WIP queue is your single source of truth for what needs to be done. Trust the queue, claim your task, complete it, and move on. No loops, no duplicates, just deterministic progress.
