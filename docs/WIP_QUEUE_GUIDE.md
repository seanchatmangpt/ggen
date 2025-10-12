# WIP Queue System Guide

## Overview

The WIP Queue system provides deterministic task management with file locks to prevent multiple agents from working on the same task.

## Markers

Use these standard markers in code:

```rust
// WIP: implement timeout mechanism
// TODO: add validation for edge cases
// UNIMPL: network isolation not yet implemented

unimplemented!("WIP: add PostgreSQL health check")
todo!("WIP: implement retry logic")
```

## Workflow

### 1. Get Next Task

```bash
cargo make wip-next
```

This will:
- Query the WIP queue in DFS order
- Claim the first unlocked task
- Create a branch: `wip/<path>__L<line>`
- Lock the file to prevent concurrent work

### 2. Implement

Work on the specific task:
- Replace the WIP marker with working code
- Add or extend tests
- Run: `cargo fmt && cargo clippy && cargo test`

### 3. Complete

```bash
# Flip marker from WIP to READY (or remove it)
# Commit changes
git add .
git commit -m "feat(area): complete WIP at <path>:<line>"

# Release lock
cargo make wip-done

# Open PR
gh pr create --draft --title "WIP: <area>: complete <file>:L<line>"
```

## Commands

- `cargo make wip-next` - Claim next task
- `cargo make wip-done` - Release current task
- `cargo make wip-status` - Show queue status
- `cargo make wip-list` - List all tasks (JSON)
- `cargo make wip-clean` - Clear all locks (emergency use)

## Queue Binary

```bash
# Run directly
cargo run -p wipqueue

# Output is JSON array of tasks in DFS order
```

## Lock Mechanism

Locks are advisory directories in `.wiplocks/`:
- Creating directory = claiming lock
- Directory exists = task is locked
- Removing directory = releasing lock

Lock paths are SHA1 hashes of file paths for uniqueness.

## Traversal Order

DFS (Depth-First Search) with lexicographic ordering:
1. Directories sorted alphabetically
2. Files within each directory sorted alphabetically
3. Lines within files processed top-to-bottom

## Completion Rules

When completing a WIP task:

1. **Remove or flip marker** - Change `// WIP:` to `// READY:` or remove entirely
2. **Add tests** - Ensure behavior is tested
3. **Single responsibility** - One WIP marker per PR
4. **Clean commit message** - Use Conventional Commits format

## Agent Integration

Agents should:

1. Always run `cargo make wip-next` before starting work
2. Never modify locked files
3. Release lock with `cargo make wip-done` after completion
4. Create draft PR with descriptive title referencing line number

## CI Integration

Recommended CI checks:

```yaml
- name: Verify WIP completion
  run: |
    # Check branch contains __L<line> and that line was changed
    if [[ $GITHUB_REF == *"wip/"* ]]; then
      branch=${GITHUB_REF##*/}
      line=$(echo $branch | grep -o 'L[0-9]*' | sed 's/L//')
      # Verify marker was flipped or removed at that line
    fi

- name: Verify no active locks
  run: |
    if [ "$(ls -A .wiplocks 2>/dev/null | grep -v .gitkeep)" ]; then
      echo "Error: Active locks found in .wiplocks/"
      exit 1
    fi
```

## Troubleshooting

### "Already locked" error

Another agent is working on this task. Either:
- Wait for them to finish
- Pick a different task with `cargo make wip-next` again
- If stale, use `cargo make wip-clean` (caution!)

### "No WIP found"

All tasks are either:
- Complete (markers removed/flipped)
- Currently locked by active work

Check status: `cargo make wip-status`

### Queue not finding markers

Ensure markers follow exact format:
- `// WIP: <description>` (note colon and space)
- `unimplemented!("WIP: <description>")`
- `todo!("WIP: <description>")`

## Best Practices

1. **Granular markers** - One marker per missing behavior
2. **Specific locations** - Place marker where code belongs, not at file top
3. **Clear descriptions** - "implement timeout" not "fix this"
4. **Test coverage** - Add tests when completing WIP
5. **Small PRs** - One WIP marker per PR for easy review
