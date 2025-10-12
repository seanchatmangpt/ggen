# WIP Queue System - Deterministic DFS Task Management

## Overview

The WIP (Work In Progress) queue system ensures agents always pick the next unimplemented item, work it to completion, and move on without looping. This system provides:

1. **Deterministic DFS traversal** - Lexicographic, depth-first ordering
2. **File-scoped advisory locks** - Prevents concurrent work on same files
3. **Single source of truth** - Central queue managed by `wipqueue` binary
4. **Lifecycle integration** - Seamless integration with ggen's lifecycle system

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      WIP Queue System                        │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌──────────────┐     ┌──────────────┐     ┌──────────────┐│
│  │  wipqueue    │────▶│  .wiplocks/  │◀────│  wip-lock    ││
│  │   (Rust)     │     │  (advisory)  │     │   (bash)     ││
│  └──────────────┘     └──────────────┘     └──────────────┘│
│         │                                           │        │
│         │                                           │        │
│         ▼                                           ▼        │
│  ┌──────────────┐                          ┌──────────────┐│
│  │ DFS Traversal│                          │ Lock Manager ││
│  │  + Filtering │                          │   (claim/    ││
│  │              │                          │   release)   ││
│  └──────────────┘                          └──────────────┘│
│         │                                           │        │
│         └───────────────────┬───────────────────────┘        │
│                             ▼                                │
│                  ┌──────────────────────┐                   │
│                  │  make.toml phases:   │                   │
│                  │  - wip.next          │                   │
│                  │  - wip.done          │                   │
│                  └──────────────────────┘                   │
└─────────────────────────────────────────────────────────────┘
```

## WIP Taxonomy

Use these exact markers across code, tests, and docs:

### Line Comments
```rust
// WIP: <short task>
// TODO: <short task>
// UNIMPL: <short task>
```

### Macro Markers
```rust
unimplemented!("WIP: <short task>")
todo!("WIP: <short task>")
```

### Completion Markers
When a task is complete, flip to:
```rust
// READY: <summary>
```
Or simply remove the marker entirely.

## DFS Traversal Rules

The `wipqueue` tool uses deterministic ordering:

1. **Directory order**: Lexicographic, depth-first (stack-based)
2. **File order**: Lexicographic within each directory
3. **In-file order**: Top-to-bottom line order

Example traversal:
```
src/
  main.rs (L10, L50, L100)
  lib.rs (L20)
  utils/
    helpers.rs (L15)
    validators.rs (L30)
tests/
  integration.rs (L40)
```

## File Locks

### Lock Semantics

- **Lock path**: `.wiplocks/<sha1(path)>`
- **Lock creation**: `mkdir` creates the lock (atomic operation)
- **Lock release**: `rm -rf` removes the lock
- **Agents must**: Skip locked files when selecting tasks

### Lock Management

```bash
# Claim a lock
./scripts/wip-lock claim "src/main.rs" "42"
# Output: claimed: src/main.rs:42

# Release a lock
./scripts/wip-lock release "src/main.rs"
# Output: released: src/main.rs
```

## Queue Tool Usage

### Get Next Task

```bash
cargo run -p wipqueue --quiet
```

Output (JSON):
```json
[
  {
    "path": "src/main.rs",
    "line": 42,
    "col": 5,
    "kind": "WIP",
    "text": "implement authentication middleware",
    "lock_path": ".wiplocks/a1b2c3d4e5f6..."
  },
  ...
]
```

### Claiming First Task

```bash
# Get the queue
QUEUE=$(cargo run -p wipqueue --quiet)

# Extract first task
FIRST=$(echo "$QUEUE" | jq -r '.[0]')
PATH=$(echo "$FIRST" | jq -r '.path')
LINE=$(echo "$FIRST" | jq -r '.line')

# Claim it
./scripts/wip-lock claim "$PATH" "$LINE"
```

## Lifecycle Integration

### Using make.toml Phases

```bash
# Get next WIP task and claim it
ggen lifecycle run wip.next

# After completing work, release the lock
ggen lifecycle run wip.done
```

### What wip.next Does

1. Runs `wipqueue` to get all tasks
2. Extracts the first unlocked task
3. Creates lock for that task
4. Creates git branch: `wip/<path>__L<line>`
5. Saves task info to `.wip_next`

### What wip.done Does

1. Reads task info from `.wip_next`
2. Releases the lock
3. Cleans up `.wip_next` file

## Agent Workflow

See [docs/wip-agent-runbook.md](./wip-agent-runbook.md) for the complete agent workflow.

Quick summary:
1. Get queue: `cargo run -p wipqueue --quiet`
2. Claim task: `./scripts/wip-lock claim <path> <line>`
3. Create branch: `git switch -c wip/<path>__L<line>`
4. Implement and test
5. Flip marker to READY or remove it
6. Commit with Conventional Commit
7. Release lock: `./scripts/wip-lock release <path>`
8. Open draft PR

## Guardrails to Prevent Loops

### Agent Rules

1. **MUST** call `cargo run -p wipqueue` before opening any file
2. **MUST** lock before editing
3. **MUST** release on PR open
4. **NEVER** modify files without addressing the WIP marker

### CI Checks

Two CI checks prevent loops:

1. **Check 1**: Branch name contains `__L<line>` and that line was changed from `WIP` → `READY` or removed
2. **Check 2**: `.wiplocks/` is empty after the job

See [docs/ci-wip-checks.md](./ci-wip-checks.md) for CI implementation.

## File Organization

```
ggen/
├── .wiplocks/              # Lock directories (gitignored)
│   └── <sha1>/            # Individual lock dirs
│       └── owner          # Lock owner info
├── scripts/
│   ├── wipqueue/          # Queue tool
│   │   ├── Cargo.toml
│   │   └── src/main.rs
│   └── wip-lock           # Lock manager script
├── docs/
│   ├── wip-queue.md       # This file
│   ├── wip-agent-runbook.md
│   └── ci-wip-checks.md
└── make.toml              # Lifecycle integration
```

## Benefits

1. **No duplicate work** - File locks prevent concurrent work
2. **Deterministic order** - Same queue for all agents
3. **Traceable progress** - Git branches and locks show who's working on what
4. **Loop prevention** - CI checks ensure markers are addressed
5. **Simple integration** - Works with existing ggen lifecycle

## Troubleshooting

### Lock is stuck

```bash
# List all locks
ls -la .wiplocks/

# Force release a lock
rm -rf .wiplocks/<sha1>
```

### No WIP found

```bash
# Search for WIP markers manually
rg "WIP:|TODO:|UNIMPL:|unimplemented!|todo!" --type rust
```

### Queue shows locked task first

The queue shows all tasks but lists unlocked ones first. Agents should:
- Check if `locked` field is false
- Pick the first task where `locked: false`

## See Also

- [Agent Runbook](./wip-agent-runbook.md) - Complete agent workflow
- [CI Checks](./ci-wip-checks.md) - CI enforcement scripts
- [Lifecycle Guide](./lifecycle.md) - Ggen lifecycle system
