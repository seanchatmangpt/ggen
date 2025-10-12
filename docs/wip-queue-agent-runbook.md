# WIP Queue Agent Runbook

## Goal
Pick the next unimplemented task via DFS, implement, test, and submit a small PR.

## Standard WIP Markers

Use these exact markers across code, tests, docs:

* `// WIP: <short task>` - Work in progress
* `// TODO: <short task>` - Todo item  
* `// UNIMPL: <short task>` - Unimplemented feature
* `unimplemented!("WIP: <short task>")`
* `todo!("WIP: <short task>")`

Completion flips to `// READY: <summary>` or removes the marker.

## Agent Workflow

### 1) Get the queue
```bash
cargo run -p wipqueue --quiet
```

### 2) Claim the first unlocked task (JSON field `path` + `line`)
```bash
./scripts/wip-lock claim "<path>" "<line>"
```

### 3) Create branch
```bash
git switch -c "wip/$(echo "<path>" | tr '/' '__')__L<line>"
```

### 4) Implement
- Replace the specific marker with working code.
- Add or extend tests.
- Run: `cargo fmt && cargo clippy -D warnings && cargo nextest run`

### 5) Flip marker
- Change `// WIP:` to `// READY:` (or remove).
- Commit with Conventional Commit:
  ```
  feat(<area>): complete WIP at <path>:<line>
  ```

### 6) Release lock
```bash
./scripts/wip-lock release "<path>"
```

### 7) Open draft PR with title
```
WIP: <area>: complete <file>:L<line>
```

## Editorial Rules

* Place markers at **the exact location** of missing behavior, not at file tops.
* One marker per missing behavior. If multiple, split lines.
* Prefer code stubs over comments when possible:

  ```rust
  // WIP: enforce Offline net profile here
  return Err(CleanroomError::Policy(PolicyError::Msg("net profile not enforced yet".into())));
  ```

* When completing, update or remove the specific marker line.

## Guardrails

* Agents **must** call `cargo run -p wipqueue` before opening any file.
* Agents **must** lock before editing and **must** release on PR open.
* CI job rejects PRs that modify files without flipping or removing at least one `WIP:` marker referenced in the branch name.
* CI job fails if new `WIP:` markers are added outside touched files.

## CI Validation

The `scripts/validate-wip-markers.sh` script enforces:

1. **Branch name validation**: Branch name contains `__L<line>` and that line was changed from `WIP` → `READY` or removed.
2. **Lock cleanup**: `.wiplocks/` is empty after the job.
3. **No new WIP markers**: No new `WIP:` markers are added outside touched files.

## Example Workflow

```bash
# 1. Get next task
$ cargo run -p wipqueue --quiet | jq -r '.[0]'
{
  "path": "cleanroom/src/backend/local.rs",
  "line": 35,
  "col": 63,
  "kind": "WIP",
  "text": "Apply determinism constraints in local backend",
  "lock_path": ".wiplocks/edb4a632a53a56fe4d29f6051dbebeb486f2593c"
}

# 2. Claim task
$ ./scripts/wip-lock claim "cleanroom/src/backend/local.rs" "35"
claimed: cleanroom/src/backend/local.rs:35

# 3. Create branch
$ git switch -c "wip/cleanroom__src__backend__local__rs__L35"

# 4. Implement (edit the file, replace WIP marker)
# 5. Test
$ cargo fmt && cargo clippy -D warnings && cargo nextest run

# 6. Commit
$ git add cleanroom/src/backend/local.rs
$ git commit -m "feat(cleanroom): complete WIP at cleanroom/src/backend/local.rs:35"

# 7. Release lock
$ ./scripts/wip-lock release "cleanroom/src/backend/local.rs"
released: cleanroom/src/backend/local.rs

# 8. Push and open PR
$ git push -u origin wip/cleanroom__src__backend__local__rs__L35
```

## DFS Traversal Order

The wipqueue tool uses deterministic DFS traversal:

* **Directory order**: Lexicographic, depth-first (stack)
* **File order in each dir**: Lexicographic  
* **In-file order**: Top-to-bottom line order

This ensures all agents see the same "next task" regardless of when they run.

## File Locks

* **Lock path**: `.wiplocks/<sha1(path)>`
* **Lock semantics**: Creating the directory is the lock; removal releases it
* **Agents must skip locked files**

## Queue Tool Usage

```bash
# Get all tasks (unlocked first, then locked)
cargo run -p wipqueue --quiet

# Get just the first unlocked task
cargo run -p wipqueue --quiet | jq -r '.[0]'

# Get task details
cargo run -p wipqueue --quiet | jq -r '.[0] | .path, .line, .text'
```

## Troubleshooting

### Task already locked
```bash
$ ./scripts/wip-lock claim "path/to/file.rs" "42"
already locked: path/to/file.rs
```
**Solution**: Run `cargo run -p wipqueue` again to get the next unlocked task.

### No WIP found
```bash
$ cargo run -p wipqueue --quiet
[]
```
**Solution**: All WIP tasks are completed! 🎉

### CI validation fails
```bash
$ ./scripts/validate-wip-markers.sh
❌ Line 35 still contains WIP marker: // WIP: implement this
```
**Solution**: Change the marker to `// READY:` or remove it entirely.

## Integration with Make

The WIP queue integrates with the existing `make.toml` lifecycle:

```bash
# Get and claim next task
cargo make wip.next

# Release current task lock  
cargo make wip.done
```

This provides a consistent interface for both manual and automated workflows.
