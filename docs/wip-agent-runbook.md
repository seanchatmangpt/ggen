# WIP Agent Runbook

## Goal

Pick the next unimplemented task via DFS, implement it, test it, and submit a small PR.

## Prerequisites

- `wipqueue` tool available: `cargo build -p wipqueue`
- `wip-lock` script available: `chmod +x scripts/wip-lock`
- Git repository initialized
- jq installed for JSON parsing

## Complete Workflow

### Step 1: Get the Queue

```bash
cargo run -p wipqueue --quiet
```

This outputs a JSON array of all WIP tasks, ordered by:
1. Unlocked tasks first
2. DFS traversal order (lexicographic, depth-first)
3. Line order within files

Example output:
```json
[
  {
    "path": "src/auth.rs",
    "line": 42,
    "col": 5,
    "kind": "WIP",
    "text": "implement JWT validation",
    "lock_path": ".wiplocks/a1b2c3d4..."
  },
  {
    "path": "src/db.rs",
    "line": 15,
    "col": 3,
    "kind": "TODO",
    "text": "add connection pooling",
    "lock_path": ".wiplocks/e5f6g7h8..."
  }
]
```

### Step 2: Claim the First Unlocked Task

Extract the first task and claim it:

```bash
# Get first task
QUEUE=$(cargo run -p wipqueue --quiet)
FIRST=$(echo "$QUEUE" | jq -r '.[0]')

# Extract fields
PATH=$(echo "$FIRST" | jq -r '.path')
LINE=$(echo "$FIRST" | jq -r '.line')
TEXT=$(echo "$FIRST" | jq -r '.text')

# Claim the lock
./scripts/wip-lock claim "$PATH" "$LINE"
```

Expected output:
```
claimed: src/auth.rs:42
```

If the lock fails:
```
already locked: src/auth.rs
```

**Action**: If locked, pick the next task in the queue.

### Step 3: Create Branch

```bash
# Create branch from path and line
BRANCH="wip/$(echo "$PATH" | tr '/' '__')__L${LINE}"
git switch -c "$BRANCH"
```

Example branch name: `wip/src__auth.rs__L42`

### Step 4: Implement

#### 4.1 Open the File

```bash
# Open the file at the specific line
$EDITOR "$PATH" +$LINE
```

#### 4.2 Replace the Marker

Find the marker at the claimed line:

Before:
```rust
// WIP: implement JWT validation
return Err(AuthError::NotImplemented);
```

After:
```rust
// READY: JWT validation with RS256 support
let claims = decode::<Claims>(
    &token,
    &DecodingKey::from_rsa_pem(public_key)?,
    &Validation::new(Algorithm::RS256),
)?;
Ok(claims)
```

#### 4.3 Add or Extend Tests

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_jwt_validation() {
        let token = create_test_token();
        let result = validate_jwt(&token);
        assert!(result.is_ok());
    }

    #[test]
    fn test_jwt_validation_expired() {
        let token = create_expired_token();
        let result = validate_jwt(&token);
        assert!(matches!(result, Err(AuthError::TokenExpired)));
    }
}
```

#### 4.4 Run Tests

```bash
# Format code
cargo fmt

# Lint with warnings as errors
cargo clippy -D warnings

# Run tests
cargo nextest run
# Or if nextest not available:
cargo test
```

Fix any errors or warnings before continuing.

### Step 5: Flip Marker

Update the marker to indicate completion:

**Option A**: Change to READY
```rust
// WIP: implement JWT validation
↓
// READY: JWT validation with RS256 support
```

**Option B**: Remove marker entirely
```rust
// WIP: implement JWT validation
↓
(line removed)
```

### Step 6: Commit

```bash
# Stage changes
git add "$PATH" tests/

# Commit with Conventional Commit
git commit -m "feat(auth): complete WIP at ${PATH}:${LINE}

Implemented JWT validation with RS256 support.

- Added decode logic with proper error handling
- Added tests for valid and expired tokens
- Flipped WIP marker to READY

Closes WIP at ${PATH}:${LINE}"
```

### Step 7: Release Lock

```bash
./scripts/wip-lock release "$PATH"
```

Expected output:
```
released: src/auth.rs
```

### Step 8: Open Draft PR

```bash
# Push branch
git push -u origin "$BRANCH"

# Create PR
gh pr create --draft \
  --title "WIP: auth: complete ${PATH}:L${LINE}" \
  --body "## Summary

Completed WIP task at \`${PATH}:${LINE}\`: ${TEXT}

## Changes

- Implemented JWT validation with RS256 support
- Added test coverage for valid and expired tokens
- Replaced WIP marker with READY

## Checklist

- [x] Tests pass
- [x] Lint clean
- [x] WIP marker flipped to READY
- [x] Lock released

## Related

Addresses WIP marker in DFS queue."
```

## Quick Reference Commands

```bash
# Full workflow in one script
#!/usr/bin/env bash
set -euo pipefail

# 1. Get queue
QUEUE=$(cargo run -p wipqueue --quiet)
FIRST=$(echo "$QUEUE" | jq -r '.[0]')

# 2. Extract fields
PATH=$(echo "$FIRST" | jq -r '.path')
LINE=$(echo "$FIRST" | jq -r '.line')
TEXT=$(echo "$FIRST" | jq -r '.text')

# 3. Claim
./scripts/wip-lock claim "$PATH" "$LINE"

# 4. Branch
BRANCH="wip/$(echo "$PATH" | tr '/' '__')__L${LINE}"
git switch -c "$BRANCH"

echo "Claimed: $PATH:$LINE"
echo "Branch: $BRANCH"
echo "Task: $TEXT"
echo ""
echo "Now implement, test, commit, and run:"
echo "  ./scripts/wip-lock release \"$PATH\""
```

## Using Lifecycle Commands

Ggen provides lifecycle shortcuts:

```bash
# Claim next task
ggen lifecycle run wip.next

# After implementation, release lock
ggen lifecycle run wip.done
```

## Troubleshooting

### Lock Already Exists

**Symptom**: `./scripts/wip-lock claim` fails with "already locked"

**Solution**: Pick the next task in the queue
```bash
# Get second task
SECOND=$(echo "$QUEUE" | jq -r '.[1]')
```

### No WIP Found

**Symptom**: `wipqueue` returns `[]`

**Solution**: All tasks complete! Celebrate or add new WIP markers:
```bash
rg "WIP:|TODO:|UNIMPL:" --type rust
```

### Tests Fail

**Symptom**: `cargo test` fails

**Action**:
1. Do NOT flip the marker to READY
2. Keep the WIP marker as-is
3. Fix the implementation
4. Re-run tests
5. Only flip marker when tests pass

### Forgot to Release Lock

**Symptom**: Lock still exists after PR

**Solution**: Manually release
```bash
./scripts/wip-lock release "src/auth.rs"
```

## CI Integration

The CI system enforces:

1. **Marker Change Check**: Branch name contains `__L<line>` and that line was modified
2. **Lock Cleanup Check**: `.wiplocks/` is empty

See [docs/ci-wip-checks.md](./ci-wip-checks.md) for CI scripts.

## Best Practices

### DO
- ✅ Claim before implementing
- ✅ Test thoroughly before flipping marker
- ✅ Release lock immediately after PR
- ✅ Use Conventional Commits
- ✅ Add comprehensive tests

### DON'T
- ❌ Skip claiming the lock
- ❌ Flip marker if tests fail
- ❌ Leave locks after PR open
- ❌ Work on locked files
- ❌ Modify unrelated code

## Example: Complete Session

```bash
# 1. Get queue
$ cargo run -p wipqueue --quiet | jq -r '.[0]'
{
  "path": "src/auth.rs",
  "line": 42,
  "kind": "WIP",
  "text": "implement JWT validation",
  "lock_path": ".wiplocks/a1b2c3d4..."
}

# 2. Claim
$ ./scripts/wip-lock claim "src/auth.rs" "42"
claimed: src/auth.rs:42

# 3. Branch
$ git switch -c "wip/src__auth.rs__L42"
Switched to a new branch 'wip/src__auth.rs__L42'

# 4. Implement
$ vim src/auth.rs +42
# (make changes)

# 5. Test
$ cargo fmt && cargo clippy -D warnings && cargo test
All tests passed!

# 6. Commit
$ git add src/auth.rs tests/auth.rs
$ git commit -m "feat(auth): complete WIP at src/auth.rs:42"

# 7. Release
$ ./scripts/wip-lock release "src/auth.rs"
released: src/auth.rs

# 8. PR
$ git push -u origin "wip/src__auth.rs__L42"
$ gh pr create --draft --title "WIP: auth: complete src/auth.rs:L42"
```

## See Also

- [WIP Queue System](./wip-queue.md) - Architecture and design
- [CI Checks](./ci-wip-checks.md) - CI enforcement
- [Lifecycle Guide](./lifecycle.md) - Ggen lifecycle system
