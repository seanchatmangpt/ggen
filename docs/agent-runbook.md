# Agent Runbook - WIP Queue System

## Quick Start

**Goal**: Pick the next unimplemented task via DFS, implement it, test it, and submit a small PR.

## Step-by-Step Process

### 1. Get the Queue

```bash
cargo run -p wipqueue --quiet
```

**Output:**
```json
[
  {
    "path": "src/auth.rs",
    "line": 42,
    "col": 5,
    "kind": "WIP",
    "text": "implement OAuth2 token refresh",
    "lock_path": ".wiplocks/abc123",
    "locked": false
  },
  {
    "path": "src/db.rs",
    "line": 15,
    "col": 8,
    "kind": "TODO",
    "text": "add connection pooling",
    "lock_path": ".wiplocks/def456",
    "locked": true
  }
]
```

**Action**: Pick the **first unlocked task** (index 0).

### 2. Claim the Task

```bash
./scripts/wip-lock claim "src/auth.rs" "42"
```

**Expected Output:**
```
claimed: src/auth.rs:42
```

**If locked:**
```
already locked: src/auth.rs
  locked by: src/auth.rs:42
  since: 2025-10-12T10:30:00Z
```

**Action**: If locked, pick the next unlocked task.

### 3. Create Feature Branch

```bash
git switch -c "wip/$(echo "src/auth.rs" | tr '/' '__')__L42"
```

**Branch Name Format:**
- Pattern: `wip/<path-with-underscores>__L<line>`
- Example: `wip/src__auth.rs__L42`

### 4. Implement the Feature

#### 4a. Replace WIP Marker

**Before:**
```rust
// WIP: implement OAuth2 token refresh
pub fn refresh_token(refresh_token: &str) -> Result<Token> {
    unimplemented!("WIP: implement OAuth2 token refresh")
}
```

**After:**
```rust
// READY: OAuth2 token refresh implemented
pub fn refresh_token(refresh_token: &str) -> Result<Token> {
    let client = reqwest::blocking::Client::new();
    let response = client
        .post("https://oauth.example.com/token")
        .form(&[
            ("grant_type", "refresh_token"),
            ("refresh_token", refresh_token),
        ])
        .send()
        .map_err(|e| anyhow::anyhow!("Token refresh failed: {}", e))?;

    response.json::<Token>()
        .map_err(|e| anyhow::anyhow!("Failed to parse token: {}", e))
}
```

#### 4b. Add or Extend Tests

**Create test file** (`tests/auth_test.rs`):
```rust
use ggen::auth::refresh_token;

#[test]
fn test_refresh_token_success() {
    // Test implementation
    let token = refresh_token("valid_refresh_token").unwrap();
    assert_eq!(token.token_type, "Bearer");
}

#[test]
fn test_refresh_token_invalid() {
    let result = refresh_token("invalid_token");
    assert!(result.is_err());
}
```

#### 4c. Run Validation

```bash
# Format code
cargo fmt

# Check for warnings
cargo clippy -D warnings

# Run all tests
cargo test

# Run specific test
cargo test test_refresh_token
```

### 5. Flip the Marker

**Two options:**

**Option 1: Flip to READY**
```rust
// WIP: implement feature → // READY: feature implemented
```

**Option 2: Remove marker entirely**
```rust
// WIP: implement feature → (delete line)
```

**Then commit:**
```bash
git add src/auth.rs tests/auth_test.rs
git commit -m "feat(auth): complete WIP at src/auth.rs:42

Implemented OAuth2 token refresh functionality.

- Added refresh_token() function with error handling
- Added comprehensive tests for success and failure cases
- Follows OAuth2 RFC 6749 specification

Closes: src/auth.rs:42"
```

**Commit Message Format (Conventional Commits):**
```
<type>(<scope>): <subject>

<body>

<footer>
```

### 6. Release Lock

```bash
./scripts/wip-lock release "src/auth.rs"
```

**Expected Output:**
```
released: src/auth.rs
```

### 7. Push and Create PR

```bash
# Push branch
git push -u origin wip/src__auth.rs__L42

# Create draft PR using GitHub CLI
gh pr create \
  --draft \
  --title "WIP: auth: complete src/auth.rs:L42" \
  --body "Implements OAuth2 token refresh at src/auth.rs:42

## Changes
- Implemented refresh_token() function
- Added error handling with anyhow
- Added comprehensive test coverage

## Testing
- ✅ cargo fmt
- ✅ cargo clippy
- ✅ cargo test
"
```

## Using ggen Lifecycle

### Automated Workflow

```bash
# Claim next task (combines steps 1-3)
ggen lifecycle run wip.next
# Output: claimed src/auth.rs:42

# ... do your work ...

# Release lock (step 6)
ggen lifecycle run wip.done
# Output: released: src/auth.rs
```

### Complete Example

```bash
# Start
ggen lifecycle run wip.next
# → claimed src/auth.rs:42
# → switched to branch wip/src__auth.rs__L42

# Implement
vim src/auth.rs  # Remove WIP marker, add implementation
vim tests/auth_test.rs  # Add tests

# Validate
cargo fmt
cargo clippy -D warnings
cargo test

# Commit
git add src/auth.rs tests/auth_test.rs
git commit -m "feat(auth): complete WIP at src/auth.rs:42"

# Finish
ggen lifecycle run wip.done
git push -u origin wip/src__auth.rs__L42
gh pr create --draft --title "WIP: auth: complete src/auth.rs:L42"
```

## Decision Tree

```
Start
  ↓
Get queue → Any tasks? ──No──→ Done! 🎉
  ↓
  Yes
  ↓
Pick first unlocked task
  ↓
Claim lock → Success? ──No──→ Go to next task
  ↓
  Yes
  ↓
Create branch
  ↓
Implement feature
  ↓
Write tests
  ↓
Run validation → Pass? ──No──→ Fix issues, repeat
  ↓
  Yes
  ↓
Flip marker (WIP → READY)
  ↓
Commit changes
  ↓
Release lock
  ↓
Push branch
  ↓
Create PR
  ↓
Go to Start
```

## Common Patterns

### Pattern 1: Small Function Implementation

```rust
// Before
// WIP: add validation logic
pub fn validate_input(input: &str) -> Result<()> {
    todo!("WIP: add validation logic")
}

// After
// READY: input validation implemented
pub fn validate_input(input: &str) -> Result<()> {
    if input.is_empty() {
        return Err(anyhow::anyhow!("Input cannot be empty"));
    }
    if input.len() > 1000 {
        return Err(anyhow::anyhow!("Input too long"));
    }
    Ok(())
}
```

### Pattern 2: Error Handling Stub

```rust
// Before
// WIP: implement proper error handling
match result {
    Ok(v) => v,
    Err(_) => panic!("not implemented yet"),
}

// After
// READY: error handling implemented
match result {
    Ok(v) => v,
    Err(e) => return Err(anyhow::anyhow!("Failed to process: {}", e)),
}
```

### Pattern 3: Complex Feature

Break into multiple WIP markers:

```rust
// WIP: implement user authentication (phase 1: password validation)
// WIP: implement user authentication (phase 2: token generation)
// WIP: implement user authentication (phase 3: session management)
```

Implement one phase per PR.

## Error Handling

### Lock Already Claimed

```bash
./scripts/wip-lock claim "src/auth.rs" "42"
# already locked: src/auth.rs
#   locked by: src/auth.rs:42
#   since: 2025-10-12T10:30:00Z
```

**Solution**: Pick the next task in the queue.

### No WIP Tasks Found

```bash
cargo run -p wipqueue --quiet
# []
```

**Solution**: Add new WIP markers or celebrate completion! 🎉

### Tests Failing

```bash
cargo test
# test test_refresh_token_success ... FAILED
```

**Solution**: Fix the implementation, don't flip the WIP marker until tests pass.

### Lock Stuck

```bash
./scripts/wip-lock status "src/auth.rs"
# locked: src/auth.rs
#   by: src/auth.rs:42
#   since: 2025-10-12T08:00:00Z  # 3 hours ago!
```

**Solution**: Check if process still running, otherwise manually release:
```bash
rm -rf .wiplocks/<sha1-hash>
```

## Best Practices

1. **One Task at a Time**: Don't claim multiple tasks
2. **Small PRs**: Keep changes focused on one WIP marker
3. **Test First**: Write tests before removing WIP marker
4. **Clean Commits**: Use Conventional Commits format
5. **Release Locks**: Always release before moving to next task
6. **Document Changes**: Update docs if behavior changes

## Anti-Patterns

❌ **Don't do this:**
- Claiming multiple files at once
- Leaving locks without PRs
- Adding new WIP markers in same PR
- Removing unrelated WIP markers
- Force-pushing to WIP branches
- Merging without CI passing

✅ **Do this instead:**
- One file/task at a time
- Release locks immediately after PR
- Focus on one WIP per PR
- Only touch the claimed WIP marker
- Create new branches for each task
- Wait for CI validation

## Checklist

Before opening a PR, verify:

- [ ] WIP marker flipped to READY or removed
- [ ] Tests added and passing
- [ ] `cargo fmt` run
- [ ] `cargo clippy` passing
- [ ] Lock released
- [ ] Branch pushed
- [ ] PR title follows format: `WIP: <area>: complete <file>:L<line>`
- [ ] PR description includes testing notes
- [ ] Only changed files related to the WIP marker

## References

- [WIP Queue System Documentation](./wip-queue-system.md)
- [ggen Lifecycle Documentation](./lifecycle.md)
- [Conventional Commits](https://www.conventionalcommits.org/)

---

**Remember**: Trust the queue, claim your task, complete it, release the lock, and move on. Keep it simple, keep it focused, keep it moving.
