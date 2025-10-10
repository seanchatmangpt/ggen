# Pattern 011: DRY-RUN DIFF

**Difficulty**: ⭐ Beginner
**Time**: 10min
**Tags**: #workflow #preview #diff #safety #review

## Problem

You want to see exactly what changes will be made before committing to generation. You need a clear, reviewable diff showing additions, deletions, and modifications - similar to `git diff` or `terraform plan` - to catch errors before they happen.

## Solution

Use `ggen project diff` to generate unified diffs comparing current files against what would be generated. This provides a safe preview for code review, verification, and catching unintended changes.

## Prerequisites

- ggen installed (v0.2.4+)
- Templates ready to generate
- Basic understanding of unified diff format

## Step-by-Step

### 1. Preview Changes for a Template

```bash
# Show diff without creating files
ggen project diff templates/rust.tmpl \
  --vars name=MyModule
```

Output shows unified diff:
```diff
--- a/src/my_module.rs (doesn't exist)
+++ b/src/my_module.rs (would be created)
@@ -0,0 +1,5 @@
+pub struct MyModule {
+    // Fields here
+}
+
+impl MyModule {}
```

### 2. Preview Changes from a Plan

```bash
# Create plan first
ggen project plan templates/*.tmpl \
  --vars resource=User \
  --output-plan plans/user.plan.json

# Show diff from plan
ggen project diff --plan plans/user.plan.json
```

### 3. Save Diff for Review

```bash
# Save diff to file for PR review
ggen project diff --plan plans/api.plan.json \
  > reviews/api-changes.diff

# Share with team
git add reviews/api-changes.diff
git commit -m "feat: API generation plan review"
```

## Complete Example

**Scenario**: You're modifying an existing file with injection and want to preview the exact changes.

**Existing File** (`src/lib.rs`):
```rust
pub mod utils;
pub mod config;

// Application code
```

**Template** (`templates/add-module.tmpl`):
```yaml
---
to: "src/lib.rs"
inject: true
after: "pub mod config;"
skip_if: "pub mod {{module}}"
vars:
  module: "auth"
---
pub mod {{module}};
```

**Command**:
```bash
# Preview injection diff
ggen project diff templates/add-module.tmpl \
  --vars module=auth
```

**Output**:
```diff
--- a/src/lib.rs
+++ b/src/lib.rs
@@ -1,4 +1,5 @@
 pub mod utils;
 pub mod config;
+pub mod auth;

 // Application code
```

**Verification**:
```bash
# Review looks good? Apply the change
ggen gen templates/add-module.tmpl --vars module=auth

# Verify actual result matches diff
git diff src/lib.rs
```

## Explanation

### Diff Output Format

#### New File Creation
```diff
--- a/src/models/user.rs (doesn't exist)
+++ b/src/models/user.rs (would be created)
@@ -0,0 +1,10 @@
+use serde::{Deserialize, Serialize};
+
+#[derive(Debug, Serialize, Deserialize)]
+pub struct User {
+    pub id: u64,
+    pub name: String,
+}
```
- `---` line shows source (non-existent file)
- `+++` line shows target (would be created)
- All lines start with `+` (additions)

#### File Modification
```diff
--- a/Cargo.toml
+++ b/Cargo.toml
@@ -5,6 +5,7 @@
 edition = "2021"

 [dependencies]
+serde = "1.0"
 anyhow = "1.0"
 clap = { version = "4.0", features = ["derive"] }
```
- Shows context lines (unchanged)
- `+` prefix for additions
- `-` prefix for deletions (if overwriting)
- Line numbers in `@@` header

#### No Changes (Idempotent)
```
✅ No changes detected
📊 All files match expected state:
   src/models/user.rs (skip_if matched)
   src/lib.rs (unless_exists: file exists)
```

### Diff Modes

#### 1. **Template Diff** (default)
```bash
ggen project diff templates/file.tmpl --vars key=value
```
- Renders template in memory
- Compares against current files
- Fast, no plan file needed

#### 2. **Plan Diff**
```bash
ggen project diff --plan plans/feature.plan.json
```
- Uses pre-computed plan
- Shows all changes from plan
- Useful for complex multi-template generations

#### 3. **Unified Diff Format**
```bash
# Standard unified diff (default)
ggen project diff --plan plans/feature.plan.json

# Colored output for terminal
ggen project diff --plan plans/feature.plan.json --color

# Machine-readable JSON
ggen project diff --plan plans/feature.plan.json --format json
```

## Expected Output

**Multiple File Changes**:
```diff
Previewing changes from plan: plans/feature.plan.json

📝 File: src/models/user.rs (new file)
--- /dev/null
+++ b/src/models/user.rs
@@ -0,0 +1,8 @@
+#[derive(Debug, Clone)]
+pub struct User {
+    pub id: u64,
+    pub email: String,
+}

📝 File: src/api/users.rs (new file)
--- /dev/null
+++ b/src/api/users.rs
@@ -0,0 +1,12 @@
+use axum::{Router, Json};
+use crate::models::User;
+
+pub fn routes() -> Router {
+    Router::new()
+        .route("/users", get(list_users))
+}

📝 File: src/lib.rs (modified)
--- a/src/lib.rs
+++ b/src/lib.rs
@@ -1,3 +1,5 @@
 pub mod utils;
+pub mod models;
+pub mod api;

📊 Summary:
   New files: 2
   Modified files: 1
   Unchanged files: 0
   Total changes: 3

💡 Run 'ggen project apply plans/feature.plan.json' to apply changes
```

## Common Pitfalls

❌ **Mistake**: Not reviewing diff before applying
- **Symptom**: Unintended changes committed to codebase
- **Fix**: Always run `diff` before `apply` or `gen`

❌ **Mistake**: Diff shows unexpected changes
- **Symptom**: Generated content doesn't match expectations
- **Fix**: Check template variables, frontmatter, and RDF data

❌ **Mistake**: Ignoring "no changes" warnings
- **Symptom**: Re-running generation thinking it will do something
- **Fix**: Review `skip_if` and `unless_exists` conditions

❌ **Mistake**: Diff too large to review
- **Symptom**: Hundreds of lines of diff, easy to miss issues
- **Fix**: Break into smaller templates, use `--include` filters

## Variations

### 💡 Filter Diff by Path

```bash
# Show diff only for specific paths
ggen project diff --plan plans/full.plan.json \
  --include "src/models/*.rs"

# Exclude certain files
ggen project diff --plan plans/full.plan.json \
  --exclude "tests/*"
```

### 💡 Context Lines

```bash
# Show more context around changes (default: 3 lines)
ggen project diff --plan plans/api.plan.json \
  --context 10

# Minimal context
ggen project diff --plan plans/api.plan.json \
  --context 0
```

### 💡 Side-by-Side Diff

```bash
# Generate side-by-side diff for easier reading
ggen project diff --plan plans/feature.plan.json \
  --format side-by-side

# Or pipe to external tool
ggen project diff --plan plans/feature.plan.json | \
  diff-so-fancy | less -R
```

### 💡 Stats Only

```bash
# Just show summary, no detailed diff
ggen project diff --plan plans/feature.plan.json \
  --stat

# Output:
# 3 files changed, 87 insertions(+), 2 deletions(-)
# src/models/user.rs | 45 ++++++++++++++++++++++++++++++
# src/api/users.rs   | 40 ++++++++++++++++++++++++++
# src/lib.rs         |  4 ++-
```

## Troubleshooting

**Issue**: Diff shows entire file as changed (not just delta)
**Cause**: File encoding or line ending mismatch
**Solution**:
```bash
# Normalize line endings
dos2unix src/file.rs

# Check file encoding
file src/file.rs

# Ensure template uses same encoding
```

**Issue**: Diff output is garbled or unreadable
**Cause**: Binary files or special characters
**Solution**:
```bash
# Skip binary files
ggen project diff --plan plans/full.plan.json \
  --exclude "*.png" --exclude "*.jpg"

# Or filter in plan creation
ggen project plan templates/text-only.tmpl \
  --output-plan plans/text.plan.json
```

**Issue**: Diff shows changes even though file looks identical
**Cause**: Whitespace differences (spaces vs tabs, trailing spaces)
**Solution**:
```bash
# Ignore whitespace in diff
ggen project diff --plan plans/feature.plan.json \
  --ignore-whitespace

# Or fix template whitespace handling
```

**Issue**: No diff shown but expecting changes
**Cause**: Idempotency checks (`skip_if`, `unless_exists`) preventing changes
**Solution**:
```bash
# Show why files were skipped
ggen project diff --plan plans/feature.plan.json \
  --verbose

# Override checks to see full diff
ggen project diff --plan plans/feature.plan.json \
  --force
```

## Code Review Integration

### GitHub PR Comments

```bash
# Generate diff for PR review
ggen project diff --plan plans/api.plan.json \
  --format markdown > pr-review.md

# Include in PR description
cat pr-review.md | gh pr create \
  --title "feat: Generate API endpoints" \
  --body-file -
```

### Git Pre-commit Hook

```bash
#!/bin/bash
# .git/hooks/pre-commit

# Check if any plan files changed
if git diff --cached --name-only | grep -q "^plans/"; then
  echo "📝 Plan files changed, generating diff..."

  for plan in $(git diff --cached --name-only | grep "^plans/.*\.json$"); do
    echo "Reviewing: $plan"
    ggen project diff --plan "$plan" --stat
  done

  # Require explicit confirmation
  read -p "Continue with commit? [y/N] " -n 1 -r
  echo
  if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    exit 1
  fi
fi
```

### CI/CD Verification

```yaml
# .github/workflows/verify-plans.yml
name: Verify Plans

on:
  pull_request:
    paths:
      - 'plans/**'
      - 'templates/**'

jobs:
  diff:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Install ggen
        run: cargo install ggen

      - name: Generate diffs for review
        run: |
          for plan in plans/*.plan.json; do
            echo "## Diff for $plan" >> $GITHUB_STEP_SUMMARY
            ggen project diff --plan "$plan" >> $GITHUB_STEP_SUMMARY
          done

      - name: Check for unexpected changes
        run: |
          # Fail if any plan would modify untracked files
          ggen project diff --plan plans/production.plan.json \
            --include "config/*" --fail-on-changes
```

## See Also

- Pattern 009: Project Plan - Create plans for diffing
- Pattern 010: Idempotent Apply - Safely apply changes
- Pattern 012: CI Drift Check - Automated drift detection
- Recipe 5.7: Safe Injection Testing - Dry-run mode for injection
- Recipe 13.5: Coverage Analysis - Test generation coverage

## Next Steps

- Integrate diff checks into PR workflow
- Add pre-commit hooks for plan validation
- Create diff snapshots for regression testing
- Set up CI jobs to comment diffs on PRs
