<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Pattern 010: IDEMPOTENT APPLY](#pattern-010-idempotent-apply)
  - [Problem](#problem)
  - [Solution](#solution)
  - [Prerequisites](#prerequisites)
  - [Step-by-Step](#step-by-step)
    - [1. Apply a Plan File](#1-apply-a-plan-file)
    - [2. Re-run the Same Apply (Idempotent)](#2-re-run-the-same-apply-idempotent)
    - [3. Apply with Verification](#3-apply-with-verification)
  - [Complete Example](#complete-example)
  - [Explanation](#explanation)
    - [Idempotency Mechanisms](#idempotency-mechanisms)
      - [1. `unless_exists`](#1-unless_exists)
      - [2. `skip_if`](#2-skip_if)
      - [3. `idempotent: true`](#3-idempotent-true)
      - [4. Content Hash Verification](#4-content-hash-verification)
    - [Apply Behavior Matrix](#apply-behavior-matrix)
  - [Expected Output](#expected-output)
  - [Common Pitfalls](#common-pitfalls)
  - [Variations](#variations)
    - [💡 Force Apply (Override Checks)](#-force-apply-override-checks)
    - [💡 Selective Apply](#-selective-apply)
    - [💡 Dry-Run Apply](#-dry-run-apply)
  - [Troubleshooting](#troubleshooting)
  - [Automation Examples](#automation-examples)
    - [CI/CD Pipeline (GitHub Actions)](#cicd-pipeline-github-actions)
    - [Makefile Integration](#makefile-integration)
    - [Pre-commit Hook](#pre-commit-hook)
  - [See Also](#see-also)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Pattern 010: IDEMPOTENT APPLY

**Difficulty**: ⭐⭐ Intermediate
**Time**: 15min
**Tags**: #workflow #safety #idempotency #automation

## Problem

You need to apply code generation safely in automated environments (CI/CD, scripts, Makefiles) where the same command might run multiple times. You want guarantees that re-running generation won't create duplicates, corrupt files, or fail unexpectedly.

## Solution

Use `ggen project apply` with idempotency features (`skip_if`, `unless_exists`, `idempotent: true`) to ensure that applying a plan multiple times produces the same result without side effects. Combine with plan files for fully reproducible, safe automation.

## Prerequisites

- Understanding of Pattern 009: Project Plan
- ggen installed (v0.2.4+)
- A plan file created with `ggen project plan`

## Step-by-Step

### 1. Apply a Plan File

```bash
# Apply a previously created plan
ggen project apply plans/my-module.plan.json
```

This will:
- Verify plan integrity (content hashes)
- Check `unless_exists` and `skip_if` conditions
- Create/modify only files that need changes
- Skip files that already match expected state

### 2. Re-run the Same Apply (Idempotent)

```bash
# Run again - should be no-op
ggen project apply plans/my-module.plan.json
```

Expected output:
```
✅ Plan applied: plans/my-module.plan.json
📊 Summary:
   Created: 0 files
   Modified: 0 files
   Skipped: 3 files (already up-to-date)

💡 All files already match plan state
```

### 3. Apply with Verification

```bash
# Apply and verify content matches expected hashes
ggen project apply plans/my-module.plan.json --verify
```

## Complete Example

**Scenario**: Scaffold a CLI command that can be re-run safely in automation.

**Template** (`templates/cli-cmd.tmpl`):
```yaml
---
to: "src/cmds/{{cmd}}.rs"
unless_exists: true
vars:
  cmd: "example"
  summary: "Example command"
---
use clap::Args;
use anyhow::Result;

#[derive(Args, Debug)]
pub struct {{cmd | title}}Args {}

pub async fn run(args: &{{cmd | title}}Args) -> Result<()> {
    println!("{{summary}}");
    Ok(())
}
```

**Mod Injection Template** (`templates/mod-inject.tmpl`):
```yaml
---
to: "src/cmds/mod.rs"
inject: true
after: "// COMMANDS"
skip_if: "pub mod {{cmd}}"
idempotent: true
vars:
  cmd: "example"
---
pub mod {{cmd}};
```

**Workflow**:

```bash
# Step 1: Create plan
ggen project plan templates/cli-cmd.tmpl templates/mod-inject.tmpl \
  --vars cmd=hello summary="Print greeting" \
  --output-plan plans/hello-cmd.plan.json

# Step 2: Apply plan (creates files)
ggen project apply plans/hello-cmd.plan.json
# ✅ Created: src/cmds/hello.rs
# ✅ Modified: src/cmds/mod.rs (injected "pub mod hello;")

# Step 3: Re-run apply (idempotent)
ggen project apply plans/hello-cmd.plan.json
# ✅ Skipped: src/cmds/hello.rs (unless_exists: true)
# ✅ Skipped: src/cmds/mod.rs (skip_if matched "pub mod hello")

# Step 4: Verify integrity
ggen project apply plans/hello-cmd.plan.json --verify
# ✅ All content hashes match plan
```

## Explanation

### Idempotency Mechanisms

#### 1. `unless_exists`
```yaml
---
to: "src/file.rs"
unless_exists: true  # Only create if file doesn't exist
---
```
- **First run**: File created
- **Subsequent runs**: File skipped (no error)
- **Use case**: Bootstrap files that shouldn't be regenerated

#### 2. `skip_if`
```yaml
---
to: "src/lib.rs"
inject: true
append: true
skip_if: "pub mod {{module}}"  # Skip if pattern found in file
---
```
- **First run**: Pattern not found, content injected
- **Subsequent runs**: Pattern found, injection skipped
- **Use case**: Idempotent file modifications

#### 3. `idempotent: true`
```yaml
---
to: "src/exports.rs"
inject: true
idempotent: true  # Skip if exact content exists anywhere in file
---
```
- Automatically detects if content already exists
- More powerful than `skip_if` (no pattern needed)
- Checks full content, not just pattern

#### 4. Content Hash Verification
```json
{
  "outputs": [{
    "path": "src/file.rs",
    "content_hash": "sha256:abc123..."
  }]
}
```
- Plan stores expected content hash
- Apply verifies file matches expected state
- Detects drift from plan

### Apply Behavior Matrix

| File State | `unless_exists` | `skip_if` Match | `idempotent` | Result |
|------------|-----------------|-----------------|--------------|--------|
| Missing | false | N/A | N/A | **Create** |
| Missing | true | N/A | N/A | **Create** |
| Exists | false | false | false | **Overwrite** |
| Exists | true | N/A | N/A | **Skip** |
| Exists | false | true | N/A | **Skip** |
| Exists | false | false | true (content matches) | **Skip** |
| Modified | false | false | false | **Conflict** (use `--force`) |

## Expected Output

**First Apply**:
```
✅ Applying plan: plans/feature.plan.json
📝 Processing templates...
   ✅ Created: src/models/user.rs
   ✅ Created: src/api/users.rs
   ✅ Modified: src/lib.rs (injected exports)

📊 Summary:
   Created: 2 files
   Modified: 1 file
   Skipped: 0 files
   Duration: 0.3s

💡 All changes applied successfully
```

**Second Apply (Idempotent)**:
```
✅ Applying plan: plans/feature.plan.json
📝 Processing templates...
   ⏭️  Skipped: src/models/user.rs (unless_exists: file exists)
   ⏭️  Skipped: src/api/users.rs (unless_exists: file exists)
   ⏭️  Skipped: src/lib.rs (skip_if: pattern matched)

📊 Summary:
   Created: 0 files
   Modified: 0 files
   Skipped: 3 files
   Duration: 0.1s

💡 All files already match plan state
```

## Common Pitfalls

❌ **Mistake**: Using `force: true` instead of idempotency checks
- **Symptom**: Files overwritten even when unchanged
- **Fix**: Use `unless_exists` or `skip_if` for controlled updates

❌ **Mistake**: Not using `skip_if` with injection
- **Symptom**: Duplicate injections on re-run
- **Fix**: Always add `skip_if` pattern to match injected content

❌ **Mistake**: Applying outdated plans
- **Symptom**: Generated code doesn't match current templates
- **Fix**: Regenerate plans when templates change

❌ **Mistake**: Manual file edits breaking idempotency
- **Symptom**: Content hash mismatches, apply fails
- **Fix**: Use `--force` to override, or regenerate plan

## Variations

### 💡 Force Apply (Override Checks)

```bash
# Override all safety checks (use with caution)
ggen project apply plans/feature.plan.json --force

# Useful when:
# - Recovering from manual edits
# - Updating generated code with template changes
# - Resetting to known good state
```

### 💡 Selective Apply

```bash
# Apply only specific files from plan
ggen project apply plans/feature.plan.json \
  --include "src/models/*.rs"

# Exclude certain paths
ggen project apply plans/feature.plan.json \
  --exclude "tests/*"
```

### 💡 Dry-Run Apply

```bash
# Preview what would happen without making changes
ggen project apply plans/feature.plan.json --dry-run

# Useful for:
# - CI checks (verify plan is still valid)
# - Reviewing changes before applying
# - Detecting configuration drift
```

## Troubleshooting

**Issue**: Apply fails with "content hash mismatch"
**Cause**: File was manually edited after plan creation
**Solution**:
```bash
# View diff to see changes
ggen project diff --plan plans/feature.plan.json

# Either regenerate plan to match current state
ggen project plan ... --output-plan plans/feature.plan.json

# Or force apply to overwrite with plan content
ggen project apply plans/feature.plan.json --force
```

**Issue**: Injection happens every time (not idempotent)
**Cause**: Missing or incorrect `skip_if` pattern
**Solution**:
```yaml
# Add specific pattern to detect injection
skip_if: "pub mod {{module}}"  # Must exactly match injected content

# Or use idempotent mode
idempotent: true
```

**Issue**: Apply creates files in wrong location
**Cause**: Running from wrong directory
**Solution**:
```bash
# Always run from project root
cd /path/to/project
ggen project apply plans/feature.plan.json

# Or use absolute paths in plan
```

## Automation Examples

### CI/CD Pipeline (GitHub Actions)

```yaml
# .github/workflows/generate.yml
name: Generate Code

on:
  push:
    paths:
      - 'templates/**'
      - 'plans/**'

jobs:
  generate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Install ggen
        run: cargo install ggen

      - name: Apply generation plans
        run: |
          ggen project apply plans/api.plan.json --verify
          ggen project apply plans/models.plan.json --verify

      - name: Commit changes (if any)
        run: |
          git config user.name "ggen-bot"
          git config user.email "bot@example.com"
          git add .
          git diff --quiet || git commit -m "chore: apply code generation"
          git push
```

### Makefile Integration

```makefile
# Makefile
.PHONY: plan apply verify clean

plan:
	ggen project plan templates/*.tmpl \
		--vars env=dev \
		--output-plan plans/dev.plan.json

apply: plan
	ggen project apply plans/dev.plan.json --verify

verify:
	ggen project apply plans/dev.plan.json --dry-run

clean:
	rm -rf plans/*.plan.json
```

### Pre-commit Hook

```bash
#!/bin/bash
# .git/hooks/pre-commit

# Verify all plans are still valid before committing
for plan in plans/*.plan.json; do
  echo "Verifying $plan..."
  ggen project apply "$plan" --dry-run --verify || exit 1
done

echo "✅ All plans valid"
```

## See Also

- Pattern 009: Project Plan - Create generation plans
- Pattern 011: Dry-Run Diff - Preview changes before applying
- Pattern 012: CI Drift Check - Detect configuration drift
- Recipe 5.3: Idempotent Updates - Using `skip_if` in templates
- Recipe 6.1: Reproducible Output - Deterministic generation

## Next Steps

- Set up automated plan application in CI/CD
- Create plans for different environments (dev, staging, prod)
- Add plan verification to pre-commit hooks
- Monitor drift between plans and actual files
