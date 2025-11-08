# Hooks System Guide

**ggen v2.5.0** | **Automation & Workflows** | **Event-Driven Development**

---

## Overview

The ggen **Hooks System** enables **automated workflows** triggered by specific events during code generation. Think of hooks as programmable automation points that execute custom scripts whenever certain actions occur.

### Key Benefits

- ✅ **Automated quality checks** (lint, format, test)
- ✅ **Continuous regeneration** (ontology changes → code updates)
- ✅ **Pre-commit validation** (prevent bad code from being committed)
- ✅ **Post-generation tasks** (documentation, deployment)
- ✅ **Custom workflows** (notifications, backups, CI/CD triggers)

---

## Table of Contents

- [Quick Start](#quick-start)
- [Hook Events](#hook-events)
- [Command Reference](#command-reference)
- [Common Workflows](#common-workflows)
- [Best Practices](#best-practices)
- [Advanced Patterns](#advanced-patterns)
- [Troubleshooting](#troubleshooting)

---

## Quick Start

### Create Your First Hook

```bash
# 1. Create a script that runs after code generation
cat > format-code.sh << 'EOF'
#!/bin/bash
echo "Auto-formatting generated code..."
cargo fmt
cargo clippy --fix --allow-dirty
echo "Formatting complete!"
EOF

chmod +x format-code.sh

# 2. Register the hook
ggen hook create \
  --event post-generate \
  --script ./format-code.sh \
  --name "auto-format"

# 3. Verify it's registered
ggen hook list
```

**Output:**
```json
{
  "hooks": [
    {
      "id": "hook_abc123",
      "trigger": "post-generate",
      "action": "./format-code.sh",
      "created_at": "2025-11-07T12:00:00Z"
    }
  ],
  "total": 1
}
```

### Test Your Hook

```bash
# Generate code - your hook will run automatically
ggen project gen test-project --graph domain.ttl

# Watch for hook execution in output
# → "Auto-formatting generated code..."
# → "Formatting complete!"
```

---

## Hook Events

ggen supports **6 primary event types** for automation:

### 1. `post-generate`

**Triggers:** After code generation completes

**Use Cases:**
- Auto-format generated code
- Run linters and static analysis
- Generate documentation
- Update package metadata
- Run initial tests

**Example:**
```bash
ggen hook create \
  --event post-generate \
  --script ./scripts/post-gen.sh \
  --name "post-generation-tasks"
```

**Script Template:**
```bash
#!/bin/bash
# post-gen.sh

echo "Running post-generation tasks..."

# Format code
cargo fmt

# Run clippy
cargo clippy --fix --allow-dirty --allow-staged

# Generate docs
cargo doc --no-deps

# Update README
ggen ai generate "Update README with new features" \
  --code "$(cat README.md)" > README.md

echo "Post-generation complete!"
```

---

### 2. `pre-commit`

**Triggers:** Before Git commit (requires Git hooks integration)

**Use Cases:**
- Validate code quality
- Run tests
- Check formatting
- Verify ontology consistency
- Prevent broken code from being committed

**Example:**
```bash
ggen hook create \
  --event pre-commit \
  --script ./scripts/pre-commit.sh \
  --name "commit-validation"
```

**Script Template:**
```bash
#!/bin/bash
# pre-commit.sh

set -e  # Exit on first error

echo "Running pre-commit checks..."

# 1. Validate ontology
echo "Validating ontology..."
ggen graph load domain.ttl

# 2. Run tests
echo "Running tests..."
cargo test

# 3. Check formatting
echo "Checking formatting..."
cargo fmt -- --check

# 4. Run clippy
echo "Running clippy..."
cargo clippy -- -D warnings

# 5. Build
echo "Building project..."
cargo build --release

echo "All pre-commit checks passed!"
```

---

### 3. `on-ontology-change`

**Triggers:** When RDF ontology files are modified

**Use Cases:**
- Automatically regenerate code
- Update database schema
- Regenerate API documentation
- Notify team of domain changes
- Trigger CI/CD pipeline

**Example:**
```bash
ggen hook create \
  --event on-ontology-change \
  --script ./scripts/regen-on-change.sh \
  --name "auto-regenerate"
```

**Script Template:**
```bash
#!/bin/bash
# regen-on-change.sh

CHANGED_FILE=$1  # Passed by hook system

echo "Ontology changed: $CHANGED_FILE"
echo "Regenerating code..."

# Backup current code
BACKUP_DIR="backups/$(date +%Y%m%d_%H%M%S)"
mkdir -p "$BACKUP_DIR"
cp -r src "$BACKUP_DIR/"

# Regenerate from updated ontology
ggen project gen . \
  --graph "$CHANGED_FILE" \
  --force

# Run tests to verify regeneration
cargo test || {
  echo "Tests failed! Restoring backup..."
  rm -rf src
  cp -r "$BACKUP_DIR/src" .
  exit 1
}

echo "Regeneration successful!"
```

---

### 4. `pre-build`

**Triggers:** Before compilation/build process

**Use Cases:**
- Code generation
- Asset compilation
- Environment validation
- Dependency checks
- Configuration generation

**Example:**
```bash
ggen hook create \
  --event pre-build \
  --script ./scripts/pre-build.sh \
  --name "build-preparation"
```

**Script Template:**
```bash
#!/bin/bash
# pre-build.sh

echo "Pre-build tasks..."

# 1. Check environment
ggen utils doctor

# 2. Generate build-time code
ggen ai generate "Generate build metadata" > src/build_info.rs

# 3. Update version
VERSION=$(cargo metadata --format-version 1 | jq -r '.packages[0].version')
echo "Building version: $VERSION"

# 4. Verify dependencies
cargo fetch

echo "Pre-build complete!"
```

---

### 5. `post-deploy`

**Triggers:** After deployment to production

**Use Cases:**
- Update live documentation
- Send notifications
- Generate metrics
- Archive artifacts
- Update status pages

**Example:**
```bash
ggen hook create \
  --event post-deploy \
  --script ./scripts/post-deploy.sh \
  --name "deployment-tasks"
```

**Script Template:**
```bash
#!/bin/bash
# post-deploy.sh

ENVIRONMENT=$1  # staging | production
VERSION=$2

echo "Deployed $VERSION to $ENVIRONMENT"

# 1. Update documentation site
if [ "$ENVIRONMENT" = "production" ]; then
  cargo doc --no-deps
  rsync -avz target/doc/ docs.example.com:/var/www/docs/
fi

# 2. Send Slack notification
curl -X POST https://hooks.slack.com/services/YOUR/WEBHOOK/URL \
  -H 'Content-Type: application/json' \
  -d "{\"text\": \"Deployed $VERSION to $ENVIRONMENT\"}"

# 3. Record deployment
echo "$(date): $VERSION deployed to $ENVIRONMENT" >> deployment.log

echo "Post-deployment tasks complete!"
```

---

### 6. `on-test-fail`

**Triggers:** When test suite fails

**Use Cases:**
- Create bug reports
- Notify developers
- Collect diagnostic information
- Rollback changes
- Generate failure reports

**Example:**
```bash
ggen hook create \
  --event on-test-fail \
  --script ./scripts/test-failure.sh \
  --name "handle-test-failures"
```

**Script Template:**
```bash
#!/bin/bash
# test-failure.sh

TEST_OUTPUT=$1

echo "Tests failed! Collecting diagnostics..."

# 1. Save test output
mkdir -p test-failures
FAILURE_FILE="test-failures/$(date +%Y%m%d_%H%M%S).log"
echo "$TEST_OUTPUT" > "$FAILURE_FILE"

# 2. Analyze with AI
ggen ai analyze --project . --complexity --security > analysis.json

# 3. Create GitHub issue (if in CI)
if [ -n "$GITHUB_ACTIONS" ]; then
  gh issue create \
    --title "Test Failure: $(date)" \
    --body "$(cat $FAILURE_FILE)" \
    --label "test-failure"
fi

# 4. Notify team
curl -X POST https://hooks.slack.com/services/YOUR/WEBHOOK/URL \
  -H 'Content-Type: application/json' \
  -d "{\"text\": \"Test failure detected. See $FAILURE_FILE\"}"

echo "Diagnostics collected in $FAILURE_FILE"
```

---

## Command Reference

### `ggen hook create`

**Create a new hook**

#### Syntax

```bash
ggen hook create --event <event> --script <path> --name <name>
```

#### Options

| Option | Required | Type | Description |
|--------|----------|------|-------------|
| `--event` | ✅ | enum | Event trigger (see [Hook Events](#hook-events)) |
| `--script` | ✅ | path | Path to executable script |
| `--name` | Optional | string | Human-readable hook name |

#### Examples

```bash
# Basic hook creation
ggen hook create \
  --event post-generate \
  --script ./format.sh

# With custom name
ggen hook create \
  --event pre-commit \
  --script ./validate.sh \
  --name "pre-commit-validator"
```

---

### `ggen hook list`

**List all registered hooks**

#### Syntax

```bash
ggen hook list [OPTIONS]
```

#### Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `--filter` | string | - | Filter by event type |
| `--verbose` | bool | false | Show detailed information |

#### Examples

```bash
# List all hooks
ggen hook list

# Filter by event type
ggen hook list --filter post-generate

# Verbose output
ggen hook list --verbose
```

**Output:**
```json
{
  "hooks": [
    {
      "id": "hook_abc123",
      "trigger": "post-generate",
      "action": "./format.sh",
      "created_at": "2025-11-07T12:00:00Z"
    },
    {
      "id": "hook_def456",
      "trigger": "pre-commit",
      "action": "./validate.sh",
      "created_at": "2025-11-07T12:05:00Z"
    }
  ],
  "total": 2
}
```

---

### `ggen hook remove`

**Remove a hook**

#### Syntax

```bash
ggen hook remove <hook-id> [OPTIONS]
```

#### Arguments

| Argument | Required | Description |
|----------|----------|-------------|
| `<hook-id>` | ✅ | Hook ID from `ggen hook list` |

#### Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `--force` | bool | false | Skip confirmation prompt |

#### Examples

```bash
# Remove with confirmation
ggen hook remove hook_abc123

# Force removal (no prompt)
ggen hook remove hook_abc123 --force
```

---

### `ggen hook monitor`

**Monitor hook activity in real-time**

#### Syntax

```bash
ggen hook monitor [OPTIONS]
```

#### Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `--graph` | path | - | Monitor specific ontology file |
| `--interval` | int | 1000 | Polling interval (ms) |
| `--once` | bool | false | Check once and exit |

#### Examples

```bash
# Monitor all hooks
ggen hook monitor

# Monitor ontology changes
ggen hook monitor --graph domain.ttl

# Single check
ggen hook monitor --once
```

**Output:**
```json
{
  "active_hooks": 3,
  "watching": 1,
  "hooks": [
    {
      "id": "hook_abc123",
      "trigger": "on-ontology-change",
      "action": "./regen.sh",
      "created_at": "2025-11-07T12:00:00Z"
    }
  ]
}
```

---

## Common Workflows

### Workflow 1: Continuous Code Quality

**Goal:** Automatically format and lint all generated code

```bash
# 1. Create format script
cat > format-and-lint.sh << 'EOF'
#!/bin/bash
set -e

echo "Formatting code..."
cargo fmt

echo "Running clippy..."
cargo clippy --fix --allow-dirty --allow-staged

echo "Checking for security issues..."
cargo audit

echo "Code quality checks complete!"
EOF

chmod +x format-and-lint.sh

# 2. Register hook
ggen hook create \
  --event post-generate \
  --script ./format-and-lint.sh \
  --name "code-quality"

# 3. Test by generating code
ggen project gen test-app --graph domain.ttl
# → Automatically formats and lints!
```

---

### Workflow 2: Ontology-Driven Development

**Goal:** Automatically regenerate code when ontology changes

```bash
# 1. Create regeneration script
cat > auto-regen.sh << 'EOF'
#!/bin/bash
ONTOLOGY_FILE=$1

echo "Ontology changed: $ONTOLOGY_FILE"

# Backup current code
mkdir -p .backups
tar -czf ".backups/$(date +%Y%m%d_%H%M%S).tar.gz" src/

# Regenerate
ggen project gen . --graph "$ONTOLOGY_FILE" --force

# Verify with tests
if cargo test; then
  echo "Regeneration successful!"
  git add .
  git commit -m "feat: Regenerate from ontology changes"
else
  echo "Tests failed! Check regenerated code."
  exit 1
fi
EOF

chmod +x auto-regen.sh

# 2. Register hook
ggen hook create \
  --event on-ontology-change \
  --script ./auto-regen.sh \
  --name "auto-regenerate"

# 3. Monitor ontology
ggen hook monitor --graph domain.ttl &

# 4. Edit ontology
vim domain.ttl  # Save changes
# → Code automatically regenerates!
```

---

### Workflow 3: Pre-Commit Validation

**Goal:** Prevent broken code from being committed

```bash
# 1. Create validation script
cat > validate-commit.sh << 'EOF'
#!/bin/bash
set -e

echo "Running pre-commit validation..."

# 1. Validate ontology syntax
if [ -f domain.ttl ]; then
  ggen graph load domain.ttl || exit 1
fi

# 2. Check formatting
cargo fmt -- --check || {
  echo "Code not formatted! Run: cargo fmt"
  exit 1
}

# 3. Run clippy
cargo clippy -- -D warnings || {
  echo "Clippy warnings detected!"
  exit 1
}

# 4. Run tests
cargo test || {
  echo "Tests failed!"
  exit 1
}

# 5. Check for TODO/FIXME
if git diff --cached | grep -E "TODO|FIXME"; then
  echo "Warning: Committing code with TODO/FIXME"
fi

echo "Pre-commit validation passed!"
EOF

chmod +x validate-commit.sh

# 2. Register hook
ggen hook create \
  --event pre-commit \
  --script ./validate-commit.sh \
  --name "commit-validator"

# 3. Install Git hook
cat > .git/hooks/pre-commit << 'EOF'
#!/bin/bash
./validate-commit.sh
EOF
chmod +x .git/hooks/pre-commit

# 4. Try committing broken code
# → Blocked by validation!
```

---

### Workflow 4: Documentation Generation

**Goal:** Auto-generate and deploy documentation

```bash
# 1. Create docs script
cat > generate-docs.sh << 'EOF'
#!/bin/bash

echo "Generating documentation..."

# 1. Rust API docs
cargo doc --no-deps

# 2. Generate README from ontology
ggen ai generate \
  "Generate README.md from this ontology" \
  --code "$(cat domain.ttl)" \
  > README.md

# 3. Generate API guide
ggen ai generate \
  "Generate API usage guide" \
  --project . \
  > docs/API.md

# 4. Build mdBook (if using)
if [ -f book.toml ]; then
  mdbook build
fi

echo "Documentation complete!"
EOF

chmod +x generate-docs.sh

# 2. Register hook
ggen hook create \
  --event post-generate \
  --script ./generate-docs.sh \
  --name "auto-docs"

# 3. Generate code - docs created automatically!
ggen project gen my-app --graph domain.ttl
```

---

### Workflow 5: CI/CD Integration

**Goal:** Trigger CI/CD pipeline on code changes

```bash
# 1. Create CI trigger script
cat > trigger-ci.sh << 'EOF'
#!/bin/bash

echo "Triggering CI/CD pipeline..."

# Commit generated code
git add .
git commit -m "chore: Regenerate code from ontology changes"

# Push to trigger CI
git push origin main

# Trigger GitHub Actions workflow
gh workflow run deploy.yml

echo "CI/CD triggered!"
EOF

chmod +x trigger-ci.sh

# 2. Register hook
ggen hook create \
  --event post-generate \
  --script ./trigger-ci.sh \
  --name "ci-trigger"

# 3. Create GitHub Actions workflow
cat > .github/workflows/deploy.yml << 'EOF'
name: Deploy

on:
  workflow_dispatch:
  push:
    branches: [main]

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Build
        run: cargo build --release

      - name: Test
        run: cargo test

      - name: Deploy
        run: ./deploy.sh
EOF
```

---

## Best Practices

### 1. Make Scripts Idempotent

**Scripts should be safe to run multiple times:**

```bash
# ❌ Bad: Fails on second run
echo "new_line" >> config.txt

# ✅ Good: Idempotent
if ! grep -q "new_line" config.txt; then
  echo "new_line" >> config.txt
fi
```

### 2. Use Exit Codes Properly

**Return appropriate exit codes:**

```bash
#!/bin/bash
set -e  # Exit on first error

# Success
cargo test && exit 0

# Failure
exit 1
```

### 3. Log Everything

**Comprehensive logging helps debugging:**

```bash
#!/bin/bash

LOG_FILE="hook-$(date +%Y%m%d).log"

{
  echo "=== Hook Started: $(date) ==="
  cargo fmt
  cargo clippy
  echo "=== Hook Completed: $(date) ==="
} 2>&1 | tee -a "$LOG_FILE"
```

### 4. Handle Errors Gracefully

**Don't fail silently:**

```bash
#!/bin/bash

if ! cargo test; then
  echo "Tests failed!" >&2
  ggen ai analyze --project . --complexity > failure-analysis.json
  exit 1
fi
```

### 5. Use Environment Variables

**Make scripts configurable:**

```bash
#!/bin/bash

# Configuration
GRAPH_FILE="${GGEN_GRAPH:-domain.ttl}"
FORCE="${GGEN_FORCE:-false}"

# Use variables
ggen project gen . --graph "$GRAPH_FILE" $([ "$FORCE" = "true" ] && echo "--force")
```

---

## Advanced Patterns

### Conditional Hooks

**Execute hooks based on conditions:**

```bash
#!/bin/bash
# conditional-hook.sh

# Only run in CI environment
if [ -n "$CI" ]; then
  cargo test --release
fi

# Only format Rust files
if git diff --name-only | grep -q "\.rs$"; then
  cargo fmt
fi

# Only regenerate if ontology changed
if git diff --name-only | grep -q "\.ttl$"; then
  ggen project gen . --graph domain.ttl --force
fi
```

### Hook Chains

**Chain multiple hooks together:**

```bash
#!/bin/bash
# hook-chain.sh

# 1. Format
./format.sh || exit 1

# 2. Lint
./lint.sh || exit 1

# 3. Test
./test.sh || exit 1

# 4. Deploy
./deploy.sh
```

### Parallel Hook Execution

**Run independent hooks in parallel:**

```bash
#!/bin/bash
# parallel-hooks.sh

# Start background jobs
./format.sh &
PID1=$!

./generate-docs.sh &
PID2=$!

./run-tests.sh &
PID3=$!

# Wait for all
wait $PID1 $PID2 $PID3

echo "All hooks completed!"
```

### Hook Dependencies

**Ensure hooks run in correct order:**

```bash
#!/bin/bash
# hook-with-deps.sh

# Check prerequisites
if [ ! -f "target/debug/ggen" ]; then
  echo "Build required first!"
  cargo build
fi

# Run dependent tasks
ggen utils doctor
ggen graph load domain.ttl
ggen project gen . --graph domain.ttl
```

---

## Troubleshooting

### Hook Not Executing

**Problem:** Hook registered but doesn't run

**Solution:**
```bash
# 1. Verify hook is registered
ggen hook list

# 2. Check script is executable
chmod +x your-script.sh

# 3. Test script manually
./your-script.sh

# 4. Check hook monitor
ggen hook monitor --once
```

### Script Errors

**Problem:** Hook script fails with errors

**Solution:**
```bash
# Add debugging
set -x  # Print commands
set -e  # Exit on error

# Check logs
cat hook-*.log

# Run with verbose output
bash -x your-script.sh
```

### Permission Issues

**Problem:** `Permission denied`

**Solution:**
```bash
# Make script executable
chmod +x script.sh

# Check file permissions
ls -la script.sh

# Use absolute path
ggen hook create --event post-generate --script "$(pwd)/script.sh"
```

### Infinite Loops

**Problem:** Hook triggers itself recursively

**Solution:**
```bash
# Add guard condition
if [ -f ".hook-running" ]; then
  echo "Hook already running, skipping..."
  exit 0
fi

touch .hook-running
# ... your hook logic ...
rm .hook-running
```

---

## Next Steps

1. **Explore Examples:** See `docs/src/examples/hooks/` for real-world scripts
2. **Template Library:** Use pre-built hook templates from marketplace
3. **Advanced Integration:** Combine hooks with AI commands for intelligent automation
4. **Contribute:** Share your hook scripts with the community

---

## References

- **Release Notes:** `docs/src/whats-new-2.5.0.md`
- **AI Integration:** `docs/src/guides/ai-guide.md`
- **Command Reference:** `docs/src/reference/cli.md`
- **Examples:** `docs/src/examples/hooks/`
