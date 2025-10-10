<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Pattern 012: CI DRIFT CHECK](#pattern-012-ci-drift-check)
  - [Problem](#problem)
  - [Solution](#solution)
  - [Prerequisites](#prerequisites)
  - [Step-by-Step](#step-by-step)
    - [1. Create Reproducible Plans](#1-create-reproducible-plans)
    - [2. Add CI Drift Check Script](#2-add-ci-drift-check-script)
    - [3. Integrate into CI Pipeline](#3-integrate-into-ci-pipeline)
  - [Complete Example](#complete-example)
    - [Project Structure](#project-structure)
    - [Template (`templates/config.tmpl`)](#template-templatesconfigtmpl)
    - [Generate Plans for Each Environment](#generate-plans-for-each-environment)
    - [Comprehensive Drift Check Script](#comprehensive-drift-check-script)
    - [CI Configuration (GitLab CI)](#ci-configuration-gitlab-ci)
  - [Explanation](#explanation)
    - [Drift Detection Strategies](#drift-detection-strategies)
      - [1. **Plan-Based Verification** (Recommended)](#1-plan-based-verification-recommended)
      - [2. **Regeneration Comparison**](#2-regeneration-comparison)
      - [3. **Content Hash Verification**](#3-content-hash-verification)
    - [Handling Drift](#handling-drift)
      - [Option 1: Accept Drift (Update Plan)](#option-1-accept-drift-update-plan)
      - [Option 2: Reject Drift (Regenerate)](#option-2-reject-drift-regenerate)
      - [Option 3: Merge Drift](#option-3-merge-drift)
  - [Expected Output](#expected-output)
  - [Common Pitfalls](#common-pitfalls)
  - [Variations](#variations)
    - [💡 Scheduled Drift Detection](#-scheduled-drift-detection)
    - [💡 Drift Auto-Fix PR](#-drift-auto-fix-pr)
    - [💡 Drift Severity Levels](#-drift-severity-levels)
    - [💡 Environment-Specific Checks](#-environment-specific-checks)
  - [Troubleshooting](#troubleshooting)
  - [See Also](#see-also)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Pattern 012: CI DRIFT CHECK

**Difficulty**: ⭐⭐⭐ Advanced
**Time**: 30min
**Tags**: #ci-cd #drift-detection #automation #validation #continuous-verification

## Problem

Generated code can drift from its source templates when:
- Developers manually edit generated files
- Templates are updated but code isn't regenerated
- Environment-specific configurations diverge from plans

You need automated checks in CI/CD to detect and prevent drift, ensuring generated code stays synchronized with templates and preventing "configuration drift" at scale.

## Solution

Implement CI pipeline checks that:
1. Regenerate code from templates in a clean environment
2. Compare generated output against committed code
3. Fail the build if drift is detected
4. Provide actionable reports on what diverged

## Prerequisites

- CI/CD system (GitHub Actions, GitLab CI, Jenkins, etc.)
- ggen installed (v0.2.4+)
- Plans checked into version control
- Understanding of Patterns 009, 010, 011

## Step-by-Step

### 1. Create Reproducible Plans

```bash
# Generate deterministic plans for CI
ggen project plan templates/*.tmpl \
  --vars env=ci \
  --determinism 42 \
  --output-plan plans/ci-verification.plan.json

# Commit to version control
git add plans/ci-verification.plan.json
git commit -m "ci: add verification plan"
```

### 2. Add CI Drift Check Script

```bash
#!/bin/bash
# scripts/check-drift.sh
set -e

PLAN_FILE="${1:-plans/ci-verification.plan.json}"
TEMP_DIR=$(mktemp -d)

echo "🔍 Checking for drift in: $PLAN_FILE"

# Apply plan to temporary directory
ggen project apply "$PLAN_FILE" \
  --output-dir "$TEMP_DIR" \
  --verify

# Compare against committed files
if diff -r "$TEMP_DIR" . --exclude=".git" --exclude="target"; then
  echo "✅ No drift detected - generated code matches templates"
  exit 0
else
  echo "❌ Drift detected - generated code differs from templates"
  echo ""
  echo "📊 Detailed diff:"
  diff -ur "$TEMP_DIR" . --exclude=".git" --exclude="target" || true
  exit 1
fi
```

### 3. Integrate into CI Pipeline

**GitHub Actions**:
```yaml
# .github/workflows/drift-check.yml
name: Drift Check

on:
  pull_request:
    paths:
      - 'src/**'
      - 'templates/**'
      - 'plans/**'
  schedule:
    - cron: '0 0 * * 0'  # Weekly

jobs:
  check-drift:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Install ggen
        run: |
          curl -L https://github.com/seanchatmangpt/ggen/releases/download/v0.2.4/ggen-x86_64-linux.tar.gz | tar xz
          sudo mv ggen /usr/local/bin/

      - name: Check for drift
        run: |
          chmod +x scripts/check-drift.sh
          ./scripts/check-drift.sh plans/ci-verification.plan.json

      - name: Report drift (if any)
        if: failure()
        run: |
          echo "⚠️ Drift detected! Generated code doesn't match templates."
          echo "To fix:"
          echo "  1. Review the diff above"
          echo "  2. Run: ggen project apply plans/ci-verification.plan.json"
          echo "  3. Commit the changes"
```

## Complete Example

**Scenario**: Multi-environment configuration with drift detection.

### Project Structure
```
my-project/
├── templates/
│   ├── config.tmpl          # Environment configuration
│   └── deployment.tmpl      # Deployment manifests
├── plans/
│   ├── dev.plan.json       # Development plan
│   ├── staging.plan.json   # Staging plan
│   └── prod.plan.json      # Production plan
├── config/
│   ├── dev.yaml            # Generated config
│   ├── staging.yaml
│   └── prod.yaml
└── .github/workflows/
    └── drift-check.yml
```

### Template (`templates/config.tmpl`)
```yaml
---
to: "config/{{env}}.yaml"
determinism: 42
vars:
  env: "dev"
  db_host: "localhost"
  db_port: 5432
  log_level: "debug"
---
environment: {{env}}
database:
  host: {{db_host}}
  port: {{db_port}}
logging:
  level: {{log_level}}
```

### Generate Plans for Each Environment

```bash
# Development
ggen project plan templates/config.tmpl \
  --vars env=dev db_host=localhost log_level=debug \
  --output-plan plans/dev.plan.json

# Staging
ggen project plan templates/config.tmpl \
  --vars env=staging db_host=staging.db.internal log_level=info \
  --output-plan plans/staging.plan.json

# Production
ggen project plan templates/config.tmpl \
  --vars env=prod db_host=prod.db.internal log_level=warn \
  --output-plan plans/prod.plan.json
```

### Comprehensive Drift Check Script

```bash
#!/bin/bash
# scripts/check-all-drift.sh
set -e

FAILED=0
PLANS=(plans/*.plan.json)

echo "🔍 Checking drift for ${#PLANS[@]} plans..."
echo ""

for plan in "${PLANS[@]}"; do
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  echo "📋 Plan: $plan"
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

  # Dry-run apply to check for changes
  if ggen project apply "$plan" --dry-run --verify 2>&1 | tee /tmp/drift.log; then
    echo "✅ No drift detected"
  else
    echo "❌ Drift detected!"
    FAILED=$((FAILED + 1))

    # Extract affected files
    echo ""
    echo "📝 Affected files:"
    ggen project diff --plan "$plan" --stat
  fi

  echo ""
done

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "📊 Summary"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Total plans checked: ${#PLANS[@]}"
echo "Plans with drift: $FAILED"
echo ""

if [ $FAILED -gt 0 ]; then
  echo "❌ Drift check failed"
  echo ""
  echo "To fix drift:"
  echo "  1. Review diffs above"
  echo "  2. Apply plans: make apply-all"
  echo "  3. Commit changes"
  exit 1
else
  echo "✅ All plans up-to-date"
  exit 0
fi
```

### CI Configuration (GitLab CI)

```yaml
# .gitlab-ci.yml
stages:
  - verify
  - report

drift-check:
  stage: verify
  image: rust:latest
  before_script:
    - cargo install ggen
  script:
    - chmod +x scripts/check-all-drift.sh
    - ./scripts/check-all-drift.sh
  artifacts:
    when: on_failure
    paths:
      - drift-report.txt
    expire_in: 1 week
  only:
    - merge_requests
    - schedules

drift-report:
  stage: report
  when: on_failure
  dependencies:
    - drift-check
  script:
    - |
      cat > comment.md <<EOF
      ## ⚠️ Configuration Drift Detected

      Generated code has drifted from templates. See artifacts for details.

      **Action Required**:
      1. Download drift report artifact
      2. Review changes
      3. Run \`make apply-all\` to resync
      4. Commit updated files
      EOF
    - gitlab-mr-comment comment.md
```

## Explanation

### Drift Detection Strategies

#### 1. **Plan-Based Verification** (Recommended)
```bash
# Verify files match plan without regenerating
ggen project apply plans/prod.plan.json --dry-run --verify
```
- **Pros**: Fast, uses content hashes, deterministic
- **Cons**: Requires plans to be up-to-date
- **Use case**: Production environments, strict change control

#### 2. **Regeneration Comparison**
```bash
# Regenerate in temp dir and compare
TEMP=$(mktemp -d)
ggen project apply plans/prod.plan.json --output-dir "$TEMP"
diff -r "$TEMP" config/
```
- **Pros**: Catches template changes, end-to-end verification
- **Cons**: Slower, requires full regeneration
- **Use case**: Development, CI pipelines

#### 3. **Content Hash Verification**
```bash
# Check content hashes without full diff
ggen project verify-hashes plans/prod.plan.json
```
- **Pros**: Very fast, cryptographically verifiable
- **Cons**: Doesn't show what changed
- **Use case**: Quick pre-commit checks

### Handling Drift

When drift is detected, you have several options:

#### Option 1: Accept Drift (Update Plan)
```bash
# Manual edits were intentional, update plan to match
ggen project plan templates/config.tmpl \
  --from-existing config/prod.yaml \
  --output-plan plans/prod.plan.json

# Or extract variables from existing file
ggen project extract-vars config/prod.yaml \
  > extracted-vars.json

ggen project plan templates/config.tmpl \
  --vars-file extracted-vars.json \
  --output-plan plans/prod.plan.json
```

#### Option 2: Reject Drift (Regenerate)
```bash
# Overwrite manual edits, restore from template
ggen project apply plans/prod.plan.json --force
```

#### Option 3: Merge Drift
```bash
# Create new template incorporating manual changes
ggen project diff --plan plans/prod.plan.json > drift.patch

# Review and selectively apply
patch -p1 < drift.patch

# Update template to match
vim templates/config.tmpl
```

## Expected Output

**No Drift (Success)**:
```
🔍 Checking drift for 3 plans...

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
📋 Plan: plans/dev.plan.json
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
✅ No drift detected

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
📋 Plan: plans/staging.plan.json
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
✅ No drift detected

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
📋 Plan: plans/prod.plan.json
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
✅ No drift detected

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
📊 Summary
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Total plans checked: 3
Plans with drift: 0

✅ All plans up-to-date
```

**Drift Detected (Failure)**:
```
🔍 Checking drift for 3 plans...

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
📋 Plan: plans/prod.plan.json
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
❌ Drift detected!

📝 Affected files:
--- a/config/prod.yaml
+++ b/config/prod.yaml (from plan)
@@ -3,4 +3,4 @@
   host: prod.db.internal
   port: 5432
 logging:
-  level: error  # ← Manual edit
+  level: warn   # ← Plan expects this

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
📊 Summary
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Total plans checked: 3
Plans with drift: 1

❌ Drift check failed

To fix drift:
  1. Review diffs above
  2. Apply plans: make apply-all
  3. Commit changes
```

## Common Pitfalls

❌ **Mistake**: Not using deterministic plans
- **Symptom**: False positive drift from non-deterministic generation
- **Fix**: Set `determinism: <seed>` in templates, use `ORDER BY` in SPARQL

❌ **Mistake**: Checking drift on every commit
- **Symptom**: CI slowdowns, developer friction
- **Fix**: Only check drift on template/plan changes or scheduled runs

❌ **Mistake**: No process for handling drift
- **Symptom**: Drift warnings ignored, builds always failing
- **Fix**: Document drift resolution process, make it actionable

❌ **Mistake**: Plans not in version control
- **Symptom**: Can't reproduce drift checks, inconsistent results
- **Fix**: Commit all plans, treat them as source of truth

## Variations

### 💡 Scheduled Drift Detection

```yaml
# Run weekly drift check even without code changes
on:
  schedule:
    - cron: '0 0 * * 0'  # Every Sunday at midnight
```

### 💡 Drift Auto-Fix PR

```yaml
# Automatically create PR to fix drift
- name: Auto-fix drift
  if: failure()
  run: |
    ggen project apply plans/*.plan.json --force
    git config user.name "drift-bot"
    git config user.email "bot@example.com"
    git checkout -b fix/drift-$(date +%s)
    git add .
    git commit -m "fix: resolve configuration drift"
    git push origin HEAD
    gh pr create --title "Fix: Configuration Drift" \
      --body "Automated PR to resolve detected drift"
```

### 💡 Drift Severity Levels

```bash
# Allow minor drift (formatting), fail on major drift (logic)
ggen project diff --plan plans/prod.plan.json \
  --ignore-whitespace \
  --ignore-comments \
  --fail-on major
```

### 💡 Environment-Specific Checks

```yaml
jobs:
  dev-drift:
    if: github.ref == 'refs/heads/develop'
    steps:
      - run: ./scripts/check-drift.sh plans/dev.plan.json

  prod-drift:
    if: github.ref == 'refs/heads/main'
    steps:
      - run: ./scripts/check-drift.sh plans/prod.plan.json
```

## Troubleshooting

**Issue**: Drift check always fails with whitespace differences
**Cause**: Editor auto-formatting or line ending differences
**Solution**:
```bash
# Normalize before checking
prettier --write config/*.yaml
dos2unix config/*.yaml

# Or ignore whitespace in diff
ggen project diff --plan plans/prod.plan.json --ignore-whitespace
```

**Issue**: Drift check times out in CI
**Cause**: Large plans, slow regeneration
**Solution**:
```bash
# Use hash verification instead of full diff
ggen project verify-hashes plans/prod.plan.json --fast

# Or parallelize checks
parallel -j4 './scripts/check-drift.sh {}' ::: plans/*.plan.json
```

**Issue**: False positives from environment-specific values
**Cause**: Hardcoded timestamps, UUIDs, or dynamic values
**Solution**:
```yaml
# Use deterministic values in templates
determinism: 42
vars:
  timestamp: "2025-01-01T00:00:00Z"  # Fixed
  # Not: timestamp: "{{ now() }}"
```

## See Also

- Pattern 009: Project Plan - Create verifiable plans
- Pattern 010: Idempotent Apply - Safe plan application
- Pattern 011: Dry-Run Diff - Preview changes
- Recipe 6.1: Reproducible Output - Deterministic generation
- Recipe 13.6: Linting Templates - Template quality checks

## Next Steps

- Set up weekly drift checks in CI
- Create runbooks for drift resolution
- Add drift metrics to dashboards
- Integrate with change management process
- Document approved drift exceptions
