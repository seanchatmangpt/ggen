<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [CI/CD Integration: Automate Code Generation in Pipelines](#cicd-integration-automate-code-generation-in-pipelines)
  - [Goal](#goal)
  - [Prerequisites](#prerequisites)
  - [Step 1: Create GitHub Actions Workflow](#step-1-create-github-actions-workflow)
  - [Step 2: Add Validation Workflow](#step-2-add-validation-workflow)
  - [Step 3: Add Pre-Commit Hook Locally](#step-3-add-pre-commit-hook-locally)
  - [Step 4: Create Release Workflow](#step-4-create-release-workflow)
  - [Step 5: Configure Gitflow](#step-5-configure-gitflow)
  - [Step 6: Monitor Code Generation](#step-6-monitor-code-generation)
  - [Step 7: Real-World Example](#step-7-real-world-example)
  - [Best Practices](#best-practices)
  - [Troubleshooting](#troubleshooting)
  - [Next Steps](#next-steps)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# CI/CD Integration: Automate Code Generation in Pipelines

Integrate ggen into your GitHub Actions workflow to automatically regenerate code when your ontology changes.

## Goal

Create a GitHub Actions workflow that validates your ontology and regenerates code on every push, keeping your repository in sync.

## Prerequisites

- ggen installed locally
- Git repository on GitHub
- Completed [Multi-Language Project](07-multi-language-project.md)
- Basic familiarity with GitHub Actions

## Step 1: Create GitHub Actions Workflow

Create `.github/workflows/generate-code.yml`:

```yaml
name: Generate Code from Ontology

on:
  push:
    branches: [ main, develop ]
    paths:
      - '**.ttl'  # Trigger on ontology changes
      - '.github/workflows/generate-code.yml'
  pull_request:
    branches: [ main ]

jobs:
  generate:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v3
        with:
          fetch-depth: 0

      - name: Install ggen
        run: |
          cargo install ggen --version "0.1"

      - name: Validate ontology
        run: |
          ggen ontology validate shared-domain.ttl
          echo "✓ Ontology validated"

      - name: Generate Rust models
        run: |
          ggen ontology generate shared-domain.ttl \
            --language rust \
            --output rust/src/models.rs

      - name: Generate TypeScript models
        run: |
          ggen ontology generate shared-domain.ttl \
            --language typescript \
            --output typescript/src/models.ts

      - name: Generate Python models
        run: |
          ggen ontology generate shared-domain.ttl \
            --language python \
            --output python/models.py

      - name: Check for changes
        id: changes
        run: |
          if git diff --quiet; then
            echo "has_changes=false" >> $GITHUB_OUTPUT
          else
            echo "has_changes=true" >> $GITHUB_OUTPUT
          fi

      - name: Commit generated code
        if: steps.changes.outputs.has_changes == 'true'
        run: |
          git config --local user.email "ggen-bot@example.com"
          git config --local user.name "ggen-bot"
          git add rust/src/models.rs typescript/src/models.ts python/models.py
          git commit -m "chore: regenerate models from ontology [skip ci]"

      - name: Push changes
        if: steps.changes.outputs.has_changes == 'true'
        uses: ad-m/github-push-action@master
        with:
          github_token: ${{ secrets.GITHUB_TOKEN }}
          branch: ${{ github.ref }}
```

## Step 2: Add Validation Workflow

Create `.github/workflows/validate-models.yml`:

```yaml
name: Validate Generated Models

on:
  push:
  pull_request:

jobs:
  validate:
    runs-on: ubuntu-latest

    strategy:
      matrix:
        language: [ rust, typescript, python ]

    steps:
      - uses: actions/checkout@v3

      - name: Set up Rust
        if: matrix.language == 'rust'
        uses: dtolnay/rust-toolchain@stable

      - name: Set up Node.js
        if: matrix.language == 'typescript'
        uses: actions/setup-node@v3
        with:
          node-version: '18'

      - name: Set up Python
        if: matrix.language == 'python'
        uses: actions/setup-python@v4
        with:
          python-version: '3.11'

      - name: Validate Rust models
        if: matrix.language == 'rust'
        run: |
          cd rust
          cargo check
          cargo test --lib

      - name: Validate TypeScript models
        if: matrix.language == 'typescript'
        run: |
          cd typescript
          npm install
          npm run type-check
          npm test

      - name: Validate Python models
        if: matrix.language == 'python'
        run: |
          cd python
          pip install -e .
          python -m pytest
```

## Step 3: Add Pre-Commit Hook Locally

Create `.husky/pre-commit`:

```bash
#!/bin/bash

# Regenerate code before committing if ontology changed
ONTOLOGY_CHANGED=$(git diff --cached --name-only | grep -E '\.ttl$')

if [ ! -z "$ONTOLOGY_CHANGED" ]; then
  echo "Ontology changed, regenerating code..."

  ggen ontology generate shared-domain.ttl --language rust --output rust/src/models.rs
  ggen ontology generate shared-domain.ttl --language typescript --output typescript/src/models.ts
  ggen ontology generate shared-domain.ttl --language python --output python/models.py

  git add rust/src/models.rs typescript/src/models.ts python/models.py
  echo "✓ Code regenerated and staged"
fi

# Run tests
cargo test --lib
npm test
python -m pytest
```

Install husky:

```bash
npm install husky --save-dev
npx husky install
```

## Step 4: Create Release Workflow

Create `.github/workflows/release.yml`:

```yaml
name: Release

on:
  push:
    tags:
      - 'v*'

jobs:
  release:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v3

      - name: Validate ontology matches release tag
        run: |
          # Get version from tag
          VERSION=${GITHUB_REF#refs/tags/v}

          # Verify ontology version matches
          ggen ontology validate shared-domain.ttl \
            --expected-version "$VERSION"

      - name: Generate release notes
        run: |
          ggen utils doctor > release-notes.md

      - name: Create Release
        uses: softprops/action-gh-release@v1
        with:
          files: release-notes.md
          body_path: CHANGELOG.md
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
```

## Step 5: Configure Gitflow

Set up branch protection with code generation requirements:

In GitHub Settings → Branches → Branch protection rules:

- ✓ Require status checks to pass before merging
- ✓ Require code reviews before merging
- ✓ Require branches to be up to date before merging
- ✓ Require linear history

## Step 6: Monitor Code Generation

Create a dashboard script `scripts/monitor-generation.sh`:

```bash
#!/bin/bash

echo "Monitoring code generation health..."

# Check if models are in sync
ggen ontology generate shared-domain.ttl --language rust --output /tmp/rust-models.rs
diff -q rust/src/models.rs /tmp/rust-models.rs || {
  echo "ERROR: Rust models out of sync!"
  exit 1
}

ggen ontology generate shared-domain.ttl --language typescript --output /tmp/ts-models.ts
diff -q typescript/src/models.ts /tmp/ts-models.ts || {
  echo "ERROR: TypeScript models out of sync!"
  exit 1
}

ggen ontology generate shared-domain.ttl --language python --output /tmp/models.py
diff -q python/models.py /tmp/models.py || {
  echo "ERROR: Python models out of sync!"
  exit 1
}

echo "✓ All models synchronized"
```

## Step 7: Real-World Example

When a developer updates the ontology:

```bash
# 1. Edit shared-domain.ttl to add a new field
git add shared-domain.ttl

# 2. Pre-commit hook regenerates code automatically
git commit -m "feat: add user roles to domain model"
# Output: "Ontology changed, regenerating code..."

# 3. GitHub Actions validates all languages on push
# Output: ✓ Rust models compile
#         ✓ TypeScript types check
#         ✓ Python models validate

# 4. Auto-commit pushes regenerated code
# (PR shows both ontology and generated code changes)

# 5. Code reviewers see:
#    - Ontology changes
#    - Exact corresponding code changes in all languages
#    - No drift, no manual sync needed
```

## Best Practices

**1. Fail fast on validation errors**:
```yaml
- name: Validate before generation
  run: ggen ontology validate shared-domain.ttl || exit 1
```

**2. Use structured commit messages**:
```
chore: regenerate models from ontology [skip ci]
```

**3. Tag releases with version**:
```bash
git tag -a v1.2.0 -m "Release with new user roles model"
```

**4. Monitor SLOs**:
```bash
# Ensure generation completes within timeout
timeout 60s ggen ontology generate shared-domain.ttl --language rust --output rust/src/models.rs
```

**5. Document breaking changes**:
```
## Breaking Changes in v2.0.0
- User.metadata changed from string to JSON object
- All services must regenerate and redeploy
```

## Troubleshooting

**Models out of sync?**
```bash
# Regenerate all
./scripts/monitor-generation.sh

# Commit the updates
git add rust/src/models.rs typescript/src/models.ts python/models.py
git commit -m "chore: sync models with ontology"
```

**Workflow failing?**
```bash
# Test locally
ggen ontology validate shared-domain.ttl
ggen ontology generate shared-domain.ttl --language rust --output /tmp/test.rs
```

## Next Steps

- [Testing Generated Code](09-testing-generated-code.md)
- Deploy with confidence knowing all services are in sync
- Scale to 10+ microservices with same approach
- [Advanced patterns](../how-to-guides/ci-cd-workflows.md)

## Summary

You've learned:
- ✅ How to automate code generation in CI/CD
- ✅ How to validate ontologies before generation
- ✅ How to handle generated code in version control
- ✅ How to keep polyglot services synchronized
- ✅ How to prevent manual synchronization errors

Your CI/CD pipeline is now ggen-enabled!
