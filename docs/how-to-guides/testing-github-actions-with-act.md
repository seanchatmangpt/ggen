# Testing GitHub Actions with Act

This guide explains how to use [act](https://github.com/nektos/act) to test GitHub Actions workflows locally before pushing to GitHub.

## Overview

`act` is a tool that allows you to run GitHub Actions workflows locally using Docker containers. This enables you to:

- Test workflow changes without pushing to GitHub
- Debug workflow issues faster
- Validate workflow syntax and logic
- Test workflows in isolation

## Prerequisites

### Installation

**macOS (Homebrew):**
```bash
brew install act
```

**Linux:**
```bash
curl https://raw.githubusercontent.com/nektos/act/master/install.sh | sudo bash
```

**Or download from:** https://github.com/nektos/act/releases

### Docker

`act` requires Docker to be installed and running:

```bash
# Check Docker is running
docker ps

# If not running, start Docker Desktop (macOS/Windows) or Docker daemon (Linux)
```

### Verification

Verify `act` is installed and Docker is running:

```bash
cargo make act-install-check
```

Or manually:

```bash
act --version
docker ps
```

## Quick Start

### List Available Workflows

See all workflows that can be run with `act`:

```bash
cargo make act-list
```

### Run a Specific Workflow

Run a specific workflow job:

```bash
# Run lint workflow
cargo make act-lint

# Run test workflow
cargo make act-test

# Run build workflow
cargo make act-build

# Run CI workflow (file organization check)
cargo make act-ci
```

### Run a Specific Job from a Workflow

Run a specific job from a workflow:

```bash
# Run CI test job
cargo make act-ci-test

# Run CI fmt job
cargo make act-ci-fmt

# Run CI clippy job
cargo make act-ci-clippy
```

### Generic Workflow Runner

Run any workflow with a generic task:

```bash
# Run entire workflow
cargo make act-workflow WORKFLOW=ci.yml

# Run specific job
cargo make act-workflow WORKFLOW=ci.yml JOB=test
```

## Available Act Tasks

### Core Workflows

| Task | Description |
|------|-------------|
| `act-lint` | Run lint workflow |
| `act-test` | Run test workflow |
| `act-build` | Run build workflow |
| `act-ci` | Run CI workflow (file organization) |
| `act-ci-test` | Run CI test job |
| `act-ci-fmt` | Run CI fmt job |
| `act-ci-clippy` | Run CI clippy job |

### Marketplace Workflows

| Task | Description |
|------|-------------|
| `act-marketplace-test` | Run marketplace-test workflow |
| `act-marketplace` | Run marketplace workflow |

### Quality & Security

| Task | Description |
|------|-------------|
| `act-security-audit` | Run security-audit workflow |
| `act-quality-gates` | Run quality-gates workflow |
| `act-audit` | Run security audit workflow |

### Testing Workflows

| Task | Description |
|------|-------------|
| `act-london-tdd` | Run london-tdd-tests workflow |

### Documentation

| Task | Description |
|------|-------------|
| `act-deploy-docs` | Run deploy-docs workflow (dry-run, no deployment) |

### Utility Tasks

| Task | Description |
|------|-------------|
| `act-list` | List all available workflows |
| `act-status` | Check act installation and Docker status |
| `act-install-check` | Verify act is installed with installation instructions |
| `act-dry-run` | Dry run all workflows (no execution) |
| `act-validate` | Validate all workflows can be parsed by act |
| `act-workflow` | Run a specific workflow (generic task) |
| `act-cleanup` | Clean up act containers and images |
| `act-parallel` | Run multiple workflows in parallel |

### Lightweight Versions

For faster testing with reduced resources:

| Task | Description |
|------|-------------|
| `act-lint-light` | Run lint workflow (lightweight, 1GB RAM, 1 CPU) |
| `act-test-light` | Run test workflow (lightweight) |
| `act-build-light` | Run build workflow (lightweight) |

## Configuration

Act configuration is defined in `Makefile.toml`:

```toml
[env]
ACT_CONTAINER_ARCH = "linux/amd64"
ACT_PLATFORM = "ubuntu-latest=catthehacker/ubuntu:act-latest"
```

**Note:** The `--memory` and `--cpus` flags are not supported in all versions of `act`. These have been removed from the tasks to ensure compatibility. Docker resource limits can be configured through Docker Desktop settings or Docker daemon configuration if needed.

You can override these values by setting environment variables:

```bash
export ACT_CONTAINER_ARCH="linux/arm64"
export ACT_PLATFORM="ubuntu-latest=catthehacker/ubuntu:act-latest"
cargo make act-test
```

## Common Use Cases

### Testing Workflow Changes

Before pushing workflow changes to GitHub:

```bash
# 1. Validate workflow syntax
cargo make act-validate

# 2. Test the specific workflow
cargo make act-lint

# 3. If workflow has multiple jobs, test each job
cargo make act-ci-test
cargo make act-ci-fmt
cargo make act-ci-clippy
```

### Debugging Workflow Failures

When a workflow fails on GitHub:

```bash
# 1. Run the same workflow locally
cargo make act-test

# 2. Run with verbose output (if needed, modify task to add --verbose)
# 3. Check logs in the container
```

### Testing New Workflows

When creating a new workflow:

```bash
# 1. Create the workflow file
# 2. Validate it
cargo make act-validate

# 3. Test it
cargo make act-workflow WORKFLOW=my-new-workflow.yml
```

### Quick Feedback Loop

For quick feedback during development:

```bash
# Use lightweight versions for faster execution
cargo make act-lint-light
cargo make act-test-light
```

## Limitations

### Not Fully Supported

Some GitHub Actions features are not fully supported by `act`:

1. **Self-hosted runners** - `act` uses Docker containers, not self-hosted runners
2. **Some actions** - Custom actions or actions that require GitHub API access may not work
3. **Secrets** - Secrets must be provided via `.secrets` file or environment variables
4. **Matrix strategies** - Some complex matrix strategies may not work as expected
5. **macOS runners** - `act` runs Linux containers, so macOS-specific workflows may not work
6. **Artifacts** - Artifact upload/download may have limitations

### Workarounds

**Secrets:**
Create a `.secrets` file in the repository root:

```bash
# .secrets
GITHUB_TOKEN=your_token_here
MY_SECRET=secret_value
```

**Environment Variables:**
Pass environment variables to `act`:

```bash
act -e .env
```

Or modify the task to include environment variables.

## Best Practices

### 1. Validate Before Testing

Always validate workflow syntax first:

```bash
cargo make act-validate
```

### 2. Test Individual Jobs

For workflows with multiple jobs, test each job individually:

```bash
cargo make act-ci-test
cargo make act-ci-fmt
cargo make act-ci-clippy
```

### 3. Use Lightweight Versions for Quick Checks

For quick feedback during development:

```bash
cargo make act-lint-light
```

### 4. Clean Up Regularly

Clean up act containers and images periodically:

```bash
cargo make act-cleanup
```

### 5. Test Before Pushing

Always test workflows locally before pushing to GitHub:

```bash
# Before pushing workflow changes
cargo make act-validate
cargo make act-lint
cargo make act-test
```

## Troubleshooting

### Docker Not Running

**Error:** `Cannot connect to the Docker daemon`

**Solution:**
```bash
# Start Docker Desktop (macOS/Windows) or Docker daemon (Linux)
docker ps  # Verify it's running
```

### Act Not Installed

**Error:** `command not found: act`

**Solution:**
```bash
# macOS
brew install act

# Linux
curl https://raw.githubusercontent.com/nektos/act/master/install.sh | sudo bash
```

### Workflow Fails Locally But Works on GitHub

**Possible Causes:**
1. Missing secrets (create `.secrets` file)
2. Missing environment variables
3. Action compatibility issues
4. Platform differences (Linux vs macOS)

**Solution:**
- Check workflow logs for specific errors
- Compare local environment with GitHub Actions environment
- Test with `--dryrun` first to see what would execute

### Out of Memory

**Error:** Container runs out of memory

**Solution:**
Increase memory allocation:

```bash
export ACT_MEMORY="4g"
cargo make act-test
```

Or modify `Makefile.toml` to increase default memory.

### Slow Execution

**Solution:**
- Use lightweight versions for quick checks
- Reduce CPU/memory allocation for faster startup
- Clean up unused containers: `cargo make act-cleanup`

## Examples

### Example 1: Testing a Lint Workflow Change

```bash
# 1. Make changes to .github/workflows/lint.yml
# 2. Validate syntax
cargo make act-validate

# 3. Test the workflow
cargo make act-lint

# 4. If successful, commit and push
git add .github/workflows/lint.yml
git commit -m "Update lint workflow"
git push
```

### Example 2: Testing CI Workflow Jobs

```bash
# Test all CI jobs individually
cargo make act-ci              # File organization
cargo make act-ci-test         # Tests
cargo make act-ci-fmt          # Formatting
cargo make act-ci-clippy       # Clippy
```

### Example 3: Quick Validation

```bash
# Quick check of all workflows
cargo make act-validate

# Quick test of lint (lightweight)
cargo make act-lint-light
```

## Integration with Development Workflow

### Pre-commit Validation

Add workflow validation to your pre-commit checks:

```bash
# In your pre-commit hook or CI
cargo make act-validate
```

### CI/CD Integration

While `act` is primarily for local testing, you can use it in CI to validate workflows:

```yaml
# .github/workflows/validate-workflows.yml
- name: Validate workflows
  run: cargo make act-validate
```

## Additional Resources

- [act Documentation](https://github.com/nektos/act)
- [act GitHub Repository](https://github.com/nektos/act)
- [GitHub Actions Documentation](https://docs.github.com/en/actions)

## Summary

Using `act` to test GitHub Actions workflows locally provides:

- ✅ Faster feedback loop
- ✅ No need to push to GitHub for testing
- ✅ Ability to debug workflows locally
- ✅ Validation of workflow syntax and logic
- ✅ Testing workflows in isolation

Remember to:
- Validate workflows before testing
- Test individual jobs for complex workflows
- Use lightweight versions for quick checks
- Clean up containers regularly
- Test before pushing to GitHub

