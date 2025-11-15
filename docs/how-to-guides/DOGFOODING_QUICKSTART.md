# How to Dogfood ggen (Fix Your Own Code)

## What is Dogfooding?

"Eating your own dog food" means using ggen to improve ggen's own codebase. This demonstrates that ggen works reliably for real, production-grade code and serves as the ultimate proof of its capabilities.

## Problem Statement

Production code may contain panic points (`unwrap()`, `expect()`, etc.) that could crash the application. Instead of manually finding and fixing these issues, you can use ggen's automation tools to:

- Automatically detect all panic points
- Fix them systematically
- Prevent new ones from being introduced
- Validate code safety continuously

## Prerequisites

- ggen installed and working
- Access to the ggen source code repository
- Basic familiarity with Rust and error handling patterns

## Step-by-Step: Fix Production Code Issues

### Step 1: Check Current State

First, scan your codebase to identify all panic points:

```bash
./scripts/check-no-panic-points.sh
```

Output:
```
PASS: No panic points found
```
or
```
FAIL: Found 403 panic points in: src/cli, src/core, src/api
```

### Step 2: Automatically Fix Panic Points

Once you understand the scope, use ggen to fix them automatically:

```bash
# Preview changes without applying them
cargo script scripts/fix-panic-points.rs --dry-run

# Apply fixes to specific paths
cargo script scripts/fix-panic-points.rs src/cli src/core

# Or fix all paths
cargo script scripts/fix-panic-points.rs
```

This command:
- Detects all `unwrap()`, `expect()`, and other panic points
- Generates safe error handling using ggen templates
- Applies fixes while preserving code logic

### Step 3: Validate the Results

After fixes, verify the code is safe:

```bash
./scripts/check-no-panic-points.sh
```

Expected output:
```
PASS: No panic points found
```

### Step 4: Set Up Prevention (Pre-Commit Hook)

Prevent new panic points from being committed:

```bash
ggen lifecycle run setup-git-hooks
```

Now, any commit containing panic points will be automatically blocked:

```bash
git commit -m "Add new feature"
# Hook runs: Check for panic points
# FAIL: New panic point detected in src/feature.rs:42
# Commit rejected
```

## Validation Strategies

### Complete Production Validation

Run all production checks:

```bash
ggen lifecycle run production-validate
```

This includes:
- Panic point detection
- Error handling validation
- Type safety checks
- Performance benchmarks

### Safety Check Only

For quick validation:

```bash
ggen lifecycle run validate-safety
```

### Full Dogfooding Workflow

Complete dogfooding cycle:

```bash
ggen lifecycle run dogfood
```

## Using Safe Error Handling Templates

After fixing panic points, reference the safe patterns for future code:

```bash
ggen template generate templates/safe-error-handling.tmpl
```

This generates examples of:
- Proper `Result<T, E>` usage
- Error propagation patterns
- Structured logging
- User-friendly error messages

## See Also

- [Production Deployment Guide](deploy-production.md) - Full production readiness checklist
- [Configure Hooks Guide](configure-hooks.md) - Setting up git hooks
- [How to Troubleshoot](troubleshoot.md) - Resolving issues
