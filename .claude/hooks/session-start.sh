#!/bin/bash
# Timeout: 3s
# Purpose: Initialize ggen environment and validate workspace

set -euo pipefail

timeout 3s bash -c '
  WORKSPACE_ROOT="/home/user/ggen"

  echo "🚀 Initializing ggen session..."

  # Verify we'\''re in the workspace
  if [[ ! -d "$WORKSPACE_ROOT" ]]; then
    echo "❌ ERROR: ggen workspace not found at $WORKSPACE_ROOT" >&2
    exit 1
  fi

  # Verify cargo make is available
  if ! command -v cargo &> /dev/null; then
    echo "❌ ERROR: cargo not found in PATH" >&2
    exit 1
  fi

  # Verify timeout command exists
  if ! command -v timeout &> /dev/null; then
    echo "⚠️  WARNING: timeout command not found" >&2
    echo "   → Install: apt-get install coreutils (Linux) or brew install coreutils (macOS)" >&2
  fi

  # Check Cargo.toml exists
  if [[ ! -f "$WORKSPACE_ROOT/Cargo.toml" ]]; then
    echo "❌ ERROR: Cargo.toml not found - not a valid workspace" >&2
    exit 1
  fi

  # Check Makefile.toml exists
  if [[ ! -f "$WORKSPACE_ROOT/Makefile.toml" ]]; then
    echo "⚠️  WARNING: Makefile.toml not found - cargo make may not work" >&2
  fi

  # Verify workspace structure
  REQUIRED_DIRS=("crates" ".specify" "tests" "docs")
  for dir in "${REQUIRED_DIRS[@]}"; do
    if [[ ! -d "$WORKSPACE_ROOT/$dir" ]]; then
      echo "⚠️  WARNING: Required directory missing: $dir" >&2
    fi
  done

  # Check git status
  if [[ -d "$WORKSPACE_ROOT/.git" ]]; then
    cd "$WORKSPACE_ROOT"

    # Check for uncommitted changes
    if ! git diff-index --quiet HEAD -- 2>/dev/null; then
      CHANGED_FILES=$(git status --porcelain | wc -l)
      echo "ℹ️  Git: $CHANGED_FILES uncommitted changes" >&2
    fi

    # Check current branch
    BRANCH=$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "unknown")
    echo "ℹ️  Git: On branch $BRANCH" >&2
  fi

  # Display key commands
  echo ""
  echo "📋 Key Commands:"
  echo "   cargo make check       - Quick compilation check (<5s)"
  echo "   cargo make test-unit   - Fast unit tests (<16s)"
  echo "   cargo make pre-commit  - Full pre-commit checks (<2min)"
  echo "   cargo make lint        - Clippy + rustfmt (<60s)"
  echo "   ggen sync              - Full μ pipeline (μ₁-μ₅)"
  echo ""

  # Display Andon reminder
  echo "🚨 Remember: STOP THE LINE when Andon signals appear!"
  echo "   🔴 CRITICAL: Compiler errors, test failures"
  echo "   🟡 HIGH: Warnings, clippy errors"
  echo "   🟢 GREEN: All checks pass"
  echo ""

  # Check for any immediate Andon signals
  cd "$WORKSPACE_ROOT"
  if command -v cargo &> /dev/null; then
    # Quick check without full build
    if ! cargo check --workspace --all-targets --quiet 2>&1 | head -10 | grep -qE "error"; then
      echo "✅ Initial workspace check: CLEAN"
    else
      echo "🔴 ANDON: Workspace has compilation errors - run: cargo make check"
    fi
  fi

  echo ""
  echo "✅ Session initialized successfully"

  exit 0
'

exit $?
