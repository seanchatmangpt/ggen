<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Git Hooks System Implementation Blueprint](#git-hooks-system-implementation-blueprint)
  - [Overview](#overview)
  - [Architecture](#architecture)
  - [1. Unified Library: `scripts/lib/hooks-common.sh`](#1-unified-library-scriptslibhooks-commonsh)
    - [Purpose](#purpose)
    - [Function Signatures](#function-signatures)
  - [2. Pre-Commit Hook: `scripts/hooks/pre-commit.sh`](#2-pre-commit-hook-scriptshookspre-commitsh)
    - [Purpose](#purpose-1)
    - [Gate Execution Order](#gate-execution-order)
    - [Andon Signal Rules](#andon-signal-rules)
    - [Error Handling Strategy](#error-handling-strategy)
    - [Script Structure](#script-structure)
  - [3. Pre-Push Hook: `scripts/hooks/pre-push.sh`](#3-pre-push-hook-scriptshookspre-pushsh)
    - [Purpose](#purpose-2)
    - [Gate Execution Order](#gate-execution-order-1)
    - [Timeout Values](#timeout-values)
    - [Andon Signal Rules](#andon-signal-rules-1)
    - [Parallel Execution Strategy](#parallel-execution-strategy)
    - [Error Handling Strategy](#error-handling-strategy-1)
    - [Script Structure](#script-structure-1)
  - [4. Installation Script: `scripts/install-hooks.sh`](#4-installation-script-scriptsinstall-hookssh)
    - [Purpose](#purpose-3)
    - [Script Structure](#script-structure-2)
    - [Installation Flow](#installation-flow)
    - [Configuration File: `.hooks.conf`](#configuration-file-hooksconf)
  - [Summary: Gate Execution Matrix](#summary-gate-execution-matrix)
  - [Implementation Priority](#implementation-priority)
  - [File Paths (Absolute)](#file-paths-absolute)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Git Hooks System Implementation Blueprint

## Overview

This document defines the implementation blueprint for the unified git hooks system with Andon signal support, timeout management, and gate-based validation.

## Architecture

```
scripts/
  lib/
    hooks-common.sh      # Unified library with core functions
  hooks/
    pre-commit.sh        # Fast-tier validation (<5s)
    pre-push.sh          # Full-tier validation (<60s)
  install-hooks.sh       # Installation script
```

---

## 1. Unified Library: `scripts/lib/hooks-common.sh`

### Purpose
Centralized library providing consistent gate execution, Andon signals, timeout handling, and output formatting for all git hooks.

### Function Signatures

```bash
#!/usr/bin/env bash
# Unified Git Hooks Library
# Provides: run_gate(), color_output(), andon_signal(), timeout_check()

#==============================================================================
# CONSTANTS
#==============================================================================

# Exit codes
readonly GATE_SUCCESS=0
readonly GATE_FAILURE=1
readonly GATE_TIMEOUT=124
readonly GATE_SKIPPED=125

# Andon signal levels
readonly ANDON_GREEN=0    # All clear
readonly ANDON_YELLOW=1   # Warning - investigate
readonly ANDON_RED=2      # Critical - stop the line

# Default timeouts (seconds)
readonly DEFAULT_TIMEOUT_FAST=5
readonly DEFAULT_TIMEOUT_STANDARD=15
readonly DEFAULT_TIMEOUT_EXTENDED=60

#==============================================================================
# COLOR OUTPUT
#==============================================================================

# color_output(level, message)
# Outputs colored message to stderr
# Args:
#   level: "info" | "success" | "warn" | "error" | "gate" | "andon_red" | "andon_yellow"
#   message: String to output
# Returns: void (prints to stderr)

#==============================================================================
# TIMEOUT CHECK
#==============================================================================

# timeout_check()
# Verifies timeout command is available
# Returns:
#   0 - timeout command available
#   1 - timeout command missing (prints install instructions)
# Side effects: Exports TIMEOUT_CMD variable

#==============================================================================
# ANDON SIGNALS
#==============================================================================

# andon_signal(level, gate_name, message)
# Emits visual Andon alert with appropriate action guidance
# Args:
#   level: "green" | "yellow" | "red"
#   gate_name: Name of the gate that triggered signal
#   message: Detailed error/warning message
# Returns:
#   0 for green
#   1 for yellow (warning)
#   2 for red (critical)
# Side effects: Logs to ANDON_LOG_FILE if set

# andon_summary(passed_count, warned_count, failed_count)
# Prints final Andon status summary
# Args:
#   passed_count: Number of green gates
#   warned_count: Number of yellow gates
#   failed_count: Number of red gates
# Returns: Overall status code (0 if no red, 1 if any red)

#==============================================================================
# GATE EXECUTION
#==============================================================================

# run_gate(name, command, timeout, retry_count, andon_on_fail)
# Executes a validation gate with timeout, retry, and Andon support
# Args:
#   name: Human-readable gate name (e.g., "Cargo Check")
#   command: Command to execute (e.g., "cargo make check")
#   timeout: Timeout in seconds (default: DEFAULT_TIMEOUT_STANDARD)
#   retry_count: Number of retries on failure (default: 0)
#   andon_on_fail: Andon level on failure - "red" | "yellow" (default: "red")
# Returns:
#   0 - Gate passed
#   1 - Gate failed (after retries)
#   124 - Gate timed out
# Side effects:
#   - Outputs progress to stderr
#   - Captures output to temp file for error display
#   - Triggers andon_signal on failure

# run_gate_parallel(gate_specs...)
# Executes multiple gates in parallel where safe
# Args:
#   gate_specs: Array of "name:command:timeout" specifications
# Returns:
#   0 - All gates passed
#   1 - One or more gates failed
# Side effects: Waits for all gates to complete before returning

#==============================================================================
# HOOK UTILITIES
#==============================================================================

# hook_header(hook_name, tier)
# Prints standardized hook header
# Args:
#   hook_name: "pre-commit" | "pre-push"
#   tier: "fast" | "full"
# Returns: void

# hook_footer(exit_code)
# Prints standardized hook footer with timing
# Args:
#   exit_code: Overall hook result
# Returns: void

# get_staged_files(extension)
# Returns list of staged files with given extension
# Args:
#   extension: File extension filter (e.g., "rs", "toml")
# Returns: Newline-separated list of staged file paths

# is_cargo_workspace_modified()
# Checks if any Cargo.toml or Cargo.lock was modified
# Returns: 0 if modified, 1 if not
```

---

## 2. Pre-Commit Hook: `scripts/hooks/pre-commit.sh`

### Purpose
Fast-tier validation for immediate developer feedback. Must complete in under 5 seconds for optimal developer experience.

### Gate Execution Order

```
TIER: FAST (<5 seconds total)

Gate 1: Cargo Check        [CRITICAL]  timeout=3s  andon=RED
  - Command: cargo make check
  - Purpose: Catch compilation errors immediately
  - On Fail: Block commit, display errors, emit RED Andon

Gate 2: Format Check       [HIGH]      timeout=2s  andon=YELLOW
  - Command: cargo fmt --all -- --check
  - Purpose: Ensure consistent formatting
  - On Fail: Auto-fix with cargo fmt, warn developer
  - Auto-fix: Run "cargo fmt --all" automatically
```

### Andon Signal Rules

| Gate | Pass | Fail | Auto-Fix |
|------|------|------|----------|
| Cargo Check | GREEN | RED - Block commit | No |
| Format Check | GREEN | YELLOW - Auto-fix then warn | Yes |

### Error Handling Strategy

1. **Compilation Errors (Gate 1)**:
   - Capture full error output to temp file
   - Display first 50 lines of errors
   - Emit RED Andon signal
   - Block commit with exit code 1
   - Suggest: "Run 'cargo make check' to see full errors"

2. **Format Issues (Gate 2)**:
   - Run auto-fix: `cargo fmt --all`
   - If auto-fix succeeds: YELLOW warning, allow commit
   - If auto-fix fails: RED signal, block commit
   - Inform developer files were modified

### Script Structure

```bash
#!/usr/bin/env bash
# Pre-Commit Hook - Fast Tier Validation
# Target: <5 seconds
# Gates: Cargo Check (3s), Format Check (2s)

set -e
cd "$(git rev-parse --show-toplevel)"

# Source unified library
source "$(dirname "$0")/../lib/hooks-common.sh"

#------------------------------------------------------------------------------
# CONFIGURATION
#------------------------------------------------------------------------------
readonly HOOK_NAME="pre-commit"
readonly HOOK_TIER="fast"
readonly TOTAL_TIMEOUT=5

#------------------------------------------------------------------------------
# GATES
#------------------------------------------------------------------------------

# gate_cargo_check()
# Runs cargo make check with 3s timeout
# Returns: 0 on success, 1 on failure

# gate_format_check()
# Checks formatting, auto-fixes if needed
# Returns: 0 on success (including auto-fix), 1 on failure
# Side effects: May modify staged files via cargo fmt

#------------------------------------------------------------------------------
# MAIN
#------------------------------------------------------------------------------

# main()
# Orchestrates pre-commit validation
# Flow:
#   1. Print header
#   2. Verify timeout command
#   3. Run Gate 1: Cargo Check
#   4. Run Gate 2: Format Check (with auto-fix)
#   5. Print Andon summary
#   6. Exit with appropriate code
```

---

## 3. Pre-Push Hook: `scripts/hooks/pre-push.sh`

### Purpose
Comprehensive validation before code reaches remote repository. Ensures CI will pass with high confidence.

### Gate Execution Order

```
TIER: FULL (<60 seconds target)

Phase 1: Parallel Safety Checks (timeout=10s each)
  Gate 1: Cargo Check      [CRITICAL]  timeout=5s   andon=RED
  Gate 2: Clippy Lint      [HIGH]      timeout=10s  andon=RED
  Gate 3: Format Check     [HIGH]      timeout=5s   andon=RED
  (Run in parallel where possible: Clippy + Format after Check passes)

Phase 2: Test Validation (timeout=30s)
  Gate 4: Unit Tests       [CRITICAL]  timeout=30s  andon=RED

Phase 3: Security (timeout=10s)
  Gate 5: Security Audit   [MEDIUM]    timeout=10s  andon=YELLOW
```

### Timeout Values

| Gate | Timeout | Rationale |
|------|---------|-----------|
| Cargo Check | 5s | Quick compilation verification |
| Clippy | 10s | Lint analysis over full workspace |
| Format Check | 5s | Fast formatting verification |
| Unit Tests | 30s | Full test suite execution |
| Security Audit | 10s | Dependency vulnerability scan |

### Andon Signal Rules

| Gate | Pass | Fail | Action on Fail |
|------|------|------|----------------|
| Cargo Check | GREEN | RED | Block push immediately |
| Clippy | GREEN | RED | Block push, show warnings |
| Format Check | GREEN | RED | Block push, suggest cargo fmt |
| Unit Tests | GREEN | RED | Block push, show failures |
| Security Audit | GREEN | YELLOW | Warn only, allow push |

### Parallel Execution Strategy

```
Sequential Flow:
  [Gate 1: Cargo Check] --pass--> [Gate 2 + Gate 3 in parallel]
                        \
                         --fail--> STOP (no parallel gates)

  [Gates 2,3 complete] --all pass--> [Gate 4: Unit Tests]
                       \
                        --any fail--> STOP

  [Gate 4 complete] --pass--> [Gate 5: Security Audit]
                    \
                     --fail--> STOP

  [Gate 5 complete] --pass--> Allow push (GREEN)
                    \
                     --fail--> Warn, allow push (YELLOW)
```

### Error Handling Strategy

1. **Compilation Errors (Gate 1)**:
   - Block immediately, no further gates
   - Display compilation errors
   - RED Andon: "Fix compilation before pushing"

2. **Linting Errors (Gate 2)**:
   - Capture clippy warnings/errors
   - Block push on errors
   - RED Andon with clippy output
   - Suggest: "Run 'cargo make lint' for details"

3. **Format Issues (Gate 3)**:
   - Block push (unlike pre-commit, no auto-fix)
   - RED Andon: "Run 'cargo fmt --all' before pushing"

4. **Test Failures (Gate 4)**:
   - Capture failing test names and output
   - Display first 100 lines of test output
   - RED Andon: "Fix failing tests before pushing"
   - List failed test names for quick reference

5. **Security Vulnerabilities (Gate 5)**:
   - YELLOW warning only (non-blocking)
   - Suggest: "Review 'cargo audit' output"
   - Log vulnerability count

### Script Structure

```bash
#!/usr/bin/env bash
# Pre-Push Hook - Full Tier Validation
# Target: <60 seconds
# Gates: Check, Clippy, Format, Tests, Audit

set -e
cd "$(git rev-parse --show-toplevel)"

# Source unified library
source "$(dirname "$0")/../lib/hooks-common.sh"

#------------------------------------------------------------------------------
# CONFIGURATION
#------------------------------------------------------------------------------
readonly HOOK_NAME="pre-push"
readonly HOOK_TIER="full"
readonly TOTAL_TIMEOUT=60

# Gate configurations
readonly GATE_CHECK_TIMEOUT=5
readonly GATE_CLIPPY_TIMEOUT=10
readonly GATE_FORMAT_TIMEOUT=5
readonly GATE_TEST_TIMEOUT=30
readonly GATE_AUDIT_TIMEOUT=10

#------------------------------------------------------------------------------
# GATES
#------------------------------------------------------------------------------

# gate_cargo_check()
# Returns: 0 on success, 1 on failure

# gate_clippy()
# Returns: 0 on success, 1 on failure

# gate_format_check()
# Returns: 0 on success, 1 on failure
# Note: NO auto-fix in pre-push (unlike pre-commit)

# gate_unit_tests()
# Returns: 0 on success, 1 on failure
# Side effects: Captures test output for failure analysis

# gate_security_audit()
# Returns: 0 on success, 1 on vulnerability found
# Note: Non-blocking (YELLOW Andon only)

#------------------------------------------------------------------------------
# PARALLEL EXECUTION
#------------------------------------------------------------------------------

# run_parallel_gates()
# Executes clippy and format check in parallel
# Returns: 0 if both pass, 1 if either fails

#------------------------------------------------------------------------------
# MAIN
#------------------------------------------------------------------------------

# main()
# Orchestrates pre-push validation
# Flow:
#   1. Print header
#   2. Verify timeout command
#   3. Phase 1: Gate 1 (sequential prerequisite)
#   4. Phase 1: Gates 2,3 (parallel if Gate 1 passes)
#   5. Phase 2: Gate 4 (unit tests)
#   6. Phase 3: Gate 5 (security audit - non-blocking)
#   7. Print Andon summary
#   8. Exit with appropriate code
```

---

## 4. Installation Script: `scripts/install-hooks.sh`

### Purpose
Automated installation of git hooks with validation, backup, and self-test.

### Script Structure

```bash
#!/usr/bin/env bash
# Git Hooks Installation Script
# Installs pre-commit and pre-push hooks via symlinks

set -e

#------------------------------------------------------------------------------
# CONFIGURATION
#------------------------------------------------------------------------------
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly GIT_DIR="$(git rev-parse --git-dir 2>/dev/null)"
readonly HOOKS_DIR="$GIT_DIR/hooks"
readonly LIB_DIR="$SCRIPT_DIR/lib"
readonly HOOKS_SRC_DIR="$SCRIPT_DIR/hooks"

#------------------------------------------------------------------------------
# FUNCTIONS
#------------------------------------------------------------------------------

# verify_prerequisites()
# Checks: git repo, timeout command, hooks directory
# Returns: 0 on success, 1 on failure

# backup_existing_hooks()
# Backs up existing hooks to *.bak
# Returns: 0 (always succeeds, creates backups if needed)

# install_hook(hook_name)
# Creates symlink for specified hook
# Args:
#   hook_name: "pre-commit" | "pre-push"
# Returns: 0 on success, 1 on failure

# verify_installation()
# Confirms hooks are properly installed and executable
# Returns: 0 on success, 1 on failure

# run_self_test()
# Executes basic hook functionality test
# Returns: 0 on success, 1 on failure

# generate_config()
# Creates .hooks.conf with configurable options
# Returns: 0 (creates config file if not exists)

# print_usage()
# Displays help and usage information

#------------------------------------------------------------------------------
# MAIN
#------------------------------------------------------------------------------

# main(args...)
# Orchestrates hook installation
# Flow:
#   1. Parse arguments (--help, --uninstall, --self-test)
#   2. Verify prerequisites (git, timeout, paths)
#   3. Backup existing hooks
#   4. Install pre-commit hook (symlink)
#   5. Install pre-push hook (symlink)
#   6. Generate hook configuration
#   7. Verify installation
#   8. Run self-test
#   9. Print success summary
```

### Installation Flow

```
[Start]
   |
   v
[Parse Arguments] --help--> [Print Usage] --> [Exit]
   |
   |--uninstall--> [Remove Symlinks] --> [Exit]
   |
   v
[Verify Prerequisites]
   |
   |--fail--> [Print Error] --> [Exit 1]
   |
   v
[Backup Existing Hooks]
   |
   v
[Install pre-commit symlink]
   |
   |--fail--> [Print Error] --> [Exit 1]
   |
   v
[Install pre-push symlink]
   |
   |--fail--> [Print Error] --> [Exit 1]
   |
   v
[Generate .hooks.conf]
   |
   v
[Verify Installation]
   |
   |--fail--> [Print Warning] --> [Continue]
   |
   v
[Run Self-Test]
   |
   |--fail--> [Print Warning] --> [Continue]
   |
   v
[Print Success Summary]
   |
   v
[Exit 0]
```

### Configuration File: `.hooks.conf`

```bash
# Git Hooks Configuration
# Generated by install-hooks.sh

# Pre-commit settings
PRE_COMMIT_ENABLED=true
PRE_COMMIT_AUTO_FIX=true
PRE_COMMIT_TIMEOUT=5

# Pre-push settings
PRE_PUSH_ENABLED=true
PRE_PUSH_PARALLEL=true
PRE_PUSH_TIMEOUT=60

# Security audit settings
AUDIT_BLOCKING=false
AUDIT_TIMEOUT=10

# Debug settings
HOOKS_DEBUG=false
ANDON_LOG_FILE=target/andon_hooks.log
```

---

## Summary: Gate Execution Matrix

| Hook | Gate | Command | Timeout | Andon | Blocking |
|------|------|---------|---------|-------|----------|
| pre-commit | Cargo Check | cargo make check | 3s | RED | Yes |
| pre-commit | Format | cargo fmt --check | 2s | YELLOW | No (auto-fix) |
| pre-push | Cargo Check | cargo make check | 5s | RED | Yes |
| pre-push | Clippy | cargo make lint | 10s | RED | Yes |
| pre-push | Format | cargo fmt --check | 5s | RED | Yes |
| pre-push | Unit Tests | cargo make test-unit | 30s | RED | Yes |
| pre-push | Audit | cargo make audit | 10s | YELLOW | No |

---

## Implementation Priority

1. **Phase 1**: `hooks-common.sh` library (foundation)
2. **Phase 2**: `pre-commit.sh` hook (fast feedback)
3. **Phase 3**: `pre-push.sh` hook (comprehensive validation)
4. **Phase 4**: `install-hooks.sh` (deployment automation)
5. **Phase 5**: Integration testing and documentation

---

## File Paths (Absolute)

- Library: `/Users/sac/ggen/scripts/lib/hooks-common.sh`
- Pre-commit: `/Users/sac/ggen/scripts/hooks/pre-commit.sh`
- Pre-push: `/Users/sac/ggen/scripts/hooks/pre-push.sh`
- Installer: `/Users/sac/ggen/scripts/install-hooks.sh` (update existing)
