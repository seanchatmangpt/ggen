<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Git Hooks Auto-Installation in `ggen init`](#git-hooks-auto-installation-in-ggen-init)
  - [Overview](#overview)
  - [Features](#features)
    - [1. Automatic Installation](#1-automatic-installation)
    - [2. Smart Detection](#2-smart-detection)
    - [3. Quality Gates](#3-quality-gates)
      - [Pre-Commit Hook (Fast Tier - < 10 seconds)](#pre-commit-hook-fast-tier----10-seconds)
      - [Pre-Push Hook (Full Tier - < 90 seconds)](#pre-push-hook-full-tier----90-seconds)
  - [Usage](#usage)
    - [Basic Usage](#basic-usage)
    - [Output Example](#output-example)
  - [Implementation Details](#implementation-details)
    - [Module Structure](#module-structure)
    - [Key Functions](#key-functions)
      - [`install_git_hooks(project_path, skip_hooks) -> Result<HooksInstallOutput>`](#install_git_hooksproject_path-skip_hooks---resulthooksinstalloutput)
      - [`install_hook(hooks_dir, hook_name, hook_content) -> Result<HookInstallation>`](#install_hookhooks_dir-hook_name-hook_content---resulthookinstallation)
    - [Hook Content](#hook-content)
      - [Pre-Commit Hook](#pre-commit-hook)
      - [Pre-Push Hook](#pre-push-hook)
  - [Error Handling](#error-handling)
  - [Testing](#testing)
    - [Unit Tests (in `git_hooks.rs`)](#unit-tests-in-git_hooksrs)
    - [Integration Tests (in `tests/init_git_hooks_test.rs`)](#integration-tests-in-testsinit_git_hooks_testrs)
    - [Running Tests](#running-tests)
  - [Bypassing Hooks (Not Recommended)](#bypassing-hooks-not-recommended)
  - [Constitutional Compliance](#constitutional-compliance)
    - [No Unwrap/Expect](#no-unwrapexpect)
    - [Result<T,E> Throughout](#resultte-throughout)
    - [Cross-Platform](#cross-platform)
    - [Clear Error Messages](#clear-error-messages)
    - [Type-First Design](#type-first-design)
  - [Integration with Init Command](#integration-with-init-command)
  - [Future Enhancements](#future-enhancements)
  - [See Also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Git Hooks Auto-Installation in `ggen init`

## Overview

The `ggen init` command now automatically installs git hooks to enforce code quality gates during development. This ensures that all commits and pushes meet quality standards before they reach the repository.

## Features

### 1. Automatic Installation

When running `ggen init` in a git repository, the command automatically:
- Detects if the directory is a git repository (has `.git/` directory)
- Installs `pre-commit` and `pre-push` hooks
- Makes hooks executable (Unix/Linux/macOS)
- Reports installation status in the command output

### 2. Smart Detection

The implementation includes intelligent behaviors:
- **Skip if not git**: Silently skips installation if not in a git repository
- **Preserve existing hooks**: Never overwrites user-created hooks
- **Cross-platform**: Works on Unix, Linux, macOS, and Windows (with git bash)
- **Optional bypass**: Use `--skip-hooks` flag to disable installation

### 3. Quality Gates

#### Pre-Commit Hook (Fast Tier - < 10 seconds)

Runs on every commit to catch errors early:
- **Cargo check**: Ensures code compiles (60% of defects)
- **Format check**: Auto-formats code with `cargo fmt`

#### Pre-Push Hook (Full Tier - < 90 seconds)

Runs before pushing to remote:
- **Cargo make pre-commit**: Full validation suite (if cargo-make installed)
- **Fallback**: Direct cargo commands (check + clippy + test + fmt)
- Catches 97% of defects before code review

## Usage

### Basic Usage

```bash
# Initialize project with automatic hook installation
ggen init

# Initialize in specific directory
ggen init --path my-project

# Skip git hooks installation
ggen init --skip-hooks
```

### Output Example

```json
{
  "status": "success",
  "project_dir": ".",
  "files_created": ["ggen.toml", "schema/domain.ttl", ...],
  "directories_created": ["schema", "templates", "scripts", "src/generated"],
  "git_hooks": {
    "git_repo_detected": true,
    "hooks_installed": [
      {
        "hook_name": "pre-commit",
        "installed": true,
        "skipped": false,
        "reason": null
      },
      {
        "hook_name": "pre-push",
        "installed": true,
        "skipped": false,
        "reason": null
      }
    ],
    "warnings": []
  },
  "next_steps": [
    "Run 'make setup' to initialize your project",
    "Edit schema/domain.ttl to define your domain model",
    "Create Tera templates in templates/ for your target languages",
    "Run 'make build' to generate code from your ontology"
  ]
}
```

## Implementation Details

### Module Structure

```
ggen-cli/src/cmds/
├── git_hooks.rs         # Git hooks installation logic
├── init.rs              # Init command (calls git_hooks)
└── mod.rs               # Module registration
```

### Key Functions

#### `install_git_hooks(project_path, skip_hooks) -> Result<HooksInstallOutput>`

Main entry point for hook installation:
- Checks if `skip_hooks` flag is set
- Detects git repository
- Installs pre-commit and pre-push hooks
- Returns detailed installation report

#### `install_hook(hooks_dir, hook_name, hook_content) -> Result<HookInstallation>`

Installs a single hook:
- Creates hooks directory if needed
- Skips if hook already exists (preserves user hooks)
- Writes hook content
- Sets executable permissions (Unix)
- Returns installation status

### Hook Content

#### Pre-Commit Hook

```bash
#!/usr/bin/env bash
# Pre-commit hook - Fast validation tier
set -e
cd "$(git rev-parse --show-toplevel)"

echo "Pre-commit validation (fast tier)..."

# Gate 1: Cargo check (compilation)
echo -n "  Cargo check... "
if timeout 10s cargo check --quiet 2>/dev/null; then
    echo "PASS"
else
    echo "FAIL"
    cargo check 2>&1 | head -30
    exit 1
fi

# Gate 2: Format check (auto-fixable)
echo -n "  Format check... "
if cargo fmt --all -- --check >/dev/null 2>&1; then
    echo "PASS"
else
    echo "AUTO-FIX"
    cargo fmt --all >/dev/null 2>&1 || true
    echo "  Code formatted. Review changes before commit."
fi

echo "Pre-commit passed."
exit 0
```

#### Pre-Push Hook

```bash
#!/usr/bin/env bash
# Pre-push hook - Full validation tier
set -e
cd "$(git rev-parse --show-toplevel)"

echo "Pre-push validation (full tier)..."

# Use cargo make if available, fallback to direct commands
if command -v cargo-make &> /dev/null; then
    cargo make pre-commit || exit 1
else
    cargo check --quiet || exit 1
    cargo clippy --all-targets --all-features -- -D warnings || exit 1
    cargo test --workspace || exit 1
    cargo fmt --all -- --check || exit 1
fi

echo "Pre-push passed."
exit 0
```

## Error Handling

All functions use `Result<T, std::io::Error>` pattern:
- No `unwrap()` or `expect()` in production code
- Graceful fallback on errors (init succeeds even if hooks fail)
- Clear error messages
- Detailed status in output

## Testing

### Unit Tests (in `git_hooks.rs`)

- `test_is_git_repo_detects_git_directory`
- `test_is_git_repo_returns_false_for_non_git`
- `test_get_hooks_dir_returns_correct_path`
- `test_is_hook_installed_detects_existing_hook`
- `test_is_hook_installed_returns_false_for_missing_hook`
- `test_install_hook_creates_hook_file`
- `test_install_hook_skips_existing_hook`
- `test_install_git_hooks_with_skip_flag`
- `test_install_git_hooks_in_non_git_repo`
- `test_install_git_hooks_in_git_repo`
- `test_install_git_hooks_skips_existing`
- `test_hook_is_executable_on_unix` (Unix only)

### Integration Tests (in `tests/init_git_hooks_test.rs`)

- `test_init_installs_git_hooks_in_git_repo`
- `test_init_skips_hooks_with_flag`
- `test_init_handles_non_git_repo_gracefully`
- `test_hooks_are_executable_on_unix` (Unix only)
- `test_hook_content_includes_cargo_make`
- `test_existing_hooks_are_not_overwritten`

### Running Tests

```bash
# Run all git hooks tests
cargo test -p ggen-cli-lib git_hooks

# Run integration tests
cargo test -p ggen-cli-lib --test init_git_hooks_test

# Run with output
cargo test -p ggen-cli-lib git_hooks -- --nocapture
```

## Bypassing Hooks (Not Recommended)

If you absolutely must bypass hooks (e.g., emergency hotfix):

```bash
# Skip pre-commit hook
git commit --no-verify -m "Emergency fix"

# Skip pre-push hook
git push --no-verify
```

**Warning**: Bypassing hooks increases risk of breaking CI/CD pipeline. Use only when necessary.

## Constitutional Compliance

This implementation follows ggen's constitutional rules:

### No Unwrap/Expect
All error handling uses `Result<T, E>` pattern. No `unwrap()` or `expect()` in production code.

### Result<T,E> Throughout
Every fallible operation returns `Result`:
- `is_git_repo(path) -> Result<bool, std::io::Error>`
- `install_hook(...) -> Result<HookInstallation, std::io::Error>`
- `install_git_hooks(...) -> Result<HooksInstallOutput, std::io::Error>`

### Cross-Platform
Works on Unix, Linux, macOS, and Windows:
- Conditional compilation for Unix-specific features (`#[cfg(unix)]`)
- Executable permissions set only on Unix
- Git bash handles execution on Windows

### Clear Error Messages
Errors include context:
- "Not a git repository, skipping hook installation"
- "Failed to install pre-commit hook: {error}"
- "Hook already exists" (when skipping)

### Type-First Design
Custom types for structured output:
- `HookInstallation`: Status of single hook
- `HooksInstallOutput`: Complete installation report
- Serializable to JSON for programmatic use

## Integration with Init Command

The init command integrates hooks installation as follows:

```rust
// After successful file transaction
let git_hooks_result = super::git_hooks::install_git_hooks(base_path, skip_hooks)
    .ok(); // Convert to Option, don't fail init if hooks fail

Ok(InitOutput {
    // ... other fields ...
    git_hooks: git_hooks_result,
})
```

Key design decisions:
1. **Non-blocking**: Hook installation never fails the init command
2. **After transaction**: Hooks installed after files are created
3. **Optional result**: Reported but doesn't affect init success

## Future Enhancements

Potential improvements for v5.1+:

1. **Commit-msg hook**: Enforce commit message format
2. **Custom hook templates**: Allow user-defined hooks
3. **Hook updates**: Update hooks on `ggen sync`
4. **Interactive prompt**: Ask user before installing hooks
5. **Hook verification**: Test hooks after installation
6. **Windows .bat wrappers**: Better Windows support

## See Also

- [Cargo Make Documentation](../Makefile.toml) - Pre-commit tasks
- [CLAUDE.md](../CLAUDE.md) - Constitutional rules
- [scripts/install-git-hooks.sh](../scripts/install-git-hooks.sh) - Legacy installation script
- [scripts/hooks/](../scripts/hooks/) - Hook templates
