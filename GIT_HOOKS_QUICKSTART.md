# Git Hooks Auto-Installation - Quick Start Guide

## What Are Git Hooks?

Git hooks are scripts that run automatically when you perform git operations (like `git commit` or `git push`). They help catch errors before they reach your repository.

## Automatic Installation

`ggen init` automatically installs two git hooks for you:

### 1. Pre-commit Hook
**Runs before:** Every git commit
**Target time:** <10 seconds
**What it does:**
- ✅ Checks if your code compiles (`cargo check`)
- ✅ Automatically formats your code (`cargo fmt`)

### 2. Pre-push Hook
**Runs before:** Every git push
**Target time:** <90 seconds
**What it does:**
- ✅ Full compilation check
- ✅ Runs clippy linter
- ✅ Runs all tests
- ✅ Verifies formatting

## Usage

### Default Behavior (Recommended)
```bash
# Initialize project in git repo
git init
ggen init

# Hooks are automatically installed!
# Location: .git/hooks/pre-commit and .git/hooks/pre-push
```

### Skip Hooks Installation
```bash
# If you want to skip hook installation
ggen init --skip_hooks true
```

### In Non-Git Directory
```bash
# No problem! ggen init works fine without git
mkdir my-project
cd my-project
ggen init

# Hooks won't be installed (no git repo), but init succeeds
```

## What Happens During Commits?

### Example 1: Successful Commit
```bash
$ git commit -m "Add feature"

Pre-commit validation (fast tier)...
  Cargo check... PASS
  Format check... PASS
Pre-commit passed.

[main abc123] Add feature
 1 file changed, 10 insertions(+)
```

### Example 2: Compilation Error
```bash
$ git commit -m "Broken code"

Pre-commit validation (fast tier)...
  Cargo check... FAIL

STOP: Compilation errors must be fixed
error[E0425]: cannot find value `x` in this scope
 --> src/main.rs:5:5
  |
5 |     x
  |     ^ not found in this scope

# Commit is blocked until you fix the error!
```

### Example 3: Auto-formatting
```bash
$ git commit -m "Unformatted code"

Pre-commit validation (fast tier)...
  Cargo check... PASS
  Format check... AUTO-FIX
  Code formatted. Review changes before commit.
Pre-commit passed.

# Your code has been auto-formatted!
# Review the changes with: git diff
```

## What Happens During Pushes?

```bash
$ git push

Pre-push validation (full tier)...
Running cargo make pre-commit...

  Cargo check... PASS
  Cargo clippy... PASS
  Cargo test... PASS (347/347 tests)
  Format check... PASS

Pre-push passed.

# Push proceeds to remote
```

## Existing Hooks

**Your custom hooks are safe!** If you already have a pre-commit or pre-push hook, `ggen init` will **NOT** overwrite it.

```bash
# You already have .git/hooks/pre-commit
$ cat .git/hooks/pre-commit
#!/bin/bash
# My custom hook
echo "Running my tests..."

$ ggen init

# Your hook is preserved!
$ cat .git/hooks/pre-commit
#!/bin/bash
# My custom hook
echo "Running my tests..."
```

## Bypassing Hooks (Not Recommended)

Sometimes you need to bypass hooks (e.g., WIP commits):

```bash
# Skip pre-commit hook for this commit only
git commit --no-verify -m "WIP: work in progress"

# Skip pre-push hook for this push only
git push --no-verify
```

**Warning:** Only use `--no-verify` when absolutely necessary. Hooks exist to protect code quality.

## Customizing Hooks

### Location
```
.git/hooks/
├── pre-commit    # Fast validation
└── pre-push      # Full validation
```

### Editing Hooks
You can edit these files directly:

```bash
# Edit pre-commit hook
nano .git/hooks/pre-commit

# Make sure it's executable
chmod +x .git/hooks/pre-commit
```

### Hook Requirements
Hooks must:
1. Start with shebang: `#!/usr/bin/env bash`
2. Be executable: `chmod +x`
3. Exit with code 0 to allow operation, non-zero to block

## Troubleshooting

### Hook Doesn't Run
```bash
# Check if hook exists
ls -l .git/hooks/pre-commit

# Check if executable (Unix)
# Should show -rwxr-xr-x (x = executable)
ls -l .git/hooks/pre-commit

# Make executable
chmod +x .git/hooks/pre-commit
```

### Hook Fails on Cargo Commands
```bash
# Make sure cargo is installed
cargo --version

# Make sure you're in a Rust project
ls Cargo.toml

# Install cargo-make for better experience
cargo install cargo-make
```

### Reinitialize Hooks
```bash
# Remove existing hooks
rm .git/hooks/pre-commit .git/hooks/pre-push

# Run ggen init again
ggen init

# Hooks are reinstalled
```

### Skip Hooks for CI/CD
If you have custom CI/CD that manages hooks:

```bash
ggen init --skip_hooks true
```

## Platform Support

### Linux / macOS
✅ Fully supported
- Executable permissions set automatically
- Portable shebang used

### Windows
✅ Supported via Git for Windows
- Git Bash executes hook scripts
- No chmod needed

## Performance Targets

| Hook | Target Time | What It Checks |
|------|-------------|----------------|
| pre-commit | <10s | Compilation + formatting |
| pre-push | <90s | Full validation (clippy + tests) |

## Best Practices

1. ✅ **Let hooks auto-install** - Default behavior is best for most projects
2. ✅ **Don't bypass hooks regularly** - Fix issues instead of skipping
3. ✅ **Keep cargo-make updated** - Hooks use cargo make targets
4. ✅ **Review auto-formatted code** - Hooks may modify your files
5. ✅ **Use --skip_hooks only for CI/CD** - If you manage hooks externally

## FAQ

**Q: Will hooks slow down my commits?**
A: Pre-commit is fast (<10s). Pre-push is thorough (<90s) but only runs on push.

**Q: Can I customize what hooks check?**
A: Yes! Edit `.git/hooks/pre-commit` and `.git/hooks/pre-push` directly.

**Q: What if I don't want hooks?**
A: Use `ggen init --skip_hooks true` or delete `.git/hooks/pre-commit`.

**Q: Do hooks work in subdirectories?**
A: Yes! Hooks run from git repo root regardless of your current directory.

**Q: What if cargo-make is not installed?**
A: pre-push hook falls back to direct cargo commands (check, clippy, test, fmt).

**Q: Can I commit broken code for WIP?**
A: Use `git commit --no-verify` but fix it before pushing.

**Q: Will reinitializing overwrite my custom hooks?**
A: No! Existing hooks are always preserved.

## Examples

### Example 1: New Project with Hooks
```bash
mkdir my-app
cd my-app
git init
ggen init

# Hooks installed automatically
# Start coding with confidence!
```

### Example 2: Existing Project
```bash
cd existing-project
git init  # If not already a git repo
ggen init

# Hooks added to existing project
```

### Example 3: CI/CD Environment
```bash
# In CI/CD pipeline
ggen init --skip_hooks true

# No hooks installed (CI has its own checks)
```

## Support

For issues or questions:
- Check verification receipt: `GIT_HOOKS_VERIFICATION_RECEIPT.md`
- Run verification tests: `bash tests/verify_git_hooks_installation.sh`
- Review implementation: `crates/ggen-cli/src/cmds/git_hooks.rs`

## Summary

Git hooks auto-installation:
- ✅ Automatic during `ggen init`
- ✅ Fast pre-commit (<10s)
- ✅ Thorough pre-push (<90s)
- ✅ Preserves existing hooks
- ✅ Cross-platform compatible
- ✅ Optional (--skip_hooks flag)
- ✅ Safe and tested

**Just run `ggen init` and you're protected!**
