# Claude Code Hooks System

Modern bash hooks for ggen workspace safety and validation.

## Overview

This hooks system provides safety checks, validation, and Andon signal monitoring for Claude Code operations in the ggen Rust workspace.

**Design Principles:**
- Timeout wrappers (3s default)
- Minimal dependencies (bash + coreutils)
- Safety-first approach
- Non-blocking validation
- Clear signal reporting

## Available Hooks

### 🛡️ `pre-tool-safety.sh`

**Purpose**: Prevent destructive operations before execution

**Blocks:**
- Direct cargo commands (requires `cargo make`)
- Force pushes to main/master
- Dangerous `rm -rf` patterns
- Git operations skipping hooks
- Saving files to root folder
- Destructive git reset without confirmation

**Usage:**
```bash
./pre-tool-safety.sh "cargo test"  # ❌ Blocked
./pre-tool-safety.sh "cargo make test"  # ✅ Allowed
```

**Exit Codes:**
- `0` - Command is safe
- `1` - Command blocked (dangerous)

---

### ✅ `post-bash-validation.sh`

**Purpose**: Verify commands succeeded and detect Andon signals

**Detects:**
- 🔴 CRITICAL: Compiler errors (`error[E...]`)
- 🔴 CRITICAL: Test failures (`test ... FAILED`)
- 🟡 HIGH: Warnings (`warning:`)
- 🟡 HIGH: Clippy issues (`clippy::`)

**Usage:**
```bash
OUTPUT=$(cargo make test 2>&1)
EXIT_CODE=$?
./post-bash-validation.sh "cargo make test" "$EXIT_CODE" "$OUTPUT"
```

**Exit Codes:**
- `0` - Always (non-blocking, reports issues to stderr)

---

### 🚀 `session-start.sh`

**Purpose**: Initialize ggen environment and validate workspace

**Validates:**
- Workspace structure (`crates/`, `.specify/`, etc.)
- cargo and cargo make availability
- timeout command presence
- Git status and branch info
- Initial compilation state

**Usage:**
```bash
./session-start.sh
```

**Output:**
```
🚀 Initializing ggen session...
ℹ️  Git: 3 uncommitted changes
ℹ️  Git: On branch main

📋 Key Commands:
   cargo make check       - Quick compilation check (<5s)
   cargo make test-unit   - Fast unit tests (<16s)
   ...

✅ Session initialized successfully
```

**Exit Codes:**
- `0` - Session initialized successfully
- `1` - Critical workspace issue

---

### 📝 `user-prompt.sh`

**Purpose**: Validate user inputs and sanitize prompts

**Checks For:**
- Empty prompts
- Relative paths (should be absolute)
- Destructive operations
- Requests to skip safety checks
- Direct cargo commands
- Saving to root folder
- TodoWrite batch size reminders
- Spec/markdown confusion (TTL vs MD)
- Completion claims without validation
- unwrap/expect in production

**Usage:**
```bash
./user-prompt.sh "Create a new feature in ./src/lib.rs"
# ⚠️  WARNING: Relative path detected in prompt
# → ggen requires absolute paths: /home/user/ggen/...
```

**Exit Codes:**
- `0` - Always (non-blocking, reports warnings to stderr)

---

## Integration with Claude Code

These hooks integrate with Claude Code's tool execution flow:

```
User Prompt
    ↓
user-prompt.sh (validate input)
    ↓
Claude Code Tool Selection
    ↓
pre-tool-safety.sh (before execution)
    ↓
Tool Execution (Bash, Edit, Write, etc.)
    ↓
post-bash-validation.sh (after execution)
    ↓
Andon Signal Detection
    ↓
Result to User
```

## Andon Signals

**STOP THE LINE when signals appear!**

| Signal | Severity | Action |
|--------|----------|--------|
| 🔴 CRITICAL | Compiler errors, test failures | HALT immediately |
| 🟡 HIGH | Warnings, clippy issues | STOP before release |
| 🟢 GREEN | All checks pass | Proceed |

**Response Protocol:**
1. **STOP** - Cease all work
2. **INVESTIGATE** - 5 Whys root cause analysis
3. **FIX** - Correct the root cause
4. **VERIFY** - Run checks to confirm signal cleared
5. **PROCEED** - Continue only when GREEN

## Examples

### Scenario 1: Blocking Dangerous Command

```bash
$ ./pre-tool-safety.sh "rm -rf ."
❌ BLOCKED: Dangerous rm -rf pattern detected
$ echo $?
1
```

### Scenario 2: Detecting Test Failures

```bash
$ OUTPUT=$(cargo make test 2>&1)
$ ./post-bash-validation.sh "cargo make test" "$?" "$OUTPUT"
🔴 CRITICAL ANDON SIGNAL: Test failure detected!
   → STOP THE LINE - Fix immediately
   → Run: cargo make test
```

### Scenario 3: Session Initialization

```bash
$ ./session-start.sh
🚀 Initializing ggen session...
ℹ️  Git: 0 uncommitted changes
ℹ️  Git: On branch feature/new-api

📋 Key Commands:
   ...

✅ Initial workspace check: CLEAN

✅ Session initialized successfully
```

### Scenario 4: Validating User Input

```bash
$ ./user-prompt.sh "Skip the tests and just build"
❌ WARNING: Request to skip safety checks detected
   → All checks are required per CLAUDE.md
   → cargo make enforces quality gates
```

## Timeout Configuration

All hooks use 3s timeout by default. Override if needed:

```bash
# Increase timeout for specific hook
timeout 5s ./session-start.sh

# Disable timeout (not recommended)
bash session-start.sh
```

## Customization

### Adding New Safety Checks

Edit `pre-tool-safety.sh`:

```bash
# Add new pattern check
if echo "$COMMAND" | grep -qE "dangerous-pattern"; then
  echo "❌ BLOCKED: Explanation" >&2
  exit 1
fi
```

### Adding New Andon Signals

Edit `post-bash-validation.sh`:

```bash
# Add new signal detection
if echo "$OUTPUT" | grep -qE "critical-pattern"; then
  echo "🔴 CRITICAL ANDON SIGNAL: Description!" >&2
  echo "   → STOP THE LINE - Action required" >&2
  exit 0
fi
```

## Maintenance

**Update frequency**: Monthly or when new patterns identified

**Test hooks:**
```bash
cd /home/user/ggen/.claude/hooks

# Test safety checks
./pre-tool-safety.sh "cargo test"              # Should block
./pre-tool-safety.sh "cargo make test"         # Should allow
./pre-tool-safety.sh "rm -rf ."                # Should block

# Test validation
echo "error[E0425]" | ./post-bash-validation.sh "test" "1" "$(cat)"

# Test initialization
./session-start.sh

# Test prompt validation
./user-prompt.sh "Create file in ./src"        # Should warn
./user-prompt.sh "Skip tests"                  # Should warn
```

## Dependencies

**Required:**
- bash 4.0+
- coreutils (timeout, grep, wc)

**Optional:**
- git (for git status checks)
- cargo (for workspace validation)

## Troubleshooting

**Hook fails with "timeout: command not found":**
```bash
# Linux
sudo apt-get install coreutils

# macOS
brew install coreutils
```

**Hook reports false positive:**
- Check pattern matching in hook script
- Add exception case if legitimate
- Submit PR to improve detection

**Hook doesn't execute:**
```bash
# Make executable
chmod +x /home/user/ggen/.claude/hooks/*.sh

# Verify shebang
head -1 /home/user/ggen/.claude/hooks/pre-tool-safety.sh
# Should output: #!/bin/bash
```

## Performance

**Benchmarks** (on typical workstation):
- `pre-tool-safety.sh`: <50ms
- `post-bash-validation.sh`: <100ms
- `session-start.sh`: <500ms
- `user-prompt.sh`: <50ms

**Total overhead**: <1s per operation cycle

## Security

**Considerations:**
- Hooks run with user permissions
- No network access required
- No sensitive data in logs
- Exit on untrusted input patterns

**Best Practices:**
- Review hook changes before deployment
- Validate hook scripts with ShellCheck
- Use version control for hooks
- Test hooks in isolation before integration

## Version History

**v1.0.0** (2026-02-08)
- Initial release
- 4 core hooks + README
- Andon signal detection
- ggen-specific safety checks

## Support

**Issues**: Report via ggen GitHub issues
**Updates**: Check `CLAUDE.md` for latest patterns
**Docs**: https://github.com/seanchatmangpt/ggen

---

**Remember**: These hooks enforce the ggen safety culture. Don't disable or bypass them. STOP THE LINE when Andon signals appear!

**Last Updated**: 2026-02-08 | v1.0.0
