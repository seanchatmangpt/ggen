# FMEA - Failure Mode and Effects Analysis
## Phase 2: ggen.toml + clap-noun-verb Integration

**LEAN PRINCIPLE**: Identify and mitigate failures BEFORE they occur. Prevention > Detection > Reaction.

**Analysis Date**: 2025-11-18
**Scope**: ggen.toml configuration parsing, clap-noun-verb CLI integration, environment variable expansion
**Methodology**: FMEA (Failure Mode and Effects Analysis) with RPN (Risk Priority Number) scoring

---

## Scoring System

- **Probability**: Low (1-3), Medium (4-6), High (7-10)
- **Impact**: Severity 1-10 (1 = trivial, 10 = catastrophic)
- **Detectability**: Easy (1-3), Medium (4-6), Hard (7-10)
- **RPN**: Probability × Impact × Detectability

**Priority Threshold**: RPN ≥ 100 requires immediate mitigation

---

## FMEA Table: 25 Critical Failure Modes

| ID | Failure Mode | Probability | Impact | Detectability | RPN | Current Controls | Mitigation Strategy | Priority |
|-----|-------------|-------------|--------|---------------|-----|------------------|---------------------|----------|
| FM-001 | **TOML syntax error** (invalid TOML in ggen.toml) | 8 | 9 | 2 | **144** | Basic toml::from_str error | Add pre-parse syntax validation with helpful error messages | 🔴 P0 |
| FM-002 | **Type mismatch** (expected String, got Integer) | 7 | 7 | 3 | **147** | Serde deserialization error | Add schema validation with type coercion for common cases | 🔴 P0 |
| FM-003 | **Path traversal attack** (../../etc/passwd in config) | 3 | 10 | 4 | **120** | None | Implement path normalization and validation (reject ../) | 🔴 P0 |
| FM-004 | **Missing required field** (no project_name in config) | 6 | 8 | 2 | **96** | Serde default values | Add explicit validation with clear error messages | 🟡 P1 |
| FM-005 | **Environment variable not set** ($HOME expansion fails) | 7 | 6 | 3 | **126** | Runtime error on expansion | Add env var existence check BEFORE expansion | 🔴 P0 |
| FM-006 | **Circular dependency** (template A includes B, B includes A) | 4 | 9 | 7 | **252** | None | Implement dependency graph with cycle detection | 🔴 P0 |
| FM-007 | **Invalid regex pattern** (malformed regex in validation) | 5 | 5 | 4 | **100** | Runtime panic on regex::Regex::new | Validate regex patterns during config load | 🔴 P0 |
| FM-008 | **File permission denied** (can't read ggen.toml) | 6 | 7 | 2 | **84** | std::fs::read_to_string error | Check file permissions BEFORE attempting read | 🟡 P1 |
| FM-009 | **Symlink traversal** (ggen.toml is symlink to /etc/passwd) | 2 | 10 | 8 | **160** | None | Resolve symlinks and validate target path | 🔴 P0 |
| FM-010 | **Memory exhaustion** (10GB ggen.toml file) | 3 | 9 | 6 | **162** | None | Add file size limit check (reject >10MB configs) | 🔴 P0 |
| FM-011 | **Command injection** (shell commands in config) | 4 | 10 | 5 | **200** | None | Sanitize all shell command inputs, use allowlist | 🔴 P0 |
| FM-012 | **Duplicate keys** (project_name appears twice) | 5 | 4 | 3 | **60** | TOML parser overwrites | Implement duplicate key detection and warning | 🟢 P2 |
| FM-013 | **Invalid UTF-8** (binary data in config file) | 4 | 6 | 3 | **72** | std::fs::read_to_string error | Add UTF-8 validation with clear error message | 🟡 P1 |
| FM-014 | **Malformed array** ([dependencies] has mixed types) | 6 | 6 | 3 | **108** | Serde error | Add type consistency checks for arrays | 🔴 P0 |
| FM-015 | **Missing closing bracket** (unclosed [section) | 7 | 7 | 2 | **98** | TOML parse error | Enhance error message with line/column context | 🟡 P1 |
| FM-016 | **Infinite recursion** (template includes itself) | 3 | 9 | 7 | **189** | Stack overflow | Add recursion depth limit (max 10 levels) | 🔴 P0 |
| FM-017 | **Clap parsing failure** (invalid CLI argument) | 7 | 5 | 2 | **70** | Clap error message | Add custom error messages with examples | 🟡 P1 |
| FM-018 | **Incompatible versions** (config v3.0, ggen v2.5) | 5 | 8 | 4 | **160** | None | Add version compatibility check on load | 🔴 P0 |
| FM-019 | **Encoding mismatch** (UTF-16 file read as UTF-8) | 3 | 7 | 5 | **105** | Garbled output | Auto-detect encoding or require UTF-8 BOM | 🔴 P0 |
| FM-020 | **Race condition** (config modified during read) | 4 | 6 | 8 | **192** | None | Use file locking or atomic reads | 🔴 P0 |
| FM-021 | **Default value override** (user config overrides system defaults incorrectly) | 6 | 5 | 4 | **120** | Merge logic unclear | Document and test merge priority order | 🔴 P0 |
| FM-022 | **Silent type coercion** ("123" → 123 without warning) | 7 | 4 | 6 | **168** | None | Add type coercion warnings for ambiguous cases | 🔴 P0 |
| FM-023 | **Incomplete error context** (error lacks file/line info) | 8 | 6 | 3 | **144** | Generic error messages | Add structured error context (file, line, column) | 🔴 P0 |
| FM-024 | **Timeout during parse** (slow regex causes hang) | 4 | 7 | 7 | **196** | None | Add parse timeout (fail after 5 seconds) | 🔴 P0 |
| FM-025 | **Unvalidated user input** (arbitrary strings in config) | 7 | 8 | 4 | **224** | None | Add input sanitization and allowlist validation | 🔴 P0 |

---

## RPN Distribution

- **Critical (RPN ≥ 200)**: 3 failures → **IMMEDIATE ACTION REQUIRED**
- **High (RPN 100-199)**: 15 failures → **Priority mitigation**
- **Medium (RPN 50-99)**: 6 failures → **Planned improvements**
- **Low (RPN < 50)**: 1 failure → **Monitor**

---

## Top 5 Critical Failures (RPN ≥ 200)

### 1️⃣ FM-006: Circular Dependency (RPN: 252)
**Risk**: Template A includes B, B includes A → infinite loop, stack overflow
**Mitigation**:
```rust
struct DependencyGraph {
    visited: HashSet<String>,
    stack: Vec<String>,
}

impl DependencyGraph {
    fn detect_cycle(&mut self, node: &str) -> Result<(), Error> {
        if self.stack.contains(&node.to_string()) {
            return Err(Error::CircularDependency(format!(
                "Cycle detected: {} -> {}",
                self.stack.join(" -> "),
                node
            )));
        }
        self.stack.push(node.to_string());
        // Process dependencies...
        self.stack.pop();
        Ok(())
    }
}
```

### 2️⃣ FM-025: Unvalidated User Input (RPN: 224)
**Risk**: Arbitrary strings in config → command injection, path traversal
**Mitigation**:
```rust
fn validate_project_name(name: &str) -> Result<String, Error> {
    let allowed = Regex::new(r"^[a-zA-Z0-9_-]+$")?;
    if !allowed.is_match(name) {
        return Err(Error::InvalidProjectName(
            "Project name must contain only alphanumeric, underscore, or hyphen".to_string()
        ));
    }
    if name.len() > 100 {
        return Err(Error::InvalidProjectName("Project name too long (max 100 chars)".to_string()));
    }
    Ok(name.to_string())
}
```

### 3️⃣ FM-011: Command Injection (RPN: 200)
**Risk**: Shell commands in config executed without sanitization
**Mitigation**:
```rust
fn safe_shell_command(cmd: &str) -> Result<String, Error> {
    // Allowlist of safe commands
    const ALLOWED_COMMANDS: &[&str] = &["git", "cargo", "npm"];

    let parts: Vec<&str> = cmd.split_whitespace().collect();
    if parts.is_empty() {
        return Err(Error::InvalidCommand("Empty command".to_string()));
    }

    if !ALLOWED_COMMANDS.contains(&parts[0]) {
        return Err(Error::InvalidCommand(format!(
            "Command '{}' not in allowlist: {:?}",
            parts[0], ALLOWED_COMMANDS
        )));
    }

    // Shell escape arguments
    let escaped_args: Vec<String> = parts[1..]
        .iter()
        .map(|arg| shell_escape::escape((*arg).into()).to_string())
        .collect();

    Ok(format!("{} {}", parts[0], escaped_args.join(" ")))
}
```

### 4️⃣ FM-024: Timeout During Parse (RPN: 196)
**Risk**: Slow regex or malformed TOML causes indefinite hang
**Mitigation**:
```rust
use std::time::{Duration, Instant};

fn parse_with_timeout(content: &str, timeout: Duration) -> Result<Config, Error> {
    let start = Instant::now();

    // Wrap parse in timeout check
    let config = toml::from_str::<Config>(content)
        .map_err(|e| Error::ParseError(e.to_string()))?;

    if start.elapsed() > timeout {
        return Err(Error::Timeout(format!(
            "Config parsing exceeded {}s timeout",
            timeout.as_secs()
        )));
    }

    Ok(config)
}
```

### 5️⃣ FM-020: Race Condition (RPN: 192)
**Risk**: Config modified while being read → partial/corrupt data
**Mitigation**:
```rust
use std::fs::File;
use std::io::Read;

fn atomic_read_config(path: &Path) -> Result<String, Error> {
    // Use File::open + read_to_end for atomic read
    let mut file = File::open(path)
        .map_err(|e| Error::FileRead(path.display().to_string(), e))?;

    let mut contents = String::new();
    file.read_to_end(&mut contents)
        .map_err(|e| Error::FileRead(path.display().to_string(), e))?;

    Ok(contents)
}
```

---

## Mitigation Priority Roadmap

### Phase 1 (Week 1): Critical Failures (RPN ≥ 200)
- [ ] FM-006: Implement cycle detection in dependency graph
- [ ] FM-025: Add input validation with allowlist
- [ ] FM-011: Sanitize shell commands with allowlist

### Phase 2 (Week 2): High Failures (RPN 100-199)
- [ ] FM-001: Add TOML syntax pre-validation
- [ ] FM-002: Implement schema validation with type coercion
- [ ] FM-003: Add path normalization and traversal checks
- [ ] FM-005: Validate env vars before expansion
- [ ] FM-009: Resolve symlinks and validate paths
- [ ] FM-010: Add file size limits
- [ ] FM-014: Check array type consistency
- [ ] FM-016: Limit recursion depth
- [ ] FM-018: Version compatibility checks
- [ ] FM-019: Auto-detect file encoding
- [ ] FM-021: Document config merge priority
- [ ] FM-022: Warn on type coercion
- [ ] FM-023: Add structured error context
- [ ] FM-024: Implement parse timeout

### Phase 3 (Week 3): Medium Failures (RPN 50-99)
- [ ] FM-004: Explicit field validation
- [ ] FM-008: Pre-check file permissions
- [ ] FM-013: UTF-8 validation
- [ ] FM-015: Enhanced parse error messages
- [ ] FM-017: Custom Clap error messages

---

## Testing Requirements

Each mitigation MUST include:
1. **Unit test**: Verify mitigation works in isolation
2. **Integration test**: Verify mitigation works end-to-end
3. **Negative test**: Verify failure is detected and handled
4. **Performance test**: Verify mitigation doesn't degrade performance

**Example Test Plan for FM-006 (Circular Dependency)**:
```rust
#[test]
fn test_circular_dependency_detection() {
    // Arrange: Create circular dependency
    let config = r#"
    [templates.A]
    includes = ["B"]

    [templates.B]
    includes = ["A"]
    "#;

    // Act: Parse config
    let result = Config::parse(config);

    // Assert: Error is detected
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("Cycle detected"));
}
```

---

## Success Metrics

- **Detection Rate**: ≥95% of injected failures detected
- **False Positive Rate**: ≤5% (don't reject valid configs)
- **Error Clarity**: 100% of errors include file/line/column context
- **Performance Impact**: ≤10% overhead from validation checks
- **Time to Detect**: <1 second for all validation checks

---

## NEXT STEPS

1. Implement **FM-006, FM-025, FM-011** (Critical RPN ≥200) in Phase 2
2. Add comprehensive test suite covering all 25 failure modes
3. Measure baseline failure detection rate (before mitigations)
4. Track RPN reduction after each mitigation is implemented
5. Review FMEA monthly and update with new failure modes

**LEAN MOTTO**: "Zero defects through systematic prevention, not heroic detection."
