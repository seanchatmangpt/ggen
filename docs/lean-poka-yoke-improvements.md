# Poka-Yoke - Mistake-Proofing Improvements
## Phase 2: ggen.toml + clap-noun-verb Integration

**LEAN PRINCIPLE**: Design systems that make errors IMPOSSIBLE, not just detectable.

**Analysis Date**: 2025-11-18
**Target**: Zero defects through prevention-first design
**Inspiration**: Toyota Production System's error-proofing methodology

---

## Poka-Yoke Categories

1. **Prevention Poka-Yoke**: Make errors impossible to occur
2. **Detection Poka-Yoke**: Detect errors immediately when they occur

---

## 🛡️ PREVENTION POKA-YOKE (Make Errors Impossible)

### PY-001: Type-Safe Config Schema (Compile-Time Prevention)

**Problem**: Runtime type mismatches (expected String, got Integer)
**Solution**: Use Rust's type system to prevent invalid configs at compile time

```rust
// ❌ BEFORE: Runtime errors
#[derive(Deserialize)]
struct Config {
    project_name: String,  // Runtime error if user passes integer
    version: String,       // Runtime error if user passes float
}

// ✅ AFTER: Compile-time prevention
#[derive(Deserialize)]
struct Config {
    #[serde(deserialize_with = "validate_project_name")]
    project_name: ProjectName,  // NewType enforces validation

    #[serde(deserialize_with = "validate_version")]
    version: SemanticVersion,   // Type ensures correct format
}

// NewType pattern - makes invalid states unrepresentable
struct ProjectName(String);

impl ProjectName {
    fn new(s: &str) -> Result<Self, ConfigError> {
        if !s.chars().all(|c| c.is_alphanumeric() || c == '_' || c == '-') {
            return Err(ConfigError::InvalidProjectName(s.to_string()));
        }
        if s.len() > 100 {
            return Err(ConfigError::ProjectNameTooLong);
        }
        Ok(ProjectName(s.to_string()))
    }
}

// Custom deserializer ensures validation happens during parse
fn validate_project_name<'de, D>(deserializer: D) -> Result<ProjectName, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let s = String::deserialize(deserializer)?;
    ProjectName::new(&s).map_err(serde::de::Error::custom)
}
```

**Benefit**: **Impossible to create invalid config** - type system enforces correctness.

---

### PY-002: Path Normalization (Prevention of Path Traversal)

**Problem**: `../../etc/passwd` in config paths
**Solution**: Normalize ALL paths BEFORE use, reject dangerous patterns

```rust
use std::path::{Path, PathBuf};

struct SafePath(PathBuf);

impl SafePath {
    fn new(path: impl AsRef<Path>) -> Result<Self, SecurityError> {
        let path = path.as_ref();

        // 1. Reject path traversal patterns
        if path.to_str().map(|s| s.contains("..")).unwrap_or(false) {
            return Err(SecurityError::PathTraversal(path.display().to_string()));
        }

        // 2. Normalize to absolute path
        let normalized = path.canonicalize()
            .map_err(|e| SecurityError::InvalidPath(path.display().to_string(), e))?;

        // 3. Ensure path is within allowed root
        let allowed_root = std::env::current_dir()?;
        if !normalized.starts_with(&allowed_root) {
            return Err(SecurityError::OutsideRoot(normalized.display().to_string()));
        }

        Ok(SafePath(normalized))
    }

    fn as_path(&self) -> &Path {
        &self.0
    }
}

// ❌ BEFORE: Vulnerable
fn load_template(path: &str) -> Result<Template, Error> {
    std::fs::read_to_string(path)  // Allows ../../etc/passwd
}

// ✅ AFTER: Impossible to escape root
fn load_template(path: &str) -> Result<Template, Error> {
    let safe_path = SafePath::new(path)?;  // Rejects path traversal
    std::fs::read_to_string(safe_path.as_path())
}
```

**Benefit**: **Impossible to traverse outside allowed root** - normalization prevents escapes.

---

### PY-003: Environment Variable Validation (Pre-Expansion Check)

**Problem**: `$NONEXISTENT_VAR` expansion fails at runtime
**Solution**: Validate all env vars BEFORE expansion

```rust
struct EnvExpander {
    required_vars: HashSet<String>,
}

impl EnvExpander {
    fn new() -> Self {
        Self {
            required_vars: HashSet::new(),
        }
    }

    // Phase 1: Collect all env vars referenced in config (NO expansion yet)
    fn scan_config(&mut self, config_str: &str) -> Result<(), ValidationError> {
        let env_regex = Regex::new(r"\$\{([A-Z_][A-Z0-9_]*)\}")?;

        for cap in env_regex.captures_iter(config_str) {
            let var_name = cap[1].to_string();
            self.required_vars.insert(var_name);
        }

        Ok(())
    }

    // Phase 2: Validate ALL vars exist BEFORE expanding ANY
    fn validate_all(&self) -> Result<(), ValidationError> {
        let mut missing = Vec::new();

        for var in &self.required_vars {
            if std::env::var(var).is_err() {
                missing.push(var.clone());
            }
        }

        if !missing.is_empty() {
            return Err(ValidationError::MissingEnvVars(missing));
        }

        Ok(())
    }

    // Phase 3: Only expand AFTER validation passes
    fn expand(&self, config_str: &str) -> Result<String, Error> {
        self.validate_all()?;  // Fail-fast if ANY var missing

        let env_regex = Regex::new(r"\$\{([A-Z_][A-Z0-9_]*)\}")?;
        let result = env_regex.replace_all(config_str, |caps: &regex::Captures| {
            let var_name = &caps[1];
            std::env::var(var_name).unwrap()  // Safe - already validated
        });

        Ok(result.to_string())
    }
}

// ❌ BEFORE: Expansion fails mid-process
let expanded = config.replace("${HOME}", &std::env::var("HOME")?);

// ✅ AFTER: Validate BEFORE expansion
let mut expander = EnvExpander::new();
expander.scan_config(&config)?;
expander.validate_all()?;  // Fail here if ANY var missing
let expanded = expander.expand(&config)?;
```

**Benefit**: **Impossible to expand invalid config** - validation happens before any side effects.

---

### PY-004: Circular Dependency Detection (Build-Time Graph Check)

**Problem**: Template A includes B, B includes A → infinite loop
**Solution**: Detect cycles BEFORE template execution

```rust
struct DependencyGraph {
    nodes: HashMap<String, Vec<String>>,
}

impl DependencyGraph {
    fn add_edge(&mut self, from: &str, to: &str) {
        self.nodes.entry(from.to_string())
            .or_default()
            .push(to.to_string());
    }

    fn detect_cycles(&self) -> Result<(), CycleError> {
        let mut visited = HashSet::new();
        let mut rec_stack = HashSet::new();

        for node in self.nodes.keys() {
            if self.has_cycle(node, &mut visited, &mut rec_stack)? {
                return Err(CycleError::CircularDependency(
                    rec_stack.iter().cloned().collect()
                ));
            }
        }

        Ok(())
    }

    fn has_cycle(
        &self,
        node: &str,
        visited: &mut HashSet<String>,
        rec_stack: &mut HashSet<String>,
    ) -> Result<bool, CycleError> {
        if rec_stack.contains(node) {
            return Ok(true);  // Cycle detected
        }

        if visited.contains(node) {
            return Ok(false);  // Already checked
        }

        visited.insert(node.to_string());
        rec_stack.insert(node.to_string());

        if let Some(neighbors) = self.nodes.get(node) {
            for neighbor in neighbors {
                if self.has_cycle(neighbor, visited, rec_stack)? {
                    return Ok(true);
                }
            }
        }

        rec_stack.remove(node);
        Ok(false)
    }
}

// ❌ BEFORE: Runtime stack overflow
fn process_template(name: &str) -> Result<Output, Error> {
    let includes = config.get_includes(name)?;
    for include in includes {
        process_template(&include)?;  // Infinite loop if cycle exists
    }
}

// ✅ AFTER: Detect cycles BEFORE execution
fn validate_config(config: &Config) -> Result<(), Error> {
    let mut graph = DependencyGraph::new();

    for (name, template) in &config.templates {
        for include in &template.includes {
            graph.add_edge(name, include);
        }
    }

    graph.detect_cycles()?;  // Fail here if cycle exists
    Ok(())
}
```

**Benefit**: **Impossible to execute cyclic templates** - graph analysis prevents execution.

---

### PY-005: Regex Validation (Compile-Time Pattern Check)

**Problem**: Invalid regex patterns cause runtime panic
**Solution**: Validate ALL regex patterns during config load

```rust
#[derive(Deserialize)]
struct ValidationRule {
    #[serde(deserialize_with = "validate_regex")]
    pattern: CompiledRegex,
}

struct CompiledRegex(Regex);

fn validate_regex<'de, D>(deserializer: D) -> Result<CompiledRegex, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let pattern_str = String::deserialize(deserializer)?;

    let regex = Regex::new(&pattern_str)
        .map_err(|e| serde::de::Error::custom(format!(
            "Invalid regex '{}': {}",
            pattern_str, e
        )))?;

    Ok(CompiledRegex(regex))
}

// ❌ BEFORE: Runtime panic
let pattern = Regex::new(&config.pattern)?;  // Panic if invalid

// ✅ AFTER: Compile-time validation
let config: ValidationRule = toml::from_str(toml_str)?;  // Fails if regex invalid
```

**Benefit**: **Impossible to use invalid regex** - compilation happens at config load.

---

## 🔍 DETECTION POKA-YOKE (Immediate Error Detection)

### PY-006: 1-Second Fail-Fast Validation

**Problem**: Errors discovered late in execution
**Solution**: Fail within 1 second if config is invalid

```rust
use std::time::{Duration, Instant};

const VALIDATION_TIMEOUT: Duration = Duration::from_secs(1);

fn validate_config_fast(config_str: &str) -> Result<Config, ValidationError> {
    let start = Instant::now();

    // Phase 1: Parse (should be <100ms)
    let config: Config = toml::from_str(config_str)?;

    // Phase 2: Validate (should be <500ms)
    validate_schema(&config)?;
    validate_paths(&config)?;
    validate_env_vars(&config)?;
    validate_regex_patterns(&config)?;
    validate_dependencies(&config)?;

    // Phase 3: Check timeout
    if start.elapsed() > VALIDATION_TIMEOUT {
        return Err(ValidationError::Timeout(start.elapsed()));
    }

    Ok(config)
}
```

**Benefit**: **Errors detected in <1 second** - instant feedback loop.

---

### PY-007: Structured Error Context

**Problem**: Generic errors like "Parse error"
**Solution**: Include file, line, column, and example in EVERY error

```rust
#[derive(Debug)]
struct ValidationError {
    file: PathBuf,
    line: usize,
    column: usize,
    error_type: ErrorType,
    message: String,
    suggestion: Option<String>,
}

impl ValidationError {
    fn display(&self) -> String {
        format!(
            "Error in {file}:{line}:{column}
  Error: {error_type}
  {message}
  {suggestion}",
            file = self.file.display(),
            line = self.line,
            column = self.column,
            error_type = self.error_type,
            message = self.message,
            suggestion = self.suggestion.as_deref().unwrap_or("")
        )
    }
}

// Example error message:
/*
Error in ggen.toml:12:5
  Error: InvalidProjectName
  Project name 'my project!' contains invalid characters
  Suggestion: Use only alphanumeric, underscore, or hyphen characters
  Example: my-project or my_project
*/
```

**Benefit**: **100% of errors include actionable context** - developers know exactly what to fix.

---

### PY-008: Panic-Safe Design (No Resource Leaks on Error)

**Problem**: Panic during validation leaves files open
**Solution**: Use RAII and panic guards

```rust
struct ConfigLoader {
    file: Option<File>,
}

impl ConfigLoader {
    fn load(&mut self, path: &Path) -> Result<Config, Error> {
        // File is auto-closed on panic (RAII)
        let mut file = File::open(path)?;
        self.file = Some(file);

        let mut contents = String::new();
        file.read_to_string(&mut contents)?;

        let config = toml::from_str(&contents)?;

        // Explicit cleanup (file closed even on panic)
        self.file = None;

        Ok(config)
    }
}

impl Drop for ConfigLoader {
    fn drop(&mut self) {
        if let Some(file) = self.file.take() {
            // Ensure file is closed even on panic
            drop(file);
        }
    }
}
```

**Benefit**: **Zero resource leaks on error** - RAII ensures cleanup.

---

### PY-009: Test Coverage on All Error Paths

**Problem**: Error paths untested
**Solution**: 100% coverage of error handling code

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_invalid_project_name_error() {
        let config = r#"
        project_name = "my project!"  // Invalid (contains space and !)
        "#;

        let result = Config::parse(config);
        assert!(result.is_err());

        let err = result.unwrap_err();
        assert!(err.to_string().contains("InvalidProjectName"));
        assert!(err.to_string().contains("my project!"));
    }

    #[test]
    fn test_missing_env_var_error() {
        std::env::remove_var("NONEXISTENT");

        let config = r#"
        output_dir = "${NONEXISTENT}/output"
        "#;

        let result = Config::parse(config);
        assert!(result.is_err());

        let err = result.unwrap_err();
        assert!(err.to_string().contains("MissingEnvVar"));
        assert!(err.to_string().contains("NONEXISTENT"));
    }
}
```

**Benefit**: **All error paths tested** - no surprises in production.

---

### PY-010: Clear Error Messages with Examples

**Problem**: "Validation failed" with no context
**Solution**: Include examples in ALL error messages

```rust
fn validate_version(version: &str) -> Result<(), Error> {
    let semver_regex = Regex::new(r"^\d+\.\d+\.\d+$")?;

    if !semver_regex.is_match(version) {
        return Err(Error::InvalidVersion {
            actual: version.to_string(),
            expected: "Semantic version (e.g., 1.2.3)".to_string(),
            examples: vec![
                "1.0.0".to_string(),
                "2.5.1".to_string(),
                "0.1.0".to_string(),
            ],
        });
    }

    Ok(())
}
```

**Benefit**: **Developers see correct format immediately** - no guessing.

---

## Summary Table: 10 Poka-Yoke Improvements

| ID | Type | Improvement | Impact | Effort | Priority |
|----|------|-------------|--------|--------|----------|
| PY-001 | Prevention | Type-safe config schema | Prevents type mismatches | Medium | 🔴 P0 |
| PY-002 | Prevention | Path normalization | Prevents path traversal | High | 🔴 P0 |
| PY-003 | Prevention | Env var pre-validation | Prevents expansion failures | Medium | 🔴 P0 |
| PY-004 | Prevention | Cycle detection | Prevents infinite loops | High | 🔴 P0 |
| PY-005 | Prevention | Regex validation | Prevents runtime panics | Low | 🟡 P1 |
| PY-006 | Detection | 1-second fail-fast | Instant error feedback | Medium | 🔴 P0 |
| PY-007 | Detection | Structured error context | Actionable error messages | Medium | 🟡 P1 |
| PY-008 | Detection | Panic-safe design | Prevents resource leaks | Low | 🟡 P1 |
| PY-009 | Detection | Error path test coverage | Prevents untested errors | High | 🔴 P0 |
| PY-010 | Detection | Clear error messages | Developer productivity | Low | 🟡 P1 |

---

## Implementation Roadmap

### Week 1: Prevention (PY-001 to PY-005)
- [ ] Implement type-safe NewType wrappers
- [ ] Add path normalization layer
- [ ] Add env var pre-validation
- [ ] Implement dependency graph cycle detection
- [ ] Add regex validation during config load

### Week 2: Detection (PY-006 to PY-010)
- [ ] Add 1-second timeout validation
- [ ] Implement structured error context
- [ ] Add RAII panic guards
- [ ] Achieve 100% error path test coverage
- [ ] Enhance all error messages with examples

---

## Success Metrics

- **Prevention Rate**: ≥90% of errors prevented (not just detected)
- **Detection Time**: 100% of errors detected in <1 second
- **Error Clarity**: 100% of errors include file/line/column context
- **Resource Leak Rate**: 0% (all resources cleaned up on panic)
- **Developer Satisfaction**: ≥4.5/5 on error message helpfulness

**LEAN MOTTO**: "If it can go wrong, make it impossible to go wrong. If that's impossible, make it fail instantly with perfect clarity."
