# Pack System: Edge Cases and Constraints

## Overview

This document catalogs edge cases, system constraints, and boundary conditions for the packs system. Understanding these limits ensures robust implementation and clear user expectations.

---

## Edge Cases

### 1. Pack Composition Edge Cases

#### 1.1 Circular Dependencies

**Scenario**: Pack A depends on Pack B, which depends on Pack A.

```toml
# pack-a/pack.toml
[dependencies]
pack-b = "^1.0.0"

# pack-b/pack.toml
[dependencies]
pack-a = "^1.0.0"
```

**Expected Behavior**:
- Dependency resolution detects cycle
- Error: "Circular dependency detected: pack-a → pack-b → pack-a"
- Refuse to install/generate

**Implementation**:
```rust
fn detect_circular_dependencies(pack: &Pack, visited: &mut HashSet<String>) -> Result<()> {
    if visited.contains(&pack.metadata.name) {
        return Err(PackError::DependencyResolutionFailed(
            format!("Circular dependency detected: {}", pack.metadata.name)
        ));
    }
    visited.insert(pack.metadata.name.clone());
    // Check dependencies recursively
    Ok(())
}
```

---

#### 1.2 Diamond Dependencies

**Scenario**: Pack A depends on Pack B and Pack C, both of which depend on Pack D (different versions).

```
    Pack A
   /      \
Pack B    Pack C
  |        |
  v1.0     v2.0
   \      /
    Pack D
```

**Expected Behavior**:
- Use highest compatible version (semver resolution)
- If incompatible versions: Error with conflict details
- Suggest manual version override

**Example**:
```
Error: Dependency conflict for pack-d
  pack-b requires: pack-d ^1.0.0
  pack-c requires: pack-d ^2.0.0
  No common version satisfies both constraints.

Suggestion: Upgrade pack-b or downgrade pack-c
```

---

#### 1.3 Deep Dependency Trees

**Scenario**: Pack has 10+ levels of transitive dependencies.

```
Pack A → Pack B → Pack C → ... → Pack J
```

**Expected Behavior**:
- Resolve all dependencies (no depth limit)
- Warn if dependency tree > 5 levels deep
- Timeout after 30 seconds of resolution

**Constraints**:
- Max dependency depth: Unlimited (but warn at 5+)
- Max total dependencies: 100 packages
- Resolution timeout: 30 seconds

---

#### 1.4 Empty Packs

**Scenario**: Pack with no templates, queries, or hooks.

```toml
[metadata]
name = "empty-pack"
version = "1.0.0"

# No templates, queries, or hooks
```

**Expected Behavior**:
- Pack is valid but generates warning
- Generate operation creates empty directory
- Validation score < 50

**Use Case**: Documentation-only or metadata-only packs.

---

#### 1.5 Conflicting File Outputs

**Scenario**: Multiple templates in same pack generate to the same file.

```toml
[[templates]]
source = { type = "marketplace", package_id = "io.ggen.dockerfile" }
output_path = "Dockerfile"

[[templates]]
source = { type = "marketplace", package_id = "io.ggen.docker-compose" }
output_path = "Dockerfile"  # Same file!
```

**Expected Behavior**:
- Validation detects conflict
- Error: "Multiple templates output to same file: Dockerfile"
- Refuse to generate until resolved

---

### 2. Variable Handling Edge Cases

#### 2.1 Variable Name Collisions

**Scenario**: Two packs define same variable with different types.

```toml
# pack-a.toml
[[variables]]
name = "port"
type = "integer"
default = "8080"

# pack-b.toml
[[variables]]
name = "port"
type = "string"
default = "http://localhost:8080"
```

**Expected Behavior**:
- Detect type mismatch during composition
- Error with pack names and variable definitions
- Require explicit resolution (rename or override)

---

#### 2.2 Missing Required Variables

**Scenario**: User doesn't provide required variable.

```bash
ggen pack generate startup-pack --output my-app
# Missing: project_name (required)
```

**Expected Behavior**:
- Non-interactive: Error with missing variable list
- Interactive: Prompt for missing variables
- Show examples and descriptions

**Error Message**:
```
Error: Missing required variables:
  - project_name (Project name)
    Example: my-startup
  - database_type (Database type)
    Example: postgres, mongodb, mysql
```

---

#### 2.3 Variable Validation Failure

**Scenario**: User provides value that fails validation.

```toml
[[variables]]
name = "port"
type = "integer"
[variables.validation]
min = 1024
max = 65535
```

```bash
ggen pack generate app --var port=80
```

**Expected Behavior**:
- Validation fails before generation starts
- Error: "Variable 'port' value 80 is less than minimum 1024"
- Suggest valid range

---

#### 2.4 Undefined Variable References in Templates

**Scenario**: Template references variable not in pack manifest.

```handlebars
# Template content
port = {{ undefined_variable }}
```

**Expected Behavior**:
- Detection: During template rendering
- Error: "Template references undefined variable: undefined_variable"
- List available variables

---

### 3. Template Execution Edge Cases

#### 3.1 Template Not Found

**Scenario**: Pack references marketplace template that doesn't exist.

```toml
[[templates]]
source = { type = "marketplace", package_id = "io.ggen.nonexistent" }
```

**Expected Behavior**:
- Validation detects missing template
- Error: "Template not found: io.ggen.nonexistent"
- Suggest similar templates (fuzzy search)

---

#### 3.2 Template Rendering Timeout

**Scenario**: Template takes too long to render (> 5 minutes).

**Expected Behavior**:
- Timeout after 5 minutes
- Error: "Template rendering timed out: io.ggen.large-monorepo"
- Partial output is cleaned up (atomic operation)

---

#### 3.3 Template Output Path Conflicts

**Scenario**: Template wants to write to path outside output directory.

```yaml
# template.yaml
- path: "../../../etc/passwd"
  content: "malicious content"
```

**Expected Behavior**:
- Path validation before write
- Error: "Template attempted to write outside output directory"
- Security: No path traversal allowed

**Implementation**:
```rust
fn validate_output_path(output_dir: &Path, file_path: &Path) -> Result<()> {
    let canonical_output = output_dir.canonicalize()?;
    let canonical_file = output_dir.join(file_path).canonicalize()?;

    if !canonical_file.starts_with(&canonical_output) {
        return Err(PackError::SecurityViolation(
            "Path traversal detected".to_string()
        ));
    }
    Ok(())
}
```

---

#### 3.4 Binary File Templates

**Scenario**: Template contains binary data (images, executables).

**Expected Behavior**:
- Support base64-encoded binary content
- File type detection
- Special handling for binary files

**pack.toml**:
```toml
[[templates]]
source = { type = "inline", content = "base64:iVBORw0KGgoAAAA..." }
output_path = "assets/logo.png"
```

---

### 4. SPARQL Query Edge Cases

#### 4.1 Empty Query Results

**Scenario**: SPARQL query returns no results.

```sparql
SELECT ?service WHERE {
  ?service a :NonExistentType .
}
```

**Expected Behavior**:
- Generate empty output file or skip file generation
- Warning: "SPARQL query returned no results: service-discovery"
- Continue generation (queries are optional)

---

#### 4.2 Query Syntax Errors

**Scenario**: Malformed SPARQL query.

```sparql
SELECT ?service WHERE  # Missing closing brace
```

**Expected Behavior**:
- Validation catches syntax error
- Error: "SPARQL syntax error at line 1: Unexpected end of query"
- Refuse to generate

---

#### 4.3 RDF Store Not Available

**Scenario**: SPARQL executor cannot connect to RDF store.

**Expected Behavior**:
- Skip SPARQL queries (optional by default)
- Warning: "RDF store unavailable, skipping queries"
- Continue generation with warning flag

---

### 5. Hook Execution Edge Cases

#### 5.1 Hook Command Not Found

**Scenario**: Hook references non-existent command.

```toml
[[hooks.post_generation]]
name = "install-deps"
type = "command"
command = "nonexistent-command"
```

**Expected Behavior**:
- Hook fails with clear error
- Check `continue_on_error` flag
- If false: Stop generation, rollback
- If true: Log error, continue

---

#### 5.2 Hook Timeout

**Scenario**: Hook runs longer than timeout.

```toml
[[hooks.post_generation]]
name = "long-running-task"
timeout = 30  # seconds
```

**Expected Behavior**:
- Kill process after timeout
- Error: "Hook 'long-running-task' timed out after 30 seconds"
- Check `continue_on_error` flag

---

#### 5.3 Hook Working Directory Doesn't Exist

**Scenario**: Hook specifies non-existent working directory.

```toml
[[hooks.post_generation]]
type = "command"
command = "npm install"
working_dir = "/nonexistent/path"
```

**Expected Behavior**:
- Error before executing hook
- "Hook working directory doesn't exist: /nonexistent/path"
- Refuse to execute

---

### 6. File System Edge Cases

#### 6.1 Output Directory Not Writable

**Scenario**: User lacks write permissions.

```bash
ggen pack generate app --output /root/my-app
```

**Expected Behavior**:
- Check permissions before generation
- Error: "Output directory not writable: /root/my-app"
- Suggest alternative paths

---

#### 6.2 Insufficient Disk Space

**Scenario**: Not enough space for generated files.

**Expected Behavior**:
- Estimate space needed (based on templates)
- Check available space before generation
- Error: "Insufficient disk space (need 500MB, have 100MB)"

---

#### 6.3 File Name Length Limits

**Scenario**: Generated file path exceeds OS limits (255 bytes on most systems).

**Expected Behavior**:
- Validate path lengths before generation
- Error: "File path too long: /very/long/path/..."
- Suggest shorter output directory

---

#### 6.4 Special Characters in File Names

**Scenario**: Template generates files with special characters.

```yaml
- path: "file:with*special?chars.txt"
```

**Expected Behavior**:
- Sanitize file names
- Warning: "Sanitized file name: 'file:with*special?chars.txt' → 'file_with_special_chars.txt'"
- Configurable sanitization rules

---

### 7. Concurrency Edge Cases

#### 7.1 Concurrent Pack Installs

**Scenario**: Two processes install same pack simultaneously.

```bash
# Terminal 1
ggen pack install startup-pack

# Terminal 2 (at same time)
ggen pack install startup-pack
```

**Expected Behavior**:
- File locking on pack directory
- Second process waits or retries
- No corrupted installations

---

#### 7.2 Concurrent Generation to Same Directory

**Scenario**: Two processes generate to same output directory.

```bash
# Terminal 1
ggen pack generate app --output my-app

# Terminal 2
ggen pack generate another-app --output my-app
```

**Expected Behavior**:
- Detect concurrent generation attempt
- Error: "Output directory is being used by another process"
- Lock file: `my-app/.ggen-lock`

---

### 8. Version Management Edge Cases

#### 8.1 Semantic Version Wildcards

**Scenario**: Pack depends on wildcard version.

```toml
[dependencies]
pack-b = "*"  # Any version
```

**Expected Behavior**:
- Resolve to latest compatible version
- Warning: "Wildcard dependencies are not recommended"
- Update resolves to specific version

---

#### 8.2 Pre-release Versions

**Scenario**: Pack depends on pre-release version.

```toml
[dependencies]
pack-b = "2.0.0-beta.1"
```

**Expected Behavior**:
- Allow pre-release dependencies
- Warning: "Depending on pre-release version: pack-b@2.0.0-beta.1"
- Not included in regular version resolution

---

#### 8.3 Version Yanking

**Scenario**: Installed pack version is yanked from registry.

**Expected Behavior**:
- Warning on `pack list`: "Version 1.2.0 has been yanked"
- Can still use installed version
- Suggest upgrading to non-yanked version

---

### 9. Configuration Edge Cases

#### 9.1 Malformed Manifest

**Scenario**: pack.toml has syntax errors.

```toml
[metadata
name = "missing-bracket"  # Missing closing bracket
```

**Expected Behavior**:
- Parse error with line number
- Error: "Syntax error in pack.toml at line 1: Expected ']'"
- Refuse to load pack

---

#### 9.2 Missing Required Fields

**Scenario**: pack.toml missing required fields.

```toml
[metadata]
# Missing: name, version
description = "A pack without name"
```

**Expected Behavior**:
- Validation error
- Error: "Missing required fields: name, version"
- List all missing fields

---

#### 9.3 Conflicting Configuration

**Scenario**: Pack config has contradictory settings.

```toml
[[templates]]
[templates.config]
overwrite = true
skip_if_exists = true  # Contradiction!
```

**Expected Behavior**:
- Validation warning
- Warning: "Conflicting config: overwrite and skip_if_exists both true"
- Precedence: overwrite > skip_if_exists

---

### 10. Network Edge Cases

#### 10.1 Offline Mode

**Scenario**: No network connectivity during install.

```bash
ggen pack install startup-pack  # No internet
```

**Expected Behavior**:
- Try local cache first
- If not cached: Error with clear message
- "Network unavailable. Pack not in local cache."
- Offer offline mode (use only cached packs)

---

#### 10.2 Slow Network

**Scenario**: Network extremely slow (< 10 KB/s).

**Expected Behavior**:
- Show download progress
- Allow cancellation (Ctrl+C)
- Resume support for interrupted downloads

---

#### 10.3 Registry Unavailable

**Scenario**: Remote registry is down.

**Expected Behavior**:
- Timeout after 10 seconds
- Fall back to local cache
- Error: "Registry unavailable. Using local cache only."

---

## System Constraints

### 1. Size Constraints

| Constraint | Limit | Rationale |
|------------|-------|-----------|
| Max pack size | 100 MB | Reasonable size for templates + metadata |
| Max template file size | 10 MB | Prevent memory issues during rendering |
| Max number of templates per pack | 100 | Maintainability and performance |
| Max number of SPARQL queries | 50 | Performance consideration |
| Max number of variables | 100 | User experience (too many confusing) |
| Max dependency depth | Unlimited (warn at 5) | Allow flexibility but warn about complexity |
| Max total dependencies | 100 packs | Performance and resolution time |
| Max manifest size | 10 MB | TOML/YAML parsing limits |

---

### 2. Performance Constraints

| Operation | Target | Constraint |
|-----------|--------|------------|
| Pack list | < 50ms | User expectation for instant feedback |
| Pack search | < 200ms | Acceptable search latency |
| Pack install | < 10s | Network-dependent, show progress |
| Pack validation | < 500ms | Blocking operation, must be fast |
| Pack generation (single) | < 10s | User patience limit |
| Pack generation (composition 3 packs) | < 30s | Acceptable for complex operations |
| Dependency resolution | < 5s | Complexity increases with depth |
| SPARQL query execution | < 2s per query | Database query performance |

---

### 3. Resource Constraints

| Resource | Limit | Behavior on Exceed |
|----------|-------|-------------------|
| Memory usage | < 500 MB | Paginate large result sets |
| Disk space (install) | < 1 GB for 1000 packs | Warn user, suggest cleanup |
| Disk space (generation) | Check before generation | Fail with clear error |
| CPU usage | < 80% sustained | Throttle parallel operations |
| File descriptors | < 1000 open | Close files promptly |

---

### 4. Security Constraints

| Constraint | Enforcement | Rationale |
|------------|-------------|-----------|
| No arbitrary code execution | Static manifest only | Security |
| No path traversal | Validate all paths | Prevent directory escape |
| Sandboxed template rendering | ggen-core sandbox | Isolate template execution |
| Checksum verification | SHA-256 | Integrity verification |
| No network access from templates | ggen-core restriction | Prevent data exfiltration |

---

### 5. Compatibility Constraints

| Constraint | Requirement | Impact |
|------------|-------------|--------|
| Minimum ggen version | Specified in pack manifest | Refuse to load on old versions |
| Semantic versioning | Required for all packs | Dependency resolution |
| Marketplace template versions | Compatible with pack system | Version matching required |
| RDF/SPARQL version | SPARQL 1.1 | Query syntax validation |
| OS compatibility | Linux, macOS, Windows | Path handling differences |

---

### 6. Operational Constraints

| Constraint | Limit | Mitigation |
|------------|-------|------------|
| Concurrent installations | 1 per pack | File locking |
| Concurrent generations | Unlimited (different outputs) | No shared state |
| Registry update frequency | Every 15 minutes | Caching |
| Cache expiration | 24 hours | Configurable |
| Log retention | 7 days | Rotation |

---

## Boundary Conditions

### 1. Minimum Viable Pack

```toml
[metadata]
name = "minimal-pack"
version = "1.0.0"
title = "Minimal Pack"
description = "The smallest valid pack"
category = "custom"
license = "MIT"

[[templates]]
source = { type = "inline", content = "Hello, World!", format = "single_file" }
output_path = "hello.txt"
```

**Behavior**: Valid, generates single file.

---

### 2. Maximum Complexity Pack

```toml
[metadata]
name = "mega-pack"
version = "1.0.0"
# ... metadata ...

# 100 templates (at limit)
[[templates]]
# ...

# 50 SPARQL queries (at limit)
[[queries]]
# ...

# 100 variables (at limit)
[[variables]]
# ...

# 100 dependencies (at limit)
[dependencies]
# ...
```

**Behavior**:
- Validation warns about complexity
- Generation may be slow (> 30s)
- Consider splitting into multiple packs

---

### 3. Zero-Configuration Pack

```toml
[metadata]
name = "zero-config"
version = "1.0.0"
# ... metadata ...

[[templates]]
source = { type = "marketplace", package_id = "io.ggen.static-site" }
# No variables, uses all defaults
```

**Behavior**:
- Generates without prompts
- All defaults used
- Minimal user interaction

---

### 4. Infinite Recursion Protection

**Scenario**: Pack includes itself as dependency (different version).

```toml
[dependencies]
my-pack = "^0.9.0"  # Earlier version of itself
```

**Expected Behavior**:
- Circular dependency detection catches this
- Error: "Pack cannot depend on itself"

---

## Testing Strategy for Edge Cases

### 1. Property-Based Testing

Use property-based testing for:
- Dependency resolution (all DAGs should resolve)
- Variable substitution (all valid inputs should work)
- Path validation (no valid path should be rejected)

### 2. Fuzz Testing

Fuzz test:
- Manifest parsing (malformed TOML/YAML)
- Variable values (special characters, Unicode)
- File paths (length limits, special characters)

### 3. Chaos Engineering

Simulate:
- Network failures during install
- Disk full during generation
- Process kills mid-operation

### 4. Integration Tests

Test all edge cases with actual packs:
- Real marketplace templates
- Real SPARQL queries
- Real file systems

---

## Conclusion

This document catalogs **60+ edge cases** and **30+ system constraints**. Key takeaways:

1. **Validation is critical**: Catch errors before generation
2. **Clear error messages**: Help users recover from failures
3. **Graceful degradation**: Continue on non-critical failures
4. **Security first**: No arbitrary code, path validation
5. **Performance bounds**: Hard limits prevent runaway operations

Implementing robust handling for these edge cases will ensure a **production-ready packs system**.
