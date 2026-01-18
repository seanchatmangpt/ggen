# Pre-flight Validation Error Codes

## Error Code Mapping (E0020-E0029)

### E0020: Insufficient Disk Space
**Severity**: Error (blocks execution)
**Check**: Disk space availability
**Requirement**: Minimum 100MB free space

**Example Error Message**:
```
error[E0020]: Insufficient disk space
  --> /home/user/project
  |
  = Available: 45.23 MB
  = Required: 100.00 MB
  = help: Free up at least 54.77 MB of disk space
```

**Resolution**:
1. Free up disk space by removing unnecessary files
2. Use `df -h` (Unix) or `dir` (Windows) to check available space
3. Consider using a different output directory with more space
4. Clear temporary files and caches

**Platform Support**:
- Unix/Linux: Uses `nix::sys::statvfs::statvfs()`
- Windows: Uses `GetDiskFreeSpaceExW()`
- Other: Assumes sufficient space (fallback)

---

### E0021: Insufficient Permissions
**Severity**: Error (blocks execution)
**Check**: Write permissions to output directory
**Requirement**: Ability to create and delete files

**Example Error Message**:
```
error[E0021]: Insufficient permissions
  --> /home/user/project
  |
  = Cannot write to directory
  = Error: Permission denied (os error 13)
  = help: Check directory permissions or run with appropriate privileges
```

**Resolution**:
1. Check directory ownership: `ls -la <directory>`
2. Verify permissions: Should have write access (w bit)
3. Change permissions if needed: `chmod u+w <directory>`
4. On Windows: Check folder properties and security settings
5. Consider running with elevated privileges if necessary
6. Use a different directory where you have write access

**Test Method**:
Creates `.ggen_preflight_test` file, then immediately removes it

---

### E0022: LLM Provider Unreachable
**Severity**: Warning (doesn't block execution for sync, can be configured)
**Check**: LLM provider connectivity
**Requirement**: Configured provider must be accessible

**Example Error Messages**:

**Ollama**:
```
error[E0022]: Ollama not reachable
  --> http://localhost:11434/api/tags
  |
  = Error: Connection refused (os error 111)
  = help: Start Ollama with 'ollama serve' or set OLLAMA_BASE_URL
```

**OpenAI**:
```
error[E0022]: OpenAI API key not configured
  |
  = Environment variable OPENAI_API_KEY is not set
  = help: Set OPENAI_API_KEY or switch to ollama provider
```

**Anthropic**:
```
error[E0022]: Anthropic API key not configured
  |
  = Environment variable ANTHROPIC_API_KEY is not set
  = help: Set ANTHROPIC_API_KEY or switch to ollama provider
```

**Resolution**:

For Ollama:
```bash
# Start Ollama service
ollama serve

# Or set custom URL
export OLLAMA_BASE_URL=http://your-server:11434
```

For OpenAI:
```bash
export OPENAI_API_KEY=sk-...
export GGEN_LLM_PROVIDER=openai
```

For Anthropic:
```bash
export ANTHROPIC_API_KEY=sk-ant-...
export GGEN_LLM_PROVIDER=anthropic
```

**Provider Detection**:
- Reads `GGEN_LLM_PROVIDER` environment variable
- Default: "ollama"
- Supported: ollama, openai, anthropic, mock

**Health Check Details**:
- Timeout: 5 seconds
- Endpoint (Ollama): `{base_url}/api/tags`
- Method: HTTP GET

---

### E0023: Manifest Syntax Error
**Severity**: Error (blocks execution)
**Check**: ggen.toml validity
**Requirement**: Valid TOML syntax and required fields

**Example Error Messages**:

**Empty project name**:
```
error[E0023]: Invalid manifest: project.name cannot be empty
  |
  = help: Set a valid project name in ggen.toml
```

**Missing ontology file**:
```
error[E0023]: Ontology file not found
  --> /home/user/project/schema/domain.ttl
  |
  = Specified in manifest: ontology.source
  = help: Create the ontology file or update the path in ggen.toml
```

**No generation rules**:
```
error[E0023]: No generation rules defined
  |
  = At least one generation rule is required
  = help: Add a [[generation.rules]] section to ggen.toml
```

**Resolution**:
1. Verify ggen.toml syntax is valid TOML
2. Ensure `project.name` is not empty
3. Check that `ontology.source` path exists
4. Add at least one `[[generation.rules]]` section
5. Run `ggen sync --validate-only` to test manifest

**Validation Checks**:
- Project name is not empty/whitespace
- Ontology source file exists at specified path
- At least one generation rule is defined

---

### E0024: Template Syntax Error
**Severity**: Error (blocks execution when template check enabled)
**Check**: Tera template syntax
**Requirement**: Valid Tera template files

**Example Error Messages**:

**Template file not found**:
```
error[E0024]: Template file not found
  --> /home/user/project/templates/rust.tera
  |
  = Rule: generate-structs
  = help: Create the template file or update the path in ggen.toml
```

**Syntax error**:
```
error[E0024]: Template syntax error
  --> /home/user/project/templates/rust.tera
  |
  = Rule: generate-structs
  = Error: Unexpected token at line 15: expected '}', got 'EOF'
  = help: Fix template syntax or check Tera documentation
```

**Resolution**:
1. Verify template file exists at specified path
2. Check Tera syntax: https://keats.github.io/tera/
3. Common issues:
   - Unclosed tags: `{% for ... %}` needs `{% endfor %}`
   - Missing braces: `{{ variable }}`
   - Invalid filters: `{{ value | unknown_filter }}`
4. Test template in isolation
5. Validate variable names match SPARQL query results

**Template Check Behavior**:
- Loads template file
- Parses with Tera engine
- Does NOT render (no context needed)
- Fast syntax validation only

---

### E0025: Missing Dependency
**Severity**: Warning (configurable per dependency)
**Check**: External tool availability
**Requirement**: Tools must be in PATH

**Example Error Message**:
```
error[E0025]: Git not found
  |
  = Error: No such file or directory (os error 2)
  = help: Install git or ensure it's in PATH
```

**Resolution**:

For Git:
```bash
# Ubuntu/Debian
sudo apt-get install git

# macOS
brew install git

# Windows
# Download from https://git-scm.com/download/win
```

**Check Method**:
```rust
std::process::Command::new("git")
    .arg("--version")
    .output()
```

**Extensibility**:
Can easily add checks for:
- Docker: `docker --version`
- Node.js: `node --version`
- Python: `python --version`
- Cargo: `cargo --version`

---

### E0026: Invalid Output Directory
**Severity**: Error (blocks execution)
**Check**: Output directory validity
**Requirement**: Valid, accessible directory path

**Example Error Message**:
```
error[E0026]: Invalid output directory
  --> /invalid/../../path
  |
  = Path contains invalid characters or is not accessible
  = help: Use a valid absolute or relative path
```

**Resolution**:
1. Check path syntax (no invalid characters)
2. Ensure path is accessible
3. Use absolute paths to avoid ambiguity
4. Verify parent directory exists

**Currently**: Reserved for future use (E0026 not yet implemented)

---

### E0027: Network Connectivity Issue
**Severity**: Error/Warning (depends on context)
**Check**: Network accessibility
**Requirement**: Network connectivity for remote operations

**Example Error Message**:
```
error[E0027]: Network client error
  |
  = Failed to create HTTP client
  = help: Check network connectivity
```

**Resolution**:
1. Check internet connection
2. Verify firewall settings
3. Check proxy configuration
4. Ensure DNS resolution works
5. Test with: `curl http://localhost:11434` (for Ollama)

**Common Causes**:
- Firewall blocking connections
- Proxy configuration issues
- Network interface down
- DNS resolution failure

---

### E0028: File System Error
**Severity**: Error (blocks execution)
**Check**: File system accessibility
**Requirement**: Readable/writable file system

**Example Error Messages**:

**Invalid path encoding**:
```
error[E0028]: Invalid path encoding
  --> /path/with/invalid/�/chars
  |
  = help: Use UTF-8 compatible paths
```

**Cannot get filesystem stats**:
```
error[E0028]: Cannot get filesystem stats
  --> /home/user/project
  |
  = Error: Stale file handle (os error 116)
  = help: Check if path exists and is accessible
```

**Resolution**:
1. Verify path uses valid UTF-8 encoding
2. Check if filesystem is mounted
3. Ensure no stale file handles (remount if needed)
4. Verify path exists and is accessible
5. Check for filesystem corruption

**Platform-Specific**:
- Unix: Uses `statvfs()` system call
- Windows: Uses `GetDiskFreeSpaceExW()`
- May fail on:
  - NFS mounts with stale handles
  - Disconnected network drives
  - Unmounted filesystems

---

### E0029: Pre-flight Check Timeout
**Severity**: Error (blocks execution)
**Check**: Total pre-flight duration
**Requirement**: All checks complete within timeout

**Example Error Message**:
```
error[E0029]: Pre-flight checks timeout
  |
  = Checks took 32.45s (limit: 30s)
  = help: Some checks may be hanging. Check network connectivity.
```

**Resolution**:
1. Check network connectivity (LLM health checks may hang)
2. Verify file system is responsive
3. Look for hanging processes
4. Increase timeout if checks are legitimately slow
5. Disable optional checks (LLM, git) if not needed

**Timeout Configuration**:
- Default: 30 seconds total
- LLM health check: 5 seconds
- Per-check timeouts:
  - Disk space: <100ms
  - Permissions: <100ms
  - LLM provider: 5s
  - Manifest: <500ms
  - Templates: <2s per template

**Common Causes**:
- Slow network (LLM checks)
- Slow filesystem (NFS, network drives)
- Many template files to validate
- System under heavy load

---

## Error Code Summary Table

| Code  | Name                      | Severity | Default Behavior | Configurable |
|-------|---------------------------|----------|------------------|--------------|
| E0020 | Insufficient Disk Space   | Error    | Blocks execution | No           |
| E0021 | Insufficient Permissions  | Error    | Blocks execution | No           |
| E0022 | LLM Provider Unreachable  | Warning  | Logs warning     | Yes          |
| E0023 | Manifest Syntax Error     | Error    | Blocks execution | No           |
| E0024 | Template Syntax Error     | Error    | Blocks execution | Yes          |
| E0025 | Missing Dependency        | Warning  | Logs warning     | Yes          |
| E0026 | Invalid Output Directory  | Error    | Blocks execution | No           |
| E0027 | Network Connectivity      | Error    | Blocks execution | Yes          |
| E0028 | File System Error         | Error    | Blocks execution | No           |
| E0029 | Pre-flight Timeout        | Error    | Blocks execution | Yes          |

## Usage Examples

### Basic Sync with Pre-flight
```bash
# Pre-flight runs automatically
ggen sync

# Verbose mode shows pre-flight checks
ggen sync --verbose
```

### Init with Pre-flight
```bash
# Pre-flight checks disk space and permissions
ggen init

# Force init (skips artifact check, but not pre-flight)
ggen init --force
```

### Disable Optional Checks
```rust
let validator = PreFlightValidator::for_sync(path)
    .with_llm_check(false)  // Disable LLM check
    .with_template_check(false)  // Disable template check
    .with_git_check(false);  // Disable git check
```

### Custom Validation
```rust
// Validate with manifest
let result = validator.validate(Some(&manifest))?;

// Check result
if result.is_success() {
    println!("All {} checks passed", result.passed_checks.len());
}

// Access warnings (non-blocking issues)
for warning in &result.warnings {
    eprintln!("Warning: {}", warning);
}
```

## Environment Variables

### LLM Provider Configuration
- `GGEN_LLM_PROVIDER`: Provider name (ollama, openai, anthropic, mock)
- `OLLAMA_BASE_URL`: Ollama server URL (default: http://localhost:11434)
- `OPENAI_API_KEY`: OpenAI API key
- `ANTHROPIC_API_KEY`: Anthropic API key

### Disk Space Configuration
*Currently hardcoded to 100MB, future enhancement for configurable threshold*

## Future Enhancements

1. **Configurable Thresholds**
   - Disk space minimum via env var
   - Per-check timeouts
   - Warning vs. error levels

2. **Parallel Execution**
   - Run independent checks concurrently
   - Reduce total validation time

3. **Caching**
   - Cache LLM health status (TTL: 60s)
   - Skip recent successful checks

4. **Detailed Reporting**
   - JSON output for CI/CD
   - HTML reports
   - Metrics collection

5. **Additional Checks**
   - Docker availability
   - Node.js/Python versions
   - Database connectivity
   - Cloud provider credentials
