# Path Validation Security Guide

## Overview

The `PathValidator` provides comprehensive path validation to prevent common security vulnerabilities in file operations.

## Attack Vectors Prevented

### 1. Path Traversal

**Attack**: Using `../` to escape workspace and access sensitive files.

```rust
// ❌ BLOCKED
validator.validate("../../../etc/passwd")?;
validator.validate("../../secrets/api_keys.txt")?;

// ✅ ALLOWED
validator.validate("templates/example.tera")?;
validator.validate("src/generated/output.rs")?;
```

### 2. Null Byte Injection

**Attack**: Embedding null bytes to truncate paths or bypass validation.

```rust
// ❌ BLOCKED
validator.validate("safe.txt\0../../etc/passwd")?;
validator.validate("file\0.evil")?;

// ✅ ALLOWED
validator.validate("safe.txt")?;
```

### 3. Absolute Path Escapes

**Attack**: Using absolute paths to access files outside workspace.

```rust
// ❌ BLOCKED (by default)
validator.validate("/etc/passwd")?;
validator.validate("/var/log/secrets")?;

// ✅ ALLOWED (when configured)
let validator = PathValidator::new(workspace)
    .with_absolute_paths(true);
validator.validate(workspace.join("file.txt"))?; // Only within workspace
```

### 4. Symlink Attacks

**Attack**: Creating symlinks that point outside workspace.

```rust
// Setup
ln -s /etc/passwd workspace/evil_link

// ❌ BLOCKED
validator.validate("evil_link")?; // Symlink points outside workspace

// ✅ ALLOWED
ln -s workspace/file.txt workspace/safe_link
validator.validate("safe_link")?; // Symlink stays within workspace
```

### 5. Extension Mismatch

**Attack**: Uploading executable files disguised as templates.

```rust
let validator = PathValidator::new(workspace)
    .with_allowed_extensions(vec!["tera", "tmpl", "ttl"]);

// ❌ BLOCKED
validator.validate("evil.sh")?;
validator.validate("malware.exe")?;

// ✅ ALLOWED
validator.validate("template.tera")?;
validator.validate("schema.ttl")?;
```

### 6. Depth Limit Bypass

**Attack**: Creating deeply nested paths to exhaust resources or bypass checks.

```rust
let validator = PathValidator::new(workspace)
    .with_max_depth(10);

// ❌ BLOCKED (exceeds depth 10)
validator.validate("a/b/c/d/e/f/g/h/i/j/k/l.txt")?;

// ✅ ALLOWED (within depth 10)
validator.validate("a/b/c/d/e/file.txt")?;
```

## Usage Patterns

### Template Loading

```rust
use ggen_utils::path_validator::PathValidator;
use std::path::Path;

fn load_template(template_path: &str) -> Result<String> {
    let workspace = Path::new("/workspace");
    let validator = PathValidator::new(workspace)
        .with_allowed_extensions(vec!["tera", "tmpl"])
        .with_max_depth(5);

    let safe_path = validator.validate(template_path)?;
    Ok(std::fs::read_to_string(safe_path.as_path())?)
}
```

### RDF Ontology Loading

```rust
fn load_ontology(ontology_path: &str) -> Result<String> {
    let workspace = Path::new("/workspace");
    let validator = PathValidator::new(workspace)
        .with_allowed_extensions(vec!["ttl", "rdf", "xml", "n3"])
        .with_max_depth(10);

    let safe_path = validator.validate(ontology_path)?;
    Ok(std::fs::read_to_string(safe_path.as_path())?)
}
```

### Output File Writing

```rust
fn write_generated_code(output_path: &str, content: &str) -> Result<()> {
    let workspace = Path::new("/workspace");
    let validator = PathValidator::new(workspace)
        .with_allowed_extensions(vec!["rs", "ts", "py", "go"])
        .with_max_depth(10);

    let safe_path = validator.validate(output_path)?;

    // Ensure parent directory exists
    if let Some(parent) = safe_path.as_path().parent() {
        std::fs::create_dir_all(parent)?;
    }

    std::fs::write(safe_path.as_path(), content)?;
    Ok(())
}
```

### Batch Validation

```rust
fn validate_all_templates(template_paths: &[&str]) -> Result<()> {
    let workspace = Path::new("/workspace");
    let validator = PathValidator::new(workspace)
        .with_allowed_extensions(vec!["tera", "tmpl"]);

    let safe_paths = validator.validate_batch(template_paths)?;

    for safe_path in safe_paths {
        println!("Template validated: {}", safe_path);
    }

    Ok(())
}
```

## Configuration Options

### Maximum Depth

Prevent deeply nested paths:

```rust
let validator = PathValidator::new(workspace)
    .with_max_depth(10); // Max 10 directory levels
```

### Allowed Extensions

Whitelist file extensions:

```rust
let validator = PathValidator::new(workspace)
    .with_allowed_extensions(vec!["tera", "tmpl", "ttl", "rdf"]);
```

### Absolute Paths

Allow absolute paths (with workspace check):

```rust
let validator = PathValidator::new(workspace)
    .with_absolute_paths(true); // Must still be within workspace
```

### Symlink Following

Control symlink behavior:

```rust
let validator = PathValidator::new(workspace)
    .with_follow_symlinks(false); // Don't follow symlinks
```

## Integration with Existing Code

### Before (Unsafe)

```rust
// ❌ DANGEROUS - No validation
fn load_file(path: &str) -> Result<String> {
    Ok(std::fs::read_to_string(path)?)
}
```

### After (Safe)

```rust
// ✅ SAFE - Full validation
fn load_file(path: &str) -> Result<String> {
    let workspace = get_workspace_root()?;
    let validator = PathValidator::new(&workspace);
    let safe_path = validator.validate(path)?;
    Ok(std::fs::read_to_string(safe_path.as_path())?)
}
```

## Testing

Comprehensive test coverage ensures security:

```bash
# Run path validator tests
cargo test --package ggen-utils path_validator

# Run security-specific tests
cargo test --package ggen-utils --test path_validator_security_tests
```

## Error Handling

All validation errors are detailed and actionable:

```rust
match validator.validate(path) {
    Ok(safe_path) => {
        // Safe to use
        std::fs::read_to_string(safe_path.as_path())?;
    }
    Err(e) => {
        eprintln!("Path validation failed: {}", e);
        // Error messages include:
        // - "Path traversal detected: ../../../etc/passwd contains '..'"
        // - "Path contains null byte: file\0.txt"
        // - "Invalid extension for script.sh, expected one of: tera, tmpl"
        // - "Path depth 15 exceeds maximum 10: a/b/c/d/e/f/g/h/i/j/k/l/m/n/o.txt"
    }
}
```

## Performance

Path validation adds minimal overhead:

- **Validation time**: <1ms for typical paths
- **Memory**: Zero allocation for SafePath (reuses input)
- **Caching**: Canonicalized paths cached internally

## Best Practices

1. **Always validate external input**:
   ```rust
   // User-provided paths
   let user_path = args.template_path; // From CLI
   let safe_path = validator.validate(&user_path)?;
   ```

2. **Use appropriate extensions**:
   ```rust
   // Template loading
   let validator = PathValidator::new(workspace)
       .with_allowed_extensions(vec!["tera", "tmpl"]);

   // RDF loading
   let validator = PathValidator::new(workspace)
       .with_allowed_extensions(vec!["ttl", "rdf", "xml"]);
   ```

3. **Set reasonable depth limits**:
   ```rust
   // Prevent deeply nested paths
   let validator = PathValidator::new(workspace)
       .with_max_depth(10); // Typical max for generated code
   ```

4. **Validate in batch when possible**:
   ```rust
   // More efficient than individual validations
   let safe_paths = validator.validate_batch(&paths)?;
   ```

5. **Use SafePath consistently**:
   ```rust
   fn process_file(safe_path: &SafePath) -> Result<()> {
       // Type system guarantees path was validated
       let content = std::fs::read_to_string(safe_path.as_path())?;
       Ok(())
   }
   ```

## Migration Guide

### Step 1: Identify Unsafe File Operations

```bash
# Find all fs::read operations
grep -r "fs::read" crates/

# Find all fs::write operations
grep -r "fs::write" crates/

# Find all PathBuf::from operations
grep -r "PathBuf::from" crates/
```

### Step 2: Add PathValidator

```rust
// Old code
let content = std::fs::read_to_string(path)?;

// New code
let validator = PathValidator::new(workspace_root);
let safe_path = validator.validate(path)?;
let content = std::fs::read_to_string(safe_path.as_path())?;
```

### Step 3: Configure Appropriately

```rust
// For templates
let template_validator = PathValidator::new(workspace)
    .with_allowed_extensions(vec!["tera", "tmpl"])
    .with_max_depth(5);

// For RDF ontologies
let rdf_validator = PathValidator::new(workspace)
    .with_allowed_extensions(vec!["ttl", "rdf", "xml", "n3"])
    .with_max_depth(10);

// For generated code output
let output_validator = PathValidator::new(workspace)
    .with_allowed_extensions(vec!["rs", "ts", "py", "go", "java"])
    .with_max_depth(10);
```

### Step 4: Update Tests

```rust
#[test]
fn test_template_loading() {
    let workspace = tempdir().unwrap();
    let validator = PathValidator::new(workspace.path());

    // Create test template
    let template_path = workspace.path().join("test.tera");
    std::fs::write(&template_path, "content").unwrap();

    // Validate and load
    let safe_path = validator.validate("test.tera").unwrap();
    let content = std::fs::read_to_string(safe_path.as_path()).unwrap();
    assert_eq!(content, "content");
}
```

## FAQ

### Q: Why not just use `canonicalize()`?

**A**: `canonicalize()` only works for existing files and doesn't validate extensions, depth, or check symlink targets.

### Q: Does this work on Windows?

**A**: Yes, path validation works cross-platform. Path separators are normalized automatically.

### Q: What about performance?

**A**: Validation adds <1ms overhead per path. For batch operations, use `validate_batch()` for better performance.

### Q: Can I disable specific checks?

**A**: Yes, configure the validator:

```rust
let permissive = PathValidator::new(workspace)
    .with_follow_symlinks(false) // Disable symlink following
    // Don't call with_allowed_extensions() to allow all extensions
    // Don't call with_max_depth() to allow any depth
```

### Q: How do I validate multiple file types?

**A**: Create separate validators or use multiple extension lists:

```rust
let template_validator = PathValidator::new(workspace)
    .with_allowed_extensions(vec!["tera", "tmpl"]);

let rdf_validator = PathValidator::new(workspace)
    .with_allowed_extensions(vec!["ttl", "rdf", "xml"]);
```

## Security Audit Checklist

- [ ] All user-provided paths validated
- [ ] Template loading uses extension whitelist
- [ ] RDF loading uses extension whitelist
- [ ] Output paths validated before writing
- [ ] Maximum depth configured appropriately
- [ ] Symlink validation enabled
- [ ] No direct use of `fs::read`, `fs::write` without validation
- [ ] No `PathBuf::from()` on untrusted input
- [ ] Batch validation used where appropriate
- [ ] Error messages don't leak sensitive path information

## References

- [OWASP Path Traversal](https://owasp.org/www-community/attacks/Path_Traversal)
- [CWE-22: Improper Limitation of a Pathname](https://cwe.mitre.org/data/definitions/22.html)
- [Rust Path Security Best Practices](https://doc.rust-lang.org/std/path/index.html)
