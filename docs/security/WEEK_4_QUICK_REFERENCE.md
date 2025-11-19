# Week 4 Security Hardening - Quick Reference

## 🚀 Quick Start

### Import Security Modules

```rust
use ggen_core::security::command::{SafeCommand, CommandExecutor};
use ggen_core::security::validation::{PathValidator, EnvVarValidator, InputValidator};
use ggen_core::security::error::{ErrorSanitizer, SanitizedError};
```

---

## 🔒 Common Patterns

### 1. Safe Command Execution

```rust
// ✅ DO: Use SafeCommand
let output = SafeCommand::new("git")?
    .arg("init")?
    .current_dir(path)?
    .execute()?;

// ❌ DON'T: Use raw Command with shell
Command::new("sh").arg("-c").arg(user_input)  // VULNERABLE!
```

### 2. Path Validation

```rust
// ✅ DO: Validate before use
let safe_path = PathValidator::validate(user_path)?;

// ✅ DO: Check within directory
let safe_path = PathValidator::validate_within(user_path, base_dir)?;

// ❌ DON'T: Use paths directly
fs::read_to_string(user_input)  // VULNERABLE!
```

### 3. Environment Variable Validation

```rust
// ✅ DO: Validate name and value
let safe_name = EnvVarValidator::validate_name(name)?;
let safe_value = EnvVarValidator::validate_value(value)?;
env::set_var(safe_name, safe_value);

// ❌ DON'T: Set directly
env::set_var(user_name, user_value)  // VULNERABLE!
```

### 4. Error Sanitization

```rust
// ✅ DO: Sanitize errors for users
let err = ErrorSanitizer::file_error("read", path, error);
eprintln!("{}", err.user_message());  // Safe for users
log::error!("{}", err.internal_message());  // Full details for logs

// ❌ DON'T: Expose internal errors
eprintln!("Error: {}", internal_error)  // Leaks paths!
```

---

## 📋 Cheat Sheet

### SafeCommand Whitelist

Allowed commands:
- `git`
- `cargo`
- `npm`
- `node`
- `rustc`
- `rustup`

### Blocked Metacharacters

`;` `|` `&` `$` `` ` `` `\n` `\r` `<` `>` `(` `)` `{` `}`

### Validation Limits

- Path: 4096 characters
- Env var: 32768 characters
- Error message: 200 characters

---

## 🧪 Testing Your Code

### Test Command Injection Prevention

```rust
#[test]
fn test_command_safety() {
    // Should block injection
    assert!(SafeCommand::new("git")?
        .arg("init; rm -rf /")
        .is_err());
}
```

### Test Path Traversal Prevention

```rust
#[test]
fn test_path_safety() {
    // Should block traversal
    assert!(PathValidator::validate(
        Path::new("../../../etc/passwd")
    ).is_err());
}
```

---

## ⚠️ Common Mistakes

### Mistake 1: Using `.unwrap()` in library code

```rust
// ❌ BAD
let value = option.unwrap();

// ✅ GOOD
let value = option.ok_or_else(|| Error::new("Missing value"))?;
```

### Mistake 2: Forgetting to validate user input

```rust
// ❌ BAD
fs::write(user_path, content)?;

// ✅ GOOD
let safe_path = PathValidator::validate(user_path)?;
fs::write(&safe_path, content)?;
```

### Mistake 3: Exposing full error messages

```rust
// ❌ BAD
return Err(format!("Failed to read {}: {}", path.display(), err));

// ✅ GOOD
ErrorSanitizer::file_error("read", path, &err.to_string())
```

---

## 🎯 Security Checklist

Before merging code, ensure:

- [ ] No `panic!()` in library code
- [ ] No `.unwrap()` in critical paths
- [ ] All user inputs validated
- [ ] Commands use `SafeCommand`
- [ ] Paths validated with `PathValidator`
- [ ] Errors sanitized for users
- [ ] Tests cover security scenarios
- [ ] No hardcoded secrets or credentials

---

## 📚 Documentation

Full documentation:
- `WEEK_4_SECURITY_HARDENING_REPORT.md` - Complete report
- `WEEK_4_IMPLEMENTATION_SUMMARY.md` - Implementation details
- `tests/security/week4_security_hardening_tests.rs` - Test examples

---

**Quick Tip**: When in doubt, validate first, fail securely, and sanitize errors!
