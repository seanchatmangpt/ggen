# User Authentication - Traditional Approach

## Overview

Hand-written Rust authentication service with:
- User registration with email/password validation
- Secure password hashing using bcrypt
- Session management with token generation
- Comprehensive error handling
- Unit tests covering main scenarios

## Files

- `types.rs` - User and Session data structures (52 LOC)
- `errors.rs` - AuthError enum with Display impl (50 LOC)
- `auth.rs` - AuthService with all business logic (147 LOC)
- `auth_test.rs` - Test suite with 11 test cases (98 LOC)
- `README.md` - This documentation (50 LOC)

**Total**: 397 LOC (347 LOC code + 50 LOC docs)

## Usage

```rust
use auth::{AuthService, User, Session, AuthError};

fn main() -> Result<(), AuthError> {
    let mut auth = AuthService::new();

    // Register user
    let user = auth.register(
        "alice@example.com".to_string(),
        "secure_pass123".to_string()
    )?;

    // Login
    let session = auth.login(
        "alice@example.com".to_string(),
        "secure_pass123".to_string()
    )?;

    // Validate session
    let validated_user = auth.validate_session(&session.token)?;
    println!("Logged in as: {}", validated_user.email);

    // Logout
    auth.logout(&session.token)?;

    Ok(())
}
```

## Design Decisions

### Password Validation
- Minimum 8 characters
- Must contain at least one number
- Could be extended with more rules

### Session Management
- 24-hour expiration
- Simple token format: `{user_id}_{timestamp}`
- In production, use cryptographically secure tokens

### Error Handling
- Custom `AuthError` enum for type-safe errors
- Descriptive error messages for debugging
- `Result<T, AuthError>` for all fallible operations

### Testing Strategy
- 11 test cases covering:
  - Happy path (register, login, logout)
  - Validation errors (email, password strength)
  - Business logic errors (duplicate user, wrong password)
  - Session management (validate, expire)

## Known Limitations

1. **In-memory storage**: Data lost on restart
   - Production: Use database (PostgreSQL, Redis)

2. **Simple token generation**: Not cryptographically secure
   - Production: Use UUID v4 or JWT

3. **Basic email validation**: Only checks for @ and .
   - Production: Use email-validator crate or regex

4. **No password reset**: Forgot password not implemented
   - Production: Add email-based password reset flow

5. **No rate limiting**: Vulnerable to brute-force attacks
   - Production: Add rate limiting middleware

6. **Documentation drift**: README manually maintained
   - Risk: Code changes → outdated docs

## Maintenance Burden

### When Adding New Features
1. Update types in `types.rs`
2. Update error variants in `errors.rs`
3. Update business logic in `auth.rs`
4. Add tests in `auth_test.rs`
5. Update documentation in `README.md`
6. Ensure consistency across all files

**High risk of inconsistency** - each file must be updated manually.

### When Refactoring
1. Touch every file
2. Ensure tests still pass
3. Update documentation
4. Review for cascading changes

**Time-consuming** - changes propagate across multiple files.

## Comparison to RDF-First

See `../rdf-first/` for the same feature implemented using:
- Single RDF ontology (89 LOC)
- Reusable templates (101 LOC)
- Auto-generated code + tests + docs

**Key Differences**:
- Traditional: 347 LOC to maintain, 5 files
- RDF-First: 190 LOC to maintain, 1 ontology + 3 templates
- Traditional: Manual synchronization required
- RDF-First: Automatic synchronization via `ggen sync`
- Traditional: Documentation drift likely
- RDF-First: Documentation drift impossible

## Running Tests

```bash
# Compile
rustc --edition 2021 --crate-type lib types.rs errors.rs auth.rs

# Run tests
cargo test --test auth_test

# Expected output:
# running 11 tests
# test tests::test_register_success ... ok
# test tests::test_register_invalid_email ... ok
# test tests::test_register_weak_password ... ok
# test tests::test_register_duplicate_user ... ok
# test tests::test_login_success ... ok
# test tests::test_login_wrong_password ... ok
# test tests::test_login_user_not_found ... ok
# test tests::test_validate_session_success ... ok
# test tests::test_validate_session_invalid_token ... ok
# test tests::test_logout_success ... ok
# test tests::test_active_sessions_count ... ok
#
# test result: ok. 11 passed; 0 failed; 0 ignored; 0 measured
```

## When to Use This Approach

✅ **Use Traditional When**:
- Learning Rust authentication patterns
- One-off script with no long-term maintenance
- Extreme customization needed
- Team has no RDF experience

❌ **Avoid Traditional When**:
- Feature will evolve frequently
- Need guaranteed consistency between code/tests/docs
- Working with multiple developers
- Compliance requires audit trails

---

**Next**: See `../rdf-first/` for the paradigm shift
