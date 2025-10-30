# Feature Development Guide

## Adding New Features to Ggen Marketplace

This guide covers how to properly add and test new features in the Ggen Marketplace codebase.

## Feature Flag Architecture

### Current Features

```toml
[features]
default = []                                          # Minimal dependencies
p2p = ["libp2p"]                                     # P2P networking
graphql = ["async-graphql", "async-graphql-axum"]   # GraphQL API
graphql-server = ["graphql", "axum", "tower", ...]  # Full GraphQL server
crypto = ["ed25519-dalek", "rand"]                   # Cryptographic signing
all = ["p2p", "graphql-server", "crypto"]           # Everything enabled
```

### Design Principles

1. **Minimal Default:** Keep `default = []` lightweight
2. **Composable Features:** Features should work independently
3. **Optional Dependencies:** Use `optional = true` for feature-specific deps
4. **Clear Naming:** Feature names should be descriptive

## Adding a New Feature

### Step-by-Step Process

#### 1. Update Cargo.toml

```toml
[dependencies]
# Add dependency with optional = true
new-feature-crate = { version = "1.0", optional = true }

[features]
# Add feature flag
new-feature = ["new-feature-crate"]
all = ["p2p", "graphql-server", "crypto", "new-feature"]
```

#### 2. Conditional Compilation

Use `#[cfg(feature = "...")]` for feature-gated code:

```rust
// In src/features/mod.rs
#[cfg(feature = "new-feature")]
pub mod new_feature;

// In src/lib.rs
#[cfg(feature = "new-feature")]
pub use features::new_feature;
```

#### 3. Write Feature Tests

```rust
// In src/features/new_feature.rs
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_feature() {
        // Test implementation
    }

    #[tokio::test]
    async fn test_async_new_feature() {
        // Async test
    }
}
```

#### 4. Update CI/CD Pipeline

Add feature to test matrix in `.github/workflows/ci.yml`:

```yaml
matrix:
  features:
    - ''
    - 'p2p'
    - 'graphql'
    - 'crypto'
    - 'new-feature'  # Add here
    - 'all'
```

#### 5. Add Smoke Test (if needed)

For complex features, add dedicated smoke test job:

```yaml
new-feature-smoke-test:
  name: New Feature Smoke Test
  runs-on: ubuntu-latest
  needs: test
  if: contains(github.event.head_commit.message, '[new-feature]') || github.ref == 'refs/heads/main'
  steps:
    - uses: actions/checkout@v4
    - name: Install Rust
      uses: dtolnay/rust-toolchain@stable
    - name: Build feature
      run: cargo build --features new-feature
    - name: Run tests
      run: cargo test --features new-feature new_feature --lib
```

#### 6. Update Build Verification Script

Add to `scripts/verify-build.sh`:

```bash
# Build with new-feature
print_step "Building with new-feature feature..."
cargo build --verbose --features new-feature
print_success "New feature build complete"
echo ""
```

#### 7. Document the Feature

Add to `docs/FEATURES.md`:

```markdown
### New Feature

**Status:** ✅ Stable / ⚠️ Beta / 🚧 Experimental

**Description:** What the feature does

**Usage:**
\`\`\`rust
use ggen_marketplace::new_feature;

// Example code
\`\`\`

**Dependencies:**
- `new-feature-crate` v1.0

**Enable:**
\`\`\`toml
[dependencies]
ggen-marketplace = { version = "0.1", features = ["new-feature"] }
\`\`\`
```

## Feature Development Checklist

### Pre-Development

- [ ] Feature design reviewed
- [ ] Dependencies identified
- [ ] Feature flag name chosen
- [ ] Breaking changes assessed

### Development

- [ ] Dependencies added as optional
- [ ] Feature flag added to `[features]`
- [ ] Feature flag added to `all` feature
- [ ] Code gated with `#[cfg(feature = "...")]`
- [ ] Tests written and passing
- [ ] Documentation comments added
- [ ] Examples created

### CI/CD Integration

- [ ] Feature added to test matrix
- [ ] Smoke tests added (if complex)
- [ ] Build verification updated
- [ ] Trigger conditions configured

### Documentation

- [ ] Feature documented in `docs/FEATURES.md`
- [ ] Usage examples provided
- [ ] Migration guide (if breaking changes)
- [ ] CHANGELOG.md updated

### Testing

- [ ] Unit tests pass: `cargo test --features new-feature`
- [ ] Integration tests pass: `cargo test --all-features`
- [ ] Clippy clean: `cargo clippy --features new-feature`
- [ ] Documentation builds: `cargo doc --features new-feature`
- [ ] Build verification passes: `./scripts/verify-build.sh`

## Feature Stability Levels

### Experimental (🚧)

- API may change significantly
- Not recommended for production
- Breaking changes expected

**Example:**
```toml
new-feature = ["new-feature-crate"]  # 🚧 Experimental
```

### Beta (⚠️)

- API mostly stable
- Minor breaking changes possible
- Production use with caution

**Example:**
```toml
new-feature = ["new-feature-crate"]  # ⚠️ Beta - API may change
```

### Stable (✅)

- API stable, breaking changes follow SemVer
- Recommended for production
- Full test coverage

**Example:**
```toml
new-feature = ["new-feature-crate"]  # ✅ Stable
```

## Feature Interaction Testing

### Testing Feature Combinations

```bash
# Test feature alone
cargo test --no-default-features --features new-feature

# Test with one other feature
cargo test --no-default-features --features "new-feature,p2p"

# Test with multiple features
cargo test --no-default-features --features "new-feature,p2p,graphql"

# Test with all features
cargo test --all-features
```

### Common Interaction Issues

**Problem:** Features conflict at compile time

**Solution:**
```rust
#[cfg(all(feature = "feature-a", feature = "feature-b"))]
compile_error!("feature-a and feature-b cannot be used together");
```

**Problem:** Features have overlapping dependencies

**Solution:** Use feature unification in Cargo.toml:
```toml
shared-dep = { version = "1.0", optional = true }
feature-a = ["shared-dep"]
feature-b = ["shared-dep"]
```

## Performance Considerations

### Compile Time

- **Minimize dependencies:** Each feature adds compilation time
- **Optional deps:** Only compile what's needed
- **Feature gates:** Keep feature code isolated

### Runtime

- **Zero-cost abstractions:** Feature flags are compile-time only
- **Dead code elimination:** Unused features don't impact binary size
- **Monomorphization:** Generic code specialized per feature

### Binary Size

Check binary size impact:
```bash
# Default features
cargo build --release
ls -lh target/release/ggen-marketplace

# With new feature
cargo build --release --features new-feature
ls -lh target/release/ggen-marketplace
```

## Deprecating Features

### Deprecation Process

1. **Announce deprecation** (version N)
   ```rust
   #[deprecated(since = "0.2.0", note = "Use new-feature instead")]
   #[cfg(feature = "old-feature")]
   pub mod old_feature;
   ```

2. **Update documentation** (version N)
   - Add migration guide
   - Update examples
   - Add deprecation notices

3. **Remove feature** (version N+1 or next major)
   - Remove from `Cargo.toml`
   - Remove gated code
   - Update CHANGELOG.md

## Testing Best Practices

### Unit Tests

```rust
#[cfg(test)]
mod tests {
    use super::*;

    // Test feature functionality
    #[test]
    fn test_core_functionality() {
        // ...
    }

    // Test error handling
    #[test]
    fn test_error_cases() {
        // ...
    }

    // Test edge cases
    #[test]
    fn test_edge_cases() {
        // ...
    }
}
```

### Integration Tests

```rust
// tests/integration_new_feature.rs
#![cfg(feature = "new-feature")]

use ggen_marketplace::new_feature;

#[tokio::test]
async fn test_new_feature_integration() {
    // Test feature with real dependencies
}
```

### Smoke Tests

Quick validation tests for complex features:

```rust
#[test]
fn smoke_test_new_feature() {
    // Basic "does it work" test
    let instance = new_feature::create();
    assert!(instance.is_valid());
}
```

## Examples

### P2P Feature Example

```rust
// src/p2p/mod.rs
#[cfg(feature = "p2p")]
use libp2p::*;

#[cfg(feature = "p2p")]
pub struct P2PNetwork {
    // Implementation
}

#[cfg(feature = "p2p")]
impl P2PNetwork {
    pub fn new() -> Result<Self> {
        // ...
    }
}

// src/lib.rs
#[cfg(feature = "p2p")]
pub use p2p::P2PNetwork;
```

### GraphQL Feature Example

```rust
// src/graphql/mod.rs
#[cfg(feature = "graphql")]
use async_graphql::*;

#[cfg(feature = "graphql")]
pub struct MarketplaceSchema {
    // Implementation
}

#[cfg(feature = "graphql")]
impl MarketplaceSchema {
    pub async fn execute_query(&self, query: &str) -> Result<Response> {
        // ...
    }
}
```

## Common Pitfalls

### ❌ Don't: Always use `expect()` or `unwrap()`

```rust
// BAD - Crashes in production
let result = operation().expect("Failed");
```

### ✅ Do: Handle errors properly

```rust
// GOOD - Graceful error handling
let result = operation()
    .map_err(|e| anyhow::anyhow!("Operation failed: {}", e))?;
```

### ❌ Don't: Forget feature gates

```rust
// BAD - Won't compile without feature
use libp2p::*;  // No feature gate!
```

### ✅ Do: Use feature gates

```rust
// GOOD - Only compiles with feature
#[cfg(feature = "p2p")]
use libp2p::*;
```

### ❌ Don't: Create circular feature dependencies

```toml
# BAD
feature-a = ["feature-b"]
feature-b = ["feature-a"]
```

### ✅ Do: Keep features independent

```toml
# GOOD
feature-a = ["dep-a"]
feature-b = ["dep-b"]
combined = ["feature-a", "feature-b"]
```

## Resources

- [Cargo Features Documentation](https://doc.rust-lang.org/cargo/reference/features.html)
- [Conditional Compilation](https://doc.rust-lang.org/reference/conditional-compilation.html)
- [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/)

---

**Last Updated:** 2025-10-13
**Version:** 1.0.0
