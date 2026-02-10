# ggen-canonical

Deterministic canonicalization system for ggen v6.0.0

## Overview

Provides deterministic canonicalization for multiple formats ensuring reproducible hashes across:
- **Rust code** - via rustfmt integration
- **JSON** - sorted keys, consistent formatting
- **RDF/TTL** - canonical sorted triples
- **Hash computation** - SHA-256 for verification

## Formula

```
C(x) = canonical(x) where hash(C(x)) is deterministic
```

Same input always produces same canonical form and same hash.

## Architecture

### Core Trait

```rust
pub trait Canonicalizer {
    type Input;
    type Output;

    fn canonicalize(&self, input: Self::Input) -> Result<Self::Output>;
    fn hash(&self, input: Self::Input) -> Result<String>;
}
```

### Modules

- `hash` - SHA-256 hash computation
- `json` - JSON canonicalization (sorted keys)
- `rust` - Rust code formatting (rustfmt)
- `ttl` - RDF/TTL normalization (sorted lines)

## Usage

### JSON Canonicalization

```rust
use ggen_canonical::json::JsonCanonicalizer;
use ggen_canonical::Canonicalizer;
use serde_json::json;

let data = json!({"z": 1, "a": 2});
let canon = JsonCanonicalizer::new();
let result = canon.canonicalize(data)?;
// Always produces: {"a":2,"z":1}
```

### Rust Code Canonicalization

```rust
use ggen_canonical::rust::RustCanonicalizer;
use ggen_canonical::Canonicalizer;

let code = "fn main(){println!(\"hello\");}";
let canon = RustCanonicalizer::new();
let formatted = canon.canonicalize(code.to_string())?;
// Produces consistently formatted Rust code
```

### Hash Computation

```rust
use ggen_canonical::hash;

let data = b"test data";
let hash1 = hash::compute_hash(&data)?;
let hash2 = hash::compute_hash(&data)?;
assert_eq!(hash1, hash2); // Always equal
```

### Hash Verification

```rust
use ggen_canonical::hash::HashVerifier;

let data = b"test";
let hash = hash::compute_hash(&data)?;
let verifier = HashVerifier::new(hash);
assert!(verifier.verify(&data).is_ok());
```

## Features

### Determinism Guarantees

- **Idempotent**: `C(C(x)) = C(x)`
- **Stable**: Same input → same output
- **Ordered**: JSON keys sorted, TTL lines sorted
- **Verified**: All outputs hashable for verification

### Test Coverage

- Unit tests per module
- Integration tests for determinism
- Property-based tests (proptest)
- 37 tests total (all passing)

## Performance

- JSON: O(n log n) for sorting
- Rust: Depends on rustfmt (fallback to normalization)
- TTL: O(n log n) for line sorting
- Hash: O(n) for SHA-256

## Dependencies

- `serde` + `serde_json` - JSON handling
- `toml` - TOML support
- `sha2` + `hex` - Cryptographic hashing
- `thiserror` - Error handling
- `oxigraph` - RDF/TTL (for future enhancements)

## Examples

### Complete Workflow

```rust
use ggen_canonical::json::{JsonCanonicalizer, canonicalize_json_str};
use ggen_canonical::hash;
use serde_json::json;

// Canonicalize JSON
let input = r#"{"project":"ggen","version":"6.0.0"}"#;
let canonical = canonicalize_json_str(input)?;

// Compute hash
let hash = hash::compute_hash(&canonical)?;

// Verify
let verifier = hash::HashVerifier::new(hash.clone());
assert!(verifier.verify(&canonical).is_ok());
```

## Error Handling

All operations return `Result<T, CanonicalError>`:

```rust
pub enum CanonicalError {
    Format(String),      // Format-specific error
    Io(std::io::Error),  // IO error
    Serialization(String), // Serialization error
    Hash(String),        // Hash computation error
    Invalid(String),     // Invalid input
}
```

## Testing

```bash
cargo test                    # Run all tests
cargo test --quiet            # Run tests quietly
cargo test determinism_tests  # Run determinism tests only
```

## Integration

Add to your `Cargo.toml`:

```toml
[dependencies]
ggen-canonical = { path = "crates/ggen-canonical", version = "0.2.0" }
```

## License

MIT

## Version

0.2.0 (2026-02-10)
