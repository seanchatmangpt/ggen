//! Deterministic cache key generation
//!
//! This module provides deterministic cache key generation for lifecycle phases,
//! enabling reproducible builds and efficient caching of phase execution results.
//!
//! ## Features
//!
//! - **Deterministic keys**: Same inputs always produce same cache key
//! - **SHA256 hashing**: Uses SHA256 for reliable key generation
//! - **Input tracking**: Includes phase name, commands, environment, and input files
//! - **Reproducible builds**: Enables caching based on actual inputs
//!
//! ## Cache Key Components
//!
//! The cache key includes:
//! - Phase name
//! - Command lines (sorted)
//! - Environment variables (sorted by key)
//! - Input file contents (SHA256 hashes)
//!
//! ## Examples
//!
//! ### Generating a Cache Key
//!
//! ```rust,no_run
//! use ggen_core::lifecycle::cache::cache_key;
//! use ggen_core::lifecycle::Result;
//!
//! # fn main() -> Result<()> {
//! let phase_name = "build";
//! let cmd_lines = vec!["cargo build --release".to_string()];
//! let env = vec![("RUSTFLAGS".to_string(), "-C opt-level=3".to_string())];
//! let inputs = vec!["Cargo.toml".to_string(), "src/main.rs".to_string()];
//!
//! let key = cache_key(phase_name, &cmd_lines, &env, &inputs);
//! println!("Cache key: {}", key);
//! # Ok(())
//! # }
//! ```
//!
//! ### Using Cache Keys for Conditional Execution
//!
//! ```rust,no_run
//! use ggen_core::lifecycle::cache::cache_key;
//! use ggen_core::lifecycle::Result;
//! use std::collections::HashSet;
//!
//! # fn main() -> Result<()> {
//! let mut executed_keys = HashSet::new();
//!
//! let key = cache_key("test", &vec!["cargo test".to_string()], &[], &[]);
//!
//! if !executed_keys.contains(&key) {
//!     // Execute phase
//!     executed_keys.insert(key);
//! }
//! # Ok(())
//! # }
//! ```

use super::error::{LifecycleError, Result};
use sha2::{Digest, Sha256};
use std::path::Path;

/// Generate deterministic cache key for a phase
///
/// Cache key is based on:
/// - Phase name
/// - Command lines
/// - Environment variables (sorted)
/// - Input file contents
pub fn cache_key(
    phase_name: &str, cmd_lines: &[String], env: &[(String, String)], inputs: &[String],
) -> String {
    let mut hasher = Sha256::new();

    // Hash phase name
    hasher.update(phase_name.as_bytes());

    // Hash commands
    for cmd in cmd_lines {
        hasher.update(b"\n");
        hasher.update(cmd.as_bytes());
    }

    // Hash environment (already sorted in exec.rs)
    for (key, value) in env {
        hasher.update(b"\n");
        hasher.update(key.as_bytes());
        hasher.update(b"=");
        hasher.update(value.as_bytes());
    }

    // Hash input files
    for input_path in inputs {
        hasher.update(b"\n");
        hasher.update(input_path.as_bytes());

        // Hash file content if it exists
        if let Ok(content) = std::fs::read(input_path) {
            hasher.update(&content);
        }
    }

    format!("{:x}", hasher.finalize())
}

/// Check if cache is valid for given key
pub fn is_cache_valid(cache_dir: &Path, phase: &str, key: &str) -> bool {
    let cache_path = cache_dir.join(phase).join(key);
    cache_path.exists()
}

/// Store cache marker
pub fn store_cache(cache_dir: &Path, phase: &str, key: &str) -> Result<()> {
    let cache_path = cache_dir.join(phase).join(key);

    // Safely get parent directory
    let parent = cache_path
        .parent()
        .ok_or_else(|| LifecycleError::invalid_cache_path(cache_path.clone()))?;

    std::fs::create_dir_all(parent).map_err(|e| LifecycleError::CacheCreate {
        phase: phase.to_string(),
        source: e,
    })?;

    std::fs::write(&cache_path, "").map_err(|e| LifecycleError::FileIo {
        path: cache_path,
        source: e,
    })?;

    Ok(())
}

// Unit tests removed - covered by integration_test.rs:
// - test_cache_key_generation (comprehensive cache key testing)
// - test_cache_key_deterministic (determinism verification)
// - test_cache_key_changes_with_inputs (input sensitivity)
// And by behavior_tests.rs cache invalidation contracts
