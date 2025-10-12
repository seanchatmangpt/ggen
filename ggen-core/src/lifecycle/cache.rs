//! Deterministic cache key generation

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
