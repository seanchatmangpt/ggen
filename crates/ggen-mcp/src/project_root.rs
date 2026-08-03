//! Resolve a tool's `root` parameter to a real, traversal-safe project
//! directory. Every tool that accepts `root` routes through this -- never a
//! naive `PathBuf::from(root)` or `starts_with` string check.

use std::path::{Path, PathBuf};

use crate::error::{ErrorCategory, McpError};
use crate::limits::MAX_PATH_BYTES;

/// Resolve `root` to a canonical, existing directory.
///
/// This intentionally reuses `ggen_engine::write::resolve_target`'s
/// canonicalize/no-traversal/symlink-safe check rather than reimplementing
/// it -- `root` here plays the same role `to:` plays there (a
/// caller-supplied relative path that must not escape a trusted base), just
/// with the roles of "base" and "candidate" reversed: we resolve `root`
/// itself against the current working directory, then require it exists
/// and is a directory.
///
/// # Errors
/// `ErrorCategory::InputTooLarge` if `root` exceeds `MAX_PATH_BYTES`.
/// `ErrorCategory::PathTraversal` if `root` cannot be canonicalized or does
/// not resolve to an existing directory.
pub fn resolve_root(root: &str) -> Result<PathBuf, McpError> {
    if root.len() > MAX_PATH_BYTES {
        return Err(McpError::new(
            ErrorCategory::InputTooLarge,
            format!("root path exceeds {MAX_PATH_BYTES} bytes"),
        ));
    }
    let candidate = Path::new(root);
    let canonical = candidate.canonicalize().map_err(|e| {
        McpError::new(
            ErrorCategory::PathTraversal,
            format!("root {root:?} could not be canonicalized: {e}"),
        )
    })?;
    if !canonical.is_dir() {
        return Err(McpError::new(
            ErrorCategory::PathTraversal,
            format!("root {root:?} does not resolve to a directory"),
        ));
    }
    Ok(canonical)
}

/// Resolve `rel` (a path the caller claims is relative to `root`, e.g. a
/// `template_path` parameter) safely under the already-resolved `root`.
/// Delegates directly to `ggen_engine::write::resolve_target` -- the exact
/// function `to:`/`from:` frontmatter resolution already uses -- so this
/// crate can never drift from the CLI's own traversal-safety guarantee.
///
/// # Errors
/// Propagates `resolve_target`'s refusal as `ErrorCategory::PathTraversal`.
pub fn resolve_relative(root: &Path, rel: &str) -> Result<PathBuf, McpError> {
    ggen_engine::write::resolve_target(root, rel)
        .map_err(|e| McpError::new(ErrorCategory::PathTraversal, e.to_string()))
}
