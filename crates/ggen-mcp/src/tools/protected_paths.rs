//! Rust port of `.claude/hooks/pre_tool_use_guard.sh`'s `is_protected()` +
//! `under_workspace_root()` (Gall CP23's fix), used by the bounded
//! unattended-write dispatcher (CP31-CP33) so a narrow, pre-declared-safe
//! write class can never land on a path that repo's own protective hook
//! would refuse to let an agent touch directly.
//!
//! **Named finding, not silently accepted**: this is a second, independent
//! implementation of "protected path," not a shared one with the bash hook
//! -- the same class of drift risk `CLAUDE.md` already documents for
//! E0010/E0011/E0013 (author-time `ggen-lsp` analyzer vs. sync-time hard
//! error in `ggen-config`, two implementations, never unified). Acceptable
//! here because the bash hook and this dispatcher run in different
//! processes (a `PreToolUse` hook has no path to be called from Rust code,
//! and vice versa) with no natural single-source option -- but a real gap
//! worth a follow-up ticket if either list changes, not a silent one.

use std::path::{Path, PathBuf};

/// Mirrors `pre_tool_use_guard.sh`'s `PROTECTED_GLOB`/`is_protected()`
/// literally: a generated-output file that must only change via `ggen sync`,
/// plus the three directories `ggen sync`/key-rotation/git-hook-install own.
const PROTECTED_SUFFIXES: &[&str] = &[
    "crates/ggen-cli/src/generated_commands.rs",
    ".ggen/receipts/",
    ".ggen/keys/",
    ".git/hooks/",
];

/// True iff `rel` (relative to `root`, or already absolute) resolves to a
/// path both (a) matching one of `PROTECTED_SUFFIXES` and (b) actually
/// inside `root` once normalized -- mirrors CP23's `under_workspace_root`
/// fix: a same-named path in a different project must never be refused just
/// because it shares this repo's own `.ggen/keys/`-style naming convention.
pub fn is_protected_path(root: &Path, rel: &Path) -> bool {
    let candidate = if rel.is_absolute() {
        rel.to_path_buf()
    } else {
        root.join(rel)
    };
    let normalized = normalize(&candidate);
    let Ok(root_normalized) = root.canonicalize() else {
        return false;
    };
    if !normalized.starts_with(&root_normalized) {
        return false;
    }
    let normalized_str = normalized.to_string_lossy();
    PROTECTED_SUFFIXES
        .iter()
        .any(|suffix| normalized_str.contains(suffix))
}

/// Resolve symlinks/`./`/`../` without requiring the final component to
/// exist (an unattended-write candidate is, by CP31's own `unless_exists`
/// precondition, always a file that does NOT yet exist) -- falls back to the
/// unresolved path if canonicalization fails partway (e.g. a missing parent
/// directory that will be created by the write itself).
fn normalize(path: &Path) -> PathBuf {
    if let Ok(canon) = path.canonicalize() {
        return canon;
    }
    // Walk up to the nearest existing ancestor, canonicalize that, then
    // re-append the non-existent suffix -- covers the common case of a
    // brand-new file inside an existing directory tree.
    let mut existing = path.to_path_buf();
    let mut suffix = PathBuf::new();
    while !existing.exists() {
        let Some(parent) = existing.parent() else {
            return path.to_path_buf();
        };
        if let Some(name) = existing.file_name() {
            let mut new_suffix = PathBuf::from(name);
            new_suffix.push(&suffix);
            suffix = new_suffix;
        }
        existing = parent.to_path_buf();
    }
    match existing.canonicalize() {
        Ok(canon) => canon.join(suffix),
        Err(_) => path.to_path_buf(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    /// Mirrors CP23's manual test case 1: a genuinely protected path under
    /// root must still be refused.
    #[test]
    fn protected_path_under_root_is_refused() {
        let tmp = TempDir::new().expect("tempdir");
        std::fs::create_dir_all(tmp.path().join(".ggen/keys")).expect("mkdir");
        assert!(is_protected_path(
            tmp.path(),
            Path::new(".ggen/keys/signing.key")
        ));
    }

    /// Mirrors CP23's manual test case 2/3: the same-named path resolving
    /// OUTSIDE root (a sibling project reusing the same naming convention)
    /// must not be refused by this root's check.
    #[test]
    fn same_named_path_outside_root_is_not_protected() {
        let root = TempDir::new().expect("tempdir");
        let other = TempDir::new().expect("tempdir");
        std::fs::create_dir_all(other.path().join(".ggen/keys")).expect("mkdir");
        let outside_key = other.path().join(".ggen/keys/signing.key");
        std::fs::write(&outside_key, b"fake key").expect("write");
        assert!(!is_protected_path(root.path(), &outside_key));
    }

    /// An ordinary generated file under root, matching none of the
    /// protected suffixes, is not protected.
    #[test]
    fn ordinary_file_under_root_is_not_protected() {
        let tmp = TempDir::new().expect("tempdir");
        assert!(!is_protected_path(tmp.path(), Path::new("src/lib.rs")));
    }

    /// A brand-new file under an already-existing directory (the common
    /// unattended-write case, since `unless_exists` guarantees the target
    /// never exists yet) still resolves and checks correctly.
    #[test]
    fn new_file_under_existing_dir_resolves_correctly() {
        let tmp = TempDir::new().expect("tempdir");
        std::fs::create_dir_all(tmp.path().join("crates/ggen-cli/src")).expect("mkdir");
        assert!(is_protected_path(
            tmp.path(),
            Path::new("crates/ggen-cli/src/generated_commands.rs")
        ));
        assert!(!is_protected_path(
            tmp.path(),
            Path::new("crates/ggen-cli/src/not_generated.rs")
        ));
    }
}
