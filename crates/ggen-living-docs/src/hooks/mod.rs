//! Git hooks for automatic documentation evolution

use crate::{Error, Result};
use std::path::{Path, PathBuf};
use std::fs;
use tracing::{info, debug};

/// Git hooks manager
pub struct GitHooksManager {
    repo_path: PathBuf,
}

impl GitHooksManager {
    /// Create a new git hooks manager
    pub fn new(repo_path: impl AsRef<Path>) -> Self {
        Self {
            repo_path: repo_path.as_ref().to_path_buf(),
        }
    }

    /// Install git hooks
    pub fn install_hooks(&self) -> Result<()> {
        info!("Installing git hooks");

        let hooks_dir = self.repo_path.join(".git/hooks");
        fs::create_dir_all(&hooks_dir).map_err(Error::Io)?;

        // Install pre-commit hook
        self.install_pre_commit_hook(&hooks_dir)?;

        // Install post-commit hook
        self.install_post_commit_hook(&hooks_dir)?;

        // Install pre-push hook
        self.install_pre_push_hook(&hooks_dir)?;

        info!("Git hooks installed successfully");
        Ok(())
    }

    /// Install pre-commit hook
    fn install_pre_commit_hook(&self, hooks_dir: &Path) -> Result<()> {
        debug!("Installing pre-commit hook");

        let hook_path = hooks_dir.join("pre-commit");
        let hook_content = Self::pre_commit_hook_script();

        fs::write(&hook_path, hook_content).map_err(Error::Io)?;
        Self::make_executable(&hook_path)?;

        Ok(())
    }

    /// Install post-commit hook
    fn install_post_commit_hook(&self, hooks_dir: &Path) -> Result<()> {
        debug!("Installing post-commit hook");

        let hook_path = hooks_dir.join("post-commit");
        let hook_content = Self::post_commit_hook_script();

        fs::write(&hook_path, hook_content).map_err(Error::Io)?;
        Self::make_executable(&hook_path)?;

        Ok(())
    }

    /// Install pre-push hook
    fn install_pre_push_hook(&self, hooks_dir: &Path) -> Result<()> {
        debug!("Installing pre-push hook");

        let hook_path = hooks_dir.join("pre-push");
        let hook_content = Self::pre_push_hook_script();

        fs::write(&hook_path, hook_content).map_err(Error::Io)?;
        Self::make_executable(&hook_path)?;

        Ok(())
    }

    /// Make file executable
    #[cfg(unix)]
    fn make_executable(path: &Path) -> Result<()> {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = fs::metadata(path).map_err(Error::Io)?.permissions();
        perms.set_mode(0o755);
        fs::set_permissions(path, perms).map_err(Error::Io)?;
        Ok(())
    }

    #[cfg(not(unix))]
    fn make_executable(_path: &Path) -> Result<()> {
        Ok(())
    }

    // Hook scripts

    fn pre_commit_hook_script() -> String {
        r#"#!/usr/bin/env bash
# Living Documentation - Pre-commit Hook
# Extract code ontology and validate documentation sync

set -e

echo "Living Documentation: Extracting code ontology..."
ggen docs extract

echo "Living Documentation: Validating documentation..."
ggen docs validate

if [ $? -ne 0 ]; then
    echo "Documentation validation failed. Please update documentation."
    exit 1
fi

echo "Living Documentation: Pre-commit checks passed"
"#.to_string()
    }

    fn post_commit_hook_script() -> String {
        r#"#!/usr/bin/env bash
# Living Documentation - Post-commit Hook
# Generate narratives from ontology changes

set -e

echo "Living Documentation: Generating documentation narratives..."
ggen docs narrate

echo "Living Documentation: Post-commit processing complete"
"#.to_string()
    }

    fn pre_push_hook_script() -> String {
        r#"#!/usr/bin/env bash
# Living Documentation - Pre-push Hook
# Ensure documentation is complete before push

set -e

echo "Living Documentation: Final validation before push..."
ggen docs validate --strict

if [ $? -ne 0 ]; then
    echo "Documentation is incomplete. Please update before pushing."
    exit 1
fi

echo "Living Documentation: Pre-push checks passed"
"#.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn test_hooks_manager_creation() {
        let dir = tempdir().unwrap();
        let manager = GitHooksManager::new(dir.path());
        assert_eq!(manager.repo_path, dir.path());
    }
}
