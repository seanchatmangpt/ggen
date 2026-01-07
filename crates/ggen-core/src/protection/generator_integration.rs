use std::path::{Path, PathBuf};
use crate::signals::{AndonSignal, AndonContext};
use super::path_protector::{PathProtector, PathProtectionError};

/// Integration point for PathProtector in code generation pipeline
///
/// This module provides guards and hooks for the generator to enforce
/// path protection before writing files, preventing protected paths from
/// being overwritten during regeneration.
pub struct GeneratorPathGuard {
    protector: PathProtector,
}

impl GeneratorPathGuard {
    pub fn new(protector: PathProtector) -> Self {
        Self { protector }
    }

    pub fn default() -> Self {
        Self {
            protector: PathProtector::default_protection(),
        }
    }

    /// Verify a path is safe to write before code generation
    ///
    /// Returns:
    /// - Ok(AndonSignal::Green) if path is regeneratable
    /// - Err(AndonContext::Red) if path is protected
    /// - Ok(AndonSignal::Yellow) if path needs review
    pub fn verify_write_path(&self, path: &Path) -> Result<AndonSignal, AndonContext> {
        match self.protector.can_write(path) {
            Ok(()) => {
                let signal = if self.protector.is_regeneratable(path) {
                    AndonSignal::Green
                } else {
                    AndonSignal::Yellow
                };
                Ok(signal)
            }
            Err(PathProtectionError::ProtectedPathViolation { path: p, pattern }) => {
                Err(AndonContext::red(
                    format!(
                        "Protected path violation: '{}' matches protected pattern '{}'",
                        p, pattern
                    ),
                    "PathGuard::verify_write_path",
                ))
            }
        }
    }

    /// Pre-generation check: verify all output paths are safe
    pub fn verify_output_paths(&self, paths: &[PathBuf]) -> Result<(), AndonContext> {
        for path in paths {
            match self.verify_write_path(path) {
                Ok(AndonSignal::Green) => continue,
                Ok(AndonSignal::Yellow) => {
                    eprintln!("⚠️  YELLOW: {} - Not explicitly regeneratable", path.display());
                    continue;
                }
                Ok(AndonSignal::Red) | Err(_) => {
                    return Err(AndonContext::red(
                        format!("Cannot write to protected path: {}", path.display()),
                        "PathGuard::verify_output_paths",
                    ))
                }
            }
        }
        Ok(())
    }

    /// Get the underlying protector for direct access
    pub fn protector(&self) -> &PathProtector {
        &self.protector
    }

    /// Check if a specific path is protected
    pub fn is_protected(&self, path: &Path) -> bool {
        self.protector.is_protected(path)
    }

    /// Check if a specific path is safe to regenerate
    pub fn is_regeneratable(&self, path: &Path) -> bool {
        self.protector.is_regeneratable(path)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn test_generator_guard_blocks_protected() {
        let guard = GeneratorPathGuard::default();
        let result = guard.verify_write_path(Path::new(".env"));
        assert!(result.is_err());
    }

    #[test]
    fn test_generator_guard_allows_regeneratable() {
        let guard = GeneratorPathGuard::default();
        let result = guard.verify_write_path(Path::new("src/main.rs"));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), AndonSignal::Green);
    }

    #[test]
    fn test_verify_multiple_paths() {
        let guard = GeneratorPathGuard::default();
        let paths = vec![
            PathBuf::from("src/main.rs"),
            PathBuf::from("src/generated/mod.rs"),
        ];
        assert!(guard.verify_output_paths(&paths).is_ok());
    }

    #[test]
    fn test_verify_paths_with_protected() {
        let guard = GeneratorPathGuard::default();
        let paths = vec![
            PathBuf::from("src/main.rs"),
            PathBuf::from(".env"),
        ];
        assert!(guard.verify_output_paths(&paths).is_err());
    }
}
