use std::fmt;
use std::path::Path;

use crate::signals::AndonSignal;

#[derive(Debug, Clone)]
pub struct GlobPattern(String);

#[derive(Debug, Clone)]
pub enum PathProtectionError {
    ProtectedPathViolation {
        path: String,
        pattern: String,
    },
}

impl fmt::Display for PathProtectionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PathProtectionError::ProtectedPathViolation { path, pattern } => {
                write!(
                    f,
                    "Protected path violation: '{}' matches protected pattern '{}'",
                    path, pattern
                )
            }
        }
    }
}

impl std::error::Error for PathProtectionError {}

impl GlobPattern {
    pub fn new(pattern: &str) -> Self {
        GlobPattern(pattern.to_string())
    }

    /// Simple glob pattern matching (supports * and ?)
    fn matches_path(&self, path: &str) -> bool {
        let pattern = &self.0;
        Self::glob_match(pattern, path)
    }

    fn glob_match(pattern: &str, path: &str) -> bool {
        let mut p_chars = pattern.chars().peekable();
        let mut t_chars = path.chars().peekable();

        while let Some(&pc) = p_chars.peek() {
            match pc {
                '*' => {
                    p_chars.next();
                    if p_chars.peek().is_none() {
                        return true;
                    }
                    while t_chars.peek().is_some() {
                        if Self::glob_match(
                            p_chars.clone().collect::<String>().as_str(),
                            t_chars.clone().collect::<String>().as_str(),
                        ) {
                            return true;
                        }
                        t_chars.next();
                    }
                    return false;
                }
                '?' => {
                    p_chars.next();
                    if t_chars.next().is_none() {
                        return false;
                    }
                }
                _ => {
                    p_chars.next();
                    if Some(pc) != t_chars.next() {
                        return false;
                    }
                }
            }
        }

        t_chars.peek().is_none()
    }
}

pub struct PathProtector {
    protected: Vec<GlobPattern>,
    regeneratable: Vec<GlobPattern>,
}

impl PathProtector {
    pub fn new(protected: Vec<&str>, regeneratable: Vec<&str>) -> Self {
        Self {
            protected: protected.into_iter().map(GlobPattern::new).collect(),
            regeneratable: regeneratable.into_iter().map(GlobPattern::new).collect(),
        }
    }

    pub fn default_protection() -> Self {
        Self::new(
            vec![
                ".env",
                ".env.*",
                "Cargo.toml",
                "Cargo.lock",
                ".git/*",
                ".gitignore",
                "src/domain/*",
                "tests/*",
            ],
            vec![
                "src/main.rs",
                "src/cli.rs",
                "src/error.rs",
                "src/generated/*",
            ],
        )
    }

    pub fn can_write(&self, path: &Path) -> Result<(), PathProtectionError> {
        let path_str = path.to_string_lossy();

        for pattern in &self.protected {
            if pattern.matches_path(&path_str) {
                return Err(PathProtectionError::ProtectedPathViolation {
                    path: path_str.to_string(),
                    pattern: pattern.0.clone(),
                });
            }
        }

        Ok(())
    }

    pub fn is_protected(&self, path: &Path) -> bool {
        self.can_write(path).is_err()
    }

    pub fn is_regeneratable(&self, path: &Path) -> bool {
        let path_str = path.to_string_lossy();
        self.regeneratable
            .iter()
            .any(|p| p.matches_path(&path_str))
    }

    pub fn andon_signal(&self, path: &Path) -> AndonSignal {
        if self.is_protected(path) {
            AndonSignal::Red
        } else if self.is_regeneratable(path) {
            AndonSignal::Green
        } else {
            AndonSignal::Yellow
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_glob_pattern_exact_match() {
        let pattern = GlobPattern::new(".env");
        assert!(pattern.matches_path(".env"));
        assert!(!pattern.matches_path(".env.local"));
    }

    #[test]
    fn test_glob_pattern_wildcard() {
        let pattern = GlobPattern::new(".env*");
        assert!(pattern.matches_path(".env"));
        assert!(pattern.matches_path(".env.local"));
        assert!(pattern.matches_path(".env.production"));
    }

    #[test]
    fn test_glob_pattern_directory() {
        let pattern = GlobPattern::new("src/domain/*");
        assert!(pattern.matches_path("src/domain/mod.rs"));
        assert!(pattern.matches_path("src/domain/core.rs"));
        assert!(!pattern.matches_path("src/main.rs"));
    }

    #[test]
    fn test_path_protector_blocks_protected() {
        let protector = PathProtector::default_protection();
        assert!(protector.can_write(Path::new(".env")).is_err());
        assert!(protector.can_write(Path::new("Cargo.toml")).is_err());
    }

    #[test]
    fn test_path_protector_allows_regeneratable() {
        let protector = PathProtector::default_protection();
        assert!(protector.can_write(Path::new("src/main.rs")).is_ok());
        assert!(protector.can_write(Path::new("src/generated/mod.rs")).is_ok());
    }

    #[test]
    fn test_andon_signals() {
        let protector = PathProtector::default_protection();
        assert_eq!(
            protector.andon_signal(Path::new(".env")),
            AndonSignal::Red
        );
        assert_eq!(
            protector.andon_signal(Path::new("src/main.rs")),
            AndonSignal::Green
        );
    }
}
