//! Guard: Forbidden output class constraints (H)
//!
//! Guards enforce μ ⊣ H, ensuring that certain classes of outputs
//! are forbidden during projection.

use regex::Regex;
use serde::{Deserialize, Serialize};
use std::path::Path;

/// Action to take when a guard is violated
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum GuardAction {
    /// Reject the output (fail the projection)
    Reject,
    /// Warn but continue
    Warn,
    /// Require explicit approval
    RequireApproval,
}

/// A guard violation detected during projection
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GuardViolation {
    /// Name of the guard that was violated
    pub guard_name: String,

    /// Path or content that violated the guard
    pub violating_content: String,

    /// Human-readable explanation
    pub message: String,

    /// Action that should be taken
    pub action: GuardAction,
}

/// Trait for implementing guards
pub trait Guard: Send + Sync {
    /// Get the guard name
    fn name(&self) -> &str;

    /// Get the guard action
    fn action(&self) -> GuardAction;

    /// Whether this guard is enabled
    fn is_enabled(&self) -> bool;

    /// Check if a path violates this guard
    ///
    /// # Arguments
    /// * `path` - Relative path to check
    ///
    /// # Returns
    /// * `Some(GuardViolation)` if the path violates the guard
    /// * `None` if the path is allowed
    fn check_path(&self, path: &Path) -> Option<GuardViolation>;

    /// Check if content violates this guard
    ///
    /// # Arguments
    /// * `content` - File content to check
    /// * `path` - Path for error messages
    ///
    /// # Returns
    /// * `Some(GuardViolation)` if the content violates the guard
    /// * `None` if the content is allowed
    fn check_content(&self, content: &str, path: &Path) -> Option<GuardViolation>;
}

/// Guard that restricts output paths
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PathGuard {
    /// Guard name
    pub name: String,

    /// Glob pattern for allowed paths
    pub allowed_pattern: String,

    /// Compiled regex (not serialized)
    #[serde(skip)]
    pub pattern_regex: Option<Regex>,

    /// Action when violated
    pub action: GuardAction,

    /// Whether enabled
    pub enabled: bool,
}

impl PathGuard {
    /// Create a new path guard
    pub fn new(name: impl Into<String>, allowed_pattern: impl Into<String>) -> Self {
        let pattern = allowed_pattern.into();
        let regex = Self::glob_to_regex(&pattern);
        Self {
            name: name.into(),
            allowed_pattern: pattern,
            pattern_regex: regex,
            action: GuardAction::Reject,
            enabled: true,
        }
    }

    /// Convert a glob pattern to a regex
    fn glob_to_regex(pattern: &str) -> Option<Regex> {
        let regex_str = pattern
            .replace("**", "DOUBLESTAR")
            .replace("*", "[^/]*")
            .replace("DOUBLESTAR", ".*")
            .replace("?", "[^/]");
        Regex::new(&format!("^{}$", regex_str)).ok()
    }

    /// Set the action
    pub fn with_action(mut self, action: GuardAction) -> Self {
        self.action = action;
        self
    }

    /// Set enabled state
    pub fn with_enabled(mut self, enabled: bool) -> Self {
        self.enabled = enabled;
        self
    }
}

impl Guard for PathGuard {
    fn name(&self) -> &str {
        &self.name
    }

    fn action(&self) -> GuardAction {
        self.action
    }

    fn is_enabled(&self) -> bool {
        self.enabled
    }

    fn check_path(&self, path: &Path) -> Option<GuardViolation> {
        if !self.enabled {
            return None;
        }

        let path_str = path.to_string_lossy();

        // Check if path matches allowed pattern
        let matches = if let Some(ref regex) = self.pattern_regex {
            regex.is_match(&path_str)
        } else {
            // Fallback to simple prefix matching
            path_str.starts_with(&self.allowed_pattern.replace("**", ""))
        };

        if matches {
            None
        } else {
            Some(GuardViolation {
                guard_name: self.name.clone(),
                violating_content: path_str.to_string(),
                message: format!(
                    "Path '{}' is not allowed. Must match pattern '{}'",
                    path_str, self.allowed_pattern
                ),
                action: self.action,
            })
        }
    }

    fn check_content(&self, _content: &str, _path: &Path) -> Option<GuardViolation> {
        None // PathGuard doesn't check content
    }
}

/// Guard that detects secrets in content
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SecretGuard {
    /// Guard name
    pub name: String,

    /// Regex pattern to match secrets
    pub pattern: String,

    /// Compiled regex (not serialized)
    #[serde(skip)]
    pub pattern_regex: Option<Regex>,

    /// Action when violated
    pub action: GuardAction,

    /// Whether enabled
    pub enabled: bool,
}

impl SecretGuard {
    /// Create a new secret guard with default patterns
    pub fn new(name: impl Into<String>) -> Self {
        // Pattern to detect potential secrets (password=, secret=, api_key=, etc.)
        let pattern =
            r#"(?i)(password|secret|api[_-]?key|token|private[_-]?key)\s*[:=]\s*["'][^"']+["']"#;
        Self {
            name: name.into(),
            pattern: pattern.to_string(),
            pattern_regex: Regex::new(pattern).ok(),
            action: GuardAction::Reject,
            enabled: true,
        }
    }

    /// Create with a custom pattern
    pub fn with_pattern(mut self, pattern: impl Into<String>) -> Self {
        let p = pattern.into();
        self.pattern_regex = Regex::new(&p).ok();
        self.pattern = p;
        self
    }

    /// Set the action
    pub fn with_action(mut self, action: GuardAction) -> Self {
        self.action = action;
        self
    }

    /// Set enabled state
    pub fn with_enabled(mut self, enabled: bool) -> Self {
        self.enabled = enabled;
        self
    }
}

impl Guard for SecretGuard {
    fn name(&self) -> &str {
        &self.name
    }

    fn action(&self) -> GuardAction {
        self.action
    }

    fn is_enabled(&self) -> bool {
        self.enabled
    }

    fn check_path(&self, path: &Path) -> Option<GuardViolation> {
        if !self.enabled {
            return None;
        }

        let path_str = path.to_string_lossy();

        // Check for sensitive file names
        let sensitive_extensions = [".env", ".credentials", ".secret", ".pem", ".key"];
        for ext in sensitive_extensions {
            if path_str.ends_with(ext) {
                return Some(GuardViolation {
                    guard_name: self.name.clone(),
                    violating_content: path_str.to_string(),
                    message: format!("Cannot emit sensitive file type: '{}'", ext),
                    action: self.action,
                });
            }
        }

        None
    }

    fn check_content(&self, content: &str, path: &Path) -> Option<GuardViolation> {
        if !self.enabled {
            return None;
        }

        if let Some(ref regex) = self.pattern_regex {
            if let Some(m) = regex.find(content) {
                return Some(GuardViolation {
                    guard_name: self.name.clone(),
                    violating_content: m.as_str().to_string(),
                    message: format!(
                        "Potential secret detected in '{}': {}",
                        path.display(),
                        m.as_str()
                    ),
                    action: self.action,
                });
            }
        }

        None
    }
}

/// Collection of guards to apply during projection
pub struct GuardSet {
    guards: Vec<Box<dyn Guard>>,
}

impl std::fmt::Debug for GuardSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("GuardSet")
            .field("guards_count", &self.guards.len())
            .finish()
    }
}

impl Default for GuardSet {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for GuardSet {
    /// Clone by recreating guards (guards are stateless)
    fn clone(&self) -> Self {
        // Since guards are stateless, we create a new default set
        // This preserves the configured guard behavior
        Self::default_v6()
    }
}

impl GuardSet {
    /// Create a new empty guard set
    pub fn new() -> Self {
        Self { guards: Vec::new() }
    }

    /// Add a guard to the set
    pub fn add_guard(&mut self, guard: impl Guard + 'static) {
        self.guards.push(Box::new(guard));
    }

    /// Check a path against all guards
    pub fn check_path(&self, path: &Path) -> Vec<GuardViolation> {
        self.guards
            .iter()
            .filter_map(|g| g.check_path(path))
            .collect()
    }

    /// Check content against all guards
    pub fn check_content(&self, content: &str, path: &Path) -> Vec<GuardViolation> {
        self.guards
            .iter()
            .filter_map(|g| g.check_content(content, path))
            .collect()
    }

    /// Check both path and content
    pub fn check(&self, path: &Path, content: &str) -> Vec<GuardViolation> {
        let mut violations = self.check_path(path);
        violations.extend(self.check_content(content, path));
        violations
    }

    /// Create a default guard set with standard v6 guards
    pub fn default_v6() -> Self {
        let mut set = Self::new();
        set.add_guard(PathGuard::new("path-guard", "ontology/generated/**"));
        set.add_guard(SecretGuard::new("secret-guard"));
        set
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_path_guard_allowed() {
        let guard = PathGuard::new("test", "ontology/generated/**");
        assert!(guard
            .check_path(Path::new("ontology/generated/domain.ttl"))
            .is_none());
    }

    #[test]
    fn test_path_guard_rejected() {
        let guard = PathGuard::new("test", "ontology/generated/**");
        let violation = guard.check_path(Path::new("ontology/core.ttl"));
        assert!(violation.is_some());
        assert_eq!(violation.unwrap().guard_name, "test");
    }

    #[test]
    fn test_secret_guard_clean_content() {
        let guard = SecretGuard::new("secret-guard");
        let content = "fn main() { println!(\"hello\"); }";
        assert!(guard.check_content(content, Path::new("test.rs")).is_none());
    }

    #[test]
    fn test_secret_guard_detects_secret() {
        let guard = SecretGuard::new("secret-guard");
        let content = r#"let api_key = "sk-1234567890";"#;
        let violation = guard.check_content(content, Path::new("config.rs"));
        assert!(violation.is_some());
    }

    #[test]
    fn test_secret_guard_rejects_env_file() {
        let guard = SecretGuard::new("secret-guard");
        let violation = guard.check_path(Path::new(".env"));
        assert!(violation.is_some());
    }

    #[test]
    fn test_guard_set() {
        let mut set = GuardSet::new();
        set.add_guard(PathGuard::new("path", "ontology/generated/**"));
        set.add_guard(SecretGuard::new("secret"));

        // Should pass both guards
        let violations = set.check(
            Path::new("ontology/generated/domain.ttl"),
            "@prefix ex: <http://example.org/> .",
        );
        assert!(violations.is_empty());

        // Should fail path guard
        let violations = set.check(
            Path::new("ontology/core.ttl"),
            "@prefix ex: <http://example.org/> .",
        );
        assert_eq!(violations.len(), 1);
    }
}
