//! Represents a generated file with content hash verification

use std::collections::HashMap;
use std::path::PathBuf;

/// Represents a single generated file
#[derive(Debug, Clone)]
pub struct GeneratedFile {
    /// Relative file path (may contain {{ }} for variable substitution)
    pub path: PathBuf,
    /// File content
    pub content: String,
    /// Source rule name
    pub source_rule: String,
    /// Content hash for determinism verification
    pub content_hash: String,
}

impl GeneratedFile {
    /// Create a new generated file with content hash
    pub fn new(path: PathBuf, content: String, source_rule: String) -> Self {
        use sha2::{Digest, Sha256};

        let mut hasher = Sha256::new();
        hasher.update(&content);
        let content_hash = format!("{:x}", hasher.finalize());

        Self {
            path,
            content,
            source_rule,
            content_hash,
        }
    }

    /// Render the file path with bindings
    pub fn render_path(&self, bindings: &HashMap<String, String>) -> crate::Result<PathBuf> {
        let path_str = self.path.to_string_lossy().to_string();

        // Simple template variable substitution ({{ varName }} → value)
        let rendered = bindings.iter().fold(path_str, |acc, (key, value)| {
            acc.replace(&format!("{{{{{}}}}}", key), value)
        });

        Ok(PathBuf::from(rendered))
    }
}
