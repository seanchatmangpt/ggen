//! Task artifact management

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;
use thiserror::Error;

/// Artifact errors
#[derive(Debug, Error)]
pub enum ArtifactError {
    #[error("Artifact not found: {0}")]
    NotFound(String),

    #[error("Invalid artifact type: {0}")]
    InvalidType(String),

    #[error("Serialization error: {0}")]
    SerializationError(String),

    #[error("IO error: {0}")]
    IoError(String),
}

/// Artifact type classification
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ArtifactType {
    /// Input data for task
    Input,
    /// Output data from task
    Output,
    /// Intermediate working data
    Intermediate,
    /// Configuration data
    Config,
    /// Log or trace data
    Log,
}

/// Artifact content storage
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ArtifactContent {
    /// Text content
    Text(String),
    /// Binary content (base64 encoded)
    Binary(Vec<u8>),
    /// JSON data
    Json(serde_json::Value),
    /// File reference
    FileRef(PathBuf),
    /// URL reference
    UrlRef(String),
}

/// Task artifact representing inputs, outputs, and intermediate data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Artifact {
    /// Artifact name/identifier
    pub name: String,
    /// Artifact type
    pub artifact_type: ArtifactType,
    /// Artifact content
    pub content: ArtifactContent,
    /// Content MIME type
    pub mime_type: Option<String>,
    /// Artifact metadata
    pub metadata: HashMap<String, String>,
    /// Creation timestamp
    pub created_at: DateTime<Utc>,
    /// Content hash for verification
    pub hash: Option<String>,
}

impl Artifact {
    /// Create a new artifact
    pub fn new(name: String, artifact_type: ArtifactType, content: ArtifactContent) -> Self {
        Self {
            name,
            artifact_type,
            content,
            mime_type: None,
            metadata: HashMap::new(),
            created_at: Utc::now(),
            hash: None,
        }
    }

    /// Create a text artifact
    pub fn text(name: String, artifact_type: ArtifactType, text: String) -> Self {
        Self::new(name, artifact_type, ArtifactContent::Text(text))
            .with_mime_type("text/plain".to_string())
    }

    /// Create a JSON artifact
    pub fn json(
        name: String,
        artifact_type: ArtifactType,
        value: serde_json::Value,
    ) -> Self {
        Self::new(name, artifact_type, ArtifactContent::Json(value))
            .with_mime_type("application/json".to_string())
    }

    /// Create a file reference artifact
    pub fn file(name: String, artifact_type: ArtifactType, path: PathBuf) -> Self {
        Self::new(name, artifact_type, ArtifactContent::FileRef(path))
    }

    /// Create a URL reference artifact
    pub fn url(name: String, artifact_type: ArtifactType, url: String) -> Self {
        Self::new(name, artifact_type, ArtifactContent::UrlRef(url))
    }

    /// Set MIME type
    pub fn with_mime_type(mut self, mime_type: String) -> Self {
        self.mime_type = Some(mime_type);
        self
    }

    /// Add metadata
    pub fn with_metadata(mut self, key: String, value: String) -> Self {
        self.metadata.insert(key, value);
        self
    }

    /// Set content hash
    pub fn with_hash(mut self, hash: String) -> Self {
        self.hash = Some(hash);
        self
    }

    /// Get artifact size estimate
    pub fn size_estimate(&self) -> usize {
        match &self.content {
            ArtifactContent::Text(s) => s.len(),
            ArtifactContent::Binary(b) => b.len(),
            ArtifactContent::Json(v) => v.to_string().len(),
            ArtifactContent::FileRef(_) => 0,
            ArtifactContent::UrlRef(u) => u.len(),
        }
    }

    /// Check if artifact is a reference (file or URL)
    pub fn is_reference(&self) -> bool {
        matches!(
            &self.content,
            ArtifactContent::FileRef(_) | ArtifactContent::UrlRef(_)
        )
    }
}

/// Artifact collection manager
#[derive(Debug, Default)]
pub struct ArtifactCollection {
    artifacts: HashMap<String, Artifact>,
}

impl ArtifactCollection {
    /// Create a new artifact collection
    pub fn new() -> Self {
        Self {
            artifacts: HashMap::new(),
        }
    }

    /// Add an artifact
    pub fn add(&mut self, artifact: Artifact) -> Result<(), ArtifactError> {
        self.artifacts.insert(artifact.name.clone(), artifact);
        Ok(())
    }

    /// Get an artifact by name
    pub fn get(&self, name: &str) -> Result<&Artifact, ArtifactError> {
        self.artifacts
            .get(name)
            .ok_or_else(|| ArtifactError::NotFound(name.to_string()))
    }

    /// Remove an artifact
    pub fn remove(&mut self, name: &str) -> Result<Artifact, ArtifactError> {
        self.artifacts
            .remove(name)
            .ok_or_else(|| ArtifactError::NotFound(name.to_string()))
    }

    /// List all artifact names
    pub fn list(&self) -> Vec<String> {
        self.artifacts.keys().cloned().collect()
    }

    /// List artifacts by type
    pub fn list_by_type(&self, artifact_type: ArtifactType) -> Vec<&Artifact> {
        self.artifacts
            .values()
            .filter(|a| a.artifact_type == artifact_type)
            .collect()
    }

    /// Get total size estimate
    pub fn total_size(&self) -> usize {
        self.artifacts.values().map(|a| a.size_estimate()).sum()
    }

    /// Clear all artifacts
    pub fn clear(&mut self) {
        self.artifacts.clear();
    }

    /// Count artifacts
    pub fn count(&self) -> usize {
        self.artifacts.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_artifact_creation() {
        let artifact = Artifact::text(
            "input.txt".to_string(),
            ArtifactType::Input,
            "test content".to_string(),
        );

        assert_eq!(artifact.name, "input.txt");
        assert_eq!(artifact.artifact_type, ArtifactType::Input);
        assert!(matches!(artifact.content, ArtifactContent::Text(_)));
        assert_eq!(artifact.mime_type, Some("text/plain".to_string()));
    }

    #[test]
    fn test_json_artifact() {
        let value = serde_json::json!({"key": "value"});
        let artifact = Artifact::json("config.json".to_string(), ArtifactType::Config, value);

        assert_eq!(artifact.artifact_type, ArtifactType::Config);
        assert!(matches!(artifact.content, ArtifactContent::Json(_)));
        assert_eq!(
            artifact.mime_type,
            Some("application/json".to_string())
        );
    }

    #[test]
    fn test_file_artifact() {
        let path = PathBuf::from("/tmp/test.dat");
        let artifact = Artifact::file("test.dat".to_string(), ArtifactType::Output, path.clone());

        assert!(artifact.is_reference());
        match artifact.content {
            ArtifactContent::FileRef(p) => assert_eq!(p, path),
            _ => panic!("Expected FileRef"),
        }
    }

    #[test]
    fn test_url_artifact() {
        let url = "https://example.com/data.json".to_string();
        let artifact = Artifact::url("remote.json".to_string(), ArtifactType::Input, url.clone());

        assert!(artifact.is_reference());
        match artifact.content {
            ArtifactContent::UrlRef(u) => assert_eq!(u, url),
            _ => panic!("Expected UrlRef"),
        }
    }

    #[test]
    fn test_artifact_metadata() {
        let artifact = Artifact::text(
            "test.txt".to_string(),
            ArtifactType::Output,
            "content".to_string(),
        )
        .with_metadata("version".to_string(), "1.0".to_string())
        .with_hash("abc123".to_string());

        assert_eq!(artifact.metadata.get("version"), Some(&"1.0".to_string()));
        assert_eq!(artifact.hash, Some("abc123".to_string()));
    }

    #[test]
    fn test_artifact_size_estimate() {
        let text = "test content";
        let artifact = Artifact::text(
            "test.txt".to_string(),
            ArtifactType::Output,
            text.to_string(),
        );

        assert_eq!(artifact.size_estimate(), text.len());
    }

    #[test]
    fn test_collection_add_and_get() {
        let mut collection = ArtifactCollection::new();
        let artifact = Artifact::text(
            "test.txt".to_string(),
            ArtifactType::Input,
            "content".to_string(),
        );

        collection.add(artifact).unwrap();
        let retrieved = collection.get("test.txt").unwrap();

        assert_eq!(retrieved.name, "test.txt");
    }

    #[test]
    fn test_collection_remove() {
        let mut collection = ArtifactCollection::new();
        let artifact = Artifact::text(
            "test.txt".to_string(),
            ArtifactType::Input,
            "content".to_string(),
        );

        collection.add(artifact).unwrap();
        let removed = collection.remove("test.txt").unwrap();

        assert_eq!(removed.name, "test.txt");
        assert!(collection.get("test.txt").is_err());
    }

    #[test]
    fn test_collection_list() {
        let mut collection = ArtifactCollection::new();
        collection
            .add(Artifact::text(
                "test1.txt".to_string(),
                ArtifactType::Input,
                "content".to_string(),
            ))
            .unwrap();
        collection
            .add(Artifact::text(
                "test2.txt".to_string(),
                ArtifactType::Output,
                "content".to_string(),
            ))
            .unwrap();

        let names = collection.list();
        assert_eq!(names.len(), 2);
    }

    #[test]
    fn test_collection_list_by_type() {
        let mut collection = ArtifactCollection::new();
        collection
            .add(Artifact::text(
                "input.txt".to_string(),
                ArtifactType::Input,
                "content".to_string(),
            ))
            .unwrap();
        collection
            .add(Artifact::text(
                "output.txt".to_string(),
                ArtifactType::Output,
                "content".to_string(),
            ))
            .unwrap();

        let inputs = collection.list_by_type(ArtifactType::Input);
        assert_eq!(inputs.len(), 1);
        assert_eq!(inputs[0].name, "input.txt");
    }

    #[test]
    fn test_collection_total_size() {
        let mut collection = ArtifactCollection::new();
        collection
            .add(Artifact::text(
                "test1.txt".to_string(),
                ArtifactType::Input,
                "12345".to_string(),
            ))
            .unwrap();
        collection
            .add(Artifact::text(
                "test2.txt".to_string(),
                ArtifactType::Output,
                "67890".to_string(),
            ))
            .unwrap();

        assert_eq!(collection.total_size(), 10);
    }

    #[test]
    fn test_collection_clear() {
        let mut collection = ArtifactCollection::new();
        collection
            .add(Artifact::text(
                "test.txt".to_string(),
                ArtifactType::Input,
                "content".to_string(),
            ))
            .unwrap();

        collection.clear();
        assert_eq!(collection.count(), 0);
    }
}
