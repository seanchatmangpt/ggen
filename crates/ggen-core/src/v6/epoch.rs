//! Epoch: Frozen input state for reproducibility
//!
//! An epoch represents a frozen point-in-time snapshot of all input ontologies.
//! This is the "O" in A = μ(O) - the immutable input substrate.

use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

/// SHA-256 hash identifying an epoch
pub type EpochId = String;

/// A frozen input epoch containing all ontology hashes
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Epoch {
    /// SHA-256 hash of all input hashes (the epoch ID)
    pub id: EpochId,

    /// When this epoch was created (ISO 8601)
    pub timestamp: String,

    /// All ontology inputs with their hashes
    pub inputs: BTreeMap<PathBuf, OntologyInput>,

    /// Total triple count across all inputs
    pub total_triples: usize,
}

/// A single ontology input file
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OntologyInput {
    /// Relative path from project root
    pub path: PathBuf,

    /// SHA-256 hash of file contents
    pub hash: String,

    /// File size in bytes
    pub size_bytes: usize,

    /// Number of RDF triples
    pub triple_count: usize,
}

impl Epoch {
    /// Create a new epoch from a set of ontology files
    ///
    /// # Arguments
    /// * `base_path` - Base directory for resolving paths
    /// * `ontology_paths` - Paths to ontology files
    ///
    /// # Returns
    /// * `Ok(Epoch)` - Frozen epoch with all hashes computed
    /// * `Err(Error)` - If any file cannot be read
    pub fn create(base_path: &Path, ontology_paths: &[PathBuf]) -> Result<Self> {
        let timestamp = chrono::Utc::now().to_rfc3339();
        let mut inputs = BTreeMap::new();
        let mut total_triples = 0;

        // Process each ontology file
        for rel_path in ontology_paths {
            let full_path = base_path.join(rel_path);
            let input = OntologyInput::from_file(&full_path, rel_path)?;
            total_triples += input.triple_count;
            inputs.insert(rel_path.clone(), input);
        }

        // Compute epoch ID from all input hashes
        let id = Self::compute_epoch_id(&inputs);

        Ok(Self {
            id,
            timestamp,
            inputs,
            total_triples,
        })
    }

    /// Compute the epoch ID by hashing all input hashes
    fn compute_epoch_id(inputs: &BTreeMap<PathBuf, OntologyInput>) -> EpochId {
        let mut hasher = Sha256::new();

        // Hash inputs in deterministic order (BTreeMap guarantees this)
        for (path, input) in inputs {
            hasher.update(path.to_string_lossy().as_bytes());
            hasher.update(b":");
            hasher.update(input.hash.as_bytes());
            hasher.update(b"\n");
        }

        format!("{:x}", hasher.finalize())
    }

    /// Verify that all input files still match their recorded hashes
    ///
    /// # Arguments
    /// * `base_path` - Base directory for resolving paths
    ///
    /// # Returns
    /// * `Ok(true)` - All files match
    /// * `Ok(false)` - At least one file has changed
    /// * `Err(Error)` - If a file cannot be read
    pub fn verify(&self, base_path: &Path) -> Result<bool> {
        for (rel_path, input) in &self.inputs {
            let full_path = base_path.join(rel_path);
            let current_hash = Self::hash_file(&full_path)?;
            if current_hash != input.hash {
                return Ok(false);
            }
        }
        Ok(true)
    }

    /// Get list of changed files since epoch was created
    pub fn get_changed_files(&self, base_path: &Path) -> Result<Vec<PathBuf>> {
        let mut changed = Vec::new();

        for (rel_path, input) in &self.inputs {
            let full_path = base_path.join(rel_path);
            let current_hash = Self::hash_file(&full_path)?;
            if current_hash != input.hash {
                changed.push(rel_path.clone());
            }
        }

        Ok(changed)
    }

    /// Compute SHA-256 hash of a file
    fn hash_file(path: &Path) -> Result<String> {
        let content = std::fs::read(path)
            .map_err(|e| Error::new(&format!("Failed to read file '{}': {}", path.display(), e)))?;

        let hash = Sha256::digest(&content);
        Ok(format!("{:x}", hash))
    }
}

impl OntologyInput {
    /// Create an ontology input from a file
    ///
    /// # Arguments
    /// * `full_path` - Full path to the file
    /// * `rel_path` - Relative path for storage
    pub fn from_file(full_path: &Path, rel_path: &Path) -> Result<Self> {
        let content = std::fs::read(full_path).map_err(|e| {
            Error::new(&format!(
                "Failed to read ontology '{}': {}",
                full_path.display(),
                e
            ))
        })?;

        let hash = format!("{:x}", Sha256::digest(&content));
        let size_bytes = content.len();

        // Count triples (approximate by counting lines starting with valid RDF patterns)
        let content_str = String::from_utf8_lossy(&content);
        let triple_count = Self::estimate_triple_count(&content_str);

        Ok(Self {
            path: rel_path.to_path_buf(),
            hash,
            size_bytes,
            triple_count,
        })
    }

    /// Estimate triple count from Turtle content
    fn estimate_triple_count(content: &str) -> usize {
        // Simple heuristic: count statements (lines ending with . or ;)
        // This is approximate but sufficient for auditing
        content
            .lines()
            .filter(|line| {
                let trimmed = line.trim();
                !trimmed.is_empty()
                    && !trimmed.starts_with('#')
                    && !trimmed.starts_with("@prefix")
                    && (trimmed.ends_with('.') || trimmed.ends_with(';'))
            })
            .count()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::TempDir;

    #[test]
    fn test_epoch_creation() {
        let temp_dir = TempDir::new().unwrap();
        let ontology_path = temp_dir.path().join("test.ttl");

        let mut file = std::fs::File::create(&ontology_path).unwrap();
        writeln!(
            file,
            r#"
            @prefix ex: <http://example.org/> .
            ex:alice a ex:Person .
            ex:bob a ex:Person .
        "#
        )
        .unwrap();

        let epoch = Epoch::create(temp_dir.path(), &[PathBuf::from("test.ttl")]).unwrap();

        assert!(!epoch.id.is_empty());
        assert_eq!(epoch.id.len(), 64); // SHA-256 hex length
        assert_eq!(epoch.inputs.len(), 1);
        assert!(epoch.total_triples > 0);
    }

    #[test]
    fn test_epoch_verify() {
        let temp_dir = TempDir::new().unwrap();
        let ontology_path = temp_dir.path().join("test.ttl");

        // Create initial file
        std::fs::write(&ontology_path, "ex:alice a ex:Person .").unwrap();

        let epoch = Epoch::create(temp_dir.path(), &[PathBuf::from("test.ttl")]).unwrap();

        // Should verify true when unchanged
        assert!(epoch.verify(temp_dir.path()).unwrap());

        // Modify file
        std::fs::write(&ontology_path, "ex:bob a ex:Person .").unwrap();

        // Should verify false when changed
        assert!(!epoch.verify(temp_dir.path()).unwrap());
    }

    #[test]
    fn test_epoch_id_deterministic() {
        let temp_dir = TempDir::new().unwrap();
        let ontology_path = temp_dir.path().join("test.ttl");
        std::fs::write(&ontology_path, "ex:alice a ex:Person .").unwrap();

        let epoch1 = Epoch::create(temp_dir.path(), &[PathBuf::from("test.ttl")]).unwrap();
        let epoch2 = Epoch::create(temp_dir.path(), &[PathBuf::from("test.ttl")]).unwrap();

        // Same content should produce same epoch ID (ignoring timestamp)
        assert_eq!(epoch1.id, epoch2.id);
    }
}
