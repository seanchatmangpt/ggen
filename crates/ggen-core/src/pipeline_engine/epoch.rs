//! Epoch: Frozen input state for reproducibility
//!
//! An epoch represents a frozen point-in-time snapshot of all input ontologies.
//! This is the "O" in A = μ(O) - the immutable input substrate.

use crate::ontology::OntologyLoader;
use crate::utils::error::{Error, Result};
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

    /// Create a new epoch with fallback support for both file paths and namespace URIs
    ///
    /// This method attempts to load ontologies from either file paths or namespace URIs,
    /// using the OntologyLoader's fallback chain:
    /// 1. Core bundle (embedded, zero-copy)
    /// 2. Local filesystem
    /// 3. Future: Marketplace (download if needed)
    ///
    /// # Arguments
    /// * `base_path` - Base directory for resolving file paths
    /// * `identifiers` - Mix of file paths and namespace URIs
    ///
    /// # Returns
    /// * `Ok(Epoch)` - Frozen epoch with all ontologies loaded and hashed
    /// * `Err(Error)` - If any ontology cannot be found or loaded
    ///
    /// # Examples
    ///
    /// ```ignore
    /// let epoch = Epoch::create_with_fallback(
    ///     Path::new("."),
    ///     &[
    ///         "ontology/custom.ttl",
    ///         "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    ///     ]
    /// )?;
    /// ```
    pub fn create_with_fallback(base_path: &Path, identifiers: &[&str]) -> Result<Self> {
        let timestamp = chrono::Utc::now().to_rfc3339();
        let mut inputs = BTreeMap::new();
        let mut total_triples = 0;

        for identifier in identifiers {
            let is_uri = identifier.starts_with("http://") || identifier.starts_with("https://");

            let input = if is_uri {
                // Load from namespace URI using OntologyLoader fallback chain
                OntologyInput::from_namespace(identifier, base_path, Some(identifier))?
            } else {
                // Load from file path
                let rel_path = PathBuf::from(identifier);
                let full_path = base_path.join(&rel_path);
                OntologyInput::from_file(&full_path, &rel_path)?
            };

            total_triples += input.triple_count;
            inputs.insert(PathBuf::from(*identifier), input);
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

    /// Epoch when inputs come only from merged pack ontologies (no project TTL files).
    ///
    /// `content_digest` should be a deterministic hash of the merged graph serialization
    /// (e.g. SHA-256 hex of canonical Turtle).
    pub fn from_pack_merged_substrate(content_digest: String, triple_count: usize) -> Self {
        let timestamp = chrono::Utc::now().to_rfc3339();
        let mut inputs = BTreeMap::new();
        let pseudo = PathBuf::from(".ggen/pack-merged-substrate.ttl");
        inputs.insert(
            pseudo.clone(),
            OntologyInput {
                path: pseudo,
                hash: content_digest,
                size_bytes: 0,
                triple_count,
            },
        );
        let id = Self::compute_epoch_id(&inputs);
        Self {
            id,
            timestamp,
            inputs,
            total_triples: triple_count,
        }
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

    /// Create an ontology input from a namespace URI using OntologyLoader
    ///
    /// # Arguments
    /// * `uri` - Namespace URI (e.g., "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    /// * `base_path` - Base path for relative file lookups
    /// * `identifier` - Optional custom identifier for the ontology (uses URI by default)
    ///
    /// # Returns
    /// * `Ok(OntologyInput)` - Successfully loaded ontology with computed hash
    /// * `Err(Error)` - If ontology cannot be found or loaded
    ///
    /// # Fallback Chain
    /// 1. Core bundle (embedded, zero-copy, always available)
    /// 2. Local filesystem (for development)
    /// 3. Future: Marketplace (download and cache)
    pub fn from_namespace(uri: &str, base_path: &Path, identifier: Option<&str>) -> Result<Self> {
        let content = OntologyLoader::load_content(uri, base_path).ok_or_else(|| {
            Error::new(&format!(
                "Failed to load ontology '{}': not found in core bundle, filesystem, or marketplace",
                uri
            ))
        })?;

        let hash = format!("{:x}", Sha256::digest(&content));
        let size_bytes = content.len();

        // Count triples from loaded content
        let content_str = String::from_utf8_lossy(&content);
        let triple_count = Self::estimate_triple_count(&content_str);

        let path = identifier.unwrap_or(uri).to_string();
        Ok(Self {
            path: PathBuf::from(path),
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

    #[test]
    fn test_ontology_input_from_namespace_embedded() {
        // Test loading RDF ontology from core bundle
        let input = OntologyInput::from_namespace(
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
            Path::new("."),
            Some("rdf-syntax-ns"),
        )
        .expect("RDF ontology should be available in core bundle");

        assert_eq!(input.path.to_string_lossy(), "rdf-syntax-ns");
        assert!(!input.hash.is_empty());
        assert_eq!(input.hash.len(), 64); // SHA-256 hex length
        assert!(input.size_bytes > 0, "Ontology should have content");
        assert!(input.triple_count > 0, "RDF ontology should have triples");
    }

    #[test]
    fn test_ontology_input_from_namespace_fallback() {
        // Test fallback for ontology not in core bundle (should fail gracefully)
        let result = OntologyInput::from_namespace(
            "http://example.com/nonexistent/ontology#",
            Path::new("."),
            Some("nonexistent"),
        );
        assert!(
            result.is_err(),
            "Nonexistent ontology should fail with clear error"
        );
    }

    #[test]
    fn test_epoch_create_with_fallback_mixed() {
        let temp_dir = TempDir::new().unwrap();
        let custom_path = temp_dir.path().join("custom.ttl");

        // Create a custom ontology file
        std::fs::write(
            &custom_path,
            r#"
            @prefix ex: <http://example.org/> .
            ex:Class1 a owl:Class .
            ex:Class2 a owl:Class .
        "#,
        )
        .unwrap();

        // Create epoch with mixed identifiers: file path + core bundle URI
        let epoch = Epoch::create_with_fallback(
            temp_dir.path(),
            &["custom.ttl", "http://www.w3.org/1999/02/22-rdf-syntax-ns#"],
        )
        .expect("Should load custom file and RDF from core bundle");

        assert_eq!(epoch.inputs.len(), 2, "Should have 2 ontology inputs");
        assert!(epoch.total_triples > 0, "Should have total triple count");
        assert!(!epoch.id.is_empty(), "Epoch should have computed ID");

        // Verify both inputs are present
        assert!(
            epoch.inputs.contains_key(&PathBuf::from("custom.ttl")),
            "Should contain custom.ttl"
        );
        assert!(
            epoch.inputs.contains_key(&PathBuf::from(
                "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
            )),
            "Should contain RDF URI"
        );
    }

    #[test]
    fn test_epoch_create_with_fallback_embedded_only() {
        // Test creating epoch with only embedded ontologies (no file I/O)
        let epoch = Epoch::create_with_fallback(
            Path::new("."),
            &[
                "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                "http://www.w3.org/2002/07/owl#",
            ],
        )
        .expect("Should load RDF and OWL from core bundle");

        assert_eq!(epoch.inputs.len(), 2, "Should have 2 embedded ontologies");
        assert!(
            epoch.total_triples > 0,
            "Should count triples across ontologies"
        );
    }
}
