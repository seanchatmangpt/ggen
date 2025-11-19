//! Just-In-Time (JIT) ontology updates with kanban pull
//!
//! This module implements delta-driven regeneration that detects field-level changes
//! and selectively regenerates only affected templates, following the TPS principle
//! of "just-in-time" production.

use crate::delta::{DeltaType, GraphDelta};
use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

/// Type of change at the field level
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ChangeType {
    /// Field added to a class
    Added,
    /// Field removed from a class
    Removed,
    /// Field type or constraints modified
    Modified,
}

/// Represents a field-level change in the ontology
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct FieldDelta {
    /// The class this field belongs to
    pub class: String,
    /// The property/field that changed
    pub property: String,
    /// Type of change
    pub change_type: ChangeType,
    /// Old value (for modifications)
    pub old_value: Option<String>,
    /// New value (for additions and modifications)
    pub new_value: Option<String>,
}

impl FieldDelta {
    /// Create a new field delta
    pub fn new(
        class: String,
        property: String,
        change_type: ChangeType,
        old_value: Option<String>,
        new_value: Option<String>,
    ) -> Self {
        Self {
            class,
            property,
            change_type,
            old_value,
            new_value,
        }
    }

    /// Extract field deltas from graph deltas
    pub fn from_graph_delta(delta: &GraphDelta) -> Vec<Self> {
        let mut field_deltas = Vec::new();

        for graph_delta in &delta.deltas {
            match graph_delta {
                DeltaType::Addition {
                    subject,
                    predicate,
                    object,
                } => {
                    // Check if this is a property addition
                    if predicate.contains("domain") || predicate.contains("range") {
                        field_deltas.push(FieldDelta::new(
                            subject.clone(),
                            predicate.clone(),
                            ChangeType::Added,
                            None,
                            Some(object.clone()),
                        ));
                    }
                }
                DeltaType::Deletion {
                    subject,
                    predicate,
                    object,
                } => {
                    if predicate.contains("domain") || predicate.contains("range") {
                        field_deltas.push(FieldDelta::new(
                            subject.clone(),
                            predicate.clone(),
                            ChangeType::Removed,
                            Some(object.clone()),
                            None,
                        ));
                    }
                }
                DeltaType::Modification {
                    subject,
                    predicate,
                    old_object,
                    new_object,
                } => {
                    field_deltas.push(FieldDelta::new(
                        subject.clone(),
                        predicate.clone(),
                        ChangeType::Modified,
                        Some(old_object.clone()),
                        Some(new_object.clone()),
                    ));
                }
            }
        }

        field_deltas
    }

    /// Check if this field delta affects a specific class
    pub fn affects_class(&self, class_iri: &str) -> bool {
        self.class.contains(class_iri)
    }
}

/// Metadata about which ontology elements a template uses
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateMetadata {
    /// Path to the template file
    pub path: PathBuf,
    /// Classes this template queries
    pub classes: HashSet<String>,
    /// Properties this template uses
    pub properties: HashSet<String>,
    /// SPARQL queries used (for advanced matching)
    pub queries: Vec<String>,
}

impl TemplateMetadata {
    /// Create new template metadata
    pub fn new(path: PathBuf) -> Self {
        Self {
            path,
            classes: HashSet::new(),
            properties: HashSet::new(),
            queries: Vec::new(),
        }
    }

    /// Add a class dependency
    pub fn add_class(&mut self, class: String) {
        self.classes.insert(class);
    }

    /// Add a property dependency
    pub fn add_property(&mut self, property: String) {
        self.properties.insert(property);
    }

    /// Add a SPARQL query
    pub fn add_query(&mut self, query: String) {
        self.queries.push(query);
    }
}

/// Selects templates affected by field-level changes
#[derive(Debug)]
pub struct TemplateSelector {
    /// Metadata for each template
    templates: HashMap<PathBuf, TemplateMetadata>,
}

impl TemplateSelector {
    /// Create a new template selector
    pub fn new() -> Self {
        Self {
            templates: HashMap::new(),
        }
    }

    /// Register a template with its metadata
    pub fn register(&mut self, metadata: TemplateMetadata) {
        self.templates.insert(metadata.path.clone(), metadata);
    }

    /// Select templates affected by the given field deltas
    pub fn select(&self, deltas: &[FieldDelta]) -> Vec<PathBuf> {
        let mut affected = HashSet::new();

        for (path, metadata) in &self.templates {
            for delta in deltas {
                // Check if template uses the affected class
                if metadata.classes.contains(&delta.class) {
                    affected.insert(path.clone());
                    break;
                }

                // Check if template uses the affected property
                if metadata.properties.contains(&delta.property) {
                    affected.insert(path.clone());
                    break;
                }
            }
        }

        affected.into_iter().collect()
    }

    /// Get impact score for a template (0.0-1.0)
    pub fn impact_score(&self, template: &PathBuf, deltas: &[FieldDelta]) -> f64 {
        let metadata = match self.templates.get(template) {
            Some(m) => m,
            None => return 0.0,
        };

        let mut score = 0.0;
        let mut matches = 0;

        for delta in deltas {
            if metadata.classes.contains(&delta.class) {
                matches += 1;
                score += match delta.change_type {
                    ChangeType::Added => 0.8,
                    ChangeType::Removed => 1.0,
                    ChangeType::Modified => 0.9,
                };
            }

            if metadata.properties.contains(&delta.property) {
                matches += 1;
                score += match delta.change_type {
                    ChangeType::Added => 0.6,
                    ChangeType::Removed => 0.8,
                    ChangeType::Modified => 0.7,
                };
            }
        }

        if matches == 0 {
            0.0
        } else {
            (score / matches as f64).min(1.0)
        }
    }
}

impl Default for TemplateSelector {
    fn default() -> Self {
        Self::new()
    }
}

/// Incremental generation manager that tracks file hashes
#[derive(Debug, Serialize, Deserialize)]
pub struct IncrementalGenerator {
    /// File hashes from previous generation
    file_hashes: HashMap<PathBuf, FileHash>,
}

/// Hash information for a generated file
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileHash {
    /// Path to the file
    pub path: PathBuf,
    /// SHA256 hash of file content
    pub content_hash: String,
    /// Hash of file metadata (permissions, etc.)
    pub metadata_hash: String,
    /// When this was generated
    pub generated_at: chrono::DateTime<chrono::Utc>,
}

impl IncrementalGenerator {
    /// Create a new incremental generator
    pub fn new() -> Self {
        Self {
            file_hashes: HashMap::new(),
        }
    }

    /// Load from a JSON file
    pub fn load(path: &PathBuf) -> Result<Self> {
        let content = std::fs::read_to_string(path)
            .map_err(|e| Error::new(&format!("Failed to read file hashes: {}", e)))?;
        serde_json::from_str(&content)
            .map_err(|e| Error::new(&format!("Failed to parse file hashes: {}", e)))
    }

    /// Save to a JSON file
    pub fn save(&self, path: &PathBuf) -> Result<()> {
        let content = serde_json::to_string_pretty(self)
            .map_err(|e| Error::new(&format!("Failed to serialize file hashes: {}", e)))?;
        std::fs::write(path, content)
            .map_err(|e| Error::new(&format!("Failed to write file hashes: {}", e)))
    }

    /// Check if a file should be generated based on content comparison
    pub fn should_generate(&self, path: &PathBuf, new_content: &str) -> bool {
        let previous = match self.file_hashes.get(path) {
            Some(h) => h,
            None => return true, // No previous hash, so generate
        };

        // Compute hash of new content
        let new_hash = Self::hash_content(new_content);

        // Compare hashes
        previous.content_hash != new_hash
    }

    /// Record a generated file
    pub fn record(&mut self, path: PathBuf, content: &str) {
        let content_hash = Self::hash_content(content);
        let metadata_hash = Self::hash_metadata(&path);

        self.file_hashes.insert(
            path.clone(),
            FileHash {
                path,
                content_hash,
                metadata_hash,
                generated_at: chrono::Utc::now(),
            },
        );
    }

    /// Compute SHA256 hash of content
    fn hash_content(content: &str) -> String {
        use sha2::{Digest, Sha256};
        let mut hasher = Sha256::new();
        hasher.update(content.as_bytes());
        format!("{:x}", hasher.finalize())
    }

    /// Compute hash of file metadata
    fn hash_metadata(path: &PathBuf) -> String {
        // For now, just hash the path
        // In the future, could include permissions, timestamps, etc.
        Self::hash_content(&path.display().to_string())
    }

    /// Get generation statistics
    pub fn stats(&self) -> GenerationStats {
        GenerationStats {
            total_files: self.file_hashes.len(),
            oldest_generation: self
                .file_hashes
                .values()
                .map(|h| h.generated_at)
                .min()
                .unwrap_or_else(chrono::Utc::now),
            newest_generation: self
                .file_hashes
                .values()
                .map(|h| h.generated_at)
                .max()
                .unwrap_or_else(chrono::Utc::now),
        }
    }
}

impl Default for IncrementalGenerator {
    fn default() -> Self {
        Self::new()
    }
}

/// Statistics about incremental generation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerationStats {
    /// Total number of tracked files
    pub total_files: usize,
    /// Oldest generation timestamp
    pub oldest_generation: chrono::DateTime<chrono::Utc>,
    /// Newest generation timestamp
    pub newest_generation: chrono::DateTime<chrono::Utc>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::Graph;
    use chicago_tdd_tools::test;

    test!(test_field_delta_creation, {
        let delta = FieldDelta::new(
            "http://example.org/User".to_string(),
            "http://example.org/name".to_string(),
            ChangeType::Added,
            None,
            Some("string".to_string()),
        );

        assert_eq!(delta.class, "http://example.org/User");
        assert_eq!(delta.change_type, ChangeType::Added);
        assert!(delta.affects_class("User"));

        Ok(())
    });

    test!(test_template_selector, {
        let mut selector = TemplateSelector::new();

        let mut metadata = TemplateMetadata::new(PathBuf::from("user_template.rs"));
        metadata.add_class("http://example.org/User".to_string());
        metadata.add_property("http://example.org/name".to_string());
        selector.register(metadata);

        let deltas = vec![FieldDelta::new(
            "http://example.org/User".to_string(),
            "http://example.org/email".to_string(),
            ChangeType::Added,
            None,
            Some("string".to_string()),
        )];

        let affected = selector.select(&deltas);
        assert_eq!(affected.len(), 1);
        assert_eq!(affected[0], PathBuf::from("user_template.rs"));

        Ok(())
    });

    test!(test_incremental_generator, {
        let mut gen = IncrementalGenerator::new();

        let path = PathBuf::from("test.rs");
        let content1 = "fn main() {}";
        let content2 = "fn main() { println!(\"hello\"); }";

        // First generation - should generate
        assert!(gen.should_generate(&path, content1));
        gen.record(path.clone(), content1);

        // Same content - should not generate
        assert!(!gen.should_generate(&path, content1));

        // Different content - should generate
        assert!(gen.should_generate(&path, content2));
        gen.record(path.clone(), content2);

        // Stats should track the file
        let stats = gen.stats();
        assert_eq!(stats.total_files, 1);

        Ok(())
    });

    test!(test_field_delta_from_graph_delta, {
        let baseline = Graph::new()?;
        baseline.insert_turtle(
            r#"
            @prefix : <http://example.org/> .
            @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
            :User a rdfs:Class .
            :name rdfs:domain :User .
        "#,
        )?;

        let current = Graph::new()?;
        current.insert_turtle(
            r#"
            @prefix : <http://example.org/> .
            @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
            :User a rdfs:Class .
            :name rdfs:domain :User .
            :email rdfs:domain :User .
        "#,
        )?;

        let graph_delta = GraphDelta::new(&baseline, &current)?;
        let field_deltas = FieldDelta::from_graph_delta(&graph_delta);

        // Should detect the email field addition
        assert!(!field_deltas.is_empty());
        assert!(field_deltas
            .iter()
            .any(|fd| fd.property.contains("domain") && fd.change_type == ChangeType::Added));

        Ok(())
    });
}
