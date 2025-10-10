//! Snapshot management for delta-driven projection
//!
//! This module provides functionality to:
//! - Store baselines of graph and file states
//! - Track generation metadata and hashes
//! - Support rollback and drift detection

use anyhow::Result;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fs::{self, File};
use std::io::{BufReader, BufWriter};
use std::path::{Path, PathBuf};

use crate::graph::Graph;

/// Represents a snapshot of a generation baseline
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Snapshot {
    /// Unique name for this snapshot
    pub name: String,
    /// When this snapshot was created
    pub created_at: DateTime<Utc>,
    /// Graph state information
    pub graph: GraphSnapshot,
    /// File state information
    pub files: Vec<FileSnapshot>,
    /// Associated templates
    pub templates: Vec<TemplateSnapshot>,
    /// Additional metadata
    pub metadata: BTreeMap<String, String>,
}

impl Snapshot {
    /// Create a new snapshot from current state
    pub fn new(
        name: String, graph: &Graph, files: Vec<(PathBuf, String)>,
        templates: Vec<(PathBuf, String)>,
    ) -> Result<Self> {
        let now = Utc::now();

        let graph_snapshot = GraphSnapshot::from_graph(graph)?;

        let file_snapshots = files
            .into_iter()
            .map(|(path, content)| FileSnapshot::new(path, content))
            .collect::<Result<Vec<_>>>()?;

        let template_snapshots = templates
            .into_iter()
            .map(|(path, content)| TemplateSnapshot::new(path, content))
            .collect::<Result<Vec<_>>>()?;

        Ok(Self {
            name,
            created_at: now,
            graph: graph_snapshot,
            files: file_snapshots,
            templates: template_snapshots,
            metadata: BTreeMap::new(),
        })
    }

    /// Check if this snapshot is compatible with a graph
    pub fn is_compatible_with(&self, graph: &Graph) -> Result<bool> {
        let current_hash = graph.compute_hash()?;
        Ok(self.graph.hash == current_hash)
    }

    /// Find a file snapshot by path
    pub fn find_file(&self, path: &Path) -> Option<&FileSnapshot> {
        self.files.iter().find(|f| f.path == path)
    }

    /// Find a template snapshot by path
    pub fn find_template(&self, path: &Path) -> Option<&TemplateSnapshot> {
        self.templates.iter().find(|t| t.path == path)
    }

    /// Add metadata to this snapshot
    pub fn add_metadata(&mut self, key: String, value: String) {
        self.metadata.insert(key, value);
    }

    /// Get metadata value
    pub fn get_metadata(&self, key: &str) -> Option<&String> {
        self.metadata.get(key)
    }
}

/// Snapshot of graph state
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GraphSnapshot {
    /// Hash of the graph content
    pub hash: String,
    /// Number of triples in the graph
    pub triple_count: usize,
    /// Source files used to build the graph
    pub sources: Vec<String>,
    /// When the graph was loaded
    pub loaded_at: DateTime<Utc>,
}

impl GraphSnapshot {
    /// Create from a live graph
    pub fn from_graph(graph: &Graph) -> Result<Self> {
        let hash = graph.compute_hash()?;
        let triple_count = graph.len();

        Ok(Self {
            hash,
            triple_count,
            sources: Vec::new(), // Would need to track source files
            loaded_at: Utc::now(),
        })
    }
}

/// Snapshot of file state
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileSnapshot {
    /// Path to the file
    pub path: PathBuf,
    /// Hash of the file content
    pub hash: String,
    /// File size in bytes
    pub size: u64,
    /// When the file was last modified
    pub modified_at: DateTime<Utc>,
    /// Generated regions in the file (for three-way merge)
    pub generated_regions: Vec<Region>,
    /// Manual regions in the file (for three-way merge)
    pub manual_regions: Vec<Region>,
}

impl FileSnapshot {
    /// Create from file path and content
    pub fn new(path: PathBuf, content: String) -> Result<Self> {
        let metadata = fs::metadata(&path)?;
        let hash = Self::compute_hash(&content);

        Ok(Self {
            path,
            hash,
            size: metadata.len(),
            modified_at: metadata.modified()?.into(),
            generated_regions: Self::detect_regions(&content),
            manual_regions: Self::detect_regions(&content),
        })
    }

    /// Compute hash of content
    fn compute_hash(content: &str) -> String {
        use sha2::{Digest, Sha256};
        let mut hasher = Sha256::new();
        hasher.update(content.as_bytes());
        format!("sha256:{:x}", hasher.finalize())
    }

    /// Detect generated and manual regions in content
    /// This is a simplified implementation - real version would parse markers
    fn detect_regions(_content: &str) -> Vec<Region> {
        Vec::new() // Placeholder
    }

    /// Check if file content has changed
    pub fn has_changed(&self, new_content: &str) -> bool {
        let new_hash = Self::compute_hash(new_content);
        self.hash != new_hash
    }
}

/// Snapshot of template state
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateSnapshot {
    /// Path to the template
    pub path: PathBuf,
    /// Hash of the template content
    pub hash: String,
    /// SPARQL queries in the template
    pub queries: Vec<String>,
    /// When the template was processed
    pub processed_at: DateTime<Utc>,
}

impl TemplateSnapshot {
    /// Create from template path and content
    pub fn new(path: PathBuf, content: String) -> Result<Self> {
        let hash = Self::compute_hash(&content);
        let queries = Self::extract_queries(&content);

        Ok(Self {
            path,
            hash,
            queries,
            processed_at: Utc::now(),
        })
    }

    /// Compute hash of content
    fn compute_hash(content: &str) -> String {
        use sha2::{Digest, Sha256};
        let mut hasher = Sha256::new();
        hasher.update(content.as_bytes());
        format!("sha256:{:x}", hasher.finalize())
    }

    /// Extract SPARQL queries from template (simplified)
    fn extract_queries(_content: &str) -> Vec<String> {
        Vec::new() // Placeholder - would parse frontmatter
    }
}

/// Represents a region in a file (for three-way merge)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Region {
    /// Start line (1-indexed)
    pub start: usize,
    /// End line (1-indexed)
    pub end: usize,
    /// Type of region
    pub region_type: RegionType,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum RegionType {
    Generated,
    Manual,
}

/// Manager for snapshot operations
pub struct SnapshotManager {
    /// Directory where snapshots are stored
    snapshot_dir: PathBuf,
}

impl SnapshotManager {
    /// Create a new snapshot manager
    pub fn new(snapshot_dir: PathBuf) -> Result<Self> {
        fs::create_dir_all(&snapshot_dir)?;
        Ok(Self { snapshot_dir })
    }

    /// Save a snapshot to disk
    pub fn save(&self, snapshot: &Snapshot) -> Result<()> {
        let file_path = self.snapshot_dir.join(format!("{}.json", snapshot.name));
        let file = File::create(file_path)?;
        let writer = BufWriter::new(file);
        serde_json::to_writer_pretty(writer, snapshot)?;
        Ok(())
    }

    /// Load a snapshot from disk
    pub fn load(&self, name: &str) -> Result<Snapshot> {
        let file_path = self.snapshot_dir.join(format!("{}.json", name));
        let file = File::open(file_path)?;
        let reader = BufReader::new(file);
        let snapshot = serde_json::from_reader(reader)?;
        Ok(snapshot)
    }

    /// List all available snapshots
    pub fn list(&self) -> Result<Vec<String>> {
        let mut snapshots = Vec::new();
        let entries = fs::read_dir(&self.snapshot_dir)?;

        for entry in entries {
            let entry = entry?;
            let path = entry.path();
            if let Some(name) = path.file_stem().and_then(|s| s.to_str()) {
                snapshots.push(name.to_string());
            }
        }

        snapshots.sort();
        Ok(snapshots)
    }

    /// Delete a snapshot
    pub fn delete(&self, name: &str) -> Result<()> {
        let file_path = self.snapshot_dir.join(format!("{}.json", name));
        if file_path.exists() {
            fs::remove_file(file_path)?;
        }
        Ok(())
    }

    /// Check if a snapshot exists
    pub fn exists(&self, name: &str) -> bool {
        let file_path = self.snapshot_dir.join(format!("{}.json", name));
        file_path.exists()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::Graph;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn test_snapshot_creation() -> Result<()> {
        let graph = Graph::new()?;
        graph.insert_turtle("@prefix : <http://example.org/> . :test a :Class .")?;

        let temp_dir = tempdir()?;
        let test_file = temp_dir.path().join("test.txt");
        let test_template = temp_dir.path().join("test.tmpl");

        // Create the actual files
        fs::write(&test_file, "test content")?;
        fs::write(&test_template, "template content")?;

        let files = vec![(test_file, "test content".to_string())];
        let templates = vec![(test_template, "template content".to_string())];

        let snapshot = Snapshot::new("test_snapshot".to_string(), &graph, files, templates)?;

        assert_eq!(snapshot.name, "test_snapshot");
        assert_eq!(snapshot.files.len(), 1);
        assert_eq!(snapshot.templates.len(), 1);
        assert!(!snapshot.graph.hash.is_empty());

        Ok(())
    }

    #[test]
    fn test_snapshot_manager() -> Result<()> {
        let temp_dir = tempdir()?;
        let manager = SnapshotManager::new(temp_dir.path().to_path_buf())?;

        let graph = Graph::new()?;
        graph.insert_turtle("@prefix : <http://example.org/> . :test a :Class .")?;

        let snapshot = Snapshot::new("manager_test".to_string(), &graph, vec![], vec![])?;

        // Save snapshot
        manager.save(&snapshot)?;
        assert!(manager.exists("manager_test"));

        // Load snapshot
        let loaded = manager.load("manager_test")?;
        assert_eq!(loaded.name, snapshot.name);

        // List snapshots
        let list = manager.list()?;
        assert!(list.contains(&"manager_test".to_string()));

        // Delete snapshot
        manager.delete("manager_test")?;
        assert!(!manager.exists("manager_test"));

        Ok(())
    }

    #[test]
    fn test_file_snapshot() -> Result<()> {
        let temp_dir = tempdir()?;
        let file_path = temp_dir.path().join("test.txt");
        fs::write(&file_path, "test content")?;

        let snapshot = FileSnapshot::new(file_path.clone(), "test content".to_string())?;

        assert_eq!(snapshot.path, file_path);
        assert!(!snapshot.hash.is_empty());
        assert!(snapshot.size > 0);

        // Test change detection
        assert!(!snapshot.has_changed("test content"));
        assert!(snapshot.has_changed("different content"));

        Ok(())
    }

    #[test]
    fn test_template_snapshot() -> Result<()> {
        let temp_dir = tempdir()?;
        let template_path = temp_dir.path().join("test.tmpl");

        let snapshot = TemplateSnapshot::new(
            template_path.clone(),
            "SELECT ?s WHERE { ?s ?p ?o }".to_string(),
        )?;

        assert_eq!(snapshot.path, template_path);
        assert!(!snapshot.hash.is_empty());

        Ok(())
    }
}
