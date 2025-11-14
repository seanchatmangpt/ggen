//! Snapshot management for delta-driven projection
//!
//! This module provides functionality to create and manage snapshots of generation
//! baselines, including RDF graph state, generated file states, and template metadata.
//! Snapshots enable rollback, drift detection, and three-way merging.
//!
//! ## Features
//!
//! - **Graph Snapshots**: Capture RDF graph state with content hashing
//! - **File Snapshots**: Track generated file content and metadata
//! - **Template Snapshots**: Store template states and rendering context
//! - **Region Tracking**: Identify generated vs manual regions in files
//! - **Snapshot Management**: Create, load, and compare snapshots
//! - **Drift Detection**: Detect when files have drifted from snapshots
//!
//! ## Snapshot Structure
//!
//! A snapshot contains:
//! - Graph state (triple count, content hash)
//! - File states (path, content hash, regions)
//! - Template states (path, content hash, variables)
//! - Metadata (creation time, description)
//!
//! ## Examples
//!
//! ### Creating a Snapshot
//!
//! ```rust,no_run
//! use ggen_core::snapshot::{Snapshot, SnapshotManager};
//! use ggen_core::graph::Graph;
//! use std::path::PathBuf;
//!
//! # fn main() -> anyhow::Result<()> {
//! let graph = Graph::new()?;
//! let files = vec![(PathBuf::from("output.rs"), "content".to_string())];
//! let templates = vec![(PathBuf::from("template.tmpl"), "template content".to_string())];
//!
//! let snapshot = Snapshot::new("baseline".to_string(), &graph, files, templates)?;
//! # Ok(())
//! # }
//! ```
//!
//! ### Managing Snapshots
//!
//! ```rust,no_run
//! use ggen_core::snapshot::SnapshotManager;
//! use std::path::Path;
//!
//! # fn main() -> anyhow::Result<()> {
//! let manager = SnapshotManager::new(Path::new(".ggen/snapshots"));
//! manager.save(&snapshot)?;
//!
//! let loaded = manager.load("baseline")?;
//! # Ok(())
//! # }
//! ```

use anyhow::Result;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fs::{self, File};
use std::io::{BufReader, BufWriter};
use std::path::{Path, PathBuf};

use crate::graph::Graph;

/// Represents a snapshot of a generation baseline
///
/// A `Snapshot` captures the complete state of a generation run, including:
/// - Graph state (hash, triple count)
/// - File states (paths, hashes, regions)
/// - Template states (paths, hashes, queries)
/// - Metadata (key-value pairs)
///
/// Snapshots enable rollback, drift detection, and three-way merging.
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_core::snapshot::Snapshot;
/// use ggen_core::graph::Graph;
/// use std::path::PathBuf;
///
/// # fn main() -> anyhow::Result<()> {
/// let graph = Graph::new()?;
/// graph.insert_turtle(r#"
///     @prefix ex: <http://example.org/> .
///     ex:alice a ex:Person .
/// "#)?;
///
/// let files = vec![
///     (PathBuf::from("output.rs"), "fn main() {}".to_string())
/// ];
/// let templates = vec![
///     (PathBuf::from("template.tmpl"), "template content".to_string())
/// ];
///
/// let snapshot = Snapshot::new("baseline".to_string(), &graph, files, templates)?;
/// println!("Snapshot: {} with {} files", snapshot.name, snapshot.files.len());
/// # Ok(())
/// # }
/// ```
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
    ///
    /// Captures the current state of a graph, generated files, and templates
    /// into a snapshot that can be used for rollback or drift detection.
    ///
    /// # Arguments
    ///
    /// * `name` - Unique name for this snapshot
    /// * `graph` - Current RDF graph state
    /// * `files` - Vector of (path, content) tuples for generated files
    /// * `templates` - Vector of (path, content) tuples for templates
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::snapshot::Snapshot;
    /// use ggen_core::graph::Graph;
    /// use std::path::PathBuf;
    ///
    /// # fn main() -> anyhow::Result<()> {
    /// let graph = Graph::new()?;
    /// let files = vec![(PathBuf::from("main.rs"), "fn main() {}".to_string())];
    /// let templates = vec![];
    ///
    /// let snapshot = Snapshot::new("v1.0".to_string(), &graph, files, templates)?;
    /// assert_eq!(snapshot.name, "v1.0");
    /// # Ok(())
    /// # }
    /// ```
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
    ///
    /// Returns `true` if the graph's hash matches the snapshot's graph hash,
    /// indicating the graph state hasn't changed since the snapshot was taken.
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::snapshot::Snapshot;
    /// use ggen_core::graph::Graph;
    ///
    /// # fn main() -> anyhow::Result<()> {
    /// let graph = Graph::new()?;
    /// let snapshot = Snapshot::new("test".to_string(), &graph, vec![], vec![])?;
    ///
    /// // Same graph should be compatible
    /// assert!(snapshot.is_compatible_with(&graph)?);
    ///
    /// // Modified graph should not be compatible
    /// graph.insert_turtle("@prefix ex: <http://example.org/> . ex:new a ex:Class .")?;
    /// assert!(!snapshot.is_compatible_with(&graph)?);
    /// # Ok(())
    /// # }
    /// ```
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
///
/// Captures the state of a generated file, including its content hash,
/// size, modification time, and regions (generated vs manual).
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_core::snapshot::FileSnapshot;
/// use std::path::PathBuf;
///
/// # fn main() -> anyhow::Result<()> {
/// let path = PathBuf::from("output.rs");
/// let content = "fn main() { println!(\"Hello\"); }";
///
/// let snapshot = FileSnapshot::new(path.clone(), content.to_string())?;
/// assert_eq!(snapshot.path, path);
/// assert!(!snapshot.hash.is_empty());
///
/// // Check if content has changed
/// assert!(!snapshot.has_changed(content));
/// assert!(snapshot.has_changed("different content"));
/// # Ok(())
/// # }
/// ```
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
    ///
    /// Compares the hash of new content with the snapshot's hash.
    /// Returns `true` if the content has changed since the snapshot was taken.
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::snapshot::FileSnapshot;
    /// use std::path::PathBuf;
    ///
    /// # fn main() -> anyhow::Result<()> {
    /// let snapshot = FileSnapshot::new(
    ///     PathBuf::from("file.rs"),
    ///     "original content".to_string()
    /// )?;
    ///
    /// assert!(!snapshot.has_changed("original content"));
    /// assert!(snapshot.has_changed("modified content"));
    /// # Ok(())
    /// # }
    /// ```
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
///
/// Provides persistence and management for snapshots, including save, load,
/// list, and delete operations.
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_core::snapshot::{SnapshotManager, Snapshot};
/// use ggen_core::graph::Graph;
/// use std::path::PathBuf;
///
/// # fn main() -> anyhow::Result<()> {
/// let manager = SnapshotManager::new(PathBuf::from(".ggen/snapshots"))?;
///
/// let graph = Graph::new()?;
/// let snapshot = Snapshot::new("baseline".to_string(), &graph, vec![], vec![])?;
///
/// // Save snapshot
/// manager.save(&snapshot)?;
///
/// // Load snapshot
/// let loaded = manager.load("baseline")?;
/// assert_eq!(loaded.name, "baseline");
///
/// // List snapshots
/// let snapshots = manager.list()?;
/// assert!(snapshots.contains(&"baseline".to_string()));
/// # Ok(())
/// # }
/// ```
pub struct SnapshotManager {
    /// Directory where snapshots are stored
    snapshot_dir: PathBuf,
}

impl SnapshotManager {
    /// Create a new snapshot manager
    ///
    /// Creates the snapshot directory if it doesn't exist.
    ///
    /// # Arguments
    ///
    /// * `snapshot_dir` - Directory path where snapshots will be stored
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::snapshot::SnapshotManager;
    /// use std::path::PathBuf;
    ///
    /// # fn main() -> anyhow::Result<()> {
    /// let manager = SnapshotManager::new(PathBuf::from(".ggen/snapshots"))?;
    /// // Directory is created automatically
    /// # Ok(())
    /// # }
    /// ```
    pub fn new(snapshot_dir: PathBuf) -> Result<Self> {
        fs::create_dir_all(&snapshot_dir)?;
        Ok(Self { snapshot_dir })
    }

    /// Save a snapshot to disk
    ///
    /// Persists a snapshot to a JSON file in the snapshot directory.
    /// The file is named `{snapshot.name}.json`.
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::snapshot::{SnapshotManager, Snapshot};
    /// use ggen_core::graph::Graph;
    /// use std::path::PathBuf;
    ///
    /// # fn main() -> anyhow::Result<()> {
    /// let manager = SnapshotManager::new(PathBuf::from(".ggen/snapshots"))?;
    /// let graph = Graph::new()?;
    /// let snapshot = Snapshot::new("my-snapshot".to_string(), &graph, vec![], vec![])?;
    ///
    /// manager.save(&snapshot)?;
    /// assert!(manager.exists("my-snapshot"));
    /// # Ok(())
    /// # }
    /// ```
    pub fn save(&self, snapshot: &Snapshot) -> Result<()> {
        let file_path = self.snapshot_dir.join(format!("{}.json", snapshot.name));
        let file = File::create(file_path)?;
        let writer = BufWriter::new(file);
        serde_json::to_writer_pretty(writer, snapshot)?;
        Ok(())
    }

    /// Load a snapshot from disk
    ///
    /// Loads a snapshot from a JSON file in the snapshot directory.
    ///
    /// # Arguments
    ///
    /// * `name` - Name of the snapshot to load (without `.json` extension)
    ///
    /// # Errors
    ///
    /// Returns an error if the snapshot file doesn't exist or cannot be parsed.
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::snapshot::{SnapshotManager, Snapshot};
    /// use ggen_core::graph::Graph;
    /// use std::path::PathBuf;
    ///
    /// # fn main() -> anyhow::Result<()> {
    /// let manager = SnapshotManager::new(PathBuf::from(".ggen/snapshots"))?;
    /// let graph = Graph::new()?;
    /// let snapshot = Snapshot::new("test".to_string(), &graph, vec![], vec![])?;
    /// manager.save(&snapshot)?;
    ///
    /// let loaded = manager.load("test")?;
    /// assert_eq!(loaded.name, "test");
    /// # Ok(())
    /// # }
    /// ```
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
