//! Drift Detector
//!
//! Detects when ontology changes make generated code stale.

use super::sync_state::{FileHashState, SyncState};
use crate::pqc::calculate_sha256_file;
use chrono::Utc;
use ggen_utils::error::Result;
use std::fs;
use std::path::{Path, PathBuf};

/// Type of change detected
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ChangeType {
    /// Ontology file changed
    Ontology,
    /// Manifest file changed
    Manifest,
    /// Inference rule changed
    InferenceRule(String),
    /// Import file changed
    Import(String),
    /// File is missing
    Missing(String),
    /// No previous state exists
    NoState,
}

impl std::fmt::Display for ChangeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ChangeType::Ontology => write!(f, "Ontology"),
            ChangeType::Manifest => write!(f, "Manifest"),
            ChangeType::InferenceRule(name) => write!(f, "Inference rule '{}'", name),
            ChangeType::Import(path) => write!(f, "Import '{}'", path),
            ChangeType::Missing(path) => write!(f, "Missing file '{}'", path),
            ChangeType::NoState => write!(f, "No previous state"),
        }
    }
}

/// Information about a detected change
#[derive(Debug, Clone)]
pub struct DriftChange {
    /// Type of change
    pub change_type: ChangeType,

    /// Old hash (if available)
    pub old_hash: Option<String>,

    /// New hash (if available)
    pub new_hash: Option<String>,

    /// Human-readable message
    pub message: String,
}

impl DriftChange {
    /// Create new drift change
    pub fn new(change_type: ChangeType, old_hash: Option<String>, new_hash: Option<String>) -> Self {
        let message = match &change_type {
            ChangeType::NoState => {
                "No previous sync state found. Run 'ggen sync' to create baseline.".to_string()
            }
            ChangeType::Missing(path) => {
                format!("File '{}' is missing", path)
            }
            _ => {
                if let (Some(old), Some(new)) = (&old_hash, &new_hash) {
                    format!(
                        "{} changed ({}..{}) since last sync",
                        change_type,
                        &old[..8.min(old.len())],
                        &new[..8.min(new.len())]
                    )
                } else {
                    format!("{} changed since last sync", change_type)
                }
            }
        };

        Self {
            change_type,
            old_hash,
            new_hash,
            message,
        }
    }
}

/// Drift detection status
#[derive(Debug, Clone)]
pub enum DriftStatus {
    /// No drift detected - code is up to date
    Clean,

    /// Drift detected - code is stale
    Drifted {
        /// List of changes detected
        changes: Vec<DriftChange>,

        /// Days since last sync
        days_since_sync: i64,
    },
}

impl DriftStatus {
    /// Check if drift was detected
    pub fn is_drifted(&self) -> bool {
        matches!(self, DriftStatus::Drifted { .. })
    }

    /// Get warning message if drifted
    pub fn warning_message(&self) -> Option<String> {
        match self {
            DriftStatus::Clean => None,
            DriftStatus::Drifted {
                changes,
                days_since_sync,
            } => {
                let mut msg = format!(
                    "⚠️  Ontology changed since last sync ({} days ago). Run 'ggen sync' to update.\n",
                    days_since_sync
                );

                for change in changes {
                    msg.push_str(&format!("   - {}\n", change.message));
                }

                Some(msg)
            }
        }
    }
}

/// Drift detector for tracking ontology changes
pub struct DriftDetector {
    /// Path to sync state file
    state_file: PathBuf,
}

impl DriftDetector {
    /// State file name
    const STATE_FILE: &'static str = "sync-state.json";

    /// Create new drift detector
    pub fn new(state_dir: &Path) -> Result<Self> {
        let state_file = state_dir.join(Self::STATE_FILE);

        Ok(Self {
            state_file,
        })
    }

    /// Check for drift between current files and last sync
    pub fn check_drift(
        &self,
        ontology_path: &Path,
        manifest_path: &Path,
    ) -> Result<DriftStatus> {
        // Load previous state
        let previous_state = match SyncState::load(&self.state_file) {
            Ok(state) => state,
            Err(_) => {
                // No previous state exists
                return Ok(DriftStatus::Drifted {
                    changes: vec![DriftChange::new(ChangeType::NoState, None, None)],
                    days_since_sync: 0,
                });
            }
        };

        let mut changes = Vec::new();

        // Check ontology
        if let Some(change) = self.check_file_drift(
            ontology_path,
            &previous_state.ontology,
            ChangeType::Ontology,
        )? {
            changes.push(change);
        }

        // Check manifest
        if let Some(change) = self.check_file_drift(
            manifest_path,
            &previous_state.manifest,
            ChangeType::Manifest,
        )? {
            changes.push(change);
        }

        // Check imports
        for (import_path, import_state) in &previous_state.imports {
            let path = PathBuf::from(import_path);
            if let Some(change) = self.check_file_drift(
                &path,
                import_state,
                ChangeType::Import(import_path.clone()),
            )? {
                changes.push(change);
            }
        }

        // Calculate days since last sync
        let days_since_sync = previous_state
            .age_since_sync()
            .map(|duration| duration.num_days())
            .unwrap_or(0);

        if changes.is_empty() {
            Ok(DriftStatus::Clean)
        } else {
            Ok(DriftStatus::Drifted {
                changes,
                days_since_sync,
            })
        }
    }

    /// Check drift for a single file
    fn check_file_drift(
        &self,
        file_path: &Path,
        previous_state: &FileHashState,
        change_type: ChangeType,
    ) -> Result<Option<DriftChange>> {
        // Check if file exists
        if !file_path.exists() {
            return Ok(Some(DriftChange::new(
                ChangeType::Missing(file_path.display().to_string()),
                Some(previous_state.hash.clone()),
                None,
            )));
        }

        // Calculate current hash
        let current_hash = calculate_sha256_file(file_path)?;

        // Compare hashes
        if current_hash != previous_state.hash {
            Ok(Some(DriftChange::new(
                change_type,
                Some(previous_state.hash.clone()),
                Some(current_hash),
            )))
        } else {
            Ok(None)
        }
    }

    /// Save current state after sync
    pub fn save_state(
        &self,
        ontology_path: &Path,
        manifest_path: &Path,
        files_synced: usize,
        duration_ms: u64,
    ) -> Result<()> {
        // Calculate hashes
        let ontology_hash = calculate_sha256_file(ontology_path)?;
        let manifest_hash = calculate_sha256_file(manifest_path)?;

        // Get file sizes
        let ontology_size = fs::metadata(ontology_path)
            .map(|m| m.len())
            .unwrap_or(0);
        let manifest_size = fs::metadata(manifest_path)
            .map(|m| m.len())
            .unwrap_or(0);

        // Create state
        let now = Utc::now();
        let ontology_state = FileHashState::new(ontology_hash, now, ontology_size);
        let manifest_state = FileHashState::new(manifest_hash, now, manifest_size);

        let mut state = SyncState::new(ontology_state, manifest_state);
        state.set_sync_metadata(files_synced, duration_ms);

        // Save state
        state.save(&self.state_file)
    }

    /// Save state with additional imports and rules
    pub fn save_state_with_details(
        &self,
        ontology_path: &Path,
        manifest_path: &Path,
        imports: Vec<PathBuf>,
        inference_rules: Vec<(String, String)>,
        files_synced: usize,
        duration_ms: u64,
    ) -> Result<()> {
        // Calculate hashes
        let ontology_hash = calculate_sha256_file(ontology_path)?;
        let manifest_hash = calculate_sha256_file(manifest_path)?;

        // Get file sizes
        let ontology_size = fs::metadata(ontology_path)
            .map(|m| m.len())
            .unwrap_or(0);
        let manifest_size = fs::metadata(manifest_path)
            .map(|m| m.len())
            .unwrap_or(0);

        // Create state
        let now = Utc::now();
        let ontology_state = FileHashState::new(ontology_hash, now, ontology_size);
        let manifest_state = FileHashState::new(manifest_hash, now, manifest_size);

        let mut state = SyncState::new(ontology_state, manifest_state);

        // Add imports
        for import_path in imports {
            if import_path.exists() {
                let import_hash = calculate_sha256_file(&import_path)?;
                let import_size = fs::metadata(&import_path)
                    .map(|m| m.len())
                    .unwrap_or(0);
                let import_state = FileHashState::new(import_hash, now, import_size);
                state.add_import(import_path.display().to_string(), import_state);
            }
        }

        // Add inference rules
        for (rule_name, rule_hash) in inference_rules {
            state.add_inference_rule(rule_name, rule_hash);
        }

        state.set_sync_metadata(files_synced, duration_ms);

        // Save state
        state.save(&self.state_file)
    }

    /// Get path to state file
    pub fn state_file_path(&self) -> &Path {
        &self.state_file
    }

    /// Check if state file exists
    pub fn has_state(&self) -> bool {
        self.state_file.exists()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::TempDir;

    fn create_test_file(dir: &Path, name: &str, content: &str) -> Result<PathBuf> {
        let path = dir.join(name);
        let mut file = fs::File::create(&path)
            .map_err(|e| Error::new(&format!("Failed to create test file: {}", e)))?;
        file.write_all(content.as_bytes())
            .map_err(|e| Error::new(&format!("Failed to write test file: {}", e)))?;
        Ok(path)
    }

    #[test]
    fn test_no_drift_when_clean() -> Result<()> {
        let temp_dir = TempDir::new()
            .map_err(|e| Error::new(&format!("Failed to create temp dir: {}", e)))?;

        let ontology_path = create_test_file(temp_dir.path(), "ontology.ttl", "content1")?;
        let manifest_path = create_test_file(temp_dir.path(), "ggen.toml", "content2")?;
        let state_dir = temp_dir.path().join(".ggen");
        fs::create_dir(&state_dir)
            .map_err(|e| Error::new(&format!("Failed to create state dir: {}", e)))?;

        let detector = DriftDetector::new(&state_dir)?;

        // Save initial state
        detector.save_state(&ontology_path, &manifest_path, 5, 1000)?;

        // Check drift (should be clean)
        let status = detector.check_drift(&ontology_path, &manifest_path)?;
        assert!(matches!(status, DriftStatus::Clean));

        Ok(())
    }

    #[test]
    fn test_drift_when_ontology_changed() -> Result<()> {
        let temp_dir = TempDir::new()
            .map_err(|e| Error::new(&format!("Failed to create temp dir: {}", e)))?;

        let ontology_path = create_test_file(temp_dir.path(), "ontology.ttl", "content1")?;
        let manifest_path = create_test_file(temp_dir.path(), "ggen.toml", "content2")?;
        let state_dir = temp_dir.path().join(".ggen");
        fs::create_dir(&state_dir)
            .map_err(|e| Error::new(&format!("Failed to create state dir: {}", e)))?;

        let detector = DriftDetector::new(&state_dir)?;

        // Save initial state
        detector.save_state(&ontology_path, &manifest_path, 5, 1000)?;

        // Modify ontology
        fs::write(&ontology_path, "modified content")
            .map_err(|e| Error::new(&format!("Failed to write file: {}", e)))?;

        // Check drift (should be drifted)
        let status = detector.check_drift(&ontology_path, &manifest_path)?;
        assert!(status.is_drifted());

        if let DriftStatus::Drifted { changes, .. } = status {
            assert_eq!(changes.len(), 1);
            assert!(matches!(changes[0].change_type, ChangeType::Ontology));
        }

        Ok(())
    }

    #[test]
    fn test_drift_when_no_state() -> Result<()> {
        let temp_dir = TempDir::new()
            .map_err(|e| Error::new(&format!("Failed to create temp dir: {}", e)))?;

        let ontology_path = create_test_file(temp_dir.path(), "ontology.ttl", "content1")?;
        let manifest_path = create_test_file(temp_dir.path(), "ggen.toml", "content2")?;
        let state_dir = temp_dir.path().join(".ggen");
        fs::create_dir(&state_dir)
            .map_err(|e| Error::new(&format!("Failed to create state dir: {}", e)))?;

        let detector = DriftDetector::new(&state_dir)?;

        // Check drift without saving state first
        let status = detector.check_drift(&ontology_path, &manifest_path)?;
        assert!(status.is_drifted());

        if let DriftStatus::Drifted { changes, .. } = status {
            assert_eq!(changes.len(), 1);
            assert!(matches!(changes[0].change_type, ChangeType::NoState));
        }

        Ok(())
    }

    #[test]
    fn test_save_state_with_details() -> Result<()> {
        let temp_dir = TempDir::new()
            .map_err(|e| Error::new(&format!("Failed to create temp dir: {}", e)))?;

        let ontology_path = create_test_file(temp_dir.path(), "ontology.ttl", "content1")?;
        let manifest_path = create_test_file(temp_dir.path(), "ggen.toml", "content2")?;
        let import_path = create_test_file(temp_dir.path(), "import.ttl", "import content")?;
        let state_dir = temp_dir.path().join(".ggen");
        fs::create_dir(&state_dir)
            .map_err(|e| Error::new(&format!("Failed to create state dir: {}", e)))?;

        let detector = DriftDetector::new(&state_dir)?;

        // Save state with imports and rules
        detector.save_state_with_details(
            &ontology_path,
            &manifest_path,
            vec![import_path.clone()],
            vec![("rule1".to_string(), "hash1".to_string())],
            5,
            1000,
        )?;

        // Load and verify
        let loaded_state = SyncState::load(&detector.state_file)?;
        assert_eq!(loaded_state.imports.len(), 1);
        assert_eq!(loaded_state.inference_rules.len(), 1);

        Ok(())
    }

    #[test]
    fn test_warning_message() -> Result<()> {
        let change = DriftChange::new(
            ChangeType::Ontology,
            Some("abc123".to_string()),
            Some("def456".to_string()),
        );

        let status = DriftStatus::Drifted {
            changes: vec![change],
            days_since_sync: 3,
        };

        let warning = status.warning_message();
        assert!(warning.is_some());
        assert!(warning.unwrap().contains("3 days ago"));

        Ok(())
    }
}
