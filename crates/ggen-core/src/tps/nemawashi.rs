//! Nemawashi - Consensus building and collaboration
//!
//! This module implements collaborative annotation and consensus-building features,
//! following the TPS principle of "laying the groundwork" for decisions.

use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Location where an annotation is anchored
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum AnnotationAnchor {
    /// Anchored to a file and line number
    FileLine {
        /// Path to the file
        file: PathBuf,
        /// Line number (1-based)
        line: usize,
    },
    /// Anchored to an RDF triple
    RdfTriple {
        /// Subject IRI
        subject: String,
        /// Predicate IRI
        predicate: String,
        /// Object value or IRI
        object: String,
    },
    /// Anchored to a template
    Template {
        /// Path to the template
        path: PathBuf,
        /// Section or line in template
        location: String,
    },
}

/// An annotation/comment on code or ontology
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Annotation {
    /// Unique annotation ID
    pub id: String,
    /// Where this annotation is anchored
    pub anchor: AnnotationAnchor,
    /// Annotation text
    pub text: String,
    /// Author
    pub author: String,
    /// Creation timestamp
    pub created_at: chrono::DateTime<chrono::Utc>,
    /// Whether this has been resolved
    pub resolved: bool,
    /// Tags for categorization
    pub tags: Vec<String>,
}

impl Annotation {
    /// Create a new annotation
    pub fn new(anchor: AnnotationAnchor, text: String, author: String) -> Self {
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            anchor,
            text,
            author,
            created_at: chrono::Utc::now(),
            resolved: false,
            tags: Vec::new(),
        }
    }

    /// Mark as resolved
    pub fn resolve(&mut self) {
        self.resolved = true;
    }

    /// Add a tag
    pub fn add_tag(&mut self, tag: String) {
        if !self.tags.contains(&tag) {
            self.tags.push(tag);
        }
    }

    /// Check if annotation is for a specific file
    pub fn is_for_file(&self, file: &PathBuf) -> bool {
        match &self.anchor {
            AnnotationAnchor::FileLine { file: f, .. } => f == file,
            AnnotationAnchor::Template { path, .. } => path == file,
            _ => false,
        }
    }
}

/// Store for managing annotations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnnotationStore {
    /// All annotations
    annotations: Vec<Annotation>,
}

impl AnnotationStore {
    /// Create a new annotation store
    pub fn new() -> Self {
        Self {
            annotations: Vec::new(),
        }
    }

    /// Load from JSON file
    pub fn load(path: &PathBuf) -> Result<Self> {
        let content = std::fs::read_to_string(path)
            .map_err(|e| Error::new(&format!("Failed to read annotations: {}", e)))?;
        serde_json::from_str(&content)
            .map_err(|e| Error::new(&format!("Failed to parse annotations: {}", e)))
    }

    /// Save to JSON file
    pub fn save(&self, path: &PathBuf) -> Result<()> {
        let content = serde_json::to_string_pretty(self)
            .map_err(|e| Error::new(&format!("Failed to serialize annotations: {}", e)))?;

        // Create parent directory if needed
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).map_err(|e| {
                Error::new(&format!("Failed to create annotations directory: {}", e))
            })?;
        }

        std::fs::write(path, content)
            .map_err(|e| Error::new(&format!("Failed to write annotations: {}", e)))
    }

    /// Add a new annotation
    pub fn add(&mut self, annotation: Annotation) {
        self.annotations.push(annotation);
    }

    /// Get all annotations for a file
    pub fn for_file(&self, file: &PathBuf) -> Vec<&Annotation> {
        self.annotations
            .iter()
            .filter(|a| a.is_for_file(file))
            .collect()
    }

    /// Get all unresolved annotations
    pub fn unresolved(&self) -> Vec<&Annotation> {
        self.annotations.iter().filter(|a| !a.resolved).collect()
    }

    /// Get annotations by author
    pub fn by_author(&self, author: &str) -> Vec<&Annotation> {
        self.annotations
            .iter()
            .filter(|a| a.author == author)
            .collect()
    }

    /// Get annotations by tag
    pub fn by_tag(&self, tag: &str) -> Vec<&Annotation> {
        self.annotations
            .iter()
            .filter(|a| a.tags.contains(&tag.to_string()))
            .collect()
    }

    /// Resolve an annotation by ID
    pub fn resolve(&mut self, id: &str) -> Result<()> {
        let annotation = self
            .annotations
            .iter_mut()
            .find(|a| a.id == id)
            .ok_or_else(|| Error::new(&format!("Annotation {} not found", id)))?;

        annotation.resolve();
        Ok(())
    }

    /// Get statistics
    pub fn stats(&self) -> AnnotationStats {
        let total = self.annotations.len();
        let unresolved = self.unresolved().len();

        AnnotationStats {
            total_annotations: total,
            unresolved_count: unresolved,
            resolved_count: total - unresolved,
        }
    }
}

impl Default for AnnotationStore {
    fn default() -> Self {
        Self::new()
    }
}

/// Annotation statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnnotationStats {
    /// Total annotations
    pub total_annotations: usize,
    /// Number of unresolved annotations
    pub unresolved_count: usize,
    /// Number of resolved annotations
    pub resolved_count: usize,
}

/// Approval request for schema changes
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApprovalRequest {
    /// Unique request ID
    pub id: String,
    /// Description of changes
    pub description: String,
    /// Created by
    pub created_by: String,
    /// Creation timestamp
    pub created_at: chrono::DateTime<chrono::Utc>,
    /// Current status
    pub status: ApprovalStatus,
    /// Required approvers
    pub required_approvers: Vec<String>,
    /// Actual approvals
    pub approvals: Vec<Approval>,
}

/// Status of an approval request
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ApprovalStatus {
    /// Waiting for approvals
    Pending,
    /// Approved
    Approved,
    /// Rejected
    Rejected,
    /// Expired (too old)
    Expired,
}

/// A single approval
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Approval {
    /// Who approved
    pub approver: String,
    /// When approved
    pub approved_at: chrono::DateTime<chrono::Utc>,
    /// Approval or rejection
    pub approved: bool,
    /// Optional comment
    pub comment: Option<String>,
}

impl ApprovalRequest {
    /// Create a new approval request
    pub fn new(description: String, created_by: String, required_approvers: Vec<String>) -> Self {
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            description,
            created_by,
            created_at: chrono::Utc::now(),
            status: ApprovalStatus::Pending,
            required_approvers,
            approvals: Vec::new(),
        }
    }

    /// Add an approval
    pub fn approve(&mut self, approver: String, comment: Option<String>) {
        self.approvals.push(Approval {
            approver,
            approved_at: chrono::Utc::now(),
            approved: true,
            comment,
        });

        self.update_status();
    }

    /// Add a rejection
    pub fn reject(&mut self, approver: String, comment: Option<String>) {
        self.approvals.push(Approval {
            approver,
            approved_at: chrono::Utc::now(),
            approved: false,
            comment,
        });

        self.status = ApprovalStatus::Rejected;
    }

    /// Update status based on approvals
    fn update_status(&mut self) {
        if self.status != ApprovalStatus::Pending {
            return;
        }

        let approved_count = self.approvals.iter().filter(|a| a.approved).count();

        if approved_count >= self.required_approvers.len() {
            self.status = ApprovalStatus::Approved;
        }
    }

    /// Check if fully approved
    pub fn is_approved(&self) -> bool {
        self.status == ApprovalStatus::Approved
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chicago_tdd_tools::test;

    test!(test_annotation_creation, {
        let annotation = Annotation::new(
            AnnotationAnchor::FileLine {
                file: PathBuf::from("src/main.rs"),
                line: 42,
            },
            "This needs review".to_string(),
            "alice@example.com".to_string(),
        );

        assert!(!annotation.resolved);
        assert_eq!(annotation.author, "alice@example.com");

        Ok(())
    });

    test!(test_annotation_store, {
        let mut store = AnnotationStore::new();

        let annotation = Annotation::new(
            AnnotationAnchor::FileLine {
                file: PathBuf::from("src/main.rs"),
                line: 10,
            },
            "Review this".to_string(),
            "bob@example.com".to_string(),
        );

        store.add(annotation.clone());

        let for_file = store.for_file(&PathBuf::from("src/main.rs"));
        assert_eq!(for_file.len(), 1);

        let unresolved = store.unresolved();
        assert_eq!(unresolved.len(), 1);

        Ok(())
    });

    test!(test_approval_request, {
        let mut request = ApprovalRequest::new(
            "Add new User class".to_string(),
            "alice@example.com".to_string(),
            vec!["bob@example.com".to_string(), "carol@example.com".to_string()],
        );

        assert_eq!(request.status, ApprovalStatus::Pending);

        // Add first approval
        request.approve("bob@example.com".to_string(), None);
        assert_eq!(request.status, ApprovalStatus::Pending);

        // Add second approval
        request.approve("carol@example.com".to_string(), None);
        assert_eq!(request.status, ApprovalStatus::Approved);
        assert!(request.is_approved());

        Ok(())
    });

    test!(test_approval_rejection, {
        let mut request = ApprovalRequest::new(
            "Remove User class".to_string(),
            "alice@example.com".to_string(),
            vec!["bob@example.com".to_string()],
        );

        request.reject(
            "bob@example.com".to_string(),
            Some("This breaks backward compatibility".to_string()),
        );

        assert_eq!(request.status, ApprovalStatus::Rejected);
        assert!(!request.is_approved());

        Ok(())
    });
}
