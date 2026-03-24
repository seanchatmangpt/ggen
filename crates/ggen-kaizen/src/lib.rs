//! Kaizen continuous improvement tracking for ggen.
//!
//! This crate provides structures and functionality for tracking continuous
//! improvements using the PDCA (Plan-Do-Check-Act) cycle with quantitative metrics.

pub mod history;
pub mod metrics;
pub mod pdca;

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::fmt;
use thiserror::Error;

/// Errors that can occur during kaizen operations.
#[derive(Debug, Error)]
pub enum KaizenError {
    /// Invalid state transition attempted.
    #[error("Invalid state transition: {0}")]
    InvalidTransition(String),

    /// Missing required data.
    #[error("Missing required data: {0}")]
    MissingData(String),

    /// Invalid metric value.
    #[error("Invalid metric value: {0}")]
    InvalidMetric(String),

    /// Calculation error.
    #[error("Calculation error: {0}")]
    CalculationError(String),
}

/// Result type for kaizen operations.
pub type Result<T> = std::result::Result<T, KaizenError>;

/// Priority level for improvements.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum Priority {
    /// Low priority improvement.
    Low,
    /// Medium priority improvement.
    Medium,
    /// High priority improvement.
    High,
    /// Critical priority improvement.
    Critical,
}

impl fmt::Display for Priority {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Priority::Low => write!(f, "Low"),
            Priority::Medium => write!(f, "Medium"),
            Priority::High => write!(f, "High"),
            Priority::Critical => write!(f, "Critical"),
        }
    }
}

/// Category of improvement.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Category {
    /// Performance improvement.
    Performance,
    /// Quality improvement.
    Quality,
    /// Process improvement.
    Process,
    /// Safety improvement.
    Safety,
    /// Cost reduction.
    Cost,
    /// Other improvement type.
    Other(String),
}

impl fmt::Display for Category {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Category::Performance => write!(f, "Performance"),
            Category::Quality => write!(f, "Quality"),
            Category::Process => write!(f, "Process"),
            Category::Safety => write!(f, "Safety"),
            Category::Cost => write!(f, "Cost"),
            Category::Other(s) => write!(f, "Other: {}", s),
        }
    }
}

/// Core improvement structure.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Improvement {
    /// Unique identifier for the improvement.
    pub id: String,
    /// Title of the improvement.
    pub title: String,
    /// Detailed description.
    pub description: String,
    /// Category of improvement.
    pub category: Category,
    /// Priority level.
    pub priority: Priority,
    /// Owner responsible for the improvement.
    pub owner: String,
    /// Creation timestamp.
    pub created_at: DateTime<Utc>,
    /// Last updated timestamp.
    pub updated_at: DateTime<Utc>,
    /// Tags for categorization.
    pub tags: Vec<String>,
}

impl Improvement {
    /// Creates a new improvement.
    pub fn new(
        id: String,
        title: String,
        description: String,
        category: Category,
        priority: Priority,
        owner: String,
    ) -> Self {
        let now = Utc::now();
        Self {
            id,
            title,
            description,
            category,
            priority,
            owner,
            created_at: now,
            updated_at: now,
            tags: Vec::new(),
        }
    }

    /// Adds a tag to the improvement.
    pub fn add_tag(&mut self, tag: String) {
        if !self.tags.contains(&tag) {
            self.tags.push(tag);
            self.updated_at = Utc::now();
        }
    }

    /// Removes a tag from the improvement.
    pub fn remove_tag(&mut self, tag: &str) -> bool {
        if let Some(pos) = self.tags.iter().position(|t| t == tag) {
            self.tags.remove(pos);
            self.updated_at = Utc::now();
            true
        } else {
            false
        }
    }

    /// Updates the priority of the improvement.
    pub fn set_priority(&mut self, priority: Priority) {
        self.priority = priority;
        self.updated_at = Utc::now();
    }

    /// Updates the description.
    pub fn set_description(&mut self, description: String) {
        self.description = description;
        self.updated_at = Utc::now();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_improvement_creation() {
        // Arrange & Act
        let improvement = Improvement::new(
            "IMP-001".to_string(),
            "Optimize build time".to_string(),
            "Reduce build time from 15s to 10s".to_string(),
            Category::Performance,
            Priority::High,
            "team@example.com".to_string(),
        );

        // Assert
        assert_eq!(improvement.id, "IMP-001");
        assert_eq!(improvement.title, "Optimize build time");
        assert_eq!(improvement.priority, Priority::High);
        assert!(improvement.tags.is_empty());
    }

    #[test]
    fn test_add_tag() {
        // Arrange
        let mut improvement = Improvement::new(
            "IMP-002".to_string(),
            "Test".to_string(),
            "Description".to_string(),
            Category::Quality,
            Priority::Medium,
            "owner".to_string(),
        );

        // Act
        improvement.add_tag("rust".to_string());
        improvement.add_tag("performance".to_string());
        improvement.add_tag("rust".to_string()); // Duplicate

        // Assert
        assert_eq!(improvement.tags.len(), 2);
        assert!(improvement.tags.contains(&"rust".to_string()));
        assert!(improvement.tags.contains(&"performance".to_string()));
    }

    #[test]
    fn test_remove_tag() {
        // Arrange
        let mut improvement = Improvement::new(
            "IMP-003".to_string(),
            "Test".to_string(),
            "Description".to_string(),
            Category::Quality,
            Priority::Low,
            "owner".to_string(),
        );
        improvement.add_tag("tag1".to_string());
        improvement.add_tag("tag2".to_string());

        // Act
        let removed = improvement.remove_tag("tag1");
        let not_found = improvement.remove_tag("tag3");

        // Assert
        assert!(removed);
        assert!(!not_found);
        assert_eq!(improvement.tags.len(), 1);
        assert_eq!(improvement.tags[0], "tag2");
    }

    #[test]
    fn test_set_priority() {
        // Arrange
        let mut improvement = Improvement::new(
            "IMP-004".to_string(),
            "Test".to_string(),
            "Description".to_string(),
            Category::Process,
            Priority::Low,
            "owner".to_string(),
        );
        let original_updated = improvement.updated_at;

        // Act
        improvement.set_priority(Priority::Critical);

        // Assert
        assert_eq!(improvement.priority, Priority::Critical);
        assert!(improvement.updated_at > original_updated);
    }

    #[test]
    fn test_priority_ordering() {
        // Arrange & Act & Assert
        assert!(Priority::Low < Priority::Medium);
        assert!(Priority::Medium < Priority::High);
        assert!(Priority::High < Priority::Critical);
    }

    #[test]
    fn test_category_display() {
        // Arrange & Act & Assert
        assert_eq!(Category::Performance.to_string(), "Performance");
        assert_eq!(Category::Quality.to_string(), "Quality");
        assert_eq!(
            Category::Other("Custom".to_string()).to_string(),
            "Other: Custom"
        );
    }
}
