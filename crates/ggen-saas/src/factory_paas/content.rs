//! Content publishing pipeline

use super::PublicationStatus;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

/// Content item
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContentItem {
    /// Content ID
    pub id: Uuid,
    /// Title
    pub title: String,
    /// Body
    pub body: String,
    /// Author ID
    pub author_id: Uuid,
    /// Publication status
    pub status: PublicationStatus,
    /// Created timestamp
    pub created_at: DateTime<Utc>,
    /// Updated timestamp
    pub updated_at: DateTime<Utc>,
    /// Published timestamp
    pub published_at: Option<DateTime<Utc>>,
    /// Scheduled publish timestamp
    pub scheduled_for: Option<DateTime<Utc>>,
    /// Tags
    pub tags: Vec<String>,
}

impl ContentItem {
    /// Create a new content item in draft status
    pub fn new(title: String, body: String, author_id: Uuid) -> Self {
        let now = Utc::now();
        Self {
            id: Uuid::new_v4(),
            title,
            body,
            author_id,
            status: PublicationStatus::Draft,
            created_at: now,
            updated_at: now,
            published_at: None,
            scheduled_for: None,
            tags: Vec::new(),
        }
    }

    /// Update content body
    pub fn update(&mut self, title: String, body: String) {
        self.title = title;
        self.body = body;
        self.updated_at = Utc::now();
    }

    /// Add tags
    pub fn add_tags(&mut self, tags: Vec<String>) {
        self.tags.extend(tags);
        self.updated_at = Utc::now();
    }

    /// Schedule for publication
    pub fn schedule(&mut self, scheduled_for: DateTime<Utc>) -> Result<(), ContentError> {
        if scheduled_for <= Utc::now() {
            return Err(ContentError::InvalidScheduleTime);
        }
        self.scheduled_for = Some(scheduled_for);
        self.status = PublicationStatus::Scheduled;
        self.updated_at = Utc::now();
        Ok(())
    }

    /// Publish immediately
    pub fn publish(&mut self) -> Result<(), ContentError> {
        if self.title.is_empty() || self.body.is_empty() {
            return Err(ContentError::InvalidContent);
        }
        let now = Utc::now();
        self.published_at = Some(now);
        self.status = PublicationStatus::Published;
        self.updated_at = now;
        Ok(())
    }

    /// Archive content
    pub fn archive(&mut self) {
        self.status = PublicationStatus::Archived;
        self.updated_at = Utc::now();
    }
}

/// Content publishing pipeline
pub struct ContentPipeline {
    /// Content items storage
    content: HashMap<Uuid, ContentItem>,
}

impl ContentPipeline {
    /// Create a new content pipeline
    pub fn new() -> Self {
        Self {
            content: HashMap::new(),
        }
    }

    /// Add content to pipeline
    pub fn add_content(&mut self, item: ContentItem) -> Uuid {
        let id = item.id;
        self.content.insert(id, item);
        id
    }

    /// Get content by ID
    pub fn get_content(&self, id: &Uuid) -> Option<&ContentItem> {
        self.content.get(id)
    }

    /// Get mutable content by ID
    pub fn get_content_mut(&mut self, id: &Uuid) -> Option<&mut ContentItem> {
        self.content.get_mut(id)
    }

    /// List content by status
    pub fn list_by_status(&self, status: PublicationStatus) -> Vec<&ContentItem> {
        self.content
            .values()
            .filter(|item| item.status == status)
            .collect()
    }

    /// Process scheduled publications (publish items due now)
    pub fn process_scheduled(&mut self) -> Vec<Uuid> {
        let now = Utc::now();
        let mut published_ids = Vec::new();

        for item in self.content.values_mut() {
            if item.status == PublicationStatus::Scheduled {
                if let Some(scheduled_for) = item.scheduled_for {
                    if scheduled_for <= now {
                        if item.publish().is_ok() {
                            published_ids.push(item.id);
                        }
                    }
                }
            }
        }

        published_ids
    }

    /// Get total content count
    pub fn total_content(&self) -> usize {
        self.content.len()
    }

    /// Validate content before publication
    pub fn validate_content(&self, id: &Uuid) -> Result<(), ContentError> {
        let item = self.content.get(id).ok_or(ContentError::NotFound)?;

        if item.title.is_empty() {
            return Err(ContentError::EmptyTitle);
        }

        if item.body.is_empty() {
            return Err(ContentError::EmptyBody);
        }

        if item.body.len() > 100_000 {
            return Err(ContentError::ContentTooLarge);
        }

        Ok(())
    }
}

impl Default for ContentPipeline {
    fn default() -> Self {
        Self::new()
    }
}

/// Content errors
#[derive(Debug, Clone, thiserror::Error)]
pub enum ContentError {
    #[error("Content not found")]
    NotFound,
    #[error("Invalid schedule time (must be in the future)")]
    InvalidScheduleTime,
    #[error("Invalid content (title and body required)")]
    InvalidContent,
    #[error("Empty title")]
    EmptyTitle,
    #[error("Empty body")]
    EmptyBody,
    #[error("Content too large (max 100KB)")]
    ContentTooLarge,
}
