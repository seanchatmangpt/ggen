//! P2P Marketplace Types

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Package metadata for marketplace
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Package {
    pub id: String,
    pub name: String,
    pub version: String,
    pub description: String,
    pub category: String,
    pub tags: Vec<String>,
    pub author: String,
    pub license: String,
    pub repository: Option<String>,
    pub homepage: Option<String>,
    pub dependencies: Vec<String>,
    pub content_hash: String,
    pub size: u64,
    pub created_at: i64,
    pub updated_at: i64,
    pub downloads: u64,
    pub metadata: PackageMetadata,
}

/// Extended package metadata
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct PackageMetadata {
    pub readme: Option<String>,
    pub keywords: Vec<String>,
    pub maintainers: Vec<String>,
    pub features: HashMap<String, String>,
    pub platforms: Vec<String>,
    pub rust_version: Option<String>,
}

/// Search query for package discovery
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Query {
    pub keywords: Vec<String>,
    pub category: Option<String>,
    pub tags: Vec<String>,
    pub limit: usize,
    pub min_downloads: Option<u64>,
}

impl Query {
    pub fn new(keywords: Vec<String>) -> Self {
        Self {
            keywords,
            category: None,
            tags: Vec::new(),
            limit: 20,
            min_downloads: None,
        }
    }

    pub fn with_category(mut self, category: String) -> Self {
        self.category = Some(category);
        self
    }

    pub fn with_tags(mut self, tags: Vec<String>) -> Self {
        self.tags = tags;
        self
    }

    pub fn with_limit(mut self, limit: usize) -> Self {
        self.limit = limit;
        self
    }
}

/// Search result with ranking
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SearchResult {
    pub package: Package,
    pub score: f64,
    pub peer_count: usize,
}

/// Package update event
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageUpdate {
    pub package_id: String,
    pub version: String,
    pub update_type: UpdateType,
    pub timestamp: i64,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum UpdateType {
    NewPackage,
    NewVersion,
    MetadataUpdate,
    Deprecated,
}

/// Peer information
#[derive(Debug, Clone)]
pub struct PeerInfo {
    pub peer_id: String,
    pub addresses: Vec<String>,
    pub packages: Vec<String>,
    pub last_seen: i64,
}

impl Default for PackageMetadata {
    fn default() -> Self {
        Self {
            readme: None,
            keywords: Vec::new(),
            maintainers: Vec::new(),
            features: HashMap::new(),
            platforms: Vec::new(),
            rust_version: None,
        }
    }
}

impl Package {
    pub fn new(name: String, version: String) -> Self {
        let now = chrono::Utc::now().timestamp();
        Self {
            id: format!("{}@{}", name, version),
            name,
            version,
            description: String::new(),
            category: String::from("uncategorized"),
            tags: Vec::new(),
            author: String::new(),
            license: String::from("MIT"),
            repository: None,
            homepage: None,
            dependencies: Vec::new(),
            content_hash: String::new(),
            size: 0,
            created_at: now,
            updated_at: now,
            downloads: 0,
            metadata: PackageMetadata::default(),
        }
    }

    pub fn matches_query(&self, query: &Query) -> bool {
        // Check category
        if let Some(ref cat) = query.category {
            if &self.category != cat {
                return false;
            }
        }

        // Check minimum downloads
        if let Some(min) = query.min_downloads {
            if self.downloads < min {
                return false;
            }
        }

        // Check keywords (must match at least one)
        if !query.keywords.is_empty() {
            let matches_keyword = query.keywords.iter().any(|kw| {
                let kw_lower = kw.to_lowercase();
                self.name.to_lowercase().contains(&kw_lower)
                    || self.description.to_lowercase().contains(&kw_lower)
                    || self.tags.iter().any(|t| t.to_lowercase().contains(&kw_lower))
            });
            if !matches_keyword {
                return false;
            }
        }

        // Check tags (must match all requested tags)
        if !query.tags.is_empty() {
            let has_all_tags = query.tags.iter().all(|qt| {
                self.tags.iter().any(|t| t.eq_ignore_ascii_case(qt))
            });
            if !has_all_tags {
                return false;
            }
        }

        true
    }

    pub fn calculate_score(&self, query: &Query) -> f64 {
        let mut score = 0.0;

        // Keyword relevance (0-100 points)
        for keyword in &query.keywords {
            let kw_lower = keyword.to_lowercase();
            if self.name.to_lowercase() == kw_lower {
                score += 50.0; // Exact name match
            } else if self.name.to_lowercase().contains(&kw_lower) {
                score += 25.0; // Partial name match
            }
            if self.description.to_lowercase().contains(&kw_lower) {
                score += 10.0; // Description match
            }
        }

        // Tag matching (0-20 points)
        let matching_tags = query.tags.iter().filter(|qt| {
            self.tags.iter().any(|t| t.eq_ignore_ascii_case(qt))
        }).count();
        score += (matching_tags as f64 / query.tags.len().max(1) as f64) * 20.0;

        // Popularity bonus (0-30 points, logarithmic scale)
        if self.downloads > 0 {
            score += (self.downloads as f64).log10().min(3.0) * 10.0;
        }

        score
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_package_matches_query() {
        let package = Package {
            name: "rust-axum-service".to_string(),
            version: "1.0.0".to_string(),
            category: "web".to_string(),
            tags: vec!["rust".to_string(), "web".to_string()],
            downloads: 1000,
            ..Package::new("test".to_string(), "1.0.0".to_string())
        };

        let query = Query::new(vec!["rust".to_string()])
            .with_category("web".to_string())
            .with_tags(vec!["rust".to_string()]);

        assert!(package.matches_query(&query));
    }

    #[test]
    fn test_package_score_calculation() {
        let package = Package {
            name: "rust-axum".to_string(),
            description: "Web service framework".to_string(),
            tags: vec!["rust".to_string(), "web".to_string()],
            downloads: 10000,
            ..Package::new("test".to_string(), "1.0.0".to_string())
        };

        let query = Query::new(vec!["rust".to_string()])
            .with_tags(vec!["web".to_string()]);

        let score = package.calculate_score(&query);
        assert!(score > 0.0);
    }
}
