//! Marketplace registry reader for querying packages.toml
//!
//! This module provides functionality to read and query the marketplace
//! registry file (`marketplace/registry/packages.toml`), enabling the CLI
//! to discover and display real package information.

use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

/// Registry file structure
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Registry {
    pub version: String,
    #[serde(rename = "package")]
    pub packages: Vec<Package>,
}

/// Package entry in registry
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Package {
    pub name: String,
    pub full_name: String,
    pub version: String,
    pub description: String,
    pub category: String,
    pub author: String,
    pub repository: String,
    pub path: String,
    pub license: String,
    pub dependencies: Vec<String>,
    pub features: Vec<String>,
    pub tags: Vec<String>,
    pub keywords: Vec<String>,
}

impl Registry {
    /// Load registry from default location
    pub async fn load() -> Result<Self> {
        let registry_path = Self::default_path()?;
        Self::load_from_path(&registry_path).await
    }

    /// Load registry synchronously (for 80/20 implementation)
    pub fn load_sync() -> Result<Self> {
        let registry_path = Self::default_path_sync()?;
        Self::load_from_path_sync(&registry_path)
    }

    /// Load registry from specific path
    pub async fn load_from_path(path: &Path) -> Result<Self> {
        let content = tokio::fs::read_to_string(path).await.map_err(|e| {
            ggen_utils::error::Error::new_fmt(format_args!("Failed to read registry file: {}", e))
        })?;

        let registry: Registry = toml::from_str(&content).map_err(|e| {
            ggen_utils::error::Error::new_fmt(format_args!("Failed to parse registry TOML: {}", e))
        })?;

        Ok(registry)
    }

    /// Load registry from specific path synchronously
    pub fn load_from_path_sync(path: &Path) -> Result<Self> {
        let content = std::fs::read_to_string(path).map_err(|e| {
            ggen_utils::error::Error::new_fmt(format_args!("Failed to read registry file: {}", e))
        })?;

        let registry: Registry = toml::from_str(&content).map_err(|e| {
            ggen_utils::error::Error::new_fmt(format_args!("Failed to parse registry TOML: {}", e))
        })?;

        Ok(registry)
    }

    /// Get default registry path synchronously
    pub fn default_path_sync() -> Result<PathBuf> {
        // Try to find workspace root by looking for Cargo.toml with [workspace]
        let current_dir = std::env::current_dir().map_err(|e| {
            ggen_utils::error::Error::new_fmt(format_args!(
                "Failed to get current directory: {}",
                e
            ))
        })?;

        // Search upwards for workspace root
        let mut search_dir = current_dir.as_path();
        loop {
            let cargo_toml = search_dir.join("Cargo.toml");
            if cargo_toml.exists() {
                // Check if it's a workspace
                if let Ok(content) = std::fs::read_to_string(&cargo_toml) {
                    if content.contains("[workspace]") {
                        // Found workspace root
                        return Ok(search_dir.join("marketplace/registry/packages.toml"));
                    }
                }
            }

            // Move up one directory
            if let Some(parent) = search_dir.parent() {
                search_dir = parent;
            } else {
                // Reached filesystem root without finding workspace
                break;
            }
        }

        // Fallback: assume we're in workspace already
        Ok(current_dir.join("marketplace/registry/packages.toml"))
    }

    /// Search packages by query string
    pub fn search(&self, query: &str, limit: usize) -> Vec<&Package> {
        let query_lower = query.to_lowercase();

        let mut matches: Vec<(&Package, f32)> = self
            .packages
            .iter()
            .filter_map(|pkg| {
                let mut score = 0.0f32;

                // Exact name match = highest score
                if pkg.name.to_lowercase() == query_lower {
                    score += 100.0;
                } else if pkg.name.to_lowercase().contains(&query_lower) {
                    score += 50.0;
                }

                // Description match
                if pkg.description.to_lowercase().contains(&query_lower) {
                    score += 20.0;
                }

                // Tag match
                for tag in &pkg.tags {
                    if tag.to_lowercase().contains(&query_lower) {
                        score += 30.0;
                    }
                }

                // Keyword match
                for keyword in &pkg.keywords {
                    if keyword.to_lowercase().contains(&query_lower) {
                        score += 25.0;
                    }
                }

                // Feature match
                for feature in &pkg.features {
                    if feature.to_lowercase().contains(&query_lower) {
                        score += 10.0;
                    }
                }

                if score > 0.0 {
                    Some((pkg, score))
                } else {
                    None
                }
            })
            .collect();

        // Sort by score descending
        matches.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));

        // Return top N matches
        matches
            .into_iter()
            .take(limit)
            .map(|(pkg, _)| pkg)
            .collect()
    }

    /// Get package by exact name
    pub fn get_package(&self, name: &str) -> Option<&Package> {
        self.packages
            .iter()
            .find(|pkg| pkg.name == name || pkg.full_name == name)
    }

    /// Get all packages in a category
    pub fn get_by_category(&self, category: &str) -> Vec<&Package> {
        self.packages
            .iter()
            .filter(|pkg| pkg.category.eq_ignore_ascii_case(category))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_search_exact_match() {
        let registry = Registry {
            version: "1.0.0".to_string(),
            packages: vec![Package {
                name: "genai-templates".to_string(),
                full_name: "genai-template-system".to_string(),
                version: "0.1.0".to_string(),
                description: "GenAI template generation system".to_string(),
                category: "ai".to_string(),
                author: "ggen-team".to_string(),
                repository: "https://github.com/test".to_string(),
                path: "marketplace/packages/genai-templates".to_string(),
                license: "MIT".to_string(),
                dependencies: vec![],
                features: vec![],
                tags: vec!["llm".to_string(), "genai".to_string()],
                keywords: vec!["llm".to_string()],
            }],
        };

        let results = registry.search("genai-templates", 10);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].name, "genai-templates");
    }

    #[test]
    fn test_search_partial_match() {
        let registry = Registry {
            version: "1.0.0".to_string(),
            packages: vec![Package {
                name: "genai-templates".to_string(),
                full_name: "genai-template-system".to_string(),
                version: "0.1.0".to_string(),
                description: "GenAI template generation system".to_string(),
                category: "ai".to_string(),
                author: "ggen-team".to_string(),
                repository: "https://github.com/test".to_string(),
                path: "marketplace/packages/genai-templates".to_string(),
                license: "MIT".to_string(),
                dependencies: vec![],
                features: vec![],
                tags: vec!["llm".to_string(), "genai".to_string()],
                keywords: vec!["llm".to_string()],
            }],
        };

        let results = registry.search("genai", 10);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].name, "genai-templates");
    }

    #[test]
    fn test_get_package() {
        let registry = Registry {
            version: "1.0.0".to_string(),
            packages: vec![Package {
                name: "genai-templates".to_string(),
                full_name: "genai-template-system".to_string(),
                version: "0.1.0".to_string(),
                description: "Test package".to_string(),
                category: "ai".to_string(),
                author: "test".to_string(),
                repository: "https://github.com/test".to_string(),
                path: "test".to_string(),
                license: "MIT".to_string(),
                dependencies: vec![],
                features: vec![],
                tags: vec![],
                keywords: vec![],
            }],
        };

        let pkg = registry.get_package("genai-templates");
        assert!(pkg.is_some());
        assert_eq!(pkg.unwrap().name, "genai-templates");
    }
}
