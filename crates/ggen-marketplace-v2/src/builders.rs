//! Type-safe builders for marketplace entities
//!
//! Uses builder pattern with compile-time validation to prevent invalid states.

use crate::error::Result;
use crate::models::{PackageId, PackageMetadata};

/// Type-safe package builder
pub struct PackageBuilder {
    id: Option<PackageId>,
    name: Option<String>,
    description: Option<String>,
    license: Option<String>,
    repository: Option<String>,
    homepage: Option<String>,
    authors: Vec<String>,
    keywords: Vec<String>,
    categories: Vec<String>,
}

impl PackageBuilder {
    /// Create a new builder
    #[must_use]
    pub fn new() -> Self {
        Self {
            id: None,
            name: None,
            description: None,
            license: None,
            repository: None,
            homepage: None,
            authors: Vec::new(),
            keywords: Vec::new(),
            categories: Vec::new(),
        }
    }

    /// Set package ID
    #[must_use]
    pub fn id(mut self, id: PackageId) -> Self {
        self.id = Some(id);
        self
    }

    /// Set package name
    #[must_use]
    pub fn name(mut self, name: impl Into<String>) -> Self {
        self.name = Some(name.into());
        self
    }

    /// Set description
    #[must_use]
    pub fn description(mut self, desc: impl Into<String>) -> Self {
        self.description = Some(desc.into());
        self
    }

    /// Set license
    #[must_use]
    pub fn license(mut self, license: impl Into<String>) -> Self {
        self.license = Some(license.into());
        self
    }

    /// Set repository URL
    #[must_use]
    pub fn repository(mut self, repo: impl Into<String>) -> Self {
        self.repository = Some(repo.into());
        self
    }

    /// Set homepage URL
    #[must_use]
    pub fn homepage(mut self, homepage: impl Into<String>) -> Self {
        self.homepage = Some(homepage.into());
        self
    }

    /// Add author
    #[must_use]
    pub fn author(mut self, author: impl Into<String>) -> Self {
        self.authors.push(author.into());
        self
    }

    /// Add keyword
    #[must_use]
    pub fn keyword(mut self, keyword: impl Into<String>) -> Self {
        self.keywords.push(keyword.into());
        self
    }

    /// Add category
    #[must_use]
    pub fn category(mut self, category: impl Into<String>) -> Self {
        self.categories.push(category.into());
        self
    }

    /// Build the package
    pub fn build(self) -> Result<PackageMetadata> {
        let id = self
            .id
            .ok_or_else(|| crate::error::Error::Other("Package ID is required".to_string()))?;

        let name = self
            .name
            .ok_or_else(|| crate::error::Error::Other("Package name is required".to_string()))?;

        let description = self.description.ok_or_else(|| {
            crate::error::Error::Other("Package description is required".to_string())
        })?;

        let license = self
            .license
            .ok_or_else(|| crate::error::Error::Other("Package license is required".to_string()))?;

        let mut metadata = PackageMetadata::new(id, name, description, license);
        metadata.repository = self.repository;
        metadata.homepage = self.homepage;
        metadata.authors = self.authors;
        metadata.keywords = self.keywords;
        metadata.categories = self.categories;

        Ok(metadata)
    }
}

impl Default for PackageBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_package_builder() {
        let result = PackageBuilder::new()
            .id(PackageId::new("test-pkg").unwrap())
            .name("Test Package")
            .description("A test package")
            .license("MIT")
            .author("Test Author")
            .keyword("testing")
            .category("development")
            .build();

        assert!(result.is_ok());
        let metadata = result.unwrap();
        assert_eq!(metadata.id.as_str(), "test-pkg");
        assert_eq!(metadata.name, "Test Package");
    }

    #[test]
    fn test_package_builder_missing_required() {
        let result = PackageBuilder::new()
            .name("Test Package")
            .description("A test package")
            .build();

        assert!(result.is_err());
    }
}
